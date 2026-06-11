---
type: component
elisp_files:
  - emacs/ecloud-rpc.el
  - emacs/ecloud-ws.el
server_files: []
rpc_methods: []
depends_on: [account-manager]
depended_by: [kubernetes, helm, cloud-run, scheduler, compute, sql, gar, gcs, secrets, service-usage]
last_verified: 2026-06-11
---

# RPC Bridge (elisp client)

The Emacs side of the wire. Every domain view calls through here; nothing in
`ecloud-*.el` talks HTTP directly.

## What It Does

- `ecloud-rpc.el` (1006 lines) — JSON-RPC 2.0 client over HTTP. Builds requests,
  POSTs them, parses `:result` / `:error`, and surfaces structured errors.
- `ecloud-ws.el` (162 lines) — a single WebSocket client that receives server
  push messages and fans them out to per-domain event hooks.

## Public API (key functions)

### `ecloud-rpc.el`

| Function | Description |
|----------|-------------|
| `ecloud-rpc-request(method &optional params)` | Synchronous (blocking) call; returns the result or signals an error |
| `ecloud-rpc-request-async(method callback &optional params error-callback)` | Non-blocking; `callback` gets the result, `error-callback` gets an error string |
| `ecloud-rpc--build-request(method params)` | Frames `{:jsonrpc "2.0" :id N :method ... :params ...}` |
| `ecloud-rpc--next-id()` | Increments the session-global `ecloud-rpc--request-id` |
| `ecloud-rpc--get-current-url()` | Resolves the active account's server URL (falls back to `ecloud-server-url`) |
| `ecloud-rpc--parse-response(str)` | Extracts `:result`, or raises with `:error.code/message/data` |
| `ecloud-rpc--quick-health-check()` | Probes `/health` (2 s timeout) — gates auto-restart |

Per-domain wrappers (`ecloud-rpc-<domain>-<action>` / `...-async`) live in this
file too, e.g. `ecloud-rpc-k8s-list-pods-async`. They are thin shims over
`ecloud-rpc-request[-async]` with the snake_case method name.

### `ecloud-ws.el`

| Function | Description |
|----------|-------------|
| `ecloud-ws-connect()` | Opens `ecloud-ws-url` (default `ws://127.0.0.1:8765/ws`), stores `ecloud-ws-client` |
| `ecloud-ws-disconnect()` | Closes and nils the client |
| `ecloud-ws--handle-message(text)` | Parses `{type,data}` JSON, routes by `type` prefix to a hook |

Customs: `ecloud-server-url` (`http://127.0.0.1:8765/jsonrpc`),
`ecloud-request-timeout` (30 s), `ecloud-ws-url`, `ecloud-ws-auto-connect` (t).

## WebSocket type → hook routing

`ecloud-ws--handle-message` dispatches by the `type` prefix:

| Prefix | Hook | Args |
|--------|------|------|
| `sql_proxy_*` | `ecloud-sql-event-hook` | `(type data)` |
| `gcs_*` | `ecloud-gcs-event-hook` | `(type data)` |
| `gar_*` | `ecloud-gar-event-hook` | `(type data)` |
| `cloud_run_*` | `ecloud-cloud-run-event-hook` | `(type data)` |
| `cloud_scheduler_*` | `ecloud-scheduler-event-hook` | `(type data)` |
| `service_usage_*` | `ecloud-service-usage-event-hook` | `(type data)` |
| `k8s_log` | `ecloud-k8s-log-hook` | `(data)` |
| `k8s_exec_output` | `ecloud-k8s-exec-hook` | `(type data)` |
| other `k8s_*` | `ecloud-k8s-event-hook` | `(type data)` |

## Key Invariants

- ⚠️ **Request IDs are unique per session.** `ecloud-rpc--request-id` is a
  module-level counter, never reset on error. Don't reuse IDs.
- ⚠️ **All elisp calls supply an `:id`** — ECloud never sends JSON-RPC
  notifications, so every request expects a response.
- ⚠️ **Health-check gate before restart.** Before treating a timeout as a dead
  server, `ecloud-rpc--quick-health-check()` must confirm `/health` is
  unresponsive. Slow ops (pod exec, big downloads) routinely exceed
  `ecloud-request-timeout`; without this gate they'd falsely restart the server.
  See [[006-health-check-before-restart]].
- ⚠️ **URL is resolved per-request** via `ecloud-rpc--get-current-url()`, so
  switching accounts transparently redirects all subsequent calls. See
  [[account-manager]].
- ⚠️ **`Content-Type: application/json`** must be set on every request.
- ⚠️ **Disconnect must nil `ecloud-ws-client`** — a lingering handle makes the
  next connect fail.

## Interactions

- Reads the active account URL from [[account-manager]]
  (`ecloud-account--get-url`).
- On connection failure + failed health check, calls
  `ecloud-account--handle-connection-error` which may restart the server and
  retry the original call once.
- WebSocket push is one-directional (server → Emacs); see [[websocket-events]].

## Gotchas

- The 2-second health-check timeout is itself a network call; a very slow
  loopback can flap restarts. Tune with care.
- `ecloud-ws--handle-message` routing is prefix-based; a new `type` with an
  unrecognized prefix is silently dropped. Add the prefix + hook when you add a
  new push event.
- WebSocket auto-connect is deferred via an idle timer in `ecloud.el` (2 s) to
  avoid load-time errors.

## See Also

- [[jsonrpc-dispatcher]] — the server side of the same wire
- [[jsonrpc-bridge]] — the request/response pattern in depth
- [[websocket-events]] — push event model
- [[001-jsonrpc-over-http]], [[006-health-check-before-restart]]
