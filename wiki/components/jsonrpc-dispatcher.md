---
type: component
elisp_files: []
server_files:
  - server/main.py
  - server/jsonrpc_handler.py
  - server/websocket_manager.py
  - server/config.py
  - server/error_handler.py
rpc_methods: [ping, get_config]
depends_on: []
depended_by: [kubernetes, helm, cloud-run, scheduler, compute, sql, gar, gcs, secrets, service-usage]
last_verified: 2026-06-11
---

# JSON-RPC Dispatcher (server)

The server's front door. A FastAPI app exposes `/jsonrpc`, `/ws`, and `/health`;
a singleton `JsonRpcHandler` maps method names to handlers that call the
per-domain GCP clients.

## What It Does

- `main.py` (160) — FastAPI app, the three endpoints, batch handling, shutdown
  cleanup.
- `jsonrpc_handler.py` (2258) — the `JsonRpcHandler` with `_register_methods()`
  registry (~140 methods), `handle()` dispatch, the Pydantic request/response
  models, and the error-code constants.
- `websocket_manager.py` (32) — `ConnectionManager` singleton: `connect`,
  `disconnect`, `broadcast(dict)`. Push channel for all domains.
- `config.py` (51) — `Config.from_env()` singleton (`host`, `port`,
  `gcs_project`, `impersonate_service_account`).
- `error_handler.py` (326) — `StructuredError` + factory functions that produce
  messages carrying a `type` and a `suggestion`.

## Endpoints (`main.py`)

| Endpoint | Purpose |
|----------|---------|
| `POST /jsonrpc` | Parse body → `JsonRpcRequest` → `handler.handle()` → `JsonRpcResponse`. Returns 204 for notifications; a JSON array for batch requests. |
| `WebSocket /ws` | `get_manager().connect()`, keep-alive loop, `disconnect` on drop |
| `GET /health` | `{"status":"ok","version":"0.1.0"}` — used by the elisp health-check gate |

## Dispatch (`JsonRpcHandler.handle`)

1. Require `jsonrpc == "2.0"` → else `INVALID_REQUEST` (-32600).
2. Look up `method` in `self._methods` → else `METHOD_NOT_FOUND` (-32601).
3. Call the handler with `params`; if `asyncio.iscoroutinefunction()`, await it.
4. Wrap as `JsonRpcResponse(id=..., result=...)`.
5. `TypeError` → `INVALID_PARAMS` (-32602). Other exceptions → parse the message
   for a structured `Type: message\nSuggestion: ...` shape and map to a
   service-specific code, defaulting to `INTERNAL_ERROR` (-32603).

### Error codes

| Code | Name | | Code | Name |
|------|------|-|------|------|
| -32700 | PARSE_ERROR | | -32002 | NOT_FOUND |
| -32600 | INVALID_REQUEST | | -32003 | GAR_ERROR |
| -32601 | METHOD_NOT_FOUND | | -32004 | COMPUTE_ERROR |
| -32602 | INVALID_PARAMS | | -32005 | K8S_ERROR |
| -32603 | INTERNAL_ERROR | | -32006 | HELM_ERROR |
| -32001 | GCS_ERROR | | -32007 | CLOUD_RUN_ERROR |
| | | | -32008 | CLOUD_SCHEDULER_ERROR |
| | | | -32009 | SERVICE_USAGE_ERROR |
| | | | -32010 | SECRET_MANAGER_ERROR |

### Models (Pydantic)

- `JsonRpcRequest(jsonrpc="2.0", id, method, params={})`
- `JsonRpcResponse(jsonrpc="2.0", id, result=None, error=None)`
- `JsonRpcError(code, message, data=None)`

## Key Invariants

- ⚠️ **`GRPC_DNS_RESOLVER=native` is set at the very top of `main.py` (line 9),
  before any import.** Importing a Google client first locks in the c-ares
  resolver and breaks under VPN. See [[004-grpc-native-dns-resolver]].
- ⚠️ **Method names are snake_case and case-sensitive.** Adding a method = handler
  + `_register_methods()` entry + an `ecloud-rpc-*` wrapper.
- ⚠️ **Either `result` or `error`, never both.** Errors always carry `code` (int)
  and `message` (str); `data` optional.
- ⚠️ **Notifications (`id is None`) get no response body** — `main.py` returns 204
  and omits them from batch arrays.
- ⚠️ **Async detection is by `asyncio.iscoroutinefunction()`.** An `async def`
  handler that isn't awaited returns a coroutine that fails to serialize — keep
  the registry honest about which handlers are coroutines.
- ⚠️ **Clients are singletons** (`get_<domain>_client()`); state (e.g. the K8s
  connection) is shared across all requests in that process.
- ⚠️ **CORS is wide open** (`allow_origins=["*"]`) — fine for loopback dev, not
  for exposure. There is no transport auth.

## Interactions

- Each handler calls a `get_<domain>_client()` singleton from the matching
  `server/<domain>_client.py`.
- Long/streaming ops broadcast via `get_manager().broadcast(...)` — see
  [[websocket-events]].
- On shutdown, `main.py` cleans up the SQL proxy manager and the Helm temp
  kubeconfig.

## Gotchas

- Structured-error parsing is **string-pattern based**: raise via
  `error_handler.py` (or format `Type: msg\nSuggestion: ...`) or the error
  collapses to a generic `INTERNAL_ERROR`.
- `broadcast()` is fire-and-forget: send failures to a dead socket are swallowed,
  no retry/queue. Clients that miss an event must refresh.
- Batch requests are processed **sequentially**, not in parallel.
- `config` is read at import time; env changes need a process restart (which is
  exactly what [[account-manager]] does per account).

## See Also

- [[rpc-bridge]] — the elisp client
- [[jsonrpc-bridge]], [[websocket-events]]
- [[001-jsonrpc-over-http]], [[004-grpc-native-dns-resolver]]
