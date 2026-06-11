# ECloud Conventions

Style, naming, and workflow rules that hold across the whole codebase. When in
doubt, match the surrounding code.

## The Two-Tier Mental Model

Every feature has an elisp half and a Python half:

```
emacs/ecloud-<domain>.el   ──calls──▶  emacs/ecloud-rpc.el
                                            │ HTTP POST /jsonrpc
                                            ▼
                            server/jsonrpc_handler.py  (_register_methods)
                                            │ dispatch by method name
                                            ▼
                            server/<domain>_client.py  ──▶ GCP SDK
```

Async results and progress come back the other way over the WebSocket
(`/ws` → `emacs/ecloud-ws.el` → a per-domain event hook). See
[[jsonrpc-bridge]] and [[websocket-events]].

## RPC Method Naming

- Server method names are **snake_case** and registered in
  `JsonRpcHandler._register_methods()` (`server/jsonrpc_handler.py`), e.g.
  `k8s_list_pods`, `cloud_run_deploy_service`, `gar_pull`.
- Domain prefix matches the client module: `k8s_*`, `cloud_run_*`,
  `cloud_scheduler_*`, `compute_*`, `sql_*`, `gar_*`, `gcs`/GCS verbs,
  `secret_manager_*`, `service_usage_*`, `helm_*`.
- Elisp wrappers live in `ecloud-rpc.el` as `ecloud-rpc-<kebab-name>` and
  `...-async`, e.g. `ecloud-rpc-k8s-list-pods-async`.
- The method-name string passed over the wire is the snake_case server name, not
  the kebab elisp name. **Method lookup is case-sensitive.**

## Elisp View Module Pattern

Every domain view follows the same shape (see [[ui-conventions]]):

- Entry command: `ecloud-<domain>-list` — creates `*ECloud-<Domain>*` buffer,
  enters the mode, refreshes, switches to it.
- Major mode: `ecloud-<domain>-mode`, derived from `tabulated-list-mode`.
- Keymap: `ecloud-<domain>-mode-map`.
- Refresh: `ecloud-<domain>-refresh` (interactive) → `ecloud-<domain>--refresh-data`
  (sets `tabulated-list-format` / `-entries`, calls `tabulated-list-init-header`
  + `tabulated-list-print`).
- Help: `ecloud-<domain>-help` — a `transient-define-prefix` bound to `?`.
- Common keys: `?` help, `q` quit-window, `g`/`r` refresh, `RET` enter/act.
- Faces: `ecloud-<domain>-<element>-face` (e.g. `ecloud-k8s-name-face`).
- WebSocket events: a `ecloud-<domain>-event-hook` (and for k8s, separate
  `-log-hook` / `-exec-hook`), wired by `ecloud-ws--handle-message`.

⚠️ **Keymap-before-derived-mode:** define `ecloud-<domain>-mode-map` *before*
`define-derived-mode`, or the macro's auto-created empty keymap shadows it
(this bit `ecloud-secrets` — commit `c0f2f10`).

## Server Client Pattern

- One module per service: `server/<domain>_client.py`, exposing a class and a
  `get_<domain>_client()` singleton accessor.
- Return typed `@dataclass` DTOs with a `to_dict()` for JSON serialization
  (e.g. `PodInfo`, `ServiceInfo`, `InstanceInfo`).
- GKE/K8s calls are wrapped with the `@auto_refresh_token` decorator and gated by
  `_ensure_connected()` — see [[token-refresh]].
- **`GRPC_DNS_RESOLVER=native` must be set before importing any Google client
  library** (`main.py` line 9, plus defensively in several `*_client.py`). See
  [[004-grpc-native-dns-resolver]].

## Errors

- Server error codes are JSON-RPC codes plus ECloud-specific ones (`-32001`
  GCS … `-32010` Secret Manager). See [[jsonrpc-dispatcher]] for the full table.
- Raise structured errors via `server/error_handler.py` so the message carries a
  `type` and a `suggestion`; the dispatcher parses these into `error.data` for
  the elisp client to display.

## Commit & PR Rules

- Branch off `main`; do not commit directly to `main` unless asked.
- Commit only files relevant to the change. The repo carries pre-existing
  untracked files (`.kiro/`, test scripts) — do not sweep them in with
  `git add -A`.
- After a change that touches a documented module, run **session-end sync**
  (see [[schema]] § Sync Protocol) and append to [[changelog]].

## Reload Workflow

`reload-ecloud.el` (repo root) reloads all elisp modules in dependency order
without restarting Emacs:

- `M-x reload-ecloud` — reload everything except stateful modules
  (`ecloud-account-manager`, which owns running server subprocesses).
- `C-u M-x reload-ecloud` — full reload (orphans running account servers; restart
  them via `ecloud-account-list-processes`).
- It purges stale `.elc` files and disconnects the WebSocket cleanly first.
- `ecloud-notify-unload-function` sweeps orphan notification posframes on unload
  (commit `1d0ae0e`).

## Phase / File Map (where things live)

| Concern | Elisp | Server |
|---------|-------|--------|
| RPC transport | `ecloud-rpc.el`, `ecloud-ws.el` | `main.py`, `jsonrpc_handler.py`, `websocket_manager.py` |
| Multi-account | `ecloud-account-manager.el` | `config.py` (per-process env) |
| Kubernetes | `ecloud-k8s.el` | `k8s_client.py`, `k8s_log_streamer.py`, `k8s_pod_exec_streamer.py` |
| Helm | `ecloud-k8s.el` (helm views) | `helm_client.py` |
| Cloud Run | `ecloud-cloud-run.el` | `cloud_run_client.py` |
| Scheduler | `ecloud-scheduler.el` | `cloud_scheduler_client.py` |
| Compute / IPs | `ecloud-compute.el`, `ecloud-ips.el` | `compute_client.py` |
| Cloud SQL | `ecloud-sql.el` | `sql_client.py`, `sql_proxy.py` |
| Artifact Registry | `ecloud-gar.el` | `gar_client.py` |
| Cloud Storage | `ecloud-browser.el`, `ecloud-commands.el` | `gcs_client.py` |
| Secret Manager | `ecloud-secrets.el` | `secret_manager_client.py` |
| Service Usage | `ecloud-services.el` | `service_usage_client.py` |
| UI shared | `ecloud.el`, `ecloud-transient.el`, `ecloud-notify.el` | — |
