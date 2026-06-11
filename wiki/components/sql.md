---
type: component
elisp_files:
  - emacs/ecloud-sql.el
server_files:
  - server/sql_client.py
  - server/sql_proxy.py
rpc_methods:
  - sql_list_instances
  - sql_list_databases
  - sql_create_database
  - sql_delete_database
  - sql_list_users
  - sql_create_user
  - sql_delete_user
  - sql_list_backups
  - sql_create_backup
  - sql_delete_backup
  - sql_restore_backup
  - sql_get_connection_info
  - sql_start_proxy
  - sql_stop_proxy
  - sql_list_proxies
depends_on: [rpc-bridge, jsonrpc-dispatcher]
depended_by: []
last_verified: 2026-06-11
---

# Cloud SQL

Manage Cloud SQL instances, databases, users, backups, and local proxy tunnels
to connect directly to an instance.

## What It Does

- `ecloud-sql.el` (295) — instance list, databases/users/backups views,
  connection info, proxy toggle.
- `sql_client.py` (206) — Cloud SQL Admin (sqladmin v1beta4) operations.
- `sql_proxy.py` (149) — `SQLProxyManager`: spawns/monitors `cloud-sql-proxy`
  subprocesses, auto-binds ports.

## Public API

Admin: `sql_list_instances`, `sql_list_databases(instance)`,
`sql_create_database` / `sql_delete_database`, `sql_list_users`,
`sql_create_user(instance, name, password, host="%")` / `sql_delete_user`,
`sql_list_backups` / `sql_create_backup` / `sql_delete_backup` /
`sql_restore_backup`, `sql_get_connection_info(instance)` (connectionName, public/
private IP, JDBC/psql URI templates).
Proxy: `sql_start_proxy(connection_name, port, db_type)` (returns bound port),
`sql_stop_proxy`, `sql_list_proxies`.

Elisp: `ecloud-sql-list`, `ecloud-sql-show-databases`, `-show-users`,
`-show-backups`, `-connection-info`, `-toggle-proxy`,
`-create-database`/`-delete-database`, `-create-user`/`-delete-user`.

## Key Invariants

- ⚠️ **Proxy lifecycle is managed.** `cloud-sql-proxy` (or `cloud_sql_proxy`) must
  be on PATH; `_monitor_proxy()` removes the registry entry when the subprocess
  exits, and `SQLProxyManager.shutdown()` runs on FastAPI shutdown
  (`main.py`). A SIGKILL can orphan proxies.
- A `RUNNABLE` instance with `activationPolicy="NEVER"` is reported as STOPPED.
- Connection name is `project:region:instance`, used as the proxy key.
- Connection strings are templates; ECloud does not store DB user/password.

## Interactions

- [[jsonrpc-dispatcher]] (`sql_*`).
- WebSocket: `sql_proxy_started` / `sql_proxy_stopped` → `ecloud-sql-event-hook`
  updates the proxy-status column. See [[websocket-events]].

## Gotchas

- Port discovery binds a socket locally; needs local network access.
- Databases/users are per-instance, not globally browsable.

## See Also
- [[rpc-bridge]], [[websocket-events]]
