---
type: concept
applies_to: [rpc-bridge, jsonrpc-dispatcher, kubernetes, cloud-run, scheduler, sql, gar, gcs, service-usage]
decisions: [003-websocket-push-streaming]
---

# WebSocket Push Events

The server → Emacs channel. Used whenever a result is incremental (log lines,
exec output, streaming lists) or a long op finishes (deploy, delete, proxy
start/stop) and the UI should react without polling.

## The mechanism

1. A handler starts background work and returns a quick ack (often a `session_id`
   / `stream_id`) over the normal JSON-RPC response.
2. As data arrives, it calls `get_manager().broadcast({"type": ..., "data": ...})`
   (`websocket_manager.py`).
3. `ecloud-ws--handle-message` parses `{type,data}` and dispatches by the `type`
   **prefix** to a per-domain hook.
4. The hook function updates the buffer / vterm / tabulated-list.

## Prefix → hook map

| Prefix | Hook |
|--------|------|
| `sql_proxy_*` | `ecloud-sql-event-hook` |
| `gcs_*` | `ecloud-gcs-event-hook` |
| `gar_*` | `ecloud-gar-event-hook` |
| `cloud_run_*` | `ecloud-cloud-run-event-hook` |
| `cloud_scheduler_*` | `ecloud-scheduler-event-hook` |
| `service_usage_*` | `ecloud-service-usage-event-hook` |
| `k8s_log` | `ecloud-k8s-log-hook` |
| `k8s_exec_output` | `ecloud-k8s-exec-hook` |
| other `k8s_*` | `ecloud-k8s-event-hook` |

## Streaming pattern (service-usage, cloud-run all-regions feel)

For huge result sets the server pages the data and broadcasts a batch per page,
tagging each with a `stream_id` and an `is_final` flag. The elisp side registers a
hook handler keyed on `stream_id` and removes it when `is_final` arrives. See
[[service-usage]].

## Invariants

- ⚠️ **Broadcast is fire-and-forget.** A dropped socket misses the event; there is
  no retry/queue. A view that depends on a missed event must be refreshed.
- ⚠️ **Unknown `type` prefixes are silently dropped.** Adding a push event means
  adding the prefix + hook in `ecloud-ws.el`.
- ⚠️ **Most hooks take `(type data)`; `ecloud-k8s-log-hook` takes `(data)` only.**

## See Also
- [[rpc-bridge]], [[jsonrpc-dispatcher]], [[003-websocket-push-streaming]], [[k8s-pod-exec-vterm]]
