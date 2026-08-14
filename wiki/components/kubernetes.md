---
type: component
elisp_files:
  - emacs/ecloud-k8s.el
server_files:
  - server/k8s_client.py
  - server/k8s_log_streamer.py
  - server/k8s_pod_exec_streamer.py
rpc_methods:
  - k8s_list_clusters
  - k8s_connect
  - k8s_disconnect
  - k8s_connection_status
  - k8s_list_namespaces
  - k8s_list_pods
  - k8s_list_services
  - k8s_list_ingresses
  - k8s_list_deployments
  - k8s_scale_deployment
  - k8s_get_yaml
  - k8s_resource_metrics
  - k8s_pod_logs
  - k8s_start_log_stream
  - k8s_stop_log_stream
  - k8s_list_log_streams
  - k8s_pod_exec
  - k8s_pod_exec_interactive
  - k8s_pod_exec_send_input
  - k8s_pod_exec_resize
  - k8s_pod_exec_stop_session
  - k8s_pod_exec_list_sessions
  - k8s_apply_manifest
depends_on: [rpc-bridge, jsonrpc-dispatcher]
depended_by: [helm]
last_verified: 2026-08-14
---

# Kubernetes (GKE)

Browse and operate GKE clusters from Emacs: connect, list pods / services /
ingresses / deployments / namespaces, view YAML, stream logs, and run interactive
shells in a pod via vterm.

## What It Does

| File | Lines | Role |
|------|-------|------|
| `ecloud-k8s.el` | 1925 | All K8s views (tabulated-list), pod exec (one-shot + vterm), log streaming, scaling, YAML viewer, transient help; also hosts the [[helm]] views |
| `k8s_client.py` | 1198 | GKE connect/auth, list/get ops returning `*Info` dataclasses, token refresh, metrics-server integration, pod exec, YAML, apply |
| `k8s_log_streamer.py` | 256 | Async multi-pod log streaming via `kubernetes.watch`; per-stream stop, text filter, broadcast |
| `k8s_pod_exec_streamer.py` | 279 | Interactive TTY exec sessions; bidirectional stdin/stdout, resize channel, session lifecycle |

## Public API (server)

- **Connect:** `connect(cluster_name, location)` — builds credentials, refreshes
  the OAuth2 token, picks an endpoint (see DNS invariant below).
  `_ensure_connected()` gates every op; `@auto_refresh_token` retries once on 401.
- **List → DTOs:** `list_clusters`, `list_pods(namespace, label_selector, limit,
  include_metrics)` → `PodInfo`, `list_services` → `ServiceInfo`,
  `list_ingresses`, `list_deployments`, `list_namespaces`.
- **Read:** `get_pod_logs(name, namespace, container, tail_lines)`,
  `get_resource_yaml(kind, name, namespace)`,
  `_get_pod_metrics(namespace)` → `{(ns,name): (cpu, memory)}` from
  `metrics.k8s.io` (empty if metrics-server absent).
- **Exec/streaming:** `K8sLogStreamer.start_stream/stop_stream/update_filter`;
  `K8sPodExecStreamer.start_exec_session/send_input/resize/stop_session`.

`PodInfo` carries `name, namespace, status, phase, ip, node, containers, ready,
restarts, age, cpu, memory` — the `cpu`/`memory` columns come from metrics-server
(added 2026-06-11, commit `9dc689e`).

## Public API (elisp)

Entry `ecloud-k8s-list` (shows clusters until connected, then pods). View
switchers `ecloud-k8s-switch-to-{pods,services,ingresses,deployments,namespaces}`.
Actions: `ecloud-k8s-connect-cluster`, `ecloud-k8s-view-yaml`,
`ecloud-k8s-view-logs`, `ecloud-k8s-stream-logs`, `ecloud-k8s-pod-exec`
(one-shot), `ecloud-k8s-pod-exec-vterm` (interactive shell),
`ecloud-k8s-scale-deployment`. Namespace filter via `ecloud-k8s--current-namespace`
(nil = all); pod cap via `ecloud-k8s-pod-fetch-limit` (default 500, 0 = unlimited).

## Key Invariants

- ⚠️ **Connect before any op.** `_ensure_connected()` raises if `_api_client` is
  None.
- ⚠️ **Token refresh on 401.** `@auto_refresh_token` calls `_refresh_token()` and
  retries once. See [[token-refresh]].
- ⚠️ **Endpoint selection avoids the 403 trap.** Prefer the DNS endpoint only when
  `control_plane_endpoints_config.dns_endpoint_config.allow_external_traffic` is
  true; otherwise fall back to the IP endpoint + self-signed CA. Picking the DNS
  endpoint without external traffic yields a 403 HTML page from the Google Front
  End on private clusters (commit `3b8e885`).
- ⚠️ **The endpoint choice picks the TLS trust anchor, and a missing CA is
  normal.** The IP endpoint needs the cluster's self-signed CA written to a temp
  file; the DNS endpoint is served by a Google public CA, so no CA file is
  written and `_config.ssl_ca_cert` stays unset (system root store). Hence
  `get_cluster_credentials()` returns `ca_cert_path=None` on DNS-endpoint
  clusters — that is a connected state, **not** a failure. Never treat a falsy
  CA path as "not connected". See [[007-gke-endpoint-ca-trust]].
- ⚠️ **Metrics are best-effort.** `_get_pod_metrics` returns `{}` if metrics-server
  is missing or RBAC denies it; the pod list still renders with `-` in CPU/Memory.
  Only fetched when `include_metrics=True` (the pods view passes it; the log
  streamer's name lookups don't, to skip the round-trip).
- ⚠️ **Exec uses a TTY stream** (`tty=True, stdin=True`); resize writes JSON
  `{"Height","Width"}` to channel 4.

## Interactions

- All ops route through [[jsonrpc-dispatcher]] (`k8s_*` methods).
- Log lines → `broadcast({type:"k8s_log",...})` → `ecloud-k8s-log-hook`.
- Exec output → `broadcast({type:"k8s_exec_output",...})` → `ecloud-k8s-exec-hook`
  → vterm render; session close → `k8s_exec_session_stopped` →
  `ecloud-k8s-event-hook`. Full path in [[k8s-pod-exec-vterm]].
- Provides cluster credentials (endpoint, CA *or `None`*, token) to [[helm]] via
  `get_cluster_credentials()`, called once from `_k8s_connect`.

## Gotchas

- **VPN / private clusters:** if the IP endpoint is unreachable, connect raises a
  `K8sConnectionError` suggesting "connect to VPN first."
- **vterm dependency:** `ecloud-k8s-pod-exec-vterm` needs the `vterm` package; the
  vterm buffer is backed by an inert `tail -f /dev/null` subprocess and bridged
  over the WebSocket. The plain `ecloud-k8s-pod-exec` strips ANSI via
  `ecloud-k8s--strip-ansi`.
- Large clusters: raise/lower the pod cap with `ecloud-k8s-set-pod-limit` /
  `ecloud-k8s-toggle-pod-limit`.

## See Also

- [[helm]] — releases on the connected cluster
- [[token-refresh]], [[websocket-events]], [[k8s-pod-exec-vterm]]
- [[004-grpc-native-dns-resolver]], [[007-gke-endpoint-ca-trust]]
