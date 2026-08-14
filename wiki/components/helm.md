---
type: component
elisp_files:
  - emacs/ecloud-k8s.el
server_files:
  - server/helm_client.py
rpc_methods:
  - helm_list_releases
  - helm_get_release_details
  - helm_install_chart
  - helm_upgrade_release
  - helm_rollback_release
  - helm_uninstall_release
  - helm_list_repositories
  - helm_add_repository
  - helm_remove_repository
  - helm_search_charts
depends_on: [rpc-bridge, jsonrpc-dispatcher, kubernetes]
depended_by: []
spec_refs:
  - .kiro/specs/helm-and-transient-ui/design.md
last_verified: 2026-08-14
---

# Helm

Manage Helm 3 releases on the connected GKE cluster: list/describe releases,
install/upgrade/rollback/uninstall charts, and manage chart repositories. The
elisp UI lives inside `ecloud-k8s.el`; the server logic is `helm_client.py`.

## What It Does

- `helm_client.py` (1021) — wraps **pyhelm3** for release operations and the
  **`helm` CLI** for repository operations. Builds a temporary kubeconfig from
  the [[kubernetes]] cluster credentials.
- Helm views in `ecloud-k8s.el` — the `*ECloud-Helm-Releases*` tabulated list and
  release detail/history buffers.

## Public API (server)

| Method | Returns / effect |
|--------|------------------|
| `initialize()` | Async; writes a temp kubeconfig from cluster endpoint + token, plus the CA when there is one |
| `list_releases(namespace, all_namespaces, fetch_details, include_all=True, max_releases=0)` | Releases; `fetch_details=True` fetches revision + chart metadata concurrently. `include_all` = `helm list -a`, `max_releases=0` = no `--max` cap |
| `get_release_details(name, namespace)` | chart, version, status, values, revision history, notes |
| `install_chart(release, chart_ref, namespace, values, version, create_namespace, wait, timeout)` | Install |
| `upgrade_release(...)`, `rollback_release(name, revision, namespace, wait)`, `uninstall_release(name, namespace, wait)` | Lifecycle |
| `list_repositories()`, `add_repository(name, url)`, `remove_repository(name)`, `search_charts(keyword, repo)` | Repo ops (via `helm` CLI) |

## Public API (elisp)

`ecloud-k8s-helm-list`, `ecloud-k8s-helm-describe`, `ecloud-k8s-helm-install`,
`ecloud-k8s-helm-upgrade`, `ecloud-k8s-helm-history`, `ecloud-k8s-helm-rollback`,
`ecloud-k8s-helm-uninstall`, and `ecloud-k8s-helm-toggle-details` (toggles the
slow `fetch_details` mode). Transient help: `ecloud-k8s-helm-help`,
`ecloud-k8s-helm-history-help`.

## Key Invariants

- ⚠️ **`initialize()` runs before any release op** — it materializes the temp
  kubeconfig (endpoint, CA path, token) from the K8s connection. It is called
  once, from `_k8s_connect`; if it is skipped, every `helm_*` call fails with
  `helm_not_initialized_error` while all K8s views keep working.
- ⚠️ **The CA path is optional.** On DNS-endpoint clusters
  `get_cluster_credentials()` yields `ca_cert_path=None`; `initialize()` must
  still proceed and simply omit `certificate-authority` from the kubeconfig, so
  Helm verifies against the system root store. Requiring a CA here is what broke
  Helm listing on public-DNS clusters. See [[007-gke-endpoint-ca-trust]].
- ⚠️ **pyhelm3 for releases, `helm` CLI for repos.** There is no CLI fallback for
  release operations.
- ⚠️ **Detail-fetch concurrency is bounded** by `HELM_CONCURRENT_REQUESTS`
  (default 20) via an `asyncio.Semaphore` — tune down for weaker clusters.
- ⚠️ **Temp kubeconfig is cleaned up** via an `atexit` handler and on FastAPI
  shutdown (`reset_helm_client()` in `main.py`). A SIGKILL leaks it.
- ⚠️ **Values are passed as raw YAML strings** (elisp sends a `:raw` plist); they
  are not pre-parsed by the bridge.

## Interactions

- Depends on [[kubernetes]] for cluster credentials; there is no direct K8s↔Helm
  method call — Helm just reads endpoint/CA-or-`None`/token.
- Routes through [[jsonrpc-dispatcher]] (`helm_*`, error code `-32006`).
- No WebSocket events; operations are request/response (with `wait`/`timeout`).

## Gotchas

- Listing many releases with `fetch_details=t` is slow (per-release metadata
  calls); leave it off for big clusters and toggle on demand.
- Releases are listed in **every** state by default (`helm list -a`, commit
  `cb2486b`) so a release whose latest revision failed stays visible for
  rollback; `ecloud-k8s-helm-include-all` turns that off. `max_releases=0`
  overrides pyhelm3's `--max 256`, which truncated alphabetically without
  saying so.
- pyhelm3 can be version-sensitive against the cluster's Helm/Tiller-less setup;
  errors surface with permission/accessibility hints.
- Repo operations shell out — the `helm` binary must be on PATH.

## See Also

- [[kubernetes]] — provides the cluster connection
- [[007-gke-endpoint-ca-trust]] — why the CA path may be `None`
- [[helm-and-transient-ui]] — distilled spec
- [[jsonrpc-dispatcher]]
