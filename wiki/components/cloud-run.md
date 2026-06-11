---
type: component
elisp_files:
  - emacs/ecloud-cloud-run.el
server_files:
  - server/cloud_run_client.py
rpc_methods:
  - cloud_run_list_regions
  - cloud_run_list_services
  - cloud_run_list_all_services
  - cloud_run_get_service
  - cloud_run_list_revisions
  - cloud_run_delete_service
  - cloud_run_get_logs
  - cloud_run_deploy_service
depends_on: [rpc-bridge, jsonrpc-dispatcher]
depended_by: []
last_verified: 2026-06-11
---

# Cloud Run

Browse, deploy, and inspect Cloud Run services across one region or all regions.

## What It Does

- `ecloud-cloud-run.el` (493) — service browser, detail/logs views, interactive
  deploy/delete, region toggle.
- `cloud_run_client.py` (753) — service/revision listing, deploy, delete, logs;
  multi-region discovery + parallel fan-out.

## Public API

| RPC method | Notes |
|-----------|-------|
| `cloud_run_list_regions` | ~38 hardcoded regions |
| `cloud_run_list_services(region)` | single region |
| `cloud_run_list_all_services` | async; Asset-API-discovered regions, parallel; latency ≈ slowest region |
| `cloud_run_get_service(name, region)` | `ServiceInfo` (status, url, image, traffic, min/max instances, cpu, memory) |
| `cloud_run_list_revisions(service, region)` | `RevisionInfo` |
| `cloud_run_deploy_service(name, image, region, port, cpu, memory, min, max, allow_unauthenticated)` | async; broadcasts `cloud_run_service_deployed` |
| `cloud_run_delete_service(name, region)` | async; broadcasts `cloud_run_service_deleted` |
| `cloud_run_get_logs(service, region, limit, severity)` | Cloud Logging, filtered to `cloud_run_revision` |

Elisp: `ecloud-cloud-run-list`, `-refresh`, `-change-region`,
`-toggle-all-regions`, `-view-service`, `-view-logs`, `-deploy`, `-delete-service`,
`-open-url`. Default region behaviour seeded by `ecloud-cloud-run-show-all-regions`;
nil `ecloud-cloud-run--current-region` = all regions.

## Key Invariants

- ⚠️ **All-regions prefers the Asset API** (1–3 s); falls back to a parallel
  region scan (`ThreadPoolExecutor(max_workers=10)`, 30 s overall / 5 s per
  region) if unavailable.
- ⚠️ **Asset API has a few-minute ingestion lag** — brand-new services may not
  appear in all-regions mode yet.
- Status maps `CONDITION_SUCCEEDED`→"Ready", `CONDITION_FAILED`→"Failed".
- Deploy waits up to 600 s and optionally grants `allUsers`/`run.invoker` for
  public access.

## Interactions

- [[jsonrpc-dispatcher]] (`cloud_run_*`, error `-32007`).
- `ecloud-cloud-run-event-hook` receives `cloud_run_service_deployed` /
  `cloud_run_service_deleted` and auto-refreshes the view. See [[websocket-events]].

## Gotchas

- Per-region scan errors in all-regions mode are reported in the `errors` map,
  not raised.
- Image strings are truncated to 50 chars for display.

## See Also
- [[scheduler]] (same all-regions fan-out pattern), [[rpc-bridge]], [[websocket-events]]
