---
type: component
elisp_files:
  - emacs/ecloud-services.el
server_files:
  - server/service_usage_client.py
rpc_methods:
  - service_usage_list_services
  - service_usage_list_services_streaming
  - service_usage_enable_service
  - service_usage_disable_service
  - service_usage_get_service
depends_on: [rpc-bridge, jsonrpc-dispatcher]
depended_by: []
last_verified: 2026-06-11
---

# Service Usage (GCP APIs)

Enable, disable, and list the GCP APIs/services on a project. The DISABLED/all
list is huge (10k+ services), so it streams in batches over the WebSocket.

## What It Does

- `ecloud-services.el` (299) — service list (`*ECloud Services*`), enable/disable
  with fuzzy completion, get-info.
- `service_usage_client.py` (317) — Service Usage (service_usage_v1) operations
  with a streaming list.

## Public API

`service_usage_list_services(filter_state)` — non-streaming; ENABLED is the fast
default. `service_usage_list_services_streaming(filter_state, callback)` — yields
batches (every ~200 services) and broadcasts them. `service_usage_enable_service`
/ `service_usage_disable_service` (wait on the LRO, ~300 s timeout);
`service_usage_get_service`.

Elisp: `ecloud-services-list(filter_state)` (ENABLED default; `C-u` to choose
ENABLED/DISABLED/All), `ecloud-services-enable`, `-enable-by-name`, `-disable`,
`-get-info`.

## Key Invariants

- ⚠️ **Page size max is 200** (API limit `SU_INVALID_PAGE_SIZE` above that);
  streaming emits a batch per page.
- ⚠️ **Streaming uses a `stream_id`** to correlate WebSocket batches with the
  request; the elisp side adds a hook handler and removes it on `is_final` (manual
  cleanup).
- ENABLED list is small/fast; DISABLED/All should always go through streaming.
- ⚠️ **`GRPC_DNS_RESOLVER=native` set in the client.** See
  [[004-grpc-native-dns-resolver]].

## Interactions

- [[jsonrpc-dispatcher]] (`service_usage_*`, error `-32009`).
- WebSocket: `service_usage_list_batch` (`stream_id, services[], total_count,
  is_final`) → `ecloud-service-usage-event-hook`. See [[websocket-events]].

## Gotchas

- A "Loading…" buffer shows progress and is torn down on completion/error;
  forgetting to remove the hook on error leaks handlers.
- Permission errors suggest `serviceusage.serviceUsageViewer/Admin`; if the
  Service Usage API itself is off, the error suggests
  `gcloud services enable serviceusage.googleapis.com`.

## See Also
- [[websocket-events]] (streaming pattern), [[rpc-bridge]], [[004-grpc-native-dns-resolver]]
- [[service-usage-quickstart]] — end-user quickstart
- [[service-usage-implementation]] — point-in-time implementation report
