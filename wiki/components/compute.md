---
type: component
elisp_files:
  - emacs/ecloud-compute.el
  - emacs/ecloud-ips.el
server_files:
  - server/compute_client.py
rpc_methods:
  - compute_list_instances
  - compute_start_instance
  - compute_stop_instance
  - compute_reset_instance
  - compute_delete_instance
  - compute_list_addresses
  - compute_reserve_address
  - compute_list_regions
depends_on: [rpc-bridge, jsonrpc-dispatcher]
depended_by: []
last_verified: 2026-06-11
---

# Compute Engine (VMs + IPs)

Browse VM instances across all zones, run lifecycle operations, SSH in via vterm,
and manage static/ephemeral IP addresses. `ecloud-ips.el` is a specialized view
over the same `list_addresses` server method.

## What It Does

- `ecloud-compute.el` (346) — instance browser, start/stop/reset/delete, SSH,
  copy name/IPs.
- `ecloud-ips.el` (246) — IP-address browser + reserve; sorts by status (IN_USE →
  RESERVED → Ephemeral). No new server logic.
- `compute_client.py` (388) — aggregated instance + address listing, lifecycle
  ops, address reservation.

## Public API

`compute_list_instances` → `InstanceInfo` (name, zone, status, internal_ip,
external_ip, machine_type) aggregated across zones; `compute_{start,stop,reset,
delete}_instance(zone, instance)` (wait for op, ~300 s); `compute_list_addresses`
→ `AddressInfo` (static regional/global + ephemeral from VM NICs, de-duped);
`compute_reserve_address(region, name)`; `compute_list_regions`.

Elisp: `ecloud-compute-list`, `-refresh`, `-ssh`, `-copy-name`,
`-copy-internal-ip`, `-copy-external-ip`, `-start/-stop/-reset/-delete-instance`
(async). IPs: `ecloud-ips-list`, `-refresh`, `-copy-address`, `-copy-name`,
`-reserve`.

## Key Invariants

- Status: `RUNNING` (green), `TERMINATED`/`STOPPED` (gray), else raw.
- Internal IP from `network_interfaces[0].network_ip`; external from
  `access_configs[0].nat_ip`. Machine type is the last path segment.
- Static IPs go into a set to avoid duplicating VM ephemeral IPs; static
  `source="static"`, ephemeral `source="instance"`.
- SSH builds a `gcloud compute ssh ...` command into vterm, honoring
  `ecloud-compute-impersonate-service-account` (→ `--impersonate-service-account`)
  or falling back to `--account` from server config. Impersonation needs the
  Token Creator role.

## Interactions

- [[jsonrpc-dispatcher]] (`compute_*`, error `-32004`). No WebSocket events; ops
  are request/response.

## Gotchas

- Instances and addresses come from AggregatedList APIs (all zones at once); no
  per-zone filtering.
- SSH errors aren't handled by ECloud — they surface in the vterm.

## See Also
- [[rpc-bridge]], [[token-refresh]]
