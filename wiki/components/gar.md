---
type: component
elisp_files:
  - emacs/ecloud-gar.el
server_files:
  - server/gar_client.py
rpc_methods:
  - gar_list_repos
  - gar_list_locations
  - gar_list_packages
  - gar_list_tags
  - gar_delete_package
  - gar_delete_tag
  - gar_create_tag
  - gar_pull
  - gar_push
  - gar_tag
depends_on: [rpc-bridge, jsonrpc-dispatcher]
depended_by: []
last_verified: 2026-06-11
---

# Artifact Registry (GAR)

Browse Docker repositories, packages (images), and tags hierarchically; pull,
push, tag, and delete via the local `docker` CLI.

## What It Does

- `ecloud-gar.el` (392) — hierarchical browser (repos → packages → tags) with
  breadcrumb navigation; pull/tag/delete.
- `gar_client.py` (321) — Artifact Registry API + `docker` subprocess calls.

## Public API

`gar_list_locations`, `gar_list_repos(location)` (`location="all"` iterates
asia/us/europe + discovered regions), `gar_list_packages(repo)` → `ImageInfo`,
`gar_list_tags(package)` → `TagInfo`, `gar_create_tag(package, tag_id, version)`,
`gar_delete_package` / `gar_delete_tag`, `gar_pull(uri)` / `gar_push(uri)` /
`gar_tag(source, target)`.

Elisp: `ecloud-gar-browse(location)`, `ecloud-gar-enter`, `-up`, `-refresh`,
`-pull`, `-tag`, `-delete`. Navigation state in `ecloud-gar--current-repo` /
`--current-package` / `--navigation-stack`.

## Key Invariants

- ⚠️ **Docker required.** `_check_docker()` verifies the `docker` binary before
  pull/push/tag.
- ⚠️ **`GRPC_DNS_RESOLVER=native` set in `gar_client.py`** (Mac/VPN DNS). See
  [[004-grpc-native-dns-resolver]].
- URI format: `{location}-docker.pkg.dev/{project}/{repo}/{image}:{tag}`.
- Package names are URL-encoded in the API; decoded with `url-unhex-string` for
  display.
- `location="all"` does not use a wildcard — it iterates regions; per-region
  failures are silently skipped.

## Interactions

- [[jsonrpc-dispatcher]] (`gar_*`, error `-32003`).
- WebSocket: `gar_pull_started/finished`, `gar_push_started/finished`,
  `gar_package_deleted`, `gar_tag_deleted` → `ecloud-gar-event-hook`. See
  [[websocket-events]].

## Gotchas

- Tag creation needs a version resource name (ending in a digest), not a bare
  digest string.
- Package deletion is a long-running op; the client waits on `operation.result()`.

## See Also
- [[rpc-bridge]], [[websocket-events]], [[004-grpc-native-dns-resolver]]
