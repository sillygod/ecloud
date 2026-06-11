---
type: component
elisp_files:
  - emacs/ecloud-browser.el
  - emacs/ecloud-commands.el
server_files:
  - server/gcs_client.py
rpc_methods:
  - list_buckets
  - list_objects
  - download_object
  - upload_object
  - delete_object
  - batch_delete_objects
  - create_folder
  - generate_presigned_url
  - copy_object
  - move_object
  - update_object_metadata
  - set_bucket_lifecycle
depends_on: [rpc-bridge, jsonrpc-dispatcher]
depended_by: []
last_verified: 2026-06-11
---

# Cloud Storage (GCS)

A bucket/object browser: navigate buckets and virtual folders, upload/download/
delete, copy/move, create folders, and generate presigned URLs.

## What It Does

- `ecloud-browser.el` (482) — the hierarchical GCS browser (buckets → objects).
- `ecloud-commands.el` (148) — cross-domain file commands
  (`ecloud-download-file`, `-upload-file`, `-upload-buffer`, `-delete-object`,
  `ecloud-copy-gs-url`, `ecloud-server-status`).
- `gcs_client.py` (484) — all storage operations.

## Public API

`list_buckets` → `BucketInfo`; `list_objects(bucket, prefix, delimiter)` →
(`ObjectInfo[]`, virtual folders); `download_object` / `upload_object` /
`delete_object` / `batch_delete_objects`; `create_folder` (empty trailing-slash
object); `generate_presigned_url(bucket, object, expiration_seconds, method)` (v4);
`copy_object` / `move_object` (copy+delete); `update_object_metadata`;
`set_bucket_lifecycle`.

Elisp browser: `ecloud-browse`, `ecloud-browser-{enter,up,refresh,download,upload,
delete,create-folder,copy-path,copy-object,move-object,generate-presigned-url}`.
State: `ecloud-browser--current-bucket` (nil = bucket list),
`ecloud-browser--current-prefix`.

## Key Invariants

- Folders are virtual: `delimiter="/"` synthesizes folder rows; a "folder" is an
  empty object whose name ends in `/`.
- Presigned URLs are v4, signed with the current service-account credentials.
- `batch_delete_objects` is sequential (per-object error reporting) rather than a
  single batch call.

## Interactions

- [[jsonrpc-dispatcher]] (GCS verbs, error `-32001`).
- WebSocket: `gcs_download_started/finished`, `gcs_upload_started/finished`,
  `gcs_delete_finished` → `ecloud-gcs-event-hook`. See [[websocket-events]].

## Gotchas

- The same buffer renders both the bucket list and the object list; the
  tabulated-list format switches based on `--current-bucket`.
- Object rows carry full paths; the display name is the path minus the prefix.
- No custom signer email for presigned URLs — always the process credentials.

## See Also
- [[rpc-bridge]], [[websocket-events]], [[service-usage]] (also reached from `ecloud-services.el`)
