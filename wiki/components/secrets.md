---
type: component
elisp_files:
  - emacs/ecloud-secrets.el
server_files:
  - server/secret_manager_client.py
rpc_methods:
  - secret_manager_list_secrets
  - secret_manager_access_version
  - secret_manager_create_secret
  - secret_manager_add_version
  - secret_manager_delete_secret
depends_on: [rpc-bridge, jsonrpc-dispatcher]
depended_by: []
last_verified: 2026-06-11
---

# Secret Manager

List, read, create, version, and delete GCP secrets. Built so that secret
payloads are never echoed, logged, or kept in command history.

## What It Does

- `ecloud-secrets.el` (343) — secret list, access (read into a read-only buffer),
  create, add version, delete.
- `secret_manager_client.py` (249) — Secret Manager (secretmanager_v1) operations.

## Public API

`secret_manager_list_secrets` → `SecretInfo` (name, full_name, create_time,
labels, replication); `secret_manager_access_version(name, version="latest")`
(returns UTF-8 payload, or hex with `encoding="binary"` if not decodable);
`secret_manager_create_secret(name, payload, labels)` (creates secret + initial
version); `secret_manager_add_version(name, payload)`;
`secret_manager_delete_secret(name)` (deletes the secret and all versions).

Elisp: `ecloud-secrets-list`, `ecloud-secrets-access[-at-point]`,
`ecloud-secrets-create`, `ecloud-secrets-add-version[-at-point]`,
`ecloud-secrets-delete[-at-point]`. Transient: `ecloud-secrets-menu`.

## Key Invariants

- ⚠️ **Payload input always uses `read-passwd`** — hidden, not echoed, not stored
  in history. Never print a payload to the minibuffer or `*Messages*`.
- ⚠️ **Payloads render only in a read-only buffer** with a header showing
  `name@version`, size, and timestamp.
- ⚠️ **`ecloud-secrets-mode-map` is defined before `define-derived-mode`** — the
  ordering bug that shadowed it was fixed in commit `c0f2f10`.
- ⚠️ **`GRPC_DNS_RESOLVER=native` set in `secret_manager_client.py`.** See
  [[004-grpc-native-dns-resolver]].
- Delete is permanent (no soft delete).

## Interactions

- [[jsonrpc-dispatcher]] (`secret_manager_*`, error `-32010`). No WebSocket
  events — operations are request/response.
- The active account is shown in the mode-line (lock icon) so it's clear *whose*
  secrets are on screen. See [[account-manager]].

## Gotchas

- Binary payloads come back as hex; check `encoding` before treating as text.
- The completion cache (`ecloud-secrets--secrets`) is buffer-local; re-run
  `ecloud-secrets-list` to refresh candidates.

## See Also
- [[account-manager]], [[rpc-bridge]], [[004-grpc-native-dns-resolver]]
