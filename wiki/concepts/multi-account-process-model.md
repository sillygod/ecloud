---
type: concept
applies_to: [account-manager, rpc-bridge, jsonrpc-dispatcher]
decisions: [002-process-per-account]
---

# Multi-Account Process Model

How ECloud manages several GCP accounts at once: **one server OS process per
account**, each isolated by its own credentials and port.

## The model

```
Emacs
 ├─ account 'staging'    ──▶ server proc :8765  (GOOGLE_APPLICATION_CREDENTIALS=staging.json)
 ├─ account 'production' ──▶ server proc :8766  (GOOGLE_APPLICATION_CREDENTIALS=prod.json)
 └─ ...                                          (port pool 8765–8774)
```

- Each process is launched with its own `GOOGLE_APPLICATION_CREDENTIALS`. The
  Google SDKs pick it up automatically; `config.py` reads it at import.
- **Credentials never travel over RPC.** A request carries no account/credential
  params — the target server *is* the account.
- Switching accounts just changes which URL `ecloud-rpc--get-current-url()`
  returns. Every domain view then transparently targets that server.

## Why processes, not threads/contexts

Google client singletons and the K8s connection hold per-account state that's
awkward to multiplex inside one process. A process boundary gives clean isolation
(credentials, connection, gRPC DNS config) for free. See
[[002-process-per-account]].

## Lifecycle

- Start is **health-gated**: not "running" until `GET /health` returns 200.
- A sentinel updates `:status` and releases the port on exit.
- Connection errors are recovered by restart-then-retry — but only after a failed
  `/health` confirms the server is actually down. See
  [[006-health-check-before-restart]].
- `ecloud-account-manager` is a **stateful module**: `reload-ecloud` skips it
  unless forced, so a code reload doesn't orphan running servers.

## Gotchas

- Port checks use `lsof`; SIGKILL of Emacs orphans servers (no `kill-emacs-hook`).
- `external`-type accounts (a custom `ecloud-server-url`) aren't process-managed.

## See Also
- [[account-manager]], [[rpc-bridge]], [[002-process-per-account]], [[account-switch]]
