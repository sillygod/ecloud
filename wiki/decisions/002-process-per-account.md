---
type: decision
status: accepted
date: 2026-06-11
related_components: [account-manager]
---

# 002 — One server OS process per GCP account

> Status: accepted · see `.kiro/specs/multi-account-support/`

## Context

Users manage multiple GCP accounts/projects. Each account has distinct
credentials, and the server's GCP client singletons + the K8s connection hold
per-account state. We needed isolation without a credential-multiplexing rewrite.

## Decision

Launch **one FastAPI process per account**, each with its own
`GOOGLE_APPLICATION_CREDENTIALS` and its own port (pool 8765–8774). The active
account is just the URL `ecloud-rpc--get-current-url()` resolves to.

## Why

- Clean isolation of credentials, client singletons, the K8s connection, and even
  the `GRPC_DNS_RESOLVER` setting — all per process, for free.
- No per-request credential plumbing; the server *is* the account, so existing
  single-account code paths keep working unchanged.
- Switching accounts is just re-pointing a URL; domain views need zero awareness.

## Consequences

- Process lifecycle to manage: start (health-gated), stop, restart, port
  allocation, sentinels, orphan cleanup — all in `ecloud-account-manager.el`.
- Memory cost scales with the number of running accounts.
- SIGKILL of Emacs can orphan server processes (ports held until OS reclaim).
- `ecloud-account-manager` must be a stateful module excluded from normal
  `reload-ecloud`.

## Alternatives rejected

- **One process, switch credentials per request:** would require threading account
  context through every handler and rebuilding clients constantly.
- **Thread/async context per account:** Google singletons and the K8s client make
  this brittle.

## See Also
- [[multi-account-process-model]], [[account-manager]], [[multi-account-support]]
