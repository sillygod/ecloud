---
type: source
spec_path: .kiro/specs/multi-account-support/
related_components: [account-manager]
---

# Spec Summary: Multi-Account Support

> Original: `.kiro/specs/multi-account-support/{requirements,design,tasks}.md`

## Intent

ECloud originally supported a single GCP account: the user manually started the
server and set `GOOGLE_APPLICATION_CREDENTIALS`. This feature lets the user define
multiple account configs in Emacs, auto-start a server process per account, and
switch between them quickly — aimed at people juggling staging/production or
multiple projects.

## Glossary (from the spec)

- **Account_Configuration** — an account name + path to its service-account JSON.
- **Server_Process** — a Python FastAPI backend launched and managed by Emacs.
- **Account_Manager** — the elisp module managing configs and processes.
- **Health_Check** — HTTP check that a server process is up.
- **Port_Allocator** — assigns a free port per process.
- **Process_Registry** — state of all running server processes.

## Key requirements (distilled)

1. **Config management** — define multiple accounts (name → SA JSON path) via
   `ecloud-accounts`.
2. **Process lifecycle** — start/stop/restart per account, with health-gated
   startup and sentinels.
3. **Port allocation** — a pool (default 8765–8774), with OS-level free checks.
4. **Switching** — change the active account; all RPC transparently re-targets.
5. **Backward compatibility** — a bare `GOOGLE_APPLICATION_CREDENTIALS` or a
   custom `ecloud-server-url` still works as a single/external account.
6. **Resilience** — recover from connection errors via restart-then-retry, gated
   on `/health`.

## As built

See [[account-manager]] (component), [[multi-account-process-model]] (concept),
and [[002-process-per-account]] (decision). The implementation matches the spec;
credentials are bound per process via the environment, never sent over RPC.
