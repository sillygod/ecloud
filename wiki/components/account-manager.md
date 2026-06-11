---
type: component
elisp_files:
  - emacs/ecloud-account-manager.el
server_files:
  - server/config.py
rpc_methods: []
depends_on: [rpc-bridge]
depended_by: [rpc-bridge]
spec_refs:
  - .kiro/specs/multi-account-support/design.md
  - .kiro/specs/multi-account-support/requirements.md
last_verified: 2026-06-11
---

# Account Manager (multi-account)

Lets one Emacs session manage multiple GCP accounts at once. Each account gets
its **own server OS process** on its own port with its own service-account
credentials. Switching accounts re-points the RPC URL — no credentials ever
travel over the wire.

## What It Does

- `ecloud-account-manager.el` (2421) — account config parsing, process lifecycle
  (start/stop/restart), port allocation, health-gated startup, process registry,
  mode-line display, connection-error recovery.
- `server/config.py` — each process reads its own `GOOGLE_APPLICATION_CREDENTIALS`
  from the environment it was launched with.

## Data Model

```elisp
;; user config: account-name → service-account JSON path
(defcustom ecloud-accounts
  '((staging    . "/path/to/staging.json")
    (production . "/path/to/prod.json")))

;; runtime registry: account-name → process-info plist
ecloud-account--processes   ; ((name . (:process P :port N :status S
                            ;           :start-time T :service-account PATH
                            ;           :buffer B :url U :ws-url W)))
```

`:status` ∈ `starting | running | stopped | error`. Port pool default
`ecloud-port-range` = `(8765 . 8774)` (10 concurrent accounts).

## Public API (key commands)

| Command / function | Description |
|--------------------|-------------|
| `ecloud-account-switch(name)` | Switch active account (auto-starts its server if needed) |
| `ecloud-account-connect(name)` | Start the server process for an account |
| `ecloud-account-disconnect(name)` | Stop it |
| `ecloud-account-restart(name)` | Disconnect + reconnect |
| `ecloud-account-current()` | Active account symbol |
| `ecloud-account-list()` | All configured account names |
| `ecloud-account-status(name)` | The process-info plist |
| `ecloud-account-list-processes()` | Tabular `*ecloud-accounts*` view |
| `ecloud-account-stop-all()` | Stop every running server (Emacs exit) |
| `ecloud-account-init()` | One-time startup init (port pool, migration hints) |
| `ecloud-account--get-url(name)` | URL used by [[rpc-bridge]] to route requests |

Config resolution order (`ecloud-account--parse-config`): managed
`ecloud-accounts` → a non-default custom `ecloud-server-url` (type `external`,
not process-managed) → bare `GOOGLE_APPLICATION_CREDENTIALS` (a single `default`
account). This preserves single-account backward compatibility.

## Key Invariants

- ⚠️ **Credentials are per-process, never per-request.** A server is launched with
  `GOOGLE_APPLICATION_CREDENTIALS=<expanded path>`; Google libs pick it up
  automatically. No RPC param carries credentials.
- ⚠️ **Startup is health-gated.** A process is not "running" until `GET /health`
  returns 200 (a few retries). Failure → kill + report.
- ⚠️ **Switching account = switching URL.** `ecloud-rpc--get-current-url()` reads
  the active account; all domain views then transparently target that server. See
  [[rpc-bridge]].
- ⚠️ **Connection-error recovery is health-gated too.** On a failed RPC + failed
  `/health`, `ecloud-account--handle-connection-error` restarts the server and
  retries once. A healthy `/health` means *don't* restart (the op was just slow).
  See [[006-health-check-before-restart]].
- ⚠️ **`ecloud-account-manager` is a stateful module:** `reload-ecloud` skips it
  unless given a prefix arg, to avoid orphaning running server subprocesses.
- ⚠️ Service-account JSON is validated (exists, readable, has `client_email` +
  `project_id`) before launch.

## Interactions

- Drives [[rpc-bridge]]: provides the URL, and the restart/retry path.
- The active account is shown in the mode-line and in the [[ui-conventions]]
  transient menu title.
- Related lock-icon / secrets mode-line work landed alongside the
  `ecloud-secrets-mode-map` fix (commit `c0f2f10`) — see [[secrets]].

## Gotchas

- Port allocation uses `lsof` for an OS-level free check (macOS/Linux); without
  `lsof` it falls back to the in-memory pool only.
- Hard-killing Emacs (SIGKILL) skips `kill-emacs-hook`, orphaning server
  processes that hold ports until the OS reclaims them.
- `external`-type accounts (custom `ecloud-server-url`) are not process-managed
  and won't be auto-restarted on failure.
- Account names are **symbols** and case-sensitive (`staging` ≠ `Staging`).

## See Also

- [[multi-account-process-model]] — the design rationale and process model
- [[002-process-per-account]] — the decision record
- [[rpc-bridge]], [[006-health-check-before-restart]]
- [[multi-account-support]] — distilled spec
- [[multi-account]] — end-user setup/usage guide
- [[service-account-setup]] — creating the GCP service account
