---
type: concept
applies_to: [kubernetes, cloud-run, scheduler, compute, sql, gar, gcs, secrets, service-usage]
decisions: [005-transient-menus]
---

# UI Conventions (views, menus, notifications)

Every Emacs view in ECloud is built the same way, so once you know one you know
them all.

## Tabulated-list views

Each domain `D` provides:

- `ecloud-D-list` — entry command: create `*ECloud-D*`, enter the mode, refresh,
  switch to it.
- `ecloud-D-mode` — `define-derived-mode` from `tabulated-list-mode`.
- `ecloud-D-mode-map` — defined **before** the derived-mode (else shadowed).
- `ecloud-D--refresh-data` — sets `tabulated-list-format` / `-entries`, then
  `tabulated-list-init-header` + `tabulated-list-print`; hooked onto
  `tabulated-list-revert-hook`.
- `ecloud-D-refresh` — interactive wrapper.
- Faces `ecloud-D-<element>-face` for column coloring.

Common keys: `?` help, `q` quit-window, `g`/`r` refresh, `RET` enter/act, plus
domain-specific keys (`c` copy, `d`/`D` delete, etc.). Evil users get `motion`
state with mirrored bindings.

## Transient menus & help

`ecloud-transient.el` defines the top-level `ecloud-menu` (title shows the active
account) and submenus (`ecloud-k8s-menu`, `ecloud-secrets-menu`). Each view's `?`
is a `transient-define-prefix ecloud-D-help` — the old `message`-style help
strings were converted to transient popups in commit `4fd9e25`. Transient needs
Emacs 28.1+ (`ecloud-transient--check-availability`). See [[005-transient-menus]].

## Notifications (`ecloud-notify.el`)

Posframe toasts that stack: `ecloud-notify`, `-info`, `-success`, `-warning`,
`-error` (errors are sticky by default). Everything is also written to
`*ecloud-notify-log*` first, so a posframe failure never loses the message.

⚠️ **Orphan-posframe cleanup on reload:** `reload-ecloud` resets bookkeeping but
child frames persist. `ecloud-notify-dismiss-all` sweeps frames/buffers by the
` *ecloud-notify*` name prefix, and `ecloud-notify-unload-function` runs it on
unload (commit `1d0ae0e`).

## Entry point & reload

`ecloud.el` declares the package (requires `posframe`, `websocket`, `transient`),
loads modules in dependency order, autoloads the entry commands, and defers
WebSocket auto-connect via a 2 s idle timer. `reload-ecloud.el` reloads modules
in-place — see [[conventions]] § Reload Workflow.

## See Also
- [[conventions]], [[005-transient-menus]], [[websocket-events]]
- [[transient-menu-verification]] — point-in-time command-mapping verification
