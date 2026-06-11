---
type: decision
status: accepted
date: 2026-06-11
related_components: [kubernetes, secrets]
---

# 005 — Transient popups for help and menus

> Status: accepted · see commit `4fd9e25` and `.kiro/specs/helm-and-transient-ui/`

## Context

Help was originally a `message`/docstring dump per view, and there was no unified
launcher. Discoverability was poor and the help text scrolled away.

## Decision

Use Magit's **transient** library: a top-level `ecloud-menu` plus submenus, and
convert every view's `?` into a `transient-define-prefix ecloud-D-help` popup
(commit `4fd9e25`).

## Why

- Transient popups are persistent, grouped, and show key + description + dynamic
  state (e.g. the active account / connected cluster in the title).
- Consistent muscle memory across all views.
- It's the de-facto Emacs idiom for this (Magit, etc.).

## Consequences

- Requires Emacs 28.1+ (or the `transient` package); guarded by
  `ecloud-transient--check-availability`.
- Menu/help definitions live in `ecloud-transient.el` and per-view `…-help`
  prefixes — keep them in sync with the keymaps.

## See Also
- [[ui-conventions]], [[helm-and-transient-ui]]
