# Wiki Changelog

Append-only log of changes to **the wiki** (not the code — code history lives in
git). Newest first. One entry per sync/edit session.

## 2026-06-25

- **Documented the evil-binding load-order convention.** `ecloud-secrets`,
  `ecloud-cloud-run`, and `ecloud-scheduler` were installing evil motion-state
  keys inside the keymap `defvar` behind `(fboundp 'evil-define-key*)`, which
  no-ops when ecloud loads before evil — keys only worked after `reload-ecloud`.
  Moved them to `with-eval-after-load 'evil` + `evil-set-initial-state` to match
  the other 7 modes. Added the convention to [[conventions]] and a gotcha to
  [[secrets]].

## 2026-06-11

- **Moved existing top-level docs into the wiki** (`git mv`, history preserved):
  - `MULTI_ACCOUNT_GUIDE.md` → `wiki/guides/multi-account.md`
  - `SERVICE_ACCOUNT_SETUP.md` → `wiki/guides/service-account-setup.md`
  - `SERVICE_USAGE_QUICKSTART.md` → `wiki/guides/service-usage-quickstart.md`
  - `SERVICE_USAGE_IMPLEMENTATION.md` → `wiki/reports/service-usage-implementation.md`
  - `TRANSIENT_MENU_VERIFICATION.md` → `wiki/reports/transient-menu-verification.md`
  Added `guide` and `report` page types to [[schema]] (reports are exempt from
  sync), registered all five in [[index]], cross-linked from the related
  component/concept pages, and updated the `README.md` link. `README.md` stays at
  the repo root.
- **Wiki bootstrapped.** Created the full structure modeled on the `jabor`
  project's wiki: [[schema]], [[index]], [[conventions]], onboarding guide, 13
  component pages (core + service domains), 5 concepts, 6 ADRs, 3 flows, and
  `sources/` summaries of `.kiro/specs/`.
- Added repo-root `CLAUDE.md` with hard rules, reading order, and a pointer to
  the sync protocol.
- Pages verified against source code as of commit `9dc689e`
  (pod CPU/memory columns from metrics-server).
