---
type: source
spec_path: .kiro/specs/helm-and-transient-ui/
related_components: [helm, kubernetes]
---

# Spec Summary: Helm & Transient UI

> Original: `.kiro/specs/helm-and-transient-ui/{requirements,design,tasks}.md`

## Intent

Two enhancements bundled together: (1) add Kubernetes **Helm** management, and
(2) replace scattered standalone commands with a unified **Transient menu**
entry point.

## Glossary (from the spec)

- **Helm_Client** — backend module that talks to Helm.
- **Release** — an installed application instance; **Chart** — a Helm package;
  **Chart_Repository** — a remote chart store.
- **Transient_Menu** — Emacs Transient hierarchical menu system.
- **Kubeconfig** — cluster auth config (Helm needs one built from the GKE
  connection).

## Key requirements (distilled)

1. **Helm release management** — list releases with basic info (name, namespace,
   chart, version, status); describe, install, upgrade, rollback, uninstall.
2. **Chart repositories** — list/add/remove repos, search charts.
3. **Auth** — build a temporary kubeconfig from the connected cluster's
   credentials.
4. **Transient UI** — a unified `ecloud-menu` and per-view `?` help popups,
   replacing ad-hoc command discovery and `message`-based help.

## As built

- Helm: [[helm]] (component). pyhelm3 for releases, `helm` CLI for repos; temp
  kubeconfig from the [[kubernetes]] connection.
- Transient UI: [[ui-conventions]] (concept) and [[005-transient-menus]]
  (decision); the `?`→transient conversion landed in commit `4fd9e25`.
