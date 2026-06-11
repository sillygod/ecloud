---
type: source
---

# Sources Index

`.kiro/specs/` holds the original Kiro spec docs for ECloud's larger features —
the **canonical intent**. The pages here are distilled summaries that link the
specs into the wiki. When code and spec disagree, code wins (see
[[schema]] § Truth Hierarchy).

## Specs

| Spec | `.kiro/specs/` path | Summary |
|------|---------------------|---------|
| Multi-account support | `.kiro/specs/multi-account-support/` | [[multi-account-support]] |
| Helm & Transient UI | `.kiro/specs/helm-and-transient-ui/` | [[helm-and-transient-ui]] |

Each spec directory contains `requirements.md` (user stories / acceptance
criteria), `design.md` (architecture), and `tasks.md` (implementation breakdown).

## How to use these

- Read the wiki summary first for orientation.
- Open the `.kiro/specs/` original when you need the full acceptance criteria or
  the task-level breakdown.
- If you find the spec no longer matches the code, note it during a sync; only
  edit the spec on explicit request (it's a historical record of intent).
