# ECloud Wiki Schema

## Purpose

This wiki is a structured knowledge base for **ECloud** — an Emacs front-end for
managing Google Cloud Platform, talking to a Python FastAPI server over JSON-RPC.

It serves two audiences:

1. **Human developers** onboarding to the project or reviewing architecture.
2. **AI coding agents** (Claude Code, etc.) that need to understand the
   architecture and its invariants before making changes.

## Reading Order for New Sessions

1. **`CLAUDE.md`** (repo root) — hard rules and dev workflow (always read first)
2. **`wiki/schema.md`** (this file) — how the wiki is organized
3. **`wiki/index.md`** — find relevant pages by topic
4. **Relevant pages** — read the component/concept/decision pages you need

## The Two Tiers

ECloud is a two-tier system, and almost every feature spans both:

```
Emacs (elisp)  ──JSON-RPC over HTTP──▶  FastAPI server (Python)  ──▶  GCP SDKs
   UI modules   ◀──WebSocket push────   jsonrpc_handler + clients
```

A "component" page therefore usually documents a **service domain** as a pair:
the elisp UI module (`emacs/ecloud-<domain>.el`) and its server client
(`server/<domain>_client.py`), described together because they are two halves of
one feature.

## Relationship to `.kiro/specs/` and `wiki/sources/`

- `.kiro/specs/` holds the original Kiro spec docs (`requirements.md`,
  `design.md`, `tasks.md`) for larger features. These are the **canonical
  intent** for what was planned.
- `wiki/sources/` contains distilled summaries of those specs, cross-linked into
  the wiki. When a spec is large, read the `wiki/sources/` summary first, then
  the `.kiro/specs/` original if you need detail.
- The rest of `wiki/` is distilled, cross-referenced knowledge derived from
  `.kiro/specs/` and the source code.
- **When they conflict**, source code wins, then `.kiro/specs/`, then wiki pages.

## Page Types

| Type | Directory | Purpose |
|------|-----------|---------|
| `component` | `components/` | One page per service domain — what it does, key RPC methods, invariants, gotchas |
| `concept` | `concepts/` | Cross-cutting design ideas used by multiple components |
| `decision` | `decisions/` | Architecture Decision Records (ADR) — why we chose A over B |
| `flow` | `flows/` | End-to-end request/event flows through the system |
| `onboarding` | `onboarding/` | Quick-start guides for new humans and agents |
| `source` | `sources/` | Distilled summaries of `.kiro/specs/`, linking back to the originals |
| `guide` | `guides/` | End-user how-to docs (setup, usage, migration). Hand-written, not auto-derived. |
| `report` | `reports/` | Point-in-time snapshots (implementation summaries, verification records). **Exempt from the sync protocol** — see below. |

### Reports are point-in-time and not synced

Pages under `reports/` capture the state of the world on the day they were
written. They intentionally **do not** participate in the sync protocol.

- Do not update a report to "match current code." If it's outdated, write a new
  one and link it from the old.
- Only edit an existing report if the user explicitly asks (e.g. typo fix).
- Reports carry `exempt_from_sync: true` in frontmatter.

### Guides are hand-written

Pages under `guides/` are end-user documentation (e.g. service-account setup,
multi-account usage). They are authoritative for *how to use/configure* ECloud;
the wiki's component pages stay focused on *how it works*. A guide should link to
its related component page(s) and vice versa, but the two should not duplicate
each other's content.

## Frontmatter Conventions

Every page has YAML frontmatter. Required fields by type:

```yaml
# Component
---
type: component
elisp_files: [emacs/ecloud-k8s.el]
server_files: [server/k8s_client.py, server/k8s_log_streamer.py]
rpc_methods: [k8s_list_pods, k8s_connect]      # method names registered in jsonrpc_handler
depends_on: [rpc-bridge, jsonrpc-dispatcher]
depended_by: [helm]
spec_refs:                                       # links back to .kiro/specs (optional)
  - .kiro/specs/helm-and-transient-ui/design.md
last_verified: 2026-06-11
---

# Concept
---
type: concept
applies_to: [rpc-bridge, account-manager]
decisions: [001-jsonrpc-over-http]
---

# Decision (ADR)
---
type: decision
status: accepted          # proposed | accepted | superseded | deprecated
date: 2026-XX-XX
related_components: [rpc-bridge, jsonrpc-dispatcher]
---
```

## Cross-References

Use `[[wikilinks]]` to reference other wiki pages by their filename (no
extension):

- `[[kubernetes]]` → `wiki/components/kubernetes.md`
- `[[jsonrpc-bridge]]` → `wiki/concepts/jsonrpc-bridge.md`
- `[[001-jsonrpc-over-http]]` → `wiki/decisions/001-jsonrpc-over-http.md`

## When to Update the Wiki

- ✅ After adding a new RPC method or domain → update the affected component page
- ✅ After making an architectural decision → create a new ADR
- ✅ After discovering a non-obvious gotcha → add it to the component's Gotchas
- ❌ Do NOT update speculatively — only document what exists in code
- ❌ Do NOT edit old ADRs — create a new one that supersedes

## Sync Protocol: Keeping Docs and Code in Sync

**Source code is the single source of truth.** Both `.kiro/specs/` and wiki pages
can drift from it. The LLM's job is to detect drift and propose corrections.

### Truth Hierarchy

```
Source Code         ← Ground truth. Always wins.
    ↓ sync
.kiro/specs/        ← Intent / design docs. Should match what was built.
    ↓ sync
wiki/sources/       ← Distilled spec summaries.
    ↓ sync
wiki pages          ← Navigational layer. Derived from code + specs.
```

When conflicts are found:
- **Code ≠ `.kiro/specs/`** → note the drift; update the spec only if the user asks
  (specs are also a historical record of intent).
- **Code ≠ wiki pages** → update wiki pages to match code.
- **`.kiro/specs/` ≠ wiki pages** → update wiki pages.

### Mode 1: Session-End Sync (after every dev session)

At the end of a session where source code was modified:

```
請做 session-end sync：
1. 比對 git diff 找出改了哪些源碼
2. 檢查 wiki/ 裡受影響的頁面是否需要更新（用 frontmatter 的 elisp_files / server_files / rpc_methods 比對）
3. 檢查 .kiro/specs/ 裡相關設計是否已過時
4. 列出所有差異，我確認後再改
```

The agent should:
1. Run `git diff --name-only` to find changed source files.
2. Match changed files against wiki pages' `elisp_files` / `server_files` frontmatter.
3. If RPC methods changed, check the component page's Public API table and
   `rpc_methods` frontmatter against the `_register_methods()` registry in
   `server/jsonrpc_handler.py`.
4. Check that Key Invariants and Gotchas are still accurate.
5. Propose specific edits (user approves before applying).
6. If any wiki pages were created/updated, append an entry to `wiki/changelog.md`.

### Mode 2: Full Audit (monthly, or after major changes)

```
執行 full audit：
1. 掃描 wiki/ 所有頁面的 elisp_files / server_files / rpc_methods，比對源碼現狀
2. 找出 stale / missing / drifted 的頁面
3. 分組列出，我確認後再改
```

### Mode 3: Targeted Sync (after modifying a specific module)

```
我改了 server/cloud_run_client.py，請同步 wiki/ 裡相關的內容
```

### What the LLM CAN and CANNOT Touch

| Layer | LLM Can Edit? | Rule |
|-------|--------------|------|
| Source code (`emacs/`, `server/`) | ✅ 正常開發 | Standard dev workflow |
| `.kiro/specs/` | ⚠️ 謹慎 | Historical intent. Only update on explicit request. |
| `wiki/components/`, `concepts/`, `flows/` | ✅ 同步 | Must verify against source code before updating |
| `wiki/decisions/` | ⚠️ 不改舊的 | Create a new ADR that supersedes, don't edit old |
| `wiki/sources/` | ✅ 同步 | Keep summaries matching `.kiro/specs/` |
| `CLAUDE.md` | ⚠️ 謹慎 | Only add/update rules, never remove without discussion |

### What Gets Checked

| Check | Scope | How to Detect |
|-------|-------|---------------|
| RPC method drift | `wiki/` | Component page's method table vs `_register_methods()` in `jsonrpc_handler.py` |
| API drift | `wiki/` | Wiki's Public API vs actual `def`/`defun` signatures |
| Invariant violation | `wiki/` | Committed code contradicts a Key Invariant |
| Missing component | `wiki/` | New `*_client.py` or `ecloud-*.el` with no wiki page |
| Stale component | `wiki/` | Wiki page references deleted or renamed files |
| Missing ADR | `wiki/` | Major architectural change with no decision record |

### Freshness Tracking

Each component page has `last_verified` in frontmatter. During audits, update it
to today's date if the page is confirmed accurate. Pages without a recent
`last_verified` are audit priorities.

## Rules for AI Agents

1. Before modifying any source file, check if a component page exists for it.
2. Read the **Key Invariants** section — these are hard constraints.
3. If your change could violate an invariant, **STOP and discuss with the user**.
4. After completing work, suggest updates to affected wiki pages.
5. Do NOT auto-generate wiki content without verifying against source code.
6. When performing a sync, always show the diff to the user before applying.
