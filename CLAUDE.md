# CLAUDE.md — ECloud

Hard rules and workflow for working in this repo. Read this first, then
`wiki/schema.md`, then `wiki/index.md`.

## What this is

ECloud manages Google Cloud Platform from inside Emacs. Two tiers:

- **Front-end** (`emacs/*.el`): tabulated-list views per GCP service, talking to
  the server via JSON-RPC.
- **Server** (`server/*.py`): a FastAPI app exposing a JSON-RPC endpoint that
  wraps the GCP SDKs, plus a WebSocket for async push (logs, exec, progress).

```
Emacs (elisp) ──JSON-RPC/HTTP──▶ FastAPI (jsonrpc_handler) ──▶ GCP SDKs
              ◀──WebSocket push──
```

## Hard Rules

1. **`GRPC_DNS_RESOLVER=native` is set before any Google import** (`server/main.py`
   line 9). Never move imports above it, never remove it. It fixes VPN/c-ares DNS
   failures resolving `googleapis.com`. See `wiki/decisions/004-grpc-native-dns-resolver.md`.
2. **K8s/GCP calls go through `@auto_refresh_token` + `_ensure_connected()`.**
   Don't bypass the connection gate or the 401-retry path.
   See `wiki/concepts/token-refresh.md`.
3. **Never echo or log secret payloads.** Secret Manager input always uses
   `read-passwd`; payloads render in read-only buffers only.
   See `wiki/components/secrets.md`.
4. **Define `ecloud-<domain>-mode-map` before `define-derived-mode`** or it gets
   shadowed by the macro's empty keymap.
5. **RPC method names are snake_case and registered in
   `JsonRpcHandler._register_methods()`.** Adding a method means: server handler +
   registry entry + elisp `ecloud-rpc-*` wrapper.
6. **Don't restart a server on RPC timeout without probing `/health` first** —
   slow ops (pod exec) must not trigger false restarts.
   See `wiki/decisions/006-health-check-before-restart.md`.
7. **Commit only relevant files.** Pre-existing untracked files (`.kiro/`, test
   scripts) are not yours — don't `git add -A` them in.

## Session-End Sync (do this after changing source)

1. Check `wiki/` pages whose `elisp_files` / `server_files` / `rpc_methods`
   frontmatter overlaps your changes — update Key Invariants, Public API, and
   Gotchas as needed.
2. If you changed an RPC method set, reconcile the component page's method table
   with `_register_methods()`.
3. Show all proposed doc/wiki edits to the user for approval before applying.
4. Append an entry to `wiki/changelog.md`.

Full protocol: `wiki/schema.md` § Sync Protocol.

## Where things live

A structured wiki lives in `wiki/` for onboarding and architectural reference:

1. `wiki/schema.md` — how the wiki works and the sync protocol
2. `wiki/index.md` — find what you need
3. `wiki/components/` — one page per service domain (elisp + server client)
4. `wiki/concepts/` — cross-cutting patterns (RPC bridge, websocket events, multi-account)
5. `wiki/decisions/` — ADRs
6. `wiki/flows/` — end-to-end request/event flows
7. `wiki/conventions.md` — style, RPC naming, faces, reload workflow
8. `wiki/sources/` — distilled summaries of `.kiro/specs/`

Source code is the single source of truth. `.kiro/specs/` and `wiki/` are
navigational aids.

## Dev basics

- Run the server: from `server/`, `uv run uvicorn main:app --reload` (or
  `python main.py`). Needs `GOOGLE_APPLICATION_CREDENTIALS` pointing at a service
  account JSON. Default bind `127.0.0.1:8765`.
- Reload elisp without restarting Emacs: `M-x reload-ecloud`.
- Co-author trailer for commits:
  `Co-Authored-By: Claude Opus 4.8 (1M context) <noreply@anthropic.com>`
