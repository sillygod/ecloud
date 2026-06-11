---
type: decision
status: accepted
date: 2026-06-11
related_components: [rpc-bridge, jsonrpc-dispatcher, kubernetes, service-usage]
---

# 003 — WebSocket push for streaming and long operations

> Status: accepted · documented retroactively from the codebase

## Context

Some results don't fit request/response: pod logs and interactive exec are
open-ended streams; "list all disabled services" is 10k+ rows; deploys/deletes
finish after the initial call returns. Polling would be wasteful and laggy.

## Decision

Add a single **WebSocket** (`/ws`) as a server→Emacs push channel. Handlers return
a quick ack, then `broadcast({type, data})`. The elisp client
(`ecloud-ws--handle-message`) fans messages out by `type` prefix to per-domain
event hooks.

## Why

- One connection multiplexes every domain's async events (prefix routing).
- Decouples "kick off the work" (RPC) from "stream the results" (WS).
- Lets the UI react live (append log lines, render exec output, refresh after a
  deploy) without polling.

## Consequences

- Broadcast is fire-and-forget — a missed event means a stale view until refresh.
- A second channel to manage: auto-connect (deferred via idle timer), reconnect on
  reload, hook cleanup for `stream_id`-scoped handlers.
- New push events require registering a `type` prefix + hook, or they're dropped.

## See Also
- [[websocket-events]], [[rpc-bridge]], [[k8s-pod-exec-vterm]]
