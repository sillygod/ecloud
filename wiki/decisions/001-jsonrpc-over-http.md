---
type: decision
status: accepted
date: 2026-06-11
related_components: [rpc-bridge, jsonrpc-dispatcher]
---

# 001 — JSON-RPC 2.0 over HTTP as the elisp↔server contract

> Status: accepted · documented retroactively from the codebase

## Context

Emacs can't talk to the GCP SDKs directly (they're Python/gRPC). Something has to
sit between elisp and Google's libraries. The options were: shell out to `gcloud`
per action; embed Python in Emacs; or run a local service and call it over a
simple protocol.

## Decision

Run a local **FastAPI server** and call it with **JSON-RPC 2.0 over HTTP POST**
to `/jsonrpc`. Elisp builds requests in `ecloud-rpc.el`; the server dispatches via
a method registry in `jsonrpc_handler.py`.

## Why

- JSON-RPC is trivial to encode/decode in elisp (`json-read`, plists) and Python
  (Pydantic).
- A single endpoint + a method-name registry scales to ~140 methods without new
  routes.
- HTTP gives batching, easy local debugging (curl), and a natural `/health`
  endpoint for liveness — which the client reuses to gate restarts
  ([[006-health-check-before-restart]]).
- Keeps the heavy GCP dependency tree out of Emacs entirely.

## Consequences

- Incremental/streaming results don't fit request/response, so a second channel
  (WebSocket) was added — see [[003-websocket-push-streaming]].
- Method names are a stringly-typed contract: adding a method touches three places
  (handler, registry, elisp wrapper). See [[conventions]].
- CORS is wide open and there's no transport auth — acceptable only because the
  server binds loopback.

## See Also
- [[jsonrpc-bridge]], [[rpc-bridge]], [[jsonrpc-dispatcher]]
