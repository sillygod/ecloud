---
type: decision
status: accepted
date: 2026-06-11
related_components: [rpc-bridge, account-manager]
---

# 006 — Probe `/health` before auto-restarting on timeout

> Status: accepted · documented retroactively from the codebase

## Context

The client auto-recovers from a dead server by restarting it and retrying. But
some legitimate operations are slow — interactive pod exec, large GCS downloads,
all-regions scans — and routinely exceed `ecloud-request-timeout` (30 s). Treating
every timeout as a crash would kill servers mid-operation and flap.

## Decision

On a failed/timed-out RPC, call `ecloud-rpc--quick-health-check()` (a 2 s
`GET /health`) **before** deciding the server is down. Only if `/health` is also
unresponsive does `ecloud-account--handle-connection-error` restart and retry the
original call once.

## Why

- `/health` is cheap and answers in milliseconds when the server is alive, even
  while a long op is in flight on another request.
- Distinguishes "slow op" from "dead server" reliably and cheaply.

## Consequences

- ⚠️ **Don't remove the health gate.** Without it, slow ops restart the server and
  break the whole session.
- A pathologically slow loopback could make `/health` itself time out and cause a
  needless restart — the 2 s budget is a deliberate trade-off.

## See Also
- [[rpc-bridge]], [[account-manager]], [[jsonrpc-bridge]]
