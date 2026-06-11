---
type: decision
status: accepted
date: 2026-06-11
related_components: [jsonrpc-dispatcher, gar, secrets, cloud-run, scheduler]
---

# 004 — Force `GRPC_DNS_RESOLVER=native` before any Google import

> Status: accepted · documented retroactively from the codebase

## Context

Under a VPN, gRPC's default **c-ares** DNS resolver frequently fails to resolve
`googleapis.com`, so every GCP call times out. The fix is to use the OS-native
resolver instead.

## Decision

Set `os.environ.setdefault('GRPC_DNS_RESOLVER', 'native')` **at the very top of
`main.py` (line 9), before any import**, and defensively in several
`*_client.py` modules (`gar_client.py`, `cloud_run_client.py`,
`secret_manager_client.py`, `cloud_scheduler_client.py`).

## Why

- gRPC reads `GRPC_DNS_RESOLVER` when its C core initializes — which happens on
  **first import** of a Google client library. Setting it afterward has no effect.
- `main.py` line 9 guarantees it's set before any `from google...` import in the
  process. The per-client repeats are belt-and-suspenders for when a client module
  is imported in another context.

## Consequences

- ⚠️ **Never move imports above the env-set in `main.py`.** A stray
  `from fastapi import ...` placed earlier is fine, but a Google import is not.
- The setting is per-process, which composes cleanly with
  [[002-process-per-account]].

## See Also
- [[jsonrpc-dispatcher]], [[conventions]]
