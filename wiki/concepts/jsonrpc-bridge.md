---
type: concept
applies_to: [rpc-bridge, jsonrpc-dispatcher]
decisions: [001-jsonrpc-over-http, 006-health-check-before-restart]
---

# The JSON-RPC Bridge

The contract between the two tiers. Everything Emacs asks the server is one
JSON-RPC 2.0 request; everything the server answers is one response.

## The shape on the wire

```jsonc
// request
{"jsonrpc": "2.0", "id": 42, "method": "k8s_list_pods",
 "params": {"namespace": "deploy-platform"}}

// success
{"jsonrpc": "2.0", "id": 42, "result": {"pods": [...], "count": 5}}

// error
{"jsonrpc": "2.0", "id": 42,
 "error": {"code": -32005, "message": "K8sConnectionError: ...",
           "data": {"type": "K8sConnectionError",
                    "details": {"suggestion": "connect to VPN first"}}}}
```

## Sync vs async (both still request/response)

- `ecloud-rpc-request` blocks until the HTTP response arrives.
- `ecloud-rpc-request-async` returns immediately; a callback fires on completion.
  This is still a single request/response — it does **not** use the WebSocket.

For results that arrive over time (logs, exec, big lists), the handler returns a
quick ack and then **pushes** data over the WebSocket. That's a different
mechanism — see [[websocket-events]].

## Method naming

snake_case server names registered in `_register_methods()`; kebab elisp wrappers
`ecloud-rpc-<name>`. The string on the wire is the server name. Lookup is
case-sensitive. See [[conventions]] § RPC Method Naming.

## Errors carry structure

Handlers raise via `error_handler.py` so the message is
`Type: message\nSuggestion: ...`. The dispatcher parses that into `error.data`,
and the elisp client surfaces the suggestion. A raw exception that doesn't match
the pattern collapses to `INTERNAL_ERROR (-32603)`.

## Timeouts ≠ dead server

A slow op (pod exec, large download) can exceed `ecloud-request-timeout` without
the server being down. The client therefore probes `/health` before treating a
timeout as a crash and restarting. See [[006-health-check-before-restart]].

## Why this design

See [[001-jsonrpc-over-http]].

## See Also
- [[rpc-bridge]], [[jsonrpc-dispatcher]], [[websocket-events]], [[rpc-request-lifecycle]]
