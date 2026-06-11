---
type: flow
related_components: [rpc-bridge, jsonrpc-dispatcher, account-manager]
---

# Flow: RPC Request Lifecycle

A plain synchronous call, end to end — e.g. listing pods.

```
Emacs                                    Server
─────                                    ──────
ecloud-k8s-switch-to-pods
  └─ ecloud-rpc-k8s-list-pods-async
       └─ ecloud-rpc-request[-async]
            ├─ ecloud-rpc--get-current-url   ← active account (account-manager)
            ├─ ecloud-rpc--next-id           → id=N
            ├─ ecloud-rpc--build-request
            │    {"jsonrpc":"2.0","id":N,
            │     "method":"k8s_list_pods",
            │     "params":{"namespace":...}}
            └─ POST /jsonrpc ───────────────▶ main.py jsonrpc_endpoint
                                               └─ JsonRpcRequest (Pydantic)
                                                  └─ JsonRpcHandler.handle()
                                                     ├─ check jsonrpc=="2.0"
                                                     ├─ _methods["k8s_list_pods"]
                                                     ├─ (await) handler(params)
                                                     │    └─ get_k8s_client()
                                                     │         └─ list_pods(...)  ← GKE API
                                                     │              (+ _get_pod_metrics)
                                                     └─ JsonRpcResponse(id=N,
                                                          result={"pods":[...],"count":k})
            ◀──────────────────────────────── HTTP 200 + JSON
       ecloud-rpc--parse-response → :result
  callback(result)
    └─ tabulated-list-print  (pods view updates)
```

## On error

The handler raises; the dispatcher maps it to a JSON-RPC code (e.g. `-32005`
K8S_ERROR) and, if the message is structured, fills `error.data.type` +
`error.data.details.suggestion`. `ecloud-rpc--parse-response` signals an elisp
error carrying the suggestion.

## On timeout

`ecloud-rpc--quick-health-check()` probes `/health`. Alive → it was just slow,
surface the timeout. Dead → `ecloud-account--handle-connection-error` restarts the
server and retries once. See [[006-health-check-before-restart]].

## See Also
- [[jsonrpc-bridge]], [[rpc-bridge]], [[jsonrpc-dispatcher]]
