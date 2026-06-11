---
type: flow
related_components: [account-manager, rpc-bridge]
---

# Flow: Switching the Active GCP Account

```
M-x ecloud-account-switch production
  └─ ecloud-account-switch('production)
       ├─ already running?  ──no──▶ ecloud-account-connect('production)
       │                              ├─ ecloud-account--validate-service-account
       │                              ├─ ecloud-account--allocate-port → e.g. 8766
       │                              ├─ start-process uvicorn
       │                              │    env GOOGLE_APPLICATION_CREDENTIALS=prod.json
       │                              └─ poll GET /health until 200  (health-gated)
       └─ ecloud-account--set-current('production)
            ├─ persist ecloud-account-last-used
            └─ update mode-line  [GCP:production]

# from now on:
any ecloud-rpc-request
  └─ ecloud-rpc--get-current-url
       └─ ecloud-account--get-url('production) → http://127.0.0.1:8766/jsonrpc
```

Every domain view (K8s, Cloud Run, GCS, …) immediately targets the production
server on its next call — no view code knows the switch happened. Credentials are
bound to the process environment, never sent in RPC params.

## See Also
- [[account-manager]], [[multi-account-process-model]], [[002-process-per-account]]
