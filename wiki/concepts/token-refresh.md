---
type: concept
applies_to: [kubernetes, helm, compute, sql, gar, gcs, secrets, service-usage, cloud-run, scheduler]
decisions: []
---

# Token Refresh & Connection Gating

GCP OAuth2 tokens expire. ECloud's K8s client (and the same pattern elsewhere)
refreshes transparently on a 401 and gates every call behind a live connection.

## `@auto_refresh_token`

A decorator (`k8s_client.py`) wrapping API calls:

1. Call the method.
2. On `ApiException` with `status == 401`, call `_refresh_token()` and retry
   **once**.
3. On connection/timeout-shaped errors, raise a `K8sConnectionError` /
   `RuntimeError` with a helpful suggestion (e.g. "connect to VPN first").

`_refresh_token()` refreshes the service-account credentials and rebuilds the
`ApiClient` with the new bearer token.

## `_ensure_connected()`

Every resource operation calls this first; it raises if `_api_client` is None.
You must `connect()` to a cluster before listing/exec/log ops.

## Where credentials come from

Per-process `GOOGLE_APPLICATION_CREDENTIALS` (see
[[multi-account-process-model]]). The Google libraries read it automatically; the
K8s client additionally builds a bearer token for the cluster API server.

## Invariants

- ⚠️ **Don't bypass the gate or the retry path.** New K8s methods should be
  decorated and assume the connection exists only after `_ensure_connected()`.
- ⚠️ **Retry is once.** Repeated 401s after a refresh are a real auth problem, not
  a transient token issue.

## See Also
- [[kubernetes]], [[multi-account-process-model]]
