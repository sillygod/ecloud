---
type: decision
status: accepted
date: 2026-08-14
related_components: [kubernetes, helm]
---

# 007 — GKE endpoint choice determines the TLS trust anchor

> Status: accepted

## Context

A GKE cluster can be reached two ways, and they do **not** share a certificate
authority:

| Endpoint | Server certificate issued by | Client must trust |
|----------|------------------------------|-------------------|
| IP (`https://34.x.x.x`) | the cluster's own CA, generated per cluster | that cluster CA, pinned explicitly |
| DNS (`https://<hash>.<region>.gke.goog`) | Google Trust Services (public PKI) | the system root CA bundle |

`connect()` picks the DNS endpoint only when
`dns_endpoint_config.allow_external_traffic` is true (see [[kubernetes]]; commit
`3b8e885`). So which trust anchor applies is decided per cluster, at connect
time.

Every client built from that connection — the `kubernetes` Python client, and
the temp kubeconfig handed to Helm — inherits the choice.

## Decision

Treat the CA path as **optional, not missing**.

- IP endpoint → write the cluster CA to a temp file, set
  `Configuration.ssl_ca_cert` / kubeconfig `certificate-authority`.
- DNS endpoint → write no CA file at all; leave `ssl_ca_cert` unset and omit
  `certificate-authority` from the kubeconfig, so the client falls back to the
  system root store.

`K8sClient.get_cluster_credentials()` therefore returns
`{"endpoint", "ca_cert_path": str | None, "token"}` — a `None` CA path is a
valid, fully-connected state.

## Why

- Pinning the cluster CA against the DNS endpoint would **fail** TLS
  verification: the presented certificate is not signed by it. Omitting the CA
  is the correct configuration, not a shortcut.
- Omitting the CA does not weaken verification. Chain building, expiry and
  hostname checks all still run, against the OS trust store. Skipping
  verification would require `insecure-skip-tls-verify`, which ECloud never
  sets.

## Consequences

- ⚠️ **Never gate "are we connected?" on `ca_cert_path` being truthy.** This was
  the bug: `get_cluster_credentials()` returned `None` on DNS-endpoint clusters,
  so `_k8s_connect` logged `Warning: Could not get cluster credentials for Helm
  client` and never initialized Helm — every `helm_*` call then failed with
  `helm_not_initialized_error`, while all K8s views kept working. Gate on the
  connection objects (`_config`, `_credentials`) instead.
- Any future consumer of the cluster credentials must accept `ca_cert_path=None`
  and drop the CA key from whatever config it builds.
- The failure is cluster-dependent and therefore looks intermittent: the same
  build works on a private cluster and breaks on a public-DNS one.

## See Also
- [[kubernetes]] — endpoint selection and the 403 trap
- [[helm]] — the temp kubeconfig built from these credentials
- [[004-grpc-native-dns-resolver]] — unrelated DNS issue (gRPC resolver, not TLS)
