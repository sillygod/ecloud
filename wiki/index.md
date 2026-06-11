# ECloud Wiki Index

> Last updated: 2026-06-11

ECloud manages Google Cloud Platform from inside Emacs. The Emacs front-end
(elisp) talks to a Python FastAPI server over JSON-RPC; the server wraps the GCP
SDKs and pushes async updates back over a WebSocket.

## 🎯 Quick Start

| Page | Audience |
|------|----------|
| [Onboarding Guide](onboarding/quick-start.md) | New humans & agents — 5-min system overview |
| [Service Account Setup](guides/service-account-setup.md) | **Read before first use** — create the GCP SA + IAM roles |
| [Schema](schema.md) | How this wiki works, sync protocol |
| [Conventions](conventions.md) | Style, RPC naming, faces, commit & reload rules |
| [Changelog](changelog.md) | What changed in the wiki |

## 🧩 Core Architecture

| Component | What It Does | Key Files |
|-----------|-------------|-----------|
| [[rpc-bridge]] | Elisp JSON-RPC client + WebSocket client | `ecloud-rpc.el`, `ecloud-ws.el` |
| [[jsonrpc-dispatcher]] | Server method registry, dispatch, errors | `main.py`, `jsonrpc_handler.py`, `error_handler.py` |
| [[account-manager]] | Multi-account: a server process per GCP account | `ecloud-account-manager.el` |

## ☁️ Service Domains

| Component | What It Does | Key Files |
|-----------|-------------|-----------|
| [[kubernetes]] | GKE clusters, pods/services/deployments, exec, logs | `ecloud-k8s.el`, `k8s_client.py` |
| [[helm]] | Helm 3 releases, repos, install/upgrade/rollback | `ecloud-k8s.el` (helm views), `helm_client.py` |
| [[cloud-run]] | Cloud Run services, revisions, deploy, logs | `ecloud-cloud-run.el`, `cloud_run_client.py` |
| [[scheduler]] | Cloud Scheduler cron jobs (HTTP/PubSub) | `ecloud-scheduler.el`, `cloud_scheduler_client.py` |
| [[compute]] | Compute Engine VMs + static/ephemeral IPs, SSH | `ecloud-compute.el`, `ecloud-ips.el`, `compute_client.py` |
| [[sql]] | Cloud SQL instances, DBs, users, backups, proxy | `ecloud-sql.el`, `sql_client.py`, `sql_proxy.py` |
| [[gar]] | Artifact Registry repos/images/tags, docker pull/push | `ecloud-gar.el`, `gar_client.py` |
| [[gcs]] | Cloud Storage browser, upload/download, presigned URLs | `ecloud-browser.el`, `gcs_client.py` |
| [[secrets]] | Secret Manager — list/access/create/delete secrets | `ecloud-secrets.el`, `secret_manager_client.py` |
| [[service-usage]] | Enable/disable GCP APIs (streaming list) | `ecloud-services.el`, `service_usage_client.py` |

## 💡 Concepts

| Concept | Applies To |
|---------|-----------|
| [[jsonrpc-bridge]] | rpc-bridge, jsonrpc-dispatcher |
| [[websocket-events]] | every domain with async/streaming ops |
| [[multi-account-process-model]] | account-manager, rpc-bridge |
| [[token-refresh]] | kubernetes, all GCP clients |
| [[ui-conventions]] | every elisp view module |

## 📋 Decisions (ADR)

| # | Decision | Status |
|---|----------|--------|
| [[001-jsonrpc-over-http]] | JSON-RPC 2.0 over HTTP as the elisp↔server contract | accepted |
| [[002-process-per-account]] | One server OS process per GCP account | accepted |
| [[003-websocket-push-streaming]] | WebSocket push for logs/exec/long ops | accepted |
| [[004-grpc-native-dns-resolver]] | Force `GRPC_DNS_RESOLVER=native` before imports | accepted |
| [[005-transient-menus]] | Transient popups for help & menus | accepted |
| [[006-health-check-before-restart]] | Probe `/health` before auto-restarting on timeout | accepted |

## 🔄 Flows

| Flow | Description |
|------|-------------|
| [[rpc-request-lifecycle]] | elisp call → HTTP → dispatch → client → response |
| [[k8s-pod-exec-vterm]] | Interactive pod shell over WebSocket into vterm |
| [[account-switch]] | Switching the active GCP account end-to-end |

## 📦 Sources (distilled specs)

| Page | Original |
|------|----------|
| [[sources-index]] | Overview of `.kiro/specs/` |
| [[multi-account-support]] | `.kiro/specs/multi-account-support/` |
| [[helm-and-transient-ui]] | `.kiro/specs/helm-and-transient-ui/` |

## 📖 Guides (end-user how-to)

| Guide | Covers |
|-------|--------|
| [[service-account-setup]] | Create the GCP service account, IAM roles, Helm perms |
| [[multi-account]] | Configure & use multiple GCP accounts (setup, migration, advanced) |
| [[service-usage-quickstart]] | Enable/disable GCP APIs — quickstart |

## 🕒 Reports (point-in-time, exempt from sync)

| Report | What it records |
|--------|-----------------|
| [[service-usage-implementation]] | Implementation summary of the service-usage feature |
| [[transient-menu-verification]] | Transient-menu command-mapping verification (task 14.1) |
