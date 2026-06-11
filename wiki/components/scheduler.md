---
type: component
elisp_files:
  - emacs/ecloud-scheduler.el
server_files:
  - server/cloud_scheduler_client.py
rpc_methods:
  - cloud_scheduler_list_locations
  - cloud_scheduler_list_jobs
  - cloud_scheduler_list_all_jobs
  - cloud_scheduler_get_job
  - cloud_scheduler_create_http_job
  - cloud_scheduler_create_pubsub_job
  - cloud_scheduler_update_job
  - cloud_scheduler_pause_job
  - cloud_scheduler_resume_job
  - cloud_scheduler_run_job
  - cloud_scheduler_delete_job
depends_on: [rpc-bridge, jsonrpc-dispatcher]
depended_by: []
last_verified: 2026-06-11
---

# Cloud Scheduler

Manage cron jobs (HTTP and Pub/Sub targets): list, create, pause/resume,
manually run, update, and delete.

## What It Does

- `ecloud-scheduler.el` (524) — job browser, detail view (with retry config),
  interactive create/pause/resume/run/delete.
- `cloud_scheduler_client.py` (565) — job CRUD + lifecycle; multi-location
  discovery and all-locations fan-out.

## Public API

`cloud_scheduler_list_locations`, `list_jobs(location)`,
`list_all_jobs` (async; injects `location` per job),
`get_job(name, location)` → `JobInfo` (schedule, timezone, state, target_type/uri,
last/next run, retry config), `create_http_job(...)`, `create_pubsub_job(...)`,
`update_job(name, location, schedule, description, timezone)`, `pause_job`,
`resume_job`, `run_job`, `delete_job`. Each mutating op broadcasts a
`cloud_scheduler_job_*` event.

Elisp: `ecloud-scheduler-list`, `-refresh`, `-change-location`,
`-show-all-locations`, `-view-job`, `-create-http-job`, `-pause-job`,
`-resume-job`, `-run-job`, `-delete-job`.

## Key Invariants

- State ∈ `ENABLED | PAUSED | UNKNOWN`; target ∈ `HTTP | PUBSUB | APP_ENGINE`.
- Default HTTP retry: 3 attempts, 600 s max duration, 5 s–3600 s backoff, 5
  doublings.
- All-locations scan triggers when `--show-all-locations` is on and
  `--current-location` is nil. `list_locations` falls back to ~23 hardcoded
  regions if the API is unavailable.

## Interactions

- [[jsonrpc-dispatcher]] (`cloud_scheduler_*`, error `-32008`).
- `ecloud-scheduler-event-hook` receives `cloud_scheduler_job_{created,updated,
  paused,resumed,triggered,deleted}` and auto-refreshes. See [[websocket-events]].

## Gotchas

- `last_attempt_time` / `next_run_time` may be empty right after creation (wrapped
  in try/except).
- `ListLocations` can return a different set than `ListJobs` under partial
  permissions; the hardcoded fallback covers that.

## See Also
- [[cloud-run]] (shares the fan-out pattern), [[rpc-bridge]], [[websocket-events]]
