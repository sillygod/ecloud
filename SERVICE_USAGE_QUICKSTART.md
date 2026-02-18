# Service Usage Quick Start

## Installation

```bash
# 1. Install Python dependency
cd server
uv sync  # or: pip install google-cloud-service-usage

# 2. Load in Emacs (already done if you load ecloud.el)
(require 'ecloud-services)
```

## Commands

| Command | Description | Streaming |
|---------|-------------|-----------|
| `M-x ecloud-services-list` | View services | ✓ Auto (for large sets) |
| `C-u M-x ecloud-services-list` | View with filter | ✓ Auto (for large sets) |
| `M-x ecloud-services-enable` | Enable with fuzzy completion | ✓ Always |
| `M-x ecloud-services-enable-by-name` | Enable by exact name | - |
| `M-x ecloud-services-disable` | Disable with confirmation | - (small set) |
| `M-x ecloud-services-get-info` | Get service details | - |

## Performance Tips

- **Default list is fast**: `ecloud-services-list` shows only ENABLED services (typically 10-50)
- **Streaming for large sets**: Automatically uses streaming for DISABLED/All filters
- **Real-time progress**: See loading progress for large result sets (10000+ services)
- **Use filters**: Always filter by state when possible for better performance

## Quick Examples

### List Enabled Services (Fast - No Streaming)
```elisp
M-x ecloud-services-list
;; Shows only enabled services (1-3 seconds)
;; Fast, no streaming needed (10-50 services)
```

### List All Services (Streaming - Shows Progress)
```elisp
C-u M-x ecloud-services-list
;; Select: All
;; Uses streaming for 10000+ services
;; Shows: "Services loaded: 50, 100, 150..."
```

### Enable Service (Streaming with Progress)
```elisp
M-x ecloud-services-enable
;; Streams disabled services with real-time progress
;; Shows: "Services loaded: 50, 100, 150..."
;; Then fuzzy completion appears
```
;; Type: compute
;; Select: compute.googleapis.com
;; Automatically loads only disabled services with fuzzy search
```

### Enable Cloud Run
```elisp
M-x ecloud-services-enable-by-name
;; Enter: run.googleapis.com
```

### List Enabled Services
```elisp
C-u M-x ecloud-services-list
;; Select: ENABLED
```

## Common Services

| Service Name | Description |
|--------------|-------------|
| `compute.googleapis.com` | Compute Engine API |
| `run.googleapis.com` | Cloud Run API |
| `container.googleapis.com` | Kubernetes Engine API |
| `sqladmin.googleapis.com` | Cloud SQL Admin API |
| `storage.googleapis.com` | Cloud Storage API |
| `cloudscheduler.googleapis.com` | Cloud Scheduler API |
| `artifactregistry.googleapis.com` | Artifact Registry API |
| `logging.googleapis.com` | Cloud Logging API |
| `monitoring.googleapis.com` | Cloud Monitoring API |

## Permissions

Your service account needs:
```bash
gcloud projects add-iam-policy-binding PROJECT_ID \
  --member="serviceAccount:EMAIL" \
  --role="roles/serviceusage.serviceUsageAdmin"
```

## Troubleshooting

**"Service Usage API not enabled"**
```elisp
M-x ecloud-services-enable-by-name
;; Enter: serviceusage.googleapis.com
```

**"Permission denied"**
- Check IAM role: `roles/serviceusage.serviceUsageAdmin`

**Timeout**
- Enabling can take 1-5 minutes
- Check status with `M-x ecloud-services-get-info`

## See Also

- Full guide: `SERVICE_USAGE_GUIDE.md`
- Implementation: `SERVICE_USAGE_IMPLEMENTATION.md`
