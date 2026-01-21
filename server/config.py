"""Configuration management for ecloud server.

Uses environment variables for sensitive settings.
"""

import os
import json
from dataclasses import dataclass


@dataclass
class Config:
    """Server configuration."""
    
    # Server settings
    host: str = "127.0.0.1"
    port: int = 8765
    
    # GCS settings - uses GOOGLE_APPLICATION_CREDENTIALS env var automatically
    # Optionally override default project
    gcs_project: str | None = None
    
    # Service Account to impersonate for gcloud commands (e.g. SSH)
    impersonate_service_account: str | None = None

    @classmethod
    def from_env(cls) -> "Config":
        """Load configuration from environment variables."""
        impersonate_sa = os.getenv("ECLOUD_IMPERSONATE_SA")
        
        # If not explicitly set, try to extract from credentials file
        if not impersonate_sa:
            creds_path = os.getenv("GOOGLE_APPLICATION_CREDENTIALS")
            if creds_path and os.path.exists(creds_path):
                try:
                    with open(creds_path, "r") as f:
                        data = json.load(f)
                        impersonate_sa = data.get("client_email")
                except Exception as e:
                    print(f"Warning: Failed to extract client_email from credentials: {e}")
        
        return cls(
            host=os.getenv("ECLOUD_HOST", "127.0.0.1"),
            port=int(os.getenv("ECLOUD_PORT", "8765")),
            gcs_project=os.getenv("ECLOUD_GCS_PROJECT"),
            impersonate_service_account=impersonate_sa,
        )


# Global config instance
config = Config.from_env()
