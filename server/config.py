"""Configuration management for ecloud server.

Uses environment variables for sensitive settings.
"""

import os
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
    
    @classmethod
    def from_env(cls) -> "Config":
        """Load configuration from environment variables."""
        return cls(
            host=os.getenv("ECLOUD_HOST", "127.0.0.1"),
            port=int(os.getenv("ECLOUD_PORT", "8765")),
            gcs_project=os.getenv("ECLOUD_GCS_PROJECT"),
        )


# Global config instance
config = Config.from_env()
