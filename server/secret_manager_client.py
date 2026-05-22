"""Google Cloud Secret Manager client wrapper.

Provides operations for managing secrets and their versions:
list / get / create / add-version / access / delete.
"""

import os
from dataclasses import dataclass
from typing import List, Optional

from google.cloud import secretmanager_v1
from google.api_core.exceptions import GoogleAPIError

from config import config
from gcs_client import _get_project_from_credentials

# Fix for gRPC DNS issues on Mac/VPN
os.environ["GRPC_DNS_RESOLVER"] = "native"


@dataclass
class SecretInfo:
    """Metadata for a secret (not its payload)."""
    name: str  # short name, e.g. "my-secret"
    full_name: str  # "projects/<proj>/secrets/<name>"
    create_time: str
    labels: dict
    replication: str  # "automatic" or "user-managed"

    def to_dict(self) -> dict:
        return {
            "name": self.name,
            "fullName": self.full_name,
            "createTime": self.create_time,
            "labels": self.labels,
            "replication": self.replication,
        }


@dataclass
class SecretVersionInfo:
    """Metadata for a secret version."""
    name: str  # version id, e.g. "1" or "latest"
    full_name: str  # "projects/<proj>/secrets/<name>/versions/<v>"
    state: str  # ENABLED / DISABLED / DESTROYED
    create_time: str
    destroy_time: str

    def to_dict(self) -> dict:
        return {
            "name": self.name,
            "fullName": self.full_name,
            "state": self.state,
            "createTime": self.create_time,
            "destroyTime": self.destroy_time,
        }


def _short_name(full: str) -> str:
    """Return the last path segment of a Secret Manager resource name."""
    return full.rsplit("/", 1)[-1] if full else ""


class SecretManagerClient:
    """Client for Google Cloud Secret Manager operations."""

    def __init__(self, project: str | None = None):
        self._project = (
            project or config.gcs_project or _get_project_from_credentials()
        )
        if not self._project:
            raise ValueError(
                "GCP Project ID is required for Secret Manager operations"
            )
        self._client = secretmanager_v1.SecretManagerServiceClient()

    @property
    def project(self) -> str:
        return self._project

    def _parent(self) -> str:
        return f"projects/{self._project}"

    def _secret_path(self, name: str) -> str:
        return f"projects/{self._project}/secrets/{name}"

    def _version_path(self, name: str, version: str = "latest") -> str:
        return f"projects/{self._project}/secrets/{name}/versions/{version}"

    # ----- secrets -----

    def list_secrets(self) -> List[SecretInfo]:
        """List all secrets in the project."""
        try:
            request = secretmanager_v1.ListSecretsRequest(parent=self._parent())
            result: List[SecretInfo] = []
            for s in self._client.list_secrets(request=request):
                if s.replication and s.replication.automatic:
                    replication = "automatic"
                elif s.replication and s.replication.user_managed:
                    replication = "user-managed"
                else:
                    replication = "unknown"
                result.append(SecretInfo(
                    name=_short_name(s.name),
                    full_name=s.name,
                    create_time=s.create_time.isoformat() if s.create_time else "",
                    labels=dict(s.labels) if s.labels else {},
                    replication=replication,
                ))
            return result
        except GoogleAPIError as e:
            raise RuntimeError(f"Failed to list secrets: {e}")

    def create_secret(
        self,
        name: str,
        payload: str,
        labels: Optional[dict] = None,
    ) -> dict:
        """Create a new secret with an initial version.

        Uses automatic replication. Returns metadata for the new secret
        and the first version.
        """
        try:
            secret = self._client.create_secret(
                request=secretmanager_v1.CreateSecretRequest(
                    parent=self._parent(),
                    secret_id=name,
                    secret=secretmanager_v1.Secret(
                        replication=secretmanager_v1.Replication(
                            automatic=secretmanager_v1.Replication.Automatic()
                        ),
                        labels=labels or {},
                    ),
                )
            )
            version = self._client.add_secret_version(
                request=secretmanager_v1.AddSecretVersionRequest(
                    parent=secret.name,
                    payload=secretmanager_v1.SecretPayload(
                        data=payload.encode("utf-8")
                    ),
                )
            )
            return {
                "success": True,
                "name": _short_name(secret.name),
                "fullName": secret.name,
                "version": _short_name(version.name),
            }
        except GoogleAPIError as e:
            raise RuntimeError(f"Failed to create secret '{name}': {e}")

    def delete_secret(self, name: str) -> dict:
        """Delete a secret and all its versions."""
        try:
            self._client.delete_secret(
                request=secretmanager_v1.DeleteSecretRequest(
                    name=self._secret_path(name)
                )
            )
            return {"success": True, "name": name}
        except GoogleAPIError as e:
            raise RuntimeError(f"Failed to delete secret '{name}': {e}")

    # ----- versions -----

    def add_secret_version(self, name: str, payload: str) -> dict:
        """Add a new version to an existing secret."""
        try:
            version = self._client.add_secret_version(
                request=secretmanager_v1.AddSecretVersionRequest(
                    parent=self._secret_path(name),
                    payload=secretmanager_v1.SecretPayload(
                        data=payload.encode("utf-8")
                    ),
                )
            )
            return {
                "success": True,
                "name": name,
                "version": _short_name(version.name),
                "fullName": version.name,
            }
        except GoogleAPIError as e:
            raise RuntimeError(
                f"Failed to add version to secret '{name}': {e}"
            )

    def access_secret_version(
        self,
        name: str,
        version: str = "latest",
    ) -> dict:
        """Access the payload of a secret version.

        Returns the payload decoded as UTF-8. If the payload is binary
        and not valid UTF-8, returns a hex representation under ``hex``.
        """
        try:
            response = self._client.access_secret_version(
                request=secretmanager_v1.AccessSecretVersionRequest(
                    name=self._version_path(name, version)
                )
            )
            data = response.payload.data
            try:
                text = data.decode("utf-8")
                return {
                    "name": name,
                    "version": _short_name(response.name),
                    "fullName": response.name,
                    "payload": text,
                    "encoding": "utf-8",
                    "size": len(data),
                }
            except UnicodeDecodeError:
                return {
                    "name": name,
                    "version": _short_name(response.name),
                    "fullName": response.name,
                    "hex": data.hex(),
                    "encoding": "binary",
                    "size": len(data),
                }
        except GoogleAPIError as e:
            raise RuntimeError(
                f"Failed to access secret '{name}' version '{version}': {e}"
            )


# Singleton instance
_client_instance: SecretManagerClient | None = None


def get_secret_manager_client(project: str | None = None) -> SecretManagerClient:
    """Get or create the Secret Manager client singleton."""
    global _client_instance
    if _client_instance is None:
        _client_instance = SecretManagerClient(project)
    return _client_instance


def reset_secret_manager_client():
    """Reset the Secret Manager client singleton (e.g. on credential change)."""
    global _client_instance
    _client_instance = None
