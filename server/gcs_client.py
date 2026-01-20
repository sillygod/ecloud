"""Google Cloud Storage client wrapper.

Provides high-level operations for bucket and object management.
"""

import json
import os
from dataclasses import dataclass
from datetime import datetime
from google.cloud import storage
from google.cloud.exceptions import NotFound, GoogleCloudError

from config import config


def _get_project_from_credentials() -> str | None:
    """Try to extract project_id from service account JSON file."""
    creds_path = os.getenv("GOOGLE_APPLICATION_CREDENTIALS")
    if creds_path and os.path.exists(creds_path):
        try:
            with open(creds_path, "r") as f:
                creds = json.load(f)
                return creds.get("project_id")
        except (json.JSONDecodeError, IOError):
            pass
    return None


@dataclass
class ObjectInfo:
    """Information about a GCS object."""
    name: str
    size: int
    updated: str
    content_type: str | None = None
    is_folder: bool = False
    
    def to_dict(self) -> dict:
        return {
            "name": self.name,
            "size": self.size,
            "updated": self.updated,
            "content_type": self.content_type,
            "is_folder": self.is_folder,
        }


@dataclass
class BucketInfo:
    """Information about a GCS bucket."""
    name: str
    location: str
    storage_class: str
    created: str
    
    def to_dict(self) -> dict:
        return {
            "name": self.name,
            "location": self.location,
            "storage_class": self.storage_class,
            "created": self.created,
        }


class GCSClient:
    """Google Cloud Storage client wrapper."""
    
    def __init__(self, project: str | None = None):
        """Initialize GCS client.
        
        Args:
            project: Optional GCP project ID. If not provided, tries to auto-detect.
        """
        # Priority: explicit param > env var > SA JSON file
        resolved_project = project or config.gcs_project or _get_project_from_credentials()
        if not resolved_project:
            raise ValueError(
                "GCP project ID is required. Set ECLOUD_GCS_PROJECT environment variable "
                "or ensure your service account JSON contains project_id."
            )
        self._project = resolved_project
        self._client = storage.Client(project=resolved_project)
    
    def list_buckets(self) -> list[BucketInfo]:
        """List all accessible buckets.
        
        Returns:
            List of bucket information.
        """
        buckets = []
        for bucket in self._client.list_buckets():
            buckets.append(BucketInfo(
                name=bucket.name,
                location=bucket.location or "unknown",
                storage_class=bucket.storage_class or "STANDARD",
                created=bucket.time_created.isoformat() if bucket.time_created else "",
            ))
        return buckets
    
    def list_objects(
        self,
        bucket_name: str,
        prefix: str = "",
        delimiter: str = "/",
    ) -> tuple[list[ObjectInfo], list[str]]:
        """List objects in a bucket with optional prefix.
        
        Args:
            bucket_name: Name of the bucket.
            prefix: Filter objects by prefix (folder path).
            delimiter: Delimiter for virtual folder hierarchy.
            
        Returns:
            Tuple of (objects, prefixes/folders).
        """
        bucket = self._client.bucket(bucket_name)
        blobs = bucket.list_blobs(prefix=prefix, delimiter=delimiter)
        
        objects = []
        for blob in blobs:
            # Skip the prefix itself if it's listed
            if blob.name == prefix:
                continue
            objects.append(ObjectInfo(
                name=blob.name,
                size=blob.size or 0,
                updated=blob.updated.isoformat() if blob.updated else "",
                content_type=blob.content_type,
                is_folder=blob.name.endswith("/"),
            ))
        
        # Get virtual folders (prefixes)
        prefixes = list(blobs.prefixes) if hasattr(blobs, 'prefixes') else []
        
        return objects, prefixes
    
    def get_object_info(self, bucket_name: str, object_path: str) -> ObjectInfo | None:
        """Get metadata for a specific object.
        
        Args:
            bucket_name: Name of the bucket.
            object_path: Full path to the object.
            
        Returns:
            Object info or None if not found.
        """
        try:
            bucket = self._client.bucket(bucket_name)
            blob = bucket.blob(object_path)
            blob.reload()  # Fetch metadata
            return ObjectInfo(
                name=blob.name,
                size=blob.size or 0,
                updated=blob.updated.isoformat() if blob.updated else "",
                content_type=blob.content_type,
                is_folder=blob.name.endswith("/"),
            )
        except NotFound:
            return None
    
    def download_object(
        self,
        bucket_name: str,
        object_path: str,
        local_path: str,
    ) -> dict:
        """Download an object to local filesystem.
        
        Args:
            bucket_name: Name of the bucket.
            object_path: Full path to the object in GCS.
            local_path: Local filesystem path to save to.
            
        Returns:
            Dict with download result info.
        """
        bucket = self._client.bucket(bucket_name)
        blob = bucket.blob(object_path)
        blob.download_to_filename(local_path)
        return {
            "success": True,
            "bucket": bucket_name,
            "object": object_path,
            "local_path": local_path,
            "size": blob.size,
        }
    
    def upload_object(
        self,
        bucket_name: str,
        object_path: str,
        local_path: str,
        content_type: str | None = None,
    ) -> dict:
        """Upload a local file to GCS.
        
        Args:
            bucket_name: Name of the bucket.
            object_path: Destination path in GCS.
            local_path: Local file path to upload.
            content_type: Optional content type override.
            
        Returns:
            Dict with upload result info.
        """
        bucket = self._client.bucket(bucket_name)
        blob = bucket.blob(object_path)
        blob.upload_from_filename(local_path, content_type=content_type)
        return {
            "success": True,
            "bucket": bucket_name,
            "object": object_path,
            "local_path": local_path,
            "size": blob.size,
            "content_type": blob.content_type,
        }
    
    def delete_object(self, bucket_name: str, object_path: str) -> dict:
        """Delete an object from GCS.
        
        Args:
            bucket_name: Name of the bucket.
            object_path: Full path to the object.
            
        Returns:
            Dict with deletion result.
        """
        bucket = self._client.bucket(bucket_name)
        blob = bucket.blob(object_path)
        blob.delete()
        return {
            "success": True,
            "bucket": bucket_name,
            "object": object_path,
        }
    
    def create_folder(self, bucket_name: str, folder_path: str) -> dict:
        """Create a virtual folder (empty object with trailing slash).
        
        Args:
            bucket_name: Name of the bucket.
            folder_path: Folder path (will add trailing slash if missing).
            
        Returns:
            Dict with creation result.
        """
        if not folder_path.endswith("/"):
            folder_path += "/"
        
        bucket = self._client.bucket(bucket_name)
        blob = bucket.blob(folder_path)
        blob.upload_from_string("", content_type="application/x-directory")
        return {
            "success": True,
            "bucket": bucket_name,
            "folder": folder_path,
        }


# Singleton instance
_gcs_client: GCSClient | None = None


def get_gcs_client() -> GCSClient:
    """Get or create the GCS client singleton."""
    global _gcs_client
    if _gcs_client is None:
        _gcs_client = GCSClient()
    return _gcs_client
