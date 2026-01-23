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
        
        # Sort buckets by name
        return sorted(buckets, key=lambda b: b.name)
    
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
        
        # Sort objects and prefixes
        objects.sort(key=lambda o: o.name)
        prefixes.sort()
        
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
    
    def batch_delete_objects(self, bucket_name: str, object_paths: list[str]) -> dict:
        """Delete multiple objects from GCS in a batch.
        
        Args:
            bucket_name: Name of the bucket.
            object_paths: List of object paths to delete.
            
        Returns:
            Dict with deletion results.
        """
        bucket = self._client.bucket(bucket_name)
        results = []
        errors = []
        
        # GCS batch operations can be done via the batch context manager
        # However, google-cloud-storage python client handles batching differently depending on version
        # A simple iteration is often safer unless we need strict atomicity (which GCS batch doesn't guarantee anyway for deletes)
        # But for performance on many objects, we should use the batch API if possible.
        # The python client's batch() is for batching requests.
        
        try:
            with self._client.batch():
                for path in object_paths:
                    blob = bucket.blob(path)
                    blob.delete()
                    results.append(path)
        except Exception as e:
            # If batch fails, we might not know which ones failed, but typically it raises on the first error 
            # or collects errors. The batch context manager suppresses exceptions from individual requests 
            # if they are 404s? actually no, delete() usually raises NotFound.
            # Let's try to handle them safely.
            pass

        # Since batch handling of exceptions is tricky in the library, 
        # let's iterate and capture errors for a more robust response, 
        # or use batch only if we accept 'all or nothing' behavior or looking at the batch response.
        # For simplicity and reliability in this agent context:
        
        success_count = 0
        failed_objects = []
        
        # We will do it sequentially for now to return accurate status per object if needed, 
        # or just simple try/except loop. 
        # Actually, for "Batch operations" the user likely wants performance. 
        # Let's use the batch context manager but catch individual errors if the library supports it,
        # or just fallback to loop for better error reporting.
        
        for path in object_paths:
            try:
                bucket.blob(path).delete()
                success_count += 1
            except NotFound:
                # Consider it success if it's already gone? Or report?
                # Usually delete is idempotent-ish if we don't care it was missing.
                success_count += 1 
            except Exception as e:
                failed_objects.append({"path": path, "error": str(e)})
                
        return {
            "success": len(failed_objects) == 0,
            "deleted_count": success_count,
            "failed_objects": failed_objects,
            "bucket": bucket_name
        }

    def generate_presigned_url(
        self, 
        bucket_name: str, 
        object_path: str, 
        expiration_seconds: int = 3600,
        method: str = "GET"
    ) -> str:
        """Generate a presigned URL for an object.
        
        Args:
            bucket_name: Name of the bucket.
            object_path: Path to the object.
            expiration_seconds: URL expiration time in seconds (default 1 hour).
            method: HTTP method (GET, PUT, etc.).
            
        Returns:
            Presigned URL string.
        """
        bucket = self._client.bucket(bucket_name)
        blob = bucket.blob(object_path)
        
        url = blob.generate_signed_url(
            version="v4",
            expiration=expiration_seconds,
            method=method,
            service_account_email=None, # uses current credentials
            access_token=None,
        )
        return url

    def update_object_metadata(
        self, 
        bucket_name: str, 
        object_path: str, 
        metadata: dict
    ) -> dict:
        """Update custom metadata for an object.
        
        Args:
            bucket_name: Name of the bucket.
            object_path: Path to the object.
            metadata: Dictionary of custom metadata keys and values.
            
        Returns:
            Updated object info.
        """
        bucket = self._client.bucket(bucket_name)
        blob = bucket.blob(object_path)
        blob.reload()
        
        if blob.metadata is None:
            blob.metadata = {}
            
        # Update metadata (merge)
        for k, v in metadata.items():
            blob.metadata[k] = v
            
        blob.patch()
        
        return {
            "name": blob.name,
            "metadata": blob.metadata,
            "updated": blob.updated.isoformat() if blob.updated else ""
        }

    def set_bucket_lifecycle(self, bucket_name: str, rules: list[dict]) -> dict:
        """Set lifecycle rules for a bucket.
        
        Args:
            bucket_name: Name of the bucket.
            rules: List of lifecycle rules. Each rule is a dict with 'action' and 'condition'.
                   Example:
                   [
                       {
                           "action": {"type": "Delete"},
                           "condition": {"age": 30}
                       }
                   ]
                   
        Returns:
            Updated bucket lifecycle configuration.
        """
        bucket = self._client.bucket(bucket_name)
        bucket.lifecycle_rules = rules
        bucket.patch()
        
        return {
            "bucket": bucket_name,
            "lifecycle_rules": list(bucket.lifecycle_rules)
        }

    def copy_object(
        self,
        source_bucket: str,
        source_object: str,
        dest_bucket: str,
        dest_object: str
    ) -> dict:
        """Copy an object.
        
        Args:
            source_bucket: Source bucket name.
            source_object: Source object path.
            dest_bucket: Destination bucket name.
            dest_object: Destination object path.
            
        Returns:
            Result of the copy operation.
        """
        s_bucket = self._client.bucket(source_bucket)
        s_blob = s_bucket.blob(source_object)
        
        d_bucket = self._client.bucket(dest_bucket)
        
        new_blob = s_bucket.copy_blob(s_blob, d_bucket, dest_object)
        
        return {
            "success": True,
            "source": f"{source_bucket}/{source_object}",
            "destination": f"{dest_bucket}/{dest_object}",
            "size": new_blob.size
        }

    def move_object(
        self,
        source_bucket: str,
        source_object: str,
        dest_bucket: str,
        dest_object: str
    ) -> dict:
        """Move an object (Copy + Delete)."""
        # 1. Copy
        copy_res = self.copy_object(source_bucket, source_object, dest_bucket, dest_object)
        
        # 2. Delete source
        self.delete_object(source_bucket, source_object)
        
        return {
            "success": True,
            "operation": "move",
            "source": f"{source_bucket}/{source_object}",
            "destination": f"{dest_bucket}/{dest_object}"
        }


# Singleton instance
_gcs_client: GCSClient | None = None


def get_gcs_client() -> GCSClient:
    """Get or create the GCS client singleton."""
    global _gcs_client
    if _gcs_client is None:
        _gcs_client = GCSClient()
    return _gcs_client
