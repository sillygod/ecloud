"""JSON-RPC 2.0 handler for ecloud server.

Implements the JSON-RPC 2.0 specification for routing method calls
to the appropriate GCS client operations.
"""

from dataclasses import dataclass
from typing import Any, Callable
from pydantic import BaseModel

from gcs_client import get_gcs_client, GCSClient
from gar_client import get_gar_client, GARClient


# JSON-RPC 2.0 Error Codes
PARSE_ERROR = -32700
INVALID_REQUEST = -32600
METHOD_NOT_FOUND = -32601
INVALID_PARAMS = -32602
INTERNAL_ERROR = -32603
# Custom error codes
GCS_ERROR = -32001
NOT_FOUND = -32002
GAR_ERROR = -32003


class JsonRpcRequest(BaseModel):
    """JSON-RPC 2.0 Request."""
    jsonrpc: str = "2.0"
    id: int | str | None = None
    method: str
    params: dict[str, Any] = {}


class JsonRpcError(BaseModel):
    """JSON-RPC 2.0 Error."""
    code: int
    message: str
    data: Any = None


class JsonRpcResponse(BaseModel):
    """JSON-RPC 2.0 Response."""
    jsonrpc: str = "2.0"
    id: int | str | None = None
    result: Any = None
    error: JsonRpcError | None = None


class JsonRpcHandler:
    """Handler for JSON-RPC method routing."""
    
    def __init__(self):
        self._methods: dict[str, Callable] = {}
        self._register_methods()
    
    def _register_methods(self):
        """Register all available JSON-RPC methods."""
        self._methods = {
            # GCS methods
            "list_buckets": self._list_buckets,
            "list_objects": self._list_objects,
            "get_object_info": self._get_object_info,
            "download_object": self._download_object,
            "upload_object": self._upload_object,
            "delete_object": self._delete_object,
            "create_folder": self._create_folder,
            # GAR methods
            "gar_list_repos": self._gar_list_repos,
            "gar_list_locations": self._gar_list_locations,
            "gar_create_tag": self._gar_create_tag,
            "gar_list_packages": self._gar_list_packages,
            "gar_list_tags": self._gar_list_tags,
            "gar_delete_package": self._gar_delete_package,
            "gar_delete_tag": self._gar_delete_tag,
            "gar_pull": self._gar_pull,
            "gar_push": self._gar_push,
            "gar_tag": self._gar_tag,
            # System
            "ping": self._ping,
        }
    
    def handle(self, request: JsonRpcRequest) -> JsonRpcResponse:
        """Handle a JSON-RPC request.
        
        Args:
            request: The incoming JSON-RPC request.
            
        Returns:
            JSON-RPC response with result or error.
        """
        # Validate JSON-RPC version
        if request.jsonrpc != "2.0":
            return JsonRpcResponse(
                id=request.id,
                error=JsonRpcError(
                    code=INVALID_REQUEST,
                    message="Invalid JSON-RPC version, must be 2.0",
                ),
            )
        
        # Find method
        method_fn = self._methods.get(request.method)
        if method_fn is None:
            return JsonRpcResponse(
                id=request.id,
                error=JsonRpcError(
                    code=METHOD_NOT_FOUND,
                    message=f"Method not found: {request.method}",
                ),
            )
        
        # Execute method
        try:
            result = method_fn(request.params)
            return JsonRpcResponse(id=request.id, result=result)
        except TypeError as e:
            return JsonRpcResponse(
                id=request.id,
                error=JsonRpcError(
                    code=INVALID_PARAMS,
                    message=str(e),
                ),
            )
        except Exception as e:
            error_code = GCS_ERROR
            if "ArtifactRegistry" in type(e).__name__ or "Docker" in str(e):
                error_code = GAR_ERROR
                
            return JsonRpcResponse(
                id=request.id,
                error=JsonRpcError(
                    code=error_code,
                    message=str(e),
                    data={"type": type(e).__name__},
                ),
            )
    
    def _get_client(self) -> GCSClient:
        """Get the GCS client instance."""
        return get_gcs_client()

    def _get_gar_client(self) -> GARClient:
        """Get the GAR client instance."""
        return get_gar_client()
    
    # --- GCS Method implementations ---
    
    def _ping(self, params: dict) -> dict:
        """Health check method."""
        return {"pong": True, "version": "0.1.0"}
    
    def _list_buckets(self, params: dict) -> dict:
        """List all accessible buckets."""
        client = self._get_client()
        buckets = client.list_buckets()
        return {
            "buckets": [b.to_dict() for b in buckets],
            "count": len(buckets),
        }
    
    def _list_objects(self, params: dict) -> dict:
        """List objects in a bucket."""
        bucket = params.get("bucket")
        if not bucket:
            raise TypeError("Missing required parameter: bucket")
        
        prefix = params.get("prefix", "")
        delimiter = params.get("delimiter", "/")
        
        client = self._get_client()
        objects, prefixes = client.list_objects(bucket, prefix, delimiter)
        
        return {
            "objects": [o.to_dict() for o in objects],
            "prefixes": prefixes,
            "bucket": bucket,
            "prefix": prefix,
        }
    
    def _get_object_info(self, params: dict) -> dict:
        """Get object metadata."""
        bucket = params.get("bucket")
        object_path = params.get("object_path")
        
        if not bucket or not object_path:
            raise TypeError("Missing required parameters: bucket, object_path")
        
        client = self._get_client()
        info = client.get_object_info(bucket, object_path)
        
        if info is None:
            raise FileNotFoundError(f"Object not found: {bucket}/{object_path}")
        
        return info.to_dict()
    
    def _download_object(self, params: dict) -> dict:
        """Download an object to local filesystem."""
        bucket = params.get("bucket")
        object_path = params.get("object_path")
        local_path = params.get("local_path")
        
        if not all([bucket, object_path, local_path]):
            raise TypeError("Missing required parameters: bucket, object_path, local_path")
        
        client = self._get_client()
        return client.download_object(bucket, object_path, local_path)
    
    def _upload_object(self, params: dict) -> dict:
        """Upload a local file to GCS."""
        bucket = params.get("bucket")
        object_path = params.get("object_path")
        local_path = params.get("local_path")
        content_type = params.get("content_type")
        
        if not all([bucket, object_path, local_path]):
            raise TypeError("Missing required parameters: bucket, object_path, local_path")
        
        client = self._get_client()
        return client.upload_object(bucket, object_path, local_path, content_type)
    
    def _delete_object(self, params: dict) -> dict:
        """Delete an object."""
        bucket = params.get("bucket")
        object_path = params.get("object_path")
        
        if not bucket or not object_path:
            raise TypeError("Missing required parameters: bucket, object_path")
        
        client = self._get_client()
        return client.delete_object(bucket, object_path)
    
    def _create_folder(self, params: dict) -> dict:
        """Create a virtual folder."""
        bucket = params.get("bucket")
        folder_path = params.get("folder_path")
        
        if not bucket or not folder_path:
            raise TypeError("Missing required parameters: bucket, folder_path")
        
        client = self._get_client()
        return client.create_folder(bucket, folder_path)

    # --- GAR Method implementations ---

    def _gar_list_repos(self, params: dict) -> dict:
        location = params.get("location")
        if not location:
            raise TypeError("Missing parameter: location")
        
        client = self._get_gar_client()
        repos = client.list_repositories(location)
        return {"repositories": [r.to_dict() for r in repos]}

    def _gar_list_locations(self, params: dict) -> dict:
        client = self._get_gar_client()
        locations = client.list_locations()
        return {"locations": locations}

    def _gar_create_tag(self, params: dict) -> dict:
        package = params.get("package")
        tag_id = params.get("tag_id")
        version = params.get("version") # Expect full version resource name
        
        if not all([package, tag_id, version]):
            raise ValueError("package, tag_id, and version are required")
            
        client = self._get_gar_client()
        return client.create_tag(package, tag_id, version)

    def _gar_list_packages(self, params: dict) -> dict:
        repo = params.get("repo") # Full resource name
        if not repo:
            raise TypeError("Missing parameter: repo")
            
        client = self._get_gar_client()
        images = client.list_packages(repo)
        return {"packages": [i.to_dict() for i in images]}

    def _gar_list_tags(self, params: dict) -> dict:
        package = params.get("package") # Full resource name
        if not package:
            raise TypeError("Missing parameter: package")
            
        client = self._get_gar_client()
        tags = client.list_tags(package)
        return {"tags": [t.to_dict() for t in tags]}

    def _gar_delete_package(self, params: dict) -> dict:
        package = params.get("package")
        if not package:
            raise TypeError("Missing parameter: package")
        return self._get_gar_client().delete_package(package)

    def _gar_delete_tag(self, params: dict) -> dict:
        name = params.get("name") # Full tag resource name
        if not name:
            raise TypeError("Missing parameter: name")
        return self._get_gar_client().delete_tag(name)

    def _gar_pull(self, params: dict) -> dict:
        uri = params.get("uri")
        if not uri:
            raise TypeError("Missing parameter: uri")
        return self._get_gar_client().docker_pull(uri)

    def _gar_push(self, params: dict) -> dict:
        uri = params.get("uri")
        if not uri:
            raise TypeError("Missing parameter: uri")
        return self._get_gar_client().docker_push(uri)

    def _gar_tag(self, params: dict) -> dict:
        source = params.get("source")
        target = params.get("target")
        if not source or not target:
            raise TypeError("Missing parameters: source, target")
        return self._get_gar_client().docker_tag(source, target)


# Singleton handler instance
handler = JsonRpcHandler()
