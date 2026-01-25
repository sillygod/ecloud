"""JSON-RPC 2.0 handler for ecloud server.

Implements the JSON-RPC 2.0 specification for routing method calls
to the appropriate GCS client operations.
"""

from dataclasses import dataclass
from typing import Any, Callable
from pydantic import BaseModel
import asyncio
import os

from gcs_client import get_gcs_client, GCSClient
from gar_client import get_gar_client, GARClient
from compute_client import get_compute_client, ComputeClient
from sql_client import get_sql_client, SQLClient
from sql_proxy import get_proxy_manager, SQLProxyManager
from k8s_client import get_k8s_client, K8sClient
from k8s_log_streamer import get_log_streamer, K8sLogStreamer
from helm_client import get_helm_client, HelmClient, reset_helm_client
from config import config
from websocket_manager import get_manager
from error_handler import (
    StructuredError,
    create_helm_error_from_exception,
    classify_helm_error,
    helm_not_initialized_error,
    helm_operation_failed_error,
)


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
COMPUTE_ERROR = -32004
K8S_ERROR = -32005
HELM_ERROR = -32006


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
            "batch_delete_objects": self._batch_delete_objects,
            "create_folder": self._create_folder,
            "generate_presigned_url": self._generate_presigned_url,
            "update_object_metadata": self._update_object_metadata,
            "set_bucket_lifecycle": self._set_bucket_lifecycle,
            "copy_object": self._copy_object,
            "move_object": self._move_object,
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
            # Compute methods
            "compute_list_addresses": self._compute_list_addresses,
            "compute_reserve_address": self._compute_reserve_address,
            "compute_list_regions": self._compute_list_regions,
            "compute_list_instances": self._compute_list_instances,
            "compute_start_instance": self._compute_start_instance,
            "compute_stop_instance": self._compute_stop_instance,
            "compute_reset_instance": self._compute_reset_instance,
            "compute_delete_instance": self._compute_delete_instance,
            # SQL methods
            "sql_list_instances": self._sql_list_instances,
            "sql_list_databases": self._sql_list_databases,
            "sql_create_database": self._sql_create_database,
            "sql_delete_database": self._sql_delete_database,
            "sql_list_users": self._sql_list_users,
            "sql_create_user": self._sql_create_user,
            "sql_delete_user": self._sql_delete_user,
            "sql_start_proxy": self._sql_start_proxy,
            "sql_stop_proxy": self._sql_stop_proxy,
            "sql_list_proxies": self._sql_list_proxies,
            "sql_list_backups": self._sql_list_backups,
            "sql_create_backup": self._sql_create_backup,
            "sql_delete_backup": self._sql_delete_backup,
            "sql_restore_backup": self._sql_restore_backup,
            "sql_get_connection_info": self._sql_get_connection_info,
            # K8s methods
            "k8s_list_clusters": self._k8s_list_clusters,
            "k8s_connect": self._k8s_connect,
            "k8s_disconnect": self._k8s_disconnect,
            "k8s_connection_status": self._k8s_connection_status,
            "k8s_list_namespaces": self._k8s_list_namespaces,
            "k8s_list_pods": self._k8s_list_pods,
            "k8s_list_services": self._k8s_list_services,
            "k8s_list_ingresses": self._k8s_list_ingresses,
            "k8s_list_deployments": self._k8s_list_deployments,
            "k8s_scale_deployment": self._k8s_scale_deployment,
            "k8s_pod_exec": self._k8s_pod_exec,
            "k8s_apply_manifest": self._k8s_apply_manifest,
            "k8s_resource_metrics": self._k8s_resource_metrics,
            "k8s_get_yaml": self._k8s_get_yaml,
            "k8s_pod_logs": self._k8s_pod_logs,
            "k8s_start_log_stream": self._k8s_start_log_stream,
            "k8s_stop_log_stream": self._k8s_stop_log_stream,
            "k8s_list_log_streams": self._k8s_list_log_streams,
            "k8s_get_resources": self._k8s_get_resources,
            "k8s_list_api_resources": self._k8s_list_api_resources,
            # Helm methods
            "helm_list_releases": self._helm_list_releases,
            "helm_get_release_details": self._helm_get_release_details,
            "helm_install_chart": self._helm_install_chart,
            "helm_upgrade_release": self._helm_upgrade_release,
            "helm_rollback_release": self._helm_rollback_release,
            "helm_uninstall_release": self._helm_uninstall_release,
            "helm_list_repositories": self._helm_list_repositories,
            "helm_add_repository": self._helm_add_repository,
            "helm_remove_repository": self._helm_remove_repository,
            "helm_search_charts": self._helm_search_charts,
            # System
            "ping": self._ping,
            "get_config": self._get_config,
        }
    
    async def handle(self, request: JsonRpcRequest) -> JsonRpcResponse:
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
            if asyncio.iscoroutinefunction(method_fn):
                result = await method_fn(request.params)
            else:
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
            # Determine error code based on exception type and message
            error_code = INTERNAL_ERROR
            error_msg = str(e)
            error_data = {"type": type(e).__name__}
            
            # Check if this is a structured error (contains error type prefix)
            if ":" in error_msg and any(err_type in error_msg for err_type in [
                "Helm", "Auth", "K8s", "Invalid"
            ]):
                # Parse structured error format: "ErrorType: message"
                parts = error_msg.split(":", 1)
                if len(parts) == 2:
                    error_type = parts[0].strip()
                    error_message = parts[1].strip()
                    
                    # Extract suggestion if present
                    suggestion = None
                    if "\nSuggestion:" in error_message:
                        msg_parts = error_message.split("\nSuggestion:", 1)
                        error_message = msg_parts[0].strip()
                        suggestion = msg_parts[1].strip()
                    
                    # Build structured error data
                    error_data = {
                        "type": error_type,
                    }
                    if suggestion:
                        error_data["details"] = {"suggestion": suggestion}
                    
                    # Set appropriate error code
                    if "Helm" in error_type:
                        error_code = HELM_ERROR
                    elif "K8s" in error_type:
                        error_code = K8S_ERROR
                    elif "Auth" in error_type:
                        error_code = INTERNAL_ERROR
                    
                    error_msg = error_message
            else:
                # Legacy error handling for non-structured errors
                if "ArtifactRegistry" in type(e).__name__ or "Docker" in error_msg:
                    error_code = GAR_ERROR
                elif "Compute" in type(e).__name__:
                    error_code = COMPUTE_ERROR
                elif "Helm" in type(e).__name__ or "helm" in error_msg.lower():
                    error_code = HELM_ERROR
                    
            return JsonRpcResponse(
                id=request.id,
                error=JsonRpcError(
                    code=error_code,
                    message=error_msg,
                    data=error_data,
                ),
            )
    
    def _get_client(self) -> GCSClient:
        """Get the GCS client instance."""
        return get_gcs_client()

    def _get_gar_client(self) -> GARClient:
        """Get the GAR client instance."""
        return get_gar_client()

    def _get_compute_client(self) -> ComputeClient:
        """Get the Compute client instance."""
        return get_compute_client()

    def _get_sql_client(self) -> SQLClient:
        """Get the SQL client instance."""
        return get_sql_client()
    
    def _get_proxy_manager(self) -> SQLProxyManager:
        return get_proxy_manager()
    
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
    
    async def _download_object(self, params: dict) -> dict:
        """Download an object to local filesystem."""
        bucket = params.get("bucket")
        object_path = params.get("object_path")
        local_path = params.get("local_path")
        if local_path:
            local_path = os.path.expanduser(local_path)
        
        if not all([bucket, object_path, local_path]):
            raise TypeError("Missing required parameters: bucket, object_path, local_path")
        
        # Broadcast start
        await get_manager().broadcast({
            "type": "gcs_download_started",
            "data": {"bucket": bucket, "object_path": object_path}
        })

        client = self._get_client()
        result = await asyncio.to_thread(client.download_object, bucket, object_path, local_path)

        # Broadcast finish
        await get_manager().broadcast({
            "type": "gcs_download_finished",
            "data": {"bucket": bucket, "object_path": object_path, "local_path": local_path}
        })
        
        return result
    
    async def _upload_object(self, params: dict) -> dict:
        """Upload a local file to GCS."""
        bucket = params.get("bucket")
        object_path = params.get("object_path")
        local_path = params.get("local_path")
        if local_path:
            local_path = os.path.expanduser(local_path)
        content_type = params.get("content_type")
        
        if not all([bucket, object_path, local_path]):
            raise TypeError("Missing required parameters: bucket, object_path, local_path")
        
        # Broadcast start
        await get_manager().broadcast({
            "type": "gcs_upload_started",
            "data": {"bucket": bucket, "object_path": object_path}
        })
        
        client = self._get_client()
        result = await asyncio.to_thread(client.upload_object, bucket, object_path, local_path, content_type)

        # Broadcast finish
        await get_manager().broadcast({
            "type": "gcs_upload_finished",
            "data": {"bucket": bucket, "object_path": object_path}
        })
        
        return result
    
    async def _delete_object(self, params: dict) -> dict:
        """Delete an object."""
        bucket = params.get("bucket")
        object_path = params.get("object_path")
        
        if not bucket or not object_path:
            raise TypeError("Missing required parameters: bucket, object_path")
        
        client = self._get_client()
        result = await asyncio.to_thread(client.delete_object, bucket, object_path)
        
        # Broadcast finish
        await get_manager().broadcast({
            "type": "gcs_delete_finished",
            "data": {"bucket": bucket, "object_path": object_path}
        })
        
        return result
    
    def _create_folder(self, params: dict) -> dict:
        """Create a virtual folder."""
        bucket = params.get("bucket")
        folder_path = params.get("folder_path")
        
        if not bucket or not folder_path:
            raise TypeError("Missing required parameters: bucket, folder_path")
        
        client = self._get_client()
        return client.create_folder(bucket, folder_path)

    def _batch_delete_objects(self, params: dict) -> dict:
        """Delete multiple objects."""
        bucket = params.get("bucket")
        object_paths = params.get("object_paths")
        
        if not bucket or not object_paths:
            raise TypeError("Missing required parameters: bucket, object_paths")
            
        if not isinstance(object_paths, list):
            raise TypeError("object_paths must be a list")
            
        client = self._get_client()
        return client.batch_delete_objects(bucket, object_paths)

    def _generate_presigned_url(self, params: dict) -> dict:
        """Generate presigned URL."""
        bucket = params.get("bucket")
        object_path = params.get("object_path")
        expiration = params.get("expiration", 3600)
        method = params.get("method", "GET")
        
        if not bucket or not object_path:
            raise TypeError("Missing required parameters: bucket, object_path")
            
        client = self._get_client()
        url = client.generate_presigned_url(bucket, object_path, expiration, method)
        return {"url": url}

    def _update_object_metadata(self, params: dict) -> dict:
        """Update object metadata."""
        bucket = params.get("bucket")
        object_path = params.get("object_path")
        metadata = params.get("metadata")
        
        if not bucket or not object_path or metadata is None:
            raise TypeError("Missing required parameters: bucket, object_path, metadata")
            
        client = self._get_client()
        return client.update_object_metadata(bucket, object_path, metadata)

    def _set_bucket_lifecycle(self, params: dict) -> dict:
        """Set bucket lifecycle rules."""
        bucket = params.get("bucket")
        rules = params.get("rules")
        
        if not bucket or rules is None:
            raise TypeError("Missing required parameters: bucket, rules")
            
        client = self._get_client()
        return client.set_bucket_lifecycle(bucket, rules)

    def _copy_object(self, params: dict) -> dict:
        """Copy an object."""
        source_bucket = params.get("source_bucket")
        source_object = params.get("source_object")
        dest_bucket = params.get("dest_bucket")
        dest_object = params.get("dest_object")
        
        if not all([source_bucket, source_object, dest_bucket, dest_object]):
            raise TypeError("Missing required parameters: source_bucket, source_object, dest_bucket, dest_object")
            
        client = self._get_client()
        return client.copy_object(source_bucket, source_object, dest_bucket, dest_object)

    def _move_object(self, params: dict) -> dict:
        """Move an object."""
        source_bucket = params.get("source_bucket")
        source_object = params.get("source_object")
        dest_bucket = params.get("dest_bucket")
        dest_object = params.get("dest_object")
        
        if not all([source_bucket, source_object, dest_bucket, dest_object]):
            raise TypeError("Missing required parameters: source_bucket, source_object, dest_bucket, dest_object")
            
        client = self._get_client()
        return client.move_object(source_bucket, source_object, dest_bucket, dest_object)

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

    async def _gar_delete_package(self, params: dict) -> dict:
        package = params.get("package")
        if not package:
            raise TypeError("Missing parameter: package")
        
        client = self._get_gar_client()
        result = await asyncio.to_thread(client.delete_package, package)
        
        # Broadcast finish
        await get_manager().broadcast({
            "type": "gar_package_deleted",
            "data": {"package": package}
        })
        
        return result

    async def _gar_delete_tag(self, params: dict) -> dict:
        name = params.get("name") # Full tag resource name
        if not name:
            raise TypeError("Missing parameter: name")
            
        client = self._get_gar_client()
        result = await asyncio.to_thread(client.delete_tag, name)
        
        # Broadcast finish
        await get_manager().broadcast({
            "type": "gar_tag_deleted",
            "data": {"name": name}
        })
        
        return result

    async def _gar_pull(self, params: dict) -> dict:
        uri = params.get("uri")
        if not uri:
            raise TypeError("Missing parameter: uri")
        
        # Broadcast start
        await get_manager().broadcast({
            "type": "gar_pull_started",
            "data": {"uri": uri}
        })

        client = self._get_gar_client()
        result = await asyncio.to_thread(client.docker_pull, uri)
        
        # Broadcast finish
        await get_manager().broadcast({
            "type": "gar_pull_finished",
            "data": {"uri": uri}
        })
        
        return result

    async def _gar_push(self, params: dict) -> dict:
        uri = params.get("uri")
        if not uri:
            raise TypeError("Missing parameter: uri")
            
        # Broadcast start
        await get_manager().broadcast({
            "type": "gar_push_started",
            "data": {"uri": uri}
        })

        client = self._get_gar_client()
        result = await asyncio.to_thread(client.docker_push, uri)
        
        # Broadcast finish
        await get_manager().broadcast({
            "type": "gar_push_finished",
            "data": {"uri": uri}
        })
        
        return result

    def _gar_tag(self, params: dict) -> dict:
        source = params.get("source")
        target = params.get("target")
        if not source or not target:
            raise TypeError("Missing parameters: source, target")
        return self._get_gar_client().docker_tag(source, target)

    # --- Compute Method implementations ---

    def _compute_list_addresses(self, params: dict) -> dict:
        """List all static IP addresses (regional and global)."""
        client = self._get_compute_client()
        addresses = client.list_addresses()
        return {
            "addresses": [a.to_dict() for a in addresses],
            "count": len(addresses),
        }

    def _compute_reserve_address(self, params: dict) -> dict:
        """Reserve a new external static IP address."""
        region = params.get("region")
        name = params.get("name")
        
        if not region or not name:
            raise TypeError("Missing required parameters: region, name")
        
        client = self._get_compute_client()
        return client.reserve_address(region, name)

    def _compute_list_regions(self, params: dict) -> dict:
        """List available regions for IP address reservation."""
        client = self._get_compute_client()
        regions = client.list_regions()
        return {"regions": regions}

    def _compute_list_instances(self, params: dict) -> dict:
        """List all VM instances."""
        client = self._get_compute_client()
        instances = client.list_instances()
        return {
            "instances": [i.to_dict() for i in instances],
            "count": len(instances),
        }

    def _compute_start_instance(self, params: dict) -> dict:
        """Start a VM instance."""
        zone = params.get("zone")
        instance = params.get("instance")
        if not zone or not instance:
            raise TypeError("Missing required parameters: zone, instance")
        client = self._get_compute_client()
        return client.start_instance(zone, instance)

    def _compute_stop_instance(self, params: dict) -> dict:
        """Stop a VM instance."""
        zone = params.get("zone")
        instance = params.get("instance")
        if not zone or not instance:
            raise TypeError("Missing required parameters: zone, instance")
        client = self._get_compute_client()
        return client.stop_instance(zone, instance)

    def _compute_reset_instance(self, params: dict) -> dict:
        """Reset a VM instance."""
        zone = params.get("zone")
        instance = params.get("instance")
        if not zone or not instance:
            raise TypeError("Missing required parameters: zone, instance")
        client = self._get_compute_client()
        return client.reset_instance(zone, instance)

    def _compute_delete_instance(self, params: dict) -> dict:
        """Delete a VM instance."""
        zone = params.get("zone")
        instance = params.get("instance")
        if not zone or not instance:
            raise TypeError("Missing required parameters: zone, instance")
        client = self._get_compute_client()
        return client.delete_instance(zone, instance)
    
    def _get_config(self, params: dict) -> dict:
        """Get public server configuration."""
        return {
            "gcs_project": config.gcs_project,
            "impersonate_service_account": config.impersonate_service_account,
        }

    # --- SQL Method implementations ---

    def _sql_list_instances(self, params: dict) -> dict:
        client = self._get_sql_client()
        instances = client.list_instances()
        return {"instances": instances}

    def _sql_list_databases(self, params: dict) -> dict:
        instance = params.get("instance")
        if not instance:
            raise TypeError("Missing parameter: instance")
        client = self._get_sql_client()
        databases = client.list_databases(instance)
        return {"databases": databases}

    async def _sql_list_users(self, params: dict) -> dict:
        instance = params.get("instance")
        if not instance:
            raise ValueError("Instance name is required")
        client = self._get_sql_client()
        users = await asyncio.to_thread(client.list_users, instance)
        return {"users": users}

    async def _sql_create_database(self, params: dict) -> dict:
        instance = params.get("instance")
        name = params.get("name")
        charset = params.get("charset", "UTF8")
        collation = params.get("collation", "en_US.UTF8")
        
        if not instance or not name:
            raise ValueError("Instance and database name are required")
            
        client = self._get_sql_client()
        result = await asyncio.to_thread(client.create_database, instance, name, charset, collation)
        return {"operation": result}

    async def _sql_delete_database(self, params: dict) -> dict:
        instance = params.get("instance")
        name = params.get("name")
        
        if not instance or not name:
            raise ValueError("Instance and database name are required")
            
        client = self._get_sql_client()
        result = await asyncio.to_thread(client.delete_database, instance, name)
        return {"operation": result}

    async def _sql_create_user(self, params: dict) -> dict:
        instance = params.get("instance")
        name = params.get("name")
        password = params.get("password")
        host = params.get("host", "%")
        
        if not instance or not name or not password:
            raise ValueError("Instance, username and password are required")
            
        client = self._get_sql_client()
        result = await asyncio.to_thread(client.create_user, instance, name, password, host)
        return {"operation": result}

    async def _sql_delete_user(self, params: dict) -> dict:
        instance = params.get("instance")
        name = params.get("name")
        host = params.get("host", "%")
        
        if not instance or not name:
            raise ValueError("Instance and username are required")
            
        client = self._get_sql_client()
        result = await asyncio.to_thread(client.delete_user, instance, name, host)
        return {"operation": result}

    async def _sql_start_proxy(self, params: dict) -> dict:
        connection_name = params.get("connection_name")
        port = params.get("port", 0)
        db_type = params.get("db_type", "POSTGRES")
        
        if not connection_name:
            raise TypeError("Missing parameter: connection_name")
        
        manager = self._get_proxy_manager()
        actual_port = await manager.start_proxy(connection_name, port, db_type)
        
        # Broadcast update
        await get_manager().broadcast({
            "type": "sql_proxy_started",
            "data": {"connection_name": connection_name, "port": actual_port}
        })
        
        return {"port": actual_port, "connection_name": connection_name}

    async def _sql_stop_proxy(self, params: dict) -> dict:
        connection_name = params.get("connection_name")
        if not connection_name:
            raise TypeError("Missing parameter: connection_name")
            
        manager = self._get_proxy_manager()
        success = await manager.stop_proxy(connection_name)
        
        if success:
            await get_manager().broadcast({
                "type": "sql_proxy_stopped",
                "data": {"connection_name": connection_name}
            })
            
        return {"success": success}

    def _sql_list_proxies(self, params: dict) -> dict:
        manager = self._get_proxy_manager()
        proxies = manager.list_proxies()
        return {"proxies": proxies}

    def _sql_list_backups(self, params: dict) -> dict:
        """List backups for an instance."""
        instance = params.get("instance")
        if not instance:
            raise TypeError("Missing parameter: instance")
        client = self._get_sql_client()
        return {"backups": client.list_backups(instance)}

    async def _sql_create_backup(self, params: dict) -> dict:
        """Create a backup."""
        instance = params.get("instance")
        description = params.get("description", "")
        if not instance:
            raise TypeError("Missing parameter: instance")
        client = self._get_sql_client()
        result = await asyncio.to_thread(client.create_backup, instance, description)
        return {"operation": result}

    async def _sql_delete_backup(self, params: dict) -> dict:
        """Delete a backup."""
        instance = params.get("instance")
        backup_id = params.get("backup_id")
        if not instance or not backup_id:
            raise TypeError("Missing parameters: instance, backup_id")
        client = self._get_sql_client()
        result = await asyncio.to_thread(client.delete_backup, instance, backup_id)
        return {"operation": result}

    async def _sql_restore_backup(self, params: dict) -> dict:
        """Restore a backup."""
        instance = params.get("instance")
        backup_id = params.get("backup_id")
        if not instance or not backup_id:
            raise TypeError("Missing parameters: instance, backup_id")
        client = self._get_sql_client()
        result = await asyncio.to_thread(client.restore_backup, instance, backup_id)
        return {"operation": result}

    def _sql_get_connection_info(self, params: dict) -> dict:
        """Get connection info."""
        instance = params.get("instance")
        if not instance:
            raise TypeError("Missing parameter: instance")
        client = self._get_sql_client()
        return client.get_connection_info(instance)

    # --- K8s Method implementations ---

    def _get_k8s_client(self) -> K8sClient:
        """Get the K8s client instance."""
        return get_k8s_client()

    def _get_log_streamer(self) -> K8sLogStreamer:
        """Get the log streamer instance."""
        return get_log_streamer(self._get_k8s_client())

    def _k8s_list_clusters(self, params: dict) -> dict:
        """List GKE clusters."""
        location = params.get("location", "-")
        client = self._get_k8s_client()
        clusters = client.list_clusters(location)
        return {
            "clusters": [c.to_dict() for c in clusters],
            "count": len(clusters),
        }

    async def _k8s_connect(self, params: dict) -> dict:
        """Connect to a GKE cluster."""
        cluster = params.get("cluster")
        location = params.get("location")
        
        if not cluster or not location:
            raise TypeError("Missing required parameters: cluster, location")
        
        client = self._get_k8s_client()
        client.connect(cluster, location)
        
        # Initialize Helm client with the same cluster credentials
        # This ensures Helm uses the same Service Account authentication as K8s
        try:
            credentials = client.get_cluster_credentials()
            if credentials:
                from helm_client import initialize_helm_client
                await initialize_helm_client(
                    cluster_endpoint=credentials["endpoint"],
                    ca_cert_path=credentials["ca_cert_path"],
                    token=credentials["token"]
                )
                print("Helm client initialized successfully with cluster credentials")
            else:
                print("Warning: Could not get cluster credentials for Helm client")
        except Exception as e:
            # Log the error but don't fail the K8s connection
            # Helm functionality will be unavailable but K8s operations will still work
            print(f"Warning: Failed to initialize Helm client: {str(e)}")
            import traceback
            traceback.print_exc()
        
        return {
            "connected": True,
            "cluster": client.connected_cluster,
        }

    def _k8s_disconnect(self, params: dict) -> dict:
        """Disconnect from current cluster."""
        client = self._get_k8s_client()
        # Stop all log streams first
        self._get_log_streamer().stop_all()
        client.disconnect()
        
        # Reset Helm client when disconnecting from cluster
        # This ensures Helm client will be re-initialized on next connection
        reset_helm_client()
        
        return {"disconnected": True}

    def _k8s_connection_status(self, params: dict) -> dict:
        """Get current connection status."""
        client = self._get_k8s_client()
        return {
            "connected": client.is_connected,
            "cluster": client.connected_cluster,
        }

    def _k8s_list_namespaces(self, params: dict) -> dict:
        """List namespaces."""
        client = self._get_k8s_client()
        namespaces = client.list_namespaces()
        return {
            "namespaces": [ns.to_dict() for ns in namespaces],
            "count": len(namespaces),
        }

    def _k8s_list_pods(self, params: dict) -> dict:
        """List pods."""
        namespace = params.get("namespace", "")
        label_selector = params.get("label_selector", "")
        
        client = self._get_k8s_client()
        pods = client.list_pods(namespace, label_selector)
        return {
            "pods": [p.to_dict() for p in pods],
            "count": len(pods),
        }

    def _k8s_list_services(self, params: dict) -> dict:
        """List services."""
        namespace = params.get("namespace", "")
        
        client = self._get_k8s_client()
        services = client.list_services(namespace)
        return {
            "services": [s.to_dict() for s in services],
            "count": len(services),
        }

    def _k8s_list_ingresses(self, params: dict) -> dict:
        """List ingresses."""
        namespace = params.get("namespace", "")
        
        client = self._get_k8s_client()
        ingresses = client.list_ingresses(namespace)
        return {
            "ingresses": [i.to_dict() for i in ingresses],
            "count": len(ingresses),
        }

    def _k8s_list_deployments(self, params: dict) -> dict:
        """List deployments."""
        namespace = params.get("namespace", "")
        
        client = self._get_k8s_client()
        deployments = client.list_deployments(namespace)
        return {
            "deployments": [d.to_dict() for d in deployments],
            "count": len(deployments),
        }
    
    def _k8s_get_resources(self, params: dict) -> dict:
        """Get resources of any Kubernetes kind."""
        kind = params.get("kind")
        namespace = params.get("namespace")
        all_namespaces = params.get("all_namespaces", False)
        
        if not kind:
            raise TypeError("Missing required parameter: kind")
        
        client = self._get_k8s_client()
        resources = client.get_resources(kind, namespace, all_namespaces)
        return {
            "resources": resources,
            "count": len(resources),
        }
    
    def _k8s_list_api_resources(self, params: dict) -> dict:
        """List all available API resources (like kubectl api-resources)."""
        client = self._get_k8s_client()
        resources = client.list_api_resources()
        return {
            "resources": resources,
            "count": len(resources),
        }

    def _k8s_scale_deployment(self, params: dict) -> dict:
        """Scale a deployment."""
        namespace = params.get("namespace")
        name = params.get("name")
        replicas = params.get("replicas")
        
        if not namespace or not name or replicas is None:
            raise TypeError("Missing required parameters: namespace, name, replicas")
        
        client = self._get_k8s_client()
        return client.scale_deployment(namespace, name, int(replicas))

    def _k8s_pod_exec(self, params: dict) -> dict:
        """Execute a command in a pod."""
        namespace = params.get("namespace")
        name = params.get("name")
        command = params.get("command")
        container = params.get("container")
        
        if not namespace or not name or not command:
            raise TypeError("Missing required parameters: namespace, name, command")
            
        if isinstance(command, str):
            command = [command]
            
        client = self._get_k8s_client()
        output = client.pod_exec(namespace, name, command, container)
        return {"output": output}

    def _k8s_apply_manifest(self, params: dict) -> dict:
        """Apply a Kubernetes manifest."""
        namespace = params.get("namespace", "default")
        manifest = params.get("manifest")
        
        if not manifest:
            raise TypeError("Missing required parameter: manifest")
            
        client = self._get_k8s_client()
        return client.apply_manifest(namespace, manifest)

    def _k8s_resource_metrics(self, params: dict) -> dict:
        """Get resource metrics."""
        client = self._get_k8s_client()
        return client.get_resource_metrics()

    def _k8s_get_yaml(self, params: dict) -> dict:
        """Get YAML representation of a resource."""
        kind = params.get("kind")
        name = params.get("name")
        namespace = params.get("namespace", "default")
        
        if not kind or not name:
            raise TypeError("Missing required parameters: kind, name")
        
        client = self._get_k8s_client()
        yaml_content = client.get_resource_yaml(kind, name, namespace)
        return {"yaml": yaml_content}

    def _k8s_pod_logs(self, params: dict) -> dict:
        """Get pod logs (non-streaming)."""
        name = params.get("name")
        namespace = params.get("namespace")
        container = params.get("container")
        tail_lines = params.get("tail_lines", 100)
        
        if not name or not namespace:
            raise TypeError("Missing required parameters: name, namespace")
        
        client = self._get_k8s_client()
        logs = client.get_pod_logs(name, namespace, container, tail_lines)
        return {"logs": logs}

    async def _k8s_start_log_stream(self, params: dict) -> dict:
        """Start streaming logs via WebSocket."""
        namespace = params.get("namespace", "")
        label_selector = params.get("label_selector", "")
        pod_name = params.get("pod_name", "")
        container = params.get("container")
        text_filter = params.get("text_filter", "")
        tail_lines = params.get("tail_lines", 50)
        
        async def on_log(stream_id: str, pod: str, container: str, line: str):
            await get_manager().broadcast({
                "type": "k8s_log",
                "data": {
                    "stream_id": stream_id,
                    "pod": pod,
                    "container": container,
                    "line": line,
                }
            })
        
        streamer = self._get_log_streamer()
        stream_id = await streamer.start_stream(
            namespace=namespace,
            label_selector=label_selector,
            pod_name=pod_name,
            container=container,
            text_filter=text_filter,
            tail_lines=tail_lines,
            on_log=on_log,
        )
        
        # Broadcast stream started
        await get_manager().broadcast({
            "type": "k8s_log_stream_started",
            "data": {"stream_id": stream_id}
        })
        
        return {"stream_id": stream_id}

    def _k8s_stop_log_stream(self, params: dict) -> dict:
        """Stop a log stream."""
        stream_id = params.get("stream_id")
        if not stream_id:
            raise TypeError("Missing parameter: stream_id")
        
        streamer = self._get_log_streamer()
        success = streamer.stop_stream(stream_id)
        return {"success": success}

    def _k8s_list_log_streams(self, params: dict) -> dict:
        """List active log streams."""
        streamer = self._get_log_streamer()
        streams = streamer.list_streams()
        return {"streams": streams}

    # --- Helm Method implementations ---

    def _get_helm_client(self) -> HelmClient:
        """Get the Helm client instance."""
        return get_helm_client()

    async def _helm_list_releases(self, params: dict) -> dict:
        """List Helm releases."""
        namespace = params.get("namespace")
        all_namespaces = params.get("all_namespaces", False)
        
        client = self._get_helm_client()
        releases = await client.list_releases(namespace, all_namespaces)
        return {
            "releases": releases,
            "count": len(releases),
        }

    async def _helm_get_release_details(self, params: dict) -> dict:
        """Get detailed information about a Helm release."""
        name = params.get("name")
        namespace = params.get("namespace", "default")
        
        if not name:
            raise TypeError("Missing required parameter: name")
        
        client = self._get_helm_client()
        details = await client.get_release_details(name, namespace)
        return {"release": details}

    async def _helm_install_chart(self, params: dict) -> dict:
        """Install a Helm chart."""
        release_name = params.get("release_name")
        chart_ref = params.get("chart_ref")
        namespace = params.get("namespace", "default")
        values = params.get("values")
        repo = params.get("repo")
        version = params.get("version")
        create_namespace = params.get("create_namespace", False)
        wait = params.get("wait", True)
        timeout = params.get("timeout", 300)
        
        if not release_name or not chart_ref:
            raise TypeError("Missing required parameters: release_name, chart_ref")
        
        client = self._get_helm_client()
        result = await client.install_chart(
            release_name=release_name,
            chart_ref=chart_ref,
            namespace=namespace,
            values=values,
            repo=repo,
            version=version,
            create_namespace=create_namespace,
            wait=wait,
            timeout=timeout
        )
        
        # Broadcast installation finished
        await get_manager().broadcast({
            "type": "helm_install_finished",
            "data": {"release_name": release_name, "namespace": namespace}
        })
        
        return result

    async def _helm_upgrade_release(self, params: dict) -> dict:
        """Upgrade a Helm release."""
        release_name = params.get("release_name")
        chart_ref = params.get("chart_ref")
        namespace = params.get("namespace", "default")
        values = params.get("values")
        version = params.get("version")
        wait = params.get("wait", True)
        timeout = params.get("timeout", 300)
        
        if not release_name:
            raise TypeError("Missing required parameter: release_name")
        
        client = self._get_helm_client()
        result = await client.upgrade_release(
            release_name=release_name,
            chart_ref=chart_ref,
            namespace=namespace,
            values=values,
            version=version,
            wait=wait,
            timeout=timeout
        )
        
        # Broadcast upgrade finished
        await get_manager().broadcast({
            "type": "helm_upgrade_finished",
            "data": {"release_name": release_name, "namespace": namespace}
        })
        
        return result

    async def _helm_rollback_release(self, params: dict) -> dict:
        """Rollback a Helm release to a specific revision."""
        release_name = params.get("release_name")
        revision = params.get("revision")
        namespace = params.get("namespace", "default")
        wait = params.get("wait", True)
        
        if not release_name or revision is None:
            raise TypeError("Missing required parameters: release_name, revision")
        
        client = self._get_helm_client()
        result = await client.rollback_release(
            release_name=release_name,
            revision=int(revision),
            namespace=namespace,
            wait=wait
        )
        
        # Broadcast rollback finished
        await get_manager().broadcast({
            "type": "helm_rollback_finished",
            "data": {"release_name": release_name, "namespace": namespace, "revision": revision}
        })
        
        return result

    async def _helm_uninstall_release(self, params: dict) -> dict:
        """Uninstall a Helm release."""
        release_name = params.get("release_name")
        namespace = params.get("namespace", "default")
        wait = params.get("wait", True)
        
        if not release_name:
            raise TypeError("Missing required parameter: release_name")
        
        client = self._get_helm_client()
        success = await client.uninstall_release(
            release_name=release_name,
            namespace=namespace,
            wait=wait
        )
        
        # Broadcast uninstall finished
        await get_manager().broadcast({
            "type": "helm_uninstall_finished",
            "data": {"release_name": release_name, "namespace": namespace}
        })
        
        return {"success": success}

    async def _helm_list_repositories(self, params: dict) -> dict:
        """List configured Helm chart repositories."""
        client = self._get_helm_client()
        repositories = await client.list_repositories()
        return {
            "repositories": repositories,
            "count": len(repositories),
        }

    async def _helm_add_repository(self, params: dict) -> dict:
        """Add a Helm chart repository."""
        name = params.get("name")
        url = params.get("url")
        
        if not name or not url:
            raise TypeError("Missing required parameters: name, url")
        
        client = self._get_helm_client()
        success = await client.add_repository(name, url)
        
        # Broadcast repository added
        await get_manager().broadcast({
            "type": "helm_repository_added",
            "data": {"name": name, "url": url}
        })
        
        return {"success": success}

    async def _helm_remove_repository(self, params: dict) -> dict:
        """Remove a Helm chart repository."""
        name = params.get("name")
        
        if not name:
            raise TypeError("Missing required parameter: name")
        
        client = self._get_helm_client()
        success = await client.remove_repository(name)
        
        # Broadcast repository removed
        await get_manager().broadcast({
            "type": "helm_repository_removed",
            "data": {"name": name}
        })
        
        return {"success": success}

    async def _helm_search_charts(self, params: dict) -> dict:
        """Search for Helm charts."""
        keyword = params.get("keyword")
        repo = params.get("repo")
        
        if not keyword:
            raise TypeError("Missing required parameter: keyword")
        
        client = self._get_helm_client()
        charts = await client.search_charts(keyword, repo)
        return {
            "charts": charts,
            "count": len(charts),
        }


# Singleton handler instance
handler = JsonRpcHandler()
