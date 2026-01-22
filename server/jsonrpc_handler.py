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
from config import config
from websocket_manager import get_manager


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
            # Compute methods
            "compute_list_addresses": self._compute_list_addresses,
            "compute_reserve_address": self._compute_reserve_address,
            "compute_list_regions": self._compute_list_regions,
            "compute_list_instances": self._compute_list_instances,
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
            "k8s_get_yaml": self._k8s_get_yaml,
            "k8s_pod_logs": self._k8s_pod_logs,
            "k8s_start_log_stream": self._k8s_start_log_stream,
            "k8s_stop_log_stream": self._k8s_stop_log_stream,
            "k8s_list_log_streams": self._k8s_list_log_streams,
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
            error_code = GCS_ERROR
            if "ArtifactRegistry" in type(e).__name__ or "Docker" in str(e):
                error_code = GAR_ERROR
            elif "Compute" in type(e).__name__:
                error_code = COMPUTE_ERROR
                
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

    def _k8s_connect(self, params: dict) -> dict:
        """Connect to a GKE cluster."""
        cluster = params.get("cluster")
        location = params.get("location")
        
        if not cluster or not location:
            raise TypeError("Missing required parameters: cluster, location")
        
        client = self._get_k8s_client()
        client.connect(cluster, location)
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


# Singleton handler instance
handler = JsonRpcHandler()
