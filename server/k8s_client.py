"""Google Kubernetes Engine client wrapper.

Provides operations for connecting to GKE clusters and interacting with
Kubernetes resources using Service Account authentication.
"""

import base64
import os
import tempfile
from dataclasses import dataclass
from functools import wraps
from typing import Any

import yaml
from google.auth.transport.requests import Request
from google.cloud import container_v1
from google.oauth2 import service_account
from kubernetes import client as k8s
from kubernetes.client.rest import ApiException

from config import config


# --- Data Transfer Objects ---

@dataclass
class ClusterInfo:
    """GKE Cluster information."""
    name: str
    location: str
    status: str
    endpoint: str
    node_count: int
    current_node_count: int
    version: str
    
    def to_dict(self) -> dict[str, Any]:
        return {
            "name": self.name,
            "location": self.location,
            "status": self.status,
            "endpoint": self.endpoint,
            "nodeCount": self.node_count,
            "currentNodeCount": self.current_node_count,
            "version": self.version,
        }


@dataclass
class NamespaceInfo:
    """Kubernetes Namespace information."""
    name: str
    status: str
    labels: dict[str, str]
    
    def to_dict(self) -> dict[str, Any]:
        return {
            "name": self.name,
            "status": self.status,
            "labels": self.labels,
        }


@dataclass
class PodInfo:
    """Kubernetes Pod information."""
    name: str
    namespace: str
    status: str
    phase: str
    ip: str
    node: str
    containers: list[str]
    ready: str
    restarts: int
    age: str
    
    def to_dict(self) -> dict[str, Any]:
        return {
            "name": self.name,
            "namespace": self.namespace,
            "status": self.status,
            "phase": self.phase,
            "ip": self.ip or "",
            "node": self.node or "",
            "containers": self.containers,
            "ready": self.ready,
            "restarts": self.restarts,
            "age": self.age,
        }


@dataclass
class ServiceInfo:
    """Kubernetes Service information."""
    name: str
    namespace: str
    type: str
    cluster_ip: str
    external_ip: str
    ports: list[dict]
    age: str
    
    def to_dict(self) -> dict[str, Any]:
        return {
            "name": self.name,
            "namespace": self.namespace,
            "type": self.type,
            "clusterIp": self.cluster_ip or "",
            "externalIp": self.external_ip or "",
            "ports": self.ports,
            "age": self.age,
        }


@dataclass
class IngressInfo:
    """Kubernetes Ingress information."""
    name: str
    namespace: str
    class_name: str
    hosts: list[str]
    addresses: list[str]
    age: str
    
    def to_dict(self) -> dict[str, Any]:
        return {
            "name": self.name,
            "namespace": self.namespace,
            "className": self.class_name or "",
            "hosts": self.hosts,
            "addresses": self.addresses,
            "age": self.age,
        }


@dataclass
class DeploymentInfo:
    """Kubernetes Deployment information."""
    name: str
    namespace: str
    ready: str
    up_to_date: int
    available: int
    age: str
    
    def to_dict(self) -> dict[str, Any]:
        return {
            "name": self.name,
            "namespace": self.namespace,
            "ready": self.ready,
            "upToDate": self.up_to_date,
            "available": self.available,
            "age": self.age,
        }


# --- Helper Functions ---

def _get_sa_path() -> str:
    """Get Service Account JSON path."""
    path = os.getenv("GOOGLE_APPLICATION_CREDENTIALS")
    if not path:
        raise ValueError("GOOGLE_APPLICATION_CREDENTIALS environment variable not set")
    if not os.path.exists(path):
        raise FileNotFoundError(f"Service account file not found: {path}")
    return path


def _get_project_from_sa() -> str | None:
    """Extract project ID from SA JSON file."""
    import json
    sa_path = _get_sa_path()
    with open(sa_path, "r") as f:
        data = json.load(f)
        return data.get("project_id")


def _format_age(creation_timestamp) -> str:
    """Format age from creation timestamp."""
    from datetime import datetime, timezone
    if not creation_timestamp:
        return ""
    now = datetime.now(timezone.utc)
    delta = now - creation_timestamp
    days = delta.days
    hours = delta.seconds // 3600
    minutes = (delta.seconds % 3600) // 60
    
    if days > 0:
        return f"{days}d"
    elif hours > 0:
        return f"{hours}h"
    else:
        return f"{minutes}m"


def _parse_quantity(quantity: str | int | float) -> float:
    """Parse k8s quantity string to float base value.
    
    CPU: returns cores (e.g. 100m -> 0.1, 1 -> 1.0)
    Memory: returns bytes (e.g. 1Ki -> 1024, 1Mi -> 1048576)
    """
    if isinstance(quantity, (int, float)):
        return float(quantity)
        
    q = str(quantity)
    if q.endswith('n'):
        return float(q[:-1]) / 1_000_000_000
    elif q.endswith('u'):
        return float(q[:-1]) / 1_000_000
    elif q.endswith('m'):
        return float(q[:-1]) / 1_000
    elif q.endswith('Ki'):
        return float(q[:-2]) * 1024
    elif q.endswith('Mi'):
        return float(q[:-2]) * 1024**2
    elif q.endswith('Gi'):
        return float(q[:-2]) * 1024**3
    elif q.endswith('Ti'):
        return float(q[:-2]) * 1024**4
    elif q.endswith('Pi'):
        return float(q[:-2]) * 1024**5
    elif q.endswith('Ei'):
        return float(q[:-2]) * 1024**6
    elif q.endswith('k'):
        return float(q[:-1]) * 1000
    elif q.endswith('M'):
        return float(q[:-1]) * 1000**2
    elif q.endswith('G'):
        return float(q[:-1]) * 1000**3
    elif q.endswith('T'):
        return float(q[:-1]) * 1000**4
    elif q.endswith('P'):
        return float(q[:-1]) * 1000**5
    elif q.endswith('E'):
        return float(q[:-1]) * 1000**6
        
    try:
        return float(q)
    except ValueError:
        return 0.0

def auto_refresh_token(method):
    """Decorator to refresh token on 401 and retry once."""
    @wraps(method)
    def wrapper(self, *args, **kwargs):
        try:
            return method(self, *args, **kwargs)
        except ApiException as e:
            if e.status == 401:
                self._refresh_token()
                return method(self, *args, **kwargs)
            raise
    return wrapper


# --- Main Client ---

class K8sClient:
    """Kubernetes client for GKE with Service Account auth."""
    
    SCOPES = ['https://www.googleapis.com/auth/cloud-platform']
    
    def __init__(self, project: str | None = None):
        self._project = project or config.gcs_project or _get_project_from_sa()
        if not self._project:
            raise ValueError("GCP Project ID is required for K8s operations")
        
        # Load SA credentials
        self._credentials = service_account.Credentials.from_service_account_file(
            _get_sa_path(), scopes=self.SCOPES
        )
        
        # GKE client for listing/getting clusters
        self._gke_client = container_v1.ClusterManagerClient(credentials=self._credentials)
        
        # K8s client state (set after connect)
        self._config: k8s.Configuration | None = None
        self._api_client: k8s.ApiClient | None = None
        self._connected_cluster: dict | None = None
        self._ca_cert_path: str | None = None
    
    @property
    def is_connected(self) -> bool:
        return self._api_client is not None
    
    @property
    def connected_cluster(self) -> dict | None:
        return self._connected_cluster
    
    def _refresh_token(self):
        """Refresh the OAuth2 token and update API client."""
        self._credentials.refresh(Request())
        if self._config:
            self._config.api_key = {"authorization": f"Bearer {self._credentials.token}"}
            self._api_client = k8s.ApiClient(self._config)
    
    def _ensure_connected(self):
        """Raise error if not connected to a cluster."""
        if not self.is_connected:
            raise RuntimeError("Not connected to a cluster. Call connect() first.")
    
    @property
    def core_api(self) -> k8s.CoreV1Api:
        self._ensure_connected()
        return k8s.CoreV1Api(self._api_client)
    
    @property
    def apps_api(self) -> k8s.AppsV1Api:
        self._ensure_connected()
        return k8s.AppsV1Api(self._api_client)
    
    @property
    def networking_api(self) -> k8s.NetworkingV1Api:
        self._ensure_connected()
        return k8s.NetworkingV1Api(self._api_client)
    
    # --- Cluster Operations ---
    
    def list_clusters(self, location: str = "-") -> list[ClusterInfo]:
        """List GKE clusters. Use '-' for all locations."""
        parent = f"projects/{self._project}/locations/{location}"
        response = self._gke_client.list_clusters(parent=parent)
        
        clusters = []
        for c in response.clusters:
            clusters.append(ClusterInfo(
                name=c.name,
                location=c.location,
                status=c.status.name if hasattr(c.status, 'name') else str(c.status),
                endpoint=c.endpoint,
                node_count=c.initial_node_count or 0,
                current_node_count=c.current_node_count or 0,
                version=c.current_master_version or "",
            ))
        return clusters
    
    def connect(self, cluster_name: str, location: str):
        """Connect to a GKE cluster."""
        # Get cluster info
        name = f"projects/{self._project}/locations/{location}/clusters/{cluster_name}"
        cluster = self._gke_client.get_cluster(name=name)
        
        # Decode and save CA cert
        ca_cert = base64.b64decode(cluster.master_auth.cluster_ca_certificate)
        with tempfile.NamedTemporaryFile(delete=False, suffix='.crt', mode='wb') as f:
            f.write(ca_cert)
            self._ca_cert_path = f.name
        
        # Build K8s configuration
        self._config = k8s.Configuration()
        self._config.host = f"https://{cluster.endpoint}"
        self._config.ssl_ca_cert = self._ca_cert_path
        
        # Get fresh token
        self._credentials.refresh(Request())
        self._config.api_key = {"authorization": f"Bearer {self._credentials.token}"}
        
        # Create API client
        self._api_client = k8s.ApiClient(self._config)
        self._connected_cluster = {
            "name": cluster_name,
            "location": location,
            "endpoint": cluster.endpoint,
        }
    
    def disconnect(self):
        """Disconnect from current cluster."""
        if self._ca_cert_path and os.path.exists(self._ca_cert_path):
            os.unlink(self._ca_cert_path)
        self._api_client = None
        self._config = None
        self._connected_cluster = None
        self._ca_cert_path = None
    
    def get_cluster_credentials(self) -> dict[str, str] | None:
        """Get current cluster credentials for Helm client.
        
        Returns:
            Dict with keys: endpoint, ca_cert_path, token
            None if not connected
        """
        if not self._config or not self._ca_cert_path or not self._credentials:
            return None
        
        # Ensure token is fresh
        self._credentials.refresh(Request())
        
        return {
            "endpoint": self._config.host,
            "ca_cert_path": self._ca_cert_path,
            "token": self._credentials.token,
        }
    
    # --- Namespace Operations ---
    
    @auto_refresh_token
    def list_namespaces(self) -> list[NamespaceInfo]:
        """List all namespaces."""
        items = self.core_api.list_namespace().items
        return [
            NamespaceInfo(
                name=ns.metadata.name,
                status=ns.status.phase if ns.status else "",
                labels=ns.metadata.labels or {},
            )
            for ns in items
        ]
    
    # --- Pod Operations ---
    
    def _pod_status(self, pod) -> str:
        """Get pod status string (similar to kubectl)."""
        if pod.metadata.deletion_timestamp:
            return "Terminating"
        
        if pod.status.container_statuses:
            for cs in pod.status.container_statuses:
                if cs.state.waiting:
                    return cs.state.waiting.reason or "Waiting"
                if cs.state.terminated:
                    return cs.state.terminated.reason or "Terminated"
        
        return pod.status.phase or "Unknown"
    
    def _pod_ready_count(self, pod) -> str:
        """Get ready/total container count."""
        if not pod.status.container_statuses:
            return "0/0"
        total = len(pod.status.container_statuses)
        ready = sum(1 for cs in pod.status.container_statuses if cs.ready)
        return f"{ready}/{total}"
    
    def _pod_restarts(self, pod) -> int:
        """Get total restart count."""
        if not pod.status.container_statuses:
            return 0
        return sum(cs.restart_count for cs in pod.status.container_statuses)
    
    @auto_refresh_token
    def list_pods(self, namespace: str = "", label_selector: str = "") -> list[PodInfo]:
        """List pods. Empty namespace = all namespaces."""
        kwargs = {}
        if label_selector:
            kwargs["label_selector"] = label_selector
        
        if namespace:
            items = self.core_api.list_namespaced_pod(namespace, **kwargs).items
        else:
            items = self.core_api.list_pod_for_all_namespaces(**kwargs).items
        
        return [
            PodInfo(
                name=p.metadata.name,
                namespace=p.metadata.namespace,
                status=self._pod_status(p),
                phase=p.status.phase or "",
                ip=p.status.pod_ip,
                node=p.spec.node_name,
                containers=[c.name for c in p.spec.containers],
                ready=self._pod_ready_count(p),
                restarts=self._pod_restarts(p),
                age=_format_age(p.metadata.creation_timestamp),
            )
            for p in items
        ]
    
    @auto_refresh_token
    def get_pod_logs(self, name: str, namespace: str, 
                     container: str | None = None, 
                     tail_lines: int = 100) -> str:
        """Get pod logs (non-streaming)."""
        kwargs = {"tail_lines": tail_lines}
        if container:
            kwargs["container"] = container
        return self.core_api.read_namespaced_pod_log(name, namespace, **kwargs)
    
    # --- Service Operations ---
    
    def _format_ports(self, ports) -> list[dict]:
        """Format service ports."""
        if not ports:
            return []
        return [
            {
                "port": p.port,
                "targetPort": str(p.target_port) if p.target_port else "",
                "protocol": p.protocol or "TCP",
                "nodePort": p.node_port,
            }
            for p in ports
        ]
    
    def _format_external_ip(self, svc) -> str:
        """Get external IP for service."""
        if svc.spec.type == "LoadBalancer" and svc.status.load_balancer.ingress:
            ingress = svc.status.load_balancer.ingress[0]
            return ingress.ip or ingress.hostname or ""
        if svc.spec.external_i_ps:
            return ",".join(svc.spec.external_i_ps)
        return ""
    
    @auto_refresh_token
    def list_services(self, namespace: str = "") -> list[ServiceInfo]:
        """List services."""
        if namespace:
            items = self.core_api.list_namespaced_service(namespace).items
        else:
            items = self.core_api.list_service_for_all_namespaces().items
        
        return [
            ServiceInfo(
                name=s.metadata.name,
                namespace=s.metadata.namespace,
                type=s.spec.type or "",
                cluster_ip=s.spec.cluster_ip,
                external_ip=self._format_external_ip(s),
                ports=self._format_ports(s.spec.ports),
                age=_format_age(s.metadata.creation_timestamp),
            )
            for s in items
        ]
    
    # --- Ingress Operations ---
    
    @auto_refresh_token
    def list_ingresses(self, namespace: str = "") -> list[IngressInfo]:
        """List ingresses."""
        if namespace:
            items = self.networking_api.list_namespaced_ingress(namespace).items
        else:
            items = self.networking_api.list_ingress_for_all_namespaces().items
        
        result = []
        for ing in items:
            hosts = []
            if ing.spec.rules:
                hosts = [r.host for r in ing.spec.rules if r.host]
            
            addresses = []
            if ing.status.load_balancer and ing.status.load_balancer.ingress:
                for lb in ing.status.load_balancer.ingress:
                    if lb.ip:
                        addresses.append(lb.ip)
                    elif lb.hostname:
                        addresses.append(lb.hostname)
            
            result.append(IngressInfo(
                name=ing.metadata.name,
                namespace=ing.metadata.namespace,
                class_name=ing.spec.ingress_class_name,
                hosts=hosts,
                addresses=addresses,
                age=_format_age(ing.metadata.creation_timestamp),
            ))
        return result
    
    # --- Deployment Operations ---
    
    @auto_refresh_token
    def list_deployments(self, namespace: str = "") -> list[DeploymentInfo]:
        """List deployments."""
        if namespace:
            items = self.apps_api.list_namespaced_deployment(namespace).items
        else:
            items = self.apps_api.list_deployment_for_all_namespaces().items
        
        return [
            DeploymentInfo(
                name=d.metadata.name,
                namespace=d.metadata.namespace,
                ready=f"{d.status.ready_replicas or 0}/{d.status.replicas or 0}",
                up_to_date=d.status.updated_replicas or 0,
                available=d.status.available_replicas or 0,
                age=_format_age(d.metadata.creation_timestamp),
            )
            for d in items
        ]
    
    # --- YAML Operations ---
    
    @auto_refresh_token
    def get_resource_yaml(self, kind: str, name: str, namespace: str) -> str:
        """Get YAML representation of a resource."""
        kind_lower = kind.lower()
        
        if kind_lower == "pod":
            resource = self.core_api.read_namespaced_pod(name, namespace)
        elif kind_lower == "service":
            resource = self.core_api.read_namespaced_service(name, namespace)
        elif kind_lower == "ingress":
            resource = self.networking_api.read_namespaced_ingress(name, namespace)
        elif kind_lower == "deployment":
            resource = self.apps_api.read_namespaced_deployment(name, namespace)
        elif kind_lower == "namespace":
            resource = self.core_api.read_namespace(name)
        elif kind_lower == "configmap":
            resource = self.core_api.read_namespaced_config_map(name, namespace)
        elif kind_lower == "secret":
            resource = self.core_api.read_namespaced_secret(name, namespace)
            # Mask secret data
            if resource.data:
                resource.data = {k: "***REDACTED***" for k in resource.data}
        else:
            raise ValueError(f"Unsupported resource kind: {kind}")
        
        # Convert to dict and dump as YAML
        resource_dict = resource.to_dict()
        # Clean up None values and empty dicts for cleaner YAML
        return yaml.dump(resource_dict, default_flow_style=False, allow_unicode=True)

    @auto_refresh_token
    def scale_deployment(self, namespace: str, name: str, replicas: int) -> dict:
        """Scale a deployment."""
        body = {"spec": {"replicas": replicas}}
        self.apps_api.patch_namespaced_deployment_scale(name, namespace, body)
        return {"success": True, "name": name, "replicas": replicas}

    @auto_refresh_token
    def pod_exec(self, namespace: str, name: str, command: list[str], container: str | None = None) -> str:
        """Execute a command in a pod container."""
        from kubernetes.stream import stream
        
        kwargs = {
            "name": name,
            "namespace": namespace,
            "command": command,
            "stderr": True,
            "stdin": False,
            "stdout": True,
            "tty": False,
        }
        if container:
            kwargs["container"] = container
            
        resp = stream(self.core_api.connect_get_namespaced_pod_exec, **kwargs)
        return resp

    @auto_refresh_token
    def apply_manifest(self, namespace: str, manifest: str) -> dict:
        """Apply a Kubernetes manifest (YAML)."""
        # Parse YAML
        try:
            # list in case of multiple documents
            objects = list(yaml.safe_load_all(manifest))
        except yaml.YAMLError as e:
            raise ValueError(f"Invalid YAML: {e}")
            
        results = []
        for obj in objects:
            if not obj:
                continue
                
            kind = obj.get("kind")
            metadata = obj.get("metadata", {})
            name = metadata.get("name")
            # Namespace in yaml overrides param, else use param
            ns = metadata.get("namespace", namespace)
            
            if not kind or not name:
                results.append({"error": "Missing kind or metadata.name", "object": obj})
                continue

            # This is a simplified apply strategy: Try Patch, if 404 then Create
            # A proper apply would use Server-Side Apply or kubectl logic
            
            try:
                # Try to map kind to API method
                api_instance = None
                create_method = None
                patch_method = None
                
                # Determine API and methods based on Kind
                # This is a partial mapping, can be expanded
                if kind == "Deployment":
                    api_instance = self.apps_api
                    create_method = api_instance.create_namespaced_deployment
                    patch_method = api_instance.patch_namespaced_deployment
                elif kind == "Service":
                    api_instance = self.core_api
                    create_method = api_instance.create_namespaced_service
                    patch_method = api_instance.patch_namespaced_service
                elif kind == "Pod":
                    api_instance = self.core_api
                    create_method = api_instance.create_namespaced_pod
                    patch_method = api_instance.patch_namespaced_pod
                elif kind == "ConfigMap":
                    api_instance = self.core_api
                    create_method = api_instance.create_namespaced_config_map
                    patch_method = api_instance.patch_namespaced_config_map
                elif kind == "Secret":
                    api_instance = self.core_api
                    create_method = api_instance.create_namespaced_secret
                    patch_method = api_instance.patch_namespaced_secret
                elif kind == "Ingress":
                    api_instance = self.networking_api
                    create_method = api_instance.create_namespaced_ingress
                    patch_method = api_instance.patch_namespaced_ingress
                # Add more as needed...
                
                if api_instance:
                    try:
                        # Try patch first (update)
                        patch_method(name, ns, obj)
                        results.append({"name": name, "kind": kind, "action": "configured"})
                    except ApiException as e:
                        if e.status == 404:
                            # Not found, create
                            create_method(ns, obj)
                            results.append({"name": name, "kind": kind, "action": "created"})
                        else:
                            raise e
                else:
                    # Fallback or error for unsupported kinds in this simple implementation
                    # Alternatively, use dynamic client if available, but we use typed clients here.
                    results.append({"name": name, "kind": kind, "error": "Unsupported kind for apply"})
                    
            except Exception as e:
                results.append({"name": name, "kind": kind, "error": str(e)})
                
        return {"results": results}

    @auto_refresh_token
    def get_resource_metrics(self) -> dict:
        """Get Pod and Node metrics with percentages."""
        # Using CustomObjectsApi to access metrics.k8s.io
        custom_api = k8s.CustomObjectsApi(self._api_client)
        
        metrics = {}
        
        try:
            # 1. Fetch Node capacity/allocatable from CoreAPI
            nodes_list = self.core_api.list_node().items
            node_capacity = {}
            for node in nodes_list:
                allocatable = node.status.allocatable
                name = node.metadata.name
                node_capacity[name] = {
                    "cpu": _parse_quantity(allocatable.get("cpu", "0")),
                    "memory": _parse_quantity(allocatable.get("memory", "0"))
                }

            # 2. Fetch Node metrics
            node_metrics = custom_api.list_cluster_custom_object(
                group="metrics.k8s.io",
                version="v1beta1",
                plural="nodes"
            )
            
            # 3. Augment metrics with percentages
            nodes_result = []
            for item in node_metrics.get("items", []):
                name = item["metadata"]["name"]
                usage = item["usage"]
                
                cpu_usage = _parse_quantity(usage.get("cpu", "0"))
                mem_usage = _parse_quantity(usage.get("memory", "0"))
                
                capacity = node_capacity.get(name, {"cpu": 1.0, "memory": 1.0})
                
                cpu_percent = (cpu_usage / capacity["cpu"]) * 100 if capacity["cpu"] > 0 else 0
                mem_percent = (mem_usage / capacity["memory"]) * 100 if capacity["memory"] > 0 else 0
                
                item["usage"]["cpu_percent"] = f"{cpu_percent:.1f}%"
                item["usage"]["memory_percent"] = f"{mem_percent:.1f}%"
                item["capacity"] = capacity # Optional: include capacity for client side calc
                
                nodes_result.append(item)
                
            metrics["nodes"] = nodes_result
        except Exception as e:
            metrics["nodes_error"] = str(e)
            
        try:
            # Pod metrics (all namespaces)
            # Note: Calculating Pod % is harder because we need requests/limits for each container 
            # in each pod. For performance on large clusters, we skip per-pod % for now unless requested.
            pod_metrics = custom_api.list_cluster_custom_object(
                group="metrics.k8s.io",
                version="v1beta1",
                plural="pods"
            )
            metrics["pods"] = pod_metrics.get("items", [])
        except Exception as e:
            metrics["pods_error"] = str(e)
            
        return metrics


# --- Singleton ---

_k8s_client: K8sClient | None = None


def get_k8s_client() -> K8sClient:
    """Get or create the singleton K8sClient instance."""
    global _k8s_client
    if _k8s_client is None:
        _k8s_client = K8sClient()
    return _k8s_client
