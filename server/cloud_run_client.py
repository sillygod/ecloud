"""Google Cloud Run client wrapper.

Provides operations for managing Cloud Run services, including deployment,
configuration, and log streaming.
"""

import os
from dataclasses import dataclass
from datetime import datetime
from typing import Any, List, Optional
from concurrent.futures import ThreadPoolExecutor, as_completed
import logging

from google.cloud import run_v2
from google.cloud import logging_v2
from google.cloud import asset_v1
from google.api_core.extended_operation import ExtendedOperation

from config import config
from gcs_client import _get_project_from_credentials

# Fix for gRPC DNS issues on Mac/VPN
os.environ["GRPC_DNS_RESOLVER"] = "native"

logger = logging.getLogger(__name__)


@dataclass
class ServiceInfo:
    """Cloud Run service information."""
    name: str
    region: str
    status: str
    url: str
    image: str
    created: str
    updated: str
    traffic: str
    min_instances: int
    max_instances: int
    cpu: str
    memory: str
    
    def to_dict(self) -> dict:
        return {
            "name": self.name,
            "region": self.region,
            "status": self.status,
            "url": self.url,
            "image": self.image,
            "created": self.created,
            "updated": self.updated,
            "traffic": self.traffic,
            "minInstances": self.min_instances,
            "maxInstances": self.max_instances,
            "cpu": self.cpu,
            "memory": self.memory,
        }


@dataclass
class RevisionInfo:
    """Cloud Run revision information."""
    name: str
    service: str
    created: str
    image: str
    traffic_percent: int
    status: str
    
    def to_dict(self) -> dict:
        return {
            "name": self.name,
            "service": self.service,
            "created": self.created,
            "image": self.image,
            "trafficPercent": self.traffic_percent,
            "status": self.status,
        }


class CloudRunClient:
    """Client for Google Cloud Run operations."""
    
    def __init__(self, project: str | None = None):
        """Initialize Cloud Run client.
        
        Args:
            project: GCP project ID. If None, uses default from credentials.
        """
        self._project = project or config.gcs_project or _get_project_from_credentials()
        self._services_client = run_v2.ServicesClient()
        self._revisions_client = run_v2.RevisionsClient()
        self._logging_client = None  # Lazy initialization
        self._asset_client = None  # Lazy initialization
    
    def _get_logging_client(self) -> logging_v2.Client:
        """Get or create logging client."""
        if self._logging_client is None:
            self._logging_client = logging_v2.Client(project=self._project)
        return self._logging_client
    
    def _get_asset_client(self) -> asset_v1.AssetServiceClient:
        """Get or create asset service client."""
        if self._asset_client is None:
            self._asset_client = asset_v1.AssetServiceClient()
        return self._asset_client
    
    def list_regions(self) -> List[str]:
        """List available Cloud Run regions.
        
        Returns:
            List of region names sorted alphabetically.
            
        Note:
            This returns a comprehensive list of known Cloud Run regions.
            The actual availability may vary by project and quotas.
        """
        # Comprehensive list of Cloud Run regions as of 2024
        # Based on: https://cloud.google.com/run/docs/locations
        regions = [
            # Americas
            "northamerica-northeast1",  # Montreal
            "northamerica-northeast2",  # Toronto
            "southamerica-east1",       # São Paulo
            "southamerica-west1",       # Santiago
            "us-central1",              # Iowa
            "us-east1",                 # South Carolina
            "us-east4",                 # Northern Virginia
            "us-east5",                 # Columbus
            "us-south1",                # Dallas
            "us-west1",                 # Oregon
            "us-west2",                 # Los Angeles
            "us-west3",                 # Salt Lake City
            "us-west4",                 # Las Vegas
            # Europe
            "europe-central2",          # Warsaw
            "europe-north1",            # Finland
            "europe-southwest1",        # Madrid
            "europe-west1",             # Belgium
            "europe-west2",             # London
            "europe-west3",             # Frankfurt
            "europe-west4",             # Netherlands
            "europe-west6",             # Zurich
            "europe-west8",             # Milan
            "europe-west9",             # Paris
            "europe-west10",            # Berlin
            "europe-west12",            # Turin
            # Asia Pacific
            "asia-east1",               # Taiwan
            "asia-east2",               # Hong Kong
            "asia-northeast1",          # Tokyo
            "asia-northeast2",          # Osaka
            "asia-northeast3",          # Seoul
            "asia-south1",              # Mumbai
            "asia-south2",              # Delhi
            "asia-southeast1",          # Singapore
            "asia-southeast2",          # Jakarta
            "australia-southeast1",     # Sydney
            "australia-southeast2",     # Melbourne
            # Middle East
            "me-central1",              # Doha
            "me-west1",                 # Tel Aviv
        ]
        
        return sorted(regions)
    
    def list_services_via_asset_api(self) -> List[ServiceInfo]:
        """List Cloud Run services from all regions using Asset API.
        
        This is much faster than querying each region individually as it uses
        the Cloud Asset Inventory API to search across all resources at once.
        
        Returns:
            List of ServiceInfo objects from all regions.
            
        Note:
            Requires Cloud Asset API to be enabled in the project.
            Falls back to parallel region queries if Asset API fails.
        """
        try:
            asset_client = self._get_asset_client()
            scope = f"projects/{self._project}"
            
            # Search for all Cloud Run services across all regions
            request = asset_v1.SearchAllResourcesRequest(
                scope=scope,
                asset_types=["run.googleapis.com/Service"],
            )
            
            response = asset_client.search_all_resources(request=request)
            
            # Parse the asset results and fetch detailed info
            services_by_region = {}
            for resource in response:
                # Extract region from location (e.g., "us-central1")
                location = resource.location
                # Extract service name from display_name
                service_name = resource.display_name
                
                if location not in services_by_region:
                    services_by_region[location] = []
                services_by_region[location].append(service_name)
            
            # Now fetch detailed info for each service
            all_services = []
            for region, service_names in services_by_region.items():
                for service_name in service_names:
                    try:
                        service_info = self.get_service(service_name, region)
                        all_services.append(service_info)
                    except Exception as e:
                        logger.debug(f"Failed to get details for {service_name} in {region}: {e}")
                        continue
            
            return all_services
            
        except Exception as e:
            logger.warning(f"Asset API failed, falling back to parallel region queries: {e}")
            # Fall back to the parallel approach
            return self._list_services_parallel()
    
    def _list_services_parallel(self) -> List[ServiceInfo]:
        """List services from all regions using parallel queries.
        
        This is a fallback method when Asset API is not available.
        
        Returns:
            List of ServiceInfo objects from all regions.
        """
        all_services = []
        regions = self.list_regions()
        
        def fetch_region_services(reg: str) -> List[ServiceInfo]:
            """Fetch services from a single region with timeout."""
            try:
                return self.list_services(region=reg, all_regions=False)
            except Exception as e:
                # Skip regions that fail (might not be enabled or have quota issues)
                logger.debug(f"Skipping region {reg}: {e}")
                return []
        
        # Use ThreadPoolExecutor for parallel requests
        # Limit to 10 concurrent requests to avoid overwhelming the API
        with ThreadPoolExecutor(max_workers=10) as executor:
            future_to_region = {
                executor.submit(fetch_region_services, reg): reg 
                for reg in regions
            }
            
            for future in as_completed(future_to_region, timeout=30):
                try:
                    services = future.result(timeout=5)
                    all_services.extend(services)
                except Exception as e:
                    region_name = future_to_region[future]
                    logger.debug(f"Failed to fetch services from {region_name}: {e}")
                    continue
        
        return all_services
    
    def list_services(self, region: str = "us-central1", all_regions: bool = False) -> List[ServiceInfo]:
        """List Cloud Run services in a region or all regions.
        
        Args:
            region: GCP region (e.g., 'us-central1'). Ignored if all_regions is True.
            all_regions: If True, list services from all regions using Asset API.
            
        Returns:
            List of ServiceInfo objects.
        """
        if all_regions:
            # Use Asset API for much faster all-regions query
            return self.list_services_via_asset_api()
        
        parent = f"projects/{self._project}/locations/{region}"
        
        try:
            services = self._services_client.list_services(parent=parent)
            result = []
            
            for service in services:
                # Extract service name (last part of full name)
                service_name = service.name.split("/")[-1]
                
                # Get status
                conditions = service.terminal_condition
                status = "Unknown"
                if conditions:
                    if conditions.state.name == "CONDITION_SUCCEEDED":
                        status = "Ready"
                    elif conditions.state.name == "CONDITION_FAILED":
                        status = "Failed"
                    else:
                        status = conditions.state.name
                
                # Get URL
                url = service.uri or ""
                
                # Get image from template
                image = ""
                if service.template and service.template.containers:
                    image = service.template.containers[0].image
                
                # Get timestamps
                created = service.create_time.isoformat() if service.create_time else ""
                updated = service.update_time.isoformat() if service.update_time else ""
                
                # Get traffic info
                traffic_info = "100%"
                if service.traffic:
                    traffic_parts = []
                    for traffic in service.traffic:
                        if traffic.percent:
                            revision_name = traffic.revision.split("/")[-1] if traffic.revision else "latest"
                            traffic_parts.append(f"{revision_name}:{traffic.percent}%")
                    if traffic_parts:
                        traffic_info = ", ".join(traffic_parts)
                
                # Get scaling info
                min_instances = 0
                max_instances = 100
                if service.template and service.template.scaling:
                    min_instances = service.template.scaling.min_instance_count
                    max_instances = service.template.scaling.max_instance_count
                
                # Get resource limits
                cpu = "1"
                memory = "512Mi"
                if service.template and service.template.containers:
                    container = service.template.containers[0]
                    if container.resources:
                        if container.resources.limits:
                            cpu = container.resources.limits.get("cpu", "1")
                            memory = container.resources.limits.get("memory", "512Mi")
                
                result.append(ServiceInfo(
                    name=service_name,
                    region=region,
                    status=status,
                    url=url,
                    image=image,
                    created=created,
                    updated=updated,
                    traffic=traffic_info,
                    min_instances=min_instances,
                    max_instances=max_instances,
                    cpu=cpu,
                    memory=memory,
                ))
            
            return result
        
        except Exception as e:
            raise RuntimeError(f"Failed to list Cloud Run services: {e}")
    
    def get_service(self, name: str, region: str = "us-central1") -> ServiceInfo:
        """Get details of a specific Cloud Run service.
        
        Args:
            name: Service name.
            region: GCP region.
            
        Returns:
            ServiceInfo object.
        """
        service_path = f"projects/{self._project}/locations/{region}/services/{name}"
        
        try:
            service = self._services_client.get_service(name=service_path)
            
            # Convert to ServiceInfo (similar to list_services)
            service_name = service.name.split("/")[-1]
            
            conditions = service.terminal_condition
            status = "Unknown"
            if conditions:
                if conditions.state.name == "CONDITION_SUCCEEDED":
                    status = "Ready"
                elif conditions.state.name == "CONDITION_FAILED":
                    status = "Failed"
                else:
                    status = conditions.state.name
            
            url = service.uri or ""
            image = ""
            if service.template and service.template.containers:
                image = service.template.containers[0].image
            
            created = service.create_time.isoformat() if service.create_time else ""
            updated = service.update_time.isoformat() if service.update_time else ""
            
            traffic_info = "100%"
            if service.traffic:
                traffic_parts = []
                for traffic in service.traffic:
                    if traffic.percent:
                        revision_name = traffic.revision.split("/")[-1] if traffic.revision else "latest"
                        traffic_parts.append(f"{revision_name}:{traffic.percent}%")
                if traffic_parts:
                    traffic_info = ", ".join(traffic_parts)
            
            min_instances = 0
            max_instances = 100
            if service.template and service.template.scaling:
                min_instances = service.template.scaling.min_instance_count
                max_instances = service.template.scaling.max_instance_count
            
            cpu = "1"
            memory = "512Mi"
            if service.template and service.template.containers:
                container = service.template.containers[0]
                if container.resources:
                    if container.resources.limits:
                        cpu = container.resources.limits.get("cpu", "1")
                        memory = container.resources.limits.get("memory", "512Mi")
            
            return ServiceInfo(
                name=service_name,
                region=region,
                status=status,
                url=url,
                image=image,
                created=created,
                updated=updated,
                traffic=traffic_info,
                min_instances=min_instances,
                max_instances=max_instances,
                cpu=cpu,
                memory=memory,
            )
        
        except Exception as e:
            raise RuntimeError(f"Failed to get Cloud Run service '{name}': {e}")
    
    def list_revisions(self, service_name: str, region: str = "us-central1") -> List[RevisionInfo]:
        """List revisions for a Cloud Run service.
        
        Args:
            service_name: Service name.
            region: GCP region.
            
        Returns:
            List of RevisionInfo objects.
        """
        parent = f"projects/{self._project}/locations/{region}/services/{service_name}"
        
        try:
            revisions = self._revisions_client.list_revisions(parent=parent)
            result = []
            
            for revision in revisions:
                revision_name = revision.name.split("/")[-1]
                
                # Get status
                conditions = revision.conditions
                status = "Unknown"
                if conditions:
                    for condition in conditions:
                        if condition.type == "Ready":
                            status = condition.state.name
                            break
                
                # Get image
                image = ""
                if revision.containers:
                    image = revision.containers[0].image
                
                created = revision.create_time.isoformat() if revision.create_time else ""
                
                # Traffic percent will be filled from service info
                traffic_percent = 0
                
                result.append(RevisionInfo(
                    name=revision_name,
                    service=service_name,
                    created=created,
                    image=image,
                    traffic_percent=traffic_percent,
                    status=status,
                ))
            
            return result
        
        except Exception as e:
            raise RuntimeError(f"Failed to list revisions for service '{service_name}': {e}")
    
    def delete_service(self, name: str, region: str = "us-central1") -> bool:
        """Delete a Cloud Run service.
        
        Args:
            name: Service name.
            region: GCP region.
            
        Returns:
            True if successful.
        """
        service_path = f"projects/{self._project}/locations/{region}/services/{name}"
        
        try:
            operation = self._services_client.delete_service(name=service_path)
            # Wait for operation to complete
            operation.result(timeout=300)
            return True
        
        except Exception as e:
            raise RuntimeError(f"Failed to delete Cloud Run service '{name}': {e}")
    
    def get_logs(
        self,
        service_name: str,
        region: str = "us-central1",
        limit: int = 100,
        severity: str | None = None,
    ) -> List[dict[str, Any]]:
        """Get logs for a Cloud Run service.
        
        Args:
            service_name: Service name.
            region: GCP region.
            limit: Maximum number of log entries to return.
            severity: Filter by severity (DEBUG, INFO, WARNING, ERROR, CRITICAL).
            
        Returns:
            List of log entry dictionaries.
        """
        logging_client = self._get_logging_client()
        
        # Build filter
        filter_parts = [
            f'resource.type="cloud_run_revision"',
            f'resource.labels.service_name="{service_name}"',
            f'resource.labels.location="{region}"',
        ]
        
        if severity:
            filter_parts.append(f'severity="{severity}"')
        
        filter_str = " AND ".join(filter_parts)
        
        try:
            entries = logging_client.list_entries(
                filter_=filter_str,
                order_by=logging_v2.DESCENDING,
                max_results=limit,
            )
            
            result = []
            for entry in entries:
                result.append({
                    "timestamp": entry.timestamp.isoformat() if entry.timestamp else "",
                    "severity": entry.severity,
                    "message": entry.payload,
                    "insertId": entry.insert_id,
                    "resource": entry.resource._properties if entry.resource else {},
                })
            
            return result
        
        except Exception as e:
            raise RuntimeError(f"Failed to get logs for service '{service_name}': {e}")
    
    def deploy_service(
        self,
        name: str,
        image: str,
        region: str = "us-central1",
        port: int = 8080,
        env_vars: dict[str, str] | None = None,
        cpu: str = "1",
        memory: str = "512Mi",
        min_instances: int = 0,
        max_instances: int = 100,
        allow_unauthenticated: bool = True,
    ) -> ServiceInfo:
        """Deploy a new Cloud Run service or update existing one.
        
        Args:
            name: Service name.
            image: Container image URL.
            region: GCP region.
            port: Container port.
            env_vars: Environment variables.
            cpu: CPU limit (e.g., "1", "2").
            memory: Memory limit (e.g., "512Mi", "1Gi").
            min_instances: Minimum number of instances.
            max_instances: Maximum number of instances.
            allow_unauthenticated: Allow unauthenticated access.
            
        Returns:
            ServiceInfo object for the deployed service.
        """
        parent = f"projects/{self._project}/locations/{region}"
        service_path = f"{parent}/services/{name}"
        
        # Build service configuration
        container = run_v2.Container(
            image=image,
            ports=[run_v2.ContainerPort(container_port=port)],
            resources=run_v2.ResourceRequirements(
                limits={"cpu": cpu, "memory": memory}
            ),
        )
        
        if env_vars:
            container.env = [
                run_v2.EnvVar(name=k, value=v) for k, v in env_vars.items()
            ]
        
        template = run_v2.RevisionTemplate(
            containers=[container],
            scaling=run_v2.RevisionScaling(
                min_instance_count=min_instances,
                max_instance_count=max_instances,
            ),
        )
        
        service = run_v2.Service(
            template=template,
        )
        
        try:
            # Check if service exists
            try:
                self._services_client.get_service(name=service_path)
                # Service exists, update it
                operation = self._services_client.update_service(
                    service=service,
                )
            except Exception:
                # Service doesn't exist, create it
                service.name = service_path
                operation = self._services_client.create_service(
                    parent=parent,
                    service=service,
                    service_id=name,
                )
            
            # Wait for operation to complete
            result = operation.result(timeout=600)
            
            # Set IAM policy for unauthenticated access if requested
            if allow_unauthenticated:
                try:
                    from google.iam.v1 import policy_pb2
                    from google.cloud.run_v2.services.services import ServicesClient
                    
                    policy = policy_pb2.Policy(
                        bindings=[
                            policy_pb2.Binding(
                                role="roles/run.invoker",
                                members=["allUsers"],
                            )
                        ]
                    )
                    
                    self._services_client.set_iam_policy(
                        resource=service_path,
                        policy=policy,
                    )
                except Exception as e:
                    print(f"Warning: Failed to set IAM policy: {e}")
            
            # Return service info
            return self.get_service(name, region)
        
        except Exception as e:
            raise RuntimeError(f"Failed to deploy Cloud Run service '{name}': {e}")


# Singleton instance
_client_instance: CloudRunClient | None = None


def get_cloud_run_client(project: str | None = None) -> CloudRunClient:
    """Get or create the Cloud Run client singleton.
    
    Args:
        project: GCP project ID. If None, uses default from credentials.
        
    Returns:
        CloudRunClient instance.
    """
    global _client_instance
    if _client_instance is None:
        _client_instance = CloudRunClient(project)
    return _client_instance


def reset_cloud_run_client():
    """Reset the Cloud Run client singleton.
    
    Useful for testing or when credentials change.
    """
    global _client_instance
    _client_instance = None
