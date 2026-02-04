"""Google Cloud Service Usage client wrapper.

Provides operations for listing and enabling GCP services/APIs.
"""

import os
from dataclasses import dataclass
from typing import List

from google.cloud import service_usage_v1
from google.api_core.extended_operation import ExtendedOperation

from config import config
from gcs_client import _get_project_from_credentials

# Fix for gRPC DNS issues on Mac/VPN
os.environ["GRPC_DNS_RESOLVER"] = "native"


@dataclass
class ServiceInfo:
    """Information about a GCP service/API."""
    name: str
    title: str
    state: str  # ENABLED, DISABLED
    
    def to_dict(self) -> dict:
        return {
            "name": self.name,
            "title": self.title,
            "state": self.state,
        }


class ServiceUsageClient:
    """Client for Google Cloud Service Usage operations."""
    
    def __init__(self, project: str | None = None):
        """Initialize Service Usage client.
        
        Args:
            project: GCP project ID. If None, uses default from credentials.
        """
        self._project = project or config.gcs_project or _get_project_from_credentials()
        if not self._project:
            raise ValueError("GCP Project ID is required for Service Usage operations")
        
        self._client = service_usage_v1.ServiceUsageClient()
    
    def list_services(self, filter_state: str = "ENABLED") -> List[ServiceInfo]:
        """List services in the project.
        
        Args:
            filter_state: Filter by state - "ENABLED", "DISABLED", or "" for all.
            
        Returns:
            List of ServiceInfo objects.
            
        Note:
            - Listing all services ("") can be slow (10000+ services)
            - Listing only ENABLED services is much faster (typically 10-50 services)
            - Recommended to use filter_state="ENABLED" for better performance
            - Maximum page_size is 200 (API limitation)
            - For streaming results, use list_services_streaming instead
        """
        parent = f"projects/{self._project}"
        
        # Build filter
        filter_str = ""
        if filter_state:
            filter_str = f"state:{filter_state}"
        
        try:
            request = service_usage_v1.ListServicesRequest(
                parent=parent,
                filter=filter_str,
                page_size=200,  # Maximum allowed by API (SU_INVALID_PAGE_SIZE if higher)
            )
            
            services = self._client.list_services(request=request)
            result = []
            
            for service in services:
                # Extract service name (e.g., "compute.googleapis.com")
                service_name = service.name.split("/")[-1]
                
                # Get display title
                title = service.config.title if service.config else service_name
                
                # Get state
                state = service.state.name if service.state else "UNKNOWN"
                
                result.append(ServiceInfo(
                    name=service_name,
                    title=title,
                    state=state,
                ))
            
            return result
        
        except Exception as e:
            error_msg = str(e)
            # Provide more helpful error messages
            if "PERMISSION_DENIED" in error_msg or "403" in error_msg:
                raise RuntimeError(
                    f"Permission denied. Ensure your service account has "
                    f"'roles/serviceusage.serviceUsageViewer' or "
                    f"'roles/serviceusage.serviceUsageAdmin' role. "
                    f"Original error: {error_msg}"
                )
            elif "serviceusage.googleapis.com" in error_msg.lower():
                raise RuntimeError(
                    f"Service Usage API is not enabled. Enable it first with: "
                    f"gcloud services enable serviceusage.googleapis.com "
                    f"Original error: {error_msg}"
                )
            else:
                raise RuntimeError(f"Failed to list services: {error_msg}")
    
    def list_services_streaming(self, filter_state: str = "ENABLED", callback=None):
        """List services with streaming/progressive results.
        
        This method yields results as they arrive from the API, allowing for
        progressive display similar to `gcloud services list --available`.
        
        Args:
            filter_state: Filter by state - "ENABLED", "DISABLED", or "" for all.
            callback: Optional callback function called for each batch of services.
                     Signature: callback(services: List[ServiceInfo], is_final: bool)
            
        Yields:
            List[ServiceInfo]: Batches of services as they arrive from API.
            
        Example:
            def on_batch(services, is_final):
                print(f"Got {len(services)} services, final={is_final}")
            
            for batch in client.list_services_streaming("DISABLED", on_batch):
                # Process batch immediately
                display_services(batch)
        """
        parent = f"projects/{self._project}"
        
        # Build filter
        filter_str = ""
        if filter_state:
            filter_str = f"state:{filter_state}"
        
        try:
            request = service_usage_v1.ListServicesRequest(
                parent=parent,
                filter=filter_str,
                page_size=200,  # Maximum allowed by API
            )
            
            # Get paginated iterator
            pages = self._client.list_services(request=request)
            
            batch = []
            batch_size = 200  # Yield every 200 services for responsive UI
            
            for service in pages:
                # Extract service name
                service_name = service.name.split("/")[-1]
                
                # Get display title
                title = service.config.title if service.config else service_name
                
                # Get state
                state = service.state.name if service.state else "UNKNOWN"
                
                batch.append(ServiceInfo(
                    name=service_name,
                    title=title,
                    state=state,
                ))
                
                # Yield batch when it reaches batch_size
                if len(batch) >= batch_size:
                    if callback:
                        callback(batch, False)
                    yield batch
                    batch = []
            
            # Yield remaining services
            if batch:
                if callback:
                    callback(batch, True)
                yield batch
        
        except Exception as e:
            error_msg = str(e)
            if "PERMISSION_DENIED" in error_msg or "403" in error_msg:
                raise RuntimeError(
                    f"Permission denied. Ensure your service account has "
                    f"'roles/serviceusage.serviceUsageViewer' or "
                    f"'roles/serviceusage.serviceUsageAdmin' role. "
                    f"Original error: {error_msg}"
                )
            elif "serviceusage.googleapis.com" in error_msg.lower():
                raise RuntimeError(
                    f"Service Usage API is not enabled. Enable it first with: "
                    f"gcloud services enable serviceusage.googleapis.com "
                    f"Original error: {error_msg}"
                )
            else:
                raise RuntimeError(f"Failed to list services: {error_msg}")
    
    def enable_service(self, service_name: str) -> dict:
        """Enable a service in the project.
        
        Args:
            service_name: Service name (e.g., "compute.googleapis.com").
            
        Returns:
            Dict with operation status.
        """
        name = f"projects/{self._project}/services/{service_name}"
        
        try:
            request = service_usage_v1.EnableServiceRequest(name=name)
            operation = self._client.enable_service(request=request)
            
            # Wait for operation to complete
            operation.result(timeout=300)
            
            return {
                "success": True,
                "service": service_name,
                "project": self._project,
            }
        
        except Exception as e:
            raise RuntimeError(f"Failed to enable service '{service_name}': {e}")
    
    def disable_service(self, service_name: str) -> dict:
        """Disable a service in the project.
        
        Args:
            service_name: Service name (e.g., "compute.googleapis.com").
            
        Returns:
            Dict with operation status.
        """
        name = f"projects/{self._project}/services/{service_name}"
        
        try:
            request = service_usage_v1.DisableServiceRequest(name=name)
            operation = self._client.disable_service(request=request)
            
            # Wait for operation to complete
            operation.result(timeout=300)
            
            return {
                "success": True,
                "service": service_name,
                "project": self._project,
            }
        
        except Exception as e:
            raise RuntimeError(f"Failed to disable service '{service_name}': {e}")
    
    def get_service(self, service_name: str) -> ServiceInfo:
        """Get details of a specific service.
        
        Args:
            service_name: Service name (e.g., "compute.googleapis.com").
            
        Returns:
            ServiceInfo object.
        """
        name = f"projects/{self._project}/services/{service_name}"
        
        try:
            request = service_usage_v1.GetServiceRequest(name=name)
            service = self._client.get_service(request=request)
            
            service_name = service.name.split("/")[-1]
            title = service.config.title if service.config else service_name
            state = service.state.name if service.state else "UNKNOWN"
            
            return ServiceInfo(
                name=service_name,
                title=title,
                state=state,
            )
        
        except Exception as e:
            raise RuntimeError(f"Failed to get service '{service_name}': {e}")


# Singleton instance
_client_instance: ServiceUsageClient | None = None


def get_service_usage_client(project: str | None = None) -> ServiceUsageClient:
    """Get or create the Service Usage client singleton.
    
    Args:
        project: GCP project ID. If None, uses default from credentials.
        
    Returns:
        ServiceUsageClient instance.
    """
    global _client_instance
    if _client_instance is None:
        _client_instance = ServiceUsageClient(project)
    return _client_instance


def reset_service_usage_client():
    """Reset the Service Usage client singleton.
    
    Useful for testing or when credentials change.
    """
    global _client_instance
    _client_instance = None
