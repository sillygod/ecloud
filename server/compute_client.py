"""Google Compute Engine client wrapper.

Provides operations for IP addresses (static and ephemeral from VMs).
"""

import os
from dataclasses import dataclass
from typing import List, Optional

from google.cloud import compute_v1
from google.api_core.extended_operation import ExtendedOperation

from config import config
from gcs_client import _get_project_from_credentials

# Fix for gRPC DNS issues on Mac/VPN
os.environ["GRPC_DNS_RESOLVER"] = "native"


@dataclass
class InstanceInfo:
    """Information about a VM instance."""
    name: str
    zone: str
    status: str
    internal_ip: str
    external_ip: str
    machine_type: str
    
    def to_dict(self) -> dict:
        return {
            "name": self.name,
            "zone": self.zone,
            "status": self.status,
            "internal_ip": self.internal_ip,
            "external_ip": self.external_ip,
            "machine_type": self.machine_type,
        }


@dataclass
class AddressInfo:
    """Information about an IP address."""
    name: str
    address: str
    region: str  # "global" for global addresses, zone for VM IPs
    status: str  # RESERVED, IN_USE, EPHEMERAL
    address_type: str  # EXTERNAL, INTERNAL
    users: List[str]  # Resources using this IP
    source: str = "static"  # "static" or "instance"
    
    def to_dict(self) -> dict:
        return {
            "name": self.name,
            "address": self.address,
            "region": self.region,
            "status": self.status,
            "address_type": self.address_type,
            "users": self.users,
            "users_short": self._format_users(),
            "source": self.source,
        }
    
    def _format_users(self) -> str:
        """Format users list for display."""
        if not self.users:
            return ""
        # Extract short names from full resource URLs
        short_names = []
        for user in self.users:
            # User format: https://www.googleapis.com/compute/v1/projects/.../zones/.../instances/name
            parts = user.split("/")
            if parts:
                short_names.append(parts[-1])
        return ", ".join(short_names)


def _wait_for_operation(operation: ExtendedOperation, timeout: int = 300) -> None:
    """Wait for a compute operation to complete."""
    operation.result(timeout=timeout)


class ComputeClient:
    """Google Compute Engine Client for IP address management."""
    
    def __init__(self, project: str | None = None):
        self._project = project or config.gcs_project or _get_project_from_credentials()
        if not self._project:
            raise ValueError("GCP Project ID is required for Compute operations")
        
        self._addresses_client = compute_v1.AddressesClient()
        self._global_addresses_client = compute_v1.GlobalAddressesClient()
        self._regions_client = compute_v1.RegionsClient()
        self._instances_client = compute_v1.InstancesClient()
    
    def list_regions(self) -> List[str]:
        """List all available regions in the project."""
        regions = []
        request = compute_v1.ListRegionsRequest(project=self._project)
        for region in self._regions_client.list(request=request):
            regions.append(region.name)
        return sorted(regions)
    
    def list_addresses(self) -> List[AddressInfo]:
        """List all IP addresses (static reserved + ephemeral from VMs)."""
        addresses = []
        
        # Track static IPs to avoid duplicates with instance IPs
        static_ips = set()
        
        # Get regional static addresses using aggregated list
        try:
            request = compute_v1.AggregatedListAddressesRequest(project=self._project)
            agg_list = self._addresses_client.aggregated_list(request=request)
            
            for region_key, response in agg_list:
                if response.addresses:
                    # region_key format: "regions/us-central1"
                    region_name = region_key.replace("regions/", "") if region_key.startswith("regions/") else region_key
                    
                    for addr in response.addresses:
                        static_ips.add(addr.address)
                        addresses.append(AddressInfo(
                            name=addr.name,
                            address=addr.address or "",
                            region=region_name,
                            status=addr.status or "UNKNOWN",
                            address_type=addr.address_type or "EXTERNAL",
                            users=list(addr.users) if addr.users else [],
                            source="static",
                        ))
        except Exception as e:
            print(f"Error listing regional addresses: {e}")
        
        # Get global static addresses
        try:
            request = compute_v1.ListGlobalAddressesRequest(project=self._project)
            for addr in self._global_addresses_client.list(request=request):
                static_ips.add(addr.address)
                addresses.append(AddressInfo(
                    name=addr.name,
                    address=addr.address or "",
                    region="global",
                    status=addr.status or "UNKNOWN",
                    address_type=addr.address_type or "EXTERNAL",
                    users=list(addr.users) if addr.users else [],
                    source="static",
                ))
        except Exception as e:
            print(f"Error listing global addresses: {e}")
        
        # Get ephemeral IPs from VM instances
        try:
            request = compute_v1.AggregatedListInstancesRequest(project=self._project)
            agg_list = self._instances_client.aggregated_list(request=request)
            
            for zone_key, response in agg_list:
                if response.instances:
                    # zone_key format: "zones/us-central1-a"
                    zone_name = zone_key.replace("zones/", "") if zone_key.startswith("zones/") else zone_key
                    # Extract region from zone (e.g., "us-central1-a" -> "us-central1")
                    region_name = "-".join(zone_name.split("-")[:-1]) if "-" in zone_name else zone_name
                    
                    for instance in response.instances:
                        instance_name = instance.name
                        
                        # Process each network interface
                        for nic in instance.network_interfaces or []:
                            # Internal IP
                            internal_ip = nic.network_i_p
                            if internal_ip and internal_ip not in static_ips:
                                addresses.append(AddressInfo(
                                    name=f"{instance_name}",
                                    address=internal_ip,
                                    region=zone_name,
                                    status="Ephemeral",
                                    address_type="INTERNAL",
                                    users=[instance_name],
                                    source="instance",
                                ))
                            
                            # External IPs from access configs
                            for access_config in nic.access_configs or []:
                                external_ip = access_config.nat_i_p
                                if external_ip and external_ip not in static_ips:
                                    addresses.append(AddressInfo(
                                        name=f"{instance_name}",
                                        address=external_ip,
                                        region=zone_name,
                                        status="Ephemeral",
                                        address_type="EXTERNAL",
                                        users=[instance_name],
                                        source="instance",
                                    ))
        except Exception as e:
            print(f"Error listing instance IPs: {e}")
        
        # Sort by address type (EXTERNAL first), then region, then name
        addresses.sort(key=lambda a: (0 if a.address_type == "EXTERNAL" else 1, a.region, a.name))
        return addresses
    
    def list_instances(self) -> List[InstanceInfo]:
        """List all VM instances across all zones."""
        instances = []
        
        try:
            request = compute_v1.AggregatedListInstancesRequest(project=self._project)
            agg_list = self._instances_client.aggregated_list(request=request)
            
            for zone_key, response in agg_list:
                if response.instances:
                    # zone_key format: "zones/us-central1-a"
                    zone_name = zone_key.replace("zones/", "") if zone_key.startswith("zones/") else zone_key
                    
                    for instance in response.instances:
                        internal_ip = ""
                        external_ip = ""
                        
                        # Get IPs from first network interface
                        if instance.network_interfaces:
                            nic = instance.network_interfaces[0]
                            internal_ip = nic.network_i_p or ""
                            if nic.access_configs:
                                external_ip = nic.access_configs[0].nat_i_p or ""
                        
                        machine_type = instance.machine_type.split("/")[-1] if instance.machine_type else ""
                        
                        instances.append(InstanceInfo(
                            name=instance.name,
                            zone=zone_name,
                            status=instance.status,
                            internal_ip=internal_ip,
                            external_ip=external_ip,
                            machine_type=machine_type,
                        ))
        except Exception as e:
            print(f"Error listing instances: {e}")
            
        # Sort by name
        instances.sort(key=lambda i: i.name)
        return instances

    def reserve_address(self, region: str, name: str) -> dict:
        """Reserve a new external static IP address.
        
        Args:
            region: The region for the address (e.g., "us-central1")
            name: The name for the new address
            
        Returns:
            Dict with success status and address details
        """
        address_resource = compute_v1.Address(
            name=name,
            address_type="EXTERNAL",
        )
        
        request = compute_v1.InsertAddressRequest(
            project=self._project,
            region=region,
            address_resource=address_resource,
        )
        
        operation = self._addresses_client.insert(request=request)
        _wait_for_operation(operation)
        
        # Fetch the created address to get the IP
        get_request = compute_v1.GetAddressRequest(
            project=self._project,
            region=region,
            address=name,
        )
        created_address = self._addresses_client.get(request=get_request)
        
        return {
            "success": True,
            "name": name,
            "address": created_address.address,
            "region": region,
        }

    def start_instance(self, zone: str, instance: str) -> dict:
        """Start a VM instance.
        
        Args:
            zone: Zone of the instance.
            instance: Name of the instance.
            
        Returns:
            Dict with operation status.
        """
        request = compute_v1.StartInstanceRequest(
            project=self._project,
            zone=zone,
            instance=instance,
        )
        operation = self._instances_client.start(request=request)
        _wait_for_operation(operation)
        return {
            "success": True,
            "operation": "start",
            "instance": instance,
            "zone": zone
        }

    def stop_instance(self, zone: str, instance: str) -> dict:
        """Stop a VM instance.
        
        Args:
            zone: Zone of the instance.
            instance: Name of the instance.
            
        Returns:
            Dict with operation status.
        """
        request = compute_v1.StopInstanceRequest(
            project=self._project,
            zone=zone,
            instance=instance,
        )
        operation = self._instances_client.stop(request=request)
        _wait_for_operation(operation)
        return {
            "success": True,
            "operation": "stop",
            "instance": instance,
            "zone": zone
        }

    def reset_instance(self, zone: str, instance: str) -> dict:
        """Reset (hard restart) a VM instance.
        
        Args:
            zone: Zone of the instance.
            instance: Name of the instance.
            
        Returns:
            Dict with operation status.
        """
        request = compute_v1.ResetInstanceRequest(
            project=self._project,
            zone=zone,
            instance=instance,
        )
        operation = self._instances_client.reset(request=request)
        _wait_for_operation(operation)
        return {
            "success": True,
            "operation": "reset",
            "instance": instance,
            "zone": zone
        }

    def delete_instance(self, zone: str, instance: str) -> dict:
        """Delete a VM instance.
        
        Args:
            zone: Zone of the instance.
            instance: Name of the instance.
            
        Returns:
            Dict with operation status.
        """
        request = compute_v1.DeleteInstanceRequest(
            project=self._project,
            zone=zone,
            instance=instance,
        )
        operation = self._instances_client.delete(request=request)
        _wait_for_operation(operation)
        return {
            "success": True,
            "operation": "delete",
            "instance": instance,
            "zone": zone
        }


# Singleton
_compute_client: ComputeClient | None = None


def get_compute_client() -> ComputeClient:
    """Get or create the singleton ComputeClient instance."""
    global _compute_client
    if _compute_client is None:
        _compute_client = ComputeClient()
    return _compute_client
