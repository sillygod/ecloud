"""Google Artifact Registry client wrapper.

Provides operations for repositories, images, and tags.
Also wraps docker CLI for push/pull operations.
"""

import subprocess
import shutil
from dataclasses import dataclass
from typing import List, Optional
from google.cloud import artifactregistry_v1
from google.api_core import exceptions
from config import config
from gcs_client import _get_project_from_credentials
import os # Ensure os is imported if not already

# Fix for gRPC DNS issues on Mac/VPN
os.environ["GRPC_DNS_RESOLVER"] = "native"

@dataclass
class Parameters:
# ... (existing code)

    def list_locations(self) -> List[str]:
        """List available locations for the project."""
        project_path = f"projects/{self._project}"
        
        locations = []
        try:
            # Use request object to avoid keyword argument issues
            # Some versions of the library mix in LocationsClient, others don't expose 'name' kwarg directly on the client method if it's generated differently.
            # However, list_locations usually takes 'request'.
            from google.cloud.location import locations_pb2
            request = locations_pb2.ListLocationsRequest(name=project_path)
            
            # If the client has list_locations, it might be the mixin. 
            # If it fails with "unexpected keyword", maybe use request positional.
            for loc in self._client.list_locations(request=request):
                 locations.append(loc.location_id)
        except Exception as e:
            print(f"Error listing locations: {e}")
            # Fallback will handle it
            pass
            
        # Ensure common multi-regions are included
        # ...
    """Standardized parameters for GAR operations."""
    project: str
    location: str
    repository: str
    package: Optional[str] = None
    tag: Optional[str] = None

@dataclass
class RepositoryInfo:
    """Information about a GAR repository."""
    name: str
    format: str
    location: str
    description: str = ""
    
    def to_dict(self) -> dict:
        return {
            "name": self.name.split("/")[-1],  # Short name
            "full_name": self.name,
            "format": self.format.name if hasattr(self.format, "name") else str(self.format),
            "location": self.location,
            "description": self.description,
        }

@dataclass
class ImageInfo:
    """Information about a Docker image (Package)."""
    name: str
    uri: str
    
    def to_dict(self) -> dict:
        return {
            "name": self.name.split("/")[-1],
            "full_name": self.name,
            "uri": self.uri,
        }

@dataclass
class TagInfo:
    """Information about an image tag."""
    name: str
    version: str
    digest: str
    
    def to_dict(self) -> dict:
        return {
            "name": self.name.split("/")[-1],
            "full_name": self.name,
            "version": self.version,
            "digest": self.digest,
        }

class GARClient:
    """Google Artifact Registry Client."""
    
    def __init__(self, project: str | None = None):
        self._project = project or config.gcs_project or _get_project_from_credentials()
        if not self._project:
            raise ValueError("GCP Project ID is required for GAR operations")
        
        self._client = artifactregistry_v1.ArtifactRegistryClient()
    
    def _parse_parent(self, location: str) -> str:
        return f"projects/{self._project}/locations/{location}"

    def list_repositories(self, location: str) -> List[RepositoryInfo]:
        """List repositories in a location (or all locations using '-')."""
        
        target_locations = []
        if location == "all" or location == "-":
            # Wildcard failed, so we manually aggregate common locations
            # Start with multi-regions
            target_locations = ["asia", "us", "europe"]
            # Add discovered locations
            try:
                discovered = self.list_locations()
                for loc in discovered:
                    if loc not in target_locations:
                        target_locations.append(loc)
            except:
                pass # Ignore list_locations failure
        else:
            target_locations = [location]
            
        all_repos = []
        
        for target_loc in target_locations:
            try:
                parent = self._parse_parent(target_loc)
                request = artifactregistry_v1.ListRepositoriesRequest(parent=parent)
                
                for repo in self._client.list_repositories(request=request):
                    if repo.format_ == artifactregistry_v1.Repository.Format.DOCKER:
                        all_repos.append(RepositoryInfo(
                            name=repo.name,
                            format=repo.format_,
                            location=target_loc,
                            description=repo.description,
                        ))
            except Exception as e:
                # Ignore errors for specific locations (e.g. if not enabled)
                print(f"DEBUG: Failed to list repos in {target_loc}: {e}")
                continue
                
        return all_repos

    def list_packages(self, parent_repo: str) -> List[ImageInfo]:
        """List packages (images) in a repository.
        parent_repo: Full resource name of the repository.
        """
        print(f"DEBUG: list_packages parent_repo={parent_repo}")
        request = artifactregistry_v1.ListPackagesRequest(parent=parent_repo)
        
        images = []
        for pkg in self._client.list_packages(request=request):
            # Construct URI: location-docker.pkg.dev/project/repo/package
            # parent_repo format: projects/p/locations/l/repositories/r
            parts = parent_repo.split("/")
            location = parts[3]
            repo_name = parts[5]
            pkg_name = pkg.name.split("/")[-1] # This is URL encoded name
            
            # Decode for URI construction if needed? usually pkg.name is used as is in URI path but decoded?
            # Actually URI should use the name as it appears in docker pull
            # pkg.name (full) ends with encoded name.
            
            uri = f"{location}-docker.pkg.dev/{self._project}/{repo_name}/{pkg_name}"
            # Note: pkg_name might be URL encoded (e.g. foo%2Fbar). docker pull uses foo/bar.
            # We should probably unquote it for the URI display.
            from urllib.parse import unquote
            uri = f"{location}-docker.pkg.dev/{self._project}/{repo_name}/{unquote(pkg_name)}"
            
            print(f"DEBUG: Found package name={pkg.name} uri={uri}")
            images.append(ImageInfo(
                name=pkg.name, # Full resource name
                uri=uri
            ))
        return images

    def list_tags(self, parent_package: str) -> List[TagInfo]:
        """List tags for a package.
        parent_package: Full resource name of the package.
        """
        print(f"DEBUG: list_tags parent_package={parent_package}")
        # Validate parent_package format to prevent "project gorilla" error
        if not parent_package.startswith("projects/"):
             print(f"ERROR: Invalid parent_package format: {parent_package}")
             # Attempt to fix it? No, just raise or let it fail but log it.
        
        request = artifactregistry_v1.ListTagsRequest(parent=parent_package)
        
        tags = []
        for tag in self._client.list_tags(request=request):
            tags.append(TagInfo(
                name=tag.name,
                version=tag.version,
                digest=tag.version.split("/")[-1] # Version is usually full resource name of version
            ))
        return tags

    def list_locations(self) -> List[str]:
        """List available locations for the project."""
        # Note: The client's list_locations returns Location objects.
        # We need to construct the request manually or use the helper if available.
        # The method signature in newer libraries is list_locations(request=None, *, name=None, ...)
        project_path = f"projects/{self._project}"
        
        locations = []
        try:
            # Use request object to avoid keyword argument issues
            # Some versions of the library mix in LocationsClient, others don't expose 'name' kwarg directly on the client method if it's generated differently.
            # However, list_locations usually takes 'request'.
            from google.cloud.location import locations_pb2
            request = locations_pb2.ListLocationsRequest(name=project_path)
            
            # If the client has list_locations, it might be the mixin. 
            # If it fails with "unexpected keyword", maybe use request positional.
            for loc in self._client.list_locations(request=request):
                 locations.append(loc.location_id)
        except Exception as e:
            print(f"Error listing locations: {e}")
            # Fallback to a default list if API fails or not supported
            locations = ["asia-east1", "us-central1", "us-west1", "europe-west1"]
            
        # Ensure common multi-regions are included
        for mr in ["asia", "us", "europe"]:
            if mr not in locations:
                locations.append(mr)
            
        return sorted(locations)

    def delete_package(self, package_name: str) -> dict:
        """Delete a package (image) and all its tags."""
        request = artifactregistry_v1.DeletePackageRequest(name=package_name)
        operation = self._client.delete_package(request=request)
        operation.result() # Wait for completion
        return {"success": True, "package": package_name}

    def delete_tag(self, tag_name: str) -> dict:
        """Delete a specific tag."""
        request = artifactregistry_v1.DeleteTagRequest(name=tag_name)
        self._client.delete_tag(request=request)
        return {"success": True, "tag": tag_name}

    def create_tag(self, parent_package: str, tag_id: str, version: str) -> dict:
        """Create a new tag for an image version.
        parent_package: Full resource name of the package.
        tag_id: The new tag string (e.g. 'v1').
        version: Full resource name of the version (e.g. .../versions/sha256:...).
        """
        tag = artifactregistry_v1.Tag(
            name=f"{parent_package}/tags/{tag_id}", # Optional, but good practice
            version=version
        )
        request = artifactregistry_v1.CreateTagRequest(
            parent=parent_package,
            tag_id=tag_id,
            tag=tag
        )
        response = self._client.create_tag(request=request)
        return {
            "success": True, 
            "tag": tag_id, 
            "full_name": response.name
        }

    # Docker CLI wrappers
    
    def _check_docker(self):
        if not shutil.which("docker"):
            raise RuntimeError("Docker executable not found in PATH")

    def docker_pull(self, image_uri: str) -> dict:
        """Pull docker image."""
        self._check_docker()
        # image_uri example: us-west1-docker.pkg.dev/my-project/my-repo/my-image:tag
        cmd = ["docker", "pull", image_uri]
        
        try:
            result = subprocess.run(cmd, capture_output=True, text=True, check=True)
            return {"success": True, "output": result.stdout}
        except subprocess.CalledProcessError as e:
            raise RuntimeError(f"Docker pull failed: {e.stderr}")

    def docker_push(self, image_uri: str) -> dict:
        """Push docker image."""
        self._check_docker()
        cmd = ["docker", "push", image_uri]
        
        try:
            result = subprocess.run(cmd, capture_output=True, text=True, check=True)
            return {"success": True, "output": result.stdout}
        except subprocess.CalledProcessError as e:
            raise RuntimeError(f"Docker push failed: {e.stderr}")

    def docker_tag(self, source: str, target: str) -> dict:
        """Tag docker image."""
        self._check_docker()
        cmd = ["docker", "tag", source, target]
        
        try:
            subprocess.run(cmd, capture_output=True, text=True, check=True)
            return {"success": True, "source": source, "target": target}
        except subprocess.CalledProcessError as e:
            raise RuntimeError(f"Docker tag failed: {e.stderr}")


# Singleton
_gar_client: GARClient | None = None

def get_gar_client() -> GARClient:
    global _gar_client
    if _gar_client is None:
        _gar_client = GARClient()
    return _gar_client
