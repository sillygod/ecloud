"""Helm 3 client wrapper for managing Kubernetes applications.

Provides operations for managing Helm releases, repositories, and charts
using Service Account authentication with pyhelm3 library.
"""

import asyncio
import json
import os
import subprocess
from dataclasses import dataclass
from datetime import datetime
from pathlib import Path
from typing import Any

from pyhelm3 import Client as Helm3Client

from config import config
from error_handler import (
    helm_not_initialized_error,
    helm_release_not_found_error,
    helm_release_already_exists_error,
    helm_chart_not_found_error,
    helm_namespace_not_found_error,
    helm_revision_not_found_error,
    helm_repository_already_exists_error,
    helm_repository_not_found_error,
    helm_invalid_url_error,
    helm_cli_not_found_error,
    auth_service_account_not_set_error,
    auth_service_account_file_not_found_error,
    create_helm_error_from_exception,
)


# --- Data Transfer Objects ---

@dataclass
class HelmRelease:
    """Helm release information."""
    name: str
    namespace: str
    chart: str
    version: str
    status: str
    revision: int
    updated: datetime
    app_version: str | None = None
    
    def to_dict(self) -> dict[str, Any]:
        return {
            "name": self.name,
            "namespace": self.namespace,
            "chart": self.chart,
            "version": self.version,
            "status": self.status,
            "revision": self.revision,
            "updated": self.updated.isoformat(),
            "appVersion": self.app_version,
        }


@dataclass
class HelmReleaseDetails(HelmRelease):
    """Detailed Helm release information including values and history."""
    values: dict[str, Any] | None = None
    revision_history: list[dict[str, Any]] | None = None
    notes: str | None = None
    
    def to_dict(self) -> dict[str, Any]:
        base = super().to_dict()
        base.update({
            "values": self.values or {},
            "revisionHistory": self.revision_history or [],
            "notes": self.notes,
        })
        return base


@dataclass
class ChartRepository:
    """Helm chart repository information."""
    name: str
    url: str
    
    def to_dict(self) -> dict[str, str]:
        return {
            "name": self.name,
            "url": self.url,
        }


@dataclass
class ChartSearchResult:
    """Helm chart search result."""
    name: str
    version: str
    app_version: str
    description: str
    repo: str
    
    def to_dict(self) -> dict[str, str]:
        return {
            "name": self.name,
            "version": self.version,
            "appVersion": self.app_version,
            "description": self.description,
            "repo": self.repo,
        }


# --- Helper Functions ---

def _get_sa_path() -> str:
    """Get Service Account JSON path from environment."""
    path = os.getenv("GOOGLE_APPLICATION_CREDENTIALS")
    if not path:
        raise auth_service_account_not_set_error().to_exception()
    if not os.path.exists(path):
        raise auth_service_account_file_not_found_error(path).to_exception()
    return path


# --- Main Client ---

class HelmClient:
    """Helm 3 client using pyhelm3 library with Service Account authentication."""
    
    def __init__(
        self, 
        cluster_endpoint: str | None = None,
        ca_cert_path: str | None = None,
        token: str | None = None
    ):
        """Initialize Helm client.
        
        Args:
            cluster_endpoint: Kubernetes API server endpoint (e.g., https://1.2.3.4)
            ca_cert_path: Path to CA certificate file
            token: Bearer token for authentication
        """
        self.cluster_endpoint = cluster_endpoint
        self.ca_cert_path = ca_cert_path
        self.token = token
        self._client: Helm3Client | None = None
        self._initialized = False
        self._temp_kubeconfig: str | None = None
    
    async def initialize(self) -> None:
        """Initialize pyhelm3 client with Service Account authentication.
        
        This method:
        1. Verifies GOOGLE_APPLICATION_CREDENTIALS is set
        2. Creates a temporary kubeconfig with cluster credentials
        3. Initializes the pyhelm3 client with the temporary kubeconfig
        
        Raises:
            ValueError: If GOOGLE_APPLICATION_CREDENTIALS is not set or cluster info is missing
            FileNotFoundError: If Service Account file doesn't exist
            Exception: If pyhelm3 initialization fails
        """
        # Ensure Service Account environment variable is set
        sa_path = _get_sa_path()
        
        # Verify we have cluster connection info
        if not self.cluster_endpoint or not self.ca_cert_path or not self.token:
            raise ValueError(
                "Cluster connection info not provided. "
                "Please connect to a Kubernetes cluster first."
            )
        
        # Log initialization for debugging
        print(f"Initializing Helm client with Service Account: {sa_path}")
        print(f"Cluster endpoint: {self.cluster_endpoint}")
        
        # Create temporary kubeconfig with cluster credentials
        # This avoids conflicts with local gcloud kubeconfig
        try:
            kubeconfig_content = {
                "apiVersion": "v1",
                "kind": "Config",
                "clusters": [{
                    "name": "ecloud-cluster",
                    "cluster": {
                        "server": self.cluster_endpoint,
                        "certificate-authority": self.ca_cert_path,
                    }
                }],
                "users": [{
                    "name": "ecloud-user",
                    "user": {
                        "token": self.token,
                    }
                }],
                "contexts": [{
                    "name": "ecloud-context",
                    "context": {
                        "cluster": "ecloud-cluster",
                        "user": "ecloud-user",
                    }
                }],
                "current-context": "ecloud-context",
            }
            
            # Write temporary kubeconfig
            import tempfile
            import yaml
            
            with tempfile.NamedTemporaryFile(
                mode='w',
                suffix='.yaml',
                delete=False,
                prefix='ecloud-kubeconfig-'
            ) as f:
                yaml.dump(kubeconfig_content, f)
                self._temp_kubeconfig = f.name
            
            print(f"Created temporary kubeconfig: {self._temp_kubeconfig}")
            
            # Initialize pyhelm3 client with temporary kubeconfig
            self._client = Helm3Client(kubeconfig=Path(self._temp_kubeconfig))
            
            self._initialized = True
            print("Helm client initialized successfully")
            
        except Exception as e:
            # Clean up temporary kubeconfig on failure
            if self._temp_kubeconfig and os.path.exists(self._temp_kubeconfig):
                os.unlink(self._temp_kubeconfig)
                self._temp_kubeconfig = None
            
            raise Exception(
                f"Failed to initialize Helm client: {str(e)}. "
                "Please ensure cluster connection is valid and accessible."
            ) from e
    
    def cleanup(self) -> None:
        """Clean up temporary resources."""
        if self._temp_kubeconfig and os.path.exists(self._temp_kubeconfig):
            try:
                os.unlink(self._temp_kubeconfig)
                print(f"Cleaned up temporary kubeconfig: {self._temp_kubeconfig}")
            except Exception as e:
                print(f"Warning: Failed to clean up temporary kubeconfig: {e}")
            finally:
                self._temp_kubeconfig = None
        
        self._initialized = False
        self._client = None
    
    def _ensure_initialized(self) -> None:
        """Ensure the client is initialized before operations."""
        if not self._initialized or self._client is None:
            raise helm_not_initialized_error().to_exception()
    
    async def list_releases(
        self,
        namespace: str | None = None,
        all_namespaces: bool = False
    ) -> list[dict[str, Any]]:
        """List Helm releases.
        
        Args:
            namespace: Target namespace (optional)
            all_namespaces: List releases from all namespaces
        
        Returns:
            List of release dicts with keys: name, namespace, chart, version, status
        """
        self._ensure_initialized()
        
        try:
            # List releases using pyhelm3
            # pyhelm3's list_releases returns a generator of Release objects (name + namespace only)
            releases_generator = await self._client.list_releases(
                namespace=namespace,
                all_namespaces=all_namespaces
            )
            
            # Convert generator to list
            releases = list(releases_generator)
            
            # For each release, get the current revision to get detailed info
            # Use asyncio.gather to fetch all revisions concurrently for better performance
            async def get_release_info(release):
                """Get detailed info for a single release."""
                try:
                    # Get current revision which has all the details
                    revision = await release.current_revision()
                    
                    # Extract chart metadata
                    chart_metadata = await revision.chart_metadata()
                    
                    return {
                        "name": release.name,
                        "namespace": release.namespace,
                        "chart": chart_metadata.name,
                        "version": chart_metadata.version,
                        "status": revision.status.value,  # ReleaseRevisionStatus enum
                        "revision": revision.revision,
                        "updated": revision.updated.isoformat(),
                        "appVersion": chart_metadata.app_version,
                    }
                    
                except Exception as e:
                    # Log the error but continue with other releases
                    print(f"Warning: Failed to get details for release {release.name}: {e}")
                    # Return minimal info
                    return {
                        "name": release.name,
                        "namespace": release.namespace,
                        "chart": "unknown",
                        "version": "unknown",
                        "status": "unknown",
                        "revision": 0,
                        "updated": datetime.now().isoformat(),
                        "appVersion": None,
                    }
            
            # Fetch all release info concurrently
            import asyncio
            result = await asyncio.gather(*[get_release_info(release) for release in releases])
            
            return list(result)
            
        except Exception as e:
            import traceback
            traceback.print_exc()
            raise Exception(
                f"Failed to list Helm releases: {str(e)}. "
                "Please ensure you have proper permissions and the cluster is accessible."
            ) from e
    
    async def get_release_details(
        self,
        name: str,
        namespace: str = "default"
    ) -> dict[str, Any]:
        """Get detailed information about a Helm release.
        
        Args:
            name: Release name
            namespace: Target namespace
        
        Returns:
            Dict with keys: name, namespace, chart, version, status, values, revisionHistory
        """
        self._ensure_initialized()
        
        try:
            # Get current revision using pyhelm3 Client method
            revision = await self._client.get_current_revision(
                name,
                namespace=namespace
            )
            
            # Extract chart metadata
            chart_metadata = await revision.chart_metadata()
            
            # Extract values
            values = await revision.values()
            
            # Get revision history using the Release object
            release = revision.release
            # IMPORTANT: history() returns a generator (not async), but the command it calls is async
            # So we need to await the command first, then iterate the generator
            history_generator = await release.history()
            history = list(history_generator)
            
            # For each revision in history, we need to await chart_metadata()
            revision_history = []
            for hist_revision in history:
                hist_chart_metadata = await hist_revision.chart_metadata()
                
                revision_entry = {
                    "revision": hist_revision.revision,
                    "updated": hist_revision.updated.isoformat(),
                    "status": hist_revision.status.value,
                    "chart": hist_chart_metadata.name,
                    "version": hist_chart_metadata.version,
                    "description": hist_revision.description or "",
                }
                revision_history.append(revision_entry)
            
            # Build detailed response
            details = {
                "name": release.name,
                "namespace": release.namespace,
                "chart": chart_metadata.name,
                "version": chart_metadata.version,
                "status": revision.status.value,
                "revision": revision.revision,
                "updated": revision.updated.isoformat(),
                "appVersion": chart_metadata.app_version,
                "values": values,
                "revisionHistory": revision_history,
                "notes": revision.notes,
            }
            
            return details
            
        except Exception as e:
            import traceback
            traceback.print_exc()
            raise Exception(
                f"Failed to get release details for '{name}': {str(e)}. "
                "Please ensure the release exists and you have proper permissions."
            ) from e
    
    async def install_chart(
        self,
        release_name: str,
        chart_ref: str,
        namespace: str = "default",
        values: dict[str, Any] | None = None,
        repo: str | None = None,
        version: str | None = None,
        create_namespace: bool = False,
        wait: bool = True,
        timeout: int = 300
    ) -> dict[str, Any]:
        """Install a Helm chart.
        
        Args:
            release_name: Name for the release
            chart_ref: Chart reference (repo/chart or local path)
            namespace: Target namespace
            values: Custom values (dict)
            repo: Chart repository URL (if chart_ref doesn't include repo)
            version: Chart version (optional)
            create_namespace: Whether to create namespace if it doesn't exist
            wait: Wait for all resources to be ready
            timeout: Timeout in seconds
        
        Returns:
            Dict with release information
        """
        self._ensure_initialized()
        
        try:
            # Prepare installation options
            install_options = {
                "wait": wait,
                "timeout": f"{timeout}s",
                "create_namespace": create_namespace,
            }
            
            if version:
                install_options["version"] = version
            
            # Install the chart using pyhelm3
            # pyhelm3's install_release method signature:
            # install_release(name, chart, namespace, values=None, **kwargs)
            release = self._client.install_release(
                name=release_name,
                chart=chart_ref,
                namespace=namespace,
                values=values or {},
                **install_options
            )
            
            # Convert release to dict format
            result = {
                "name": release.name,
                "namespace": release.namespace,
                "chart": release.chart.metadata.name if release.chart and release.chart.metadata else chart_ref,
                "version": release.chart.metadata.version if release.chart and release.chart.metadata else version or "unknown",
                "status": release.info.status if release.info else "unknown",
                "revision": release.version if hasattr(release, 'version') else 1,
                "updated": release.info.last_deployed.isoformat() if release.info and release.info.last_deployed else datetime.now().isoformat(),
                "appVersion": release.chart.metadata.app_version if release.chart and release.chart.metadata else None,
                "notes": release.info.notes if release.info and hasattr(release.info, 'notes') else None,
            }
            
            return result
            
        except Exception as e:
            error_msg = str(e)
            
            # Provide more specific error messages based on common failure scenarios
            if "already exists" in error_msg.lower():
                raise helm_release_already_exists_error(release_name, namespace).to_exception() from e
            elif "not found" in error_msg.lower() or "no chart" in error_msg.lower():
                raise helm_chart_not_found_error(chart_ref).to_exception() from e
            elif "namespace" in error_msg.lower() and not create_namespace:
                raise helm_namespace_not_found_error(namespace).to_exception() from e
            else:
                raise create_helm_error_from_exception("install", e).to_exception() from e
    
    async def upgrade_release(
        self,
        release_name: str,
        chart_ref: str | None = None,
        namespace: str = "default",
        values: dict[str, Any] | None = None,
        version: str | None = None,
        wait: bool = True,
        timeout: int = 300
    ) -> dict[str, Any]:
        """Upgrade a Helm release.
        
        Args:
            release_name: Name of the release to upgrade
            chart_ref: Chart reference (optional, uses current chart if not provided)
            namespace: Target namespace
            values: Updated values (dict)
            version: Chart version (optional)
            wait: Wait for all resources to be ready
            timeout: Timeout in seconds
        
        Returns:
            Dict with updated release information
        """
        self._ensure_initialized()
        
        try:
            # If chart_ref is not provided, get the current chart from the release
            if not chart_ref:
                current_release = self._client.get_release(name=release_name, namespace=namespace)
                if not current_release or not current_release.chart or not current_release.chart.metadata:
                    raise ValueError(
                        f"Cannot determine chart for release '{release_name}'. "
                        "Please provide chart_ref explicitly."
                    )
                chart_ref = current_release.chart.metadata.name
            
            # Prepare upgrade options
            upgrade_options = {
                "wait": wait,
                "timeout": f"{timeout}s",
            }
            
            if version:
                upgrade_options["version"] = version
            
            # Upgrade the release using pyhelm3
            # pyhelm3's upgrade_release method signature:
            # upgrade_release(name, chart, namespace, values=None, **kwargs)
            release = self._client.upgrade_release(
                name=release_name,
                chart=chart_ref,
                namespace=namespace,
                values=values or {},
                **upgrade_options
            )
            
            # Convert release to dict format
            result = {
                "name": release.name,
                "namespace": release.namespace,
                "chart": release.chart.metadata.name if release.chart and release.chart.metadata else chart_ref,
                "version": release.chart.metadata.version if release.chart and release.chart.metadata else version or "unknown",
                "status": release.info.status if release.info else "unknown",
                "revision": release.version if hasattr(release, 'version') else 1,
                "updated": release.info.last_deployed.isoformat() if release.info and release.info.last_deployed else datetime.now().isoformat(),
                "appVersion": release.chart.metadata.app_version if release.chart and release.chart.metadata else None,
                "notes": release.info.notes if release.info and hasattr(release.info, 'notes') else None,
            }
            
            return result
            
        except ValueError:
            # Re-raise ValueError as-is
            raise
        except Exception as e:
            error_msg = str(e)
            
            # Provide more specific error messages
            if "not found" in error_msg.lower() and "release" in error_msg.lower():
                raise helm_release_not_found_error(release_name, namespace).to_exception() from e
            elif "not found" in error_msg.lower() or "no chart" in error_msg.lower():
                raise helm_chart_not_found_error(chart_ref).to_exception() from e
            else:
                raise create_helm_error_from_exception("upgrade", e).to_exception() from e
    
    async def rollback_release(
        self,
        release_name: str,
        revision: int,
        namespace: str = "default",
        wait: bool = True
    ) -> dict[str, Any]:
        """Rollback a Helm release to a specific revision.
        
        Args:
            release_name: Name of the release to rollback
            revision: Target revision number
            namespace: Target namespace
            wait: Wait for rollback to complete
        
        Returns:
            Dict with release information after rollback
        """
        self._ensure_initialized()
        
        try:
            # Prepare rollback options
            rollback_options = {
                "wait": wait,
            }
            
            # Rollback the release using pyhelm3
            # pyhelm3's rollback_release method signature:
            # rollback_release(name, namespace, revision, **kwargs)
            release = self._client.rollback_release(
                name=release_name,
                namespace=namespace,
                revision=revision,
                **rollback_options
            )
            
            # Convert release to dict format
            result = {
                "name": release.name,
                "namespace": release.namespace,
                "chart": release.chart.metadata.name if release.chart and release.chart.metadata else "unknown",
                "version": release.chart.metadata.version if release.chart and release.chart.metadata else "unknown",
                "status": release.info.status if release.info else "unknown",
                "revision": release.version if hasattr(release, 'version') else revision,
                "updated": release.info.last_deployed.isoformat() if release.info and release.info.last_deployed else datetime.now().isoformat(),
                "appVersion": release.chart.metadata.app_version if release.chart and release.chart.metadata else None,
                "notes": release.info.notes if release.info and hasattr(release.info, 'notes') else None,
            }
            
            return result
            
        except Exception as e:
            error_msg = str(e)
            
            # Provide more specific error messages
            if "not found" in error_msg.lower() and "release" in error_msg.lower():
                raise helm_release_not_found_error(release_name, namespace).to_exception() from e
            elif "revision" in error_msg.lower() and "not found" in error_msg.lower():
                raise helm_revision_not_found_error(release_name, revision).to_exception() from e
            else:
                raise create_helm_error_from_exception("rollback", e).to_exception() from e
    
    async def uninstall_release(
        self,
        release_name: str,
        namespace: str = "default",
        wait: bool = True
    ) -> bool:
        """Uninstall a Helm release.
        
        Args:
            release_name: Name of the release to uninstall
            namespace: Target namespace
            wait: Wait for uninstall to complete
        
        Returns:
            True if successful
        """
        self._ensure_initialized()
        
        try:
            # Prepare uninstall options
            uninstall_options = {
                "wait": wait,
            }
            
            # Uninstall the release using pyhelm3
            # pyhelm3's uninstall_release method signature:
            # uninstall_release(name, namespace, **kwargs)
            self._client.uninstall_release(
                name=release_name,
                namespace=namespace,
                **uninstall_options
            )
            
            # If no exception was raised, the uninstall was successful
            return True
            
        except Exception as e:
            error_msg = str(e)
            
            # Provide more specific error messages
            if "not found" in error_msg.lower():
                raise helm_release_not_found_error(release_name, namespace).to_exception() from e
            else:
                raise create_helm_error_from_exception("uninstall", e).to_exception() from e
    
    async def list_repositories(self) -> list[dict[str, str]]:
        """List configured Helm chart repositories.
        
        Note: This method uses Helm CLI directly and doesn't require
        the pyhelm3 client to be initialized.
        
        Returns:
            List of dicts with keys: name, url
        """
        try:
            # Execute helm repo list command
            result = subprocess.run(
                ["helm", "repo", "list", "-o", "json"],
                capture_output=True,
                text=True,
                check=True
            )
            
            # Parse JSON output
            repos_data = json.loads(result.stdout)
            
            # Convert to ChartRepository format
            repositories = []
            for repo in repos_data:
                repositories.append({
                    "name": repo.get("name", ""),
                    "url": repo.get("url", "")
                })
            
            return repositories
            
        except subprocess.CalledProcessError as e:
            # If no repositories are configured, helm returns exit code 1
            if "no repositories to show" in e.stderr.lower() or not e.stderr:
                return []
            raise create_helm_error_from_exception("list_repositories", e).to_exception() from e
        except json.JSONDecodeError as e:
            raise create_helm_error_from_exception("list_repositories", e).to_exception() from e
        except FileNotFoundError:
            raise helm_cli_not_found_error().to_exception()
        except Exception as e:
            raise create_helm_error_from_exception("list_repositories", e).to_exception() from e
    
    async def add_repository(self, name: str, url: str) -> bool:
        """Add a Helm chart repository.
        
        Note: This method uses Helm CLI directly and doesn't require
        the pyhelm3 client to be initialized.
        
        Args:
            name: Repository name
            url: Repository URL
        
        Returns:
            True if successful
        """
        try:
            # Execute helm repo add command
            result = subprocess.run(
                ["helm", "repo", "add", name, url],
                capture_output=True,
                text=True,
                check=True
            )
            
            # Update repository index
            subprocess.run(
                ["helm", "repo", "update", name],
                capture_output=True,
                text=True,
                check=True
            )
            
            return True
            
        except subprocess.CalledProcessError as e:
            error_msg = e.stderr.strip() if e.stderr else str(e)
            
            # Provide more specific error messages
            if "already exists" in error_msg.lower():
                raise helm_repository_already_exists_error(name).to_exception() from e
            elif "invalid" in error_msg.lower() or "malformed" in error_msg.lower():
                raise helm_invalid_url_error(url).to_exception() from e
            else:
                raise create_helm_error_from_exception("add_repository", e).to_exception() from e
        except FileNotFoundError:
            raise helm_cli_not_found_error().to_exception()
        except Exception as e:
            raise create_helm_error_from_exception("add_repository", e).to_exception() from e
    
    async def remove_repository(self, name: str) -> bool:
        """Remove a Helm chart repository.
        
        Note: This method uses Helm CLI directly and doesn't require
        the pyhelm3 client to be initialized.
        
        Args:
            name: Repository name
        
        Returns:
            True if successful
        """
        try:
            # Execute helm repo remove command
            result = subprocess.run(
                ["helm", "repo", "remove", name],
                capture_output=True,
                text=True,
                check=True
            )
            
            return True
            
        except subprocess.CalledProcessError as e:
            error_msg = e.stderr.strip() if e.stderr else str(e)
            
            # Provide more specific error messages
            if "no repo named" in error_msg.lower() or "not found" in error_msg.lower():
                raise helm_repository_not_found_error(name).to_exception() from e
            else:
                raise create_helm_error_from_exception("remove_repository", e).to_exception() from e
        except FileNotFoundError:
            raise helm_cli_not_found_error().to_exception()
        except Exception as e:
            raise create_helm_error_from_exception("remove_repository", e).to_exception() from e
    
    async def search_charts(
        self,
        keyword: str,
        repo: str | None = None
    ) -> list[dict[str, Any]]:
        """Search for Helm charts.
        
        Note: This method uses Helm CLI directly and doesn't require
        the pyhelm3 client to be initialized.
        
        Args:
            keyword: Search keyword
            repo: Specific repository to search (optional, searches all if not provided)
        
        Returns:
            List of dicts with keys: name, version, appVersion, description, repo
        """
        try:
            # Build helm search repo command
            cmd = ["helm", "search", "repo", keyword, "-o", "json"]
            
            # If specific repo is provided, prepend it to the keyword
            if repo:
                cmd = ["helm", "search", "repo", f"{repo}/{keyword}", "-o", "json"]
            
            # Execute helm search repo command
            result = subprocess.run(
                cmd,
                capture_output=True,
                text=True,
                check=True
            )
            
            # Parse JSON output
            charts_data = json.loads(result.stdout)
            
            # Convert to ChartSearchResult format
            charts = []
            for chart in charts_data:
                # Extract repository name from chart name (format: repo/chart)
                chart_name = chart.get("name", "")
                repo_name = ""
                if "/" in chart_name:
                    repo_name = chart_name.split("/")[0]
                
                charts.append({
                    "name": chart_name,
                    "version": chart.get("version", ""),
                    "appVersion": chart.get("app_version", ""),
                    "description": chart.get("description", ""),
                    "repo": repo_name
                })
            
            return charts
            
        except subprocess.CalledProcessError as e:
            error_msg = e.stderr.strip() if e.stderr else str(e)
            
            # If no charts found, return empty list
            if "no results found" in error_msg.lower() or not result.stdout.strip():
                return []
            
            raise create_helm_error_from_exception("search_charts", e).to_exception() from e
        except json.JSONDecodeError as e:
            raise create_helm_error_from_exception("search_charts", e).to_exception() from e
        except FileNotFoundError:
            raise helm_cli_not_found_error().to_exception()
        except Exception as e:
            raise create_helm_error_from_exception("search_charts", e).to_exception() from e


# --- Singleton ---

_helm_client: HelmClient | None = None


def get_helm_client() -> HelmClient:
    """Get or create the singleton HelmClient instance."""
    global _helm_client
    if _helm_client is None:
        _helm_client = HelmClient()
    return _helm_client


async def initialize_helm_client(
    cluster_endpoint: str,
    ca_cert_path: str,
    token: str
) -> HelmClient:
    """Initialize and return the Helm client with cluster credentials.
    
    Args:
        cluster_endpoint: Kubernetes API server endpoint
        ca_cert_path: Path to CA certificate file
        token: Bearer token for authentication
    
    Returns:
        Initialized HelmClient instance
    """
    global _helm_client
    
    # Clean up existing client if any
    if _helm_client:
        _helm_client.cleanup()
    
    _helm_client = HelmClient(cluster_endpoint, ca_cert_path, token)
    await _helm_client.initialize()
    return _helm_client


def reset_helm_client() -> None:
    """Reset the singleton Helm client instance.
    
    This should be called when disconnecting from a cluster
    to ensure the client is re-initialized on next connection.
    """
    global _helm_client
    if _helm_client:
        _helm_client.cleanup()
    _helm_client = None
