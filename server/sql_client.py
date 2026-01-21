"""Google Cloud SQL client wrapper.

Provides operations for listing instances, databases, and users.
"""

from typing import Any
from googleapiclient.discovery import build
from config import config
from gcs_client import _get_project_from_credentials

class SQLClient:
    """Google Cloud SQL Client."""
    
    def __init__(self, project: str | None = None):
        self._project = project or config.gcs_project or _get_project_from_credentials()
        if not self._project:
            raise ValueError("GCP Project ID is required for Cloud SQL operations")
        
        # Initialize SQL Admin API service
        self._service = build('sqladmin', 'v1beta4')
    
    def list_instances(self) -> list[dict[str, Any]]:
        """List all Cloud SQL instances in the project."""
        instances = []
        request = self._service.instances().list(project=self._project)
        
        while request is not None:
            response = request.execute()
            if 'items' in response:
                instances.extend(response['items'])
            request = self._service.instances().list_next(previous_request=request, previous_response=response)
            
        return instances
    
    def list_databases(self, instance: str) -> list[dict[str, Any]]:
        """List databases for a specific instance."""
        databases = []
        try:
            request = self._service.databases().list(project=self._project, instance=instance)
            response = request.execute()
            if 'items' in response:
                databases.extend(response['items'])
        except Exception as e:
            print(f"Error listing databases: {e}")
            raise e
            
        return databases

    def list_users(self, instance: str) -> list[dict[str, Any]]:
        """List users for a specific instance."""
        users = []
        try:
            request = self._service.users().list(project=self._project, instance=instance)
            response = request.execute()
            if 'items' in response:
                users.extend(response['items'])
        except Exception as e:
            print(f"Error listing users: {e}")
            raise e
            
        return users

# Singleton
_sql_client: SQLClient | None = None

def get_sql_client() -> SQLClient:
    """Get or create the singleton SQLClient instance."""
    global _sql_client
    if _sql_client is None:
        _sql_client = SQLClient()
    return _sql_client
