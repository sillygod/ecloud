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

    def create_database(self, instance: str, name: str, charset: str, collation: str) -> dict[str, Any]:
        """Create a new database."""
        body = {
            "name": name,
            "charset": charset,
            "collation": collation
        }
        try:
            request = self._service.databases().insert(project=self._project, instance=instance, body=body)
            return request.execute()
        except Exception as e:
            print(f"Error creating database: {e}")
            raise e

    def delete_database(self, instance: str, name: str) -> dict[str, Any]:
        """Delete a database."""
        try:
            request = self._service.databases().delete(project=self._project, instance=instance, database=name)
            return request.execute()
        except Exception as e:
            print(f"Error deleting database: {e}")
            raise e

    def create_user(self, instance: str, name: str, password: str, host: str = "%") -> dict[str, Any]:
        """Create a new user."""
        body = {
            "name": name,
            "password": password,
            "host": host
        }
        try:
            request = self._service.users().insert(project=self._project, instance=instance, body=body)
            return request.execute()
        except Exception as e:
            print(f"Error creating user: {e}")
            raise e

    def delete_user(self, instance: str, name: str, host: str = "%") -> dict[str, Any]:
        """Delete a user."""
        try:
            request = self._service.users().delete(project=self._project, instance=instance, name=name, host=host)
            return request.execute()
        except Exception as e:
            print(f"Error deleting user: {e}")
            raise e

# Singleton
_sql_client: SQLClient | None = None

def get_sql_client() -> SQLClient:
    """Get or create the singleton SQLClient instance."""
    global _sql_client
    if _sql_client is None:
        _sql_client = SQLClient()
    return _sql_client
