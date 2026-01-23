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

    def list_backups(self, instance: str) -> list[dict[str, Any]]:
        """List backup runs for an instance."""
        backups = []
        try:
            request = self._service.backupRuns().list(project=self._project, instance=instance)
            while request is not None:
                response = request.execute()
                if 'items' in response:
                    backups.extend(response['items'])
                request = self._service.backupRuns().list_next(previous_request=request, previous_response=response)
        except Exception as e:
            print(f"Error listing backups: {e}")
            raise e
        return backups

    def create_backup(self, instance: str, description: str = "") -> dict[str, Any]:
        """Create a new on-demand backup run."""
        body = {
            "description": description
        }
        try:
            request = self._service.backupRuns().insert(project=self._project, instance=instance, body=body)
            return request.execute()
        except Exception as e:
            print(f"Error creating backup: {e}")
            raise e

    def delete_backup(self, instance: str, backup_id: int) -> dict[str, Any]:
        """Delete a backup run."""
        try:
            request = self._service.backupRuns().delete(project=self._project, instance=instance, id=backup_id)
            return request.execute()
        except Exception as e:
            print(f"Error deleting backup: {e}")
            raise e

    def restore_backup(self, instance: str, backup_id: int) -> dict[str, Any]:
        """Restore an instance from a backup run."""
        body = {
            "restoreBackupContext": {
                "backupRunId": backup_id
            }
        }
        try:
            request = self._service.instances().restore(project=self._project, instance=instance, body=body)
            return request.execute()
        except Exception as e:
            print(f"Error restoring backup: {e}")
            raise e

    def get_connection_info(self, instance: str) -> dict[str, Any]:
        """Get connection information and strings."""
        try:
            request = self._service.instances().get(project=self._project, instance=instance)
            info = request.execute()
            
            connection_name = info.get("connectionName", "")
            ip_addresses = info.get("ipAddresses", [])
            public_ip = next((ip["ipAddress"] for ip in ip_addresses if ip.get("type") == "PRIMARY"), "")
            private_ip = next((ip["ipAddress"] for ip in ip_addresses if ip.get("type") == "PRIVATE"), "")
            
            # Construct standard connection strings
            # Note: actual connection strings depend on DB type (Postgres, MySQL, SQL Server) and user/pass
            # We provide templates/URIs
            
            db_type = info.get("databaseVersion", "")
            
            uris = {}
            if "POSTGRES" in db_type:
                uris["jdbc"] = f"jdbc:postgresql:///{instance}?cloudSqlInstance={connection_name}&socketFactory=com.google.cloud.sql.postgres.SocketFactory"
                uris["psql_proxy"] = f"psql \"host=127.0.0.1 port=5432 sslmode=disable user=postgres dbname=postgres\""
                uris["auth_proxy"] = f"./cloud_sql_proxy -instances={connection_name}=tcp:5432"
            elif "MYSQL" in db_type:
                uris["jdbc"] = f"jdbc:mysql:///{instance}?cloudSqlInstance={connection_name}&socketFactory=com.google.cloud.sql.mysql.SocketFactory"
                uris["mysql_proxy"] = f"mysql -u root -p --host 127.0.0.1 --port 3306"
                uris["auth_proxy"] = f"./cloud_sql_proxy -instances={connection_name}=tcp:3306"
                
            return {
                "connectionName": connection_name,
                "publicIp": public_ip,
                "privateIp": private_ip,
                "databaseVersion": db_type,
                "connectionUris": uris
            }
            
        except Exception as e:
            print(f"Error getting connection info: {e}")
            raise e

# Singleton
_sql_client: SQLClient | None = None

def get_sql_client() -> SQLClient:
    """Get or create the singleton SQLClient instance."""
    global _sql_client
    if _sql_client is None:
        _sql_client = SQLClient()
    return _sql_client
