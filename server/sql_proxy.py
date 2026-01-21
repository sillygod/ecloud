"""Cloud SQL Proxy manager.

Manages local TCP proxies for Cloud SQL instances using the Python Connector.
"""

import asyncio
import socket
from typing import Dict, Optional, Tuple
from google.cloud.sql.connector import Connector

class SQLProxyManager:
    """Manages local TCP proxies for Cloud SQL instances."""

    def __init__(self):
        self._proxies: Dict[str, Tuple[asyncio.Server, int]] = {}  # instance -> (server, port)
        self._connector: Optional[Connector] = None
        
    async def _get_connector(self) -> Connector:
        """Lazy initialization of connector."""
        if self._connector is None:
            self._connector = Connector()
        return self._connector

    async def start_proxy(self, instance_connection_name: str, port: int = 0, db_type: str = "POSTGRES") -> int:
        """Start a local proxy for the given instance.

        Args:
            instance_connection_name: The instance connection string (project:region:instance).
            port: Local port to bind to. If 0, finds an available port.
            db_type: Database type (POSTGRES, MYSQL, SQLSERVER).

        Returns:
            The bound local port.
        """
        if instance_connection_name in self._proxies:
            return self._proxies[instance_connection_name][1]

        # Determine driver hint based on DB type
        driver_hint = "pg8000"  # Default to Postgres
        if "MYSQL" in db_type.upper():
            driver_hint = "pymysql"
        elif "SQLSERVER" in db_type.upper():
            driver_hint = "pytds"

        connector = await self._get_connector()

        async def handle_client(reader: asyncio.StreamReader, writer: asyncio.StreamWriter):
            remote_sock = None
            try:
                # Connect to Cloud SQL
                # Note: connect_async returns a socket object
                remote_sock = await connector.connect_async(instance_connection_name, driver_hint)
                
                # Wrap socket in asyncio streams
                loop = asyncio.get_running_loop()
                remote_reader, remote_writer = await asyncio.open_connection(sock=remote_sock)
                
                # Pipe data
                await asyncio.gather(
                    self._pipe(reader, remote_writer),
                    self._pipe(remote_reader, writer)
                )

            except Exception as e:
                print(f"Proxy connection error for {instance_connection_name}: {e}")
            finally:
                writer.close()
                if remote_sock:
                    try:
                        remote_sock.close()
                    except:
                        pass

        # Start server
        server = await asyncio.start_server(handle_client, '127.0.0.1', port)
        
        # Get allocated port
        sockets = server.sockets
        if sockets:
            port = sockets[0].getsockname()[1]

        self._proxies[instance_connection_name] = (server, port)
        
        # Start serving in background
        asyncio.create_task(server.serve_forever())
        
        return port

    async def stop_proxy(self, instance_connection_name: str) -> bool:
        """Stop the proxy for the given instance."""
        if instance_connection_name in self._proxies:
            server, _ = self._proxies[instance_connection_name]
            server.close()
            await server.wait_closed()
            del self._proxies[instance_connection_name]
            return True
        return False

    def list_proxies(self) -> Dict[str, int]:
        """List active proxies."""
        return {name: port for name, (_, port) in self._proxies.items()}
    
    async def shutdown(self):
        """Cleanup all proxies and connector."""
        for name in list(self._proxies.keys()):
            await self.stop_proxy(name)
        
        if self._connector:
            await self._connector.close_async()
            self._connector = None

    async def _pipe(self, reader: asyncio.StreamReader, writer: asyncio.StreamWriter):
        """Pipe data from reader to writer."""
        try:
            while not reader.at_eof():
                data = await reader.read(4096)
                if not data:
                    break
                writer.write(data)
                await writer.drain()
        except OSError:
            pass
        except Exception as e:
            print(f"Pipe error: {e}")
        finally:
            try:
                if not writer.is_closing():
                    writer.close()
            except:
                pass


# Singleton
_proxy_manager: SQLProxyManager | None = None

def get_proxy_manager() -> SQLProxyManager:
    """Get request logic proxy manager."""
    global _proxy_manager
    if _proxy_manager is None:
        _proxy_manager = SQLProxyManager()
    return _proxy_manager
