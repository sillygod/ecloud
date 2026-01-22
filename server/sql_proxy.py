"""Cloud SQL Proxy manager.

Manages local TCP proxies for Cloud SQL instances using the cloud-sql-proxy binary.
"""

import asyncio
import os
import shutil
from typing import Dict, Optional, Tuple


class SQLProxyManager:
    """Manages local TCP proxies for Cloud SQL instances using cloud-sql-proxy binary."""

    def __init__(self):
        self._proxies: Dict[str, Tuple[asyncio.subprocess.Process, int]] = {}  # instance -> (process, port)
        self._proxy_binary: Optional[str] = None

    def _find_proxy_binary(self) -> str:
        """Find the cloud-sql-proxy binary."""
        if self._proxy_binary:
            return self._proxy_binary

        # Try common names
        for name in ["cloud-sql-proxy", "cloud_sql_proxy"]:
            path = shutil.which(name)
            if path:
                self._proxy_binary = path
                return path

        raise RuntimeError(
            "cloud-sql-proxy binary not found. Please install it: "
            "https://cloud.google.com/sql/docs/mysql/sql-proxy#install"
        )

    async def _find_free_port(self) -> int:
        """Find an available port."""
        import socket
        with socket.socket(socket.AF_INET, socket.SOCK_STREAM) as s:
            s.bind(('127.0.0.1', 0))
            return s.getsockname()[1]

    async def start_proxy(self, instance_connection_name: str, port: int = 0, db_type: str = "POSTGRES") -> int:
        """Start a local proxy for the given instance.

        Args:
            instance_connection_name: The instance connection string (project:region:instance).
            port: Local port to bind to. If 0, finds an available port.
            db_type: Database type (not used with binary proxy, kept for API compatibility).

        Returns:
            The bound local port.
        """
        if instance_connection_name in self._proxies:
            return self._proxies[instance_connection_name][1]

        binary = self._find_proxy_binary()

        # Find a free port if not specified
        if port == 0:
            port = await self._find_free_port()

        # Start cloud-sql-proxy process
        # Format: cloud-sql-proxy --port PORT INSTANCE_CONNECTION_NAME
        cmd = [
            binary,
            f"--port={port}",
        ]

        # Use credentials file if GOOGLE_APPLICATION_CREDENTIALS is set
        creds_file = os.getenv("GOOGLE_APPLICATION_CREDENTIALS")
        if creds_file:
            cmd.append(f"--credentials-file={creds_file}")

        cmd.append(instance_connection_name)

        process = await asyncio.create_subprocess_exec(
            *cmd,
            stdout=asyncio.subprocess.PIPE,
            stderr=asyncio.subprocess.PIPE,
        )

        # Wait a moment for the proxy to start and check if it's still running
        await asyncio.sleep(1)

        if process.returncode is not None:
            # Process exited, read error
            _, stderr = await process.communicate()
            raise RuntimeError(f"cloud-sql-proxy failed to start: {stderr.decode()}")

        self._proxies[instance_connection_name] = (process, port)

        # Start background task to log any errors
        asyncio.create_task(self._monitor_proxy(instance_connection_name, process))

        return port

    async def _monitor_proxy(self, instance_name: str, process: asyncio.subprocess.Process):
        """Monitor proxy process and log errors."""
        try:
            _, stderr = await process.communicate()
            if stderr:
                print(f"Proxy {instance_name} stderr: {stderr.decode()}")
            if process.returncode and process.returncode != 0:
                print(f"Proxy {instance_name} exited with code {process.returncode}")
        except asyncio.CancelledError:
            pass
        finally:
            # Clean up from proxies dict if still there
            if instance_name in self._proxies:
                del self._proxies[instance_name]

    async def stop_proxy(self, instance_connection_name: str) -> bool:
        """Stop the proxy for the given instance."""
        if instance_connection_name in self._proxies:
            process, _ = self._proxies[instance_connection_name]
            try:
                process.terminate()
                try:
                    await asyncio.wait_for(process.wait(), timeout=5.0)
                except asyncio.TimeoutError:
                    process.kill()
                    await process.wait()
            except ProcessLookupError:
                pass  # Already dead
            del self._proxies[instance_connection_name]
            return True
        return False

    def list_proxies(self) -> Dict[str, int]:
        """List active proxies."""
        return {name: port for name, (_, port) in self._proxies.items()}

    async def shutdown(self):
        """Cleanup all proxies."""
        for name in list(self._proxies.keys()):
            await self.stop_proxy(name)


# Singleton
_proxy_manager: SQLProxyManager | None = None


def get_proxy_manager() -> SQLProxyManager:
    """Get the proxy manager singleton."""
    global _proxy_manager
    if _proxy_manager is None:
        _proxy_manager = SQLProxyManager()
    return _proxy_manager
