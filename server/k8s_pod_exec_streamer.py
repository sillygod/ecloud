"""Kubernetes pod exec streaming for interactive shell sessions.

Provides async interactive exec sessions with bidirectional I/O.
Integrates with WebSocket for real-time terminal interaction with Emacs vterm.
"""

import asyncio
import uuid
from dataclasses import dataclass, field
from typing import Callable, Awaitable
from kubernetes.stream import stream
from kubernetes.client.rest import ApiException

from k8s_client import K8sClient


@dataclass
class ExecSessionConfig:
    """Configuration for an exec session."""
    session_id: str
    namespace: str
    pod_name: str
    container: str | None = None
    command: list[str] = field(default_factory=lambda: ["/bin/sh"])


@dataclass
class ActiveSession:
    """Tracks an active exec session."""
    config: ExecSessionConfig
    ws_stream: any  # WebSocket stream from kubernetes.stream
    stop_event: asyncio.Event = field(default_factory=asyncio.Event)
    read_task: asyncio.Task | None = None


class K8sPodExecStreamer:
    """Manage interactive pod exec sessions via WebSocket."""
    
    def __init__(self, k8s_client: K8sClient):
        self._client = k8s_client
        self._active_sessions: dict[str, ActiveSession] = {}
    
    async def start_exec_session(
        self,
        namespace: str,
        pod_name: str,
        container: str | None = None,
        command: list[str] | None = None,
        on_output: Callable[[str, str], Awaitable[None]] | None = None,
    ) -> str:
        """Start interactive exec session.
        
        Args:
            namespace: Pod namespace
            pod_name: Pod name
            container: Container name (None = first container)
            command: Command to execute (default: ["/bin/sh"])
            on_output: Async callback(session_id, output_data)
        
        Returns:
            session_id for managing the session
        """
        session_id = str(uuid.uuid4())
        
        if command is None:
            command = ["/bin/sh"]
        
        config = ExecSessionConfig(
            session_id=session_id,
            namespace=namespace,
            pod_name=pod_name,
            container=container,
            command=command,
        )
        
        # Create WebSocket stream with interactive settings
        kwargs = {
            "name": pod_name,
            "namespace": namespace,
            "command": command,
            "stderr": True,
            "stdin": True,   # Enable stdin for interactive input
            "stdout": True,
            "tty": True,     # Enable TTY for proper terminal emulation
            "_preload_content": False,  # Required for streaming
        }
        if container:
            kwargs["container"] = container
        
        try:
            ws_stream = stream(
                self._client.core_api.connect_get_namespaced_pod_exec,
                **kwargs
            )
        except ApiException as e:
            raise RuntimeError(f"Failed to start exec session: {e}")
        
        # Create active session
        active = ActiveSession(
            config=config,
            ws_stream=ws_stream,
        )
        self._active_sessions[session_id] = active
        
        # Start reading output in background
        active.read_task = asyncio.create_task(
            self._read_output_loop(session_id, ws_stream, active.stop_event, on_output)
        )
        
        return session_id
    
    async def _read_output_loop(
        self,
        session_id: str,
        ws_stream,
        stop_event: asyncio.Event,
        on_output: Callable[[str, str], Awaitable[None]] | None,
    ):
        """Read output from exec session and send to callback."""
        try:
            while not stop_event.is_set() and ws_stream.is_open():
                # Read from stdout and stderr
                if ws_stream.peek_stdout():
                    output = ws_stream.read_stdout(timeout=0.1)
                    if output and on_output:
                        await on_output(session_id, output)
                
                if ws_stream.peek_stderr():
                    output = ws_stream.read_stderr(timeout=0.1)
                    if output and on_output:
                        await on_output(session_id, output)
                
                # Small delay to avoid busy loop
                await asyncio.sleep(0.01)
                
        except Exception as e:
            print(f"Error reading from exec session {session_id}: {e}")
            if on_output:
                try:
                    await on_output(session_id, f"\r\n[Session error: {e}]\r\n")
                except Exception:
                    pass
        finally:
            # Notify session closed
            if on_output:
                try:
                    await on_output(session_id, "\r\n[Session closed]\r\n")
                except Exception:
                    pass
    
    async def send_input(self, session_id: str, input_data: str):
        """Send input to exec session.
        
        Args:
            session_id: Session ID
            input_data: Input string to send to the shell
        """
        if session_id not in self._active_sessions:
            raise ValueError(f"Session not found: {session_id}")
        
        active = self._active_sessions[session_id]
        
        try:
            if active.ws_stream.is_open():
                active.ws_stream.write_stdin(input_data)
            else:
                raise RuntimeError("Session stream is closed")
        except Exception as e:
            raise RuntimeError(f"Failed to send input: {e}")
    
    def stop_session(self, session_id: str) -> bool:
        """Stop an exec session.
        
        Returns:
            True if session was found and stopped
        """
        if session_id not in self._active_sessions:
            return False
        
        active = self._active_sessions[session_id]
        active.stop_event.set()
        
        # Cancel read task
        if active.read_task and not active.read_task.done():
            active.read_task.cancel()
        
        # Close WebSocket stream
        try:
            if active.ws_stream.is_open():
                active.ws_stream.close()
        except Exception:
            pass
        
        del self._active_sessions[session_id]
        return True
    
    def stop_all(self):
        """Stop all active exec sessions."""
        for session_id in list(self._active_sessions.keys()):
            self.stop_session(session_id)
    
    def list_sessions(self) -> list[dict]:
        """List active exec sessions."""
        return [
            {
                "sessionId": session_id,
                "namespace": active.config.namespace,
                "podName": active.config.pod_name,
                "container": active.config.container,
                "command": " ".join(active.config.command),
            }
            for session_id, active in self._active_sessions.items()
        ]


# --- Singleton ---

_pod_exec_streamer: K8sPodExecStreamer | None = None


def get_pod_exec_streamer(k8s_client: K8sClient) -> K8sPodExecStreamer:
    """Get or create the singleton K8sPodExecStreamer instance."""
    global _pod_exec_streamer
    if _pod_exec_streamer is None:
        _pod_exec_streamer = K8sPodExecStreamer(k8s_client)
    return _pod_exec_streamer
