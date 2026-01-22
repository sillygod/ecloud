"""Kubernetes log streaming for multiple pods.

Provides async log streaming from multiple pods with filtering support.
Integrates with WebSocket for real-time log delivery to Emacs.
"""

import asyncio
import uuid
from dataclasses import dataclass, field
from typing import Callable, Awaitable
from kubernetes import watch
from kubernetes.client.rest import ApiException

from k8s_client import K8sClient


@dataclass
class LogStreamConfig:
    """Configuration for a log stream."""
    stream_id: str
    namespace: str = ""
    label_selector: str = ""
    pod_name: str = ""  # If set, only stream from this pod
    container: str | None = None
    text_filter: str = ""
    tail_lines: int = 50


@dataclass
class ActiveStream:
    """Tracks an active log stream."""
    config: LogStreamConfig
    stop_event: asyncio.Event = field(default_factory=asyncio.Event)
    tasks: list[asyncio.Task] = field(default_factory=list)


class K8sLogStreamer:
    """Stream logs from multiple Kubernetes pods."""
    
    def __init__(self, k8s_client: K8sClient):
        self._client = k8s_client
        self._active_streams: dict[str, ActiveStream] = {}
    
    async def start_stream(
        self,
        namespace: str = "",
        label_selector: str = "",
        pod_name: str = "",
        container: str | None = None,
        text_filter: str = "",
        tail_lines: int = 50,
        on_log: Callable[[str, str, str, str], Awaitable[None]] | None = None,
    ) -> str:
        """Start streaming logs.
        
        Args:
            namespace: Namespace filter (empty = all namespaces from matching pods)
            label_selector: Label selector (e.g., "app=nginx")
            pod_name: Specific pod name (if set, ignores label_selector)
            container: Specific container (None = all containers)
            text_filter: Text filter (grep-like)
            tail_lines: Number of lines to tail initially
            on_log: Async callback(stream_id, pod_name, container, line)
        
        Returns:
            stream_id for managing the stream
        """
        stream_id = str(uuid.uuid4())
        
        config = LogStreamConfig(
            stream_id=stream_id,
            namespace=namespace,
            label_selector=label_selector,
            pod_name=pod_name,
            container=container,
            text_filter=text_filter,
            tail_lines=tail_lines,
        )
        
        active = ActiveStream(config=config)
        self._active_streams[stream_id] = active
        
        # Find pods to stream from
        if pod_name:
            # Single pod mode
            pods = [{"name": pod_name, "namespace": namespace, "containers": []}]
            # Get containers for the pod
            try:
                pod_list = self._client.list_pods(namespace=namespace)
                for p in pod_list:
                    if p.name == pod_name:
                        pods[0]["containers"] = p.containers
                        break
            except Exception:
                pass
        else:
            # Multi-pod mode via label selector
            pods = [
                {"name": p.name, "namespace": p.namespace, "containers": p.containers}
                for p in self._client.list_pods(namespace, label_selector)
            ]
        
        if not pods:
            # No pods found, but keep stream open for future pods
            return stream_id
        
        # Start stream tasks for each pod/container
        for pod in pods:
            containers = pod["containers"]
            if container:
                # Only stream specific container
                containers = [container] if container in containers else []
            
            for c in containers:
                task = asyncio.create_task(
                    self._stream_pod_container(
                        stream_id=stream_id,
                        pod_name=pod["name"],
                        namespace=pod["namespace"],
                        container=c,
                        text_filter=text_filter,
                        tail_lines=tail_lines,
                        stop_event=active.stop_event,
                        on_log=on_log,
                    )
                )
                active.tasks.append(task)
        
        return stream_id
    
    async def _stream_pod_container(
        self,
        stream_id: str,
        pod_name: str,
        namespace: str,
        container: str,
        text_filter: str,
        tail_lines: int,
        stop_event: asyncio.Event,
        on_log: Callable[[str, str, str, str], Awaitable[None]] | None,
    ):
        """Stream logs from a single pod/container."""
        w = watch.Watch()
        
        try:
            # Use async iteration with the watch
            api = self._client.core_api
            
            stream = w.stream(
                api.read_namespaced_pod_log,
                name=pod_name,
                namespace=namespace,
                container=container,
                follow=True,
                tail_lines=tail_lines,
                _preload_content=False,
            )
            
            for line in stream:
                # Check if we should stop
                if stop_event.is_set():
                    break
                
                # Apply text filter
                if text_filter and text_filter.lower() not in line.lower():
                    continue
                
                # Call the callback
                if on_log:
                    try:
                        await on_log(stream_id, pod_name, container, line)
                    except Exception as e:
                        print(f"Error in log callback: {e}")
                
                # Yield to event loop
                await asyncio.sleep(0)
                
        except ApiException as e:
            if e.status == 404:
                # Pod not found, might have been deleted
                if on_log:
                    await on_log(stream_id, pod_name, container, f"[Pod {pod_name} not found]")
            elif e.status != 401:
                # Don't log 401 errors, they'll trigger refresh
                print(f"API error streaming logs: {e}")
        except Exception as e:
            print(f"Error streaming logs from {pod_name}/{container}: {e}")
        finally:
            w.stop()
    
    def stop_stream(self, stream_id: str) -> bool:
        """Stop a log stream.
        
        Returns:
            True if stream was found and stopped
        """
        if stream_id not in self._active_streams:
            return False
        
        active = self._active_streams[stream_id]
        active.stop_event.set()
        
        # Cancel all tasks
        for task in active.tasks:
            if not task.done():
                task.cancel()
        
        del self._active_streams[stream_id]
        return True
    
    def stop_all(self):
        """Stop all active log streams."""
        for stream_id in list(self._active_streams.keys()):
            self.stop_stream(stream_id)
    
    def list_streams(self) -> list[dict]:
        """List active streams."""
        return [
            {
                "streamId": stream_id,
                "namespace": active.config.namespace,
                "labelSelector": active.config.label_selector,
                "podName": active.config.pod_name,
                "container": active.config.container,
                "textFilter": active.config.text_filter,
            }
            for stream_id, active in self._active_streams.items()
        ]
    
    def update_filter(self, stream_id: str, text_filter: str) -> bool:
        """Update the text filter for an active stream.
        
        Note: This only affects future log lines, not the stream itself.
        For simplicity, we restart the stream with new filter.
        
        Returns:
            True if stream was found and updated
        """
        if stream_id not in self._active_streams:
            return False
        
        self._active_streams[stream_id].config.text_filter = text_filter
        return True


# --- Singleton ---

_log_streamer: K8sLogStreamer | None = None


def get_log_streamer(k8s_client: K8sClient) -> K8sLogStreamer:
    """Get or create the singleton K8sLogStreamer instance."""
    global _log_streamer
    if _log_streamer is None:
        _log_streamer = K8sLogStreamer(k8s_client)
    return _log_streamer
