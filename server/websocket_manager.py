"""WebSocket connection manager for ECloud.

Handles active WebSocket connections and broadcasting of events.
"""

from typing import Any
from fastapi import WebSocket

class ConnectionManager:
    def __init__(self):
        self.active_connections: list[WebSocket] = []

    async def connect(self, websocket: WebSocket):
        await websocket.accept()
        self.active_connections.append(websocket)

    def disconnect(self, websocket: WebSocket):
        self.active_connections.remove(websocket)

    async def broadcast(self, message: dict[str, Any]):
        for connection in self.active_connections:
            try:
                await connection.send_json(message)
            except Exception:
                # Handle disconnection or send error silently
                pass

# Singleton instance
manager = ConnectionManager()

def get_manager() -> ConnectionManager:
    return manager
