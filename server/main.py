"""FastAPI application entry point for ecloud server.

Provides a JSON-RPC endpoint for GCloud Storage operations.
"""

# IMPORTANT: Set gRPC DNS resolver BEFORE any imports
# This fixes VPN DNS issues where c-ares fails to resolve googleapis.com
import os
os.environ.setdefault('GRPC_DNS_RESOLVER', 'native')

from fastapi import FastAPI, Request, Response
from fastapi.middleware.cors import CORSMiddleware
from fastapi.responses import JSONResponse
from fastapi import WebSocket, WebSocketDisconnect
from pydantic import ValidationError
import json

from config import config
from jsonrpc_handler import (
    JsonRpcRequest,
    JsonRpcResponse,
    JsonRpcError,
    handler,
    PARSE_ERROR,
    INVALID_REQUEST,
)
from websocket_manager import get_manager


app = FastAPI(
    title="ECloud Server",
    description="GCloud Storage browser with JSON-RPC interface",
    version="0.1.0",
)

# Allow CORS for local development
app.add_middleware(
    CORSMiddleware,
    allow_origins=["*"],
    allow_credentials=True,
    allow_methods=["*"],
    allow_headers=["*"],
)


@app.get("/health")
async def health_check() -> dict:
    """Health check endpoint."""
    return {"status": "ok", "version": "0.1.0"}


@app.post("/jsonrpc")
async def jsonrpc_endpoint(request: Request) -> Response:
    """JSON-RPC 2.0 endpoint.
    
    Handles all JSON-RPC requests for GCS operations.
    """
    try:
        body = await request.body()
        data = json.loads(body)
    except json.JSONDecodeError as e:
        error_response = JsonRpcResponse(
            error=JsonRpcError(
                code=PARSE_ERROR,
                message=f"Parse error: {str(e)}",
            )
        )
        return Response(
            content=error_response.model_dump_json(),
            media_type="application/json",
        )
    
    # Handle batch requests
    if isinstance(data, list):
        responses = []
        for item in data:
            resp = await _handle_single_request(item)
            if resp is not None:  # Notifications don't return responses
                responses.append(resp)
        return Response(
            content=json.dumps([r.model_dump() for r in responses]),
            media_type="application/json",
        )
    
    # Handle single request
    response = await _handle_single_request(data)
    if response is None:
        # Notification - no response
        return Response(status_code=204)
    
    return Response(
        content=response.model_dump_json(),
        media_type="application/json",
    )


@app.websocket("/ws")
async def websocket_endpoint(websocket: WebSocket):
    manager = get_manager()
    await manager.connect(websocket)
    try:
        while True:
            # Keep connection alive and handle any incoming control messages
            await websocket.receive_text()
    except WebSocketDisconnect:
        manager.disconnect(websocket)


@app.on_event("shutdown")
async def shutdown_event():
    """Cleanup resources on shutdown."""
    from sql_proxy import get_proxy_manager
    from helm_client import reset_helm_client
    
    # Cleanup SQL proxy
    await get_proxy_manager().shutdown()
    
    # Cleanup Helm client (removes temporary kubeconfig)
    reset_helm_client()


async def _handle_single_request(data: dict) -> JsonRpcResponse | None:
    """Handle a single JSON-RPC request.
    
    Args:
        data: Parsed JSON request data.
        
    Returns:
        JsonRpcResponse or None for notifications.
    """
    try:
        rpc_request = JsonRpcRequest(**data)
    except ValidationError as e:
        return JsonRpcResponse(
            id=data.get("id"),
            error=JsonRpcError(
                code=INVALID_REQUEST,
                message=f"Invalid request: {str(e)}",
            ),
        )
    
    # Notifications (id is null) don't require a response
    is_notification = rpc_request.id is None
    
    response = await handler.handle(rpc_request)
    
    if is_notification:
        return None
    
    return response


if __name__ == "__main__":
    import uvicorn
    uvicorn.run(
        "main:app",
        host=config.host,
        port=config.port,
        reload=True,
    )
