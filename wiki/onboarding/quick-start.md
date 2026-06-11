# ECloud 快速上手指南

> 目標讀者：第一次接觸這個專案的人或 AI Agent。5 分鐘讀完，知道系統怎麼運作。

## 一句話描述

ECloud 是一個 **在 Emacs 裡管理 Google Cloud 的工具**：Emacs（elisp）透過
JSON-RPC 呼叫一個 Python FastAPI server，server 包裝各種 GCP SDK；長時間/串流
的操作（log、pod exec、部署進度）則由 server 透過 WebSocket 推回 Emacs。

## 系統架構圖

```
┌────────────────────────────┐         ┌──────────────────────────────┐
│        Emacs (elisp)        │         │      FastAPI server (Python)  │
│                            │  HTTP   │                              │
│  ecloud-<domain>.el  ─────▶│ /jsonrpc│  jsonrpc_handler.py           │
│  (tabulated-list views)    │ ───────▶│  _register_methods() 派發      │
│        │                   │         │        │                      │
│        ▼                   │         │        ▼                      │
│  ecloud-rpc.el  ───────────┘         │  <domain>_client.py ──▶ GCP   │
│        ▲                             │        │                      │
│  ecloud-ws.el  ◀───────────┐  /ws    │  websocket_manager.broadcast  │
│  (event hooks)             │◀────────│  (log / exec / progress)      │
└────────────────────────────┘  WS push└──────────────────────────────┘
```

兩層幾乎所有功能都是成對出現的：`emacs/ecloud-k8s.el` 對應 `server/k8s_client.py`，
`emacs/ecloud-cloud-run.el` 對應 `server/cloud_run_client.py`，以此類推。

## 一個請求怎麼跑（同步）

1. 使用者在 `*ECloud-K8s*` buffer 按鍵 → `ecloud-k8s-...` 指令。
2. 指令呼叫 `ecloud-rpc-request`/`-async`，組出 `{jsonrpc, id, method, params}`。
3. POST 到目前帳號的 server URL（`ecloud-rpc--get-current-url`）的 `/jsonrpc`。
4. `JsonRpcHandler.handle()` 用 method 名字在 `_methods` 找對應 handler。
5. Handler 呼叫 `get_<domain>_client()` 單例 → GCP SDK。
6. 回傳 dataclass 的 `to_dict()` 包成 `JsonRpcResponse`。
7. Emacs 解析 `:result`，更新 tabulated-list。

詳見 [[rpc-request-lifecycle]]。

## 非同步 / 串流怎麼跑

像 K8s log、pod exec、Cloud Run 部署、Service Usage 全列表這類操作，handler 會：

1. 啟動背景工作，立刻回一個 ack（例如 `session_id` / `stream_id`）。
2. 之後每有新資料就 `websocket_manager.broadcast({type, data})`。
3. `ecloud-ws--handle-message` 依 `type` 前綴派發到對應的 event hook
   （`k8s_*` → `ecloud-k8s-log-hook` 等）。
4. Hook 函式把資料塞進 buffer / vterm。

詳見 [[websocket-events]] 和 [[k8s-pod-exec-vterm]]。

## 多帳號

每個 GCP 帳號 = 一個獨立的 server OS process，各自一個 port（預設 8765–8774）和
自己的 `GOOGLE_APPLICATION_CREDENTIALS`。切帳號就是切 `ecloud-rpc--get-current-url`
指向的 server。憑證不會走 RPC 參數，而是綁在 process 的環境變數上。詳見
[[account-manager]] 與 [[multi-account-process-model]]。

## 主要進入點

| 指令 | 開什麼 |
|------|--------|
| `M-x ecloud-menu` | 總選單（transient） |
| `M-x ecloud-k8s-list` | GKE clusters / pods |
| `M-x ecloud-cloud-run-list` | Cloud Run services |
| `M-x ecloud-compute-list` | Compute Engine VMs |
| `M-x ecloud-browse` | GCS bucket/object 瀏覽 |
| `M-x ecloud-sql-list` | Cloud SQL instances |
| `M-x ecloud-account-switch` | 切換 GCP 帳號 |

每個 view 都是 `tabulated-list-mode`：`?` 看說明（transient）、`g`/`r` refresh、
`q` 離開、`RET` 進入/動作。

## 開發循環

- 跑 server：在 `server/` 下 `uv run uvicorn main:app --reload`，需要設好
  `GOOGLE_APPLICATION_CREDENTIALS`。
- 改 elisp 後：`M-x reload-ecloud`（不用重開 Emacs）。
- 改完源碼：做 session-end sync（見 [[schema]] § Sync Protocol）。

## 下一步該讀什麼

- 想懂傳輸層 → [[rpc-bridge]]、[[jsonrpc-dispatcher]]、[[jsonrpc-bridge]]
- 想懂某個服務 → `wiki/components/` 裡對應那頁
- 想懂為什麼這樣設計 → `wiki/decisions/`
- 想動手前先確認限制 → 各 component 的 **Key Invariants**
