# ECloud - Emacs Google Cloud Storage Browser

透過 Emacs 直接管理 Google Cloud Storage，使用 JSON-RPC 與 FastAPI server 溝通。

## 需求

- Python 3.10+
- Emacs 27.1+
- Google Cloud 服務帳戶 (Service Account) 且具有 Storage 存取權限

## 快速開始

### 1. 設定 Service Account

```bash
export GOOGLE_APPLICATION_CREDENTIALS=/path/to/your/service-account.json
```

### 2. 啟動 Python Server

```bash
cd /Users/jing/Desktop/ecloud/server
uv sync  # 安裝依賴
uv run uvicorn main:app --port 8765
```

或使用開發模式：

```bash
uv run uvicorn main:app --port 8765 --reload
```

Server 會在 `http://127.0.0.1:8765` 啟動。

### 3. 設定 Emacs

在你的 `init.el` 加入：

```elisp
(add-to-list 'load-path "/Users/jing/Desktop/ecloud/emacs")
(require 'ecloud)

;; 可選：自訂 server URL（預設為 http://127.0.0.1:8765/jsonrpc）
;; (setq ecloud-server-url "http://localhost:8765/jsonrpc")
```

### 4. 使用

- `M-x ecloud-browse` - 開啟瀏覽器
- `M-x ecloud-server-status` - 檢查 server 連線

## 瀏覽器快捷鍵

| 按鍵 | 功能 |
|------|------|
| `RET` | 進入 bucket/資料夾 |
| `^` | 返回上一層 |
| `g` | 重新整理 |
| `d` | 下載檔案 |
| `u` | 上傳檔案 |
| `D` | 刪除檔案 |
| `+` | 建立資料夾 |
| `c` | 複製 gs:// URL |
| `q` | 關閉視窗 |

## 其他指令

- `M-x ecloud-download-file` - 下載檔案（有補全）
- `M-x ecloud-upload-file` - 上傳檔案（有補全）
- `M-x ecloud-upload-buffer` - 直接上傳目前 buffer
- `M-x ecloud-delete-object` - 刪除物件
- `M-x ecloud-copy-gs-url` - 複製 gs:// URL

## 環境變數

| 變數名稱 | 說明 | 預設值 |
|---------|------|--------|
| `GOOGLE_APPLICATION_CREDENTIALS` | Service Account JSON 路徑 | (必填) |
| `ECLOUD_HOST` | Server 綁定位址 | 127.0.0.1 |
| `ECLOUD_PORT` | Server 埠號 | 8765 |
| `ECLOUD_GCS_PROJECT` | GCP 專案 ID | (自動偵測) |

## JSON-RPC API

Server 在 `/jsonrpc` endpoint 提供以下方法：

| 方法 | 參數 | 說明 |
|------|------|------|
| `ping` | - | 健康檢查 |
| `list_buckets` | - | 列出所有 bucket |
| `list_objects` | `bucket`, `prefix?` | 列出 bucket 中的物件 |
| `get_object_info` | `bucket`, `object_path` | 取得物件資訊 |
| `download_object` | `bucket`, `object_path`, `local_path` | 下載物件 |
| `upload_object` | `bucket`, `object_path`, `local_path` | 上傳物件 |
| `delete_object` | `bucket`, `object_path` | 刪除物件 |
| `create_folder` | `bucket`, `folder_path` | 建立資料夾 |

## 測試 Server

```bash
# 健康檢查
curl http://localhost:8765/health

# JSON-RPC ping
curl -X POST http://localhost:8765/jsonrpc \
  -H "Content-Type: application/json" \
  -d '{"jsonrpc":"2.0","id":1,"method":"ping","params":{}}'

# 列出 buckets
curl -X POST http://localhost:8765/jsonrpc \
  -H "Content-Type: application/json" \
  -d '{"jsonrpc":"2.0","id":1,"method":"list_buckets","params":{}}'
```
