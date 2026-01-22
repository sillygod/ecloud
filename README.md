# ECloud - Emacs Google Cloud Manager

透過 Emacs 直接管理 Google Cloud Platform (GCP) 資源，包含 Storage, Compute Engine, SQL 與 Artifact Registry。使用 JSON-RPC 與 FastAPI server 溝通。

## 需求

- Python 3.10+
- Emacs 27.1+
- Google Cloud 服務帳戶 (Service Account) 且具有 Storage 存取權限
- [Emacs vterm](https://github.com/akermu/emacs-libvterm) (用於 SSH 功能)

## 快速開始

### 1. 設定 Service Account

```bash
export GOOGLE_APPLICATION_CREDENTIALS=/path/to/your/service-account.json
```

### 2. 啟動 Python Server

```bash
cd /yourpath/ecloud/server
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
(add-to-list 'load-path "/yourpath/ecloud/emacs")
(require 'ecloud)

;; when you want to reload modified code
(load-file "/yourpath/ecloud/emacs/ecloud.el")
(load-file "/yourpath/ecloud/emacs/ecloud-ips.el")
(load-file "/yourpath/ecloud/emacs/ecloud-browser.el")
(load-file "/yourpath/ecloud/emacs/ecloud-gar.el")
(load-file "/yourpath/ecloud/emacs/ecloud-sql.el")
(load-file "/yourpath/ecloud/emacs/ecloud-rpc.el")
(load-file "/yourpath/ecloud/emacs/ecloud-compute.el")
(load-file "/yourpath/ecloud/emacs/ecloud-k8s.el")
(load-file "/yourpath/ecloud/emacs/ecloud-ws.el")
(load-file "/yourpath/ecloud/emacs/ecloud-commands.el")


;; 可選：自訂 server URL（預設為 http://127.0.0.1:8765/jsonrpc）
;; (setq ecloud-server-url "http://localhost:8765/jsonrpc")
```

### 4. 使用

- `M-x ecloud-browse` - 開啟 GCS 瀏覽器
- `M-x ecloud-server-status` - 檢查 server 連線
- `M-x ecloud-ips-list` - 管理靜態 IP
- `M-x ecloud-compute-list` - 管理 VM 實例與 SSH
- `M-x ecloud-sql-list` - 管理 Cloud SQL 與 Proxy
- `M-x ecloud-k8s-list` - 管理 GKE Clusters 與 K8s 資源
- `M-x ecloud-ws-connect` - 連線 WebSocket (即時更新)

## 瀏覽器快捷鍵 (GCS)

| 按鍵 | 功能 |
|------|------|
| `RET` | 進入 bucket/資料夾 |
| `^` | 返回上一層 |
| `r` | 重新整理 |
| `d` | 下載檔案 |
| `u` | 上傳檔案 |
| `D` | 刪除檔案 |
| `+` | 建立資料夾 |
| `c` | 複製 gs:// URL |
| `q` | 關閉視窗 |

> 如果您使用 **Evil mode**，上述按鍵在 `normal` state 下也支援。

## Kubernetes (GKE)

使用 `M-x ecloud-k8s-list` 管理 GKE Clusters。連線後可檢視 Pods, Services, Ingresses, Deployments 等資源。

### 支援功能
- **多視圖切換**：Pods (`p`), Services (`s`), Ingresses (`i`), Deployments (`d`), Namespaces (`n`)。
- **日誌串流**：即時查看 Pod Logs (WebSocket)。
- **YAML 檢視**：快速查看資源定義。

### 快捷鍵

| 按鍵 | 功能 |
|------|------|
| `RET` | 連線 Cluster / 查看 Log / 查看 YAML |
| `p` / `s` / `i` / `d` / `n` | 切換視圖 (Pods, Services, Ingresses, Deployments, Namespaces) |
| `N` | 設定 Namespace Filter |
| `y` | 查看資源 YAML |
| `l` | 查看 Logs (靜態) |
| `L` | 開始 Logs Streaming |
| `r` | 重新整理 |
| `Q` | 斷開 Cluster 連線 |
| `q` | 關閉視窗 |

## Cloud SQL

使用 `M-x ecloud-sql-list` 管理 SQL 實例。支援透過 `cloud-sql-python-connector` 建立安全的 TCP Proxy。

| 按鍵 | 功能 |
|------|------|
| `p` | 啟動/停止 TCP Proxy (本地連線用) |
| `d` | 列出資料庫 |
| `u` | 列出使用者 |
| `+ d` | 建立資料庫 |
| `D d` | 刪除資料庫 |
| `+ u` | 建立使用者 |
| `D u` | 刪除使用者 |
| `r` | 重新整理 |

## Real-time Updates (WebSockets)

支援透過 WebSocket 進行即時狀態更新：
- **K8s Logs**: 支援即時 Log Streaming。
- **Cloud SQL Proxy**: 啟動/停止時即時更新列表狀態 (Proxy Column)。
- **GCS**: 上傳/下載/刪除動作完成後，自動重新整理檔案列表。
- **GAR**: Docker Pull/Push 或刪除動作完成後，自動重新整理列表。

載入 `ecloud-ws.el` 時會自動嘗試連線。若需手動連線，可執行 `M-x ecloud-ws-connect`。

可透過設定 `ecloud-ws-auto-connect` 為 `nil` 來關閉自動連線。


## Artifact Registry (GAR)

使用 `M-x ecloud-gar-browse` 開啟 GAR 瀏覽器。預設會詢問 Location (e.g., `asia-east1`)。

### 快捷鍵

| 按鍵 | 功能 |
|------|------|
| `RET` | 進入 Repo/Package |
| `^` | 返回上一層 |
| `r` | 重新整理 |
| `P` | Pull Image (需安裝 docker) |
| `D` | 刪除 Repo/Image/Tag (需確認) |
| `q` | 關閉視窗 |

## Static IP Management

使用 `M-x ecloud-ips-list` 管理靜態與 Ephemeral IP。

| 按鍵 | 功能 |
|------|------|
| `r` | 重新整理 |
| `+` | 保留 (Reserve) 新的 External IP |
| `c` | 複製 IP 位址 |
| `w` | 複製名稱 |

## Compute Engine & SSH

使用 `M-x ecloud-compute-list` 列出所有 VM 實例。

| 按鍵 | 功能 |
|------|------|
| `RET` / `s` | SSH 連線 (使用 vterm) |
| `r` | 重新整理 |
| `c` | 複製 Internal IP |
| `C` | 複製 External IP |
| `w` | 複製名稱 |

### SSH 與 Service Account Impersonation

若您啟動 Server 時指定了 `GOOGLE_APPLICATION_CREDENTIALS`，Emacs 會自動嘗試使用該 Service Account 進行 SSH 連線 (`--impersonate-service-account`)。

**重要需求：**
您的個人帳號 (執行 `gcloud` 的使用者) 必須擁有目標 Service Account 的 **Service Account Token Creator** 權限 (`roles/iam.serviceAccountTokenCreator`)。

若遇到 `Permission denied` 錯誤，請執行以下指令授權：

```bash
gcloud iam service-accounts add-iam-policy-binding [SA_EMAIL] \
    --member="user:[YOUR_EMAIL]" \
    --role="roles/iam.serviceAccountTokenCreator"
```

## 實用指令 (Utility Commands)

除了瀏覽器介面外，ECloud 也提供了一些獨立指令供快速操作：

| 指令 | 說明 |
|------|------|
| `ecloud-upload-buffer` | 將當前 Buffer 內容直接上傳至 GCS |
| `ecloud-upload-file` | 上傳本地檔案至 GCS |
| `ecloud-download-file` | 從 GCS 下載檔案 |
| `ecloud-copy-gs-url` | 選取檔案並複製 `gs://` URL |
| `ecloud-delete-object` | 刪除 GCS 物件 |

## 環境變數

| 變數名稱 | 說明 | 預設值 |
|---------|------|--------|
| `GOOGLE_APPLICATION_CREDENTIALS` | Service Account JSON 路徑 | (必填) |
| `ECLOUD_HOST` | Server 綁定位址 | 127.0.0.1 |
| `ECLOUD_PORT` | Server 埠號 | 8765 |
| `ECLOUD_GCS_PROJECT` | GCP 專案 ID | (自動偵測) |
| `ECLOUD_IMPERSONATE_SA` | 指定 SSH 用的 Impersonation SA | (選填，若未設定則讀取 JSON 檔) |

## License

MIT License
