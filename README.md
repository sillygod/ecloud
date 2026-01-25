# ECloud - Emacs Google Cloud Manager

透過 Emacs 直接管理 Google Cloud Platform (GCP) 資源，包含 Storage, Compute Engine, SQL 與 Artifact Registry。使用 JSON-RPC 與 FastAPI server 溝通。

## 需求

### 基本需求

- Python 3.10+
- Emacs 27.1+ (推薦 28+ 以使用內建 Transient)
- Google Cloud 服務帳戶 (Service Account) 且具有適當權限
- [Emacs vterm](https://github.com/akermu/emacs-libvterm) (用於 SSH 功能)

### Python 依賴

Backend 使用以下主要套件（透過 `uv` 自動安裝）：

- `fastapi` - Web framework
- `uvicorn` - ASGI server
- `google-cloud-storage` - GCS 操作
- `google-cloud-compute` - Compute Engine 管理
- `google-cloud-sql-connector` - Cloud SQL 連線
- `kubernetes` - K8s API 操作
- `pyhelm3` - Helm 3 操作
- `websockets` - 即時更新支援

### 可選需求

- **Helm CLI**: 如果 `pyhelm3` 無法使用，系統會嘗試使用 `helm` binary
  ```bash
  # macOS
  brew install helm
  
  # Linux
  curl https://raw.githubusercontent.com/helm/helm/main/scripts/get-helm-3 | bash
  ```

- **Docker**: 用於 Artifact Registry 的 pull/push 操作
  ```bash
  # macOS
  brew install docker
  ```

## 快速開始

### 1. 設定 Service Account

#### 建立 Service Account

1. 前往 [Google Cloud Console - Service Accounts](https://console.cloud.google.com/iam-admin/serviceaccounts)
2. 選擇您的專案
3. 點擊「CREATE SERVICE ACCOUNT」
4. 輸入名稱和描述（例如：`ecloud-manager`）
5. 點擊「CREATE AND CONTINUE」

#### 授予必要權限

根據您要使用的功能，授予以下角色：

| 功能 | 必要角色 |
|------|---------|
| **GCS (Storage)** | `Storage Admin` 或 `Storage Object Admin` |
| **Compute Engine** | `Compute Admin` 或 `Compute Instance Admin` |
| **Cloud SQL** | `Cloud SQL Admin` 或 `Cloud SQL Client` |
| **Kubernetes (GKE)** | `Kubernetes Engine Admin` 或 `Kubernetes Engine Developer` |
| **Artifact Registry** | `Artifact Registry Administrator` 或 `Artifact Registry Reader` |
| **IP Management** | `Compute Network Admin` |

**推薦設定（完整功能）**：
- `Storage Admin` - 管理 GCS buckets 和物件
- `Compute Admin` - 管理 VM 實例和 SSH
- `Cloud SQL Admin` - 管理 SQL 實例和資料庫
- `Kubernetes Engine Admin` - 管理 GKE clusters 和資源
- `Artifact Registry Administrator` - 管理 Docker images
- `Compute Network Admin` - 管理靜態 IP

#### Helm 特定權限

如果要使用 Helm 功能，Service Account 需要：

1. **GKE 存取權限**:
   - `Kubernetes Engine Developer` (最小權限)
   - 或 `Kubernetes Engine Admin` (完整管理)

2. **Cluster 內部權限** (透過 Kubernetes RBAC):
   ```bash
   # 授予 cluster-admin 角色（開發環境）
   kubectl create clusterrolebinding ecloud-admin \
     --clusterrole=cluster-admin \
     --user=[SERVICE_ACCOUNT_EMAIL]
   
   # 或授予特定 namespace 的權限（生產環境）
   kubectl create rolebinding ecloud-helm \
     --clusterrole=edit \
     --user=[SERVICE_ACCOUNT_EMAIL] \
     --namespace=default
   ```

#### 建立並下載金鑰

1. 在 Service Account 列表中，點擊您建立的 Service Account
2. 切換到「KEYS」標籤
3. 點擊「ADD KEY」→「Create new key」
4. 選擇「JSON」格式
5. 點擊「CREATE」，金鑰檔案會自動下載

#### 設定環境變數

將下載的 JSON 金鑰檔案放在安全的位置，並設定環境變數：

```bash
export GOOGLE_APPLICATION_CREDENTIALS=/path/to/your/service-account.json
```

**永久設定**（加入 shell 設定檔）：

```bash
# For bash (~/.bashrc or ~/.bash_profile)
echo 'export GOOGLE_APPLICATION_CREDENTIALS=/path/to/your/service-account.json' >> ~/.bashrc

# For zsh (~/.zshrc)
echo 'export GOOGLE_APPLICATION_CREDENTIALS=/path/to/your/service-account.json' >> ~/.zshrc
```

#### 驗證設定

確認 Service Account 設定正確：

```bash
# 檢查環境變數
echo $GOOGLE_APPLICATION_CREDENTIALS

# 驗證 Service Account 可以存取 GCP
gcloud auth activate-service-account --key-file=$GOOGLE_APPLICATION_CREDENTIALS

# 測試權限
gcloud projects list
gcloud storage buckets list
gcloud container clusters list
```

#### 安全性建議

1. **最小權限原則**: 只授予必要的角色
2. **金鑰保護**: 
   - 不要將金鑰檔案提交到版本控制
   - 設定檔案權限：`chmod 600 /path/to/service-account.json`
3. **定期輪換**: 定期建立新金鑰並刪除舊金鑰
4. **監控使用**: 在 Cloud Console 監控 Service Account 的活動
5. **環境隔離**: 為不同環境（開發、測試、生產）使用不同的 Service Account

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

;; 載入 Transient Menu（推薦，需要 Emacs 28+ 或安裝 transient package）
(require 'ecloud-transient)

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
(load-file "/yourpath/ecloud/emacs/ecloud-transient.el")


;; 可選：自訂 server URL（預設為 http://127.0.0.1:8765/jsonrpc）
;; (setq ecloud-server-url "http://localhost:8765/jsonrpc")
```

#### Transient Menu 需求

ECloud 的 Transient Menu 需要 Emacs Transient library：

- **Emacs 28+**: Transient 已內建，無需額外安裝
- **Emacs 27**: 需要安裝 `transient` package

**安裝 Transient (Emacs 27)**:

使用 `package.el`:
```elisp
M-x package-install RET transient RET
```

或使用 `use-package`:
```elisp
(use-package transient
  :ensure t)
```

如果不使用 Transient Menu，您仍可使用所有 `M-x ecloud-*` 指令。

### 4. 使用

#### 統一入口 - Transient Menu (推薦)

ECloud 提供統一的 Transient Menu 介面，讓您快速存取所有功能：

- `M-x ecloud` - 開啟主選單，顯示所有 GCP 服務選項

從主選單可以導航到：
- **Storage & Data**: GCS Browser, Cloud SQL
- **Compute & Containers**: Compute Engine, Kubernetes (GKE)
- **Networking & Registry**: IP Addresses, Artifact Registry

#### 傳統指令 (向後相容)

您也可以直接使用獨立指令：

- `M-x ecloud-browse` - 開啟 GCS 瀏覽器
- `M-x ecloud-server-status` - 檢查 server 連線
- `M-x ecloud-ips-list` - 管理靜態 IP
- `M-x ecloud-compute-list` - 管理 VM 實例與 SSH
- `M-x ecloud-sql-list` - 管理 Cloud SQL 與 Proxy
- `M-x ecloud-k8s-list` - 管理 GKE Clusters 與 K8s 資源
- `M-x ecloud-k8s-helm-list` - 管理 Helm Releases
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

## Transient Menu 使用指南

ECloud 使用 Emacs Transient library 提供階層式選單介面，類似 Magit 的操作體驗。

### 主選單

執行 `M-x ecloud` 開啟主選單，顯示所有可用的 GCP 服務：

```
Google Cloud Platform Services
────────────────────────────────
Storage & Data          Compute & Containers    Networking & Registry
b  Browse GCS           c  Compute Engine       i  IP Addresses
s  Cloud SQL            k  Kubernetes (GKE)     a  Artifact Registry

Actions
q  Quit
```

### Kubernetes 子選單

從主選單選擇 `k` 進入 Kubernetes 子選單，顯示當前連線狀態和所有資源選項：

```
Kubernetes - Connected: my-cluster (asia-east1)
────────────────────────────────────────────────
Resources               Cluster Actions
p  Pods                 c  Connect to cluster
s  Services             D  Disconnect
i  Ingresses
d  Deployments
n  Namespaces
h  Helm Releases

Navigation
q  Back to main menu
Q  Quit
```

### 導航技巧

- 使用單鍵快捷鍵選擇選項（如 `k` 進入 Kubernetes，`h` 查看 Helm Releases）
- `q` 返回上一層選單
- `Q` 完全退出 Transient 模式
- 選單會顯示當前狀態（如已連線的 cluster 資訊）

### Transient 需求

- **Emacs 28+**: Transient library 已內建
- **Emacs 27**: 需要手動安裝 `transient` package

如果 Transient library 不可用，您仍可使用傳統的 `M-x ecloud-*` 指令。

## Kubernetes (GKE)

使用 `M-x ecloud-k8s-list` 或透過 Transient Menu (`M-x ecloud` → `k`) 管理 GKE Clusters。連線後可檢視 Pods, Services, Ingresses, Deployments, Helm Releases 等資源。

### 支援功能
- **多視圖切換**：Pods (`p`), Services (`s`), Ingresses (`i`), Deployments (`d`), Namespaces (`n`), Helm Releases (`h`)。
- **日誌串流**：即時查看 Pod Logs (WebSocket)。
- **YAML 檢視**：快速查看資源定義。
- **Helm 管理**：安裝、升級、回滾、卸載 Helm releases。

### 快捷鍵

| 按鍵 | 功能 |
|------|------|
| `RET` | 連線 Cluster / 查看 Log / 查看 YAML |
| `p` / `s` / `i` / `d` / `n` / `h` | 切換視圖 (Pods, Services, Ingresses, Deployments, Namespaces, Helm) |
| `N` | 設定 Namespace Filter |
| `y` | 查看資源 YAML |
| `l` | 查看 Logs (靜態) |
| `L` | 開始 Logs Streaming |
| `r` | 重新整理 |
| `Q` | 斷開 Cluster 連線 |
| `q` | 關閉視窗 |

## Helm 管理

ECloud 整合了 Kubernetes Helm 套件管理功能，讓您在 Emacs 中完整管理 Helm releases。

### 啟動 Helm 管理

有三種方式進入 Helm 管理介面：

1. **透過 Transient Menu**: `M-x ecloud` → `k` (Kubernetes) → `h` (Helm Releases)
2. **透過 K8s 視圖**: 在任何 K8s 資源列表中按 `h`
3. **直接指令**: `M-x ecloud-k8s-helm-list`

### Helm Releases 列表

顯示所有已安裝的 Helm releases，包含以下資訊：

| 欄位 | 說明 |
|------|------|
| Name | Release 名稱 |
| Namespace | 部署的 namespace |
| Chart | Chart 名稱 |
| Version | Chart 版本 |
| Status | 部署狀態 (deployed, failed, pending-install 等) |

### Helm 操作快捷鍵

在 Helm Releases 列表中：

| 按鍵 | 功能 |
|------|------|
| `RET` | 查看 Release 詳細資訊 (values, revision history) |
| `i` | 安裝新的 Chart |
| `u` | 升級選中的 Release |
| `r` | 回滾到指定 Revision |
| `d` | 卸載 Release |
| `g` | 重新整理列表 |
| `q` | 關閉視窗 |

### 安裝 Helm Chart

按 `i` 開始安裝流程：

1. 輸入 Release 名稱
2. 輸入 Chart 參考（格式：`repo/chart` 或本地路徑）
3. 輸入目標 Namespace（預設：default）
4. 選擇是否編輯 Values（可自訂 Chart 設定）

範例：
```
Release name: my-app
Chart: bitnami/nginx
Namespace: production
Edit values? (y/n): y
```

### 升級 Release

在列表中選擇 Release 後按 `u`：

1. 選擇是否編輯 Values
2. 確認後執行升級
3. Revision 編號會自動遞增

### 回滾 Release

在列表中選擇 Release 後按 `r`：

1. 輸入目標 Revision 編號
2. 確認後回滾到該版本
3. Release 會恢復到指定 Revision 的狀態

### 編輯 Values

安裝或升級時可以編輯 Chart 的 Values：

1. 選擇編輯 Values 後，會開啟 YAML 編輯器
2. 預設 Values 會自動填入（如果可用）
3. 編輯完成後儲存並關閉 buffer
4. Values 會傳送給 Backend 執行操作

### Helm Repository 管理

管理 Chart repositories（未來版本將加入 UI 支援）：

```elisp
;; 列出 repositories
(ecloud-rpc-call "helm_list_repositories")

;; 新增 repository
(ecloud-rpc-call "helm_add_repository" 
                 :name "bitnami" 
                 :url "https://charts.bitnami.com/bitnami")

;; 搜尋 charts
(ecloud-rpc-call "helm_search_charts" :keyword "nginx")
```

### Helm 認證

Helm 操作使用與 K8s 相同的認證機制：

- **Kubeconfig**: 自動使用當前連線的 cluster 設定
- **Service Account**: 透過 `GOOGLE_APPLICATION_CREDENTIALS` 進行 GCP 認證
- **Context**: 切換 cluster 時自動更新 Helm context

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

## 疑難排解

### Helm 相關問題

#### 錯誤：Helm client not initialized

**原因**: 尚未連線到 Kubernetes cluster

**解決方法**:
1. 使用 `M-x ecloud-k8s-list` 或透過 Transient Menu 連線到 cluster
2. 確認連線成功後再執行 Helm 操作

#### 錯誤：Permission denied (Helm operations)

**原因**: Service Account 缺少必要的 GKE 或 Kubernetes 權限

**解決方法**:
1. 確認 Service Account 有 `Kubernetes Engine Developer` 或更高權限
2. 在 cluster 中授予 RBAC 權限：
   ```bash
   kubectl create clusterrolebinding ecloud-admin \
     --clusterrole=cluster-admin \
     --user=[SERVICE_ACCOUNT_EMAIL]
   ```

#### 錯誤：Chart not found

**原因**: Chart repository 未設定或 chart 名稱錯誤

**解決方法**:
1. 確認 chart 參考格式正確（`repo/chart` 或本地路徑）
2. 新增 repository：
   ```elisp
   (ecloud-rpc-call "helm_add_repository" 
                    :name "bitnami" 
                    :url "https://charts.bitnami.com/bitnami")
   ```
3. 搜尋可用的 charts：
   ```elisp
   (ecloud-rpc-call "helm_search_charts" :keyword "nginx")
   ```

#### 錯誤：Release already exists

**原因**: Release 名稱已被使用

**解決方法**:
1. 使用不同的 release 名稱
2. 或升級現有的 release 而非重新安裝

### Transient Menu 問題

#### 錯誤：Transient library not available

**原因**: Emacs 版本 < 28 且未安裝 transient package

**解決方法**:
1. 升級到 Emacs 28+，或
2. 安裝 transient package：`M-x package-install RET transient RET`
3. 或使用傳統指令（不需要 Transient）

### 連線問題

#### Backend 無法連線

**檢查項目**:
1. Backend server 是否正在執行：
   ```bash
   curl http://127.0.0.1:8765/health
   ```
2. 檢查 `ecloud-server-url` 設定是否正確
3. 查看 server logs 是否有錯誤訊息

#### Service Account 認證失敗

**檢查項目**:
1. 確認 `GOOGLE_APPLICATION_CREDENTIALS` 環境變數已設定
2. 確認 JSON 金鑰檔案存在且可讀取
3. 驗證 Service Account 權限：
   ```bash
   gcloud auth activate-service-account --key-file=$GOOGLE_APPLICATION_CREDENTIALS
   gcloud projects list
   ```

### 效能問題

#### Helm 操作很慢

**可能原因**:
1. Cluster 回應緩慢
2. 網路延遲
3. 大量 releases 或複雜的 charts

**改善方法**:
1. 使用 namespace 過濾減少查詢範圍
2. 檢查 cluster 健康狀態
3. 考慮使用本地 kubeconfig 快取

## License

MIT License
