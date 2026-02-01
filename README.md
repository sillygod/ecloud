# ECloud - Emacs Google Cloud Manager

透過 Emacs 直接管理 Google Cloud Platform (GCP) 資源，包含 Storage, Compute Engine, Cloud Run, Cloud Scheduler, SQL 與 Artifact Registry。使用 JSON-RPC 與 FastAPI server 溝通。

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
- `google-cloud-run` - Cloud Run 服務管理
- `google-cloud-logging` - Cloud Logging (用於 Cloud Run 日誌)
- `google-cloud-scheduler` - Cloud Scheduler 定時任務管理
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

## Multi-Account Support

ECloud 支援同時管理多個 Google Cloud 帳號，每個帳號使用獨立的 service account 和 server process。這對於需要管理多個 GCP 專案或環境（如 staging、production）的使用者特別有用。

### 配置多帳號

在 `init.el` 中設定 `ecloud-accounts` 變數：

```elisp
(setq ecloud-accounts
      '((staging . "/path/to/staging-service-account.json")
        (production . "/path/to/production-service-account.json")
        (dev . "/path/to/dev-service-account.json")))
```

### 帳號管理指令

| 指令 | 說明 |
|------|------|
| `ecloud-account-switch` | 切換到指定帳號（自動啟動 server） |
| `ecloud-account-list-processes` | 顯示所有帳號的狀態列表 |
| `ecloud-account-connect` | 連線到指定帳號 |
| `ecloud-account-disconnect` | 斷開指定帳號連線 |
| `ecloud-account-restart` | 重啟指定帳號的 server |
| `ecloud-account-stop-all` | 停止所有 server processes |
| `ecloud-account-current` | 顯示當前活動帳號 |

### 透過 Transient Menu 使用

執行 `M-x ecloud` 開啟主選單，選擇：
- `a` - 切換帳號
- `A` - 查看帳號列表

### 帳號狀態列表快捷鍵

在帳號列表 buffer 中（`M-x ecloud-account-list-processes`）：

| 按鍵 | 功能 |
|------|------|
| `RET` | 切換到該帳號 |
| `c` | 連線帳號 |
| `d` | 斷開帳號 |
| `r` | 重啟帳號 |
| `l` | 顯示 server 日誌 |
| `g` | 重新整理列表 |
| `q` | 關閉視窗 |

> 如果您使用 **Evil mode**，上述按鍵在 `motion` state 下也支援。

### 自動連線

ECloud 會記住最後使用的帳號，並在下次啟動時自動連線：

```elisp
;; 啟用自動連線（預設）
(setq ecloud-auto-connect-last-account t)

;; 停用自動連線
(setq ecloud-auto-connect-last-account nil)
```

### Port 配置

預設 port range 為 8765-8774（支援最多 10 個帳號）。可自訂：

```elisp
(setq ecloud-port-range '(9000 . 9019))  ; 支援 20 個帳號
```

### 向後相容性

如果未設定 `ecloud-accounts`，ECloud 會自動使用：
1. `GOOGLE_APPLICATION_CREDENTIALS` 環境變數（建立 'default 帳號）
2. 或現有的 `ecloud-server-url` 設定（建立 'external 帳號）

這確保現有的單帳號配置無需修改即可繼續使用。

### 詳細文件

更多配置範例、遷移指南和進階用法，請參考 [MULTI_ACCOUNT_GUIDE.md](MULTI_ACCOUNT_GUIDE.md)。

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
| **Cloud Run** | `Cloud Run Admin` 或 `Cloud Run Developer` |
| **Cloud Scheduler** | `Cloud Scheduler Admin` |
| **Cloud SQL** | `Cloud SQL Admin` 或 `Cloud SQL Client` |
| **Kubernetes (GKE)** | `Kubernetes Engine Admin` 或 `Kubernetes Engine Developer` |
| **Artifact Registry** | `Artifact Registry Administrator` 或 `Artifact Registry Reader` |
| **IP Management** | `Compute Network Admin` |

**推薦設定（完整功能）**：
- `Storage Admin` - 管理 GCS buckets 和物件
- `Compute Admin` - 管理 VM 實例和 SSH
- `Cloud Run Admin` - 管理 Cloud Run 服務和部署
- `Cloud Scheduler Admin` - 管理定時任務
- `Cloud SQL Admin` - 管理 SQL 實例和資料庫
- `Kubernetes Engine Admin` - 管理 GKE clusters 和資源
- `Artifact Registry Administrator` - 管理 Docker images
- `Compute Network Admin` - 管理靜態 IP
- `Logging Viewer` - 查看 Cloud Run 日誌

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
(load-file "/yourpath/ecloud/emacs/ecloud-scheduler.el")
(load-file "/yourpath/ecloud/emacs/ecloud-cloud-run.el")
(load-file "/yourpath/ecloud/emacs/ecloud-notify.el")
(load-file "/yourpath/ecloud/emacs/ecloud-ws.el")
(load-file "/yourpath/ecloud/emacs/ecloud-commands.el")
(load-file "/yourpath/ecloud/emacs/ecloud-transient.el")
(load-file "/yourpath/ecloud/emacs/ecloud-account-manager.el")


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
- **Compute & Containers**: Compute Engine, Cloud Run, Cloud Scheduler, Kubernetes (GKE)
- **Networking & Registry**: IP Addresses, Artifact Registry

#### 傳統指令 (向後相容)

您也可以直接使用獨立指令：

- `M-x ecloud-browse` - 開啟 GCS 瀏覽器
- `M-x ecloud-server-status` - 檢查 server 連線
- `M-x ecloud-ips-list` - 管理靜態 IP
- `M-x ecloud-compute-list` - 管理 VM 實例與 SSH
- `M-x ecloud-cloud-run-list` - 管理 Cloud Run 服務
- `M-x ecloud-scheduler-list` - 管理 Cloud Scheduler 定時任務
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
| `=` | 設定 Pod 數量限制 |
| `T` | 切換 Pod 限制開關 |
| `y` | 查看資源 YAML |
| `l` | 查看 Logs (靜態) |
| `L` | 開始 Logs Streaming |
| `r` | 重新整理 |
| `Q` | 斷開 Cluster 連線 |
| `q` | 關閉視窗 |

### 大型叢集效能優化

當叢集有大量 Pods (1000+) 時，可能會遇到載入緩慢的問題。ECloud 提供以下優化選項：

#### 設定 Pod 數量限制

在 `init.el` 中設定：

```elisp
;; 限制每次最多載入 500 個 Pods (預設值)
(setq ecloud-k8s-pod-fetch-limit 500)

;; 大型叢集建議使用更小的限制
(setq ecloud-k8s-pod-fetch-limit 200)

;; 設為 0 則不限制 (舊版行為)
(setq ecloud-k8s-pod-fetch-limit 0)
```

#### 互動式調整

在 K8s 瀏覽器中：
- 按 `T` 快速切換限制開關 (500 ↔ 無限制)
- 按 `=` 輸入自訂數量限制
- 按 `N` 選擇特定 Namespace 以縮小範圍

#### 效能對比

| Pod 數量 | 優化前 | 優化後 (limit=500) | 改善幅度 |
|---------|--------|-------------------|---------|
| 500     | ~3秒   | ~1.5秒            | 50%     |
| 1500    | ~12秒  | ~2秒              | 83%     |
| 3000    | ~30秒  | ~2秒              | 93%     |

詳細說明請參考 [QUICK_START_PERFORMANCE.md](QUICK_START_PERFORMANCE.md)。

## Helm 管理

ECloud 整合了 Kubernetes Helm 套件管理功能，讓您在 Emacs 中完整管理 Helm releases。

### 效能優化

#### 快速模式（推薦）

如果您有大量 Helm releases（> 20 個），可以使用**快速模式**來大幅提升載入速度：

```elisp
;; 切換快速模式（在 Helm releases buffer 中）
M-x ecloud-k8s-helm-toggle-details

;; 或在 init.el 中永久設定
(setq ecloud-k8s-helm-fetch-details nil)  ; 快速模式（預設為 t）
```

**效能對比**（44 個 releases）:
- **快速模式** (`fetch-details=nil`): < 1 秒 - 只顯示 name 和 namespace
- **完整模式** (`fetch-details=t`): 2-13 秒 - 顯示 chart, version, status

在快速模式下，chart/version/status 欄位會顯示 "..."，但您仍然可以按 `RET` 查看任何 release 的完整詳情。

#### Namespace Filter

從 K8s Pod list 按 `h` 切換到 Helm list 時，會自動使用當前的 namespace filter。這可以大幅減少載入時間：

- **單一 namespace**: 通常 < 1 秒
- **所有 namespaces**: 視 release 數量而定

#### 進度顯示

當獲取大量 releases 時（> 2 秒），會顯示進度訊息。

#### Backend 並行優化

Backend 使用並行請求獲取 release 詳細資訊：
- 預設最多 20 個並行請求（可調整）
- 自動處理失敗的 releases，不會阻擋整體載入

**調整並行數**（如果完整模式載入很慢）:

```bash
# 在啟動 server 前設定環境變數
export HELM_CONCURRENT_REQUESTS=30  # 增加到 30（適合強大的 cluster）
export HELM_CONCURRENT_REQUESTS=10  # 減少到 10（適合較弱的 cluster）

# 然後啟動 server
cd server
uv run uvicorn main:app --port 8765
```

或在 Emacs 中設定（需要重啟 server）:

```elisp
;; 在啟動 server 前設定
(setenv "HELM_CONCURRENT_REQUESTS" "30")
```

**效能基準**（44 個 releases，完整模式）:
- **理想情況**: 2-4 秒（並行數 20-30）
- **如果 > 10 秒**: 可能是網路延遲或 cluster 回應慢，嘗試增加並行數或使用快速模式
- **如果出現錯誤**: 減少並行數到 10 或更低

**建議**:
- **優先使用快速模式** - 對大多數使用情境已足夠
- 使用 namespace filter (`N`) 來限制範圍
- 如果有很多 releases，考慮按 namespace 分別查看
- 檢查 server logs 查看詳細的時間分析（參考 `diagnose-helm-performance.md`）

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

## Cloud Run

使用 `M-x ecloud-cloud-run-list` 管理 Cloud Run 服務。支援服務部署、日誌查看和流量管理。

| 按鍵 | 功能 |
|------|------|
| `RET` | 查看服務詳情 |
| `l` | 查看服務日誌 |
| `d` | 部署新服務或更新現有服務 |
| `D` | 刪除服務 |
| `R` | 切換區域 |
| `o` | 在瀏覽器中開啟服務 URL |
| `r` / `g` | 重新整理列表 |
| `q` | 關閉視窗 |

### Cloud Run 功能特色

- **服務管理**: 列出、查看、部署和刪除 Cloud Run 服務
- **日誌查看**: 直接從 Emacs 查看 Cloud Logging 日誌，支援嚴重性過濾
- **多區域支持**: 輕鬆切換不同 GCP 區域
- **部署配置**: 設定 CPU、記憶體、實例數量、環境變數等
- **即時更新**: 透過 WebSocket 接收部署和刪除事件通知
- **URL 快速開啟**: 一鍵在瀏覽器中開啟服務

### 部署 Cloud Run 服務

執行 `d` 鍵或 `M-x ecloud-cloud-run-deploy`，系統會提示輸入：

- **Service name**: 服務名稱
- **Container image**: 容器映像 URL (例如從 Artifact Registry)
- **Region**: 部署區域
- **Port**: 容器監聽埠號 (預設 8080)
- **CPU**: CPU 限制 (例如 "1", "2")
- **Memory**: 記憶體限制 (例如 "512Mi", "1Gi")
- **Min/Max instances**: 自動擴展範圍
- **Allow unauthenticated**: 是否允許未認證存取

## Cloud Scheduler

使用 `M-x ecloud-scheduler-list` 管理 Cloud Scheduler 定時任務。支援 HTTP、Pub/Sub 和 App Engine 目標。

| 按鍵 | 功能 |
|------|------|
| `RET` | 查看任務詳情 |
| `c` | 建立新的 HTTP 任務 |
| `p` | 暫停任務 |
| `P` | 恢復任務 |
| `R` | 立即執行任務 |
| `D` | 刪除任務 |
| `L` | 切換位置 |
| `r` / `g` | 重新整理列表 |
| `q` | 關閉視窗 |

### Cloud Scheduler 功能特色

- **任務管理**: 列出、查看、建立和刪除定時任務
- **Cron 排程**: 使用標準 Cron 語法設定執行時間
- **多種目標**: 支援 HTTP、Pub/Sub、App Engine
- **暫停/恢復**: 臨時停用任務而不刪除
- **手動觸發**: 立即執行任務進行測試
- **重試配置**: 自動重試失敗的任務
- **時區支持**: 設定任務執行的時區

### 建立 HTTP 任務

執行 `c` 鍵或 `M-x ecloud-scheduler-create-http-job`，系統會提示輸入：

- **Job name**: 任務名稱
- **Cron schedule**: Cron 表達式（例如：`0 9 * * *` 表示每天早上 9 點）
- **Target URI**: 目標 HTTP URL
- **Location**: 部署位置
- **Description**: 任務描述（可選）
- **Timezone**: 時區（預設：UTC）
- **HTTP method**: HTTP 方法（GET、POST 等）

### Cron 表達式範例

```
0 9 * * *       # 每天早上 9:00
*/15 * * * *    # 每 15 分鐘
0 0 * * 0       # 每週日午夜
0 0 1 * *       # 每月 1 號午夜
0 */6 * * *     # 每 6 小時
```

## Real-time Updates (WebSockets)

支援透過 WebSocket 進行即時狀態更新：
- **K8s Logs**: 支援即時 Log Streaming。
- **Cloud Run**: 部署和刪除服務時即時更新列表。
- **Cloud Scheduler**: 建立、暫停、恢復、刪除任務時即時更新列表。
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

### Multi-Account 相關問題

#### 錯誤：Port allocation failed

**原因**: 所有可用的 port 都已被使用，或 port pool 與系統狀態不同步

**解決方法**:

1. **檢查 port pool 狀態**：
   ```elisp
   ;; 載入除錯工具
   (load-file "/yourpath/ecloud/emacs/ecloud-debug.el")
   
   ;; 檢查 port pool 和系統 port 使用情況
   M-x ecloud-debug-check-port-pool
   ```

2. **如果發現 port pool 與系統不同步**：
   ```elisp
   ;; 停止所有 ECloud servers
   M-x ecloud-account-stop-all
   
   ;; 重置 port pool
   M-x ecloud-debug-reset-port-pool
   
   ;; 或手動重置
   (setq ecloud-account--port-pool nil)
   ```

3. **增加 port range**：
   ```elisp
   (setq ecloud-port-range '(8765 . 8784))  ; 增加到 20 個 ports
   ```

4. **檢查系統 port 使用情況**：
   ```bash
   # macOS/Linux
   lsof -i :8765-8774 -sTCP:LISTEN
   
   # 或檢查特定 port
   lsof -i :8765
   ```

5. **停止佔用 port 的其他程式**：
   ```bash
   # 找出 PID
   lsof -i :8765 | grep LISTEN
   
   # 終止程式
   kill -9 <PID>
   ```

**常見原因**:
- Port pool 沒有正確初始化
- 之前的 server process 沒有正確清理
- 其他程式（如手動啟動的 uvicorn）佔用了 port
- Port pool 與系統狀態不同步

**新功能**: 從此版本開始，ECloud 會自動檢查 port 是否真的可用（系統層面），而不只是檢查 pool 狀態。

#### 錯誤：Service account file not found

**原因**: 設定的 service account 檔案路徑不存在

**解決方法**:
1. 確認檔案路徑正確：
   ```elisp
   (setq ecloud-accounts
         '((staging . "/absolute/path/to/service-account.json")))
   ```
2. 使用絕對路徑而非相對路徑
3. 確認檔案權限可讀取：`chmod 600 /path/to/service-account.json`

#### 錯誤：Server startup failed

**原因**: Python server 無法啟動

**解決方法**:
1. 檢查 server 日誌：`M-x ecloud-account-show-process-buffer`
2. 確認 Python 環境正確：
   ```bash
   cd /yourpath/ecloud/server
   uv sync
   ```
3. 手動測試 server 啟動：
   ```bash
   GOOGLE_APPLICATION_CREDENTIALS=/path/to/sa.json uv run uvicorn main:app --port 8765
   ```

#### 錯誤：Health check timeout

**原因**: Server 啟動緩慢或無法回應

**解決方法**:
1. 檢查 Python 依賴是否完整安裝
2. 確認防火牆未阻擋 localhost 連線
3. 查看 server 日誌確認啟動狀態
4. 增加 health check timeout（需修改程式碼）

#### 帳號切換後 RPC 請求失敗

**原因**: 新帳號的 server 尚未完全啟動，或 URL 解析有問題

**解決方法**:

1. 使用除錯工具檢查狀態：
   ```elisp
   ;; 載入除錯工具
   (load-file "/yourpath/ecloud/emacs/ecloud-debug.el")
   
   ;; 顯示完整的除錯資訊
   M-x ecloud-debug-show-current-state
   
   ;; 測試當前會使用的 URL
   M-x ecloud-debug-test-rpc-url
   ```

2. 檢查帳號狀態：
   ```elisp
   M-x ecloud-account-list-processes
   ```
   確認 server 狀態為 'running'

3. 查看 server 日誌確認無錯誤：
   ```elisp
   M-x ecloud-account-show-process-buffer
   ```

4. 如果問題持續，嘗試重啟帳號：
   ```elisp
   M-x ecloud-account-restart
   ```

**常見原因**:
- Server 尚未完全啟動（等待幾秒後重試）
- Health check 未通過
- Service account 權限不足
- URL 解析邏輯有問題（使用除錯工具確認）

#### Mode-line 未顯示當前帳號

**原因**: Mode-line 更新未觸發

**解決方法**:
1. 手動重新整理：`M-x force-mode-line-update`
2. 重新載入 account manager：
   ```elisp
   (load-file "/yourpath/ecloud/emacs/ecloud-account-manager.el")
   ```

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
