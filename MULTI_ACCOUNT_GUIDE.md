# ECloud Multi-Account Support - 完整指南

本指南詳細說明 ECloud 的多帳號支援功能，包含設定、使用方式、遷移指南和進階用法。

## 目錄

- [概述](#概述)
- [快速開始](#快速開始)
- [配置範例](#配置範例)
- [帳號管理](#帳號管理)
- [遷移指南](#遷移指南)
- [進階用法](#進階用法)
- [故障排除](#故障排除)
- [常見問題](#常見問題)

## 概述

### 什麼是多帳號支援？

ECloud 的多帳號支援允許您：
- 同時管理多個 Google Cloud 帳號
- 為每個帳號使用不同的 service account
- 在帳號之間快速切換
- 自動管理 server processes 和 port 分配

### 架構說明

每個帳號配置會：
1. 啟動獨立的 Python FastAPI server process
2. 使用指定的 service account credentials
3. 分配唯一的 port 號（預設範圍：8765-8774）
4. 維護獨立的連線狀態

```
Emacs (Account Manager)
  ├─ staging  → Server (port 8765) → staging-sa.json
  ├─ production → Server (port 8766) → production-sa.json
  └─ dev → Server (port 8767) → dev-sa.json
```

### 主要功能

- **自動化管理**: Emacs 自動啟動、監控和終止 server processes
- **健康檢查**: 確保 server 正常運作後才允許操作
- **Port 分配**: 自動分配和釋放 port，避免衝突
- **狀態追蹤**: 即時查看所有帳號的連線狀態
- **持久化**: 記住最後使用的帳號，下次自動連線
- **向後相容**: 無需修改現有單帳號配置

## 快速開始

### 1. 準備 Service Accounts

為每個環境建立 service account 並下載 JSON 金鑰：

```bash
# Staging 環境
gcloud iam service-accounts create ecloud-staging \
  --display-name="ECloud Staging"

gcloud iam service-accounts keys create ~/gcp-keys/staging-sa.json \
  --iam-account=ecloud-staging@PROJECT_ID.iam.gserviceaccount.com

# Production 環境
gcloud iam service-accounts create ecloud-production \
  --display-name="ECloud Production"

gcloud iam service-accounts keys create ~/gcp-keys/production-sa.json \
  --iam-account=ecloud-production@PROJECT_ID.iam.gserviceaccount.com
```

### 2. 配置 Emacs

在 `init.el` 中加入：

```elisp
;; 載入 ECloud
(add-to-list 'load-path "/yourpath/ecloud/emacs")
(require 'ecloud)
(require 'ecloud-transient)

;; 配置多帳號
(setq ecloud-accounts
      '((staging . "~/gcp-keys/staging-sa.json")
        (production . "~/gcp-keys/production-sa.json")))

;; 啟用自動連線（可選）
(setq ecloud-auto-connect-last-account t)
```

### 3. 啟動使用

重新載入 Emacs 或執行：

```elisp
M-x ecloud-account-init
```

ECloud 會自動連線到上次使用的帳號（如果啟用自動連線）。

### 4. 切換帳號

使用以下任一方式切換帳號：

- **Transient Menu**: `M-x ecloud` → `a` (Switch account)
- **直接指令**: `M-x ecloud-account-switch`
- **帳號列表**: `M-x ecloud-account-list-processes` → `RET`

## 配置範例

### 基本配置（2-3 個帳號）

```elisp
(setq ecloud-accounts
      '((staging . "/path/to/staging-sa.json")
        (production . "/path/to/production-sa.json")))
```

### 多專案配置

```elisp
(setq ecloud-accounts
      '((project-a-dev . "/path/to/project-a-dev-sa.json")
        (project-a-prod . "/path/to/project-a-prod-sa.json")
        (project-b-dev . "/path/to/project-b-dev-sa.json")
        (project-b-prod . "/path/to/project-b-prod-sa.json")
        (shared-services . "/path/to/shared-sa.json")))
```

### 使用環境變數

```elisp
(setq ecloud-accounts
      `((staging . ,(expand-file-name "~/gcp-keys/staging-sa.json"))
        (production . ,(expand-file-name "~/gcp-keys/production-sa.json"))))
```

### 自訂 Port Range

```elisp
;; 支援更多帳號（20 個）
(setq ecloud-port-range '(9000 . 9019))

;; 使用高 port 號避免衝突
(setq ecloud-port-range '(18000 . 18009))
```

### 停用自動連線

```elisp
(setq ecloud-auto-connect-last-account nil)
```

### 完整配置範例

```elisp
;; ECloud Multi-Account Configuration
(use-package ecloud
  :load-path "/yourpath/ecloud/emacs"
  :config
  ;; 多帳號配置
  (setq ecloud-accounts
        '((dev . "~/gcp-keys/dev-sa.json")
          (staging . "~/gcp-keys/staging-sa.json")
          (production . "~/gcp-keys/production-sa.json")))
  
  ;; Port 配置
  (setq ecloud-port-range '(8765 . 8774))
  
  ;; 自動連線設定
  (setq ecloud-auto-connect-last-account t)
  
  ;; 載入 Transient Menu
  (require 'ecloud-transient)
  
  ;; 初始化
  (ecloud-account-init))
```

## 帳號管理

### 查看帳號狀態

#### 使用帳號列表 Buffer

```elisp
M-x ecloud-account-list-processes
```

顯示所有帳號的詳細資訊：

```
ECloud Accounts

Account              Status     Port     Started
──────────────────────────────────────────────────────────────────
* staging            running    8765     2024-01-29 10:30:15
  production         stopped    -        -
  dev                running    8766     2024-01-29 10:32:20
```

- `*` 標記表示當前活動帳號
- Status: running, stopped, starting, error
- Port: 分配的 port 號（stopped 時顯示 `-`）
- Started: server 啟動時間

**快捷鍵**:

| 按鍵 | 功能 |
|------|------|
| `RET` | 切換到該帳號 |
| `c` | 連線帳號 |
| `d` | 斷開帳號 |
| `r` | 重啟帳號 |
| `l` | 顯示 server 日誌 |
| `g` | 重新整理列表 |
| `q` | 關閉視窗 |

> **Evil mode 使用者**: 所有快捷鍵在 `motion` state 下也可使用。

#### 查詢當前帳號

```elisp
M-x ecloud-account-current
```

或在 Lisp 程式碼中：

```elisp
(ecloud-account-current)  ; 回傳 'staging 或 nil
```

#### 查詢特定帳號狀態

```elisp
(ecloud-account-status 'staging)
;; 回傳: (:status running :port 8765 :start-time ...)
```

### 連線管理

#### 連線到帳號

```elisp
M-x ecloud-account-connect RET staging RET
```

這會：
1. 驗證 service account 檔案
2. 分配可用 port
3. 啟動 Python server process
4. 執行 health check
5. 設定為當前活動帳號

#### 斷開帳號連線

```elisp
M-x ecloud-account-disconnect RET staging RET
```

這會：
1. 優雅地終止 server process (SIGTERM)
2. 釋放 port
3. 清理 process buffer
4. 更新狀態為 stopped

#### 重啟帳號

```elisp
M-x ecloud-account-restart RET staging RET
```

等同於先 disconnect 再 connect。

#### 停止所有帳號

```elisp
M-x ecloud-account-stop-all
```

終止所有正在運行的 server processes。Emacs 退出時會自動執行。

### 切換帳號

#### 互動式切換

```elisp
M-x ecloud-account-switch
```

會顯示帳號選擇列表，包含每個帳號的狀態：

```
Select account: 
  staging [running] *
  production [stopped]
  dev [running]
```

選擇帳號後：
- 如果帳號已在運行，立即切換
- 如果帳號已停止，自動啟動後切換

#### 程式化切換

```elisp
(ecloud-account-switch 'production)
```

### 查看 Server 日誌

#### 顯示 Process Buffer

```elisp
M-x ecloud-account-show-process-buffer RET staging RET
```

或在帳號列表中按 `l`。

Process buffer 包含：
- Server 啟動訊息
- 所有 stdout/stderr 輸出
- 錯誤訊息和 stack traces
- Health check 結果

#### 日誌輪替

Process buffer 會自動限制大小（預設 10000 行），防止記憶體問題。


## 進階用法

### 動態帳號配置

```elisp
(defun my-load-gcp-accounts ()
  "從配置檔載入 GCP 帳號。"
  (let ((config-file "~/.config/ecloud/accounts.el"))
    (when (file-exists-p config-file)
      (load-file config-file))))

(my-load-gcp-accounts)
```

### 帳號切換 Hook

```elisp
(defun my-after-account-switch (account-name)
  "帳號切換後的自訂動作。"
  (message "Switched to %s environment" account-name)
  ;; 可以在這裡執行其他動作，如更新 project ID
  )

;; 使用 advice 機制
(advice-add 'ecloud-account-switch :after
            (lambda (account-name)
              (my-after-account-switch account-name)))
```

### 條件式帳號選擇

```elisp
(defun my-select-account-by-time ()
  "根據時間自動選擇帳號。"
  (let ((hour (string-to-number (format-time-string "%H"))))
    (cond
     ((< hour 12) 'dev)
     ((< hour 18) 'staging)
     (t 'production))))

;; 在特定情況下使用
(ecloud-account-switch (my-select-account-by-time))
```

### 批次操作多個帳號

```elisp
(defun my-refresh-all-accounts ()
  "重新整理所有帳號的連線。"
  (interactive)
  (dolist (account (ecloud-account-list))
    (when (eq (plist-get (ecloud-account-status account) :status) 'running)
      (message "Restarting %s..." account)
      (ecloud-account-restart account)
      (sleep-for 2))))
```

### 自訂 Mode-line 顯示

```elisp
(defun my-ecloud-mode-line-format ()
  "自訂 mode-line 顯示格式。"
  (when-let ((account (ecloud-account-current)))
    (let ((status (plist-get (ecloud-account-status account) :status)))
      (propertize (format " [GCP:%s]" account)
                  'face (if (eq status 'running)
                           '(:foreground "green")
                         '(:foreground "red"))))))

;; 替換預設的 mode-line
(setq ecloud-account-mode-line
      '(:eval (my-ecloud-mode-line-format)))
```

### 整合 Projectile

```elisp
(defun my-ecloud-account-by-project ()
  "根據 Projectile project 自動選擇帳號。"
  (when (and (fboundp 'projectile-project-name)
             (projectile-project-p))
    (let ((project (projectile-project-name)))
      (cond
       ((string-match-p "staging" project) 'staging)
       ((string-match-p "prod" project) 'production)
       (t 'dev)))))

;; 在 project 切換時自動切換帳號
(add-hook 'projectile-after-switch-project-hook
          (lambda ()
            (when-let ((account (my-ecloud-account-by-project)))
              (ecloud-account-switch account))))
```

### 安全性增強

```elisp
;; 生產環境需要確認
(defun my-ecloud-production-guard ()
  "在操作 production 帳號前要求確認。"
  (when (eq (ecloud-account-current) 'production)
    (unless (yes-or-no-p "You are in PRODUCTION. Continue? ")
      (user-error "Operation cancelled"))))

;; 在危險操作前執行
(advice-add 'ecloud-delete-object :before
            (lambda (&rest _) (my-ecloud-production-guard)))
```

## 故障排除

### 診斷工具

#### 檢查配置

```elisp
;; 列出所有配置的帳號
(ecloud-account-list)

;; 檢查特定帳號配置
(ecloud-account--parse-config)
```

#### 檢查 Port 狀態

```elisp
;; 查看 port pool 狀態
ecloud-account--port-pool

;; 範例輸出: ((8765 . t) (8766 . t) (8767 . nil) ...)
;; t = 使用中, nil = 可用
```

#### 檢查 Process Registry

```elisp
;; 查看所有 process 資訊
ecloud-account--processes

;; 範例輸出:
;; ((staging . (:process #<process> :port 8765 :status running ...))
;;  (production . (:process #<process> :port 8766 :status running ...)))
```

### 診斷工具

ECloud 提供除錯工具幫助診斷帳號切換問題。

#### 載入除錯工具

```elisp
(load-file "/yourpath/ecloud/emacs/ecloud-debug.el")
```

或在 init.el 中加入：

```elisp
(require 'ecloud-debug)
```

#### 使用除錯工具

**顯示完整狀態**:

```elisp
M-x ecloud-debug-show-current-state
```

這會顯示：
- 當前活動帳號
- 所有註冊的 processes
- 將用於 RPC 請求的 URL
- Process registry 詳細資訊
- URL 解析測試結果

**測試 RPC URL**:

```elisp
M-x ecloud-debug-test-rpc-url
```

顯示下一個 RPC 請求將使用的 URL。

**強制刷新 URL**:

```elisp
M-x ecloud-debug-force-refresh-url
```

如果 URL 快取導致問題，使用此函數強制刷新。

#### 除錯範例

如果切換到 `wg` 帳號後，RPC 請求仍然連到 `unno-stg`：

1. 執行 `M-x ecloud-debug-show-current-state`
2. 檢查 "Current Account" 是否為 `wg`
3. 檢查 "RPC URL" 是否指向正確的 port
4. 檢查 Process Registry 中 `wg` 的狀態和 port
5. 如果 URL 不正確，檢查 `ecloud-account--processes` 是否有 `wg` 的條目

### 常見問題解決

#### Server 無法啟動

**症狀**: 連線帳號時出現 "Server startup failed" 錯誤

**診斷步驟**:

1. 查看 server 日誌：
   ```elisp
   M-x ecloud-account-show-process-buffer
   ```

2. 手動測試 server：
   ```bash
   cd /yourpath/ecloud/server
   GOOGLE_APPLICATION_CREDENTIALS=/path/to/sa.json \
   uv run uvicorn main:app --port 8765
   ```

3. 檢查 Python 環境：
   ```bash
   cd /yourpath/ecloud/server
   uv sync
   ```

**可能原因**:
- Python 依賴未安裝
- Service account 檔案無效
- Port 已被佔用
- 檔案權限問題

#### Health Check 失敗

**症狀**: Server 啟動但 health check 逾時

**診斷步驟**:

1. 手動測試 health endpoint：
   ```bash
   curl http://127.0.0.1:8765/health
   ```

2. 檢查防火牆設定：
   ```bash
   # macOS
   sudo /usr/libexec/ApplicationFirewall/socketfilterfw --getglobalstate
   ```

3. 增加 timeout（需修改程式碼）

**可能原因**:
- Server 啟動緩慢
- 防火牆阻擋 localhost 連線
- 系統資源不足

#### Port 衝突

**症狀**: "Port allocation failed" 或 "Address already in use"

**診斷步驟**:

1. **使用除錯工具檢查 port pool**：
   ```elisp
   (load-file "/yourpath/ecloud/emacs/ecloud-debug.el")
   M-x ecloud-debug-check-port-pool
   ```
   
   這會顯示：
   - Port pool 配置
   - 哪些 ports 在 pool 中被標記為使用中
   - 哪些 ports 在系統層面實際被使用
   - Pool 狀態與系統狀態的差異

2. **檢查系統 port 使用情況**：
   ```bash
   # 檢查整個 port range
   lsof -i :8765-8774 -sTCP:LISTEN
   
   # 檢查特定 port
   lsof -i :8765
   
   # 更詳細的資訊
   netstat -an | grep LISTEN | grep 876
   ```

3. **找出佔用 port 的程式**：
   ```bash
   lsof -i :8765 | grep LISTEN
   # 輸出範例：
   # python3  12345 user   3u  IPv4  ...  TCP *:8765 (LISTEN)
   ```

**解決方法**:

**方法 1: 重置 Port Pool**（推薦）

如果 port pool 與系統狀態不同步：

```elisp
;; 1. 停止所有 ECloud servers
M-x ecloud-account-stop-all

;; 2. 重置 port pool
M-x ecloud-debug-reset-port-pool

;; 3. 重新連線需要的帳號
M-x ecloud-account-connect
```

**方法 2: 停止佔用 port 的程式**

如果是其他程式佔用：

```bash
# 找出 PID
lsof -i :8765 | grep LISTEN | awk '{print $2}'

# 終止程式
kill -9 <PID>

# 或使用 killall（如果是 uvicorn）
killall -9 uvicorn
```

**方法 3: 增加 Port Range**

如果需要更多 ports：

```elisp
;; 在 init.el 中設定
(setq ecloud-port-range '(8765 . 8794))  ; 30 個 ports

;; 或動態設定
(setq ecloud-port-range '(9000 . 9019))  ; 使用不同的 range
```

**方法 4: 手動清理**

如果問題持續：

```elisp
;; 1. 檢查 process registry
ecloud-account--processes

;; 2. 手動清理
(setq ecloud-account--processes nil)
(setq ecloud-account--port-pool nil)

;; 3. 重新初始化
M-x ecloud-account-init
```

**預防措施**:

1. **總是使用 ECloud 指令停止 servers**：
   ```elisp
   M-x ecloud-account-disconnect
   M-x ecloud-account-stop-all
   ```
   不要直接 kill process

2. **定期檢查 port pool 狀態**：
   ```elisp
   M-x ecloud-debug-check-port-pool
   ```

3. **避免手動啟動 server**：
   讓 ECloud 自動管理 server processes

4. **Emacs 退出時自動清理**：
   ECloud 會在 `kill-emacs-hook` 中自動停止所有 servers

**新功能**: 

從最新版本開始，ECloud 使用 `ecloud-account--allocate-port-safe` 函數，會：
- 檢查 port 在 pool 中是否可用
- 檢查 port 在系統層面是否真的可用（使用 `lsof`）
- 自動跳過被其他程式佔用的 ports
- 記錄警告訊息到 log

這大幅減少了 port 衝突的問題。

#### 帳號切換後操作失敗

**症狀**: 切換帳號後 RPC 請求失敗

**診斷步驟**:

1. 確認帳號狀態：
   ```elisp
   M-x ecloud-account-list-processes
   ```

2. 檢查 server 是否完全啟動（status 應為 'running'）

3. 查看 server 日誌確認無錯誤

**可能原因**:
- Server 尚未完全啟動
- Health check 未通過
- Service account 權限不足

#### Memory 使用過高

**症狀**: Emacs 記憶體使用持續增長

**診斷步驟**:

1. 檢查 process buffer 大小：
   ```elisp
   (dolist (account (ecloud-account-list))
     (when-let ((info (ecloud-account-status account))
                (buffer (plist-get info :buffer)))
       (message "%s buffer size: %d" account (buffer-size buffer))))
   ```

2. 手動清理 buffer：
   ```elisp
   (ecloud-account--rotate-process-buffer buffer)
   ```

**解決方法**:
- Process buffer 會自動輪替（保留最後 10000 行）
- 定期重啟長時間運行的 servers
- 關閉不需要的帳號連線

### 除錯模式

啟用詳細日誌：

```elisp
;; 在 ecloud-account-manager.el 中設定
(setq ecloud-account--debug t)

;; 查看詳細日誌
(switch-to-buffer "*ecloud-account-log*")
```

## 常見問題

### Q: 可以同時連線多少個帳號？

A: 預設支援 10 個（port range 8765-8774）。可透過 `ecloud-port-range` 增加：

```elisp
(setq ecloud-port-range '(8765 . 8794))  ; 支援 30 個
```

實際限制取決於系統資源（記憶體、CPU）。

### Q: 帳號切換會影響現有的操作嗎？

A: 不會。每個帳號的 server 是獨立的。切換帳號只改變後續操作使用的 server，不影響其他帳號的連線。

### Q: 可以使用相同的 service account 建立多個帳號嗎？

A: 技術上可以，但不建議。每個帳號會啟動獨立的 server，使用相同的 credentials 沒有實際意義。

### Q: 如何在不同機器間同步帳號配置？

A: 將配置放在版本控制中（注意不要提交 service account 檔案）：

```elisp
;; ~/.emacs.d/ecloud-config.el
(setq ecloud-accounts
      '((staging . "~/gcp-keys/staging-sa.json")
        (production . "~/gcp-keys/production-sa.json")))

;; init.el
(load-file "~/.emacs.d/ecloud-config.el")
```

Service account 檔案使用統一的路徑結構（如 `~/gcp-keys/`）。

### Q: 自動連線失敗怎麼辦？

A: 自動連線失敗不會阻擋 Emacs 啟動。可以：

1. 手動連線：`M-x ecloud-account-connect`
2. 停用自動連線：`(setq ecloud-auto-connect-last-account nil)`
3. 檢查上次使用的帳號是否仍在配置中

### Q: 可以在 server 啟動時傳遞額外參數嗎？

A: 目前不支援。如需自訂 server 行為，可以：

1. 修改 `ecloud-account--start-process` 函數
2. 或使用環境變數（在 `process-environment` 中設定）

### Q: 如何監控所有帳號的健康狀態？

A: 使用帳號列表 buffer：

```elisp
M-x ecloud-account-list-processes
```

或編寫自訂函數：

```elisp
(defun my-check-all-accounts ()
  "檢查所有帳號的健康狀態。"
  (interactive)
  (dolist (account (ecloud-account-list))
    (let* ((status (ecloud-account-status account))
           (state (plist-get status :status)))
      (message "%s: %s" account state))))
```

### Q: 支援 Windows 嗎？

A: 理論上支援，但未經充分測試。主要考量：

- Process 管理在 Windows 上可能有差異
- Port 分配機制應該相同
- 建議在 WSL 中使用以獲得最佳體驗

### Q: 可以使用 Docker 容器中的 server 嗎？

A: 可以，但需要手動管理。使用 'external 帳號類型：

```elisp
;; 不設定 ecloud-accounts，直接設定 URL
(setq ecloud-server-url "http://localhost:8765/jsonrpc")
```

或在配置中指定（需要程式碼修改以支援 external 類型）。

### Q: 如何實現帳號間的資源複製？

A: 需要手動切換帳號並執行操作：

```elisp
(defun my-copy-bucket-between-accounts (bucket-name from-account to-account)
  "在帳號間複製 bucket。"
  (interactive)
  ;; 切換到來源帳號
  (ecloud-account-switch from-account)
  ;; 下載資料
  (let ((data (ecloud-rpc-call "list_objects" :bucket bucket-name)))
    ;; 切換到目標帳號
    (ecloud-account-switch to-account)
    ;; 上傳資料
    ;; ... 實作上傳邏輯
    ))
```

### Q: 效能影響如何？

A: 每個 server process 約佔用：
- 記憶體：50-100 MB（取決於操作）
- CPU：閒置時幾乎為 0

建議：
- 只啟動需要的帳號
- 定期停止不使用的帳號
- 監控系統資源使用

## 參考資源

- [README.md](README.md) - 主要文件
- [Requirements Document](.kiro/specs/multi-account-support/requirements.md) - 需求規格
- [Design Document](.kiro/specs/multi-account-support/design.md) - 設計文件
- [Google Cloud IAM Documentation](https://cloud.google.com/iam/docs) - Service Account 管理

## 貢獻

如有問題或建議，請提交 issue 或 pull request。

## License

MIT License
