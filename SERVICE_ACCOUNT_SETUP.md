# ECloud Service Account 設定指南

本文件詳細說明如何為 ECloud 建立和設定 Google Cloud Service Account，包含所有必要的權限和 Helm 特定設定。

## 目錄

1. [建立 Service Account](#建立-service-account)
2. [授予權限](#授予權限)
3. [Helm 特定設定](#helm-特定設定)
4. [下載和設定金鑰](#下載和設定金鑰)
5. [驗證設定](#驗證設定)
6. [安全性最佳實踐](#安全性最佳實踐)
7. [疑難排解](#疑難排解)

## 建立 Service Account

### 使用 Cloud Console

1. 前往 [Google Cloud Console - Service Accounts](https://console.cloud.google.com/iam-admin/serviceaccounts)
2. 選擇您的專案
3. 點擊「**CREATE SERVICE ACCOUNT**」
4. 填寫以下資訊：
   - **Service account name**: `ecloud-manager`（或您偏好的名稱）
   - **Service account ID**: 自動生成（例如：`ecloud-manager@project-id.iam.gserviceaccount.com`）
   - **Service account description**: `Service account for ECloud Emacs integration`
5. 點擊「**CREATE AND CONTINUE**」

### 使用 gcloud CLI

```bash
# 設定專案 ID
export PROJECT_ID="your-project-id"

# 建立 Service Account
gcloud iam service-accounts create ecloud-manager \
    --display-name="ECloud Manager" \
    --description="Service account for ECloud Emacs integration" \
    --project=$PROJECT_ID

# 取得 Service Account email
export SA_EMAIL="ecloud-manager@${PROJECT_ID}.iam.gserviceaccount.com"
echo "Service Account created: $SA_EMAIL"
```

## 授予權限

### 功能對應的 IAM 角色

根據您要使用的 ECloud 功能，授予對應的 IAM 角色：

| ECloud 功能 | 必要 IAM 角色 | 權限說明 |
|------------|-------------|---------|
| **GCS Browser** | `roles/storage.admin` | 完整的 Storage 管理權限 |
| | `roles/storage.objectAdmin` | 物件層級的管理權限（最小權限） |
| **Compute Engine** | `roles/compute.admin` | 完整的 Compute 管理權限 |
| | `roles/compute.instanceAdmin.v1` | VM 實例管理（最小權限） |
| **Cloud SQL** | `roles/cloudsql.admin` | 完整的 Cloud SQL 管理權限 |
| | `roles/cloudsql.client` | 連線和查詢權限（最小權限） |
| **Kubernetes (GKE)** | `roles/container.admin` | 完整的 GKE 管理權限 |
| | `roles/container.developer` | 開發者權限（最小權限） |
| **Helm Management** | `roles/container.developer` | 必要（包含 GKE 存取） |
| **Artifact Registry** | `roles/artifactregistry.admin` | 完整的 Artifact Registry 管理 |
| | `roles/artifactregistry.reader` | 唯讀權限（最小權限） |
| **IP Management** | `roles/compute.networkAdmin` | 網路資源管理 |

### 使用 Cloud Console 授予角色

1. 在 Service Accounts 頁面，找到您建立的 Service Account
2. 點擊右側的「⋮」選單，選擇「**Manage permissions**」
3. 或前往 [IAM 頁面](https://console.cloud.google.com/iam-admin/iam)
4. 點擊「**GRANT ACCESS**」
5. 在「New principals」欄位輸入 Service Account email
6. 在「Select a role」下拉選單中選擇角色
7. 點擊「**SAVE**」

### 使用 gcloud CLI 授予角色

```bash
# 設定變數
export PROJECT_ID="your-project-id"
export SA_EMAIL="ecloud-manager@${PROJECT_ID}.iam.gserviceaccount.com"

# 授予 Storage 權限
gcloud projects add-iam-policy-binding $PROJECT_ID \
    --member="serviceAccount:$SA_EMAIL" \
    --role="roles/storage.admin"

# 授予 Compute Engine 權限
gcloud projects add-iam-policy-binding $PROJECT_ID \
    --member="serviceAccount:$SA_EMAIL" \
    --role="roles/compute.admin"

# 授予 Cloud SQL 權限
gcloud projects add-iam-policy-binding $PROJECT_ID \
    --member="serviceAccount:$SA_EMAIL" \
    --role="roles/cloudsql.admin"

# 授予 GKE 權限（包含 Helm）
gcloud projects add-iam-policy-binding $PROJECT_ID \
    --member="serviceAccount:$SA_EMAIL" \
    --role="roles/container.admin"

# 授予 Artifact Registry 權限
gcloud projects add-iam-policy-binding $PROJECT_ID \
    --member="serviceAccount:$SA_EMAIL" \
    --role="roles/artifactregistry.admin"

# 授予網路管理權限
gcloud projects add-iam-policy-binding $PROJECT_ID \
    --member="serviceAccount:$SA_EMAIL" \
    --role="roles/compute.networkAdmin"
```

### 推薦的最小權限組合

如果您想使用所有 ECloud 功能，推薦授予以下角色：

```bash
# 完整功能的最小權限組合
ROLES=(
    "roles/storage.objectAdmin"
    "roles/compute.instanceAdmin.v1"
    "roles/cloudsql.client"
    "roles/container.developer"
    "roles/artifactregistry.reader"
    "roles/compute.networkAdmin"
)

for ROLE in "${ROLES[@]}"; do
    gcloud projects add-iam-policy-binding $PROJECT_ID \
        --member="serviceAccount:$SA_EMAIL" \
        --role="$ROLE"
done
```

## Helm 特定設定

Helm 操作需要額外的 Kubernetes RBAC 權限設定。

### 前置需求

1. Service Account 必須有 GKE 存取權限（`roles/container.developer` 或更高）
2. 您必須能夠連線到目標 GKE cluster
3. 您需要有 `kubectl` 和 cluster admin 權限

### 設定 Kubernetes RBAC

#### 選項 1: Cluster Admin 權限（開發/測試環境）

授予 Service Account 完整的 cluster 管理權限：

```bash
# 連線到您的 GKE cluster
gcloud container clusters get-credentials CLUSTER_NAME \
    --region=REGION \
    --project=$PROJECT_ID

# 授予 cluster-admin 角色
kubectl create clusterrolebinding ecloud-admin-binding \
    --clusterrole=cluster-admin \
    --user=$SA_EMAIL
```

#### 選項 2: Namespace 特定權限（生產環境）

只授予特定 namespace 的權限（更安全）：

```bash
# 為特定 namespace 授予 edit 權限
kubectl create rolebinding ecloud-helm-binding \
    --clusterrole=edit \
    --user=$SA_EMAIL \
    --namespace=default

# 如果需要管理多個 namespaces，重複執行
kubectl create rolebinding ecloud-helm-binding-prod \
    --clusterrole=edit \
    --user=$SA_EMAIL \
    --namespace=production
```

#### 選項 3: 自訂 Helm 權限（最小權限）

建立自訂 ClusterRole 只包含 Helm 必要權限：

```yaml
# helm-manager-role.yaml
apiVersion: rbac.authorization.k8s.io/v1
kind: ClusterRole
metadata:
  name: helm-manager
rules:
  # Helm release 管理
  - apiGroups: [""]
    resources: ["secrets", "configmaps"]
    verbs: ["get", "list", "create", "update", "delete"]
  
  # 查看和管理 Helm 部署的資源
  - apiGroups: ["apps"]
    resources: ["deployments", "statefulsets", "daemonsets"]
    verbs: ["get", "list", "create", "update", "delete", "patch"]
  
  - apiGroups: [""]
    resources: ["services", "pods", "persistentvolumeclaims"]
    verbs: ["get", "list", "create", "update", "delete"]
  
  - apiGroups: ["batch"]
    resources: ["jobs", "cronjobs"]
    verbs: ["get", "list", "create", "update", "delete"]
  
  - apiGroups: ["networking.k8s.io"]
    resources: ["ingresses"]
    verbs: ["get", "list", "create", "update", "delete"]
  
  # Namespace 查看
  - apiGroups: [""]
    resources: ["namespaces"]
    verbs: ["get", "list"]
---
apiVersion: rbac.authorization.k8s.io/v1
kind: ClusterRoleBinding
metadata:
  name: ecloud-helm-manager-binding
roleRef:
  apiGroup: rbac.authorization.k8s.io
  kind: ClusterRole
  name: helm-manager
subjects:
  - kind: User
    name: ecloud-manager@your-project-id.iam.gserviceaccount.com
```

套用設定：

```bash
# 更新 SA_EMAIL 後套用
sed "s/ecloud-manager@your-project-id.iam.gserviceaccount.com/$SA_EMAIL/g" \
    helm-manager-role.yaml | kubectl apply -f -
```

### 驗證 Helm 權限

```bash
# 測試 Service Account 是否能列出 releases
kubectl auth can-i list secrets --as=$SA_EMAIL --namespace=default

# 測試是否能建立 deployments
kubectl auth can-i create deployments --as=$SA_EMAIL --namespace=default

# 查看 Service Account 的所有權限
kubectl auth can-i --list --as=$SA_EMAIL
```

## 下載和設定金鑰

### 下載 JSON 金鑰

#### 使用 Cloud Console

1. 在 Service Accounts 頁面，點擊您的 Service Account
2. 切換到「**KEYS**」標籤
3. 點擊「**ADD KEY**」→「**Create new key**」
4. 選擇「**JSON**」格式
5. 點擊「**CREATE**」
6. 金鑰檔案會自動下載到您的電腦

#### 使用 gcloud CLI

```bash
# 建立並下載金鑰
gcloud iam service-accounts keys create ~/ecloud-sa-key.json \
    --iam-account=$SA_EMAIL

# 確認金鑰已建立
ls -lh ~/ecloud-sa-key.json
```

### 設定環境變數

#### 臨時設定（當前 shell session）

```bash
export GOOGLE_APPLICATION_CREDENTIALS=~/ecloud-sa-key.json
```

#### 永久設定

**Bash** (`~/.bashrc` 或 `~/.bash_profile`):

```bash
echo 'export GOOGLE_APPLICATION_CREDENTIALS=~/ecloud-sa-key.json' >> ~/.bashrc
source ~/.bashrc
```

**Zsh** (`~/.zshrc`):

```bash
echo 'export GOOGLE_APPLICATION_CREDENTIALS=~/ecloud-sa-key.json' >> ~/.zshrc
source ~/.zshrc
```

**Fish** (`~/.config/fish/config.fish`):

```fish
echo 'set -gx GOOGLE_APPLICATION_CREDENTIALS ~/ecloud-sa-key.json' >> ~/.config/fish/config.fish
source ~/.config/fish/config.fish
```

### 設定檔案權限

保護金鑰檔案，只允許當前使用者讀取：

```bash
chmod 600 ~/ecloud-sa-key.json

# 驗證權限
ls -l ~/ecloud-sa-key.json
# 應該顯示: -rw------- 1 user user ...
```

## 驗證設定

### 檢查環境變數

```bash
# 確認環境變數已設定
echo $GOOGLE_APPLICATION_CREDENTIALS

# 確認檔案存在
test -f $GOOGLE_APPLICATION_CREDENTIALS && echo "Key file exists" || echo "Key file not found"
```

### 啟用 Service Account

```bash
# 使用 Service Account 進行認證
gcloud auth activate-service-account --key-file=$GOOGLE_APPLICATION_CREDENTIALS

# 確認當前使用的帳號
gcloud auth list
```

### 測試權限

```bash
# 測試 GCP 專案存取
gcloud projects list

# 測試 Storage 權限
gcloud storage buckets list

# 測試 Compute Engine 權限
gcloud compute instances list

# 測試 GKE 權限
gcloud container clusters list

# 測試 Cloud SQL 權限
gcloud sql instances list

# 測試 Artifact Registry 權限
gcloud artifacts repositories list
```

### 測試 Helm 功能

```bash
# 連線到 GKE cluster
gcloud container clusters get-credentials CLUSTER_NAME \
    --region=REGION \
    --project=$PROJECT_ID

# 測試 kubectl 存取
kubectl get namespaces

# 測試 Helm（如果已安裝）
helm list --all-namespaces
```

## 安全性最佳實踐

### 1. 最小權限原則

只授予完成任務所需的最小權限：

- 開發環境：可以使用較寬鬆的權限（如 `admin` 角色）
- 生產環境：使用最小權限組合（如 `objectAdmin`, `developer` 等）

### 2. 金鑰管理

#### 保護金鑰檔案

```bash
# 設定嚴格的檔案權限
chmod 600 $GOOGLE_APPLICATION_CREDENTIALS

# 不要將金鑰提交到版本控制
echo "*.json" >> .gitignore
echo "*-key.json" >> .gitignore
```

#### 定期輪換金鑰

```bash
# 列出現有金鑰
gcloud iam service-accounts keys list --iam-account=$SA_EMAIL

# 建立新金鑰
gcloud iam service-accounts keys create ~/ecloud-sa-key-new.json \
    --iam-account=$SA_EMAIL

# 測試新金鑰
export GOOGLE_APPLICATION_CREDENTIALS=~/ecloud-sa-key-new.json
gcloud auth activate-service-account --key-file=$GOOGLE_APPLICATION_CREDENTIALS

# 刪除舊金鑰（取得 KEY_ID 從上面的 list 指令）
gcloud iam service-accounts keys delete KEY_ID \
    --iam-account=$SA_EMAIL
```

### 3. 監控和稽核

#### 啟用稽核日誌

```bash
# 查看 Service Account 的活動
gcloud logging read "protoPayload.authenticationInfo.principalEmail=$SA_EMAIL" \
    --limit 50 \
    --format json
```

#### 設定告警

在 Cloud Console 中設定告警，監控：
- Service Account 的異常使用
- 權限變更
- 金鑰建立/刪除

### 4. 環境隔離

為不同環境使用不同的 Service Accounts：

```bash
# 開發環境
gcloud iam service-accounts create ecloud-dev \
    --project=$DEV_PROJECT_ID

# 測試環境
gcloud iam service-accounts create ecloud-staging \
    --project=$STAGING_PROJECT_ID

# 生產環境
gcloud iam service-accounts create ecloud-prod \
    --project=$PROD_PROJECT_ID
```

### 5. 金鑰過期政策

建議設定金鑰自動過期：

```bash
# 使用 Cloud Scheduler 定期提醒輪換金鑰
# 或使用 Workload Identity（GKE 環境）避免使用金鑰檔案
```

## 疑難排解

### 問題 1: Permission denied 錯誤

**症狀**: 執行操作時出現 `403 Permission denied` 錯誤

**可能原因**:
1. Service Account 缺少必要的 IAM 角色
2. Kubernetes RBAC 權限未設定
3. 金鑰檔案過期或無效

**解決方法**:

```bash
# 1. 檢查 IAM 角色
gcloud projects get-iam-policy $PROJECT_ID \
    --flatten="bindings[].members" \
    --filter="bindings.members:serviceAccount:$SA_EMAIL"

# 2. 檢查 Kubernetes RBAC
kubectl get clusterrolebindings -o json | \
    jq -r ".items[] | select(.subjects[]?.name==\"$SA_EMAIL\") | .metadata.name"

# 3. 驗證金鑰
gcloud auth activate-service-account --key-file=$GOOGLE_APPLICATION_CREDENTIALS
```

### 問題 2: Helm operations fail with authentication error

**症狀**: Helm 操作失敗，顯示認證錯誤

**解決方法**:

```bash
# 1. 確認 Service Account 有 GKE 存取權限
gcloud projects get-iam-policy $PROJECT_ID \
    --flatten="bindings[].members" \
    --filter="bindings.members:serviceAccount:$SA_EMAIL AND bindings.role:roles/container.*"

# 2. 重新取得 cluster credentials
gcloud container clusters get-credentials CLUSTER_NAME \
    --region=REGION \
    --project=$PROJECT_ID

# 3. 測試 kubectl 存取
kubectl auth can-i list secrets --as=$SA_EMAIL
```

### 問題 3: GOOGLE_APPLICATION_CREDENTIALS not set

**症狀**: Backend 啟動失敗，顯示環境變數未設定

**解決方法**:

```bash
# 檢查環境變數
echo $GOOGLE_APPLICATION_CREDENTIALS

# 如果未設定，設定它
export GOOGLE_APPLICATION_CREDENTIALS=~/ecloud-sa-key.json

# 永久設定（加入 shell 設定檔）
echo 'export GOOGLE_APPLICATION_CREDENTIALS=~/ecloud-sa-key.json' >> ~/.bashrc
source ~/.bashrc
```

### 問題 4: Service Account key file not found

**症狀**: 金鑰檔案路徑錯誤或檔案不存在

**解決方法**:

```bash
# 確認檔案存在
ls -l $GOOGLE_APPLICATION_CREDENTIALS

# 如果不存在，重新下載
gcloud iam service-accounts keys create ~/ecloud-sa-key.json \
    --iam-account=$SA_EMAIL

# 更新環境變數
export GOOGLE_APPLICATION_CREDENTIALS=~/ecloud-sa-key.json
```

### 問題 5: Insufficient permissions for Helm operations

**症狀**: Helm 可以列出 releases 但無法安裝或升級

**解決方法**:

```bash
# 檢查 Kubernetes RBAC 權限
kubectl auth can-i create deployments --as=$SA_EMAIL --namespace=default
kubectl auth can-i create secrets --as=$SA_EMAIL --namespace=default

# 如果權限不足，授予必要的角色
kubectl create clusterrolebinding ecloud-helm-admin \
    --clusterrole=cluster-admin \
    --user=$SA_EMAIL
```

## 參考資源

- [Google Cloud Service Accounts 文件](https://cloud.google.com/iam/docs/service-accounts)
- [IAM Roles 參考](https://cloud.google.com/iam/docs/understanding-roles)
- [Kubernetes RBAC 文件](https://kubernetes.io/docs/reference/access-authn-authz/rbac/)
- [Helm 安全性最佳實踐](https://helm.sh/docs/topics/rbac/)
- [GKE Workload Identity](https://cloud.google.com/kubernetes-engine/docs/how-to/workload-identity)

## 總結

完成以上步驟後，您的 Service Account 應該已經：

✅ 建立並設定完成  
✅ 授予所有必要的 IAM 角色  
✅ 設定 Kubernetes RBAC 權限（用於 Helm）  
✅ 下載並保護金鑰檔案  
✅ 設定 `GOOGLE_APPLICATION_CREDENTIALS` 環境變數  
✅ 通過所有驗證測試  

現在您可以啟動 ECloud backend 並開始使用所有功能，包括 Helm 管理！
