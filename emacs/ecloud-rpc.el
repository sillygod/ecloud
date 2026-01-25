;;; ecloud-rpc.el --- JSON-RPC connection for ecloud -*- lexical-binding: t; -*-

;; Copyright (C) 2024

;; Author: ecloud
;; Keywords: tools, cloud

;;; Commentary:

;; JSON-RPC connection management for ecloud.
;; Uses url.el to communicate with the FastAPI server over HTTP.

;;; Code:

(require 'json)
(require 'url)
(require 'url-http)

;;; User customization

(defgroup ecloud-rpc nil
  "ECloud JSON-RPC settings."
  :group 'ecloud)

(defcustom ecloud-server-url "http://127.0.0.1:8765/jsonrpc"
  "URL of the ecloud JSON-RPC server endpoint."
  :type 'string
  :group 'ecloud-rpc)

(defcustom ecloud-request-timeout 30
  "Timeout in seconds for JSON-RPC requests."
  :type 'integer
  :group 'ecloud-rpc)

;;; Internal state

(defvar ecloud-rpc--request-id 0
  "Counter for JSON-RPC request IDs.")

;;; Helper functions

(defun ecloud-rpc--next-id ()
  "Generate the next request ID."
  (cl-incf ecloud-rpc--request-id))

(defun ecloud-rpc--build-request (method params)
  "Build a JSON-RPC 2.0 request for METHOD with PARAMS."
  (let ((request (list :jsonrpc "2.0"
                       :id (ecloud-rpc--next-id)
                       :method method)))
    (when params
      (setq request (plist-put request :params params)))
    request))

(defun ecloud-rpc--parse-response (response-string)
  "Parse RESPONSE-STRING as JSON-RPC response.
Returns the result on success, signals an error on failure."
  (let* ((json-object-type 'plist)
         (json-array-type 'list)
         (json-key-type 'keyword)
         (json-false nil)
         (response (json-read-from-string response-string))
         (error-obj (plist-get response :error))
         (result (plist-get response :result)))
    (if error-obj
        (let* ((code (plist-get error-obj :code))
               (message (plist-get error-obj :message))
               (data (plist-get error-obj :data))
               (error-type (when data (plist-get data :type)))
               (details (when data (plist-get data :details)))
               (suggestion (when details (plist-get details :suggestion))))
          ;; Format error message with structured information
          (let ((formatted-msg (format "JSON-RPC Error %d: %s" code message)))
            (when error-type
              (setq formatted-msg (format "%s\nType: %s" formatted-msg error-type)))
            (when suggestion
              (setq formatted-msg (format "%s\nSuggestion: %s" formatted-msg suggestion)))
            (error "%s" formatted-msg)))
      result)))

;;; Public API

(defun ecloud-rpc-request (method &optional params)
  "Send a synchronous JSON-RPC request.
METHOD is the method name (string).
PARAMS is an optional plist of parameters.
Returns the result on success, signals an error on failure."
  (let* ((url-request-method "POST")
         (url-request-extra-headers
          '(("Content-Type" . "application/json")))
         (request (ecloud-rpc--build-request method params))
         (url-request-data (encode-coding-string
                           (json-encode request)
                           'utf-8))
         (buffer (condition-case err
                     (url-retrieve-synchronously
                      ecloud-server-url
                      nil nil
                      ecloud-request-timeout)
                   (error
                    (error "Failed to connect to ecloud server at %s: %s"
                           ecloud-server-url (error-message-string err))))))
    (unless buffer
      (error "Failed to connect to ecloud server at %s. Please ensure the server is running"
             ecloud-server-url))
    (unwind-protect
        (with-current-buffer buffer
          ;; Skip HTTP headers
          (goto-char (point-min))
          (re-search-forward "^\r?\n" nil t)
          (let ((response-body (buffer-substring-no-properties
                               (point) (point-max))))
            (ecloud-rpc--parse-response response-body)))
      (kill-buffer buffer))))

(defun ecloud-rpc-request-async (method callback &optional params error-callback)
  "Send an asynchronous JSON-RPC request.
METHOD is the method name (string).
CALLBACK is called with the result on success.
PARAMS is an optional plist of parameters.
ERROR-CALLBACK is called with error message on failure."
  (let* ((url-request-method "POST")
         (url-request-extra-headers
          '(("Content-Type" . "application/json")))
         (request (ecloud-rpc--build-request method params))
         (url-request-data (encode-coding-string
                           (json-encode request)
                           'utf-8)))
    (url-retrieve
     ecloud-server-url
     (lambda (status callback error-callback)
       (if-let ((err (plist-get status :error)))
           (if error-callback
               (funcall error-callback (format "Connection error: %s" err))
             (message "ECloud error: %s" err))
         (goto-char (point-min))
         (re-search-forward "^\r?\n" nil t)
         (let ((response-body (buffer-substring-no-properties
                              (point) (point-max))))
           (condition-case err
               (funcall callback (ecloud-rpc--parse-response response-body))
             (error
              (if error-callback
                  (funcall error-callback (error-message-string err))
                (message "ECloud error: %s" (error-message-string err))))))))
     (list callback error-callback)
     t  ; silent
     t  ; inhibit cookies
     )))

;;; Convenience wrappers for common operations

(defun ecloud-rpc-ping ()
  "Ping the server to check connectivity."
  (ecloud-rpc-request "ping"))

(defun ecloud-rpc-list-buckets ()
  "List all accessible GCS buckets."
  (ecloud-rpc-request "list_buckets"))

(defun ecloud-rpc-list-objects (bucket &optional prefix)
  "List objects in BUCKET with optional PREFIX."
  (let ((params (list :bucket bucket)))
    (when prefix
      (setq params (plist-put params :prefix prefix)))
    (ecloud-rpc-request "list_objects" params)))

(defun ecloud-rpc-download (bucket object-path local-path)
  "Download OBJECT-PATH from BUCKET to LOCAL-PATH."
  (ecloud-rpc-request "download_object"
                      (list :bucket bucket
                            :object_path object-path
                            :local_path local-path)))

(defun ecloud-rpc-download-async (bucket object-path local-path callback &optional error-callback)
  "Download OBJECT-PATH from BUCKET to LOCAL-PATH asynchronously."
  (ecloud-rpc-request-async "download_object" callback 
                            (list :bucket bucket
                                  :object_path object-path
                                  :local_path local-path)
                            error-callback))

(defun ecloud-rpc-upload (bucket object-path local-path)
  "Upload LOCAL-PATH to OBJECT-PATH in BUCKET."
  (ecloud-rpc-request "upload_object"
                      (list :bucket bucket
                            :object_path object-path
                            :local_path local-path)))

(defun ecloud-rpc-upload-async (bucket object-path local-path callback &optional error-callback)
  "Upload LOCAL-PATH to OBJECT-PATH in BUCKET asynchronously."
  (ecloud-rpc-request-async "upload_object" callback
                            (list :bucket bucket
                                  :object_path object-path
                                  :local_path local-path)
                            error-callback))

(defun ecloud-rpc-delete (bucket object-path)
  "Delete OBJECT-PATH from BUCKET."
  (ecloud-rpc-request "delete_object"
                      (list :bucket bucket
                            :object_path object-path)))

(defun ecloud-rpc-delete-async (bucket object-path callback &optional error-callback)
  "Delete OBJECT-PATH from BUCKET asynchronously."
  (ecloud-rpc-request-async "delete_object" callback
                            (list :bucket bucket
                                  :object_path object-path)
                            error-callback))

(defun ecloud-rpc-create-folder (bucket folder-path)
  "Create FOLDER-PATH in BUCKET."
  (ecloud-rpc-request "create_folder"
                      (list :bucket bucket
                            :folder_path folder-path)))

(defun ecloud-rpc-create-folder-async (bucket folder-path callback &optional error-callback)
  "Create FOLDER-PATH in BUCKET asynchronously."
  (ecloud-rpc-request-async "create_folder" callback
                            (list :bucket bucket
                                  :folder_path folder-path)
                            error-callback))

(defun ecloud-rpc-batch-delete-async (bucket object-paths callback &optional error-callback)
  "Delete multiple OBJECT-PATHS from BUCKET asynchronously."
  (ecloud-rpc-request-async "batch_delete_objects" callback
                            (list :bucket bucket
                                  :object_paths object-paths)
                            error-callback))

(defun ecloud-rpc-generate-presigned-url-async (bucket object-path method expiration callback &optional error-callback)
  "Generate presigned URL asynchronously."
  (let ((params (list :bucket bucket :object_path object-path)))
    (when method (setq params (plist-put params :method method)))
    (when expiration (setq params (plist-put params :expiration expiration)))
    (ecloud-rpc-request-async "generate_presigned_url" callback params error-callback)))

(defun ecloud-rpc-update-metadata-async (bucket object-path metadata callback &optional error-callback)
  "Update metadata for OBJECT-PATH asynchronously."
  (ecloud-rpc-request-async "update_object_metadata" callback
                            (list :bucket bucket
                                  :object_path object-path
                                  :metadata metadata)
                            error-callback))

(defun ecloud-rpc-copy-object-async (source-bucket source-object dest-bucket dest-object callback &optional error-callback)
  "Copy object asynchronously."
  (ecloud-rpc-request-async "copy_object" callback
                            (list :source_bucket source-bucket
                                  :source_object source-object
                                  :dest_bucket dest-bucket
                                  :dest_object dest-object)
                            error-callback))

(defun ecloud-rpc-move-object-async (source-bucket source-object dest-bucket dest-object callback &optional error-callback)
  "Move object asynchronously."
  (ecloud-rpc-request-async "move_object" callback
                            (list :source_bucket source-bucket
                                  :source_object source-object
                                  :dest_bucket dest-bucket
                                  :dest_object dest-object)
                            error-callback))

(defun ecloud-rpc-set-lifecycle-async (bucket rules callback &optional error-callback)
  "Set lifecycle RULES for BUCKET asynchronously."
  (ecloud-rpc-request-async "set_bucket_lifecycle" callback
                            (list :bucket bucket
                                  :rules rules)
                            error-callback))

;;; GAR Operations

(defun ecloud-rpc-gar-list-locations ()
  "List available GAR locations."
  (ecloud-rpc-request "gar_list_locations" nil))

(defun ecloud-rpc-gar-list-repos (location)
  "List repositories in LOCATION."
  (ecloud-rpc-request "gar_list_repos" (list :location location)))

(defun ecloud-rpc-gar-list-packages (repo)
  "List packages in REPO."
  (ecloud-rpc-request "gar_list_packages" (list :repo repo)))

(defun ecloud-rpc-gar-list-tags (package)
  "List tags for PACKAGE."
  (ecloud-rpc-request "gar_list_tags" (list :package package)))

(defun ecloud-rpc-gar-delete-package (package)
  "Delete PACKAGE and all its tags."
  (ecloud-rpc-request "gar_delete_package" (list :package package)))

(defun ecloud-rpc-gar-delete-package-async (package callback &optional error-callback)
  "Delete PACKAGE asynchronously."
  (ecloud-rpc-request-async "gar_delete_package" callback (list :package package) error-callback))

(defun ecloud-rpc-gar-delete-tag (name)
  "Delete tag NAME."
  (ecloud-rpc-request "gar_delete_tag" (list :name name)))

(defun ecloud-rpc-gar-delete-tag-async (name callback &optional error-callback)
  "Delete tag NAME asynchronously."
  (ecloud-rpc-request-async "gar_delete_tag" callback (list :name name) error-callback))

(defun ecloud-rpc-gar-pull (uri)
  "Pull docker image URI."
  (ecloud-rpc-request "gar_pull" (list :uri uri)))

(defun ecloud-rpc-gar-pull-async (uri callback &optional error-callback)
  "Pull docker image URI asynchronously."
  (ecloud-rpc-request-async "gar_pull" callback (list :uri uri) error-callback))

(defun ecloud-rpc-gar-push (uri)
  "Push docker image URI."
  (ecloud-rpc-request "gar_push" (list :uri uri)))

(defun ecloud-rpc-gar-push-async (uri callback &optional error-callback)
  "Push docker image URI asynchronously."
  (ecloud-rpc-request-async "gar_push" callback (list :uri uri) error-callback))

(defun ecloud-rpc-gar-tag (source target)
  "Tag docker image SOURCE as TARGET."
  (ecloud-rpc-request "gar_tag" (list :source source :target target)))

(defun ecloud-rpc-gar-create-tag (package tag-id version)
  "Create a new tag for VERSION in PACKAGE."
  (ecloud-rpc-request "gar_create_tag"
                      (list :package package
                            :tag_id tag-id
                            :version version)))

;;; Compute Operations

(defun ecloud-rpc-compute-list-addresses ()
  "List all static IP addresses (regional and global)."
  (ecloud-rpc-request "compute_list_addresses" nil))

(defun ecloud-rpc-compute-reserve-address (region name)
  "Reserve a new external static IP address in REGION with NAME."
  (ecloud-rpc-request "compute_reserve_address"
                      (list :region region :name name)))

(defun ecloud-rpc-compute-list-regions ()
  "List available regions for IP address reservation."
  (ecloud-rpc-request "compute_list_regions" nil))

(defun ecloud-rpc-compute-list-instances ()
  "List all VM instances."
  (ecloud-rpc-request "compute_list_instances" nil))

(defun ecloud-rpc-compute-start-instance-async (zone instance callback &optional error-callback)
  "Start VM instance asynchronously."
  (ecloud-rpc-request-async "compute_start_instance" callback
                            (list :zone zone :instance instance)
                            error-callback))

(defun ecloud-rpc-compute-stop-instance-async (zone instance callback &optional error-callback)
  "Stop VM instance asynchronously."
  (ecloud-rpc-request-async "compute_stop_instance" callback
                            (list :zone zone :instance instance)
                            error-callback))

(defun ecloud-rpc-compute-reset-instance-async (zone instance callback &optional error-callback)
  "Reset VM instance asynchronously."
  (ecloud-rpc-request-async "compute_reset_instance" callback
                            (list :zone zone :instance instance)
                            error-callback))

(defun ecloud-rpc-compute-delete-instance-async (zone instance callback &optional error-callback)
  "Delete VM instance asynchronously."
  (ecloud-rpc-request-async "compute_delete_instance" callback
                            (list :zone zone :instance instance)
                            error-callback))

;;; System

(defun ecloud-rpc-get-config ()
  "Get server configuration."
  (ecloud-rpc-request "get_config" nil))




;;; SQL Operations

(defun ecloud-rpc-sql-list-instances ()
  "List all Cloud SQL instances."
  (ecloud-rpc-request "sql_list_instances" nil))

(defun ecloud-rpc-sql-list-instances-async (callback &optional error-callback)
  "List all Cloud SQL instances asynchronously."
  (ecloud-rpc-request-async "sql_list_instances" callback nil error-callback))

(defun ecloud-rpc-sql-list-databases (instance)
  "List databases for INSTANCE."
  (ecloud-rpc-request "sql_list_databases" (list :instance instance)))

(defun ecloud-rpc-sql-list-databases-async (instance callback &optional error-callback)
  "List databases for INSTANCE asynchronously."
  (ecloud-rpc-request-async "sql_list_databases" callback (list :instance instance) error-callback))

(defun ecloud-rpc-sql-list-users (instance)
  "List users for INSTANCE."
  (ecloud-rpc-request "sql_list_users" (list :instance instance)))

(defun ecloud-rpc-sql-list-backups-async (instance callback &optional error-callback)
  "List backups for INSTANCE asynchronously."
  (ecloud-rpc-request-async "sql_list_backups" callback (list :instance instance) error-callback))

(defun ecloud-rpc-sql-create-backup-async (instance description callback &optional error-callback)
  "Create backup for INSTANCE asynchronously."
  (ecloud-rpc-request-async "sql_create_backup" callback 
                            (list :instance instance :description description) error-callback))

(defun ecloud-rpc-sql-delete-backup-async (instance backup-id callback &optional error-callback)
  "Delete BACKUP-ID asynchronously."
  (ecloud-rpc-request-async "sql_delete_backup" callback
                            (list :instance instance :backup_id backup-id) error-callback))

(defun ecloud-rpc-sql-restore-backup-async (instance backup-id callback &optional error-callback)
  "Restore BACKUP-ID asynchronously."
  (ecloud-rpc-request-async "sql_restore_backup" callback
                            (list :instance instance :backup_id backup-id) error-callback))

(defun ecloud-rpc-sql-get-connection-info-async (instance callback &optional error-callback)
  "Get connection info asynchronously."
  (ecloud-rpc-request-async "sql_get_connection_info" callback (list :instance instance) error-callback))

(defun ecloud-rpc-sql-create-database (instance name &optional charset collation)
  "Create database NAME in INSTANCE."
  (let ((params (list :instance instance :name name)))
    (when charset (setq params (plist-put params :charset charset)))
    (when collation (setq params (plist-put params :collation collation)))
    (ecloud-rpc-request "sql_create_database" params)))

(defun ecloud-rpc-sql-create-database-async (instance name charset collation callback &optional error-callback)
  "Create database asynchronously."
  (let ((params (list :instance instance :name name)))
    (when charset (setq params (plist-put params :charset charset)))
    (when collation (setq params (plist-put params :collation collation)))
    (ecloud-rpc-request-async "sql_create_database" callback params error-callback)))

(defun ecloud-rpc-sql-delete-database (instance name)
  "Delete database NAME from INSTANCE."
  (ecloud-rpc-request "sql_delete_database" (list :instance instance :name name)))

(defun ecloud-rpc-sql-delete-database-async (instance name callback &optional error-callback)
  "Delete database asynchronously."
  (ecloud-rpc-request-async "sql_delete_database" callback (list :instance instance :name name) error-callback))

(defun ecloud-rpc-sql-list-users-async (instance callback &optional error-callback)
  "List users for INSTANCE asynchronously."
  (ecloud-rpc-request-async "sql_list_users" callback (list :instance instance) error-callback))

(defun ecloud-rpc-sql-create-user (instance name password &optional host)
  "Create user NAME in INSTANCE."
  (let ((params (list :instance instance :name name :password password)))
    (when host (setq params (plist-put params :host host)))
    (ecloud-rpc-request "sql_create_user" params)))

(defun ecloud-rpc-sql-create-user-async (instance name password host callback &optional error-callback)
  "Create user asynchronously."
  (let ((params (list :instance instance :name name :password password)))
    (when host (setq params (plist-put params :host host)))
    (ecloud-rpc-request-async "sql_create_user" callback params error-callback)))

(defun ecloud-rpc-sql-delete-user (instance name &optional host)
  "Delete user NAME from INSTANCE."
  (let ((params (list :instance instance :name name)))
    (when host (setq params (plist-put params :host host)))
    (ecloud-rpc-request "sql_delete_user" params)))

(defun ecloud-rpc-sql-delete-user-async (instance name host callback &optional error-callback)
  "Delete user asynchronously."
  (let ((params (list :instance instance :name name)))
    (when host (setq params (plist-put params :host host)))
    (ecloud-rpc-request-async "sql_delete_user" callback params error-callback)))

(defun ecloud-rpc-sql-start-proxy (connection-name &optional port db-type)
  "Start proxy for CONNECTION-NAME.
Returns plist with :port."
  (let ((params (list :connection_name connection-name)))
    (when port (setq params (plist-put params :port port)))
    (when db-type (setq params (plist-put params :db_type db-type)))
    (ecloud-rpc-request "sql_start_proxy" params)))

(defun ecloud-rpc-sql-start-proxy-async (connection-name port db-type callback &optional error-callback)
  "Start proxy asynchronously."
  (let ((params (list :connection_name connection-name)))
    (when port (setq params (plist-put params :port port)))
    (when db-type (setq params (plist-put params :db_type db-type)))
    (ecloud-rpc-request-async "sql_start_proxy" callback params error-callback)))

(defun ecloud-rpc-sql-stop-proxy (connection-name)
  "Stop proxy for CONNECTION-NAME."
  (ecloud-rpc-request "sql_stop_proxy" (list :connection_name connection-name)))

(defun ecloud-rpc-sql-stop-proxy-async (connection-name callback &optional error-callback)
  "Stop proxy asynchronously."
  (ecloud-rpc-request-async "sql_stop_proxy" callback (list :connection_name connection-name) error-callback))

(defun ecloud-rpc-sql-list-proxies ()
  "List active proxies."
  (ecloud-rpc-request "sql_list_proxies" nil))

(defun ecloud-rpc-sql-list-proxies-async (callback &optional error-callback)
  "List active proxies asynchronously."
  (ecloud-rpc-request-async "sql_list_proxies" callback nil error-callback))

;;; K8s Operations

(defun ecloud-rpc-k8s-list-clusters (&optional location)
  "List GKE clusters. LOCATION defaults to all (-).."
  (ecloud-rpc-request "k8s_list_clusters" (list :location (or location "-"))))

(defun ecloud-rpc-k8s-list-clusters-async (callback &optional location error-callback)
  "List GKE clusters asynchronously."
  (ecloud-rpc-request-async "k8s_list_clusters" callback 
                            (list :location (or location "-")) error-callback))

(defun ecloud-rpc-k8s-connect (cluster location)
  "Connect to CLUSTER in LOCATION."
  (ecloud-rpc-request "k8s_connect" (list :cluster cluster :location location)))

(defun ecloud-rpc-k8s-connect-async (cluster location callback &optional error-callback)
  "Connect to cluster asynchronously."
  (ecloud-rpc-request-async "k8s_connect" callback 
                            (list :cluster cluster :location location) error-callback))

(defun ecloud-rpc-k8s-disconnect ()
  "Disconnect from current cluster."
  (ecloud-rpc-request "k8s_disconnect" nil))

(defun ecloud-rpc-k8s-connection-status ()
  "Get current connection status."
  (ecloud-rpc-request "k8s_connection_status" nil))

(defun ecloud-rpc-k8s-list-namespaces ()
  "List namespaces."
  (ecloud-rpc-request "k8s_list_namespaces" nil))

(defun ecloud-rpc-k8s-list-namespaces-async (callback &optional error-callback)
  "List namespaces asynchronously."
  (ecloud-rpc-request-async "k8s_list_namespaces" callback nil error-callback))

(defun ecloud-rpc-k8s-list-pods (&optional namespace label-selector)
  "List pods. NAMESPACE empty means all. LABEL-SELECTOR for filtering."
  (let ((params nil))
    (when namespace (setq params (plist-put params :namespace namespace)))
    (when label-selector (setq params (plist-put params :label_selector label-selector)))
    (ecloud-rpc-request "k8s_list_pods" params)))

(defun ecloud-rpc-k8s-list-pods-async (callback &optional namespace label-selector error-callback limit)
  "List pods asynchronously.
LIMIT can be used to restrict the number of pods returned for performance."
  (let ((params nil))
    (when namespace (setq params (plist-put params :namespace namespace)))
    (when label-selector (setq params (plist-put params :label_selector label-selector)))
    (when limit (setq params (plist-put params :limit limit)))
    (ecloud-rpc-request-async "k8s_list_pods" callback params error-callback)))

(defun ecloud-rpc-k8s-list-services (&optional namespace)
  "List services."
  (let ((params nil))
    (when namespace (setq params (plist-put params :namespace namespace)))
    (ecloud-rpc-request "k8s_list_services" params)))

(defun ecloud-rpc-k8s-list-services-async (callback &optional namespace error-callback)
  "List services asynchronously."
  (let ((params nil))
    (when namespace (setq params (plist-put params :namespace namespace)))
    (ecloud-rpc-request-async "k8s_list_services" callback params error-callback)))

(defun ecloud-rpc-k8s-list-ingresses (&optional namespace)
  "List ingresses."
  (let ((params nil))
    (when namespace (setq params (plist-put params :namespace namespace)))
    (ecloud-rpc-request "k8s_list_ingresses" params)))

(defun ecloud-rpc-k8s-list-ingresses-async (callback &optional namespace error-callback)
  "List ingresses asynchronously."
  (let ((params nil))
    (when namespace (setq params (plist-put params :namespace namespace)))
    (ecloud-rpc-request-async "k8s_list_ingresses" callback params error-callback)))

(defun ecloud-rpc-k8s-list-deployments (&optional namespace)
  "List deployments."
  (let ((params nil))
    (when namespace (setq params (plist-put params :namespace namespace)))
    (ecloud-rpc-request "k8s_list_deployments" params)))

(defun ecloud-rpc-k8s-list-deployments-async (callback &optional namespace error-callback)
  "List deployments asynchronously."
  (let ((params nil))
    (when namespace (setq params (plist-put params :namespace namespace)))
    (ecloud-rpc-request-async "k8s_list_deployments" callback params error-callback)))

(defun ecloud-rpc-k8s-get-yaml (kind name namespace)
  "Get YAML for resource of KIND with NAME in NAMESPACE."
  (ecloud-rpc-request "k8s_get_yaml" (list :kind kind :name name :namespace namespace)))

(defun ecloud-rpc-k8s-get-yaml-async (kind name namespace callback &optional error-callback)
  "Get YAML asynchronously."
  (ecloud-rpc-request-async "k8s_get_yaml" callback 
                            (list :kind kind :name name :namespace namespace) error-callback))

(defun ecloud-rpc-k8s-pod-logs (name namespace &optional container tail-lines)
  "Get pod logs for NAME in NAMESPACE."
  (let ((params (list :name name :namespace namespace)))
    (when container (setq params (plist-put params :container container)))
    (when tail-lines (setq params (plist-put params :tail_lines tail-lines)))
    (ecloud-rpc-request "k8s_pod_logs" params)))

(defun ecloud-rpc-k8s-pod-logs-async (name namespace callback &optional container tail-lines error-callback)
  "Get pod logs asynchronously."
  (let ((params (list :name name :namespace namespace)))
    (when container (setq params (plist-put params :container container)))
    (when tail-lines (setq params (plist-put params :tail_lines tail-lines)))
    (ecloud-rpc-request-async "k8s_pod_logs" callback params error-callback)))

(defun ecloud-rpc-k8s-start-log-stream (&optional namespace label-selector pod-name container text-filter tail-lines)
  "Start log stream. Returns stream-id."
  (let ((params nil))
    (when namespace (setq params (plist-put params :namespace namespace)))
    (when label-selector (setq params (plist-put params :label_selector label-selector)))
    (when pod-name (setq params (plist-put params :pod_name pod-name)))
    (when container (setq params (plist-put params :container container)))
    (when text-filter (setq params (plist-put params :text_filter text-filter)))
    (when tail-lines (setq params (plist-put params :tail_lines tail-lines)))
    (ecloud-rpc-request "k8s_start_log_stream" params)))

(defun ecloud-rpc-k8s-stop-log-stream (stream-id)
  "Stop log STREAM-ID."
  (ecloud-rpc-request "k8s_stop_log_stream" (list :stream_id stream-id)))

(defun ecloud-rpc-k8s-list-log-streams ()
  "List active log streams."
  (ecloud-rpc-request "k8s_list_log_streams" nil))

(defun ecloud-rpc-k8s-scale-deployment-async (namespace name replicas callback &optional error-callback)
  "Scale deployment asynchronously."
  (ecloud-rpc-request-async "k8s_scale_deployment" callback
                            (list :namespace namespace :name name :replicas replicas)
                            error-callback))

(defun ecloud-rpc-k8s-pod-exec (namespace name command &optional container)
  "Execute command in pod. Returns output string."
  (let ((params (list :namespace namespace :name name :command command)))
    (when container (setq params (plist-put params :container container)))
    (plist-get (ecloud-rpc-request "k8s_pod_exec" params) :output)))

(defun ecloud-rpc-k8s-apply-manifest-async (manifest namespace callback &optional error-callback)
  "Apply manifest asynchronously."
  (ecloud-rpc-request-async "k8s_apply_manifest" callback
                            (list :manifest manifest :namespace (or namespace "default"))
                            error-callback))

(defun ecloud-rpc-k8s-resource-metrics-async (callback &optional error-callback)
  "Get resource metrics asynchronously."
  (ecloud-rpc-request-async "k8s_resource_metrics" callback nil error-callback))

;;; Helm Operations

(defun ecloud-rpc-helm-list-releases (&optional namespace all-namespaces)
  "List Helm releases. NAMESPACE for filtering, ALL-NAMESPACES to list all."
  (let ((params nil))
    (when namespace (setq params (plist-put params :namespace namespace)))
    (when all-namespaces (setq params (plist-put params :all_namespaces all-namespaces)))
    (ecloud-rpc-request "helm_list_releases" params)))

(defun ecloud-rpc-helm-list-releases-async (callback &optional namespace all-namespaces error-callback)
  "List Helm releases asynchronously."
  (let ((params nil))
    (when namespace (setq params (plist-put params :namespace namespace)))
    (when all-namespaces (setq params (plist-put params :all_namespaces all-namespaces)))
    (ecloud-rpc-request-async "helm_list_releases" callback params error-callback)))

(defun ecloud-rpc-helm-get-release-details (name &optional namespace)
  "Get details for Helm release NAME in NAMESPACE."
  (let ((params (list :name name)))
    (when namespace (setq params (plist-put params :namespace namespace)))
    (ecloud-rpc-request "helm_get_release_details" params)))

(defun ecloud-rpc-helm-get-release-details-async (name callback &optional namespace error-callback)
  "Get details for Helm release NAME asynchronously."
  (let ((params (list :name name)))
    (when namespace (setq params (plist-put params :namespace namespace)))
    (ecloud-rpc-request-async "helm_get_release_details" callback params error-callback)))

(defun ecloud-rpc-helm-install-chart (release-name chart-ref &optional namespace values create-namespace wait timeout)
  "Install Helm chart CHART-REF as RELEASE-NAME."
  (let ((params (list :release_name release-name :chart_ref chart-ref)))
    (when namespace (setq params (plist-put params :namespace namespace)))
    (when values (setq params (plist-put params :values values)))
    (when create-namespace (setq params (plist-put params :create_namespace create-namespace)))
    (when wait (setq params (plist-put params :wait wait)))
    (when timeout (setq params (plist-put params :timeout timeout)))
    (ecloud-rpc-request "helm_install_chart" params)))

(defun ecloud-rpc-helm-install-chart-async (release-name chart-ref callback &optional namespace values create-namespace wait timeout error-callback)
  "Install Helm chart CHART-REF as RELEASE-NAME asynchronously."
  (let ((params (list :release_name release-name :chart_ref chart-ref)))
    (when namespace (setq params (plist-put params :namespace namespace)))
    (when values (setq params (plist-put params :values values)))
    (when create-namespace (setq params (plist-put params :create_namespace create-namespace)))
    (when wait (setq params (plist-put params :wait wait)))
    (when timeout (setq params (plist-put params :timeout timeout)))
    (ecloud-rpc-request-async "helm_install_chart" callback params error-callback)))

(defun ecloud-rpc-helm-upgrade-release (release-name &optional chart-ref namespace values version wait timeout)
  "Upgrade Helm release RELEASE-NAME."
  (let ((params (list :release_name release-name)))
    (when chart-ref (setq params (plist-put params :chart_ref chart-ref)))
    (when namespace (setq params (plist-put params :namespace namespace)))
    (when values (setq params (plist-put params :values values)))
    (when version (setq params (plist-put params :version version)))
    (when wait (setq params (plist-put params :wait wait)))
    (when timeout (setq params (plist-put params :timeout timeout)))
    (ecloud-rpc-request "helm_upgrade_release" params)))

(defun ecloud-rpc-helm-upgrade-release-async (release-name callback &optional chart-ref namespace values version wait timeout error-callback)
  "Upgrade Helm release RELEASE-NAME asynchronously."
  (let ((params (list :release_name release-name)))
    (when chart-ref (setq params (plist-put params :chart_ref chart-ref)))
    (when namespace (setq params (plist-put params :namespace namespace)))
    (when values (setq params (plist-put params :values values)))
    (when version (setq params (plist-put params :version version)))
    (when wait (setq params (plist-put params :wait wait)))
    (when timeout (setq params (plist-put params :timeout timeout)))
    (ecloud-rpc-request-async "helm_upgrade_release" callback params error-callback)))

(defun ecloud-rpc-helm-rollback-release (release-name revision &optional namespace wait)
  "Rollback Helm release RELEASE-NAME to REVISION."
  (let ((params (list :release_name release-name :revision revision)))
    (when namespace (setq params (plist-put params :namespace namespace)))
    (when wait (setq params (plist-put params :wait wait)))
    (ecloud-rpc-request "helm_rollback_release" params)))

(defun ecloud-rpc-helm-rollback-release-async (release-name revision callback &optional namespace wait error-callback)
  "Rollback Helm release RELEASE-NAME to REVISION asynchronously."
  (let ((params (list :release_name release-name :revision revision)))
    (when namespace (setq params (plist-put params :namespace namespace)))
    (when wait (setq params (plist-put params :wait wait)))
    (ecloud-rpc-request-async "helm_rollback_release" callback params error-callback)))

(defun ecloud-rpc-helm-uninstall-release (release-name &optional namespace wait)
  "Uninstall Helm release RELEASE-NAME."
  (let ((params (list :release_name release-name)))
    (when namespace (setq params (plist-put params :namespace namespace)))
    (when wait (setq params (plist-put params :wait wait)))
    (ecloud-rpc-request "helm_uninstall_release" params)))

(defun ecloud-rpc-helm-uninstall-release-async (release-name callback &optional namespace wait error-callback)
  "Uninstall Helm release RELEASE-NAME asynchronously."
  (let ((params (list :release_name release-name)))
    (when namespace (setq params (plist-put params :namespace namespace)))
    (when wait (setq params (plist-put params :wait wait)))
    (ecloud-rpc-request-async "helm_uninstall_release" callback params error-callback)))

(defun ecloud-rpc-k8s-get-resources (kind &optional namespace all-namespaces)
  "Get resources of KIND. NAMESPACE for filtering, ALL-NAMESPACES to list all."
  (let ((params (list :kind kind)))
    (when namespace (setq params (plist-put params :namespace namespace)))
    (when all-namespaces (setq params (plist-put params :all_namespaces all-namespaces)))
    (ecloud-rpc-request "k8s_get_resources" params)))

(defun ecloud-rpc-k8s-get-resources-async (kind callback &optional namespace all-namespaces error-callback)
  "Get resources of KIND asynchronously."
  (let ((params (list :kind kind)))
    (when namespace (setq params (plist-put params :namespace namespace)))
    (when all-namespaces (setq params (plist-put params :all_namespaces all-namespaces)))
    (ecloud-rpc-request-async "k8s_get_resources" callback params error-callback)))

(defun ecloud-rpc-k8s-list-api-resources ()
  "List all available API resources (like kubectl api-resources)."
  (ecloud-rpc-request "k8s_list_api_resources" nil))

(defun ecloud-rpc-k8s-list-api-resources-async (callback &optional error-callback)
  "List all available API resources asynchronously."
  (ecloud-rpc-request-async "k8s_list_api_resources" callback nil error-callback))

(provide 'ecloud-rpc)
;;; ecloud-rpc.el ends here
