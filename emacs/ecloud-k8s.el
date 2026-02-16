;;; ecloud-k8s.el --- Kubernetes browser for ecloud -*- lexical-binding: t; -*-

;;; Commentary:

;; A browser for GKE clusters and Kubernetes resources.
;; Supports viewing pods, services, ingresses, deployments,
;; viewing YAML, and streaming logs from multiple pods.

;;; Code:

(require 'tabulated-list)
(require 'ecloud-rpc)
(require 'ecloud-notify)

;; Hook into WebSocket events for log streaming
(defvar ecloud-k8s-log-hook nil
  "Hook for receiving K8s log events from WebSocket.")

(add-hook 'ecloud-k8s-log-hook #'ecloud-k8s--on-log-event)

;;; Customization

(defgroup ecloud-k8s nil
  "ECloud Kubernetes settings."
  :group 'ecloud)

(defcustom ecloud-k8s-pod-fetch-limit 500
  "Maximum number of pods to fetch at once.
Set to 0 for no limit. For clusters with many pods (>1000),
setting a limit improves performance significantly."
  :type 'integer
  :group 'ecloud-k8s)

(defcustom ecloud-k8s-helm-fetch-details t
  "Whether to fetch detailed info for Helm releases.
When t, fetches chart name, version, and status for each release (slower).
When nil, only fetches release name and namespace (much faster).
You can toggle this with `ecloud-k8s-helm-toggle-details'."
  :type 'boolean
  :group 'ecloud-k8s)

(defface ecloud-k8s-name-face
  '((t :inherit font-lock-function-name-face :weight bold))
  "Face for resource names."
  :group 'ecloud-k8s)

(defface ecloud-k8s-namespace-face
  '((t :inherit font-lock-type-face))
  "Face for namespaces."
  :group 'ecloud-k8s)

(defface ecloud-k8s-running-face
  '((t :inherit font-lock-string-face))
  "Face for Running status."
  :group 'ecloud-k8s)

(defface ecloud-k8s-pending-face
  '((t :inherit font-lock-warning-face))
  "Face for Pending status."
  :group 'ecloud-k8s)

(defface ecloud-k8s-error-face
  '((t :inherit font-lock-warning-face :weight bold))
  "Face for Error status."
  :group 'ecloud-k8s)

;;; State

(defvar ecloud-k8s--current-view 'pods
  "Current view type: clusters, namespaces, pods, services, ingresses, deployments, helm-releases.")

(defvar ecloud-k8s--current-namespace "default"
  "Current namespace filter. nil means all namespaces.")

(defvar ecloud-k8s--current-cluster nil
  "Currently connected cluster info.")

(defvar ecloud-k8s--log-stream-id nil
  "Active log stream ID.")

(defvar ecloud-k8s--log-buffer-name "*ECloud-K8s-Logs*"
  "Name of the log streaming buffer.")

;;; Helper functions

(defun ecloud-k8s--display-error (error-msg &optional context)
  "Display ERROR-MSG with optional CONTEXT in a user-friendly way.
Parses structured error messages and displays them appropriately."
  (let ((lines (split-string error-msg "\n"))
        (main-msg "")
        (error-type nil)
        (suggestion nil))
    ;; Parse structured error format
    (dolist (line lines)
      (cond
       ((string-match "^JSON-RPC Error \\([0-9-]+\\): \\(.*\\)" line)
        (setq main-msg (match-string 2 line)))
       ((string-match "^Type: \\(.*\\)" line)
        (setq error-type (match-string 1 line)))
       ((string-match "^Suggestion: \\(.*\\)" line)
        (setq suggestion (match-string 1 line)))))
    
    ;; Build display message
    (let ((display-msg main-msg))
      (when context
        (setq display-msg (format "%s: %s" context display-msg)))
      (when suggestion
        (setq display-msg (format "%s\n\n💡 %s" display-msg suggestion)))
      
      ;; Display based on error type
      (if (or error-type suggestion)
          ;; Show in a temporary buffer for structured errors
          (let ((buffer (get-buffer-create "*ECloud Error*")))
            (with-current-buffer buffer
              (let ((inhibit-read-only t))
                (erase-buffer)
                (insert "❌ ECloud Error\n")
                (insert "═══════════════\n\n")
                (when error-type
                  (insert (format "Type: %s\n\n" error-type)))
                (insert (format "%s\n" main-msg))
                (when suggestion
                  (insert (format "\n💡 Suggestion:\n%s\n" suggestion)))
                (goto-char (point-min))
                (special-mode))
              (pop-to-buffer buffer)))
        ;; Simple message for non-structured errors
        (message "%s" display-msg)))))

(defun ecloud-k8s--format-status (status)
  "Format STATUS with appropriate face."
  (cond
   ((string= status "Running") (propertize status 'face 'ecloud-k8s-running-face))
   ((string= status "Active") (propertize status 'face 'ecloud-k8s-running-face))
   ((string= status "Pending") (propertize status 'face 'ecloud-k8s-pending-face))
   ((string= status "Waiting") (propertize status 'face 'ecloud-k8s-pending-face))
   ((member status '("Error" "Failed" "CrashLoopBackOff" "ImagePullBackOff"))
    (propertize status 'face 'ecloud-k8s-error-face))
   (t status)))

(defun ecloud-k8s--format-age (creation-time)
  "Format CREATION-TIME as a human-readable age string."
  (let* ((created (date-to-time creation-time))
         (now (current-time))
         (diff (time-subtract now created))
         (seconds (time-to-seconds diff)))
    (cond
     ((< seconds 60) (format "%ds" (floor seconds)))
     ((< seconds 3600) (format "%dm" (floor (/ seconds 60))))
     ((< seconds 86400) (format "%dh" (floor (/ seconds 3600))))
     (t (format "%dd" (floor (/ seconds 86400)))))))

;;; Clusters view

(defun ecloud-k8s--fetch-clusters ()
  "Fetch and display clusters."
  (setq ecloud-k8s--current-view 'clusters)
  (setq tabulated-list-format
        [("Name" 30 t)
         ("Location" 15 t)
         ("Status" 12 t)
         ("Version" 15 t)
         ("Nodes" 8 t)])
  (setq tabulated-list-entries nil)
  (tabulated-list-init-header)
  (tabulated-list-print t)
  (message "Fetching GKE clusters...")
  (let ((buffer (current-buffer)))
    (ecloud-rpc-k8s-list-clusters-async
     (lambda (resp)
       (let ((clusters (plist-get resp :clusters)))
         (when (buffer-live-p buffer)
           (with-current-buffer buffer
             (setq tabulated-list-entries
                   (mapcar (lambda (c)
                             (list c
                                   (vector
                                    (propertize (or (plist-get c :name) "???") 'face 'ecloud-k8s-name-face)
                                    (or (plist-get c :location) "")
                                    (ecloud-k8s--format-status (or (plist-get c :status) ""))
                                    (or (plist-get c :version) "")
                                    (format "%d" (or (plist-get c :currentNodeCount) 0)))))
                           clusters))
             (tabulated-list-print)
             (message "Found %d clusters." (length clusters))))))
     nil
     (lambda (err) (message "Error: %s" err)))))

(defun ecloud-k8s-connect-cluster ()
  "Connect to the cluster at point."
  (interactive)
  (let* ((entry (tabulated-list-get-id))
         (name (plist-get entry :name))
         (location (plist-get entry :location))
         (buffer (current-buffer)))
    (unless name (user-error "No cluster selected"))
    (ecloud-notify (format "Connecting to %s..." name))
    (ecloud-rpc-k8s-connect-async
     name location
     (lambda (resp)
       (when (buffer-live-p buffer)
         (with-current-buffer buffer
           (setq ecloud-k8s--current-cluster (plist-get resp :cluster))
           (ecloud-notify (format "Connected to %s" name))
           (ecloud-k8s--fetch-pods))))
     (lambda (err) (ecloud-notify (format "Failed to connect: %s" err) 5)))))

;;; Pods view

(defun ecloud-k8s--fetch-pods ()
  "Fetch and display pods."
  (setq ecloud-k8s--current-view 'pods)
  (setq tabulated-list-format
        [("Name" 40 t)
         ("Namespace" 20 t)
         ("Ready" 8 t)
         ("Status" 15 t)
         ("Restarts" 8 t)
         ("Age" 6 t)
         ("IP" 15 nil)])
  (setq tabulated-list-entries nil)
  (tabulated-list-init-header)
  (tabulated-list-print t)
  (let ((limit ecloud-k8s-pod-fetch-limit))
    (if (and limit (> limit 0))
        (message "Fetching up to %d pods..." limit)
      (message "Fetching pods...")))
  (let ((buffer (current-buffer))
        (start-time (current-time)))
    (ecloud-rpc-k8s-list-pods-async
     (lambda (resp)
       (let ((pods (plist-get resp :pods))
             (elapsed (float-time (time-subtract (current-time) start-time))))
         (when (buffer-live-p buffer)
           (with-current-buffer buffer
             ;; Use a more efficient approach for large lists
             (let ((entries nil))
               (dolist (p pods)
                 (push (list p
                             (vector
                              (propertize (or (plist-get p :name) "???") 'face 'ecloud-k8s-name-face)
                              (propertize (or (plist-get p :namespace) "???") 'face 'ecloud-k8s-namespace-face)
                              (or (plist-get p :ready) "")
                              (ecloud-k8s--format-status (or (plist-get p :status) ""))
                              (format "%d" (or (plist-get p :restarts) 0))
                              (or (plist-get p :age) "")
                              (or (plist-get p :ip) "")))
                       entries))
               (setq tabulated-list-entries (nreverse entries)))
             (tabulated-list-print)
             (message "Found %d pods in %.2fs." (length pods) elapsed)))))
     ecloud-k8s--current-namespace 
     nil  ; label-selector
     (lambda (err) (message "Error: %s" err))
     ecloud-k8s-pod-fetch-limit)))

;;; Services view

(defun ecloud-k8s--fetch-services ()
  "Fetch and display services."
  (setq ecloud-k8s--current-view 'services)
  (setq tabulated-list-format
        [("Name" 35 t)
         ("Namespace" 20 t)
         ("Type" 12 t)
         ("Cluster-IP" 15 nil)
         ("External-IP" 20 nil)
         ("Age" 6 t)])
  (setq tabulated-list-entries nil)
  (tabulated-list-init-header)
  (tabulated-list-print t)
  (message "Fetching services...")
  (let ((buffer (current-buffer)))
    (ecloud-rpc-k8s-list-services-async
     (lambda (resp)
       (let ((services (plist-get resp :services)))
         (when (buffer-live-p buffer)
           (with-current-buffer buffer
             (setq tabulated-list-entries
                   (mapcar (lambda (s)
                             (list s
                                   (vector
                                    (propertize (or (plist-get s :name) "???") 'face 'ecloud-k8s-name-face)
                                    (propertize (or (plist-get s :namespace) "???") 'face 'ecloud-k8s-namespace-face)
                                    (or (plist-get s :type) "")
                                    (or (plist-get s :clusterIp) "")
                                    (or (plist-get s :externalIp) "")
                                    (or (plist-get s :age) ""))))
                           services))
             (tabulated-list-print)
             (message "Found %d services." (length services))))))
     ecloud-k8s--current-namespace
     (lambda (err) (message "Error: %s" err)))))

;;; Ingresses view

(defun ecloud-k8s--fetch-ingresses ()
  "Fetch and display ingresses."
  (setq ecloud-k8s--current-view 'ingresses)
  (setq tabulated-list-format
        [("Name" 35 t)
         ("Namespace" 20 t)
         ("Class" 15 t)
         ("Hosts" 30 nil)
         ("Address" 20 nil)
         ("Age" 6 t)])
  (setq tabulated-list-entries nil)
  (tabulated-list-init-header)
  (tabulated-list-print t)
  (message "Fetching ingresses...")
  (let ((buffer (current-buffer)))
    (ecloud-rpc-k8s-list-ingresses-async
     (lambda (resp)
       (let ((ingresses (plist-get resp :ingresses)))
         (when (buffer-live-p buffer)
           (with-current-buffer buffer
             (setq tabulated-list-entries
                   (mapcar (lambda (i)
                             (list i
                                   (vector
                                    (propertize (or (plist-get i :name) "???") 'face 'ecloud-k8s-name-face)
                                    (propertize (or (plist-get i :namespace) "???") 'face 'ecloud-k8s-namespace-face)
                                    (or (plist-get i :className) "")
                                    (string-join (or (plist-get i :hosts) '()) ",")
                                    (string-join (or (plist-get i :addresses) '()) ",")
                                    (or (plist-get i :age) ""))))
                           ingresses))
             (tabulated-list-print)
             (message "Found %d ingresses." (length ingresses))))))
     ecloud-k8s--current-namespace
     (lambda (err) (message "Error: %s" err)))))

;;; Deployments view

(defun ecloud-k8s--fetch-deployments ()
  "Fetch and display deployments."
  (setq ecloud-k8s--current-view 'deployments)
  (setq tabulated-list-format
        [("Name" 40 t)
         ("Namespace" 20 t)
         ("Ready" 10 t)
         ("Up-to-date" 10 t)
         ("Available" 10 t)
         ("Age" 6 t)])
  (setq tabulated-list-entries nil)
  (tabulated-list-init-header)
  (tabulated-list-print t)
  (message "Fetching deployments...")
  (let ((buffer (current-buffer)))
    (ecloud-rpc-k8s-list-deployments-async
     (lambda (resp)
       (let ((deployments (plist-get resp :deployments)))
         (when (buffer-live-p buffer)
           (with-current-buffer buffer
             (setq tabulated-list-entries
                   (mapcar (lambda (d)
                             (list d
                                   (vector
                                    (propertize (or (plist-get d :name) "???") 'face 'ecloud-k8s-name-face)
                                    (propertize (or (plist-get d :namespace) "???") 'face 'ecloud-k8s-namespace-face)
                                    (or (plist-get d :ready) "")
                                    (format "%d" (or (plist-get d :upToDate) 0))
                                    (format "%d" (or (plist-get d :available) 0))
                                    (or (plist-get d :age) ""))))
                           deployments))
             (tabulated-list-print)
             (message "Found %d deployments." (length deployments))))))
     ecloud-k8s--current-namespace
     (lambda (err) (message "Error: %s" err)))))

;;; Namespaces

(defun ecloud-k8s--fetch-namespaces ()
  "Fetch and display namespaces."
  (setq ecloud-k8s--current-view 'namespaces)
  (setq tabulated-list-format
        [("Name" 40 t)
         ("Status" 15 t)])
  (setq tabulated-list-entries nil)
  (tabulated-list-init-header)
  (tabulated-list-print t)
  (message "Fetching namespaces...")
  (let ((buffer (current-buffer)))
    (ecloud-rpc-k8s-list-namespaces-async
     (lambda (resp)
       (let ((namespaces (plist-get resp :namespaces)))
         (when (buffer-live-p buffer)
           (with-current-buffer buffer
             (setq tabulated-list-entries
                   (mapcar (lambda (ns)
                             (list ns
                                   (vector
                                    (propertize (or (plist-get ns :name) "???") 'face 'ecloud-k8s-name-face)
                                    (ecloud-k8s--format-status (or (plist-get ns :status) "")))))
                           namespaces))
             (tabulated-list-print)
             (message "Found %d namespaces." (length namespaces))))))
     (lambda (err) (message "Error: %s" err)))))

;;; Helm Releases

(defun ecloud-k8s--fetch-helm-releases ()
  "Fetch and display Helm releases.
Uses the current namespace filter if set, otherwise fetches from all namespaces."
  (let ((buffer (get-buffer-create "*ECloud-Helm-Releases*"))
        (namespace ecloud-k8s--current-namespace))
    (with-current-buffer buffer
      (ecloud-helm-list-mode)
      (setq ecloud-k8s--current-view 'helm-releases)
      (setq tabulated-list-entries nil)
      (tabulated-list-print t)
      ;; Show which namespace we're fetching from
      (if namespace
          (message "Fetching Helm releases from namespace '%s'..." namespace)
        (message "Fetching Helm releases from all namespaces (this may take a while)..."))
      (let ((buffer (current-buffer))
            (start-time (current-time))
            (progress-timer nil))
        ;; Show progress every 2 seconds for long operations
        (setq progress-timer
              (run-at-time 2 2
                          (lambda ()
                            (let ((elapsed (float-time (time-subtract (current-time) start-time))))
                              (if namespace
                                  (message "Still fetching Helm releases from '%s'... (%.1fs elapsed)"
                                          namespace elapsed)
                                (message "Still fetching Helm releases from all namespaces... (%.1fs elapsed)"
                                        elapsed))))))
        (ecloud-rpc-helm-list-releases-async
         (lambda (resp)
           ;; Cancel progress timer
           (when progress-timer
             (cancel-timer progress-timer))
           (let* ((releases (plist-get resp :releases))
                  (elapsed (float-time (time-subtract (current-time) start-time))))
             (when (buffer-live-p buffer)
               (with-current-buffer buffer
                 (setq tabulated-list-entries
                       (mapcar #'ecloud-k8s--helm-release-to-entry releases))
                 (tabulated-list-print)
                 (if namespace
                     (message "Found %d Helm releases in namespace '%s' (%.2fs)."
                             (length releases) namespace elapsed)
                   (message "Found %d Helm releases across all namespaces (%.2fs)."
                           (length releases) elapsed))))))
         namespace  ; Use current namespace filter
         nil        ; Don't fetch all namespaces if namespace is set
         ecloud-k8s-helm-fetch-details  ; Use customizable variable
         (lambda (err)
           ;; Cancel progress timer on error
           (when progress-timer
             (cancel-timer progress-timer))
           (ecloud-k8s--display-error err "Failed to fetch Helm releases"))))
    (switch-to-buffer buffer))))

(defun ecloud-k8s--helm-release-to-entry (release)
  "Convert RELEASE dict to tabulated-list entry."
  (list release
        (vector
         (propertize (or (plist-get release :name) "???") 'face 'ecloud-k8s-name-face)
         (propertize (or (plist-get release :namespace) "???") 'face 'ecloud-k8s-namespace-face)
         (or (plist-get release :chart) "...")
         (or (plist-get release :version) "...")
         (ecloud-k8s--format-status (or (plist-get release :status) "...")))))

(defun ecloud-k8s-helm-toggle-details ()
  "Toggle between fast mode (basic info) and full mode (detailed info) for Helm releases.
Fast mode only shows release name and namespace.
Full mode also shows chart, version, and status (slower)."
  (interactive)
  (setq ecloud-k8s-helm-fetch-details (not ecloud-k8s-helm-fetch-details))
  (message "Helm fetch details: %s (refresh to apply)" 
           (if ecloud-k8s-helm-fetch-details "enabled" "disabled"))
  (when (eq ecloud-k8s--current-view 'helm-releases)
    (ecloud-k8s-refresh)))

(defun ecloud-k8s-helm-list ()
  "List Helm releases in current cluster."
  (interactive)
  (unless ecloud-k8s--current-cluster
    (user-error "Not connected to a cluster. Connect first with ecloud-k8s-list"))
  (ecloud-k8s--fetch-helm-releases))

(defun ecloud-k8s-helm-describe ()
  "Show details of Helm release at point."
  (interactive)
  (let* ((entry (tabulated-list-get-id))
         (release-name (plist-get entry :name))
         (namespace (plist-get entry :namespace)))
    (unless release-name
      (user-error "No Helm release selected"))
    (message "Fetching details for %s/%s..." namespace release-name)
    (ecloud-rpc-helm-get-release-details-async
     release-name
     (lambda (resp)
       (let ((details (plist-get resp :release))
             (buffer (get-buffer-create (format "*Helm Release: %s*" release-name))))
         (with-current-buffer buffer
           (let ((inhibit-read-only t))
             (erase-buffer)
             (insert (format "=== Helm Release: %s ===\n\n" release-name))
             (insert (format "Namespace:    %s\n" (plist-get details :namespace)))
             (insert (format "Chart:        %s\n" (plist-get details :chart)))
             (insert (format "Version:      %s\n" (plist-get details :version)))
             (insert (format "App Version:  %s\n" (or (plist-get details :app_version) "N/A")))
             (insert (format "Status:       %s\n" (plist-get details :status)))
             (insert (format "Revision:     %d\n" (or (plist-get details :revision) 0)))
             (insert (format "Updated:      %s\n\n" (or (plist-get details :updated) "N/A")))
             
             ;; Values section
             (insert "=== Values ===\n\n")
             (let ((values (plist-get details :values)))
               (if values
                   (insert (pp-to-string values))
                 (insert "No custom values\n")))
             (insert "\n")
             
             ;; Revision history section
             (insert "=== Revision History ===\n\n")
             (let ((history (plist-get details :revisionHistory)))
               (if history
                   (dolist (rev history)
                     (insert (format "Revision %d: %s - %s\n"
                                     (plist-get rev :revision)
                                     (plist-get rev :status)
                                     (or (plist-get rev :description) ""))))
                 (insert "No revision history available\n")))
             
             ;; Notes section
             (when-let ((notes (plist-get details :notes)))
               (insert "\n=== Notes ===\n\n")
               (insert notes))
             
             (goto-char (point-min))
             (special-mode))
           (pop-to-buffer buffer))))
     namespace
     (lambda (err) (ecloud-k8s--display-error err "Failed to fetch release details")))))

(defun ecloud-k8s-helm-install ()
  "Install a new Helm chart."
  (interactive)
  (unless ecloud-k8s--current-cluster
    (user-error "Not connected to a cluster. Connect first with ecloud-k8s-list"))
  (let* ((release-name (read-string "Release name: "))
         (chart-ref (read-string "Chart (repo/chart or path): "))
         (namespace (read-string "Namespace: " (or ecloud-k8s--current-namespace "default")))
         (edit-values (y-or-n-p "Edit values? ")))
    (when (string-empty-p release-name)
      (user-error "Release name cannot be empty"))
    (when (string-empty-p chart-ref)
      (user-error "Chart reference cannot be empty"))
    (if edit-values
        (ecloud-k8s-helm--edit-values-and-install release-name chart-ref namespace)
      (ecloud-k8s-helm--install-with-defaults release-name chart-ref namespace))))

(defun ecloud-k8s-helm--install-with-defaults (release-name chart-ref namespace)
  "Install Helm chart with default values."
  (message "Installing %s as %s in namespace %s..." chart-ref release-name namespace)
  (ecloud-rpc-helm-install-chart-async
   release-name
   chart-ref
   (lambda (resp)
     (let ((release (plist-get resp :release)))
       (message "Successfully installed %s (status: %s)"
                release-name
                (plist-get release :status))
       (when (eq ecloud-k8s--current-view 'helm-releases)
         (ecloud-k8s-refresh))))
   namespace
   nil  ; values
   t    ; create-namespace
   t    ; wait
   300  ; timeout
   (lambda (err) (ecloud-k8s--display-error err "Failed to install chart"))))

(defun ecloud-k8s-helm--edit-values-and-install (release-name chart-ref namespace)
  "Open values editor and install chart with custom values."
  (let ((buffer (get-buffer-create (format "*Helm Values: %s*" release-name))))
    (with-current-buffer buffer
      (erase-buffer)
      (insert "# Edit values for " chart-ref "\n")
      (insert "# Save and close (C-c C-c) to install, or kill buffer (C-c C-k) to cancel\n\n")
      (yaml-mode)
      (local-set-key (kbd "C-c C-c")
                     (lambda ()
                       (interactive)
                       (let* ((content (buffer-substring-no-properties (point-min) (point-max)))
                              ;; Remove comment lines
                              (lines (split-string content "\n"))
                              (yaml-lines (seq-filter (lambda (line)
                                                        (not (string-prefix-p "#" (string-trim line))))
                                                      lines))
                              (yaml-str (string-join yaml-lines "\n")))
                         (kill-buffer)
                         (if (string-empty-p (string-trim yaml-str))
                             (ecloud-k8s-helm--install-with-defaults release-name chart-ref namespace)
                           ;; Parse YAML to dict (simplified - in real implementation would use yaml parser)
                           (message "Installing %s with custom values..." release-name)
                           (ecloud-rpc-helm-install-chart-async
                            release-name
                            chart-ref
                            (lambda (resp)
                              (let ((release (plist-get resp :release)))
                                (message "Successfully installed %s (status: %s)"
                                         release-name
                                         (plist-get release :status))
                                (when (eq ecloud-k8s--current-view 'helm-releases)
                                  (ecloud-k8s-refresh))))
                            namespace
                            (list :raw yaml-str)  ; Send raw YAML string
                            t    ; create-namespace
                            t    ; wait
                            300  ; timeout
                            (lambda (err) (ecloud-k8s--display-error err "Failed to install chart")))))))
      (local-set-key (kbd "C-c C-k")
                     (lambda ()
                       (interactive)
                       (when (y-or-n-p "Cancel installation? ")
                         (kill-buffer)
                         (message "Installation cancelled")))))
    (pop-to-buffer buffer)
    (message "Edit values and press C-c C-c to install, or C-c C-k to cancel")))

(defun ecloud-k8s-helm-upgrade ()
  "Upgrade Helm release at point."
  (interactive)
  (unless ecloud-k8s--current-cluster
    (user-error "Not connected to a cluster"))
  (let* ((entry (tabulated-list-get-id))
         (release-name (plist-get entry :name))
         (namespace (plist-get entry :namespace)))
    (unless release-name
      (user-error "No Helm release selected"))
    (let ((edit-values (y-or-n-p (format "Edit values for %s? " release-name))))
      (if edit-values
          (ecloud-k8s-helm--edit-values-and-upgrade release-name namespace)
        (ecloud-k8s-helm--upgrade-with-current-values release-name namespace)))))

(defun ecloud-k8s-helm--upgrade-with-current-values (release-name namespace)
  "Upgrade Helm release with current values."
  (message "Upgrading %s in namespace %s..." release-name namespace)
  (ecloud-rpc-helm-upgrade-release-async
   release-name
   (lambda (resp)
     (let ((release (plist-get resp :release)))
       (message "Successfully upgraded %s to revision %d"
                release-name
                (or (plist-get release :revision) 0))
       (when (eq ecloud-k8s--current-view 'helm-releases)
         (ecloud-k8s-refresh))))
   nil  ; chart-ref (use current)
   namespace
   nil  ; values (use current)
   nil  ; version
   t    ; wait
   300  ; timeout
   (lambda (err) (ecloud-k8s--display-error err "Failed to upgrade release"))))

(defun ecloud-k8s-helm--edit-values-and-upgrade (release-name namespace)
  "Open values editor and upgrade release with custom values."
  ;; First fetch current values
  (message "Fetching current values for %s..." release-name)
  (ecloud-rpc-helm-get-release-details-async
   release-name
   (lambda (resp)
     (let* ((details (plist-get resp :release))
            (current-values (plist-get details :values))
            (buffer (get-buffer-create (format "*Helm Values: %s*" release-name))))
       (with-current-buffer buffer
         (erase-buffer)
         (insert "# Edit values for " release-name "\n")
         (insert "# Save and close (C-c C-c) to upgrade, or kill buffer (C-c C-k) to cancel\n\n")
         (when current-values
           (insert (pp-to-string current-values)))
         (yaml-mode)
         (local-set-key (kbd "C-c C-c")
                        (lambda ()
                          (interactive)
                          (let* ((content (buffer-substring-no-properties (point-min) (point-max)))
                                 ;; Remove comment lines
                                 (lines (split-string content "\n"))
                                 (yaml-lines (seq-filter (lambda (line)
                                                           (not (string-prefix-p "#" (string-trim line))))
                                                         lines))
                                 (yaml-str (string-join yaml-lines "\n")))
                            (kill-buffer)
                            (message "Upgrading %s with custom values..." release-name)
                            (ecloud-rpc-helm-upgrade-release-async
                             release-name
                             (lambda (resp)
                               (let ((release (plist-get resp :release)))
                                 (message "Successfully upgraded %s to revision %d"
                                          release-name
                                          (or (plist-get release :revision) 0))
                                 (when (eq ecloud-k8s--current-view 'helm-releases)
                                   (ecloud-k8s-refresh))))
                             nil  ; chart-ref
                             namespace
                             (list :raw yaml-str)  ; Send raw YAML string
                             nil  ; version
                             t    ; wait
                             300  ; timeout
                             (lambda (err) (ecloud-k8s--display-error err "Failed to upgrade release"))))))
         (local-set-key (kbd "C-c C-k")
                        (lambda ()
                          (interactive)
                          (when (y-or-n-p "Cancel upgrade? ")
                            (kill-buffer)
                            (message "Upgrade cancelled")))))
       (pop-to-buffer buffer)
       (message "Edit values and press C-c C-c to upgrade, or C-c C-k to cancel")))
   namespace
   (lambda (err) (ecloud-k8s--display-error err "Failed to fetch current values"))))

(defun ecloud-k8s-helm-history ()
  "Show revision history for Helm release at point in an interactive list."
  (interactive)
  (unless ecloud-k8s--current-cluster
    (user-error "Not connected to a cluster"))
  (let* ((entry (tabulated-list-get-id))
         (release-name (plist-get entry :name))
         (namespace (plist-get entry :namespace)))
    (unless release-name
      (user-error "No Helm release selected"))
    (message "Fetching history for %s..." release-name)
    (ecloud-rpc-helm-get-release-details-async
     release-name
     (lambda (resp)
       (let* ((details (plist-get resp :release))
              (history (plist-get details :revisionHistory))
              (current-revision (plist-get details :revision))
              (buffer (get-buffer-create (format "*Helm History: %s*" release-name))))
         (with-current-buffer buffer
           (ecloud-helm-history-mode)
           ;; Store release info for rollback
           (setq-local ecloud-k8s--helm-release-name release-name)
           (setq-local ecloud-k8s--helm-namespace namespace)
           (setq-local ecloud-k8s--helm-current-revision current-revision)
           
           ;; Set up tabulated list
           (setq tabulated-list-format
                 [("Revision" 10 t)
                  ("Updated" 20 t)
                  ("Status" 15 t)
                  ("Chart" 30 t)
                  ("Description" 50 nil)])
           (setq tabulated-list-padding 2)
           (tabulated-list-init-header)
           
           ;; Populate entries (newest first)
           (setq tabulated-list-entries
                 (mapcar
                  (lambda (rev)
                    (let* ((rev-num (plist-get rev :revision))
                           (updated (plist-get rev :updated))
                           (status (plist-get rev :status))
                           (chart (plist-get rev :chart))
                           (version (plist-get rev :version))
                           (desc (or (plist-get rev :description) ""))
                           (is-current (= rev-num current-revision)))
                      (list rev
                            (vector
                             (if is-current
                                 (propertize (format "%d *" rev-num) 'face 'bold)
                               (format "%d" rev-num))
                             (if updated
                                 (format-time-string "%Y-%m-%d %H:%M:%S"
                                                     (date-to-time updated))
                               "N/A")
                             (propertize status
                                         'face (cond
                                                ((string= status "deployed") 'ecloud-k8s-running-face)
                                                ((string= status "failed") 'ecloud-k8s-error-face)
                                                (t 'default)))
                             (format "%s-%s" chart version)
                             desc))))
                  (reverse history)))  ; Newest first
           
           (tabulated-list-print t)
           (goto-char (point-min)))
         (pop-to-buffer buffer)
         (message "Press 'r' to rollback to selected revision, 'q' to quit")))
     namespace
     (lambda (err) (ecloud-k8s--display-error err "Failed to fetch release history")))))

(defun ecloud-k8s-helm-rollback ()
  "Rollback Helm release at point."
  (interactive)
  (unless ecloud-k8s--current-cluster
    (user-error "Not connected to a cluster"))
  (let* ((entry (tabulated-list-get-id))
         (release-name (plist-get entry :name))
         (namespace (plist-get entry :namespace)))
    (unless release-name
      (user-error "No Helm release selected"))
    (let ((revision (read-number "Rollback to revision: ")))
      (when (y-or-n-p (format "Rollback %s to revision %d? " release-name revision))
        (message "Rolling back %s to revision %d..." release-name revision)
        (ecloud-rpc-helm-rollback-release-async
         release-name
         revision
         (lambda (resp)
           (let ((release (plist-get resp :release)))
             (message "Successfully rolled back %s to revision %d"
                      release-name
                      revision)
             (when (eq ecloud-k8s--current-view 'helm-releases)
               (ecloud-k8s-refresh))))
         namespace
         t  ; wait
         (lambda (err) (ecloud-k8s--display-error err "Failed to rollback release")))))))

(defun ecloud-k8s-helm-uninstall ()
  "Uninstall Helm release at point."
  (interactive)
  (unless ecloud-k8s--current-cluster
    (user-error "Not connected to a cluster"))
  (let* ((entry (tabulated-list-get-id))
         (release-name (plist-get entry :name))
         (namespace (plist-get entry :namespace)))
    (unless release-name
      (user-error "No Helm release selected"))
    (when (y-or-n-p (format "Uninstall %s from namespace %s? " release-name namespace))
      (message "Uninstalling %s..." release-name)
      (ecloud-rpc-helm-uninstall-release-async
       release-name
       (lambda (_resp)
         (message "Successfully uninstalled %s" release-name)
         (when (eq ecloud-k8s--current-view 'helm-releases)
           (ecloud-k8s-refresh)))
       namespace
       t  ; wait
       (lambda (err) (ecloud-k8s--display-error err "Failed to uninstall release"))))))

;;; Actions

(defun ecloud-k8s-enter ()
  "Perform default action on the item at point."
  (interactive)
  (pcase ecloud-k8s--current-view
    ('clusters (ecloud-k8s-connect-cluster))
    ('pods (ecloud-k8s-view-logs))
    ('services (ecloud-k8s-view-yaml))
    ('ingresses (ecloud-k8s-view-yaml))
    ('deployments (ecloud-k8s-view-yaml))
    ('namespaces (ecloud-k8s-select-this-namespace))
    ('helm-releases (ecloud-k8s-helm-describe))
    (_ (message "No action defined for this view"))))

(defun ecloud-k8s-select-this-namespace ()
  "Select the namespace at point and switch to pods."
  (interactive)
  (let* ((entry (tabulated-list-get-id))
         (name (plist-get entry :name)))
    (when name
      (setq ecloud-k8s--current-namespace name)
      (message "Namespace filter: %s" name)
      (ecloud-k8s-switch-to-pods))))

(defun ecloud-k8s-view-yaml ()
  "View YAML for resource at point."
  (interactive)
  (let* ((entry (tabulated-list-get-id))
         (name (plist-get entry :name))
         (namespace (or (plist-get entry :namespace) "default"))
         (kind (pcase ecloud-k8s--current-view
                 ('pods "pod")
                 ('services "service")
                 ('ingresses "ingress")
                 ('deployments "deployment")
                 ('namespaces "namespace")
                 (_ nil))))
    (unless (and name kind) (user-error "Cannot view YAML for this resource"))
    (message "Fetching YAML for %s/%s..." kind name)
    (ecloud-rpc-k8s-get-yaml-async
     kind name namespace
     (lambda (resp)
       (let ((yaml (plist-get resp :yaml))
             (buffer (get-buffer-create (format "*K8s YAML: %s/%s*" kind name))))
         (with-current-buffer buffer
           (erase-buffer)
           (insert yaml)
           (yaml-mode)
           (goto-char (point-min))
           (setq buffer-read-only t))
         (pop-to-buffer buffer)))
     (lambda (err) (message "Error: %s" err)))))

(defun ecloud-k8s-view-logs ()
  "View logs for pod at point (non-streaming)."
  (interactive)
  (unless (eq ecloud-k8s--current-view 'pods)
    (user-error "Logs only available for pods"))
  (let* ((entry (tabulated-list-get-id))
         (name (plist-get entry :name))
         (namespace (plist-get entry :namespace))
         (containers (plist-get entry :containers))
         (container (if (> (length containers) 1)
                        (completing-read "Container: " containers nil t)
                      (car containers))))
    (message "Fetching logs for %s/%s..." name container)
    (ecloud-rpc-k8s-pod-logs-async
     name namespace
     (lambda (resp)
       (let ((logs (plist-get resp :logs))
             (buffer (get-buffer-create (format "*K8s Logs: %s*" name))))
         (with-current-buffer buffer
           (let ((inhibit-read-only t))
             (erase-buffer)
             (insert logs))
           (goto-char (point-max))
           (special-mode))
         (pop-to-buffer buffer)))
     container 200
     (lambda (err) (message "Error: %s" err)))))

(defun ecloud-k8s-stream-logs ()
  "Start streaming logs for pod at point."
  (interactive)
  (unless (eq ecloud-k8s--current-view 'pods)
    (user-error "Logs only available for pods"))
  (let* ((entry (tabulated-list-get-id))
         (name (plist-get entry :name))
         (namespace (plist-get entry :namespace))
         (containers (plist-get entry :containers))
         (container (if (> (length containers) 1)
                        (completing-read "Container: " containers nil t)
                      (car containers))))
    (message "Starting log stream for %s/%s..." name container)
    ;; Stop existing stream if any
    (when ecloud-k8s--log-stream-id
      (ecloud-rpc-k8s-stop-log-stream ecloud-k8s--log-stream-id))
    ;; Create log buffer
    (let ((buffer (get-buffer-create ecloud-k8s--log-buffer-name)))
      (with-current-buffer buffer
        (let ((inhibit-read-only t))
          (erase-buffer)
          (insert (format "=== Streaming logs for %s/%s (container: %s) ===\n\n" 
                          namespace name container)))
        (ecloud-k8s-logs-mode))
      (pop-to-buffer buffer))
    ;; Start stream (logs come via WebSocket, handled by ecloud-ws)
    (let ((resp (ecloud-rpc-k8s-start-log-stream namespace nil name container nil 50)))
      (setq ecloud-k8s--log-stream-id (plist-get resp :stream_id))
      (message "Log stream started: %s" ecloud-k8s--log-stream-id))))

(defun ecloud-k8s-stop-log-stream ()
  "Stop the current log stream."
  (interactive)
  (when ecloud-k8s--log-stream-id
    (ecloud-rpc-k8s-stop-log-stream ecloud-k8s--log-stream-id)
    (setq ecloud-k8s--log-stream-id nil)
    (message "Log stream stopped")))

(defun ecloud-k8s--on-log-event (data)
  "Handle incoming log event DATA."
  (let ((stream-id (plist-get data :stream_id))
        (pod (plist-get data :pod))
        (container (plist-get data :container))
        (line (plist-get data :line)))
    (when (and (equal stream-id ecloud-k8s--log-stream-id)
               (get-buffer ecloud-k8s--log-buffer-name))
      (with-current-buffer ecloud-k8s--log-buffer-name
        (let ((inhibit-read-only t))
          (goto-char (point-max))
          (insert (format "[%s/%s] %s\n" pod container line)))))))

;;; Navigation

(defun ecloud-k8s-set-pod-limit (limit)
  "Set the pod fetch limit to LIMIT.
When LIMIT is 0, fetch all pods (no limit).
When LIMIT is positive, fetch up to that many pods."
  (interactive "nPod fetch limit (0 for no limit): ")
  (setq ecloud-k8s-pod-fetch-limit limit)
  (message "Pod fetch limit set to %s" 
           (if (zerop limit) "unlimited" (format "%d" limit)))
  ;; Refresh if we're currently viewing pods
  (when (eq ecloud-k8s--current-view 'pods)
    (ecloud-k8s--fetch-pods)))

(defun ecloud-k8s-toggle-pod-limit ()
  "Toggle between limited (500) and unlimited (0) pod fetching."
  (interactive)
  (setq ecloud-k8s-pod-fetch-limit
        (if (zerop ecloud-k8s-pod-fetch-limit) 500 0))
  (message "Pod fetch limit: %s"
           (if (zerop ecloud-k8s-pod-fetch-limit) "unlimited" 
             (format "%d" ecloud-k8s-pod-fetch-limit)))
  ;; Refresh if we're currently viewing pods
  (when (eq ecloud-k8s--current-view 'pods)
    (ecloud-k8s--fetch-pods)))

(defun ecloud-k8s-switch-to-pods ()
  "Switch to pods view."
  (interactive)
  (ecloud-k8s--fetch-pods))

(defun ecloud-k8s-switch-to-services ()
  "Switch to services view."
  (interactive)
  (ecloud-k8s--fetch-services))

(defun ecloud-k8s-switch-to-ingresses ()
  "Switch to ingresses view."
  (interactive)
  (ecloud-k8s--fetch-ingresses))

(defun ecloud-k8s-switch-to-deployments ()
  "Switch to deployments view."
  (interactive)
  (ecloud-k8s--fetch-deployments))

(defun ecloud-k8s-switch-to-namespaces ()
  "Switch to namespaces view."
  (interactive)
  (ecloud-k8s--fetch-namespaces))

(defun ecloud-k8s-select-namespace ()
  "Select a namespace to filter by."
  (interactive)
  (let* ((resp (ecloud-rpc-k8s-list-namespaces))
         (namespaces (mapcar (lambda (ns) (plist-get ns :name))
                             (plist-get resp :namespaces)))
         (choice (completing-read "Namespace (empty for all): " 
                                  (cons "" namespaces) nil nil)))
    (setq ecloud-k8s--current-namespace (if (string= choice "") nil choice))
    (message "Namespace filter: %s" (or ecloud-k8s--current-namespace "all"))
    ;; Refresh current view
    (ecloud-k8s-refresh)))

(defun ecloud-k8s-refresh ()
  "Refresh current view."
  (interactive)
  (pcase ecloud-k8s--current-view
    ('clusters (ecloud-k8s--fetch-clusters))
    ('pods (ecloud-k8s--fetch-pods))
    ('services (ecloud-k8s--fetch-services))
    ('ingresses (ecloud-k8s--fetch-ingresses))
    ('deployments (ecloud-k8s--fetch-deployments))
    ('namespaces (ecloud-k8s--fetch-namespaces))
    ('helm-releases (ecloud-k8s--fetch-helm-releases))))

(defun ecloud-k8s-disconnect ()
  "Disconnect from cluster and return to cluster list."
  (interactive)
  (ecloud-k8s-stop-log-stream)
  (ecloud-rpc-k8s-disconnect)
  (setq ecloud-k8s--current-cluster nil)
  (setq ecloud-k8s--current-namespace nil)
  (ecloud-k8s--fetch-clusters)
  (message "Disconnected"))

(defun ecloud-k8s-scale-deployment ()
  "Scale the deployment at point."
  (interactive)
  (unless (eq ecloud-k8s--current-view 'deployments)
    (user-error "Scaling only available for deployments"))
  (let* ((entry (tabulated-list-get-id))
         (name (plist-get entry :name))
         (namespace (plist-get entry :namespace)))
    (when (and name namespace)
      (let ((replicas (read-number "Replicas: ")))
        (ecloud-notify (format "Scaling %s/%s to %d..." namespace name replicas))
        (ecloud-k8s-scale-deployment-async
         namespace name replicas
         (lambda (_resp)
           (ecloud-notify (format "Scale request sent."))
           (ecloud-k8s-refresh))
         (lambda (err) (ecloud-notify (format "Failed to scale: %s" err) 5)))))))

(defun ecloud-k8s-pod-exec ()
  "Execute command in pod."
  (interactive)
  (unless (eq ecloud-k8s--current-view 'pods)
    (user-error "Exec only available for pods"))
  (let* ((entry (tabulated-list-get-id))
         (name (plist-get entry :name))
         (namespace (plist-get entry :namespace))
         (containers (plist-get entry :containers))
         (container (if (> (length containers) 1)
                        (completing-read "Container: " containers nil t)
                      (car containers)))
         (cmd-str (read-string "Command: " "/bin/sh -c 'ls -la'")))
    
    (ecloud-notify (format "Executing in %s/%s..." name container))
    ;; Exec is synchronous currently as it returns output directly
    (let ((output (ecloud-rpc-k8s-pod-exec namespace name (split-string cmd-str) container)))
      (if (string-empty-p output)
          (ecloud-notify "Command executed (no output)")
        (with-current-buffer (get-buffer-create "*ECloud-K8s-Exec*")
          (erase-buffer)
          (insert output)
          (pop-to-buffer (current-buffer)))))))

;; --- Interactive Shell (Simple Buffer Mode) ---

;; Cleanup function for exec sessions
(defun ecloud-k8s--cleanup-exec-session ()
  "Clean up exec session when buffer is killed."
  (when (and (bound-and-true-p ecloud-k8s--exec-session-id)
             (not (string= ecloud-k8s--exec-session-id "pending")))
    (condition-case err
        (ecloud-rpc-k8s-pod-exec-stop-session ecloud-k8s--exec-session-id)
      (error (message "Error stopping exec session: %s" err)))))

;; Simple shell mode for K8s pod exec
(define-derived-mode ecloud-k8s-shell-mode fundamental-mode "K8s-Shell"
  "Major mode for K8s pod interactive shell."
  (setq buffer-read-only nil)
  (use-local-map (make-sparse-keymap))
  (local-set-key (kbd "RET") #'ecloud-k8s--shell-send-input)
  (local-set-key (kbd "C-c C-c") #'ecloud-k8s--shell-send-input)
  (add-hook 'kill-buffer-hook #'ecloud-k8s--cleanup-exec-session nil t))

(defun ecloud-k8s-pod-exec-vterm ()
  "Execute interactive shell in pod."
  (interactive)
  (unless (eq ecloud-k8s--current-view 'pods)
    (user-error "Exec only available for pods"))
  
  (let* ((entry (tabulated-list-get-id))
         (name (plist-get entry :name))
         (namespace (plist-get entry :namespace))
         (containers (plist-get entry :containers))
         (container (if (> (length containers) 1)
                        (completing-read "Container: " containers nil t)
                      (car containers)))
         (buffer-name (format "*k8s-shell-%s/%s*" namespace name)))
    
    (ecloud-notify (format "Starting interactive shell in %s/%s..." name container))
    
    ;; Kill existing buffer if any
    (when (get-buffer buffer-name)
      (kill-buffer buffer-name))
    
    ;; Create shell buffer
    (let ((buf (get-buffer-create buffer-name)))
      (with-current-buffer buf
        (ecloud-k8s-shell-mode)
        (setq-local ecloud-k8s--exec-session-id "pending")
        (setq-local ecloud-k8s--input-start-marker (point-max-marker))
        (insert (format "Connecting to %s/%s...\n" namespace name)))
      
      ;; Display buffer
      (switch-to-buffer buf)
      (goto-char (point-max))
      
      ;; Start exec session
      (let ((resp (ecloud-rpc-k8s-pod-exec-interactive namespace name container)))
        (let ((session-id (plist-get resp :session_id)))
          (unless session-id
            (with-current-buffer buf
              (insert "\nError: Failed to start exec session\n"))
            (user-error "Failed to start exec session"))
          
          (if (not (buffer-live-p buf))
              (user-error "Buffer was deleted unexpectedly")
            
            ;; Update session ID
            (with-current-buffer buf
              (setq-local ecloud-k8s--exec-session-id session-id)
              (insert (format "Connected! Session: %s\n" session-id))
              (set-marker ecloud-k8s--input-start-marker (point-max)))
            
            (ecloud-notify (format "Interactive shell started (session: %s)" session-id))))))))

(defun ecloud-k8s--on-exec-output (data)
  "Handle exec output from WebSocket."
  (let ((session-id (plist-get data :session_id))
        (output (plist-get data :output)))
    ;; Filter out carriage returns for cleaner display
    (setq output (replace-regexp-in-string "\r" "" output))
    (let ((found nil))
      (dolist (buf (buffer-list))
        (when (buffer-live-p buf)
          (with-current-buffer buf
            (when (and (eq major-mode 'ecloud-k8s-shell-mode)
                       (bound-and-true-p ecloud-k8s--exec-session-id)
                       (not (string= ecloud-k8s--exec-session-id "pending"))
                       (equal ecloud-k8s--exec-session-id session-id))
              (setq found t)
              (let ((at-end (= (point) (point-max))))
                ;; Insert at end
                (save-excursion
                  (goto-char (point-max))
                  (insert output))
                ;; Move point if we were at end
                (when at-end
                  (goto-char (point-max)))
                ;; Update input marker
                (set-marker ecloud-k8s--input-start-marker (point-max))))))))))

(defun ecloud-k8s--shell-send-input ()
  "Send input from shell buffer to exec session."
  (interactive)
  (when (and (eq major-mode 'ecloud-k8s-shell-mode)
             (bound-and-true-p ecloud-k8s--exec-session-id)
             (not (string= ecloud-k8s--exec-session-id "pending")))
    (let* ((input-start (marker-position ecloud-k8s--input-start-marker))
           (input (buffer-substring-no-properties input-start (point-max))))
      (when (> (length input) 0)
        (ecloud-rpc-k8s-pod-exec-send-input ecloud-k8s--exec-session-id (concat input "\n"))
        ;; Clear input area
        (delete-region input-start (point-max))
        (set-marker ecloud-k8s--input-start-marker (point-max))))))

;; Hook for WebSocket exec output events
(defvar ecloud-k8s-exec-hook nil
  "Hook run when exec output is received via WebSocket.")

(add-hook 'ecloud-k8s-exec-hook #'ecloud-k8s--on-exec-output)


(defun ecloud-k8s-apply-manifest ()
  "Apply a Kubernetes manifest."
  (interactive)
  (let ((manifest (read-string "Manifest (YAML): "))) ;; Simple read for now, ideally from region/buffer
    (when (not (string-empty-p manifest))
      (ecloud-k8s-apply-manifest-async
       manifest nil
       (lambda (resp)
         (let ((results (plist-get resp :results)))
           (ecloud-notify (format "Applied: %s" results))
           (ecloud-k8s-refresh)))
       (lambda (err) (ecloud-notify (format "Failed to apply: %s" err) 5))))))

(defun ecloud-k8s-show-metrics ()
  "Show resource metrics."
  (interactive)
  (ecloud-notify "Fetching metrics...")
  (ecloud-rpc-k8s-resource-metrics-async
   (lambda (resp)
     (let ((nodes (plist-get resp :nodes))
           (pods (plist-get resp :pods))
           (buffer (get-buffer-create "*ECloud-K8s-Metrics*")))
       (with-current-buffer buffer
         (let ((inhibit-read-only t))
           (erase-buffer)
           (insert "=== Node Metrics ===\n\n")
           (dolist (n nodes)
             (let* ((metadata (plist-get n :metadata))
                    (usage (plist-get n :usage))
                    (cpu-pct (plist-get usage :cpu_percent))
                    (mem-pct (plist-get usage :memory_percent)))
               (insert (format "Node: %s\n" (plist-get metadata :name)))
               (insert (format "  CPU: %s (%s)\n" (plist-get usage :cpu) (or cpu-pct "?")))
               (insert (format "  Mem: %s (%s)\n\n" (plist-get usage :memory) (or mem-pct "?")))))
           (insert "\n=== Pod Metrics ===\n\n")
           (dolist (p pods)
             (let ((metadata (plist-get p :metadata))
                   (containers (plist-get p :containers)))
               (insert (format "Pod: %s/%s\n" 
                               (plist-get metadata :namespace)
                               (plist-get metadata :name)))
               (dolist (c containers)
                 (let ((usage (plist-get c :usage)))
                   (insert (format "  Container: %s\n    CPU: %s\n    Mem: %s\n"
                                   (plist-get c :name)
                                   (plist-get usage :cpu)
                                   (plist-get usage :memory)))))
               (insert "\n"))))
         (special-mode)
         (pop-to-buffer buffer))))
   (lambda (err) (ecloud-notify (format "Failed to fetch metrics: %s" err) 5))))

(defun ecloud-k8s-view-kind ()
  "View resources of a specific Kubernetes kind with completion.
Fetches available kinds dynamically from the Kubernetes API.
Shows annotations with API version, namespace scope, and kind."
  (interactive)
  (unless ecloud-k8s--current-cluster
    (user-error "Not connected to a cluster"))
  
  ;; Fetch API resources dynamically
  (ecloud-notify "Fetching available resource kinds...")
  (ecloud-rpc-k8s-list-api-resources-async
   (lambda (resp)
     (let* ((api-resources (plist-get resp :resources))
            ;; Create completion table with annotations
            (completion-table
             (lambda (string pred action)
               (if (eq action 'metadata)
                   ;; Return metadata for completion annotations
                   `(metadata
                     (annotation-function . ecloud-k8s--annotate-resource)
                     (category . kubernetes-resource))
                 ;; Standard completion
                 (complete-with-action action
                                       (mapcar (lambda (r) (plist-get r :name)) api-resources)
                                       string pred))))
            ;; Store resources in buffer-local variable for annotation function
            (kind nil))
       
       ;; Store api-resources for annotation function
       (setq ecloud-k8s--api-resources-cache api-resources)
       
       ;; Prompt user to select a kind with annotations
       (setq kind (completing-read "Kubernetes kind: " completion-table nil t))
       
       (when (and kind (not (string-empty-p kind)))
         (ecloud-notify (format "Fetching %s..." kind))
         (ecloud-rpc-k8s-get-resources-async
          kind
          (lambda (resp)
            (let ((resources (plist-get resp :resources))
                  (buffer (get-buffer-create (format "*ECloud-K8s-%s*" (capitalize kind)))))
              (with-current-buffer buffer
                (ecloud-k8s-mode)
                (setq ecloud-k8s--current-view 'custom-kind)
                (setq-local ecloud-k8s--custom-kind kind)
                
                ;; Set up tabulated list format
                (setq tabulated-list-format
                      [("Name" 40 t)
                       ("Namespace" 20 t)
                       ("Age" 15 t)
                       ("Status" 20 nil)])
                (setq tabulated-list-padding 2)
                (tabulated-list-init-header)
                
                ;; Populate entries
                (setq tabulated-list-entries
                      (mapcar
                       (lambda (resource)
                         (let* ((metadata (plist-get resource :metadata))
                                (name (plist-get metadata :name))
                                (namespace (or (plist-get metadata :namespace) "-"))
                                (creation-time (plist-get metadata :creationTimestamp))
                                (age (if creation-time
                                         (ecloud-k8s--format-age creation-time)
                                       "N/A"))
                                (status (or (plist-get resource :status_summary) "N/A")))
                           (list resource
                                 (vector
                                  (propertize name 'face 'ecloud-k8s-name-face)
                                  (propertize namespace 'face 'ecloud-k8s-namespace-face)
                                  age
                                  status))))
                       resources))
                
                (tabulated-list-print t)
                (goto-char (point-min)))
              (switch-to-buffer buffer)
              (ecloud-notify (format "Found %d %s" (length resources) kind))))
          ecloud-k8s--current-namespace
          (not ecloud-k8s--current-namespace)
          (lambda (err) (ecloud-k8s--display-error err (format "Failed to fetch %s" kind)))))))
   (lambda (err) (ecloud-k8s--display-error err "Failed to fetch API resources"))))

(defvar ecloud-k8s--api-resources-cache nil
  "Cache of API resources for annotation function.")

(defun ecloud-k8s--annotate-resource (candidate)
  "Annotate CANDIDATE with API version, namespace scope, and kind.
Similar to kubectl api-resources output."
  (when-let* ((resources ecloud-k8s--api-resources-cache)
              (resource (seq-find (lambda (r) (string= (plist-get r :name) candidate))
                                  resources)))
    (let* ((api-group (plist-get resource :apiGroup))
           (api-version (plist-get resource :apiVersion))
           (namespaced (plist-get resource :namespaced))
           (kind (plist-get resource :kind))
           ;; Format API version with group
           (full-api (if (and api-group (not (string-empty-p api-group)))
                         (format "%s/%s" api-group api-version)
                       api-version))
           ;; Namespace scope
           (scope (if namespaced "ns" "cluster"))
           ;; Use shorter labels and better spacing
           ;; Format: " API-VERSION (scope) Kind"
           (annotation (format " %s (%s) %s" full-api scope kind)))
      annotation)))

(defun ecloud-k8s-help ()
  "Show help for ecloud-k8s-mode."
  (interactive)
  (message "K8s: [RET]Action [p]Pods [s]Services [i]Ingresses [d]Deploys [h]Helm [n]Namespaces [K]Kind [N]Filter [=]SetLimit [T]ToggleLimit [y]YAML [l]Logs [L]Stream [S]Scale [e]Exec [A]Apply [M]Metrics [r]Refresh [Q]Disconnect [?]Help"))

(defun ecloud-k8s-helm-help ()
  "Show help for ecloud-helm-list-mode."
  (interactive)
  (message "Helm: [RET]Describe [i]Install [u]Upgrade [r]Rollback [h]History [d]Uninstall [g]Refresh [N]Filter [p]Pods [s]Services [n]Namespaces [Q]Disconnect [?]Help"))

;;; Mode definitions

(defvar ecloud-k8s-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "RET") #'ecloud-k8s-enter)
    (define-key map (kbd "p") #'ecloud-k8s-switch-to-pods)
    (define-key map (kbd "s") #'ecloud-k8s-switch-to-services)
    (define-key map (kbd "i") #'ecloud-k8s-switch-to-ingresses)
    (define-key map (kbd "d") #'ecloud-k8s-switch-to-deployments)
    (define-key map (kbd "h") #'ecloud-k8s-helm-list)
    (define-key map (kbd "n") #'ecloud-k8s-switch-to-namespaces)
    (define-key map (kbd "K") #'ecloud-k8s-view-kind)
    (define-key map (kbd "N") #'ecloud-k8s-select-namespace)
    (define-key map (kbd "=") #'ecloud-k8s-set-pod-limit)
    (define-key map (kbd "T") #'ecloud-k8s-toggle-pod-limit)
    (define-key map (kbd "y") #'ecloud-k8s-view-yaml)
    (define-key map (kbd "l") #'ecloud-k8s-view-logs)
    (define-key map (kbd "L") #'ecloud-k8s-stream-logs)
    (define-key map (kbd "S") #'ecloud-k8s-scale-deployment)
    (define-key map (kbd "e") #'ecloud-k8s-pod-exec)
    (define-key map (kbd "A") #'ecloud-k8s-apply-manifest)
    (define-key map (kbd "M") #'ecloud-k8s-show-metrics)
    (define-key map (kbd "r") #'ecloud-k8s-refresh)
    (define-key map (kbd "Q") #'ecloud-k8s-disconnect)
    (define-key map (kbd "?") #'ecloud-k8s-help)
    (define-key map (kbd "q") #'quit-window)
    map)
  "Keymap for `ecloud-k8s-mode'.")

(define-derived-mode ecloud-k8s-mode tabulated-list-mode "ECloud-K8s"
  "Major mode for browsing Kubernetes resources.
\\{ecloud-k8s-mode-map}"
  (setq tabulated-list-padding 2)
  (add-hook 'tabulated-list-revert-hook #'ecloud-k8s-refresh nil t)
  (when (fboundp 'evil-motion-state)
    (evil-motion-state)))

;;; Evil mode support

(with-eval-after-load 'evil
  (evil-set-initial-state 'ecloud-k8s-mode 'motion)
  (evil-define-key 'motion ecloud-k8s-mode-map
    (kbd "RET") #'ecloud-k8s-enter
    (kbd "p") #'ecloud-k8s-switch-to-pods
    (kbd "s") #'ecloud-k8s-switch-to-services
    (kbd "i") #'ecloud-k8s-switch-to-ingresses
    (kbd "d") #'ecloud-k8s-switch-to-deployments
    (kbd "h") #'ecloud-k8s-helm-list
    (kbd "n") #'ecloud-k8s-switch-to-namespaces
    (kbd "K") #'ecloud-k8s-view-kind
    (kbd "N") #'ecloud-k8s-select-namespace
    (kbd "=") #'ecloud-k8s-set-pod-limit
    (kbd "T") #'ecloud-k8s-toggle-pod-limit
    (kbd "y") #'ecloud-k8s-view-yaml
    (kbd "l") #'ecloud-k8s-view-logs
    (kbd "L") #'ecloud-k8s-stream-logs
    (kbd "S") #'ecloud-k8s-scale-deployment
    (kbd "e") #'ecloud-k8s-pod-exec
    (kbd "E") #'ecloud-k8s-pod-exec-vterm
    (kbd "A") #'ecloud-k8s-apply-manifest
    (kbd "M") #'ecloud-k8s-show-metrics
    (kbd "r") #'ecloud-k8s-refresh
    (kbd "Q") #'ecloud-k8s-disconnect
    (kbd "?") #'ecloud-k8s-help
    (kbd "q") #'quit-window))

;;; Logs mode

(defvar ecloud-k8s-logs-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "q") #'ecloud-k8s-stop-and-close-logs)
    (define-key map (kbd "c") #'ecloud-k8s-clear-logs)
    (define-key map (kbd "G") #'end-of-buffer)
    map)
  "Keymap for `ecloud-k8s-logs-mode'.")

(define-derived-mode ecloud-k8s-logs-mode special-mode "K8s-Logs"
  "Major mode for viewing Kubernetes logs.
\\{ecloud-k8s-logs-mode-map}")

(defun ecloud-k8s-stop-and-close-logs ()
  "Stop log stream and close buffer."
  (interactive)
  (ecloud-k8s-stop-log-stream)
  (quit-window t))

(defun ecloud-k8s-clear-logs ()
  "Clear the logs buffer."
  (interactive)
  (let ((inhibit-read-only t))
    (erase-buffer)
    (insert "=== Log buffer cleared ===\n\n")))

;;; Helm history mode

(defvar ecloud-helm-history-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "r") #'ecloud-k8s-helm-history-rollback)
    (define-key map (kbd "g") #'ecloud-k8s-helm-history-refresh)
    (define-key map (kbd "q") #'quit-window)
    (define-key map (kbd "?") #'ecloud-k8s-helm-history-help)
    map)
  "Keymap for `ecloud-helm-history-mode'.")

(define-derived-mode ecloud-helm-history-mode tabulated-list-mode "Helm-History"
  "Major mode for viewing Helm release revision history.
\\{ecloud-helm-history-mode-map}"
  (setq tabulated-list-padding 2)
  (when (fboundp 'evil-motion-state)
    (evil-motion-state)))

(defun ecloud-k8s-helm-history-help ()
  "Show help for helm history mode."
  (interactive)
  (message "Helm History: [r]Rollback [g]Refresh [?]Help [q]Quit"))

(defun ecloud-k8s-helm-history-rollback ()
  "Rollback to the revision at point."
  (interactive)
  (let* ((entry (tabulated-list-get-id))
         (revision (plist-get entry :revision))
         (release-name ecloud-k8s--helm-release-name)
         (namespace ecloud-k8s--helm-namespace)
         (current-revision ecloud-k8s--helm-current-revision))
    (unless revision
      (user-error "No revision selected"))
    (when (= revision current-revision)
      (user-error "Already at revision %d" revision))
    (when (y-or-n-p (format "Rollback %s to revision %d? " release-name revision))
      (message "Rolling back %s to revision %d..." release-name revision)
      (ecloud-rpc-helm-rollback-release-async
       release-name
       revision
       (lambda (resp)
         (message "Successfully rolled back %s to revision %d" release-name revision)
         ;; Refresh the history view
         (ecloud-k8s-helm-history-refresh))
       namespace
       t  ; wait
       (lambda (err) (ecloud-k8s--display-error err "Failed to rollback release"))))))

(defun ecloud-k8s-helm-history-refresh ()
  "Refresh the helm history view."
  (interactive)
  (let ((release-name ecloud-k8s--helm-release-name)
        (namespace ecloud-k8s--helm-namespace))
    (message "Refreshing history for %s..." release-name)
    (ecloud-rpc-helm-get-release-details-async
     release-name
     (lambda (resp)
       (let* ((details (plist-get resp :release))
              (history (plist-get details :revisionHistory))
              (current-revision (plist-get details :revision)))
         ;; Update local variables
         (setq-local ecloud-k8s--helm-current-revision current-revision)
         
         ;; Update entries
         (setq tabulated-list-entries
               (mapcar
                (lambda (rev)
                  (let* ((rev-num (plist-get rev :revision))
                         (updated (plist-get rev :updated))
                         (status (plist-get rev :status))
                         (chart (plist-get rev :chart))
                         (version (plist-get rev :version))
                         (desc (or (plist-get rev :description) ""))
                         (is-current (= rev-num current-revision)))
                    (list rev
                          (vector
                           (if is-current
                               (propertize (format "%d *" rev-num) 'face 'bold)
                             (format "%d" rev-num))
                           (if updated
                               (format-time-string "%Y-%m-%d %H:%M:%S"
                                                   (date-to-time updated))
                             "N/A")
                           (propertize status
                                       'face (cond
                                              ((string= status "deployed") 'ecloud-k8s-running-face)
                                              ((string= status "failed") 'ecloud-k8s-error-face)
                                              (t 'default)))
                           (format "%s-%s" chart version)
                           desc))))
                (reverse history)))
         
         (tabulated-list-print t)
         (message "History refreshed")))
     namespace
     (lambda (err) (ecloud-k8s--display-error err "Failed to refresh history")))))

;;; Evil mode support for Helm history mode

(with-eval-after-load 'evil
  (evil-set-initial-state 'ecloud-helm-history-mode 'motion)
  (evil-define-key 'motion ecloud-helm-history-mode-map
    (kbd "r") #'ecloud-k8s-helm-history-rollback
    (kbd "g") #'ecloud-k8s-helm-history-refresh
    (kbd "?") #'ecloud-k8s-helm-history-help
    (kbd "q") #'quit-window))

;;; Helm list mode

(defvar ecloud-helm-list-mode-map
  (let ((map (make-sparse-keymap)))
    (set-keymap-parent map ecloud-k8s-mode-map)
    ;; Helm-specific keybindings
    (define-key map (kbd "RET") #'ecloud-k8s-helm-describe)
    (define-key map (kbd "i") #'ecloud-k8s-helm-install)
    (define-key map (kbd "u") #'ecloud-k8s-helm-upgrade)
    (define-key map (kbd "r") #'ecloud-k8s-helm-rollback)
    (define-key map (kbd "h") #'ecloud-k8s-helm-history)
    (define-key map (kbd "d") #'ecloud-k8s-helm-uninstall)
    (define-key map (kbd "g") #'ecloud-k8s-refresh)
    (define-key map (kbd "?") #'ecloud-k8s-helm-help)
    map)
  "Keymap for `ecloud-helm-list-mode'.")

(define-derived-mode ecloud-helm-list-mode tabulated-list-mode "ECloud-Helm"
  "Major mode for listing Helm releases.
\\{ecloud-helm-list-mode-map}"
  (setq tabulated-list-format
        [("Name" 30 t)
         ("Namespace" 20 t)
         ("Chart" 30 t)
         ("Version" 15 t)
         ("Status" 15 t)])
  (setq tabulated-list-padding 2)
  (tabulated-list-init-header)
  (add-hook 'tabulated-list-revert-hook #'ecloud-k8s-refresh nil t)
  (when (fboundp 'evil-motion-state)
    (evil-motion-state)))

;;; Evil mode support for Helm list mode

(with-eval-after-load 'evil
  (evil-set-initial-state 'ecloud-helm-list-mode 'motion)
  (evil-define-key 'motion ecloud-helm-list-mode-map
    (kbd "RET") #'ecloud-k8s-helm-describe
    (kbd "i") #'ecloud-k8s-helm-install
    (kbd "u") #'ecloud-k8s-helm-upgrade
    (kbd "r") #'ecloud-k8s-helm-rollback
    (kbd "h") #'ecloud-k8s-helm-history
    (kbd "d") #'ecloud-k8s-helm-uninstall
    (kbd "g") #'ecloud-k8s-refresh
    (kbd "p") #'ecloud-k8s-switch-to-pods
    (kbd "s") #'ecloud-k8s-switch-to-services
    (kbd "n") #'ecloud-k8s-switch-to-namespaces
    (kbd "N") #'ecloud-k8s-select-namespace
    (kbd "y") #'ecloud-k8s-view-yaml
    (kbd "Q") #'ecloud-k8s-disconnect
    (kbd "?") #'ecloud-k8s-helm-help
    (kbd "q") #'quit-window))

;;; Entry point

;;;###autoload
(defun ecloud-k8s-list ()
  "Open the ECloud Kubernetes browser."
  (interactive)
  (let ((buffer (get-buffer-create "*ECloud-K8s*")))
    (with-current-buffer buffer
      (ecloud-k8s-mode)
      ;; Check connection status
      (let ((status (ecloud-rpc-k8s-connection-status)))
        (if (plist-get status :connected)
            (progn
              (setq ecloud-k8s--current-cluster (plist-get status :cluster))
              (ecloud-k8s--fetch-pods))
          (ecloud-k8s--fetch-clusters))))
    (switch-to-buffer buffer)))

(provide 'ecloud-k8s)
;;; ecloud-k8s.el ends here
