;;; ecloud-k8s.el --- Kubernetes browser for ecloud -*- lexical-binding: t; -*-

;;; Commentary:

;; A browser for GKE clusters and Kubernetes resources.
;; Supports viewing pods, services, ingresses, deployments,
;; viewing YAML, and streaming logs from multiple pods.

;;; Code:

(require 'tabulated-list)
(require 'ecloud-rpc)

;; Hook into WebSocket events for log streaming
(defvar ecloud-k8s-log-hook nil
  "Hook for receiving K8s log events from WebSocket.")

(add-hook 'ecloud-k8s-log-hook #'ecloud-k8s--on-log-event)

;;; Customization

(defgroup ecloud-k8s nil
  "ECloud Kubernetes settings."
  :group 'ecloud)

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
  "Current view type: clusters, namespaces, pods, services, ingresses, deployments.")

(defvar ecloud-k8s--current-namespace nil
  "Current namespace filter. nil means all namespaces.")

(defvar ecloud-k8s--current-cluster nil
  "Currently connected cluster info.")

(defvar ecloud-k8s--log-stream-id nil
  "Active log stream ID.")

(defvar ecloud-k8s--log-buffer-name "*ECloud-K8s-Logs*"
  "Name of the log streaming buffer.")

;;; Helper functions

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
    (message "Connecting to %s..." name)
    (ecloud-rpc-k8s-connect-async
     name location
     (lambda (resp)
       (when (buffer-live-p buffer)
         (with-current-buffer buffer
           (setq ecloud-k8s--current-cluster (plist-get resp :cluster))
           (message "Connected to %s" name)
           (ecloud-k8s--fetch-pods))))
     (lambda (err) (message "Failed to connect: %s" err)))))

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
  (message "Fetching pods...")
  (let ((buffer (current-buffer)))
    (ecloud-rpc-k8s-list-pods-async
     (lambda (resp)
       (let ((pods (plist-get resp :pods)))
         (when (buffer-live-p buffer)
           (with-current-buffer buffer
             (setq tabulated-list-entries
                   (mapcar (lambda (p)
                             (list p
                                   (vector
                                    (propertize (or (plist-get p :name) "???") 'face 'ecloud-k8s-name-face)
                                    (propertize (or (plist-get p :namespace) "???") 'face 'ecloud-k8s-namespace-face)
                                    (or (plist-get p :ready) "")
                                    (ecloud-k8s--format-status (or (plist-get p :status) ""))
                                    (format "%d" (or (plist-get p :restarts) 0))
                                    (or (plist-get p :age) "")
                                    (or (plist-get p :ip) ""))))
                           pods))
             (tabulated-list-print)
             (message "Found %d pods." (length pods))))))
     ecloud-k8s--current-namespace nil
     (lambda (err) (message "Error: %s" err)))))

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
    ('namespaces (ecloud-k8s--fetch-namespaces))))

(defun ecloud-k8s-disconnect ()
  "Disconnect from cluster and return to cluster list."
  (interactive)
  (ecloud-k8s-stop-log-stream)
  (ecloud-rpc-k8s-disconnect)
  (setq ecloud-k8s--current-cluster nil)
  (setq ecloud-k8s--current-namespace nil)
  (ecloud-k8s--fetch-clusters)
  (message "Disconnected"))

(defun ecloud-k8s-help ()
  "Show help for ecloud-k8s-mode."
  (interactive)
  (message "K8s: [RET]Action [p]Pods [s]Services [i]Ingresses [d]Deploys [n]Namespaces [N]Filter [y]YAML [l]Logs [L]Stream [r]Refresh [Q]Disconnect [?]Help"))

;;; Mode definitions

(defvar ecloud-k8s-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "RET") #'ecloud-k8s-enter)
    (define-key map (kbd "p") #'ecloud-k8s-switch-to-pods)
    (define-key map (kbd "s") #'ecloud-k8s-switch-to-services)
    (define-key map (kbd "i") #'ecloud-k8s-switch-to-ingresses)
    (define-key map (kbd "d") #'ecloud-k8s-switch-to-deployments)
    (define-key map (kbd "n") #'ecloud-k8s-switch-to-namespaces)
    (define-key map (kbd "N") #'ecloud-k8s-select-namespace)
    (define-key map (kbd "y") #'ecloud-k8s-view-yaml)
    (define-key map (kbd "l") #'ecloud-k8s-view-logs)
    (define-key map (kbd "L") #'ecloud-k8s-stream-logs)
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
    (kbd "n") #'ecloud-k8s-switch-to-namespaces
    (kbd "N") #'ecloud-k8s-select-namespace
    (kbd "y") #'ecloud-k8s-view-yaml
    (kbd "l") #'ecloud-k8s-view-logs
    (kbd "L") #'ecloud-k8s-stream-logs
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
