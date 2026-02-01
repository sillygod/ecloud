;;; ecloud-cloud-run.el --- Cloud Run browser for ecloud -*- lexical-binding: t; -*-

;;; Commentary:

;; A browser for Google Cloud Run services.
;; Supports viewing services, revisions, logs, and deployment.

;;; Code:

(require 'tabulated-list)
(require 'ecloud-rpc)
(require 'ecloud-notify)

;; Hook into WebSocket events
(defvar ecloud-cloud-run-event-hook nil
  "Hook for receiving Cloud Run events from WebSocket.")

(add-hook 'ecloud-cloud-run-event-hook #'ecloud-cloud-run--on-event)

;;; Customization

(defgroup ecloud-cloud-run nil
  "ECloud Cloud Run settings."
  :group 'ecloud)

(defcustom ecloud-cloud-run-default-region "us-central1"
  "Default region for Cloud Run operations."
  :type 'string
  :group 'ecloud-cloud-run)

(defcustom ecloud-cloud-run-show-all-regions nil
  "Whether to show services from all regions by default.
When nil, only shows services from the current region.
Note: With Asset API enabled, fetching all regions takes only 1-3 seconds.
Without Asset API, it falls back to parallel queries (10-30 seconds)."
  :type 'boolean
  :group 'ecloud-cloud-run)

(defface ecloud-cloud-run-name-face
  '((t :inherit font-lock-function-name-face :weight bold))
  "Face for service names."
  :group 'ecloud-cloud-run)

(defface ecloud-cloud-run-ready-face
  '((t :inherit font-lock-string-face))
  "Face for Ready status."
  :group 'ecloud-cloud-run)

(defface ecloud-cloud-run-failed-face
  '((t :inherit font-lock-warning-face :weight bold))
  "Face for Failed status."
  :group 'ecloud-cloud-run)

(defface ecloud-cloud-run-url-face
  '((t :inherit link))
  "Face for service URLs."
  :group 'ecloud-cloud-run)

;;; State

(defvar ecloud-cloud-run--current-region nil
  "Currently selected region. When nil and show-all-regions is t, shows all regions.")

(defvar ecloud-cloud-run--current-view 'services
  "Current view type: services, revisions, logs.")

(defvar ecloud-cloud-run--current-service nil
  "Currently selected service for detail views.")

;;; Helper functions

(defun ecloud-cloud-run--format-status (status)
  "Format STATUS with appropriate face."
  (cond
   ((string= status "Ready")
    (propertize status 'face 'ecloud-cloud-run-ready-face))
   ((string= status "Failed")
    (propertize status 'face 'ecloud-cloud-run-failed-face))
   (t status)))

(defun ecloud-cloud-run--format-url (url)
  "Format URL with link face."
  (if (and url (not (string-empty-p url)))
      (propertize url 'face 'ecloud-cloud-run-url-face)
    ""))

(defun ecloud-cloud-run--truncate-image (image)
  "Truncate IMAGE path for display."
  (if (> (length image) 50)
      (concat "..." (substring image -47))
    image))

;;; Data fetching

(defun ecloud-cloud-run--parse-services (services)
  "Parse SERVICES list into tabulated list entries."
  (mapcar
   (lambda (svc)
     (let* ((name (plist-get svc :name))
            (region (plist-get svc :region))
            (status (plist-get svc :status))
            (url (plist-get svc :url))
            (image (plist-get svc :image))
            (traffic (plist-get svc :traffic))
            (min-inst (plist-get svc :minInstances))
            (max-inst (plist-get svc :maxInstances)))
       (list svc
             (vector
              (propertize name 'face 'ecloud-cloud-run-name-face)
              region
              (ecloud-cloud-run--format-status status)
              (ecloud-cloud-run--format-url url)
              (ecloud-cloud-run--truncate-image image)
              traffic
              (format "%d-%d" min-inst max-inst)))))
   services))

(defun ecloud-cloud-run--fetch-services ()
  "Fetch Cloud Run services for current region or all regions."
  (let* ((show-all (and (null ecloud-cloud-run--current-region)
                       ecloud-cloud-run-show-all-regions))
         (region (or ecloud-cloud-run--current-region
                    ecloud-cloud-run-default-region))
         (params (if show-all
                    (list :all_regions t)
                  (list :region region))))
    (when show-all
      (ecloud-notify-info "Fetching services from all regions..."))
    (condition-case err
        (let* ((response (ecloud-rpc-request "cloud_run_list_services" params))
               (services (plist-get response :services)))
          (when show-all
            (ecloud-notify-info (format "Found %d services across all regions" (length services))))
          (ecloud-cloud-run--parse-services services))
      (error
       (ecloud-notify-error (format "Failed to fetch Cloud Run services: %s"
                                   (error-message-string err)))
       nil))))

;;; Browser mode

(defvar ecloud-cloud-run-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "RET") #'ecloud-cloud-run-view-service)
    (define-key map (kbd "r") #'ecloud-cloud-run-change-region)
    (define-key map (kbd "g") #'ecloud-cloud-run-refresh)
    (define-key map (kbd "l") #'ecloud-cloud-run-view-logs)
    (define-key map (kbd "d") #'ecloud-cloud-run-deploy)
    (define-key map (kbd "D") #'ecloud-cloud-run-delete-service)
    (define-key map (kbd "A") #'ecloud-cloud-run-toggle-all-regions)
    (define-key map (kbd "o") #'ecloud-cloud-run-open-url)
    (define-key map (kbd "q") #'quit-window)
    ;; Evil mode support
    (when (fboundp 'evil-define-key*)
      (evil-define-key* 'motion map
        (kbd "RET") #'ecloud-cloud-run-view-service
        (kbd "r") #'ecloud-cloud-run-change-region
        (kbd "g") #'ecloud-cloud-run-refresh
        (kbd "l") #'ecloud-cloud-run-view-logs
        (kbd "d") #'ecloud-cloud-run-deploy
        (kbd "D") #'ecloud-cloud-run-delete-service
        (kbd "A") #'ecloud-cloud-run-toggle-all-regions
        (kbd "o") #'ecloud-cloud-run-open-url
        (kbd "q") #'quit-window))
    map)
  "Keymap for `ecloud-cloud-run-mode'.")

(define-derived-mode ecloud-cloud-run-mode tabulated-list-mode "ECloud-CloudRun"
  "Major mode for browsing Google Cloud Run services.

\\{ecloud-cloud-run-mode-map}"
  (setq tabulated-list-format
        [("Name" 25 t)
         ("Region" 20 t)
         ("Status" 10 t)
         ("URL" 35 nil)
         ("Image" 40 nil)
         ("Traffic" 15 nil)
         ("Instances" 10 nil)])
  (setq tabulated-list-padding 2)
  (setq tabulated-list-sort-key (cons "Name" nil))
  (add-hook 'tabulated-list-revert-hook #'ecloud-cloud-run--refresh nil t)
  (tabulated-list-init-header))

(defun ecloud-cloud-run--refresh ()
  "Refresh the Cloud Run services list."
  (let ((entries (ecloud-cloud-run--fetch-services)))
    (setq tabulated-list-entries entries)
    ;; Update mode line to show current view
    (setq mode-name
          (if ecloud-cloud-run--current-region
              (format "ECloud-CloudRun[%s]" ecloud-cloud-run--current-region)
            "ECloud-CloudRun[All Regions]"))))

;;; Interactive commands

;;;###autoload
(defun ecloud-cloud-run-list (&optional region)
  "List Cloud Run services in REGION."
  (interactive)
  (when region
    (setq ecloud-cloud-run--current-region region))
  (let ((buffer (get-buffer-create "*ECloud-CloudRun*")))
    (with-current-buffer buffer
      (ecloud-cloud-run-mode)
      (ecloud-cloud-run--refresh)
      (tabulated-list-print))
    (switch-to-buffer buffer)))

(defun ecloud-cloud-run-refresh ()
  "Refresh the current Cloud Run view."
  (interactive)
  (ecloud-cloud-run--refresh)
  (tabulated-list-print)
  (ecloud-notify-info "Cloud Run services refreshed"))

(defun ecloud-cloud-run-change-region ()
  "Change the current Cloud Run region."
  (interactive)
  (let* ((response (ecloud-rpc-request "cloud_run_list_regions"))
         (regions (plist-get response :regions))
         (region (completing-read "Region: " regions nil t)))
    (setq ecloud-cloud-run--current-region region)
    (ecloud-cloud-run-refresh)))

(defun ecloud-cloud-run-toggle-all-regions ()
  "Toggle between showing all regions and current region only."
  (interactive)
  (if ecloud-cloud-run--current-region
      (progn
        (setq ecloud-cloud-run--current-region nil)
        (ecloud-notify-info "Fetching services from all regions...")
        (ecloud-cloud-run-refresh))
    (let* ((response (ecloud-rpc-request "cloud_run_list_regions"))
           (regions (plist-get response :regions))
           (region (completing-read "Select region: " regions nil t
                                   ecloud-cloud-run-default-region)))
      (setq ecloud-cloud-run--current-region region)
      (ecloud-notify-info (format "Showing services from %s only" region))
      (ecloud-cloud-run-refresh))))

(defun ecloud-cloud-run-view-service ()
  "View details of the Cloud Run service at point."
  (interactive)
  (let* ((entry (tabulated-list-get-entry))
         (service (tabulated-list-get-id))
         (name (plist-get service :name)))
    (if service
        (ecloud-cloud-run--show-service-details service)
      (ecloud-notify-error "No service at point"))))

(defun ecloud-cloud-run--show-service-details (service)
  "Show details for SERVICE in a dedicated buffer."
  (let* ((name (plist-get service :name))
         (buffer (get-buffer-create (format "*ECloud-CloudRun: %s*" name))))
    (with-current-buffer buffer
      (let ((inhibit-read-only t))
        (erase-buffer)
        (insert (propertize (format "Cloud Run Service: %s\n" name)
                           'face 'bold))
        (insert (make-string 60 ?=) "\n\n")
        
        (insert (propertize "General Information\n" 'face 'bold))
        (insert (format "  Name:           %s\n" (plist-get service :name)))
        (insert (format "  Region:         %s\n" (plist-get service :region)))
        (insert (format "  Status:         %s\n" (plist-get service :status)))
        (insert (format "  URL:            %s\n" (plist-get service :url)))
        (insert "\n")
        
        (insert (propertize "Container Configuration\n" 'face 'bold))
        (insert (format "  Image:          %s\n" (plist-get service :image)))
        (insert (format "  CPU:            %s\n" (plist-get service :cpu)))
        (insert (format "  Memory:         %s\n" (plist-get service :memory)))
        (insert "\n")
        
        (insert (propertize "Scaling Configuration\n" 'face 'bold))
        (insert (format "  Min Instances:  %d\n" (plist-get service :minInstances)))
        (insert (format "  Max Instances:  %d\n" (plist-get service :maxInstances)))
        (insert "\n")
        
        (insert (propertize "Traffic\n" 'face 'bold))
        (insert (format "  %s\n" (plist-get service :traffic)))
        (insert "\n")
        
        (insert (propertize "Timestamps\n" 'face 'bold))
        (insert (format "  Created:        %s\n" (plist-get service :created)))
        (insert (format "  Updated:        %s\n" (plist-get service :updated)))
        
        (goto-char (point-min))
        (view-mode)))
    (switch-to-buffer buffer)))

(defun ecloud-cloud-run-view-logs ()
  "View logs for the Cloud Run service at point."
  (interactive)
  (let* ((service (tabulated-list-get-id))
         (name (plist-get service :name))
         (region (or ecloud-cloud-run--current-region
                    ecloud-cloud-run-default-region)))
    (if service
        (ecloud-cloud-run--show-logs name region)
      (ecloud-notify-error "No service at point"))))

(defun ecloud-cloud-run--show-logs (service-name region)
  "Show logs for SERVICE-NAME in REGION."
  (let* ((limit (read-number "Number of log entries: " 100))
         (severity (completing-read "Severity (optional): "
                                   '("" "DEBUG" "INFO" "WARNING" "ERROR" "CRITICAL")
                                   nil t))
         (params (list :service_name service-name
                      :region region
                      :limit limit))
         (buffer (get-buffer-create (format "*ECloud-CloudRun-Logs: %s*" service-name))))
    
    (when (and severity (not (string-empty-p severity)))
      (setq params (plist-put params :severity severity)))
    
    (ecloud-notify-info (format "Fetching logs for %s..." service-name))
    
    (condition-case err
        (let* ((response (ecloud-rpc-request "cloud_run_get_logs" params))
               (logs (plist-get response :logs)))
          (with-current-buffer buffer
            (let ((inhibit-read-only t))
              (erase-buffer)
              (insert (propertize (format "Cloud Run Logs: %s\n" service-name)
                                 'face 'bold))
              (insert (format "Region: %s | Entries: %d\n" region (length logs)))
              (insert (make-string 80 ?=) "\n\n")
              
              (if (null logs)
                  (insert "No logs found.\n")
                (dolist (log logs)
                  (let ((timestamp (plist-get log :timestamp))
                        (severity (plist-get log :severity))
                        (message (plist-get log :message)))
                    (insert (format "[%s] %s: %s\n\n"
                                  timestamp
                                  (propertize severity 'face
                                            (cond
                                             ((string= severity "ERROR") 'error)
                                             ((string= severity "WARNING") 'warning)
                                             (t 'default)))
                                  message)))))
              
              (goto-char (point-min))
              (view-mode)))
          (switch-to-buffer buffer)
          (ecloud-notify-info "Logs loaded"))
      (error
       (ecloud-notify-error (format "Failed to fetch logs: %s"
                                   (error-message-string err)))))))

(defun ecloud-cloud-run-deploy ()
  "Deploy a new Cloud Run service or update existing one."
  (interactive)
  (let* ((name (read-string "Service name: "))
         (image (read-string "Container image: "))
         (region (or ecloud-cloud-run--current-region
                    (completing-read "Region: "
                                   (plist-get (ecloud-rpc-request "cloud_run_list_regions")
                                            :regions)
                                   nil t
                                   ecloud-cloud-run-default-region)))
         (port (read-number "Port: " 8080))
         (cpu (read-string "CPU (e.g., 1, 2): " "1"))
         (memory (read-string "Memory (e.g., 512Mi, 1Gi): " "512Mi"))
         (min-instances (read-number "Min instances: " 0))
         (max-instances (read-number "Max instances: " 100))
         (allow-unauth (y-or-n-p "Allow unauthenticated access? "))
         (params (list :name name
                      :image image
                      :region region
                      :port port
                      :cpu cpu
                      :memory memory
                      :min_instances min-instances
                      :max_instances max-instances
                      :allow_unauthenticated allow-unauth)))
    
    (ecloud-notify-info (format "Deploying service %s..." name))
    
    (ecloud-rpc-request-async
     "cloud_run_deploy_service"
     (lambda (response)
       (ecloud-notify-info (format "Service %s deployed successfully!" name))
       (ecloud-cloud-run-refresh))
     params
     (lambda (err)
       (ecloud-notify-error (format "Deployment failed: %s" err))))))

(defun ecloud-cloud-run-delete-service ()
  "Delete the Cloud Run service at point."
  (interactive)
  (let* ((service (tabulated-list-get-id))
         (name (plist-get service :name))
         (region (or ecloud-cloud-run--current-region
                    ecloud-cloud-run-default-region)))
    (if service
        (when (yes-or-no-p (format "Delete Cloud Run service '%s'? " name))
          (ecloud-notify-info (format "Deleting service %s..." name))
          (ecloud-rpc-request-async
           "cloud_run_delete_service"
           (lambda (_response)
             (ecloud-notify-info (format "Service %s deleted" name))
             (ecloud-cloud-run-refresh))
           (list :name name :region region)
           (lambda (err)
             (ecloud-notify-error (format "Delete failed: %s" err)))))
      (ecloud-notify-error "No service at point"))))

(defun ecloud-cloud-run-open-url ()
  "Open the URL of the Cloud Run service at point in browser."
  (interactive)
  (let* ((service (tabulated-list-get-id))
         (url (plist-get service :url)))
    (if (and url (not (string-empty-p url)))
        (browse-url url)
      (ecloud-notify-error "No URL available for this service"))))

;;; WebSocket event handlers

(defun ecloud-cloud-run--on-event (type data)
  "Handle Cloud Run WebSocket event of TYPE with DATA."
  (cond
   ((string= type "cloud_run_service_deployed")
    (let ((name (plist-get data :name)))
      (ecloud-notify-info (format "Cloud Run service '%s' deployed" name))
      (ecloud-cloud-run-refresh)))
   
   ((string= type "cloud_run_service_deleted")
    (let ((name (plist-get data :name)))
      (ecloud-notify-info (format "Cloud Run service '%s' deleted" name))
      (ecloud-cloud-run-refresh)))))

(provide 'ecloud-cloud-run)

;;; ecloud-cloud-run.el ends here
