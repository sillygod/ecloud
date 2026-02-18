;;; ecloud-compute.el --- Compute Engine browser for ecloud -*- lexical-binding: t; -*-

;; Copyright (C) 2024

;; Author: jing
;; Keywords: tools, cloud, compute, ssh

;;; Commentary:

;; A browser for Google Compute Engine VM instances.
;; Supports listing instances and SSHing into them via vterm.

;;; Code:

(require 'tabulated-list)
(require 'ecloud-rpc)
(require 'ecloud-notify)

;;; Customization

(defgroup ecloud-compute nil
  "ECloud Compute Engine settings."
  :group 'ecloud)

(defcustom ecloud-compute-impersonate-service-account nil
  "Service Account email to use for SSH connections.
If nil, checks the server configuration or defaults to user identity."
  :type '(choice (const :tag "None" nil)
                 (string :tag "Service Account Email"))
  :group 'ecloud-compute)

(defcustom ecloud-compute-auth-method 'account
  "Method to use when a Service Account is defined.
'impersonate (default): Uses --impersonate-service-account. Requires IAM Service Account Token Creator role.
'account: Uses --account. Requires the SA to be logged in via 'gcloud auth activate-service-account'."
  :type '(choice (const :tag "Impersonate (--impersonate-service-account)" impersonate)
                 (const :tag "Account (--account)" account))
  :group 'ecloud-compute)

(defface ecloud-compute-name-face
  '((t :inherit font-lock-function-name-face :weight bold))
  "Face for instance names."
  :group 'ecloud-compute)

(defface ecloud-compute-zone-face
  '((t :inherit font-lock-keyword-face))
  "Face for zones."
  :group 'ecloud-compute)

(defface ecloud-compute-status-running-face
  '((t :inherit font-lock-string-face))
  "Face for RUNNING status."
  :group 'ecloud-compute)

(defface ecloud-compute-status-stopped-face
  '((t :inherit font-lock-comment-face))
  "Face for TERMINATED/STOPPED status."
  :group 'ecloud-compute)

;;; Buffer-local state

(defvar-local ecloud-compute--server-config nil
  "Cached server configuration.")

;;; Helper functions

(defun ecloud-compute--format-status (status)
  "Format STATUS with appropriate face."
  (cond
   ((string= status "RUNNING")
    (propertize status 'face 'ecloud-compute-status-running-face))
   ((or (string= status "TERMINATED") (string= status "STOPPED"))
    (propertize status 'face 'ecloud-compute-status-stopped-face))
   (t status)))

(defun ecloud-compute--get-config ()
  "Fetch and cache server configuration."
  (unless ecloud-compute--server-config
    (condition-case nil
        (setq ecloud-compute--server-config (ecloud-rpc-get-config))
      (error nil)))
  ecloud-compute--server-config)

(defun ecloud-compute--get-impersonate-sa ()
  "Get the SA to impersonate, checking local custom var then server config."
  (or ecloud-compute-impersonate-service-account
      (plist-get (ecloud-compute--get-config) :impersonate_service_account)))

(defun ecloud-compute--get-project ()
  "Get the GCS project from server config."
  (plist-get (ecloud-compute--get-config) :gcs_project))

;;; Data fetching

(defun ecloud-compute--fetch-instances ()
  "Fetch and return instance list entries for tabulated-list."
  (let* ((response (ecloud-rpc-compute-list-instances))
         (instances (plist-get response :instances)))
    (mapcar
     (lambda (inst)
       (let ((name (plist-get inst :name))
             (zone (plist-get inst :zone))
             (status (plist-get inst :status))
             (int-ip (plist-get inst :internal_ip))
             (ext-ip (plist-get inst :external_ip))
             (type (plist-get inst :machine_type)))
         (list name  ; ID
               (vector
                (propertize name 'face 'ecloud-compute-name-face)
                (propertize zone 'face 'ecloud-compute-zone-face)
                (ecloud-compute--format-status status)
                (or int-ip "")
                (or ext-ip "")
                (or type "")))))
     instances)))

(defun ecloud-compute--refresh-data ()
  "Refresh the tabulated list data."
  (setq tabulated-list-format
        [("Name" 40 t)
         ("Zone" 15 t)
         ("Status" 12 t)
         ("Internal IP" 15 nil)
         ("External IP" 15 nil)
         ("Type" 15 nil)])
  (setq tabulated-list-entries #'ecloud-compute--fetch-instances)
  (tabulated-list-init-header)
  (tabulated-list-print t))

;;; SSH Implementation

(defun ecloud-compute-ssh ()
  "SSH into the instance at point using vterm."
  (interactive)
  (unless (require 'vterm nil t)
    (user-error "vterm is required for SSH functionality"))
  
  (let* ((instance-name (tabulated-list-get-id))
         (entry (tabulated-list-get-entry))
         (zone (substring-no-properties (aref entry 1))) ;; Zone is 2nd column
         (project (ecloud-compute--get-project))
         (impersonate-sa (ecloud-compute--get-impersonate-sa)))
    
    (unless instance-name
      (user-error "No instance selected"))
    
    (let ((cmd (concat "gcloud compute ssh " instance-name
                       (when project (concat " --project " project))
                       " --zone " zone
                       (when impersonate-sa
                         (if (eq ecloud-compute-auth-method 'account)
                             (concat " --account " impersonate-sa)
                           (concat " --impersonate-service-account " impersonate-sa))))))
      
      (message "Opening SSH connection to %s..." instance-name)
      
      ;; Create or switch to vterm buffer
      (let ((buffer-name (format "*vterm-ssh-%s*" instance-name)))
        (if (get-buffer buffer-name)
            (switch-to-buffer buffer-name)
          (vterm buffer-name))
        
        ;; Send command
        (vterm-send-string cmd)
        (vterm-send-return)))))

;;; Interactive commands

(defun ecloud-compute-refresh ()
  "Refresh the instance list."
  (interactive)
  (ecloud-compute--refresh-data)
  (message "Instances refreshed"))

(defun ecloud-compute-copy-name ()
  "Copy instance name."
  (interactive)
  (let ((id (tabulated-list-get-id)))
    (when id
      (kill-new id)
      (ecloud-notify (format "Copied: %s" id)))))

(defun ecloud-compute-copy-internal-ip ()
  "Copy internal IP."
  (interactive)
  (let ((entry (tabulated-list-get-entry)))
    (when entry
      (let ((ip (substring-no-properties (aref entry 3))))
        (kill-new ip)
        (ecloud-notify (format "Copied: %s" ip))))))

(defun ecloud-compute-copy-external-ip ()
  "Copy external IP."
  (interactive)
  (let ((entry (tabulated-list-get-entry)))
    (when entry
      (let ((ip (substring-no-properties (aref entry 4))))
        (kill-new ip)
        (ecloud-notify (format "Copied: %s" ip))))))

(defun ecloud-compute-start-instance ()
  "Start the instance at point."
  (interactive)
  (let* ((entry (tabulated-list-get-entry))
         (instance (tabulated-list-get-id))
         (zone (substring-no-properties (aref entry 1))))
    (when (yes-or-no-p (format "Start %s? " instance))
      (ecloud-notify (format "Starting %s..." instance))
      (ecloud-rpc-compute-start-instance-async
       zone instance
       (lambda (_resp)
         (ecloud-notify (format "Start request sent for %s" instance))
         (ecloud-compute-refresh))
       (lambda (err) (ecloud-notify (format "Failed to start: %s" err) 5))))))

(defun ecloud-compute-stop-instance ()
  "Stop the instance at point."
  (interactive)
  (let* ((entry (tabulated-list-get-entry))
         (instance (tabulated-list-get-id))
         (zone (substring-no-properties (aref entry 1))))
    (when (yes-or-no-p (format "Stop %s? " instance))
      (ecloud-notify (format "Stopping %s..." instance))
      (ecloud-rpc-compute-stop-instance-async
       zone instance
       (lambda (_resp)
         (ecloud-notify (format "Stop request sent for %s" instance))
         (ecloud-compute-refresh))
       (lambda (err) (ecloud-notify (format "Failed to stop: %s" err) 5))))))

(defun ecloud-compute-reset-instance ()
  "Reset (hard restart) the instance at point."
  (interactive)
  (let* ((entry (tabulated-list-get-entry))
         (instance (tabulated-list-get-id))
         (zone (substring-no-properties (aref entry 1))))
    (when (yes-or-no-p (format "Reset (hard restart) %s? " instance))
      (ecloud-notify (format "Resetting %s..." instance))
      (ecloud-rpc-compute-reset-instance-async
       zone instance
       (lambda (_resp)
         (ecloud-notify (format "Reset request sent for %s" instance))
         (ecloud-compute-refresh))
       (lambda (err) (ecloud-notify (format "Failed to reset: %s" err) 5))))))

(defun ecloud-compute-delete-instance ()
  "Delete the instance at point."
  (interactive)
  (let* ((entry (tabulated-list-get-entry))
         (instance (tabulated-list-get-id))
         (zone (substring-no-properties (aref entry 1))))
    (when (yes-or-no-p (format "DELETE %s? This cannot be undone! " instance))
      (ecloud-notify (format "Deleting %s..." instance))
      (ecloud-rpc-compute-delete-instance-async
       zone instance
       (lambda (_resp)
         (ecloud-notify (format "Delete request sent for %s" instance))
         (ecloud-compute-refresh))
       (lambda (err) (ecloud-notify (format "Failed to delete: %s" err) 5))))))

(defun ecloud-compute-help ()
  "Show help."
  (interactive)
  (message "Keys: [RET/s]SSH [S]Start [T]Stop [!]Reset [D]Delete [r]Refresh [c]CopyInternalIP [C]CopyExternalIP [w]CopyName [?]Help [q]Quit"))

;;; Mode definition

(defvar ecloud-compute-mode-map nil
  "Keymap for `ecloud-compute-mode'.")

(unless ecloud-compute-mode-map
  (setq ecloud-compute-mode-map (make-sparse-keymap))
  (define-key ecloud-compute-mode-map (kbd "RET") #'ecloud-compute-ssh)
  (define-key ecloud-compute-mode-map (kbd "s") #'ecloud-compute-ssh)
  (define-key ecloud-compute-mode-map (kbd "S") #'ecloud-compute-start-instance)
  (define-key ecloud-compute-mode-map (kbd "T") #'ecloud-compute-stop-instance)
  (define-key ecloud-compute-mode-map (kbd "!") #'ecloud-compute-reset-instance)
  (define-key ecloud-compute-mode-map (kbd "D") #'ecloud-compute-delete-instance)
  (define-key ecloud-compute-mode-map (kbd "r") #'ecloud-compute-refresh)
  (define-key ecloud-compute-mode-map (kbd "c") #'ecloud-compute-copy-internal-ip)
  (define-key ecloud-compute-mode-map (kbd "C") #'ecloud-compute-copy-external-ip)
  (define-key ecloud-compute-mode-map (kbd "w") #'ecloud-compute-copy-name)
  (define-key ecloud-compute-mode-map (kbd "?") #'ecloud-compute-help)
  (define-key ecloud-compute-mode-map (kbd "q") #'quit-window))

(define-derived-mode ecloud-compute-mode tabulated-list-mode "ECloud-Compute"
  "Major mode for browsing Compute Engine instances.

\\{ecloud-compute-mode-map}"
  (setq tabulated-list-padding 2)
  (add-hook 'tabulated-list-revert-hook #'ecloud-compute--refresh-data nil t)
  
  ;; Fetch config immediately to cache project/sa
  (ecloud-compute--get-config)
  
  (when (fboundp 'evil-motion-state)
    (evil-motion-state)))

;;; Evil mode support

(with-eval-after-load 'evil
  (evil-set-initial-state 'ecloud-compute-mode 'motion)
  (evil-define-key* 'motion ecloud-compute-mode-map
    (kbd "RET") #'ecloud-compute-ssh
    (kbd "s")   #'ecloud-compute-ssh
    (kbd "S")   #'ecloud-compute-start-instance
    (kbd "T")   #'ecloud-compute-stop-instance
    (kbd "!")   #'ecloud-compute-reset-instance
    (kbd "D")   #'ecloud-compute-delete-instance
    (kbd "r")   #'ecloud-compute-refresh
    (kbd "c")   #'ecloud-compute-copy-internal-ip
    (kbd "C")   #'ecloud-compute-copy-external-ip
    (kbd "w")   #'ecloud-compute-copy-name
    (kbd "?")   #'ecloud-compute-help
    (kbd "q")   #'quit-window))

;;; Entry point

;;;###autoload
(defun ecloud-compute-list ()
  "Open the ECloud Compute instances browser."
  (interactive)
  (let ((buffer (get-buffer-create "*ECloud-Compute*")))
    (with-current-buffer buffer
      (ecloud-compute-mode)
      (ecloud-compute--refresh-data))
    (switch-to-buffer buffer)))

(provide 'ecloud-compute)
;;; ecloud-compute.el ends here
