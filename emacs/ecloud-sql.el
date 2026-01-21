;;; ecloud-sql.el --- Cloud SQL browser for ecloud -*- lexical-binding: t; -*-

;;; Commentary:

;; A browser for Google Cloud SQL instances.

;;; Code:

(require 'tabulated-list)
(require 'ecloud-rpc)

;;; Customization

(defgroup ecloud-sql nil
  "ECloud SQL settings."
  :group 'ecloud)

(defface ecloud-sql-name-face
  '((t :inherit font-lock-function-name-face :weight bold))
  "Face for instance names."
  :group 'ecloud-sql)

(defface ecloud-sql-state-runnable-face
  '((t :inherit font-lock-string-face))
  "Face for RUNNABLE state."
  :group 'ecloud-sql)

;;; Helper functions

(defun ecloud-sql--format-state (state)
  "Format STATE with appropriate face."
  (cond
   ((string= state "RUNNABLE")
    (propertize state 'face 'ecloud-sql-state-runnable-face))
   (t state)))

(defun ecloud-sql--get-ip (ip-addresses type)
  "Get IP address of TYPE (PRIMARY/PRIVATE) from list."
  (let ((ip (seq-find (lambda (x) (string= (plist-get x :type) type)) ip-addresses)))
    (if ip (plist-get ip :ipAddress) "")))

;;; Data fetching

(defun ecloud-sql--fetch-instances ()
  "Fetch and return instance list entries."
  (let* ((response (ecloud-rpc-sql-list-instances))
         (instances (plist-get response :instances))
         (proxies-resp (ecloud-rpc-sql-list-proxies))
         (proxies (plist-get proxies-resp :proxies)))
    (mapcar
     (lambda (inst)
       (let* ((name (plist-get inst :name))
              (connection-name (plist-get inst :connectionName))
              (version (plist-get inst :databaseVersion))
              (region (plist-get inst :region))
              (state (plist-get inst :state))
              (ip-addresses (plist-get inst :ipAddresses))
              (public-ip (ecloud-sql--get-ip ip-addresses "PRIMARY"))
              (private-ip (ecloud-sql--get-ip ip-addresses "PRIVATE"))
              (proxy-port (plist-get proxies (intern (concat ":" connection-name)))))
         (list inst
               (vector
                (propertize name 'face 'ecloud-sql-name-face)
                version
                region
                (ecloud-sql--format-state state)
                public-ip
                private-ip
                (if proxy-port (format "ON (%s)" proxy-port) "")))))
     instances)))

(defun ecloud-sql--refresh-data ()
  "Refresh the tabulated list data."
  (setq tabulated-list-format
        [("Name" 25 t)
         ("Version" 15 t)
         ("Region" 15 t)
         ("State" 12 t)
         ("Public IP" 15 nil)
         ("Private IP" 15 nil)
         ("Proxy" 15 nil)])
  (setq tabulated-list-entries #'ecloud-sql--fetch-instances)
  (tabulated-list-init-header)
  (tabulated-list-print t))

;;; Actions

(defun ecloud-sql-show-databases ()
  "Show databases for the instance at point."
  (interactive)
  (let* ((inst (tabulated-list-get-id))
         (name (plist-get inst :name)))
    (unless name (user-error "No instance selected"))
    (message "Fetching databases for %s..." name)
    (let ((resp (ecloud-rpc-sql-list-databases name)))
      (with-current-buffer (get-buffer-create (format "*ECloud-SQL-Databases: %s*" name))
        (erase-buffer)
        (insert (format "Databases for %s:\n\n" name))
        (dolist (db (plist-get resp :databases))
          (insert (format "- %s (Charset: %s, Collation: %s)\n"
                          (plist-get db :name)
                          (plist-get db :charset)
                          (plist-get db :collation))))
        (special-mode)
        (pop-to-buffer (current-buffer))))))

(defun ecloud-sql-show-users ()
  "Show users for the instance at point."
  (interactive)
  (let* ((inst (tabulated-list-get-id))
         (name (plist-get inst :name)))
    (unless name (user-error "No instance selected"))
    (message "Fetching users for %s..." name)
    (let ((resp (ecloud-rpc-sql-list-users name)))
      (with-current-buffer (get-buffer-create (format "*ECloud-SQL-Users: %s*" name))
        (erase-buffer)
        (insert (format "Users for %s:\n\n" name))
        (dolist (user (plist-get resp :users))
          (insert (format "- %s (Host: %s, Type: %s)\n"
                          (plist-get user :name)
                          (plist-get user :host)
                          (plist-get user :type))))
        (special-mode)
        (pop-to-buffer (current-buffer))))))

(defun ecloud-sql-toggle-proxy ()
  "Start or stop proxy for the instance at point.
Prompts for a port (leave empty for random port)."
  (interactive)
  (let* ((inst (tabulated-list-get-id))
         (connection-name (plist-get inst :connectionName))
         (version (plist-get inst :databaseVersion))
         (proxies-resp (ecloud-rpc-sql-list-proxies))
         (proxies (plist-get proxies-resp :proxies))
         (current-port (plist-get proxies (intern (concat ":" connection-name)))))
    
    (if current-port
        (progn
          (message "Stopping proxy for %s..." connection-name)
          (ecloud-rpc-sql-stop-proxy connection-name)
          (message "Proxy stopped"))
      (progn
        (let* ((port-str (read-string "Port (default random): "))
               (port (if (string= port-str "") nil (string-to-number port-str)))
               (resp nil))
               
          (message "Starting proxy for %s..." connection-name)
          (setq resp (ecloud-rpc-sql-start-proxy connection-name port version))
          (message "Proxy started on port %s" (plist-get resp :port)))))
    (ecloud-sql--refresh-data)))

(defun ecloud-sql-refresh ()
  "Refresh the list."
  (interactive)
  (ecloud-sql--refresh-data)
  (message "Refreshed"))

;;; Mode definition

(defvar ecloud-sql-mode-map nil "Keymap for `ecloud-sql-mode'.")

(unless ecloud-sql-mode-map
  (setq ecloud-sql-mode-map (make-sparse-keymap))
  (define-key ecloud-sql-mode-map (kbd "d") #'ecloud-sql-show-databases)
  (define-key ecloud-sql-mode-map (kbd "u") #'ecloud-sql-show-users)
  (define-key ecloud-sql-mode-map (kbd "p") #'ecloud-sql-toggle-proxy)
  (define-key ecloud-sql-mode-map (kbd "r") #'ecloud-sql-refresh)
  (define-key ecloud-sql-mode-map (kbd "q") #'quit-window))

(define-derived-mode ecloud-sql-mode tabulated-list-mode "ECloud-SQL"
  "Major mode for browsing Cloud SQL instances.
\\{ecloud-sql-mode-map}"
  (setq tabulated-list-padding 2)
  (add-hook 'tabulated-list-revert-hook #'ecloud-sql--refresh-data nil t)
  (when (fboundp 'evil-motion-state)
    (evil-motion-state)))

;;; Evil mode support

(with-eval-after-load 'evil
  (evil-set-initial-state 'ecloud-sql-mode 'motion)
  (evil-define-key 'motion ecloud-sql-mode-map
    (kbd "d") #'ecloud-sql-show-databases
    (kbd "u") #'ecloud-sql-show-users
    (kbd "p") #'ecloud-sql-toggle-proxy
    (kbd "r") #'ecloud-sql-refresh
    (kbd "q") #'quit-window))

;;; Entry point

;;;###autoload
(defun ecloud-sql-list ()
  "Open the ECloud SQL instances browser."
  (interactive)
  (let ((buffer (get-buffer-create "*ECloud-SQL*")))
    (with-current-buffer buffer
      (ecloud-sql-mode)
      (ecloud-sql--refresh-data))
    (switch-to-buffer buffer)))

(provide 'ecloud-sql)
;;; ecloud-sql.el ends here
