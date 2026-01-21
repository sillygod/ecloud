;;; ecloud-sql.el --- Cloud SQL browser for ecloud -*- lexical-binding: t; -*-

;;; Commentary:

;; A browser for Google Cloud SQL instances.

;;; Code:

(require 'tabulated-list)
(require 'ecloud-rpc)
;; Hooking into ecloud-ws events if available
(defvar ecloud-sql-event-hook nil)

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

(defun ecloud-sql--parse-instances (instances proxies)
  "Parse INSTANCES list and PROXIES plist into tabulated list entries."
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
   instances))

(defun ecloud-sql--fetch-data-async ()
  "Fetch instances and proxies asynchronously."
  (setq tabulated-list-items nil)
  (let ((buffer (current-buffer)))
    (ecloud-rpc-sql-list-proxies-async
     (lambda (proxies-resp)
       (let ((proxies (plist-get proxies-resp :proxies)))
         (ecloud-rpc-sql-list-instances-async
          (lambda (response)
            (let ((instances (plist-get response :instances)))
              (when (buffer-live-p buffer)
                (with-current-buffer buffer
                  (setq tabulated-list-entries
                        (ecloud-sql--parse-instances instances proxies))
                  (tabulated-list-print t)
                  (message "Cloud SQL instances refreshed."))))
            (lambda (err)
              (message "Error fetching instances: %s" err)))))
       (lambda (err)
         (message "Error fetching proxies: %s" err))))))

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
   (setq tabulated-list-entries nil)
   (tabulated-list-init-header)
   (tabulated-list-print t)
   (message "Refreshing Cloud SQL instances...")
   (ecloud-sql--fetch-data-async))

(defun ecloud-sql--update-proxy-local (connection-name port)
  "Update proxy status locally for CONNECTION-NAME to PORT."
  ;; 1. Update entry in tabulated-list-entries
  (let ((entry (seq-find (lambda (e) 
                           (string= (plist-get (car e) :connectionName) connection-name))
                         tabulated-list-entries)))
    (when entry
      (let* ((inst (car entry))
             (vec (cadr entry))
             (proxy-str (if port (format "ON (%s)" port) "")))
        ;; Update the vector in place (index 6 is Proxy column)
        (aset vec 6 proxy-str)
        
        ;; 2. Update the buffer line if visible
        (when (get-buffer "*ECloud-SQL*")
          (with-current-buffer "*ECloud-SQL*"
            (save-excursion
              (goto-char (point-min))
              (while (not (eobp))
                (let ((id (tabulated-list-get-id)))
                  (if (and id (string= (plist-get id :connectionName) connection-name))
                      (progn
                        (tabulated-list-delete-entry)
                        (tabulated-list-print-entry id vec)
                        (goto-char (point-max))) ;; Break loop
                    (forward-line 1)))))))))))

(defun ecloud-sql--on-event (type data)
  "Handle WebSocket event TYPE with DATA."
  (cond
   ((string= type "sql_proxy_started")
    (let ((name (plist-get data :connection_name))
          (port (plist-get data :port)))
      (message "Proxy started for %s on port %s" name port)
      (ecloud-sql--update-proxy-local name port)))
   ((string= type "sql_proxy_stopped")
    (let ((name (plist-get data :connection_name)))
      (message "Proxy stopped for %s" name)
      (ecloud-sql--update-proxy-local name nil)))))

;;; Actions

(defun ecloud-sql-show-databases ()
  "Show databases for the instance at point."
  (interactive)
  (let* ((inst (tabulated-list-get-id))
         (name (plist-get inst :name)))
    (unless name (user-error "No instance selected"))
    (message "Fetching databases for %s..." name)
    (let ((buffer (get-buffer-create (format "*ECloud-SQL-Databases: %s*" name))))
      (with-current-buffer buffer (special-mode)) ;; Ensure mode is set
      (pop-to-buffer buffer)
      (ecloud-rpc-sql-list-databases-async 
       name
       (lambda (resp)
         (when (buffer-live-p buffer)
           (with-current-buffer buffer
             (let ((inhibit-read-only t))
               (erase-buffer)
               (insert (format "Databases for %s:\n\n" name))
               (dolist (db (plist-get resp :databases))
                 (insert (format "- %s (Charset: %s, Collation: %s)\n"
                                 (plist-get db :name)
                                 (plist-get db :charset)
                                 (plist-get db :collation))))
               (message "Databases loaded for %s" name)))))
       (lambda (err)
         (message "Failed to fetch databases: %s" err))))))

(defun ecloud-sql-show-users ()
  "Show users for the instance at point."
  (interactive)
  (let* ((inst (tabulated-list-get-id))
         (name (plist-get inst :name)))
    (unless name (user-error "No instance selected"))
    (message "Fetching users for %s..." name)
    (let ((buffer (get-buffer-create (format "*ECloud-SQL-Users: %s*" name))))
      (with-current-buffer buffer (special-mode)) ;; Ensure mode is set
      (pop-to-buffer buffer) 
      (ecloud-rpc-sql-list-users-async 
       name
       (lambda (resp)
         (when (buffer-live-p buffer)
           (with-current-buffer buffer
             (let ((inhibit-read-only t))
               (erase-buffer)
               (insert (format "Users for %s:\n\n" name))
               (dolist (user (plist-get resp :users))
                 (insert (format "- %s (Host: %s, Type: %s)\n"
                                 (plist-get user :name)
                                 (plist-get user :host)
                                 (plist-get user :type))))
               (message "Users loaded for %s" name)))))
       (lambda (err)
         (message "Failed to fetch users: %s" err))))))

(defun ecloud-sql-toggle-proxy ()
  "Start or stop proxy for the instance at point.
Prompts for a port (leave empty for random port)."
  (interactive)
  (let* ((inst (tabulated-list-get-id))
         (connection-name (plist-get inst :connectionName))
         (version (plist-get inst :databaseVersion))
         (proxies-resp (ecloud-rpc-sql-list-proxies))
         (proxies (plist-get proxies-resp :proxies))
         (current-port (plist-get proxies (intern (concat ":" connection-name))))
         (buffer (current-buffer)))

    (if current-port
        (progn
          (message "Stopping proxy for %s..." connection-name)
          (ecloud-rpc-sql-stop-proxy-async
           connection-name
           (lambda (_resp)
             ;; Updates will come via WebSocket
             (message "Proxy stopped for %s" connection-name))
           (lambda (err)
             (message "Failed to stop proxy: %s" err))))
      (progn
        (let* ((port-str (read-string "Port (default random): "))
               (port (if (string= port-str "") nil (string-to-number port-str))))

          (message "Starting proxy for %s..." connection-name)
          (ecloud-rpc-sql-start-proxy-async
           connection-name port version
           (lambda (resp)
             ;; Updates will come via WebSocket
             (message "Proxy started on port %s" (plist-get resp :port)))
           (lambda (err)
             (message "Failed to start proxy: %s" err))))))))

(defun ecloud-sql-create-database ()
  "Create a new database in the current instance."
  (interactive)
  (let* ((inst (tabulated-list-get-id))
         (instance (plist-get inst :name)))
    (unless instance (user-error "No instance selected"))
    (let ((name (read-string "Database name: "))
          (charset (read-string "Charset (default UTF8): " nil nil "UTF8"))
          (collation (read-string "Collation (default en_US.UTF8): " nil nil "en_US.UTF8")))
      (when (not (string-empty-p name))
        (message "Creating database %s..." name)
        (ecloud-rpc-sql-create-database-async 
         instance name charset collation
         (lambda (_resp)
           (message "Database %s created" name))
         (lambda (err)
           (message "Failed to create database: %s" err)))))))

(defun ecloud-sql-delete-database ()
  "Delete a database from the current instance."
  (interactive)
  (let* ((inst (tabulated-list-get-id))
         (instance (plist-get inst :name)))
    (unless instance (user-error "No instance selected"))
    (let ((name (read-string "Database to delete: ")))
      (when (yes-or-no-p (format "Delete database %s from %s? " name instance))
        (message "Deleting database %s..." name)
        (ecloud-rpc-sql-delete-database-async
         instance name
         (lambda (_resp)
           (message "Database %s deleted" name))
         (lambda (err)
           (message "Failed to delete database: %s" err)))))))

(defun ecloud-sql-create-user ()
  "Create a new user in the current instance."
  (interactive)
  (let* ((inst (tabulated-list-get-id))
         (instance (plist-get inst :name)))
    (unless instance (user-error "No instance selected"))
    (let ((name (read-string "Username: "))
          (password (read-passwd "Password: "))
          (host (read-string "Host (default %): " nil nil "%")))
      (when (and (not (string-empty-p name)) (not (string-empty-p password)))
        (message "Creating user %s..." name)
        (ecloud-rpc-sql-create-user-async 
         instance name password host
         (lambda (_resp)
           (message "User %s created" name))
         (lambda (err)
           (message "Failed to create user: %s" err)))))))

(defun ecloud-sql-delete-user ()
  "Delete a user from the current instance."
  (interactive)
  (let* ((inst (tabulated-list-get-id))
         (instance (plist-get inst :name)))
    (unless instance (user-error "No instance selected"))
    (let ((name (read-string "Username to delete: "))
          (host (read-string "Host (default %): " nil nil "%")))
      (when (yes-or-no-p (format "Delete user %s@%s from %s? " name host instance))
        (message "Deleting user %s..." name)
        (ecloud-rpc-sql-delete-user-async
         instance name host
         (lambda (_resp)
           (message "User %s deleted" name))
         (lambda (err)
           (message "Failed to delete user: %s" err)))))))

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
  (define-key ecloud-sql-mode-map (kbd "+ d") #'ecloud-sql-create-database)
  (define-key ecloud-sql-mode-map (kbd "D d") #'ecloud-sql-delete-database)
  (define-key ecloud-sql-mode-map (kbd "+ u") #'ecloud-sql-create-user)
  (define-key ecloud-sql-mode-map (kbd "D u") #'ecloud-sql-delete-user)
  (define-key ecloud-sql-mode-map (kbd "p") #'ecloud-sql-toggle-proxy)
  (define-key ecloud-sql-mode-map (kbd "r") #'ecloud-sql-refresh)
  (define-key ecloud-sql-mode-map (kbd "q") #'quit-window))

(define-derived-mode ecloud-sql-mode tabulated-list-mode "ECloud-SQL"
  "Major mode for browsing Cloud SQL instances.
\\{ecloud-sql-mode-map}"
  (setq tabulated-list-padding 2)
  (add-hook 'tabulated-list-revert-hook #'ecloud-sql--refresh-data nil t)
  (add-hook 'ecloud-sql-event-hook #'ecloud-sql--on-event nil t)
  (when (fboundp 'evil-motion-state)
    (evil-motion-state)))

;;; Evil mode support

(with-eval-after-load 'evil
  (evil-set-initial-state 'ecloud-sql-mode 'motion)
  (evil-define-key 'motion ecloud-sql-mode-map
    (kbd "d") #'ecloud-sql-show-databases
    (kbd "u") #'ecloud-sql-show-users
    (kbd "+ d") #'ecloud-sql-create-database
    (kbd "D d") #'ecloud-sql-delete-database
    (kbd "+ u") #'ecloud-sql-create-user
    (kbd "D u") #'ecloud-sql-delete-user
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
