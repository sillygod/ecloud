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

(defface ecloud-sql-state-stopped-face
  '((t :inherit font-lock-comment-face))
  "Face for STOPPED state."
  :group 'ecloud-sql)

;;; Helper functions

(defun ecloud-sql--format-state (state)
  "Format STATE with appropriate face."
  (cond
   ((string= state "RUNNABLE")
    (propertize state 'face 'ecloud-sql-state-runnable-face))
   ((string= state "STOPPED")
    (propertize state 'face 'ecloud-sql-state-stopped-face))
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
            (raw-state (plist-get inst :state))
            (settings (plist-get inst :settings))
            (activation-policy (plist-get settings :activationPolicy))
            (state (if (and (string= raw-state "RUNNABLE")
                            (string= activation-policy "NEVER"))
                       "STOPPED"
                     raw-state))
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

(defun ecloud-sql-show-backups ()
  "Show backups for the instance at point."
  (interactive)
  (let* ((inst (tabulated-list-get-id))
         (name (plist-get inst :name)))
    (unless name (user-error "No instance selected"))
    (message "Fetching backups for %s..." name)
    (let ((buffer (get-buffer-create (format "*ECloud-SQL-Backups: %s*" name))))
      (with-current-buffer buffer (special-mode))
      (pop-to-buffer buffer)
      (ecloud-rpc-sql-list-backups-async
       name
       (lambda (resp)
         (when (buffer-live-p buffer)
           (with-current-buffer buffer
             (let ((inhibit-read-only t))
               (erase-buffer)
               (insert (format "Backups for %s:\n\n" name))
               (dolist (bk (plist-get resp :backups))
                 (insert (format "- ID: %s (Status: %s, End: %s)\n  Desc: %s\n"
                                 (plist-get bk :id)
                                 (plist-get bk :status)
                                 (plist-get bk :endTime)
                                 (or (plist-get bk :description) "Auto/None"))))
               (insert "\n[Press 'q' to close]")))))
       (lambda (err) (message "Failed to fetch backups: %s" err))))))

(defun ecloud-sql-connection-info ()
  "Show connection info for the instance at point."
  (interactive)
  (let* ((inst (tabulated-list-get-id))
         (name (plist-get inst :name)))
    (unless name (user-error "No instance selected"))
    (message "Fetching info for %s..." name)
    (ecloud-rpc-sql-get-connection-info-async
     name
     (lambda (resp)
       (let ((buffer (get-buffer-create (format "*ECloud-SQL-Info: %s*" name))))
         (with-current-buffer buffer
           (let ((inhibit-read-only t))
             (erase-buffer)
             (insert (format "Connection Info for %s:\n\n" name))
             (insert (format "Connection Name: %s\n" (plist-get resp :connectionName)))
             (insert (format "Public IP:       %s\n" (plist-get resp :publicIp)))
             (insert (format "Private IP:      %s\n" (plist-get resp :privateIp)))
             (insert (format "DB Version:      %s\n\n" (plist-get resp :databaseVersion)))
             (insert "Connection Strings (Templates):\n")
             (let ((uris (plist-get resp :connectionUris)))
               (dolist (key (mapcar #'car (seq-partition uris 2))) ; Iterate plist keys
                 (let ((val (plist-get uris key)))
                   (insert (format "%s:\n  %s\n" key val)))))
             (special-mode)))
         (pop-to-buffer buffer)))
     (lambda (err) (message "Failed to fetch info: %s" err)))))

(defun ecloud-sql-help ()
  "Show help for ecloud-sql-mode."
  (interactive)
  (message "SQL Keys: [d]DBs [u]Users [b]Backups [i]Info [+d]NewDB [Dd]DelDB [+u]NewUser [Du]DelUser [p]Proxy [r]Refresh [?]Help [q]Quit"))

(defvar ecloud-sql-mode-map nil "Keymap for `ecloud-sql-mode'.")

(unless ecloud-sql-mode-map
  (setq ecloud-sql-mode-map (make-sparse-keymap))
  (define-key ecloud-sql-mode-map (kbd "d") #'ecloud-sql-show-databases)
  (define-key ecloud-sql-mode-map (kbd "u") #'ecloud-sql-show-users)
  (define-key ecloud-sql-mode-map (kbd "b") #'ecloud-sql-show-backups)
  (define-key ecloud-sql-mode-map (kbd "i") #'ecloud-sql-connection-info)
  (define-key ecloud-sql-mode-map (kbd "+ d") #'ecloud-sql-create-database)
  (define-key ecloud-sql-mode-map (kbd "D d") #'ecloud-sql-delete-database)
  (define-key ecloud-sql-mode-map (kbd "+ u") #'ecloud-sql-create-user)
  (define-key ecloud-sql-mode-map (kbd "D u") #'ecloud-sql-delete-user)
  (define-key ecloud-sql-mode-map (kbd "p") #'ecloud-sql-toggle-proxy)
  (define-key ecloud-sql-mode-map (kbd "r") #'ecloud-sql-refresh)
  (define-key ecloud-sql-mode-map (kbd "?") #'ecloud-sql-help)
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
  (evil-define-key* 'motion ecloud-sql-mode-map
    (kbd "d") #'ecloud-sql-show-databases
    (kbd "u") #'ecloud-sql-show-users
    (kbd "b") #'ecloud-sql-show-backups
    (kbd "i") #'ecloud-sql-connection-info
    (kbd "+ d") #'ecloud-sql-create-database
    (kbd "D d") #'ecloud-sql-delete-database
    (kbd "+ u") #'ecloud-sql-create-user
    (kbd "D u") #'ecloud-sql-delete-user
    (kbd "p") #'ecloud-sql-toggle-proxy
    (kbd "r") #'ecloud-sql-refresh
    (kbd "?") #'ecloud-sql-help
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
