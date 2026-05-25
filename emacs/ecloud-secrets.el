;;; ecloud-secrets.el --- GCP Secret Manager browser for ecloud -*- lexical-binding: t; -*-

;; Copyright (C) 2024

;; Author: jing
;; Keywords: tools, cloud, gcp, secret-manager

;;; Commentary:

;; Provides interactive commands for managing GCP Secret Manager secrets:
;; list, access (read payload), create, add new version, and delete.
;;
;; Sensitive payload input is read via `read-passwd' so it never lands in
;; the echo area or command history.
;;
;; Entry points:
;;   M-x ecloud-secrets-list           - tabulated list of secrets
;;   M-x ecloud-secrets-access         - read a secret's payload into a buffer
;;   M-x ecloud-secrets-create         - create a new secret + initial version
;;   M-x ecloud-secrets-add-version    - add a new version to an existing secret
;;   M-x ecloud-secrets-delete         - delete an entire secret

;;; Code:

(require 'tabulated-list)
(require 'transient)
(require 'ecloud-rpc)
(require 'ecloud-notify)

(defgroup ecloud-secrets nil
  "ECloud Secret Manager settings."
  :group 'ecloud)

(defface ecloud-secrets-name-face
  '((t :inherit font-lock-function-name-face :weight bold))
  "Face for secret names."
  :group 'ecloud-secrets)

(defface ecloud-secrets-replication-face
  '((t :inherit font-lock-type-face))
  "Face for replication policy."
  :group 'ecloud-secrets)

;;; Internal helpers

(defun ecloud-secrets--secrets-cache ()
  "Return cached secrets list for the current buffer, or nil."
  (when (boundp 'ecloud-secrets--secrets)
    ecloud-secrets--secrets))

(defvar-local ecloud-secrets--secrets nil
  "List of secret plists currently displayed in this buffer.")

(defun ecloud-secrets--parse (secrets)
  "Parse SECRETS into tabulated-list entries."
  (mapcar
   (lambda (s)
     (let ((name (plist-get s :name))
           (replication (plist-get s :replication))
           (create-time (plist-get s :createTime))
           (labels (plist-get s :labels)))
       (list s
             (vector
              (propertize (or name "") 'face 'ecloud-secrets-name-face)
              (propertize (or replication "") 'face 'ecloud-secrets-replication-face)
              (or create-time "")
              (if labels
                  (mapconcat
                   (lambda (kv)
                     (format "%s=%s"
                             (substring (symbol-name (car kv)) 1)
                             (cdr kv)))
                   (let (acc (l labels))
                     ;; labels comes in as a plist; convert to alist
                     (while l
                       (push (cons (car l) (cadr l)) acc)
                       (setq l (cddr l)))
                     (nreverse acc))
                   ",")
                "-")))))
   secrets))

(defun ecloud-secrets--name-at-point ()
  "Return the secret name at point in the list buffer, or nil."
  (when-let ((entry (tabulated-list-get-id)))
    (plist-get entry :name)))

(defun ecloud-secrets--read-name (prompt)
  "Read a secret name, defaulting to the secret at point.
PROMPT is the prompt string."
  (let ((default (ecloud-secrets--name-at-point))
        (choices (mapcar (lambda (s) (plist-get s :name))
                         (ecloud-secrets--secrets-cache))))
    (if choices
        (completing-read
         (if default
             (format "%s (default %s): " prompt default)
           (format "%s: " prompt))
         choices nil nil nil nil default)
      (read-string (format "%s: " prompt) default))))

;;; List buffer

;; Keymap MUST be declared before `define-derived-mode' below, otherwise
;; the macro auto-creates an empty keymap, binds the symbol, and our
;; `defvar' becomes a no-op (defvar only sets unbound variables).
(defvar ecloud-secrets-mode-map
  (let ((map (make-sparse-keymap)))
    (set-keymap-parent map tabulated-list-mode-map)
    (define-key map (kbd "g") #'ecloud-secrets-refresh)
    (define-key map (kbd "RET") #'ecloud-secrets-access-at-point)
    (define-key map (kbd "a") #'ecloud-secrets-access-at-point)
    (define-key map (kbd "v") #'ecloud-secrets-add-version-at-point)
    (define-key map (kbd "+") #'ecloud-secrets-create)
    (define-key map (kbd "D") #'ecloud-secrets-delete-at-point)
    (define-key map (kbd "?") #'ecloud-secrets-help)
    (define-key map (kbd "q") #'quit-window)
    ;; Evil mode support: bind in motion state so they work in
    ;; tabulated-list-mode-derived buffers under evil-mode.
    (when (fboundp 'evil-define-key*)
      (evil-define-key* 'motion map
        (kbd "g") #'ecloud-secrets-refresh
        (kbd "RET") #'ecloud-secrets-access-at-point
        (kbd "a") #'ecloud-secrets-access-at-point
        (kbd "v") #'ecloud-secrets-add-version-at-point
        (kbd "+") #'ecloud-secrets-create
        (kbd "D") #'ecloud-secrets-delete-at-point
        (kbd "?") #'ecloud-secrets-help
        (kbd "q") #'quit-window))
    map)
  "Keymap for `ecloud-secrets-mode'.")

(define-derived-mode ecloud-secrets-mode tabulated-list-mode "ECloud-Secrets"
  "Major mode for browsing GCP Secret Manager secrets."
  (setq tabulated-list-format
        [("Name" 40 t)
         ("Replication" 14 t)
         ("Created" 26 t)
         ("Labels" 30 nil)])
  (setq tabulated-list-padding 1)
  (setq tabulated-list-sort-key (cons "Name" nil))
  (tabulated-list-init-header))

(transient-define-prefix ecloud-secrets-help ()
  "Secret Manager browser key bindings."
  [:description "Secret Manager"
   :class transient-columns
   ["Browse"
    ("RET" "Access at point"     ecloud-secrets-access-at-point)
    ("a"   "Access at point"     ecloud-secrets-access-at-point)
    ("g"   "Refresh list"        ecloud-secrets-refresh)]
   ["Modify"
    ("+"   "Create secret"       ecloud-secrets-create)
    ("v"   "Add new version"     ecloud-secrets-add-version-at-point)
    ("D"   "Delete secret"       ecloud-secrets-delete-at-point)]
   ["Window"
    ("q"   "Quit window"         quit-window)]])

(defun ecloud-secrets--display (secrets)
  "Render SECRETS into the *ECloud Secrets* buffer."
  (let ((buf (get-buffer-create "*ECloud Secrets*")))
    (with-current-buffer buf
      (ecloud-secrets-mode)
      (setq ecloud-secrets--secrets secrets)
      (setq tabulated-list-entries (ecloud-secrets--parse secrets))
      (tabulated-list-print t))
    (display-buffer buf)
    (select-window (get-buffer-window buf))))

;;;###autoload
(defun ecloud-secrets-list ()
  "List GCP Secret Manager secrets in the current project."
  (interactive)
  (ecloud-notify-info "Loading secrets...")
  (ecloud-rpc-secret-manager-list-secrets-async
   (lambda (response)
     (let ((secrets (plist-get response :secrets))
           (count (plist-get response :count)))
       (ecloud-secrets--display (or secrets '()))
       (ecloud-notify-success (format "Loaded %d secret(s)" (or count 0)))))
   (lambda (error-msg)
     (ecloud-notify-error (format "Failed to list secrets: %s" error-msg)))))

(defun ecloud-secrets-refresh ()
  "Refresh the secrets list buffer."
  (interactive)
  (ecloud-secrets-list))

;;; Access

(defun ecloud-secrets--show-payload (name version payload size)
  "Display PAYLOAD for secret NAME at VERSION (SIZE bytes) in a buffer."
  (let* ((buf-name (format "*Secret: %s@%s*" name version))
         (buf (get-buffer-create buf-name)))
    (with-current-buffer buf
      (let ((inhibit-read-only t))
        (erase-buffer)
        (insert payload))
      (goto-char (point-min))
      (setq buffer-read-only t)
      (setq-local header-line-format
                  (format " Secret: %s   Version: %s   Size: %d bytes   (q to bury)"
                          name version size))
      (use-local-map (let ((m (make-sparse-keymap)))
                       (define-key m (kbd "q") #'bury-buffer)
                       m)))
    (display-buffer buf)
    (select-window (get-buffer-window buf))))

;;;###autoload
(defun ecloud-secrets-access (name &optional version)
  "Access secret NAME at VERSION (default \"latest\") and show payload.
The payload is shown in a read-only buffer; it is NOT written to
the echo area or any file."
  (interactive
   (let* ((name (ecloud-secrets--read-name "Secret name"))
          (version (read-string "Version (default latest): " nil nil "latest")))
     (list name version)))
  (when (or (null name) (string-empty-p name))
    (user-error "Secret name is required"))
  (let ((ver (or version "latest")))
    (ecloud-notify-info (format "Accessing %s@%s..." name ver))
    (ecloud-rpc-secret-manager-access-version-async
     name ver
     (lambda (response)
       (let ((payload (plist-get response :payload))
             (encoding (plist-get response :encoding))
             (size (or (plist-get response :size) 0))
             (resolved (or (plist-get response :version) ver)))
         (if (string= encoding "binary")
             (ecloud-notify-info
              (format "Secret %s@%s is binary (%d bytes); hex: %s"
                      name resolved size (plist-get response :hex)))
           (ecloud-secrets--show-payload name resolved payload size)
           (ecloud-notify-success
            (format "Loaded %s@%s (%d bytes)" name resolved size)))))
     (lambda (error-msg)
       (ecloud-notify-error
        (format "Failed to access secret: %s" error-msg))))))

(defun ecloud-secrets-access-at-point ()
  "Access the secret at point (latest version)."
  (interactive)
  (let ((name (ecloud-secrets--name-at-point)))
    (unless name (user-error "No secret at point"))
    (ecloud-secrets-access name "latest")))

;;; Create

(defun ecloud-secrets--read-payload (prompt)
  "Read a secret payload from the minibuffer using PROMPT.
Uses `read-passwd' so it is not echoed."
  (let ((p (read-passwd (concat prompt ": ") t)))
    (when (or (null p) (string-empty-p p))
      (user-error "Payload cannot be empty"))
    p))

;;;###autoload
(defun ecloud-secrets-create (name)
  "Create a new secret NAME and prompt for its initial payload.
Payload is read with `read-passwd' (hidden input, not stored in history)."
  (interactive "sNew secret name: ")
  (when (string-empty-p name)
    (user-error "Secret name cannot be empty"))
  (let ((payload (ecloud-secrets--read-payload
                  (format "Payload for %s" name))))
    (ecloud-notify-info (format "Creating secret %s..." name))
    (ecloud-rpc-secret-manager-create-secret-async
     name payload
     (lambda (result)
       (ecloud-notify-success
        (format "Created secret %s (version %s)"
                (plist-get result :name)
                (plist-get result :version)))
       (when (get-buffer "*ECloud Secrets*")
         (ecloud-secrets-list)))
     (lambda (error-msg)
       (ecloud-notify-error
        (format "Failed to create secret: %s" error-msg))))))

;;; Add version

;;;###autoload
(defun ecloud-secrets-add-version (name)
  "Add a new version to existing secret NAME.
Payload is read with `read-passwd'."
  (interactive
   (list (ecloud-secrets--read-name "Add version to secret")))
  (when (or (null name) (string-empty-p name))
    (user-error "Secret name is required"))
  (let ((payload (ecloud-secrets--read-payload
                  (format "New payload for %s" name))))
    (ecloud-notify-info (format "Adding new version to %s..." name))
    (ecloud-rpc-secret-manager-add-version-async
     name payload
     (lambda (result)
       (ecloud-notify-success
        (format "Added version %s to secret %s"
                (plist-get result :version)
                (plist-get result :name))))
     (lambda (error-msg)
       (ecloud-notify-error
        (format "Failed to add version: %s" error-msg))))))

(defun ecloud-secrets-add-version-at-point ()
  "Add a new version to the secret at point."
  (interactive)
  (let ((name (ecloud-secrets--name-at-point)))
    (unless name (user-error "No secret at point"))
    (ecloud-secrets-add-version name)))

;;; Delete

;;;###autoload
(defun ecloud-secrets-delete (name)
  "Delete secret NAME and all of its versions, after confirmation."
  (interactive
   (list (ecloud-secrets--read-name "Delete secret")))
  (when (or (null name) (string-empty-p name))
    (user-error "Secret name is required"))
  (when (yes-or-no-p
         (format "Really delete secret '%s' and ALL its versions? This cannot be undone. "
                 name))
    (ecloud-notify-info (format "Deleting %s..." name))
    (ecloud-rpc-secret-manager-delete-secret-async
     name
     (lambda (_result)
       (ecloud-notify-success (format "Deleted secret %s" name))
       (when (get-buffer "*ECloud Secrets*")
         (ecloud-secrets-list)))
     (lambda (error-msg)
       (ecloud-notify-error
        (format "Failed to delete secret: %s" error-msg))))))

(defun ecloud-secrets-delete-at-point ()
  "Delete the secret at point."
  (interactive)
  (let ((name (ecloud-secrets--name-at-point)))
    (unless name (user-error "No secret at point"))
    (ecloud-secrets-delete name)))

(provide 'ecloud-secrets)
;;; ecloud-secrets.el ends here
