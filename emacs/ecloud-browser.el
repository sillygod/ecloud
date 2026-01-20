;;; ecloud-browser.el --- File browser UI for ecloud -*- lexical-binding: t; -*-

;; Copyright (C) 2024

;; Author: ecloud
;; Keywords: tools, cloud

;;; Commentary:

;; A dired-like browser for GCS buckets using tabulated-list-mode.

;;; Code:

(require 'tabulated-list)
(require 'ecloud-rpc)

;;; Customization

(defgroup ecloud-browser nil
  "ECloud browser settings."
  :group 'ecloud)

(defface ecloud-browser-bucket-face
  '((t :inherit font-lock-constant-face :weight bold))
  "Face for bucket names."
  :group 'ecloud-browser)

(defface ecloud-browser-folder-face
  '((t :inherit font-lock-function-name-face))
  "Face for folder names."
  :group 'ecloud-browser)

(defface ecloud-browser-file-face
  '((t :inherit default))
  "Face for file names."
  :group 'ecloud-browser)

(defface ecloud-browser-size-face
  '((t :inherit font-lock-type-face))
  "Face for file sizes."
  :group 'ecloud-browser)

;;; Buffer-local state

(defvar-local ecloud-browser--current-bucket nil
  "Current bucket being browsed, nil for bucket list.")

(defvar-local ecloud-browser--current-prefix ""
  "Current prefix (folder path) within the bucket.")

(defvar-local ecloud-browser--navigation-stack nil
  "Stack of (bucket . prefix) for back navigation.")

;;; Helper functions

(defun ecloud-browser--format-size (size)
  "Format SIZE in bytes to human-readable string."
  (cond
   ((< size 1024) (format "%d B" size))
   ((< size (* 1024 1024)) (format "%.1f KB" (/ size 1024.0)))
   ((< size (* 1024 1024 1024)) (format "%.1f MB" (/ size 1024.0 1024.0)))
   (t (format "%.1f GB" (/ size 1024.0 1024.0 1024.0)))))

(defun ecloud-browser--extract-name (full-path prefix)
  "Extract display name from FULL-PATH given current PREFIX."
  (let ((name (if (string-prefix-p prefix full-path)
                  (substring full-path (length prefix))
                full-path)))
    ;; Remove trailing slash for folder display
    (if (string-suffix-p "/" name)
        (substring name 0 -1)
      name)))

(defun ecloud-browser--format-date (iso-date)
  "Format ISO-DATE string for display."
  (if (and iso-date (not (string-empty-p iso-date)))
      (substring iso-date 0 (min 19 (length iso-date)))
    ""))

;;; Mode definition


(defun ecloud-browser-help ()
  "Show help for ecloud-browser-mode."
  (interactive)
  (message "GCS Keys: [RET]Enter [^]Up [g]Refresh [d]Download [u]Upload [D]Delete [+]Mkdir [c]CopyPath [?]Help [q]Quit"))

(defvar ecloud-browser-mode-map nil
  "Keymap for `ecloud-browser-mode'.")

(unless ecloud-browser-mode-map
  (setq ecloud-browser-mode-map (make-sparse-keymap))
  (define-key ecloud-browser-mode-map (kbd "RET") #'ecloud-browser-enter)
  (define-key ecloud-browser-mode-map (kbd "^") #'ecloud-browser-up)
  (define-key ecloud-browser-mode-map (kbd "g") #'ecloud-browser-refresh)
  (define-key ecloud-browser-mode-map (kbd "d") #'ecloud-browser-download)
  (define-key ecloud-browser-mode-map (kbd "u") #'ecloud-browser-upload)
  (define-key ecloud-browser-mode-map (kbd "D") #'ecloud-browser-delete)
  (define-key ecloud-browser-mode-map (kbd "+") #'ecloud-browser-create-folder)
  (define-key ecloud-browser-mode-map (kbd "c") #'ecloud-browser-copy-path)
  (define-key ecloud-browser-mode-map (kbd "?") #'ecloud-browser-help)
  (define-key ecloud-browser-mode-map (kbd "q") #'quit-window))

(define-derived-mode ecloud-browser-mode tabulated-list-mode "ECloud"
  "Major mode for browsing GCS buckets.

\\{ecloud-browser-mode-map}"
  (setq tabulated-list-padding 2)
  (add-hook 'tabulated-list-revert-hook #'ecloud-browser--refresh-data nil t)
  ;; Force Evil Motion state if available
  (when (fboundp 'evil-motion-state)
    (evil-motion-state)))

;;; Data fetching

(defun ecloud-browser--fetch-buckets ()
  "Fetch and return bucket list entries for tabulated-list."
  (let* ((response (ecloud-rpc-list-buckets))
         (buckets (plist-get response :buckets)))
    (mapcar
     (lambda (bucket)
       (let ((name (plist-get bucket :name))
             (location (plist-get bucket :location))
             (storage-class (plist-get bucket :storage_class)))
         (list name
               (vector
                (propertize name 'face 'ecloud-browser-bucket-face)
                location
                storage-class
                ""))))
     buckets)))

(defun ecloud-browser--fetch-objects ()
  "Fetch and return object list entries for tabulated-list."
  (let* ((response (ecloud-rpc-list-objects
                    ecloud-browser--current-bucket
                    ecloud-browser--current-prefix))
         (objects (plist-get response :objects))
         (prefixes (plist-get response :prefixes))
         (entries '()))

    ;; Add folders first
    (dolist (prefix prefixes)
      (let ((name (ecloud-browser--extract-name prefix ecloud-browser--current-prefix)))
        (push (list prefix
                    (vector
                     (propertize (concat name "/") 'face 'ecloud-browser-folder-face)
                     ""
                     "folder"
                     ""))
              entries)))

    ;; Add files
    (dolist (obj objects)
      (let ((full-name (plist-get obj :name))
            (size (plist-get obj :size))
            (updated (plist-get obj :updated))
            (is-folder (plist-get obj :is_folder)))
        (unless is-folder  ; Skip folder placeholder objects
          (let ((name (ecloud-browser--extract-name full-name ecloud-browser--current-prefix)))
            (push (list full-name
                        (vector
                         (propertize name 'face 'ecloud-browser-file-face)
                         (propertize (ecloud-browser--format-size size)
                                    'face 'ecloud-browser-size-face)
                         "file"
                         (ecloud-browser--format-date updated)))
                  entries)))))

    (nreverse entries)))

(defun ecloud-browser--refresh-data ()
  "Refresh the tabulated list data."
  (if (not ecloud-browser--current-bucket)
      ;; List buckets
      (progn
        (setq tabulated-list-format [("Bucket Name" 50 t) ("Location" 15 t) ("Storage Class" 15 t) ("Created" 20 nil)])
        (setq tabulated-list-entries #'ecloud-browser--fetch-buckets))
    ;; List objects
    (progn
      (setq tabulated-list-format [("Name" 60 t) ("Size" 10 nil) ("Type" 10 nil) ("Updated" 20 nil)])
      (setq tabulated-list-entries #'ecloud-browser--fetch-objects)))

  (setq mode-name
        (if ecloud-browser--current-bucket
            (format "ECloud %s/%s" ecloud-browser--current-bucket ecloud-browser--current-prefix)
          "ECloud Buckets"))
  (force-mode-line-update)

  (tabulated-list-init-header) ;; Ensure headers are initialized
  (tabulated-list-print t))

;;; Interactive commands

(defun ecloud-browser-enter ()
  "Enter the item at point (bucket or folder)."
  (interactive)
  (message "!!!!!")
  (let* ((entry (tabulated-list-get-entry))
         (id (tabulated-list-get-id)))
    (when entry
      (if ecloud-browser--current-bucket
          ;; In bucket view - check if it's a folder (prefix)
          (when (string-suffix-p "/" id)
            (push (cons ecloud-browser--current-bucket
                        ecloud-browser--current-prefix)
                  ecloud-browser--navigation-stack)
            (setq ecloud-browser--current-prefix id)
            (ecloud-browser-refresh))
        ;; In bucket list - enter bucket
        (push (cons nil "") ecloud-browser--navigation-stack)
        (setq ecloud-browser--current-bucket id)
        (setq ecloud-browser--current-prefix "")
        (ecloud-browser-refresh)))))

(defun ecloud-browser-up ()
  "Go up one level (parent folder or bucket list)."
  (interactive)
  (cond
   ;; In a subfolder - go to parent
   ((and ecloud-browser--current-bucket
         (not (string-empty-p ecloud-browser--current-prefix)))
    (let ((parts (split-string (string-trim-right ecloud-browser--current-prefix "/") "/")))
      (setq ecloud-browser--current-prefix
            (if (> (length parts) 1)
                (concat (string-join (butlast parts) "/") "/")
              ""))
      (ecloud-browser-refresh)))
   ;; In bucket root - go to bucket list
   (ecloud-browser--current-bucket
    (setq ecloud-browser--current-bucket nil)
    (setq ecloud-browser--current-prefix "")
    (ecloud-browser-refresh))
   ;; Already at bucket list
   (t (message "Already at top level"))))

(defun ecloud-browser-refresh ()
  "Refresh the current view."
  (interactive)
  (ecloud-browser--refresh-data))

(defun ecloud-browser-download ()
  "Download the file at point."
  (interactive)
  (unless ecloud-browser--current-bucket
    (user-error "No file selected - enter a bucket first"))
  (let* ((id (tabulated-list-get-id))
         (entry (tabulated-list-get-entry)))
    (when (and id entry)
      (if (string-suffix-p "/" id)
          (user-error "Cannot download a folder")
        (let* ((filename (file-name-nondirectory id))
               (default-dir (or (and (derived-mode-p 'dired-mode)
                                    (dired-current-directory))
                               default-directory))
               (local-path (read-file-name
                           (format "Download %s to: " filename)
                           default-dir
                           (expand-file-name filename default-dir))))
          (message "Downloading %s..." filename)
          (condition-case err
              (progn
                (ecloud-rpc-download ecloud-browser--current-bucket id local-path)
                (message "Downloaded %s to %s" filename local-path))
            (error (message "Download failed: %s" (error-message-string err)))))))))

(defun ecloud-browser-upload ()
  "Upload a file to the current location."
  (interactive)
  (unless ecloud-browser--current-bucket
    (user-error "Enter a bucket first"))
  (let* ((local-path (read-file-name "Upload file: " nil nil t))
         (filename (file-name-nondirectory local-path))
         (object-path (concat ecloud-browser--current-prefix filename)))
    (when (yes-or-no-p (format "Upload %s to %s/%s? "
                                filename
                                ecloud-browser--current-bucket
                                object-path))
      (message "Uploading %s..." filename)
      (condition-case err
          (progn
            (ecloud-rpc-upload ecloud-browser--current-bucket object-path local-path)
            (message "Uploaded %s" filename)
            (ecloud-browser-refresh))
        (error (message "Upload failed: %s" (error-message-string err)))))))

(defun ecloud-browser-delete ()
  "Delete the item at point."
  (interactive)
  (unless ecloud-browser--current-bucket
    (user-error "Enter a bucket first"))
  (let* ((id (tabulated-list-get-id))
         (entry (tabulated-list-get-entry))
         (name (aref entry 0)))
    (when (and id entry)
      (when (yes-or-no-p (format "Delete %s? " name))
        (message "Deleting %s..." name)
        (condition-case err
            (progn
              (ecloud-rpc-delete ecloud-browser--current-bucket id)
              (message "Deleted %s" name)
              (ecloud-browser-refresh))
          (error (message "Delete failed: %s" (error-message-string err))))))))

(defun ecloud-browser-create-folder ()
  "Create a new folder in the current location."
  (interactive)
  (unless ecloud-browser--current-bucket
    (user-error "Enter a bucket first"))
  (let* ((name (read-string "Folder name: "))
         (folder-path (concat ecloud-browser--current-prefix name)))
    (when (and name (not (string-empty-p name)))
      (message "Creating folder %s..." name)
      (condition-case err
          (progn
            (ecloud-rpc-create-folder ecloud-browser--current-bucket folder-path)
            (message "Created folder %s" name)
            (ecloud-browser-refresh))
        (error (message "Create folder failed: %s" (error-message-string err)))))))

(defun ecloud-browser-copy-path ()
  "Copy the GCS path of the item at point to kill ring."
  (interactive)
  (let* ((id (tabulated-list-get-id)))
    (when id
      (let ((path (if ecloud-browser--current-bucket
                      (format "gs://%s/%s" ecloud-browser--current-bucket id)
                    (format "gs://%s" id))))
        (kill-new path)
        (message "Copied: %s" path)))))

;;; Entry point

;;;###autoload
(defun ecloud-browse ()
  "Open the ECloud browser."
  (interactive)
  (let ((buffer (get-buffer-create "*ECloud*")))
    (with-current-buffer buffer
      (ecloud-browser-mode)
      (setq ecloud-browser--current-bucket nil)
      (setq ecloud-browser--current-prefix "")
      (setq ecloud-browser--navigation-stack nil)
      (ecloud-browser-refresh))
    (switch-to-buffer buffer)))

;;; Evil mode support

(with-eval-after-load 'evil
  (evil-set-initial-state 'ecloud-browser-mode 'motion)
  (evil-define-key 'motion ecloud-browser-mode-map
    (kbd "RET") #'ecloud-browser-enter
    (kbd "^")   #'ecloud-browser-up
    (kbd "g")   #'ecloud-browser-refresh
    (kbd "d")   #'ecloud-browser-download
    (kbd "u")   #'ecloud-browser-upload
    (kbd "D")   #'ecloud-browser-delete
    (kbd "+")   #'ecloud-browser-create-folder
    (kbd "c")   #'ecloud-browser-copy-path
    (kbd "?")   #'ecloud-browser-help
    (kbd "q")   #'quit-window))

(provide 'ecloud-browser)
;;; ecloud-browser.el ends here
