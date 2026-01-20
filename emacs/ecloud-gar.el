;;; ecloud-gar.el --- Artifact Registry browser for ecloud -*- lexical-binding: t; -*-

;; Copyright (C) 2024

;; Author: ecloud
;; Keywords: tools, cloud, docker

;;; Commentary:

;; A browser for Google Artifact Registry (GAR) using tabulated-list-mode.
;; Supports browsing Repositories -> Packages -> Tags.

;;; Code:

(require 'tabulated-list)
(require 'ecloud-rpc)

;;; Customization

(defgroup ecloud-gar nil
  "ECloud Artifact Registry settings."
  :group 'ecloud)

(defcustom ecloud-gar-default-location "asia-east1"
  "Default location/region for listing repositories."
  :type 'string
  :group 'ecloud-gar)

(defface ecloud-gar-repo-face
  '((t :inherit font-lock-constant-face :weight bold))
  "Face for repository names."
  :group 'ecloud-gar)

(defface ecloud-gar-package-face
  '((t :inherit font-lock-function-name-face))
  "Face for package (image) names."
  :group 'ecloud-gar)

(defface ecloud-gar-tag-face
  '((t :inherit default))
  "Face for tag names."
  :group 'ecloud-gar)

;;; Buffer-local state

(defvar-local ecloud-gar--location nil
  "Current location (region).")

(defvar-local ecloud-gar--current-repo nil
  "Current repository full name.")

(defvar-local ecloud-gar--current-package nil
  "Current package (image) full name.")

(defvar-local ecloud-gar--navigation-stack nil
  "Stack of (repo . package) for back navigation.")

;;; Helper functions

(defun ecloud-gar--confirm-delete (name type)
  "Ask for confirmation to delete NAME of TYPE.
Returns t if confirmed."
  (let ((confirm (read-string (format "Type 'delete' to confirm deleting %s '%s': " type name))))
    (string-equal confirm "delete")))

;;; Data fetching

(defun ecloud-gar--fetch-repos ()
  "Fetch repositories for ecloud-gar--location."
  (let* ((response (ecloud-rpc-gar-list-repos ecloud-gar--location))
         (repos (plist-get response :repositories)))
    (mapcar
     (lambda (repo)
       (let ((name (plist-get repo :name))
             (full-name (plist-get repo :full_name))
             (format (plist-get repo :format))
             (desc (plist-get repo :description)))
         (list full-name
               (vector
                (propertize name 'face 'ecloud-gar-repo-face)
                format
                desc))))
     repos)))

(require 'url-util)

(defun ecloud-gar--fetch-packages ()
  "Fetch packages for ecloud-gar--current-repo."
  (let* ((response (ecloud-rpc-gar-list-packages ecloud-gar--current-repo))
         (packages (plist-get response :packages)))
    (mapcar
     (lambda (pkg)
       (let ((name (plist-get pkg :name))
             (full-name (plist-get pkg :full_name))
             (uri (plist-get pkg :uri)))
         (list full-name ; Use full resource name as ID
               (vector
                (propertize (url-unhex-string name) 'face 'ecloud-gar-package-face)
                uri
                ""))))
     packages)))

(defun ecloud-gar--fetch-tags ()
  "Fetch tags for ecloud-gar--current-package."
  (let* ((response (ecloud-rpc-gar-list-tags ecloud-gar--current-package))
         (tags (plist-get response :tags)))
    (mapcar
     (lambda (tag)
       (let ((name (plist-get tag :name))
             (full-name (plist-get tag :full_name))
             (digest (plist-get tag :digest)))
         (list full-name ; Use full tag resource name as ID for deletion
               (vector
                (propertize name 'face 'ecloud-gar-tag-face)
                digest))))
     tags)))

(defun ecloud-gar--refresh-data ()
  "Refresh data based on current state."
  (cond
   (ecloud-gar--current-package
    (setq tabulated-list-format [("Tag" 20 t) ("Digest" 60 t)])
    (setq tabulated-list-entries #'ecloud-gar--fetch-tags))
   (ecloud-gar--current-repo
    (setq tabulated-list-format [("Image" 30 t) ("URI" 50 nil) ("" 10 nil)])
    (setq tabulated-list-entries #'ecloud-gar--fetch-packages))
   (t
    (setq tabulated-list-format [("Repository" 30 t) ("Format" 10 t) ("Description" 40 nil)])
    (setq tabulated-list-entries #'ecloud-gar--fetch-repos)))
  (tabulated-list-init-header))

(defun ecloud-gar--update-header ()
  "Update header line."
  ;; Show location path in mode-name
  (setq mode-name
        (format "GAR [%s] %s%s"
                (or ecloud-gar--location "?")
                (if ecloud-gar--current-repo
                    (concat " > " (file-name-nondirectory ecloud-gar--current-repo))
                  "")
                (if ecloud-gar--current-package
                    (concat " > " (url-unhex-string (file-name-nondirectory ecloud-gar--current-package)))
                  "")))
  (force-mode-line-update))

;;; Interactive commands

(defun ecloud-gar-enter ()
  "Enter the item at point."
  (interactive)
  (let ((id (tabulated-list-get-id)))
    (when id
      (cond
       (ecloud-gar--current-package
        ;; At tag level - maybe show details or pull?
        (message "Selected tag: %s" id))
       (ecloud-gar--current-repo
        ;; Enter package -> tags
        (push (cons ecloud-gar--current-repo nil) ecloud-gar--navigation-stack)
        (setq ecloud-gar--current-package id)
        (ecloud-gar-refresh))
       (t
        ;; Enter repo -> packages
        (push (cons nil nil) ecloud-gar--navigation-stack)
        (setq ecloud-gar--current-repo id)
        (ecloud-gar-refresh))))))

(defun ecloud-gar-up ()
  "Go up one level."
  (interactive)
  (cond
   (ecloud-gar--current-package
    (setq ecloud-gar--current-package nil)
    (ecloud-gar-refresh))
   (ecloud-gar--current-repo
    (setq ecloud-gar--current-repo nil)
    (ecloud-gar-refresh))
   (t
    (message "Already at top level"))))

(defun ecloud-gar-refresh ()
  "Refresh view."
  (interactive)
  (ecloud-gar--refresh-data)
  (ecloud-gar--update-header)
  (tabulated-list-print t))

(defun ecloud-gar-pull ()
  "Pull docker image."
  (interactive)
  (let ((uri nil))
    ;; Determine URI based on selection
    (cond
     (ecloud-gar--current-package
      ;; Pull specific tag
      (let ((entry (tabulated-list-get-entry))
            (id (tabulated-list-get-id)))
        ;; Need to reconstruct URI since ID is version resource
        ;; But wait, list-tags returns name (tag name) and version (tag resource)
        ;; We need the image URI + tag name.
        ;; For simplicity, let's ask user to confirm full URI
        (let* ((tag-name (aref entry 0)) ;; Propertized string
               (clean-tag (substring-no-properties tag-name))
               ;; Get URI from parent package (we need to fetch it again or store it)
               ;; Hack: construct it from package name.
               ;; Package name: projects/.../packages/image-name
               (pkg-parts (split-string ecloud-gar--current-package "/"))
               (image-name (car (last pkg-parts)))
               (repo-parts (split-string ecloud-gar--current-repo "/"))
               (repo-name (car (last repo-parts)))
               (location ecloud-gar--location)
               (project (nth 1 pkg-parts)) ;; projects/PROJECT/locations/...
               (base-uri (format "%s-docker.pkg.dev/%s/%s/%s"
                                location project repo-name (url-unhex-string image-name))))
          
          (setq uri (format "%s:%s" base-uri clean-tag)))))
     
     (ecloud-gar--current-repo
      ;; Pull latest from package
      (let ((entry (tabulated-list-get-entry)))
        (setq uri (aref entry 1)))))
    
    (when (and uri (yes-or-no-p (format "Pull %s? " uri)))
      (message "Pulling %s..." uri)
      (condition-case err
          (progn
            (ecloud-rpc-gar-pull uri)
            (message "Pulled %s" uri))
        (error (message "Pull failed: %s" (error-message-string err)))))))

(defun ecloud-gar-tag ()
  "Create a new tag for the current image version."
  (interactive)
  (unless ecloud-gar--current-package
    (user-error "Not in a package view"))
  (let* ((entry (tabulated-list-get-entry))
         (digest (aref entry 1))
         (version-resource (format "%s/versions/%s" ecloud-gar--current-package digest))
         (new-tag (read-string "New tag name: ")))
    (when (and new-tag (not (string-empty-p new-tag)))
      (message "Creating tag %s..." new-tag)
      (condition-case err
          (progn
            (ecloud-rpc-gar-create-tag ecloud-gar--current-package new-tag version-resource)
            (message "Created tag %s" new-tag)
            (ecloud-gar-refresh))
        (error (message "Tag creation failed: %s" (error-message-string err)))))))

(defun ecloud-gar-delete ()
  "Delete item with confirmation."
  (interactive)
  (let ((id (tabulated-list-get-id))
        (entry (tabulated-list-get-entry)))
    (when id
      (cond
       (ecloud-gar--current-package
        ;; Delete Tag
        (let ((tag-name (substring-no-properties (aref entry 0))))
          (when (ecloud-gar--confirm-delete tag-name "tag")
            (message "Deleting tag %s..." tag-name)
            (ecloud-rpc-gar-delete-tag id) ;; id is full version resource? wait, gar_delete_tag expects tag resource name
            ;; My backend gar_delete_tag takes tag resource name.
            ;; In fetch-tags, I used 'version' as ID. Let's fix fetch-tags to use tag full name as ID.
            ;; Wait, `list_tags` returns TagInfo(name=tag.name (full), version=tag.version).
            ;; Let's check ecloud-gar--fetch-tags implementation.
            ;; OK, let's fix ID in fetch-tags.
            (ecloud-gar-refresh))))
       (ecloud-gar--current-repo
        ;; Delete Package (Image)
        (let ((pkg-name (substring-no-properties (aref entry 0))))
          (when (ecloud-gar--confirm-delete pkg-name "image")
             (message "Deleting image %s..." pkg-name)
             ;; id is package full name
             (ecloud-rpc-gar-delete-package id)
             (ecloud-gar-refresh))))
       (t
        (message "Cannot delete repository via Emacs"))))))

;;; Mode definition

(defun ecloud-gar-help ()
  "Show help for ecloud-gar-mode."
  (interactive)
  (message "GAR Keys: [RET]Enter [^]Up [g]Refresh [P]Pull [D]Delete [t]Tag [?]Help [q]Quit"))

(defvar ecloud-gar-mode-map nil
  "Keymap for `ecloud-gar-mode'.")

(unless ecloud-gar-mode-map
  (setq ecloud-gar-mode-map (make-sparse-keymap))
  (define-key ecloud-gar-mode-map (kbd "RET") #'ecloud-gar-enter)
  (define-key ecloud-gar-mode-map (kbd "^") #'ecloud-gar-up)
  (define-key ecloud-gar-mode-map (kbd "g") #'ecloud-gar-refresh)
  (define-key ecloud-gar-mode-map (kbd "P") #'ecloud-gar-pull)
  (define-key ecloud-gar-mode-map (kbd "D") #'ecloud-gar-delete)
  (define-key ecloud-gar-mode-map (kbd "t") #'ecloud-gar-tag)
  (define-key ecloud-gar-mode-map (kbd "+") #'ecloud-gar-tag)
  (define-key ecloud-gar-mode-map (kbd "?") #'ecloud-gar-help)
  (define-key ecloud-gar-mode-map (kbd "q") #'quit-window))

(define-derived-mode ecloud-gar-mode tabulated-list-mode "ECloud-GAR"
  "Major mode for browsing Artifact Registry."
  (setq tabulated-list-padding 2)
  (add-hook 'tabulated-list-revert-hook #'ecloud-gar--refresh-data nil t))

;;; Evil mode
(with-eval-after-load 'evil
  (evil-set-initial-state 'ecloud-gar-mode 'motion)
  (evil-define-key 'motion ecloud-gar-mode-map
    (kbd "RET") #'ecloud-gar-enter
    (kbd "^")   #'ecloud-gar-up
    (kbd "g")   #'ecloud-gar-refresh
    (kbd "P")   #'ecloud-gar-pull
    (kbd "D")   #'ecloud-gar-delete
    (kbd "t")   #'ecloud-gar-tag
    (kbd "+")   #'ecloud-gar-tag
    (kbd "?")   #'ecloud-gar-help
    (kbd "q")   #'quit-window))

;;; Entry point

;;;###autoload
(defun ecloud-gar-browse (&optional location)
  "Open GAR browser.
Using 'all' (default) lists repositories from all locations."
  (interactive)
  (let ((location (or location "all"))
        (buffer (get-buffer-create "*ECloud-GAR*")))
    (with-current-buffer buffer
      (ecloud-gar-mode)
      (setq ecloud-gar--location location)
      (setq ecloud-gar--current-repo nil)
      (setq ecloud-gar--current-package nil)
      (setq ecloud-gar--navigation-stack nil)
      (ecloud-gar-refresh))
    (switch-to-buffer buffer)))

(provide 'ecloud-gar)
