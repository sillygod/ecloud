;;; ecloud-ips.el --- Static IP address browser for ecloud -*- lexical-binding: t; -*-

;; Copyright (C) 2024

;; Author: jing
;; Keywords: tools, cloud, compute

;;; Commentary:

;; A browser for Google Cloud static IP addresses using tabulated-list-mode.
;; Lists both regional and global addresses.

;;; Code:

(require 'tabulated-list)
(require 'ecloud-rpc)

;;; Customization

(defgroup ecloud-ips nil
  "ECloud IP address settings."
  :group 'ecloud)

(defface ecloud-ips-name-face
  '((t :inherit font-lock-function-name-face))
  "Face for IP address names."
  :group 'ecloud-ips)

(defface ecloud-ips-address-face
  '((t :inherit font-lock-constant-face :weight bold))
  "Face for IP addresses."
  :group 'ecloud-ips)

(defface ecloud-ips-reserved-face
  '((t :inherit font-lock-type-face))
  "Face for RESERVED status."
  :group 'ecloud-ips)

(defface ecloud-ips-in-use-face
  '((t :inherit font-lock-string-face))
  "Face for IN_USE status."
  :group 'ecloud-ips)

(defface ecloud-ips-region-face
  '((t :inherit font-lock-keyword-face))
  "Face for region names."
  :group 'ecloud-ips)

;;; Buffer-local state

(defvar-local ecloud-ips--cached-regions nil
  "Cached list of regions for completion.")

;;; Helper functions

(defun ecloud-ips--format-status (status)
  "Format STATUS with appropriate face."
  (cond
   ((string= status "RESERVED")
    (propertize status 'face 'ecloud-ips-reserved-face))
   ((string= status "IN_USE")
    (propertize status 'face 'ecloud-ips-in-use-face))
   (t status)))

;;; Data fetching

(defun ecloud-ips--status-priority (status)
  "Return sort priority for STATUS. Lower number = higher priority."
  (cond
   ((string= status "IN_USE") 0)
   ((string= status "RESERVED") 1)
   ((string= status "Ephemeral") 2)
   (t 3)))

(defun ecloud-ips--fetch-addresses ()
  "Fetch and return address list entries for tabulated-list."
  (let* ((response (ecloud-rpc-compute-list-addresses))
         (addresses (plist-get response :addresses))
         (entries
          (mapcar
           (lambda (addr)
             (let ((name (plist-get addr :name))
                   (address (plist-get addr :address))
                   (region (plist-get addr :region))
                   (status (plist-get addr :status))
                   (addr-type (plist-get addr :address_type))
                   (users-short (plist-get addr :users_short)))
               (list address  ; Use address as ID for uniqueness
                     (vector
                      (propertize name 'face 'ecloud-ips-name-face)
                      (propertize (or address "") 'face 'ecloud-ips-address-face)
                      (or addr-type "")
                      (propertize (or region "") 'face 'ecloud-ips-region-face)
                      (ecloud-ips--format-status (or status ""))
                      (or users-short "")))))
           addresses)))
    ;; Sort by status priority first, then by name alphabetically
    (sort entries
          (lambda (a b)
            (let ((status-a (substring-no-properties (aref (cadr a) 4)))
                  (status-b (substring-no-properties (aref (cadr b) 4)))
                  (name-a (substring-no-properties (aref (cadr a) 0)))
                  (name-b (substring-no-properties (aref (cadr b) 0))))
              (let ((priority-a (ecloud-ips--status-priority status-a))
                    (priority-b (ecloud-ips--status-priority status-b)))
                (if (= priority-a priority-b)
                    (string< name-a name-b)
                  (< priority-a priority-b))))))))

(defun ecloud-ips--refresh-data ()
  "Refresh the tabulated list data."
  (setq tabulated-list-format
        [("Name" 30 t)
         ("Address" 16 nil)
         ("Type" 10 t)
         ("Region" 18 t)
         ("Status" 10 t)
         ("Users" 25 nil)])
  (setq tabulated-list-entries #'ecloud-ips--fetch-addresses)
  (tabulated-list-init-header)
  (tabulated-list-print t))

;;; Interactive commands

(defun ecloud-ips-refresh ()
  "Refresh the IP addresses view."
  (interactive)
  (ecloud-ips--refresh-data)
  (message "IP addresses refreshed"))

(defun ecloud-ips-copy-address ()
  "Copy the IP address at point to kill ring."
  (interactive)
  (let ((entry (tabulated-list-get-entry)))
    (when entry
      (let ((address (substring-no-properties (aref entry 1))))
        (if (string-empty-p address)
            (message "No IP address available (may still be reserving)")
          (kill-new address)
          (message "Copied: %s" address))))))

(defun ecloud-ips-copy-name ()
  "Copy the name of the IP address at point to kill ring."
  (interactive)
  (let ((id (tabulated-list-get-id)))
    (when id
      (kill-new id)
      (message "Copied: %s" id))))

(defun ecloud-ips--get-regions ()
  "Get list of regions, using cache if available."
  (unless ecloud-ips--cached-regions
    (condition-case err
        (let ((response (ecloud-rpc-compute-list-regions)))
          (setq ecloud-ips--cached-regions (plist-get response :regions)))
      (error
       (message "Failed to fetch regions: %s" (error-message-string err))
       ;; Fallback to common regions
       (setq ecloud-ips--cached-regions
             '("us-central1" "us-east1" "us-west1"
               "europe-west1" "asia-east1" "asia-northeast1")))))
  ecloud-ips--cached-regions)

(defun ecloud-ips-reserve ()
  "Reserve a new external static IP address."
  (interactive)
  (let* ((regions (ecloud-ips--get-regions))
         (region (completing-read "Region: " regions nil t))
         (name (read-string "IP address name: ")))
    (when (string-empty-p name)
      (user-error "Name cannot be empty"))
    (when (yes-or-no-p (format "Reserve external IP '%s' in %s? " name region))
      (message "Reserving IP address %s in %s..." name region)
      (condition-case err
          (let ((result (ecloud-rpc-compute-reserve-address region name)))
            (message "Reserved IP: %s (%s)"
                     (plist-get result :name)
                     (plist-get result :address))
            (ecloud-ips-refresh))
        (error (message "Reserve failed: %s" (error-message-string err)))))))

(defun ecloud-ips-help ()
  "Show help for ecloud-ips-mode."
  (interactive)
  (message "IP Keys: [r]Refresh [+]Reserve [c]CopyIP [w]CopyName [?]Help [q]Quit | Type: EXTERNAL/INTERNAL | Status: Ephemeral/RESERVED/IN_USE"))

;;; Mode definition

(defvar ecloud-ips-mode-map nil
  "Keymap for `ecloud-ips-mode'.")

(unless ecloud-ips-mode-map
  (setq ecloud-ips-mode-map (make-sparse-keymap))
  (define-key ecloud-ips-mode-map (kbd "r") #'ecloud-ips-refresh)
  (define-key ecloud-ips-mode-map (kbd "+") #'ecloud-ips-reserve)
  (define-key ecloud-ips-mode-map (kbd "c") #'ecloud-ips-copy-address)
  (define-key ecloud-ips-mode-map (kbd "w") #'ecloud-ips-copy-name)
  (define-key ecloud-ips-mode-map (kbd "?") #'ecloud-ips-help)
  (define-key ecloud-ips-mode-map (kbd "q") #'quit-window))

(define-derived-mode ecloud-ips-mode tabulated-list-mode "ECloud-IPs"
  "Major mode for browsing Google Cloud static IP addresses.

\\{ecloud-ips-mode-map}"
  (setq tabulated-list-padding 2)
  (add-hook 'tabulated-list-revert-hook #'ecloud-ips--refresh-data nil t)
  ;; Force Evil Motion state if available
  (when (fboundp 'evil-motion-state)
    (evil-motion-state)))

;;; Evil mode support

(with-eval-after-load 'evil
  (evil-set-initial-state 'ecloud-ips-mode 'motion)
  (evil-define-key* 'motion ecloud-ips-mode-map
    (kbd "r")   #'ecloud-ips-refresh
    (kbd "+")   #'ecloud-ips-reserve
    (kbd "c")   #'ecloud-ips-copy-address
    (kbd "w")   #'ecloud-ips-copy-name
    (kbd "?")   #'ecloud-ips-help
    (kbd "q")   #'quit-window))

;;; Entry point

;;;###autoload
(defun ecloud-ips-list ()
  "Open the ECloud IP addresses browser."
  (interactive)
  (let ((buffer (get-buffer-create "*ECloud-IPs*")))
    (with-current-buffer buffer
      (ecloud-ips-mode)
      (setq ecloud-ips--cached-regions nil)
      (ecloud-ips-refresh))
    (switch-to-buffer buffer)))

(provide 'ecloud-ips)
;;; ecloud-ips.el ends here
