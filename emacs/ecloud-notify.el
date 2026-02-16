;;; ecloud-notify.el --- Posframe notifications for ecloud -*- lexical-binding: t; -*-

;; Copyright (C) 2024

;; Author: jing
;; Keywords: tools, cloud, notification

;;; Commentary:

;; Notification utility for ecloud using posframe.
;; Displays stacking notifications in the top-right corner.

;;; Code:

(require 'posframe)
(require 'cl-lib)

(defgroup ecloud-notify nil
  "ECloud notifications."
  :group 'ecloud)

(defcustom ecloud-notify-timeout 3
  "Default timeout for notifications in seconds."
  :type 'integer
  :group 'ecloud-notify)

(defface ecloud-notify-face
  '((t :inherit mode-line))
  "Face for ecloud notification content."
  :group 'ecloud-notify)

(defface ecloud-notify-border-face
  '((t :inherit font-lock-keyword-face))
  "Face for ecloud notification border."
  :group 'ecloud-notify)

(defvar ecloud-notify--stack nil
  "List of currently displayed notification buffers.")

(defun ecloud-notify--get-face-attribute (face attribute)
  "Get ATTRIBUTE from FACE safely, handling terminal/GUI differences."
  (let ((val (face-attribute face attribute nil)))
    (if (eq val 'unspecified)
        nil
      val)))

(defun ecloud-notify (string &optional timeout)
  "Display STRING as a notification in top-right corner.
TIMEOUT defaults to `ecloud-notify-timeout`."
  (let* ((seconds (or timeout ecloud-notify-timeout))
         (buffer-name "*ecloud-notify*")
         ;; Calculate offset: base 20px + (existing count * 70px)
         (offset-y (+ 20 (* (length ecloud-notify--stack) 70)))
         (bg-color (or (ecloud-notify--get-face-attribute 'ecloud-notify-face :background) "#282c34"))
         (fg-color (or (ecloud-notify--get-face-attribute 'ecloud-notify-face :foreground) "#abb2bf"))
         (border-color (or (ecloud-notify--get-face-attribute 'ecloud-notify-border-face :foreground) "#51afef")))

    ;; 1. Prepare content
    (with-current-buffer (get-buffer-create buffer-name)
      (erase-buffer)
      (insert (concat " " string " "))
      ;; We set local variables to style the buffer content if needed,
      ;; though posframe colors usually handle main look.
      (setq-local face-remapping-alist `((default :background ,bg-color :foreground ,fg-color :height 1.1))))

    ;; 2. Show posframe
    (posframe-show buffer-name
                   :poshandler 'posframe-poshandler-frame-top-center
                   :parent-frame (selected-frame)
                   :background-color bg-color
                   :foreground-color fg-color
                   :internal-border-width 2
                   :internal-border-color border-color
                   :y-pixel-offset offset-y
                   :x-pixel-offset -20
                   :override-parameters '((min-width . 30)
                                          (cursor-type . nil)))

    ;; 3. Add to stack
    (push buffer-name ecloud-notify--stack)

    ;; 4. Set timer to hide
    (run-with-timer seconds nil
                    (lambda (buf)
                      (when (buffer-live-p (get-buffer buf))
                        (posframe-hide buf)
                        (setq ecloud-notify--stack (delete buf ecloud-notify--stack))
                        (kill-buffer buf)))
                    buffer-name)))

(defun ecloud-notify-info (string &optional timeout)
  "Display info notification with STRING.
TIMEOUT defaults to `ecloud-notify-timeout`."
  (ecloud-notify (concat "ℹ " string) timeout))

(defun ecloud-notify-error (string &optional timeout)
  "Display error notification with STRING.
TIMEOUT defaults to `ecloud-notify-timeout`."
  (ecloud-notify (concat "✗ " string) timeout))

(defun ecloud-notify-success (string &optional timeout)
  "Display success notification with STRING.
TIMEOUT defaults to `ecloud-notify-timeout`."
  (ecloud-notify (concat "✓ " string) timeout))

(defun ecloud-notify-warning (string &optional timeout)
  "Display warning notification with STRING.
TIMEOUT defaults to `ecloud-notify-timeout`."
  (ecloud-notify (concat "⚠ " string) timeout))

(provide 'ecloud-notify)
;;; ecloud-notify.el ends here
