;;; ecloud-notify.el --- Posframe notifications for ecloud -*- lexical-binding: t; -*-

;; Copyright (C) 2024

;; Author: jing
;; Keywords: tools, cloud, notification

;;; Commentary:

;; Notification utility for ecloud using posframe.
;; Displays stacking notifications in the top-right corner.
;;
;; Features:
;;   - Per-level timeouts (info / success / warning / error).
;;     Errors default to "sticky" (no auto-dismiss) so they don't
;;     vanish before you can read them.
;;   - Length-aware scaling: long messages get more time on screen
;;     (controlled by `ecloud-notify-chars-per-second').
;;   - All notifications are appended to `*ecloud-notify-log*' so
;;     you can review past messages even after they auto-dismiss.
;;   - `M-x ecloud-notify-dismiss-all' clears all visible posframes.
;;   - `M-x ecloud-notify-show-log' opens the history buffer.

;;; Code:

(require 'posframe)
(require 'cl-lib)

(defgroup ecloud-notify nil
  "ECloud notifications."
  :group 'ecloud)

(defcustom ecloud-notify-timeout 4
  "Minimum (and default info/success) notification timeout in seconds.
Info and success messages start from this floor and may be scaled
up by `ecloud-notify-chars-per-second' for long content."
  :type 'number
  :group 'ecloud-notify)

(defcustom ecloud-notify-warning-timeout 8
  "Auto-dismiss timeout for warning notifications, in seconds."
  :type 'number
  :group 'ecloud-notify)

(defcustom ecloud-notify-error-timeout nil
  "Auto-dismiss timeout for error notifications, in seconds.
If nil (the default), error notifications are STICKY — they remain
visible until you call `ecloud-notify-dismiss-all'."
  :type '(choice (const :tag "Sticky (no auto-dismiss)" nil)
                 (number :tag "Seconds"))
  :group 'ecloud-notify)

(defcustom ecloud-notify-chars-per-second 25
  "Reading speed used to scale info/success timeouts to message length.
A 100-char message gets ceil(100/25) = 4 extra seconds beyond the
`ecloud-notify-timeout' floor."
  :type 'number
  :group 'ecloud-notify)

(defcustom ecloud-notify-log-buffer-name "*ecloud-notify-log*"
  "Name of the buffer used to log all notifications."
  :type 'string
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
  "List of currently displayed notification buffer names.")

(defun ecloud-notify--get-face-attribute (face attribute)
  "Get ATTRIBUTE from FACE safely, handling terminal/GUI differences."
  (let ((val (face-attribute face attribute nil)))
    (if (eq val 'unspecified)
        nil
      val)))

(defun ecloud-notify--default-timeout (level string)
  "Choose default auto-dismiss timeout (seconds) for LEVEL and STRING.
Returns nil for sticky notifications."
  (pcase level
    ('error ecloud-notify-error-timeout)
    ('warning ecloud-notify-warning-timeout)
    (_ (max ecloud-notify-timeout
            (ceiling (/ (float (length (or string "")))
                        (max 1 ecloud-notify-chars-per-second)))))))

(defun ecloud-notify--log (level string)
  "Append a timestamped record of LEVEL/STRING to the log buffer."
  (let ((buf (get-buffer-create ecloud-notify-log-buffer-name)))
    (with-current-buffer buf
      (unless (derived-mode-p 'special-mode)
        (let ((inhibit-read-only t))
          (special-mode)))
      (let ((inhibit-read-only t)
            (was-at-end (= (point) (point-max))))
        (save-excursion
          (goto-char (point-max))
          (insert (format "[%s] %-7s %s\n"
                          (format-time-string "%Y-%m-%d %H:%M:%S")
                          (upcase (symbol-name (or level 'info)))
                          string)))
        ;; Follow tail if user was already at end of buffer.
        (when was-at-end
          (goto-char (point-max))
          (when-let ((win (get-buffer-window buf 'visible)))
            (with-selected-window win
              (goto-char (point-max)))))))))

(defun ecloud-notify--show (level display-string log-string &optional timeout)
  "Display DISPLAY-STRING as a notification at LEVEL.
LOG-STRING (the version without emoji prefix) is also recorded in
the log buffer. TIMEOUT in seconds overrides the level default;
nil means use the level default (which may itself be nil = sticky)."
  ;; Log first so even posframe failures don't lose history.
  (ecloud-notify--log level log-string)

  (let* ((seconds (or timeout
                      (ecloud-notify--default-timeout level display-string)))
         (buffer-name (generate-new-buffer-name " *ecloud-notify*"))
         ;; Calculate offset: base 20px + (existing count * 70px)
         (offset-y (+ 20 (* (length ecloud-notify--stack) 70)))
         (bg-color (or (ecloud-notify--get-face-attribute 'ecloud-notify-face :background) "#282c34"))
         (fg-color (or (ecloud-notify--get-face-attribute 'ecloud-notify-face :foreground) "#abb2bf"))
         (border-color (or (ecloud-notify--get-face-attribute 'ecloud-notify-border-face :foreground) "#51afef")))

    ;; 1. Prepare content
    (with-current-buffer (get-buffer-create buffer-name)
      (erase-buffer)
      (insert (concat " " display-string " "))
      (setq-local face-remapping-alist
                  `((default :background ,bg-color :foreground ,fg-color :height 1.1))))

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

    ;; 4. Set timer to hide (skipped when sticky)
    (when (and seconds (> seconds 0))
      (run-with-timer
       seconds nil
       (lambda (buf)
         (when (buffer-live-p (get-buffer buf))
           (posframe-hide buf)
           (setq ecloud-notify--stack (delete buf ecloud-notify--stack))
           (kill-buffer buf)))
       buffer-name))

    buffer-name))

(defun ecloud-notify (string &optional timeout)
  "Display STRING as a generic info-level notification.
TIMEOUT overrides the level default; nil uses
`ecloud-notify-timeout' scaled by message length."
  (ecloud-notify--show 'info string string timeout))

(defun ecloud-notify-info (string &optional timeout)
  "Display info notification with STRING. TIMEOUT overrides default."
  (ecloud-notify--show 'info (concat "ℹ " string) string timeout))

(defun ecloud-notify-error (string &optional timeout)
  "Display error notification with STRING.
By default errors are STICKY; pass TIMEOUT to override, or customise
`ecloud-notify-error-timeout'."
  (ecloud-notify--show 'error (concat "✗ " string) string timeout))

(defun ecloud-notify-success (string &optional timeout)
  "Display success notification with STRING. TIMEOUT overrides default."
  (ecloud-notify--show 'success (concat "✓ " string) string timeout))

(defun ecloud-notify-warning (string &optional timeout)
  "Display warning notification with STRING. TIMEOUT overrides default."
  (ecloud-notify--show 'warning (concat "⚠ " string) string timeout))

;;;###autoload
(defun ecloud-notify-dismiss-all ()
  "Dismiss all currently visible ecloud notifications.
Useful for clearing sticky error notifications once you've read them.

Robust against orphans left over from a previous `unload-feature' /
`reload-ecloud': the stack variable gets makunbound and re-inited to
nil, but the posframe child frames persist on screen. We sweep by
buffer-name prefix and by child-frame parent so those still get
cleaned up."
  (interactive)
  (let ((killed 0))
    ;; Child frames showing one of our notify buffers — delete frame
    ;; AND kill the buffer. Covers the post-reload orphan case.
    (dolist (frame (frame-list))
      (when (frame-parameter frame 'parent-frame)
        (let* ((win (frame-root-window frame))
               (buf (and win (window-buffer win))))
          (when (and buf (buffer-live-p buf)
                     (string-prefix-p " *ecloud-notify*" (buffer-name buf)))
            (ignore-errors (kill-buffer buf))
            (ignore-errors (delete-frame frame))
            (cl-incf killed)))))
    ;; Leftover notify buffers not currently shown in any child frame.
    (dolist (buf (buffer-list))
      (when (and (buffer-live-p buf)
                 (string-prefix-p " *ecloud-notify*" (buffer-name buf)))
        (ignore-errors (kill-buffer buf))
        (cl-incf killed)))
    (setq ecloud-notify--stack nil)
    (when (called-interactively-p 'interactive)
      (message "Dismissed %d ecloud notification(s)" killed))))

(defun ecloud-notify-unload-function ()
  "Called by `unload-feature' before this feature is torn down.
Without this hook, sticky notifications outlive their bookkeeping
and become undismissable until the freshly-loaded
`ecloud-notify-dismiss-all' cleans them up by buffer-name sweep."
  (ignore-errors (ecloud-notify-dismiss-all))
  nil)

;;;###autoload
(defun ecloud-notify-show-log ()
  "Open the notification history log buffer."
  (interactive)
  (let ((buf (get-buffer-create ecloud-notify-log-buffer-name)))
    (with-current-buffer buf
      (unless (derived-mode-p 'special-mode)
        (let ((inhibit-read-only t))
          (special-mode)))
      (goto-char (point-max)))
    (pop-to-buffer buf)))

;;;###autoload
(defun ecloud-notify-clear-log ()
  "Erase the contents of the notification log buffer."
  (interactive)
  (when-let ((buf (get-buffer ecloud-notify-log-buffer-name)))
    (with-current-buffer buf
      (let ((inhibit-read-only t))
        (erase-buffer)))
    (when (called-interactively-p 'interactive)
      (message "Cleared ecloud notification log"))))

(provide 'ecloud-notify)
;;; ecloud-notify.el ends here
