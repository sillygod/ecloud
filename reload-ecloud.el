;;; reload-ecloud.el --- Reload all ECloud modules -*- lexical-binding: t; -*-

;; Reloads ECloud modules in-place to pick up code changes without
;; restarting Emacs.
;;
;; Usage:
;;   M-x reload-ecloud           ; reload everything safe to reload
;;   C-u M-x reload-ecloud       ; also reload ecloud-account-manager
;;                                 (this orphans currently-running
;;                                  server subprocesses — they keep
;;                                  running but the in-memory registry
;;                                  is lost. Restart them via
;;                                  `ecloud-account-list-processes'.)
;;
;; Add new modules to `reload-ecloud-modules' below — load and unload
;; share that single ordered list, so there's no second place to edit.

;;; Code:

(require 'cl-lib)
(require 'subr-x)  ;; string-remove-suffix

(defvar reload-ecloud-modules
  ;; Ordered: each entry must be loadable once all earlier entries are.
  ;; This is also used in reverse for unload.
  '(ecloud-notify
    ecloud-rpc
    ecloud-account-manager
    ecloud-ws
    ecloud-browser
    ecloud-commands
    ecloud-gar
    ecloud-ips
    ecloud-compute
    ecloud-cloud-run
    ecloud-scheduler
    ecloud-sql
    ecloud-services
    ecloud-secrets
    ecloud-k8s
    ecloud-transient
    ecloud)
  "Ordered list of ECloud feature symbols.")

(defvar reload-ecloud-stateful-modules
  '(ecloud-account-manager)
  "Modules whose in-memory state should survive a reload.
Skipped unless `reload-ecloud' is called with a prefix argument.
Unloading these orphans long-lived subprocesses or connections.")

(defun reload-ecloud--module-path (base feature)
  "Resolve absolute .el path for FEATURE.
Probes two layouts in order:
  1. <BASE>/emacs/<feature>.el  (source-repo layout)
  2. <BASE>/<feature>.el        (flattened build-dir layout, e.g. straight.el)
Falls back to `locate-library' if neither exists under BASE.
Returns nil if the module cannot be found anywhere."
  (let* ((name (symbol-name feature))
         (candidates (list
                      (expand-file-name (format "emacs/%s.el" name) base)
                      (expand-file-name (format "%s.el" name) base))))
    (or (cl-find-if #'file-exists-p candidates)
        ;; Last-resort: trust load-path. locate-library may return
        ;; .elc; convert back to .el so we read fresh source.
        (let ((found (locate-library name)))
          (when found
            (let ((el (if (string-suffix-p ".elc" found)
                          (concat (string-remove-suffix ".elc" found) ".el")
                        found)))
              (and (file-exists-p el) el)))))))

(defun reload-ecloud--purge-stale-elc (el-path)
  "Delete the .elc next to EL-PATH if it exists.
Stops Emacs from picking up stale bytecode when other code calls
`require' for the just-reloaded feature."
  (let ((elc (concat el-path "c")))
    (when (file-exists-p elc)
      (condition-case _ (delete-file elc) (error nil)))))

(defun reload-ecloud--safe-unload (feature)
  "Unload FEATURE, swallowing the not-loaded-yet case."
  (when (featurep feature)
    (condition-case err
        (progn (unload-feature feature t) t)
      (error
       (message "reload-ecloud: failed to unload %s: %s"
                feature (error-message-string err))
       nil))))

(defun reload-ecloud--safe-load (path)
  "Load PATH, returning t on success."
  (condition-case err
      (progn (load-file path) t)
    (error
     (message "reload-ecloud: failed to load %s: %s"
              (file-name-nondirectory path) (error-message-string err))
     nil)))

;;;###autoload
(defun reload-ecloud (&optional full)
  "Reload ECloud modules in-place.
Without a prefix arg, skip modules listed in
`reload-ecloud-stateful-modules' to preserve their state (running
subprocesses, account registry, etc.).
With prefix arg FULL, reload everything — this leaks any
subprocesses managed by `ecloud-account-manager'; you'll need to
restart accounts manually afterwards."
  (interactive "P")
  (let* ((base (file-name-directory (or load-file-name buffer-file-name
                                        default-directory)))
         (modules (if full
                      reload-ecloud-modules
                    (cl-remove-if (lambda (m)
                                    (memq m reload-ecloud-stateful-modules))
                                  reload-ecloud-modules)))
         (skipped (cl-set-difference reload-ecloud-modules modules))
         (ws-was-connected (and (boundp 'ecloud-ws-client)
                                ecloud-ws-client))
         (unloaded 0)
         (loaded 0)
         (failed nil))

    (message "reload-ecloud: reloading %d module(s) from %s%s"
             (length modules) base
             (if skipped (format " (skipping %s)" skipped) ""))

    ;; 1. Drop the websocket cleanly. ecloud.el's deferred timer will
    ;; bring it back after reload if `ecloud-ws-auto-connect' is set.
    (when (and ws-was-connected (fboundp 'ecloud-ws-disconnect))
      (condition-case err
          (ecloud-ws-disconnect)
        (error (message "reload-ecloud: ws disconnect failed: %s"
                        (error-message-string err)))))

    ;; 2. Unload in reverse dependency order.
    (dolist (feature (reverse modules))
      (when (reload-ecloud--safe-unload feature)
        (cl-incf unloaded)))

    ;; 3. Load in forward dependency order.
    (dolist (feature modules)
      (let ((path (reload-ecloud--module-path base feature)))
        (cond
         ((null path)
          (message "reload-ecloud: could not locate source for %s" feature)
          (push feature failed))
         (t
          (reload-ecloud--purge-stale-elc path)
          (if (reload-ecloud--safe-load path)
              (cl-incf loaded)
            (push feature failed))))))

    (message "reload-ecloud: unloaded %d, loaded %d%s%s"
             unloaded loaded
             (if failed (format ", FAILED: %s" (nreverse failed)) "")
             (if skipped " (stateful modules preserved)" ""))

    (when (and full (memq 'ecloud-account-manager modules))
      (message
       "reload-ecloud: account-manager was reloaded — running server subprocesses are now orphaned. Use `ecloud-account-list-processes' to restart."))))

;; Run it when this file is loaded directly (preserves the old UX
;; of "load reload-ecloud.el to reload"). Interactive use should
;; prefer M-x reload-ecloud.
(when load-file-name
  (reload-ecloud))

(provide 'reload-ecloud)
;;; reload-ecloud.el ends here
