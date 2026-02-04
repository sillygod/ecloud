;;; reload-ecloud.el --- Reload all ECloud modules

;; This script reloads all ECloud modules to pick up changes

(defun reload-ecloud ()
  "Reload all ECloud modules."
  (interactive)
  (let ((ecloud-path (file-name-directory (or load-file-name buffer-file-name))))
    (message "Reloading ECloud modules from %s..." ecloud-path)
    
    ;; Unload features to force reload
    (unload-feature 'ecloud-services t)
    (unload-feature 'ecloud-cloud-run t)
    (unload-feature 'ecloud-scheduler t)
    (unload-feature 'ecloud-k8s t)
    (unload-feature 'ecloud-rpc t)
    (unload-feature 'ecloud-account-manager t)
    (unload-feature 'ecloud-ws t)
    (unload-feature 'ecloud-sql t)
    (unload-feature 'ecloud-compute t)
    (unload-feature 'ecloud-ips t)
    (unload-feature 'ecloud-gar t)
    (unload-feature 'ecloud-browser t)
    (unload-feature 'ecloud-commands t)
    (unload-feature 'ecloud-notify t)
    (unload-feature 'ecloud-transient t)
    (unload-feature 'ecloud t)
    
    ;; Reload in correct order
    (load-file (concat ecloud-path "emacs/ecloud-notify.el"))
    (load-file (concat ecloud-path "emacs/ecloud-rpc.el"))
    (load-file (concat ecloud-path "emacs/ecloud-account-manager.el"))
    (load-file (concat ecloud-path "emacs/ecloud-ws.el"))
    (load-file (concat ecloud-path "emacs/ecloud-browser.el"))
    (load-file (concat ecloud-path "emacs/ecloud-commands.el"))
    (load-file (concat ecloud-path "emacs/ecloud-gar.el"))
    (load-file (concat ecloud-path "emacs/ecloud-ips.el"))
    (load-file (concat ecloud-path "emacs/ecloud-compute.el"))
    (load-file (concat ecloud-path "emacs/ecloud-cloud-run.el"))
    (load-file (concat ecloud-path "emacs/ecloud-scheduler.el"))
    (load-file (concat ecloud-path "emacs/ecloud-sql.el"))
    (load-file (concat ecloud-path "emacs/ecloud-services.el"))
    (load-file (concat ecloud-path "emacs/ecloud-k8s.el"))
    (load-file (concat ecloud-path "emacs/ecloud-transient.el"))
    (load-file (concat ecloud-path "emacs/ecloud.el"))
    
    ;; Force re-setup Evil mode bindings for existing K8s buffers
    (when (featurep 'evil)
      (dolist (buffer (buffer-list))
        (with-current-buffer buffer
          (when (eq major-mode 'ecloud-k8s-mode)
            (message "Re-setting up Evil bindings for buffer %s" (buffer-name))
            ;; Re-apply Evil mode bindings
            (evil-set-initial-state 'ecloud-k8s-mode 'motion)
            (evil-define-key 'motion ecloud-k8s-mode-map
              (kbd "RET") #'ecloud-k8s-enter
              (kbd "p") #'ecloud-k8s-switch-to-pods
              (kbd "s") #'ecloud-k8s-switch-to-services
              (kbd "i") #'ecloud-k8s-switch-to-ingresses
              (kbd "d") #'ecloud-k8s-switch-to-deployments
              (kbd "h") #'ecloud-k8s-helm-list
              (kbd "n") #'ecloud-k8s-switch-to-namespaces
              (kbd "K") #'ecloud-k8s-view-kind
              (kbd "N") #'ecloud-k8s-select-namespace
              (kbd "=") #'ecloud-k8s-set-pod-limit
              (kbd "T") #'ecloud-k8s-toggle-pod-limit
              (kbd "y") #'ecloud-k8s-view-yaml
              (kbd "l") #'ecloud-k8s-view-logs
              (kbd "L") #'ecloud-k8s-stream-logs
              (kbd "S") #'ecloud-k8s-scale-deployment
              (kbd "e") #'ecloud-k8s-pod-exec
              (kbd "A") #'ecloud-k8s-apply-manifest
              (kbd "M") #'ecloud-k8s-show-metrics
              (kbd "r") #'ecloud-k8s-refresh
              (kbd "Q") #'ecloud-k8s-disconnect
              (kbd "?") #'ecloud-k8s-help
              (kbd "q") #'quit-window)
            ;; Force Evil to re-initialize the buffer
            (when (fboundp 'evil-motion-state)
              (evil-motion-state))))))
    
    (message "ECloud modules reloaded successfully!")
    (message "Key binding changes:")
    (message "  - K8s: 'k' is now free for Evil motion, use 'K' for view-kind")
    (message "  - Helm: Now respects namespace filter")
    (message "  - Account Manager: Evil mode support added")
    (message "")
    (message "If 'k' still triggers view-kind, close and reopen the K8s buffer.")))

;; Run it
(reload-ecloud)

(provide 'reload-ecloud)
