;;; ecloud-services.el --- GCP Service/API management for ecloud -*- lexical-binding: t; -*-

;; Copyright (C) 2024

;; Author: ecloud
;; Keywords: tools, cloud, gcp

;;; Commentary:

;; Provides interactive commands for managing GCP services/APIs.
;; Allows listing, enabling, and disabling services with fuzzy completion.

;;; Code:

(require 'ecloud-rpc)
(require 'ecloud-notify)
(require 'ecloud-ws)  ; For WebSocket streaming support

;;; Interactive commands

;;;###autoload
(defun ecloud-services-list (&optional filter-state)
  "List GCP services/APIs in the project with streaming support.
FILTER-STATE can be \"ENABLED\", \"DISABLED\", or nil for all.
Without prefix argument, shows only ENABLED services (fast).
With prefix argument, prompt for filter state."
  (interactive
   (list (if current-prefix-arg
             (let ((choice (completing-read "Filter by state: "
                                          '("ENABLED" "DISABLED" "All")
                                          nil t nil nil "ENABLED")))
               (if (equal choice "All") nil choice))
           "ENABLED")))  ; Default to ENABLED for speed
  (let* ((filter (if (equal filter-state "All") nil filter-state))
         (filter-display (or filter "All"))
         (buffer-name "*ECloud Services*")
         (stream-id (format "list-%d" (random 100000)))
         (use-streaming (or (null filter) (equal filter "DISABLED"))))  ; Stream for large result sets
    
    (if (not use-streaming)
        ;; Fast path for ENABLED services (small result set)
        (progn
          (ecloud-notify-info (format "Loading %s services..." filter-display))
          (ecloud-rpc-service-usage-list-services-async
           (lambda (response)
             (let ((services (plist-get response :services))
                   (count (plist-get response :count)))
               (ecloud-services--display-list services count filter-display)
               (ecloud-notify-success (format "Loaded %d services" count))))
           filter
           (lambda (error-msg)
             (ecloud-notify-error (format "Failed to load services: %s" error-msg)))))
      
      ;; Streaming path for large result sets
      (let ((services-list '()))
        ;; Create loading buffer
        (with-current-buffer (get-buffer-create buffer-name)
          (let ((inhibit-read-only t))
            (erase-buffer)
            (insert (format "Loading %s services...\n" filter-display))
            (insert "This may take a while for large projects (10000+ services)\n\n")
            (insert "Services loaded: 0\n"))
          (view-mode)
          (display-buffer (current-buffer)))
        
        (ecloud-notify-info (format "Loading %s services (streaming)..." filter-display))
        
        ;; Set up hook for streaming results
        (let ((handler-fn
               (lambda (type data)
                 (when (and (string= type "service_usage_list_batch")
                           (equal (plist-get data :stream_id) stream-id))
                   (let ((batch-services (plist-get data :services))
                         (total-count (plist-get data :total_count))
                         (is-final (plist-get data :is_final)))
                     
                     ;; Add to list
                     (setq services-list (append services-list batch-services))
                     
                     ;; Update loading buffer
                     (when (get-buffer buffer-name)
                       (with-current-buffer buffer-name
                         (let ((inhibit-read-only t))
                           (goto-char (point-min))
                           (forward-line 3)
                           (delete-region (point) (point-max))
                           (insert (format "Services loaded: %d\n" total-count))
                           (when is-final
                             (insert "\nLoading complete! Rendering list...")))))
                     
                     ;; When final, display full list
                     (when is-final
                       ;; Remove handler
                       (remove-hook 'ecloud-service-usage-event-hook handler-fn)
                       
                       ;; Display services
                       (ecloud-services--display-list services-list total-count filter-display)
                       (ecloud-notify-success (format "Loaded %d services" total-count))))))))
          
          ;; Add hook
          (add-hook 'ecloud-service-usage-event-hook handler-fn)
          
          ;; Start streaming
          (ecloud-rpc-service-usage-list-services-streaming-async
           (lambda (result)
             ;; Streaming complete
             nil)  ; Handler already processed everything
           filter
           stream-id
           (lambda (error-msg)
             ;; Remove handler on error
             (remove-hook 'ecloud-service-usage-event-hook handler-fn)
             (when (get-buffer buffer-name)
               (kill-buffer buffer-name))
             (ecloud-notify-error (format "Failed to load services: %s" error-msg)))))))))

(defun ecloud-services--display-list (services count filter-display)
  "Display SERVICES list with COUNT total in buffer.
FILTER-DISPLAY is the filter description string."
  (let ((buffer-name "*ECloud Services*"))
    (with-current-buffer (get-buffer-create buffer-name)
      (let ((inhibit-read-only t))
        (erase-buffer)
        (insert (format "GCP Services/APIs (%d)\n" count))
        (insert (format "Filter: %s\n" filter-display))
        (insert (format "Tip: Use C-u M-x ecloud-services-list to change filter\n\n"))
        (insert (format "%-50s %-10s %s\n" "Service Name" "State" "Title"))
        (insert (make-string 120 ?-) "\n")
        (dolist (service services)
          (let ((name (plist-get service :name))
                (title (plist-get service :title))
                (state (plist-get service :state)))
            (insert (format "%-50s %-10s %s\n" name state title))))
        (goto-char (point-min)))
      (view-mode)
      (display-buffer (current-buffer)))))

;;;###autoload
(defun ecloud-services-enable ()
  "Enable a GCP service/API with fuzzy completion and streaming load."
  (interactive)
  (let* ((buffer-name "*ECloud Services Loading*")
         (services-list '())
         (stream-id (format "enable-%d" (random 100000)))
         (handler-fn
          (lambda (type data)
            (when (and (string= type "service_usage_list_batch")
                      (equal (plist-get data :stream_id) stream-id))
              (let ((batch-services (plist-get data :services))
                    (total-count (plist-get data :total_count))
                    (is-final (plist-get data :is_final)))
                
                ;; Add to list
                (setq services-list (append services-list batch-services))
                
                ;; Update loading buffer
                (when (get-buffer buffer-name)
                  (with-current-buffer buffer-name
                    (let ((inhibit-read-only t))
                      (goto-char (point-min))
                      (forward-line 3)
                      (delete-region (point) (point-max))
                      (insert (format "Services loaded: %d\n" total-count))
                      (when is-final
                        (insert "\nLoading complete! Preparing selection...")))))
                
                ;; When final, show completion
                (when is-final
                  ;; Remove handler
                  (remove-hook 'ecloud-service-usage-event-hook handler-fn)
                  
                  ;; Kill loading buffer
                  (when (get-buffer buffer-name)
                    (kill-buffer buffer-name))
                  
                  ;; Show completion
                  (if (null services-list)
                      (ecloud-notify-info "All services are already enabled")
                    (let* ((choices (mapcar
                                   (lambda (s)
                                     (let ((name (plist-get s :name))
                                           (title (plist-get s :title)))
                                       (cons (format "%s - %s" name title) name)))
                                   services-list))
                           (selection (completing-read 
                                     (format "Enable service (%d available): " (length services-list))
                                     choices nil t))
                           (service-name (cdr (assoc selection choices))))
                      (when service-name
                        (ecloud-notify-info (format "Enabling %s..." service-name))
                        (ecloud-rpc-service-usage-enable-service-async
                         service-name
                         (lambda (result)
                           (ecloud-notify-success
                            (format "Successfully enabled %s" service-name)))
                         (lambda (error-msg)
                           (ecloud-notify-error
                            (format "Failed to enable service: %s" error-msg)))))))))))))
    
    ;; Create loading buffer
    (with-current-buffer (get-buffer-create buffer-name)
      (let ((inhibit-read-only t))
        (erase-buffer)
        (insert "Loading disabled services...\n")
        (insert "This may take a while for large projects (10000+ services)\n\n")
        (insert "Services loaded: 0\n"))
      (view-mode)
      (display-buffer (current-buffer)))
    
    (ecloud-notify-info "Loading disabled services (streaming)...")
    
    ;; Add hook
    (add-hook 'ecloud-service-usage-event-hook handler-fn)
    
    ;; Start streaming
    (ecloud-rpc-service-usage-list-services-streaming-async
     (lambda (result)
       ;; Streaming complete
       (ecloud-notify-success 
        (format "Loaded %d services" (plist-get result :total_count))))
     "DISABLED"  ; Only disabled services
     stream-id
     (lambda (error-msg)
       ;; Remove handler on error
       (remove-hook 'ecloud-service-usage-event-hook handler-fn)
       (when (get-buffer buffer-name)
         (kill-buffer buffer-name))
       (ecloud-notify-error (format "Failed to load services: %s" error-msg))))))

;;;###autoload
(defun ecloud-services-enable-by-name (service-name)
  "Enable a GCP service/API by SERVICE-NAME directly.
This is useful when you know the exact service name (e.g., compute.googleapis.com)."
  (interactive "sService name (e.g., compute.googleapis.com): ")
  (when (string-empty-p service-name)
    (user-error "Service name cannot be empty"))
  (ecloud-notify-info (format "Enabling %s..." service-name))
  (ecloud-rpc-service-usage-enable-service-async
   service-name
   (lambda (result)
     (ecloud-notify-success
      (format "Successfully enabled %s" service-name)))
   (lambda (error-msg)
     (ecloud-notify-error
      (format "Failed to enable service: %s" error-msg)))))

;;;###autoload
(defun ecloud-services-disable ()
  "Disable a GCP service/API with fuzzy completion."
  (interactive)
  ;; ENABLED services are usually small (10-50), so no need for streaming
  (ecloud-notify-info "Loading enabled services...")
  (ecloud-rpc-service-usage-list-services-async
   (lambda (response)
     (let* ((services (plist-get response :services))
            (choices (mapcar
                     (lambda (s)
                       (let ((name (plist-get s :name))
                             (title (plist-get s :title)))
                         (cons (format "%s - %s" name title) name)))
                     services)))
       (if (null choices)
           (ecloud-notify-info "No enabled services found")
         (let* ((selection (completing-read 
                          (format "Disable service (%d enabled): " (length services))
                          choices nil t))
                (service-name (cdr (assoc selection choices))))
           (when service-name
             (when (yes-or-no-p (format "Really disable %s? This may break dependent services! " service-name))
               (ecloud-notify-info (format "Disabling %s..." service-name))
               (ecloud-rpc-service-usage-disable-service-async
                service-name
                (lambda (result)
                  (ecloud-notify-success
                   (format "Successfully disabled %s" service-name)))
                (lambda (error-msg)
                  (ecloud-notify-error
                   (format "Failed to disable service: %s" error-msg))))))))))
   "ENABLED"  ; Only get enabled services (fast)
   (lambda (error-msg)
     (ecloud-notify-error (format "Failed to load services: %s" error-msg)))))

;;;###autoload
(defun ecloud-services-get-info (service-name)
  "Get detailed information about a GCP service/API SERVICE-NAME."
  (interactive "sService name (e.g., compute.googleapis.com): ")
  (when (string-empty-p service-name)
    (user-error "Service name cannot be empty"))
  (condition-case err
      (let* ((response (ecloud-rpc-service-usage-get-service service-name))
             (service (plist-get response :service))
             (name (plist-get service :name))
             (title (plist-get service :title))
             (state (plist-get service :state)))
        (message "Service: %s\nTitle: %s\nState: %s" name title state))
    (error (message "Failed to get service info: %s" (error-message-string err)))))

(provide 'ecloud-services)
;;; ecloud-services.el ends here
