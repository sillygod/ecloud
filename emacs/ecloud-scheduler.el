;;; ecloud-scheduler.el --- Cloud Scheduler browser for ecloud -*- lexical-binding: t; -*-

;;; Commentary:

;; A browser for Google Cloud Scheduler jobs.
;; Supports viewing, creating, pausing, resuming, and deleting scheduled tasks.

;;; Code:

(require 'tabulated-list)
(require 'ecloud-rpc)
(require 'ecloud-notify)

;; Hook into WebSocket events
(defvar ecloud-scheduler-event-hook nil
  "Hook for receiving Cloud Scheduler events from WebSocket.")

(add-hook 'ecloud-scheduler-event-hook #'ecloud-scheduler--on-event)

;;; Customization

(defgroup ecloud-scheduler nil
  "ECloud Cloud Scheduler settings."
  :group 'ecloud)

(defcustom ecloud-scheduler-default-location "us-central1"
  "Default location for Cloud Scheduler operations."
  :type 'string
  :group 'ecloud-scheduler)

(defface ecloud-scheduler-name-face
  '((t :inherit font-lock-function-name-face :weight bold))
  "Face for job names."
  :group 'ecloud-scheduler)

(defface ecloud-scheduler-enabled-face
  '((t :inherit font-lock-string-face))
  "Face for ENABLED state."
  :group 'ecloud-scheduler)

(defface ecloud-scheduler-paused-face
  '((t :inherit font-lock-comment-face))
  "Face for PAUSED state."
  :group 'ecloud-scheduler)

;;; State

(defvar ecloud-scheduler--current-location nil
  "Currently selected location. If nil, shows jobs from all locations.")

(defvar ecloud-scheduler--show-all-locations t
  "Whether to show jobs from all locations by default.")

;;; Helper functions

(defun ecloud-scheduler--format-state (state)
  "Format STATE with appropriate face."
  (cond
   ((string= state "ENABLED")
    (propertize state 'face 'ecloud-scheduler-enabled-face))
   ((string= state "PAUSED")
    (propertize state 'face 'ecloud-scheduler-paused-face))
   (t state)))

(defun ecloud-scheduler--truncate-uri (uri)
  "Truncate URI for display."
  (if (> (length uri) 50)
      (concat (substring uri 0 47) "...")
    uri))

;;; Data fetching

(defun ecloud-scheduler--parse-jobs (jobs)
  "Parse JOBS list into tabulated list entries."
  (mapcar
   (lambda (job)
     (let* ((name (plist-get job :name))
            (schedule (plist-get job :schedule))
            (state (plist-get job :state))
            (target-type (plist-get job :targetType))
            (target-uri (plist-get job :targetUri))
            (next-run (plist-get job :nextRunTime))
            (location (plist-get job :location)))  ; Add location field
       (list job
             (vector
              (propertize name 'face 'ecloud-scheduler-name-face)
              schedule
              (ecloud-scheduler--format-state state)
              target-type
              (ecloud-scheduler--truncate-uri target-uri)
              (if (string-empty-p next-run) "-" next-run)
              (or location "-")))))  ; Add location column
   jobs))

(defun ecloud-scheduler--fetch-jobs ()
  "Fetch Cloud Scheduler jobs for current location or all locations."
  (if (and ecloud-scheduler--show-all-locations
           (not ecloud-scheduler--current-location))
      ;; Fetch from all locations
      (ecloud-scheduler--fetch-jobs-all-locations)
    ;; Fetch from specific location
    (let ((location (or ecloud-scheduler--current-location
                       ecloud-scheduler-default-location)))
      (message "Fetching Cloud Scheduler jobs from location: %s..." location)
      (condition-case err
          (let* ((response (ecloud-rpc-request "cloud_scheduler_list_jobs"
                                              (list :location location)))
                 (jobs (plist-get response :jobs))
                 (count (plist-get response :count)))
            (message "Found %d Cloud Scheduler job(s) in %s" (or count 0) location)
            (if (or (null jobs) (= (length jobs) 0))
                (progn
                  (message "No jobs found in location '%s'. Try pressing 'L' to change location or 'A' to show all." location)
                  nil)
              (ecloud-scheduler--parse-jobs jobs)))
        (error
         (ecloud-notify-error (format "Failed to fetch Cloud Scheduler jobs: %s"
                                     (error-message-string err)))
         (message "Error details: %S" err)
         nil)))))

(defun ecloud-scheduler--fetch-jobs-all-locations ()
  "Fetch Cloud Scheduler jobs from all locations."
  (message "Fetching Cloud Scheduler jobs from all locations...")
  (condition-case err
      (let* ((locations-response (ecloud-rpc-request "cloud_scheduler_list_locations"))
             (locations (plist-get locations-response :locations))
             (all-jobs nil)
             (total-count 0))
        
        ;; Fetch jobs from each location
        (dolist (location locations)
          (condition-case loc-err
              (let* ((response (ecloud-rpc-request "cloud_scheduler_list_jobs"
                                                  (list :location location)))
                     (jobs (plist-get response :jobs)))
                (when jobs
                  ;; Add location info to each job and collect them
                  (dolist (job jobs)
                    (let ((job-with-location (plist-put (copy-sequence job) :location location)))
                      (push job-with-location all-jobs)
                      (setq total-count (+ total-count 1))))))
            (error
             ;; Ignore errors for individual locations (might not have access)
             (message "Skipping location %s: %s" location (error-message-string loc-err)))))
        
        (message "Found %d Cloud Scheduler job(s) across all locations" total-count)
        (if (null all-jobs)
            (progn
              (message "No jobs found in any location. Press 'c' to create a new job.")
              nil)
          (ecloud-scheduler--parse-jobs (nreverse all-jobs))))
    (error
     (ecloud-notify-error (format "Failed to fetch Cloud Scheduler jobs: %s"
                                 (error-message-string err)))
     (message "Error details: %S" err)
     nil)))

;;; Browser mode

(defvar ecloud-scheduler-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "RET") #'ecloud-scheduler-view-job)
    (define-key map (kbd "r") #'ecloud-scheduler-refresh)
    (define-key map (kbd "g") #'ecloud-scheduler-refresh)
    (define-key map (kbd "c") #'ecloud-scheduler-create-http-job)
    (define-key map (kbd "p") #'ecloud-scheduler-pause-job)
    (define-key map (kbd "P") #'ecloud-scheduler-resume-job)
    (define-key map (kbd "R") #'ecloud-scheduler-run-job)
    (define-key map (kbd "D") #'ecloud-scheduler-delete-job)
    (define-key map (kbd "L") #'ecloud-scheduler-change-location)
    (define-key map (kbd "A") #'ecloud-scheduler-show-all-locations)
    (define-key map (kbd "?") #'ecloud-scheduler-help)
    (define-key map (kbd "q") #'quit-window)
    ;; Evil mode support
    (when (fboundp 'evil-define-key*)
      (evil-define-key* 'motion map
        (kbd "RET") #'ecloud-scheduler-view-job
        (kbd "r") #'ecloud-scheduler-refresh
        (kbd "g") #'ecloud-scheduler-refresh
        (kbd "c") #'ecloud-scheduler-create-http-job
        (kbd "p") #'ecloud-scheduler-pause-job
        (kbd "P") #'ecloud-scheduler-resume-job
        (kbd "R") #'ecloud-scheduler-run-job
        (kbd "D") #'ecloud-scheduler-delete-job
        (kbd "L") #'ecloud-scheduler-change-location
        (kbd "A") #'ecloud-scheduler-show-all-locations
        (kbd "?") #'ecloud-scheduler-help
        (kbd "q") #'quit-window))
    map)
  "Keymap for `ecloud-scheduler-mode'.")

(define-derived-mode ecloud-scheduler-mode tabulated-list-mode "ECloud-Scheduler"
  "Major mode for browsing Google Cloud Scheduler jobs.

\\{ecloud-scheduler-mode-map}"
  (setq tabulated-list-format
        [("Name" 30 t)
         ("Schedule" 15 t)
         ("State" 10 t)
         ("Type" 10 t)
         ("Target" 35 nil)
         ("Next Run" 20 t)
         ("Location" 18 t)])
  (setq tabulated-list-padding 2)
  (setq tabulated-list-sort-key (cons "Name" nil))
  (add-hook 'tabulated-list-revert-hook #'ecloud-scheduler--refresh nil t)
  (tabulated-list-init-header))

(defun ecloud-scheduler--refresh ()
  "Refresh the Cloud Scheduler jobs list."
  (let ((entries (ecloud-scheduler--fetch-jobs)))
    (setq tabulated-list-entries entries)))

;;; Interactive commands

;;;###autoload
(defun ecloud-scheduler-list (&optional location)
  "List Cloud Scheduler jobs in LOCATION or all locations."
  (interactive)
  (when location
    (setq ecloud-scheduler--current-location location)
    (setq ecloud-scheduler--show-all-locations nil))
  
  (let ((buffer (get-buffer-create "*ECloud-Scheduler*")))
    (with-current-buffer buffer
      (ecloud-scheduler-mode)
      (ecloud-scheduler--refresh)
      (tabulated-list-print))
    (switch-to-buffer buffer)
    
    ;; Show current mode in message
    (if ecloud-scheduler--show-all-locations
        (message "Cloud Scheduler jobs from all locations (Press 'L' to filter, '?' for help)")
      (message "Cloud Scheduler jobs in location: %s (Press 'A' for all locations, '?' for help)"
               (or ecloud-scheduler--current-location ecloud-scheduler-default-location)))))

(defun ecloud-scheduler-refresh ()
  "Refresh the current Cloud Scheduler view."
  (interactive)
  (ecloud-scheduler--refresh)
  (tabulated-list-print)
  (ecloud-notify-info "Cloud Scheduler jobs refreshed"))

(defun ecloud-scheduler-help ()
  "Show help for Cloud Scheduler browser."
  (interactive)
  (let ((help-text "
Cloud Scheduler Browser - Key Bindings
======================================

Navigation:
  RET     View job details
  L       Filter to specific location
  A       Show all locations

Actions:
  c       Create new HTTP job
  p       Pause job
  P       Resume job
  R       Run job now (manual trigger)
  D       Delete job

General:
  r, g    Refresh job list
  ?       Show this help
  q       Quit window

Current Mode: %s

Tips:
- By default, jobs from all locations are shown
- Press 'L' to filter to a specific location
- Press 'A' to show all locations again
- Jobs are location-specific (e.g., us-central1, asia-east1)
- Press 'c' to create a new scheduled job
- Use Cron expressions for scheduling (e.g., '0 9 * * *' = daily at 9am)
"))
    (message help-text 
             (if ecloud-scheduler--show-all-locations
                 "All Locations"
               (format "Location: %s" 
                      (or ecloud-scheduler--current-location 
                          ecloud-scheduler-default-location))))))

(defun ecloud-scheduler-change-location ()
  "Change the current Cloud Scheduler location."
  (interactive)
  (let* ((response (ecloud-rpc-request "cloud_scheduler_list_locations"))
         (locations (plist-get response :locations))
         (location (completing-read "Location: " locations nil t)))
    (setq ecloud-scheduler--current-location location)
    (setq ecloud-scheduler--show-all-locations nil)
    (ecloud-scheduler-refresh)))

(defun ecloud-scheduler-show-all-locations ()
  "Show jobs from all locations."
  (interactive)
  (setq ecloud-scheduler--show-all-locations t)
  (setq ecloud-scheduler--current-location nil)
  (ecloud-scheduler-refresh))

(defun ecloud-scheduler-view-job ()
  "View details of the Cloud Scheduler job at point."
  (interactive)
  (let* ((job (tabulated-list-get-id))
         (name (plist-get job :name)))
    (if job
        (ecloud-scheduler--show-job-details job)
      (ecloud-notify-error "No job at point"))))

(defun ecloud-scheduler--show-job-details (job)
  "Show details for JOB in a dedicated buffer."
  (let* ((name (plist-get job :name))
         (buffer (get-buffer-create (format "*ECloud-Scheduler: %s*" name))))
    (with-current-buffer buffer
      (let ((inhibit-read-only t))
        (erase-buffer)
        (insert (propertize (format "Cloud Scheduler Job: %s\n" name)
                           'face 'bold))
        (insert (make-string 60 ?=) "\n\n")
        
        (insert (propertize "General Information\n" 'face 'bold))
        (insert (format "  Name:           %s\n" (plist-get job :name)))
        (insert (format "  Description:    %s\n" (plist-get job :description)))
        (insert (format "  State:          %s\n" (plist-get job :state)))
        (insert "\n")
        
        (insert (propertize "Schedule\n" 'face 'bold))
        (insert (format "  Cron:           %s\n" (plist-get job :schedule)))
        (insert (format "  Timezone:       %s\n" (plist-get job :timezone)))
        (insert (format "  Next Run:       %s\n" (plist-get job :nextRunTime)))
        (insert (format "  Last Attempt:   %s\n" (plist-get job :lastAttemptTime)))
        (insert "\n")
        
        (insert (propertize "Target\n" 'face 'bold))
        (insert (format "  Type:           %s\n" (plist-get job :targetType)))
        (insert (format "  URI:            %s\n" (plist-get job :targetUri)))
        (insert "\n")
        
        (insert (propertize "Retry Configuration\n" 'face 'bold))
        (let ((retry-config (plist-get job :retryConfig)))
          (insert (format "  Retry Count:    %s\n" (plist-get retry-config :retryCount)))
          (insert (format "  Max Duration:   %s\n" (plist-get retry-config :maxRetryDuration)))
          (insert (format "  Min Backoff:    %s\n" (plist-get retry-config :minBackoffDuration)))
          (insert (format "  Max Backoff:    %s\n" (plist-get retry-config :maxBackoffDuration))))
        
        (goto-char (point-min))
        (view-mode)))
    (switch-to-buffer buffer)))

(defun ecloud-scheduler-create-http-job ()
  "Create a new HTTP Cloud Scheduler job."
  (interactive)
  (let* ((name (read-string "Job name: "))
         (schedule (read-string "Cron schedule (e.g., '0 9 * * *'): "))
         (uri (read-string "Target URI: "))
         (location (or ecloud-scheduler--current-location
                      (completing-read "Location: "
                                     (plist-get (ecloud-rpc-request "cloud_scheduler_list_locations")
                                              :locations)
                                     nil t
                                     ecloud-scheduler-default-location)))
         (description (read-string "Description (optional): "))
         (timezone (read-string "Timezone (default: UTC): " nil nil "UTC"))
         (http-method (completing-read "HTTP method: "
                                      '("GET" "POST" "PUT" "DELETE" "PATCH")
                                      nil t "POST"))
         (params (list :name name
                      :schedule schedule
                      :uri uri
                      :location location
                      :http_method http-method
                      :timezone timezone)))
    
    (when (not (string-empty-p description))
      (setq params (plist-put params :description description)))
    
    (ecloud-notify-info (format "Creating job %s..." name))
    
    (ecloud-rpc-request-async
     "cloud_scheduler_create_http_job"
     (lambda (response)
       (ecloud-notify-info (format "Job %s created successfully!" name))
       (ecloud-scheduler-refresh))
     params
     (lambda (err)
       (ecloud-notify-error (format "Creation failed: %s" err))))))

(defun ecloud-scheduler-pause-job ()
  "Pause the Cloud Scheduler job at point."
  (interactive)
  (let* ((job (tabulated-list-get-id))
         (name (plist-get job :name))
         (location (or (plist-get job :location)
                      ecloud-scheduler--current-location
                      ecloud-scheduler-default-location)))
    (if job
        (progn
          (ecloud-notify-info (format "Pausing job %s..." name))
          (ecloud-rpc-request-async
           "cloud_scheduler_pause_job"
           (lambda (_response)
             (ecloud-notify-info (format "Job %s paused" name))
             (ecloud-scheduler-refresh))
           (list :name name :location location)
           (lambda (err)
             (ecloud-notify-error (format "Pause failed: %s" err)))))
      (ecloud-notify-error "No job at point"))))

(defun ecloud-scheduler-resume-job ()
  "Resume the paused Cloud Scheduler job at point."
  (interactive)
  (let* ((job (tabulated-list-get-id))
         (name (plist-get job :name))
         (location (or (plist-get job :location)
                      ecloud-scheduler--current-location
                      ecloud-scheduler-default-location)))
    (if job
        (progn
          (ecloud-notify-info (format "Resuming job %s..." name))
          (ecloud-rpc-request-async
           "cloud_scheduler_resume_job"
           (lambda (_response)
             (ecloud-notify-info (format "Job %s resumed" name))
             (ecloud-scheduler-refresh))
           (list :name name :location location)
           (lambda (err)
             (ecloud-notify-error (format "Resume failed: %s" err)))))
      (ecloud-notify-error "No job at point"))))

(defun ecloud-scheduler-run-job ()
  "Manually trigger the Cloud Scheduler job at point."
  (interactive)
  (let* ((job (tabulated-list-get-id))
         (name (plist-get job :name))
         (location (or (plist-get job :location)
                      ecloud-scheduler--current-location
                      ecloud-scheduler-default-location)))
    (if job
        (when (yes-or-no-p (format "Trigger job '%s' now? " name))
          (ecloud-notify-info (format "Triggering job %s..." name))
          (ecloud-rpc-request-async
           "cloud_scheduler_run_job"
           (lambda (_response)
             (ecloud-notify-info (format "Job %s triggered" name))
             (ecloud-scheduler-refresh))
           (list :name name :location location)
           (lambda (err)
             (ecloud-notify-error (format "Trigger failed: %s" err)))))
      (ecloud-notify-error "No job at point"))))

(defun ecloud-scheduler-delete-job ()
  "Delete the Cloud Scheduler job at point."
  (interactive)
  (let* ((job (tabulated-list-get-id))
         (name (plist-get job :name))
         (location (or (plist-get job :location)
                      ecloud-scheduler--current-location
                      ecloud-scheduler-default-location)))
    (if job
        (when (yes-or-no-p (format "Delete Cloud Scheduler job '%s'? " name))
          (ecloud-notify-info (format "Deleting job %s..." name))
          (ecloud-rpc-request-async
           "cloud_scheduler_delete_job"
           (lambda (_response)
             (ecloud-notify-info (format "Job %s deleted" name))
             (ecloud-scheduler-refresh))
           (list :name name :location location)
           (lambda (err)
             (ecloud-notify-error (format "Delete failed: %s" err)))))
      (ecloud-notify-error "No job at point"))))

(defun ecloud-scheduler-debug-info ()
  "Show debug information about current Cloud Scheduler state."
  (interactive)
  (let* ((location (or ecloud-scheduler--current-location
                      ecloud-scheduler-default-location))
         (buffer (get-buffer-create "*ECloud-Scheduler-Debug*")))
    (with-current-buffer buffer
      (erase-buffer)
      (insert "Cloud Scheduler Debug Information\n")
      (insert "==================================\n\n")
      (insert (format "Current Location: %s\n" location))
      (insert (format "Default Location: %s\n" ecloud-scheduler-default-location))
      (insert (format "Server URL: %s\n" (if (fboundp 'ecloud-rpc--get-current-url)
                                             (ecloud-rpc--get-current-url)
                                           ecloud-server-url)))
      (insert "\nTrying to fetch jobs...\n")
      
      (condition-case err
          (let* ((response (ecloud-rpc-request "cloud_scheduler_list_jobs"
                                              (list :location location)))
                 (jobs (plist-get response :jobs))
                 (count (plist-get response :count)))
            (insert (format "\n✓ Success! Found %d job(s)\n" (or count 0)))
            (insert (format "\nRaw response:\n%S\n" response))
            (when jobs
              (insert "\nJobs:\n")
              (dolist (job jobs)
                (insert (format "  - %s (%s)\n" 
                              (plist-get job :name)
                              (plist-get job :state))))))
        (error
         (insert (format "\n✗ Error: %s\n" (error-message-string err)))
         (insert (format "\nFull error:\n%S\n" err))))
      
      (goto-char (point-min))
      (view-mode))
    (switch-to-buffer buffer)))

;;; WebSocket event handlers

(defun ecloud-scheduler--on-event (type data)
  "Handle Cloud Scheduler WebSocket event of TYPE with DATA."
  (cond
   ((string= type "cloud_scheduler_job_created")
    (let ((name (plist-get data :name)))
      (ecloud-notify-info (format "Cloud Scheduler job '%s' created" name))
      (ecloud-scheduler-refresh)))
   
   ((string= type "cloud_scheduler_job_paused")
    (let ((name (plist-get data :name)))
      (ecloud-notify-info (format "Cloud Scheduler job '%s' paused" name))
      (ecloud-scheduler-refresh)))
   
   ((string= type "cloud_scheduler_job_resumed")
    (let ((name (plist-get data :name)))
      (ecloud-notify-info (format "Cloud Scheduler job '%s' resumed" name))
      (ecloud-scheduler-refresh)))
   
   ((string= type "cloud_scheduler_job_deleted")
    (let ((name (plist-get data :name)))
      (ecloud-notify-info (format "Cloud Scheduler job '%s' deleted" name))
      (ecloud-scheduler-refresh)))))

(provide 'ecloud-scheduler)

;;; ecloud-scheduler.el ends here
