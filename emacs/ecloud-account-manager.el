;;; ecloud-account-manager.el --- Multi-account management for ECloud -*- lexical-binding: t; -*-

;; Copyright (C) 2024

;; Author: jing
;; Keywords: cloud, gcp, google-cloud
;; Package-Requires: ((emacs "27.1"))

;; This file is part of ECloud.

;;; Commentary:

;; This module provides multi-account support for ECloud, allowing users to
;; manage multiple Google Cloud Platform accounts simultaneously. Each account
;; can have its own service account credentials and dedicated server process.
;;
;; Features:
;; - Define multiple account configurations
;; - Automatic server process management
;; - Dynamic port allocation
;; - Account switching with auto-start
;; - Process health monitoring
;; - Persistent last-used account

;;; Code:

(require 'json)
(require 'cl-lib)

;; Forward declaration for backward compatibility
(defvar ecloud-server-url)

;;; Customization Group

(defgroup ecloud-account nil
  "Multi-account management for ECloud.
Configure and manage multiple Google Cloud Platform accounts,
each with its own service account credentials and server process."
  :group 'ecloud
  :prefix "ecloud-account-")

;;; Custom Variables

(defcustom ecloud-accounts nil
  "Alist of account configurations for multi-account support.

Each element should be a cons cell of the form:
  (ACCOUNT-NAME . SERVICE-ACCOUNT-PATH)

Where:
  ACCOUNT-NAME is a symbol identifying the account (e.g., \\='staging, \\='production)
  SERVICE-ACCOUNT-PATH is a string path to the service account JSON file

Example:
  (setq ecloud-accounts
        \\='((staging . \"/path/to/staging-service-account.json\")
          (production . \"/path/to/production-service-account.json\")
          (dev . \"/path/to/dev-service-account.json\")))

If nil, the account manager will attempt to use the
GOOGLE_APPLICATION_CREDENTIALS environment variable or the
existing ecloud-server-url configuration for backward compatibility."
  :type '(alist :key-type symbol :value-type string)
  :group 'ecloud-account)

(defcustom ecloud-port-range '(8765 . 8774)
  "Port range for server processes.

A cons cell of (START . END) defining the range of ports that can be
allocated for server processes. The default range supports up to 10
concurrent account connections.

Example:
  (setq ecloud-port-range \\='(9000 . 9019))  ; Support 20 accounts"
  :type '(cons integer integer)
  :group 'ecloud-account)

(defcustom ecloud-account-last-used nil
  "Last used account name for auto-connect on startup.

This variable is automatically updated when switching accounts and
is used to restore the previous session when `ecloud-auto-connect-last-account'
is enabled.

Do not set this manually; it is managed by the account manager."
  :type '(choice (const nil) symbol)
  :group 'ecloud-account)

(defcustom ecloud-auto-connect-last-account t
  "Whether to automatically connect to the last used account on startup.

When non-nil, ECloud will attempt to connect to the account specified
in `ecloud-account-last-used' during initialization. If that account
no longer exists in the configuration, the user will be prompted to
select an account.

Set to nil to disable auto-connect behavior."
  :type 'boolean
  :group 'ecloud-account)

(defcustom ecloud-account-show-in-mode-line t
  "Whether to display the current account in the mode-line.

When non-nil, the current active account name will be displayed in
the mode-line with the format \" [GCP:ACCOUNT]\".

Set to nil to disable mode-line display."
  :type 'boolean
  :group 'ecloud-account)

;;; Internal Variables

(defvar ecloud-account--current nil
  "Current active account name (symbol).

This variable tracks which account is currently being used for
GCP operations. All RPC requests will be routed to this account's
server process.

Do not set this directly; use `ecloud-account-switch' or
`ecloud-account-connect' instead.")

(defvar ecloud-account--log-buffer-name "*ecloud-account-log*"
  "Name of the dedicated log buffer for account manager operations.")

(defvar ecloud-account--processes nil
  "Registry of server processes.

An alist where each element has the form:
  (ACCOUNT-NAME . PROCESS-INFO)

Where PROCESS-INFO is a plist with the following keys:
  :process         - Emacs process object
  :port            - Port number the server is listening on
  :status          - Process status (\\='starting, \\='running, \\='stopped, \\='error)
  :start-time      - Time when the process was started
  :service-account - Path to the service account JSON file
  :buffer          - Buffer containing process output
  :url             - JSON-RPC URL (e.g., \"http://127.0.0.1:8765/jsonrpc\")
  :ws-url          - WebSocket URL (e.g., \"ws://127.0.0.1:8765/ws\")

This registry is used to track and manage all running server processes.")

(defvar ecloud-account--port-pool nil
  "Port allocation tracking.

A list of cons cells of the form (PORT . IN-USE-P), where:
  PORT is an integer port number
  IN-USE-P is t if the port is currently allocated, nil if available

This pool is initialized from `ecloud-port-range' and is used to
ensure each server process gets a unique port.")

;;; Mode-line Integration

;;; Error Handling and Logging Helpers

(defun ecloud-account--log (level message &optional context)
  "Log MESSAGE with LEVEL to messages buffer and optionally to dedicated log buffer.

Arguments:
  LEVEL - Log level symbol (\\='info, \\='warning, \\='error, \\='debug)
  MESSAGE - String message to log
  CONTEXT - Optional plist with contextual information (e.g., :account, :port, :path)

The log entry includes:
  - Timestamp
  - Log level
  - Message
  - Context information (if provided)

Side effects:
  - Writes to *Messages* buffer
  - Optionally writes to dedicated log buffer `ecloud-account--log-buffer-name'

Example:
  (ecloud-account--log \\='error \"Server startup failed\"
                       \\='(:account staging :port 8765))"
  (let* ((timestamp (format-time-string "%Y-%m-%d %H:%M:%S"))
         (level-str (upcase (symbol-name level)))
         (context-str (when context
                       (format " [%s]"
                              (mapconcat
                               (lambda (pair)
                                 (format "%s=%s"
                                        (substring (symbol-name (car pair)) 1)
                                        (cadr pair)))
                               (cl-loop for (k v) on context by #'cddr
                                       collect (list k v))
                               ", "))))
         (log-entry (format "[%s] %s: %s%s"
                           timestamp
                           level-str
                           message
                           (or context-str ""))))
    
    ;; Always log to *Messages*
    (message "%s" log-entry)
    
    ;; Optionally log to dedicated buffer for debugging
    (when (memq level '(error warning debug))
      (with-current-buffer (get-buffer-create ecloud-account--log-buffer-name)
        (goto-char (point-max))
        (insert log-entry "\n")
        ;; Keep buffer size manageable (last 1000 lines)
        (when (> (count-lines (point-min) (point-max)) 1000)
          (goto-char (point-min))
          (forward-line 100)
          (delete-region (point-min) (point)))))))

(defun ecloud-account--error (error-type message &optional context suggestions)
  "Signal an error with detailed context and suggestions.

Arguments:
  ERROR-TYPE - Symbol identifying the error type (e.g., \\='startup-failure, \\='invalid-config)
  MESSAGE - String describing the error
  CONTEXT - Optional plist with contextual information (e.g., :account, :port, :path)
  SUGGESTIONS - Optional list of strings with actionable suggestions

This function:
1. Logs the error with full context
2. Formats a user-friendly error message
3. Includes suggestions for resolution
4. Signals an error with the formatted message

The error message format:
  ECloud [ERROR-TYPE]: MESSAGE
  Context: key=value, key=value
  Suggestions:
    - suggestion 1
    - suggestion 2

Example:
  (ecloud-account--error
   \\='port-allocation-failure
   \"No available ports in the configured range\"
   \\='(:range \"8765-8774\" :in-use 10)
   \\='(\"Increase the port range by setting ecloud-port-range\"
     \"Stop unused account connections with ecloud-account-stop-all\"))"
  (let* ((error-type-str (symbol-name error-type))
         (context-str (when context
                       (format "\nContext: %s"
                              (mapconcat
                               (lambda (pair)
                                 (format "%s=%s"
                                        (substring (symbol-name (car pair)) 1)
                                        (cadr pair)))
                               (cl-loop for (k v) on context by #'cddr
                                       collect (list k v))
                               ", "))))
         (suggestions-str (when suggestions
                           (format "\n\nSuggestions:\n%s"
                                  (mapconcat
                                   (lambda (s) (format "  - %s" s))
                                   suggestions
                                   "\n"))))
         (full-message (format "ECloud [%s]: %s%s%s"
                              error-type-str
                              message
                              (or context-str "")
                              (or suggestions-str ""))))
    
    ;; Log the error with full context
    (ecloud-account--log 'error message context)
    
    ;; Signal the error
    (error "%s" full-message)))

;;; Mode-line Integration

(defvar ecloud-account-mode-line
  '(:eval (when (and ecloud-account-show-in-mode-line
                     (ecloud-account-current))
            (format " [GCP:%s]" (symbol-name (ecloud-account-current)))))
  "Mode-line construct for displaying the current account.

This variable is designed to be added to `mode-line-format' to show
the current active account in the mode-line. The display format is:
  \" [GCP:ACCOUNT]\"

The account name is only shown when:
  - `ecloud-account-show-in-mode-line' is non-nil
  - There is a current active account

The `:eval` form ensures the display updates dynamically when the
current account changes.")

(defun ecloud-account--update-mode-line ()
  "Update mode-line to show current account.

This function adds `ecloud-account-mode-line' to `mode-line-format'
if it's not already present, and forces a mode-line update to reflect
the current account.

The mode-line element is added to the end of `mode-line-format' to
avoid interfering with other mode-line elements.

This function is called automatically when:
  - An account is switched
  - An account is connected
  - An account is disconnected

Side effects:
  - Modifies `mode-line-format' if the element is not present
  - Forces a mode-line update in all windows"
  (unless (member 'ecloud-account-mode-line mode-line-format)
    (setq mode-line-format
          (append mode-line-format '(ecloud-account-mode-line))))
  (force-mode-line-update t))

;;; Configuration Parser Functions

(defun ecloud-account--get-server-directory ()
  "Find the server directory relative to the emacs directory.
Returns the absolute path to the server directory.
Signals an error if the server directory or main.py cannot be found."
  (let* ((emacs-dir (cond
                     ;; When loaded from a file
                     (load-file-name
                      (file-name-directory load-file-name))
                     ;; When in a buffer
                     (buffer-file-name
                      (file-name-directory buffer-file-name))
                     ;; Fallback: try to find ecloud.el in load-path
                     (t
                      (let ((ecloud-file (locate-library "ecloud")))
                        (when ecloud-file
                          (file-name-directory ecloud-file))))))
         ;; Go up one level from emacs/ to project root, then into server/
         (project-root (when emacs-dir (expand-file-name ".." emacs-dir)))
         (server-dir (when project-root (expand-file-name "server" project-root)))
         (main-py (when server-dir (expand-file-name "main.py" server-dir))))
    
    (unless server-dir
      (ecloud-account--error
       'server-directory-not-found
       "Cannot determine server directory location"
       nil
       '("Ensure ecloud is properly installed"
         "Check that the emacs/ directory structure is intact"
         "Try reinstalling ecloud")))
    
    (unless (file-directory-p server-dir)
      (ecloud-account--error
       'server-directory-missing
       "Server directory not found"
       (list :expected-path server-dir)
       '("Ensure the server/ directory exists in the ecloud installation"
         "Check that ecloud was installed completely"
         "Try reinstalling ecloud")))
    
    (unless (file-exists-p main-py)
      (ecloud-account--error
       'server-main-missing
       "Server main.py not found"
       (list :expected-path main-py)
       '("Ensure the server/main.py file exists"
         "Check that the server code is installed"
         "Try reinstalling ecloud")))
    
    server-dir))

(defun ecloud-account--validate-service-account (path)
  "Validate service account JSON file at PATH.
Returns t if valid, signals an error with a descriptive message otherwise.

Validation checks:
- File exists and is readable
- File contains valid JSON
- JSON contains required fields: client_email and project_id"
  (unless (file-exists-p path)
    (ecloud-account--error
     'service-account-not-found
     "Service account file does not exist"
     (list :path path)
     (list "Check that the file path is correct"
           "Ensure the file has not been moved or deleted"
           (format "Expected file at: %s" path))))
  
  (unless (file-readable-p path)
    (ecloud-account--error
     'service-account-not-readable
     "Service account file is not readable"
     (list :path path)
     (list "Check file permissions"
           (format "Try: chmod 644 %s" path)
           "Ensure the file is not locked by another process")))
  
  (condition-case err
      (let* ((json-object-type 'alist)
             (json-key-type 'string)
             (json-array-type 'list)
             (json-data (json-read-file path))
             (client-email (cdr (assoc "client_email" json-data)))
             (project-id (cdr (assoc "project_id" json-data))))
        
        (unless client-email
          (ecloud-account--error
           'service-account-missing-field
           "Service account file missing required field 'client_email'"
           (list :path path)
           (list "Ensure the file is a valid GCP service account JSON"
                 "Download a new service account key from GCP Console"
                 "Check that the file has not been manually edited")))
        
        (unless project-id
          (ecloud-account--error
           'service-account-missing-field
           "Service account file missing required field 'project_id'"
           (list :path path)
           (list "Ensure the file is a valid GCP service account JSON"
                 "Download a new service account key from GCP Console"
                 "Check that the file has not been manually edited")))
        
        t)
    (json-error
     (ecloud-account--error
      'service-account-invalid-json
      "Service account file contains invalid JSON"
      (list :path path :error (error-message-string err))
      (list "Check that the file is valid JSON"
            "Ensure the file has not been corrupted or manually edited"
            "Download a new service account key from GCP Console"
            (format "JSON error: %s" (error-message-string err)))))
    (error
     (ecloud-account--error
      'service-account-validation-failed
      "Failed to validate service account file"
      (list :path path :error (error-message-string err))
      (list "Check that the file is a valid GCP service account JSON"
            "Ensure the file is readable and not corrupted"
            (format "Error: %s" (error-message-string err)))))))

(defun ecloud-account--parse-config ()
  "Parse and validate account configuration.
Returns a list of account configuration plists.

Each plist has the following keys:
  :name            - Account name (symbol)
  :service-account - Path to service account JSON file (string)
  :type            - Account type (\\='managed or \\='external)
  :url             - For external accounts, the server URL (string)

Configuration priority:
1. If `ecloud-accounts' is set, use it
2. If `ecloud-server-url' is not default, create \\='external account
3. If GOOGLE_APPLICATION_CREDENTIALS env var exists, create \\='default account
4. Otherwise, return empty list

For backward compatibility:
- External accounts (custom server URL) don't start a process
- Default account uses GOOGLE_APPLICATION_CREDENTIALS from environment"
  (cond
   ;; Case 1: User has configured ecloud-accounts
   (ecloud-accounts
    (mapcar (lambda (account-pair)
              (let ((name (car account-pair))
                    (path (cdr account-pair)))
                (list :name name
                      :service-account path
                      :type 'managed)))
            ecloud-accounts))
   
   ;; Case 2: User has custom ecloud-server-url (external server)
   ((and (boundp 'ecloud-server-url)
         (not (string-prefix-p "http://127.0.0.1:8765" ecloud-server-url)))
    (list (list :name 'external
                :url ecloud-server-url
                :type 'external)))
   
   ;; Case 3: GOOGLE_APPLICATION_CREDENTIALS environment variable
   ((getenv "GOOGLE_APPLICATION_CREDENTIALS")
    (let ((creds-path (getenv "GOOGLE_APPLICATION_CREDENTIALS")))
      (list (list :name 'default
                  :service-account creds-path
                  :type 'managed))))
   
   ;; Case 4: No configuration found
   (t
    nil)))

;;; Port Allocator Functions

(defun ecloud-account--init-port-pool ()
  "Initialize port pool from `ecloud-port-range'.

Creates a list of (PORT . IN-USE-P) cons cells for each port in the
configured range. All ports are initially marked as available (nil).

The port pool is stored in `ecloud-account--port-pool'."
  (let* ((range ecloud-port-range)
         (start (car range))
         (end (cdr range))
         (pool nil))
    (when (or (not (integerp start))
              (not (integerp end))
              (< end start))
      (ecloud-account--error
       'invalid-port-range
       "Invalid port range configuration"
       (list :range (format "%s-%s" start end)
             :expected "START <= END where both are integers")
       (list "Set ecloud-port-range to a valid cons cell (START . END)"
             "Example: (setq ecloud-port-range '(8765 . 8774))"
             "Ensure START is less than or equal to END")))
    
    ;; Build pool from start to end (inclusive)
    (dotimes (i (1+ (- end start)))
      (push (cons (+ start i) nil) pool))
    
    ;; Store in reverse order so ports are allocated in ascending order
    (setq ecloud-account--port-pool (nreverse pool))))

(defun ecloud-account--allocate-port ()
  "Allocate an available port from the pool.

Finds the first available port (where IN-USE-P is nil), marks it as
in-use, and returns the port number.

Returns:
  Port number (integer) if a port is available
  nil if all ports in the pool are in use

Side effects:
  Modifies `ecloud-account--port-pool' to mark the allocated port as in-use."
  ;; Initialize pool if not already done
  (unless ecloud-account--port-pool
    (ecloud-account--init-port-pool))
  
  ;; Find first available port
  (let ((entry (cl-find-if (lambda (entry) (not (cdr entry)))
                           ecloud-account--port-pool)))
    (when entry
      ;; Mark as in-use
      (setcdr entry t)
      ;; Return port number
      (car entry))))

(defun ecloud-account--release-port (port)
  "Release PORT back to the pool, making it available for reuse.

Arguments:
  PORT - Port number (integer) to release

Side effects:
  Modifies `ecloud-account--port-pool' to mark the port as available.
  If the port is not found in the pool, no action is taken."
  (let ((entry (assoc port ecloud-account--port-pool)))
    (when entry
      ;; Mark as available
      (setcdr entry nil))))

(defun ecloud-account--is-port-available (port)
  "Check if PORT is available in the pool.

Arguments:
  PORT - Port number (integer) to check

Returns:
  t if the port exists in the pool and is not in use
  nil if the port is in use or not in the pool"
  (let ((entry (assoc port ecloud-account--port-pool)))
    (and entry (not (cdr entry)))))

(defun ecloud-account--is-port-in-use-system (port)
  "Check if PORT is actually in use at the system level.

Uses lsof (macOS/Linux) to check if the port is bound.

Arguments:
  PORT - Port number (integer) to check

Returns:
  t if the port is in use by another process
  nil if the port is available"
  (let ((result (shell-command-to-string
                 (format "lsof -i :%d -sTCP:LISTEN 2>/dev/null" port))))
    (not (string-empty-p (string-trim result)))))

(defun ecloud-account--allocate-port-safe ()
  "Allocate an available port from the pool, checking system availability.

This is a safer version of `ecloud-account--allocate-port' that also
checks if the port is actually available at the system level, not just
in the pool.

Returns:
  Port number (integer) if a port is available
  nil if all ports in the pool are in use or occupied by other processes

Side effects:
  Modifies `ecloud-account--port-pool' to mark the allocated port as in-use."
  ;; Initialize pool if not already done
  (unless ecloud-account--port-pool
    (ecloud-account--init-port-pool))
  
  ;; Try to find an available port that's also free at system level
  (let ((port nil)
        (attempts 0)
        (max-attempts (length ecloud-account--port-pool)))
    
    (while (and (not port) (< attempts max-attempts))
      ;; Find first available port in pool
      (let ((entry (cl-find-if (lambda (entry) (not (cdr entry)))
                               ecloud-account--port-pool)))
        (if entry
            (let ((candidate-port (car entry)))
              ;; Check if port is actually available at system level
              (if (ecloud-account--is-port-in-use-system candidate-port)
                  (progn
                    ;; Port is in use by another process, mark it as unavailable
                    ;; in our pool and try next
                    (ecloud-account--log 'warning
                                        (format "Port %d is in use by another process, skipping"
                                               candidate-port)
                                        (list :port candidate-port))
                    (setcdr entry t)  ; Mark as in-use in pool
                    (setq attempts (1+ attempts)))
                ;; Port is available, use it
                (progn
                  (setcdr entry t)  ; Mark as in-use
                  (setq port candidate-port))))
          ;; No more entries in pool
          (setq attempts max-attempts))))
    
    port))

;;; Process Management Functions

(defun ecloud-account--start-process (account-name service-account-path)
  "Start server process for ACCOUNT-NAME using SERVICE-ACCOUNT-PATH.

This function:
1. Allocates a port from the port pool
2. Creates a dedicated process buffer
3. Sets up the environment with GOOGLE_APPLICATION_CREDENTIALS
4. Starts the uvicorn server process
5. Registers the process in the process registry

Arguments:
  ACCOUNT-NAME - Symbol identifying the account
  SERVICE-ACCOUNT-PATH - String path to the service account JSON file

Returns:
  The Emacs process object if successful

Signals an error if:
  - No ports are available
  - The server directory cannot be found
  - The process fails to start

Side effects:
  - Allocates a port from the pool
  - Creates a process buffer named *ecloud-server-ACCOUNT-NAME*
  - Adds an entry to `ecloud-account--processes'
  - On error, releases the allocated port and cleans up"
  (let ((port nil)
        (buffer nil)
        (process nil))
    (condition-case err
        (progn
          ;; Step 1: Allocate port (with system-level check)
          (setq port (ecloud-account--allocate-port-safe))
          (unless port
            (let* ((range ecloud-port-range)
                   (total-ports (1+ (- (cdr range) (car range))))
                   (in-use (length (cl-remove-if-not #'cdr ecloud-account--port-pool))))
              (ecloud-account--error
               'port-allocation-failure
               "No available ports in the configured range"
               (list :range (format "%d-%d" (car range) (cdr range))
                     :total total-ports
                     :in-use in-use
                     :available (- total-ports in-use))
               (list "All ports in the range are either in use by ECloud or other processes"
                     "Increase the port range by setting ecloud-port-range"
                     (format "Example: (setq ecloud-port-range '(%d . %d))"
                            (car range) (+ (cdr range) 10))
                     "Stop unused account connections with M-x ecloud-account-stop-all"
                     "Check running accounts with M-x ecloud-account-list-processes"
                     "Check system port usage: lsof -i -sTCP:LISTEN | grep LISTEN"))))
          
          ;; Step 2: Create process buffer
          (setq buffer (generate-new-buffer (format "*ecloud-server-%s*" account-name)))
          
          ;; Step 3: Set up environment and start process
          (let* ((server-dir (ecloud-account--get-server-directory))
                 (process-environment
                  (append
                   (list (format "GOOGLE_APPLICATION_CREDENTIALS=%s" 
                                (expand-file-name service-account-path))
                         ;; Fix for VPN DNS issues with gRPC
                         "GRPC_DNS_RESOLVER=native")
                   process-environment))
                 (default-directory server-dir))
            
            ;; Step 4: Start the process
            (setq process (start-process
                          (format "ecloud-server-%s" account-name)
                          buffer
                          "uv" "run" "uvicorn" "main:app"
                          "--port" (number-to-string port)
                          "--host" "127.0.0.1"))
            
            ;; Step 5: Set up process handlers
            (set-process-sentinel process #'ecloud-account--process-sentinel)
            (set-process-filter process #'ecloud-account--process-filter)
            
            ;; Step 6: Create process info and register
            (let* ((start-time (current-time))
                   (url (format "http://127.0.0.1:%d/jsonrpc" port))
                   (ws-url (format "ws://127.0.0.1:%d/ws" port))
                   (process-info (list :process process
                                      :port port
                                      :status 'starting
                                      :start-time start-time
                                      :service-account service-account-path
                                      :buffer buffer
                                      :url url
                                      :ws-url ws-url)))
              
              ;; Add to registry
              (setq ecloud-account--processes
                    (cons (cons account-name process-info)
                          (assq-delete-all account-name ecloud-account--processes)))
              
              ;; Log startup
              (with-current-buffer buffer
                (insert (format "[%s] Starting server for account '%s'\n"
                               (format-time-string "%Y-%m-%d %H:%M:%S" start-time)
                               account-name))
                (insert (format "[%s] Port: %d\n"
                               (format-time-string "%Y-%m-%d %H:%M:%S")
                               port))
                (insert (format "[%s] Service account: %s\n"
                               (format-time-string "%Y-%m-%d %H:%M:%S")
                               service-account-path))
                (insert (format "[%s] Command: uv run uvicorn main:app --port %d --host 127.0.0.1\n"
                               (format-time-string "%Y-%m-%d %H:%M:%S")
                               port))
                (insert "---\n"))
              
              ;; Log to account manager log
              (ecloud-account--log 'info
                                  (format "Starting server for account '%s'" account-name)
                                  (list :account account-name :port port))
              
              ;; Return process object
              process)))
      
      ;; Error handling: clean up on failure
      (error
       (when port
         (ecloud-account--release-port port))
       (when buffer
         (kill-buffer buffer))
       (when process
         (delete-process process))
       ;; Remove from registry if added
       (setq ecloud-account--processes
             (assq-delete-all account-name ecloud-account--processes))
       
       ;; Get stderr output if available
       (let ((stderr-output (when (buffer-live-p buffer)
                             (with-current-buffer buffer
                               (buffer-string)))))
         (ecloud-account--error
          'server-startup-failure
          (format "Failed to start server for account '%s'" account-name)
          (list :account account-name
                :port (or port "not-allocated")
                :service-account service-account-path
                :error (error-message-string err)
                :stderr (or stderr-output "no output"))
          (list "Check that 'uv' is installed and in PATH"
                "Verify Python environment: uv run python --version"
                "Check server dependencies: cd server && uv sync"
                "Ensure the service account file is valid"
                "Check the process buffer for detailed output"
                (format "View logs: M-x ecloud-account-show-process-buffer RET %s"
                       account-name)
                (format "Original error: %s" (error-message-string err)))))))))

(defun ecloud-account--process-sentinel (process event)
  "Sentinel function for server processes.

Called when PROCESS state changes. Updates the process registry
to reflect the new state and handles cleanup on termination.

Arguments:
  PROCESS - The Emacs process object
  EVENT - String describing the state change

Side effects:
  - Updates process status in `ecloud-account--processes'
  - Releases port if process terminates
  - Logs event to process buffer"
  ;; Find the account for this process
  (let* ((account-entry (cl-find-if
                         (lambda (entry)
                           (eq (plist-get (cdr entry) :process) process))
                         ecloud-account--processes))
         (account-name (car account-entry))
         (process-info (cdr account-entry)))
    
    (when account-name
      (let* ((event-trimmed (string-trim event))
             (buffer (plist-get process-info :buffer))
             (port (plist-get process-info :port))
             (new-status (cond
                         ((string-match "finished" event-trimmed) 'stopped)
                         ((string-match "exited abnormally" event-trimmed) 'error)
                         ((string-match "killed" event-trimmed) 'error)
                         ((string-match "signal" event-trimmed) 'error)
                         (t 'error))))
        
        ;; Update status in registry
        (plist-put process-info :status new-status)
        
        ;; Log event to buffer
        (when (buffer-live-p buffer)
          (with-current-buffer buffer
            (goto-char (point-max))
            (insert (format "[%s] Process event: %s\n"
                           (format-time-string "%Y-%m-%d %H:%M:%S")
                           event-trimmed))
            (insert (format "[%s] Status changed to: %s\n"
                           (format-time-string "%Y-%m-%d %H:%M:%S")
                           new-status))))
        
        ;; Release port on termination
        (when (memq new-status '(stopped error))
          (when port
            (ecloud-account--release-port port))
          (message "ECloud server for account '%s' terminated: %s"
                   account-name event-trimmed))))))

(defun ecloud-account--process-filter (process output)
  "Filter function for server processes.

Appends OUTPUT to the process buffer and detects startup completion.
When the server reports \"Application startup complete\", this triggers
a health check to verify the server is ready.

Arguments:
  PROCESS - The Emacs process object
  OUTPUT - String output from the process

Side effects:
  - Appends output to process buffer
  - Detects startup completion message
  - Updates process status when startup is detected
  - Rotates log if buffer exceeds 10000 lines"
  ;; Find the account for this process
  (let* ((account-entry (cl-find-if
                         (lambda (entry)
                           (eq (plist-get (cdr entry) :process) process))
                         ecloud-account--processes))
         (account-name (car account-entry))
         (process-info (cdr account-entry))
         (buffer (when process-info (plist-get process-info :buffer))))
    
    ;; Append output to buffer
    (when (and buffer (buffer-live-p buffer))
      (with-current-buffer buffer
        (goto-char (point-max))
        (insert output))
      
      ;; Rotate log if needed
      (ecloud-account--rotate-process-buffer buffer))
    
    ;; Detect startup completion
    (when (and process-info
               (string-match-p "Application startup complete" output))
      (let ((buffer (plist-get process-info :buffer)))
        (when (buffer-live-p buffer)
          (with-current-buffer buffer
            (insert (format "[%s] Server startup detected\n"
                           (format-time-string "%Y-%m-%d %H:%M:%S")))))
        ;; Note: Health check will be implemented in task 2.3
        ;; For now, we just log the detection
        (message "ECloud server for account '%s' startup detected" account-name)))))

;;; Health Check Functions

(defun ecloud-account--health-check-request (url timeout)
  "Internal function to make HTTP request to URL with TIMEOUT.
Returns the response buffer or nil on failure.
This function is separated to make testing easier."
  (require 'url)
  (condition-case err
      (url-retrieve-synchronously url nil nil timeout)
    (error nil)))

(defun ecloud-account--health-check (port &optional max-retries)
  "Check if server at PORT is healthy.

Sends an HTTP GET request to the /health endpoint and checks for
a 200 OK response. Retries up to MAX-RETRIES times (default 3)
with 1 second delay between attempts.

Arguments:
  PORT - Port number (integer) where the server is listening
  MAX-RETRIES - Optional maximum number of retry attempts (default 3)

Returns:
  t if the server responds with HTTP 200
  nil if all retry attempts fail

The health check uses `url-retrieve-synchronously' with a 2 second
timeout per attempt. Between retries, there is a 1 second sleep."
  (let ((retries (or max-retries 3))
        (url (format "http://127.0.0.1:%d/health" port))
        (attempt 0)
        (success nil))
    
    (while (and (< attempt retries) (not success))
      (setq attempt (1+ attempt))
      
      (let ((response-buffer (ecloud-account--health-check-request url 2)))
        (when response-buffer
          (with-current-buffer response-buffer
            (goto-char (point-min))
            ;; Check for HTTP 200 response
            (when (re-search-forward "HTTP/[0-9.]+ 200" nil t)
              (setq success t)))
          (kill-buffer response-buffer)))
      
      (unless success
        ;; Log error
        (message "Health check attempt %d/%d failed for port %d"
                 attempt retries port))
      
      ;; Sleep between retries (but not after the last attempt)
      (unless (or success (= attempt retries))
        (sleep-for 1)))
    
    success))

(defun ecloud-account--wait-for-startup (account-name timeout)
  "Wait for server to start up, up to TIMEOUT seconds.

This function waits for the process filter to detect the startup
completion message, then performs a health check to verify the
server is responding correctly.

Arguments:
  ACCOUNT-NAME - Symbol identifying the account
  TIMEOUT - Maximum number of seconds to wait (integer)

Returns:
  t if the server started successfully and passed health check
  nil if timeout occurred or health check failed

The function polls the process status every 0.5 seconds until:
1. The startup message is detected (status changes from 'starting)
2. The timeout is reached
3. The process terminates abnormally

After detecting startup, it performs a health check with 3 retries."
  (let* ((process-info (cdr (assq account-name ecloud-account--processes)))
         (start-time (current-time))
         (deadline (time-add start-time (seconds-to-time timeout)))
         (port (plist-get process-info :port))
         (buffer (plist-get process-info :buffer))
         (startup-detected nil)
         (health-ok nil))
    
    (unless process-info
      (ecloud-account--error
       'process-not-found
       (format "No process info found for account '%s'" account-name)
       (list :account account-name)
       (list "This is an internal error - the process should have been registered"
             "Try restarting the account")))
    
    ;; Wait for startup message detection
    (while (and (not startup-detected)
                (time-less-p (current-time) deadline))
      (let ((status (plist-get process-info :status)))
        (cond
         ;; Startup detected when status changes from 'starting
         ((eq status 'running)
          (setq startup-detected t))
         
         ;; Check if process terminated abnormally
         ((memq status '(stopped error))
          (let ((stderr-output (when (buffer-live-p buffer)
                                (with-current-buffer buffer
                                  (buffer-string)))))
            (ecloud-account--error
             'server-startup-terminated
             (format "Server process for account '%s' terminated during startup" account-name)
             (list :account account-name
                   :port port
                   :status status
                   :stderr stderr-output)
             (list "Check the process buffer for error messages"
                   (format "View logs: M-x ecloud-account-show-process-buffer RET %s" account-name)
                   "Verify Python environment: uv run python --version"
                   "Check server dependencies: cd server && uv sync"
                   "Ensure the service account file is valid"))))
         
         ;; Still starting, check buffer for startup message
         ((eq status 'starting)
          (when (buffer-live-p buffer)
            (with-current-buffer buffer
              (save-excursion
                (goto-char (point-min))
                (when (re-search-forward "Application startup complete" nil t)
                  (setq startup-detected t)))))
          
          ;; Sleep briefly before next check
          (unless startup-detected
            (sleep-for 0.5))))))
    
    ;; Check if we timed out
    (unless startup-detected
      (let ((stderr-output (when (buffer-live-p buffer)
                            (with-current-buffer buffer
                              (buffer-string)))))
        (when (buffer-live-p buffer)
          (with-current-buffer buffer
            (insert (format "[%s] Startup timeout after %d seconds\n"
                           (format-time-string "%Y-%m-%d %H:%M:%S")
                           timeout))))
        (ecloud-account--error
         'server-startup-timeout
         (format "Server startup timeout for account '%s' after %d seconds" 
                account-name timeout)
         (list :account account-name
               :port port
               :timeout timeout
               :stderr stderr-output)
         (list "Check that Python and uvicorn are installed correctly"
               "Verify Python environment: uv run python --version"
               "Check server dependencies: cd server && uv sync"
               "Ensure port is not blocked by firewall"
               (format "View logs: M-x ecloud-account-show-process-buffer RET %s" account-name)
               "Try increasing the timeout (currently 30 seconds)"
               "Check system resources (CPU, memory)"))))
    
    ;; Perform health check
    (when (buffer-live-p buffer)
      (with-current-buffer buffer
        (insert (format "[%s] Performing health check...\n"
                       (format-time-string "%Y-%m-%d %H:%M:%S")))))
    
    (setq health-ok (ecloud-account--health-check port 3))
    
    (if health-ok
        (progn
          ;; Update status to running
          (plist-put process-info :status 'running)
          (when (buffer-live-p buffer)
            (with-current-buffer buffer
              (insert (format "[%s] Health check passed - server is ready\n"
                             (format-time-string "%Y-%m-%d %H:%M:%S")))))
          (ecloud-account--log 'info
                              (format "Server ready for account '%s'" account-name)
                              (list :account account-name :port port))
          (message "ECloud server for account '%s' is ready on port %d"
                   account-name port)
          t)
      
      ;; Health check failed
      (plist-put process-info :status 'error)
      (when (buffer-live-p buffer)
        (with-current-buffer buffer
          (insert (format "[%s] Health check failed after all retries\n"
                         (format-time-string "%Y-%m-%d %H:%M:%S")))))
      (ecloud-account--error
       'health-check-failure
       (format "Health check failed for account '%s' on port %d" 
              account-name port)
       (list :account account-name :port port :retries 3)
       (list "Server started but is not responding to health checks"
             (format "Check if port %d is accessible: curl http://127.0.0.1:%d/health" port port)
             "Verify firewall is not blocking the port"
             (format "View logs: M-x ecloud-account-show-process-buffer RET %s" account-name)
             "Check server dependencies: cd server && uv sync"
             "Try restarting the account")))))

;;; Process Termination Functions

(defun ecloud-account--stop-process (account-name &optional force)
  "Stop server process for ACCOUNT-NAME.

This function gracefully terminates the server process by:
1. Sending SIGTERM to allow graceful shutdown
2. Waiting up to 5 seconds for the process to terminate
3. If still running after 5 seconds, sending SIGKILL to force termination
4. Cleaning up resources (port, buffer, registry entry)

Arguments:
  ACCOUNT-NAME - Symbol identifying the account
  FORCE - If non-nil, immediately use SIGKILL instead of SIGTERM

Returns:
  t if the process was stopped successfully
  nil if no process was running for the account

Side effects:
  - Terminates the server process
  - Releases the allocated port
  - Removes entry from `ecloud-account--processes'
  - Kills the process buffer

Signals an error if the process cannot be terminated."
  (let* ((process-info (cdr (assq account-name ecloud-account--processes)))
         (process (when process-info (plist-get process-info :process)))
         (port (when process-info (plist-get process-info :port)))
         (buffer (when process-info (plist-get process-info :buffer))))
    
    (if (not process-info)
        (progn
          (message "No process running for account '%s'" account-name)
          nil)
      
      (if (not (process-live-p process))
          (progn
            (message "Process for account '%s' is not running" account-name)
            ;; Clean up stale entry
            (when port
              (ecloud-account--release-port port))
            (when (buffer-live-p buffer)
              (kill-buffer buffer))
            (setq ecloud-account--processes
                  (assq-delete-all account-name ecloud-account--processes))
            nil)
        
        ;; Process is live, proceed with termination
        (progn
          ;; Log termination attempt
          (when (buffer-live-p buffer)
            (with-current-buffer buffer
              (goto-char (point-max))
              (insert (format "[%s] Stopping server process%s...\n"
                             (format-time-string "%Y-%m-%d %H:%M:%S")
                             (if force " (forced)" "")))))
          
          (if force
              ;; Force kill immediately
              (progn
                (message "Force killing server for account '%s'..." account-name)
                (kill-process process))
            
            ;; Graceful shutdown with SIGTERM
            (progn
              (message "Stopping server for account '%s'..." account-name)
              (interrupt-process process)
              
              ;; Wait up to 5 seconds for graceful shutdown
              (let ((deadline (time-add (current-time) (seconds-to-time 5)))
                    (stopped nil))
                
                (while (and (not stopped)
                            (time-less-p (current-time) deadline))
                  (if (process-live-p process)
                      (sleep-for 0.2)
                    (setq stopped t)))
                
                ;; If still running after 5 seconds, force kill
                (when (process-live-p process)
                  (when (buffer-live-p buffer)
                    (with-current-buffer buffer
                      (goto-char (point-max))
                      (insert (format "[%s] Graceful shutdown timeout, forcing termination...\n"
                                     (format-time-string "%Y-%m-%d %H:%M:%S")))))
                  (message "Graceful shutdown timeout for account '%s', forcing termination..."
                           account-name)
                  (kill-process process)
                  ;; Wait a bit for forced kill to complete
                  (sleep-for 0.5)))))
          
          ;; Clean up resources
          (when port
            (ecloud-account--release-port port)
            (message "Released port %d for account '%s'" port account-name))
          
          (when (buffer-live-p buffer)
            (with-current-buffer buffer
              (goto-char (point-max))
              (insert (format "[%s] Server stopped\n"
                             (format-time-string "%Y-%m-%d %H:%M:%S"))))
            (kill-buffer buffer))
          
          ;; Remove from registry
          (setq ecloud-account--processes
                (assq-delete-all account-name ecloud-account--processes))
          
          (message "Stopped server for account '%s'" account-name)
          t)))))

(defun ecloud-account-stop-all ()
  "Stop all running server processes.

Iterates through all registered processes and stops each one using
`ecloud-account--stop-process'. This function is useful for cleanup
and is automatically called when Emacs exits.

Returns:
  The number of processes that were stopped (integer)

Side effects:
  - Stops all server processes
  - Releases all allocated ports
  - Clears the process registry"
  (interactive)
  (let ((accounts (mapcar #'car ecloud-account--processes))
        (count 0))
    
    (if (null accounts)
        (message "No server processes running")
      
      (message "Stopping %d server process%s..."
               (length accounts)
               (if (= (length accounts) 1) "" "es"))
      
      (dolist (account-name accounts)
        (when (ecloud-account--stop-process account-name)
          (setq count (1+ count))))
      
      (message "Stopped %d server process%s" count (if (= count 1) "" "es")))
    
    count))

;;; Account Connection Functions

(defun ecloud-account-connect (account-name)
  "Connect to ACCOUNT-NAME by starting its server process.

This function:
1. Validates that the account exists in the configuration
2. Checks if the account is already connected
3. If not connected, starts the server process and waits for it to be ready
4. Sets the account as the current active account

Arguments:
  ACCOUNT-NAME - Symbol identifying the account to connect

Returns:
  A success message string

Signals an error if:
  - The account is not configured
  - The service account file is invalid
  - The server fails to start
  - The health check fails

Side effects:
  - Starts a server process if not already running
  - Sets `ecloud-account--current' to ACCOUNT-NAME
  - Allocates a port from the pool
  - Creates a process buffer

Example:
  (ecloud-account-connect \\='staging)"
  (interactive
   (list (intern (completing-read "Connect to account: "
                                  (mapcar (lambda (cfg)
                                           (symbol-name (plist-get cfg :name)))
                                         (ecloud-account--parse-config))
                                  nil t))))
  
  ;; Step 1: Parse configuration and validate account exists
  (let* ((config (ecloud-account--parse-config))
         (account-config (cl-find-if
                         (lambda (cfg)
                           (eq (plist-get cfg :name) account-name))
                         config)))
    
    (unless account-config
      (let ((available-accounts (mapcar (lambda (cfg)
                                         (symbol-name (plist-get cfg :name)))
                                       config)))
        (ecloud-account--error
         'account-not-configured
         (format "Account '%s' is not configured" account-name)
         (list :requested-account account-name
               :available-accounts (mapconcat #'identity available-accounts ", "))
         (list "Check your ecloud-accounts configuration"
               (format "Available accounts: %s" (mapconcat #'identity available-accounts ", "))
               "Add the account to ecloud-accounts:"
               (format "  (setq ecloud-accounts '((%s . \"/path/to/service-account.json\") ...))"
                      account-name)
               "Or select an existing account with M-x ecloud-account-switch"))))
    
    ;; Step 2: Check if already connected
    (let* ((process-info (cdr (assq account-name ecloud-account--processes)))
           (process (when process-info (plist-get process-info :process)))
           (status (when process-info (plist-get process-info :status))))
      
      (if (and process (process-live-p process) (eq status 'running))
          ;; Already connected
          (progn
            (message "Account '%s' is already connected" account-name)
            (setq ecloud-account--current account-name)
            (format "Already connected to account '%s'" account-name))
        
        ;; Not connected, proceed with connection
        (let ((account-type (plist-get account-config :type)))
          (if (eq account-type 'external)
              ;; Handle external accounts (don't start process)
              (progn
                (setq ecloud-account--current account-name)
                (ecloud-account--update-mode-line)
                (message "Switched to external account '%s'" account-name)
                (format "Connected to external account '%s'" account-name))
            
            ;; Start the server process for managed accounts
            (let ((service-account-path (plist-get account-config :service-account)))
              
              ;; Validate service account file
              (ecloud-account--validate-service-account service-account-path)
              
              ;; Start the process
              (message "Starting server for account '%s'..." account-name)
              (condition-case err
                  (progn
                    (ecloud-account--start-process account-name service-account-path)
                    
                    ;; Wait for startup with 30 second timeout
                    (message "Waiting for server to be ready...")
                    (ecloud-account--wait-for-startup account-name 30)
                    
                    ;; Set as current account
                    (setq ecloud-account--current account-name)
                    
                    (let ((msg (format "Successfully connected to account '%s'" account-name)))
                      (message "%s" msg)
                      msg))
                
                (error
                 ;; Cleanup on failure
                 (message "Failed to connect to account '%s': %s"
                          account-name (error-message-string err))
                 (ecloud-account--stop-process account-name t)
                 (error "Failed to connect to account '%s': %s"
                        account-name (error-message-string err)))))))))))

(defun ecloud-account-disconnect (account-name)
  "Disconnect from ACCOUNT-NAME by stopping its server process.

This function:
1. Stops the server process for the specified account
2. If the account was the current active account, clears the current account
3. Cleans up all resources (port, buffer, registry entry)
4. Updates the mode-line display

Arguments:
  ACCOUNT-NAME - Symbol identifying the account to disconnect

Returns:
  A success message string

Side effects:
  - Stops the server process
  - Releases the allocated port
  - Removes the process from the registry
  - Clears `ecloud-account--current' if it was this account
  - Updates mode-line display
  - Kills the process buffer

Example:
  (ecloud-account-disconnect \\='staging)"
  (interactive
   (list (intern (completing-read "Disconnect from account: "
                                  (mapcar (lambda (entry)
                                           (symbol-name (car entry)))
                                         ecloud-account--processes)
                                  nil t))))
  
  ;; Stop the process
  (let ((stopped (ecloud-account--stop-process account-name)))
    
    ;; Clear current account if this was it
    (when (eq ecloud-account--current account-name)
      (setq ecloud-account--current nil)
      (ecloud-account--update-mode-line)
      (message "Cleared current account"))
    
    (if stopped
        (let ((msg (format "Disconnected from account '%s'" account-name)))
          (message "%s" msg)
          msg)
      (let ((msg (format "Account '%s' was not connected" account-name)))
        (message "%s" msg)
        msg))))

;;; Account Switching Functions

(defun ecloud-account--set-current (account-name)
  "Set ACCOUNT-NAME as the current active account and persist to custom variable.

This function:
1. Sets `ecloud-account--current' to ACCOUNT-NAME
2. Saves the account name to `ecloud-account-last-used' custom variable
3. Persists the change using `customize-save-variable'
4. Updates the mode-line display

Arguments:
  ACCOUNT-NAME - Symbol identifying the account to set as current

Side effects:
  - Updates `ecloud-account--current'
  - Updates and persists `ecloud-account-last-used'
  - Writes to the custom file
  - Updates mode-line display

This function is called internally by `ecloud-account-switch' and
`ecloud-account-connect' to maintain the current account state."
  (setq ecloud-account--current account-name)
  (customize-save-variable 'ecloud-account-last-used account-name)
  (ecloud-account--update-mode-line)
  (message "Current account set to '%s'" account-name))

(defun ecloud-account--select-account ()
  "Prompt user to select an account with completion.

Displays all configured accounts with their current status and marks
the current active account with an asterisk (*).

Returns:
  Symbol - The selected account name

The completion format shows:
  ACCOUNT-NAME [STATUS] *
  
Where:
  - ACCOUNT-NAME is the account identifier
  - STATUS is one of: running, stopped, starting, error
  - * appears only for the current active account

Signals an error if no accounts are configured."
  (let* ((config (ecloud-account--parse-config))
         (current (ecloud-account-current)))
    
    (unless config
      (ecloud-account--error
       'no-accounts-configured
       "No accounts configured"
       nil
       (list "Configure accounts by setting ecloud-accounts:"
             "  (setq ecloud-accounts"
             "        '((staging . \"/path/to/staging-service-account.json\")"
             "          (production . \"/path/to/production-service-account.json\")))"
             "Or set GOOGLE_APPLICATION_CREDENTIALS environment variable"
             "Or configure ecloud-server-url for an external server")))
    
    ;; Build completion choices with status information
    (let* ((choices
            (mapcar
             (lambda (cfg)
               (let* ((name (plist-get cfg :name))
                      (process-info (cdr (assq name ecloud-account--processes)))
                      (status (if process-info
                                 (plist-get process-info :status)
                               'stopped))
                      (current-marker (if (eq name current) " *" "")))
                 (cons (format "%s [%s]%s"
                              (symbol-name name)
                              status
                              current-marker)
                       name)))
             config))
           (selection (completing-read "Select account: "
                                      choices
                                      nil t)))
      
      ;; Extract account name from selection
      (cdr (assoc selection choices)))))

(defun ecloud-account-switch (account-name)
  "Switch to ACCOUNT-NAME as the current active account.

This function:
1. Validates that the account exists in the configuration
2. Checks if the account has a running server
3. If not running, automatically starts the server (auto-start)
4. Sets the account as current and persists the choice
5. Updates the mode-line display

Arguments:
  ACCOUNT-NAME - Symbol identifying the account to switch to

Returns:
  A success message string

Signals an error if:
  - The account is not configured
  - The server fails to start (for managed accounts)
  - The service account file is invalid

Side effects:
  - May start a server process if not already running
  - Updates `ecloud-account--current'
  - Persists the choice to `ecloud-account-last-used'
  - Updates mode-line display

Interactive usage:
  When called interactively, prompts for account selection with
  completion showing all accounts and their current status.

Example:
  (ecloud-account-switch \\='staging)
  
  Interactively:
  M-x ecloud-account-switch RET staging RET"
  (interactive (list (ecloud-account--select-account)))
  
  ;; Step 1: Parse configuration and validate account exists
  (let* ((config (ecloud-account--parse-config))
         (account-config (cl-find-if
                         (lambda (cfg)
                           (eq (plist-get cfg :name) account-name))
                         config)))
    
    (unless account-config
      (let ((available-accounts (mapcar (lambda (cfg)
                                         (symbol-name (plist-get cfg :name)))
                                       config)))
        (ecloud-account--error
         'account-not-configured
         (format "Account '%s' is not configured" account-name)
         (list :requested-account account-name
               :available-accounts (mapconcat #'identity available-accounts ", "))
         (list "Check your ecloud-accounts configuration"
               (format "Available accounts: %s" (mapconcat #'identity available-accounts ", "))
               "Add the account to ecloud-accounts:"
               (format "  (setq ecloud-accounts '((%s . \"/path/to/service-account.json\") ...))"
                      account-name)
               "Or select an existing account with M-x ecloud-account-switch"))))
    
    ;; Step 2: Check if account has running server
    (let* ((process-info (cdr (assq account-name ecloud-account--processes)))
           (process (when process-info (plist-get process-info :process)))
           (status (when process-info (plist-get process-info :status))))
      
      (if (and process (process-live-p process) (eq status 'running))
          ;; Account already running - immediate switch
          (progn
            (ecloud-account--set-current account-name)
            (let ((msg (format "Switched to account '%s' (already running)" account-name)))
              (message "%s" msg)
              msg))
        
        ;; Account not running - auto-start then switch
        (message "Account '%s' is not running, starting server..." account-name)
        (condition-case err
            (progn
              ;; Use ecloud-account-connect to start the server
              (ecloud-account-connect account-name)
              
              ;; Set as current (connect already does this, but be explicit)
              (ecloud-account--set-current account-name)
              
              (let ((msg (format "Switched to account '%s' (server started)" account-name)))
                (message "%s" msg)
                msg))
          
          (error
           (error "Failed to switch to account '%s': %s"
                  account-name (error-message-string err))))))))

(defun ecloud-account-current ()
  "Return the current active account name.

Returns:
  Symbol - The current account name, or nil if no account is active

The current account is the one that will be used for all GCP operations
and RPC requests. It is set by `ecloud-account-switch' or
`ecloud-account-connect'.

Example:
  (ecloud-account-current)  ; => \\='staging"
  ecloud-account--current)

;;; Status Query Functions

(defun ecloud-account-list ()
  "Return list of all configured account names.

Parses the account configuration and returns a list of all account
names (symbols) that are currently configured.

Returns:
  List of symbols - All configured account names

The list includes accounts from:
  - `ecloud-accounts' custom variable
  - GOOGLE_APPLICATION_CREDENTIALS environment variable (as \\='default)
  - Custom `ecloud-server-url' (as \\='external)

Returns nil if no accounts are configured.

Example:
  (ecloud-account-list)  ; => (staging production dev)"
  (let ((config (ecloud-account--parse-config)))
    (mapcar (lambda (cfg)
              (plist-get cfg :name))
            config)))

(defun ecloud-account-status (account-name)
  "Return status information for ACCOUNT-NAME.

Looks up the account in the process registry and returns its
process information plist.

Arguments:
  ACCOUNT-NAME - Symbol identifying the account

Returns:
  Plist with process information, containing keys:
    :process         - Emacs process object (or nil if stopped)
    :port            - Port number (or nil if stopped)
    :status          - Process status symbol (\\='starting, \\='running, \\='stopped, \\='error)
    :start-time      - Time when process started (or nil if stopped)
    :service-account - Path to service account JSON file (or nil if stopped)
    :buffer          - Process output buffer (or nil if stopped)
    :url             - JSON-RPC URL (or nil if stopped)
    :ws-url          - WebSocket URL (or nil if stopped)

If the account is not in the registry (not running), returns:
  (:status \\='stopped)

Example:
  (ecloud-account-status \\='staging)
  ; => (:process #<process ecloud-server-staging>
  ;     :port 8765
  ;     :status running
  ;     :start-time (25000 0 0 0)
  ;     :service-account \"/path/to/staging.json\"
  ;     :buffer #<buffer *ecloud-server-staging*>
  ;     :url \"http://127.0.0.1:8765/jsonrpc\"
  ;     :ws-url \"ws://127.0.0.1:8765/ws\")"
  (let ((process-info (cdr (assq account-name ecloud-account--processes))))
    (if process-info
        process-info
      ;; Not in registry, return stopped status
      (list :status 'stopped))))

(defun ecloud-account--get-url (account-name)
  "Get the JSON-RPC URL for ACCOUNT-NAME.

Looks up the account in the process registry and returns its URL.
If the URL is not set in the process info, constructs it from the port.

Arguments:
  ACCOUNT-NAME - Symbol identifying the account

Returns:
  String - The JSON-RPC URL (e.g., \"http://127.0.0.1:8765/jsonrpc\")
  nil if the account is not running or not found

For external accounts (configured with custom server URL), returns
the configured URL directly.

Example:
  (ecloud-account--get-url \\='staging)
  ; => \"http://127.0.0.1:8765/jsonrpc\""
  (let* ((process-info (cdr (assq account-name ecloud-account--processes)))
         (url (when process-info (plist-get process-info :url)))
         (port (when process-info (plist-get process-info :port))))
    
    (cond
     ;; URL is already set in process info
     (url url)
     
     ;; Construct URL from port
     (port (format "http://127.0.0.1:%d/jsonrpc" port))
     
     ;; Check if this is an external account
     (t
      (let* ((config (ecloud-account--parse-config))
             (account-config (cl-find-if
                             (lambda (cfg)
                               (eq (plist-get cfg :name) account-name))
                             config)))
        (when (and account-config
                   (eq (plist-get account-config :type) 'external))
          (plist-get account-config :url)))))))

;;; Connection Error Handling

(defun ecloud-account-restart (account-name)
  "Restart the server process for ACCOUNT-NAME.

This function performs a full restart by:
1. Disconnecting the account (stopping the server)
2. Waiting 1 second for cleanup
3. Reconnecting the account (starting a new server)

Arguments:
  ACCOUNT-NAME - Symbol identifying the account to restart

Returns:
  t if restart was successful
  nil if restart failed

Signals an error if:
  - The account is not configured
  - The new server fails to start
  - The health check fails

Side effects:
  - Stops the existing server process
  - Releases and reallocates a port
  - Creates a new process buffer
  - Updates the process registry

This function is typically called automatically by the connection
error handler when an RPC request fails.

Example:
  (ecloud-account-restart \\='staging)"
  (interactive
   (list (intern (completing-read "Restart account: "
                                  (mapcar (lambda (entry)
                                           (symbol-name (car entry)))
                                         ecloud-account--processes)
                                  nil t))))
  
  (message "Restarting server for account '%s'..." account-name)
  
  (condition-case err
      (progn
        ;; Step 1: Disconnect (stop the server)
        (ecloud-account-disconnect account-name)
        
        ;; Step 2: Wait for cleanup
        (sleep-for 1)
        
        ;; Step 3: Reconnect (start new server)
        (ecloud-account-connect account-name)
        
        (message "Successfully restarted server for account '%s'" account-name)
        t)
    
    (error
     (message "Failed to restart server for account '%s': %s"
              account-name (error-message-string err))
     nil)))

(defun ecloud-account--handle-connection-error ()
  "Handle connection error by attempting to restart the current account's server.

This function is called automatically when an RPC request fails due to
a connection error. It attempts to recover by restarting the server
process for the current active account.

Returns:
  t if the restart was successful
  nil if restart failed or no current account is set

Behavior:
1. Checks if there is a current active account
2. If no current account, signals an error
3. Logs the connection error
4. Calls `ecloud-account-restart' to restart the server
5. Returns t if restart succeeds, nil otherwise

This function is designed to be called from `ecloud-rpc-request' when
a connection error occurs. The RPC layer will retry the original request
if this function returns t.

Side effects:
  - Restarts the server process for the current account
  - Logs messages about the restart attempt
  - May signal an error if no current account is set

Example usage (typically called automatically):
  (when (ecloud-account--handle-connection-error)
    ;; Retry the failed request
    ...)"
  (let ((current-account (ecloud-account-current)))
    
    (unless current-account
      (ecloud-account--error
       'no-active-account
       "Connection error occurred but no account is currently active"
       nil
       (list "Select an account with M-x ecloud-account-switch"
             "Or connect to an account with M-x ecloud-account-connect"
             "Check configured accounts with M-x ecloud-account-list-processes")))
    
    ;; Log the connection error
    (ecloud-account--log 'warning
                        (format "Connection error detected for account '%s', attempting restart" current-account)
                        (list :account current-account))
    (message "ECloud: Connection error detected for account '%s', attempting restart..."
             current-account)
    
    ;; Attempt to restart the server
    (let ((restart-result (ecloud-account-restart current-account)))
      
      (if restart-result
          (progn
            (ecloud-account--log 'info
                                (format "Server restart successful for account '%s'" current-account)
                                (list :account current-account))
            (message "ECloud: Server restart successful, retrying request...")
            t)
        (progn
          (ecloud-account--log 'error
                              (format "Server restart failed for account '%s'" current-account)
                              (list :account current-account))
          (message "ECloud: Server restart failed")
          nil)))))

;;; Emacs Exit Hook

(defun ecloud-account--cleanup-on-exit ()
  "Cleanup function called when Emacs exits.
Stops all running server processes to ensure clean shutdown."
  (when ecloud-account--processes
    (message "ECloud: Cleaning up server processes...")
    (ecloud-account-stop-all)))

;; Register cleanup hook
(add-hook 'kill-emacs-hook #'ecloud-account--cleanup-on-exit)

;;; Process Buffer Display

(defun ecloud-account-show-process-buffer (account-name)
  "Display the process output buffer for ACCOUNT-NAME.

Shows the dedicated buffer containing all stdout and stderr output
from the server process for the specified account. The buffer is
displayed in another window.

Arguments:
  ACCOUNT-NAME - Symbol identifying the account

Signals an error if:
  - The account is not found in the process registry
  - The process buffer does not exist or has been killed

Side effects:
  - Displays the process buffer in another window
  - Switches focus to the process buffer window

The process buffer contains:
  - Startup information (port, service account path, command)
  - All server output (stdout and stderr)
  - Process events (startup detected, health check results, termination)
  - Timestamps for all events

This function is useful for:
  - Debugging server startup issues
  - Viewing server logs and error messages
  - Monitoring server activity

Example:
  (ecloud-account-show-process-buffer \\='staging)
  
  Interactively:
  M-x ecloud-account-show-process-buffer RET staging RET"
  (interactive
   (list (intern (completing-read "Show process buffer for account: "
                                  (mapcar (lambda (entry)
                                           (symbol-name (car entry)))
                                         ecloud-account--processes)
                                  nil t))))
  
  (let* ((process-info (cdr (assq account-name ecloud-account--processes)))
         (buffer (when process-info (plist-get process-info :buffer))))
    
    (unless process-info
      (ecloud-account--error
       'process-not-found
       (format "No process found for account '%s'" account-name)
       (list :account account-name)
       (list "The account may not be running"
             "Check running accounts with M-x ecloud-account-list-processes"
             "Start the account with M-x ecloud-account-connect")))
    
    (unless (and buffer (buffer-live-p buffer))
      (ecloud-account--error
       'buffer-not-found
       (format "Process buffer for account '%s' does not exist" account-name)
       (list :account account-name)
       (list "The buffer may have been killed"
             "Try restarting the account with M-x ecloud-account-restart")))
    
    ;; Display the buffer in another window
    (display-buffer buffer '(display-buffer-pop-up-window))
    (message "Showing process buffer for account '%s'" account-name)))

(defun ecloud-account--rotate-process-buffer (buffer)
  "Rotate log in BUFFER to prevent excessive memory usage.

Keeps only the last 10000 lines in the buffer, removing older lines
to prevent the buffer from growing indefinitely.

Arguments:
  BUFFER - The buffer to rotate (buffer object or buffer name)

Side effects:
  - Modifies the buffer content by removing old lines
  - Preserves the last 10000 lines
  - Adds a marker indicating log rotation occurred

This function is called automatically by the process filter when
the buffer exceeds the line limit. It ensures that long-running
server processes don't consume excessive memory.

The rotation preserves recent output while discarding old output,
maintaining a useful debugging window without memory issues."
  (when (buffer-live-p buffer)
    (with-current-buffer buffer
      (let ((line-count (count-lines (point-min) (point-max))))
        (when (> line-count 10000)
          (save-excursion
            (goto-char (point-min))
            ;; Keep last 10000 lines, delete the rest
            (forward-line (- line-count 10000))
            (let ((inhibit-read-only t))
              (delete-region (point-min) (point))
              (goto-char (point-min))
              (insert (format "[%s] Log rotated - older entries removed to save memory\n"
                             (format-time-string "%Y-%m-%d %H:%M:%S")))
              (insert "---\n"))))))))

;;; Account Status Buffer

(defvar ecloud-account-list-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "RET") #'ecloud-account-list-switch)
    (define-key map (kbd "c") #'ecloud-account-list-connect)
    (define-key map (kbd "d") #'ecloud-account-list-disconnect)
    (define-key map (kbd "r") #'ecloud-account-list-restart)
    (define-key map (kbd "l") #'ecloud-account-list-show-log)
    (define-key map (kbd "g") #'ecloud-account-list-refresh)
    (define-key map (kbd "q") #'quit-window)
    map)
  "Keymap for `ecloud-account-list-mode'.")

(define-derived-mode ecloud-account-list-mode special-mode "ECloud-Accounts"
  "Major mode for displaying ECloud account statuses.

This mode provides a tabular view of all configured accounts with
their current status, port numbers, and start times. The current
active account is marked with an asterisk (*).

Key bindings:
\\{ecloud-account-list-mode-map}

Commands:
  RET - Switch to account at point
  c   - Connect to account at point
  d   - Disconnect from account at point
  r   - Restart account at point
  l   - Show process log for account at point
  g   - Refresh the buffer
  q   - Quit window"
  (setq truncate-lines t)
  (setq buffer-read-only t))

;; Evil mode integration
(with-eval-after-load 'evil
  (evil-set-initial-state 'ecloud-account-list-mode 'motion)
  (evil-define-key 'motion ecloud-account-list-mode-map
    (kbd "RET") #'ecloud-account-list-switch
    (kbd "c")   #'ecloud-account-list-connect
    (kbd "d")   #'ecloud-account-list-disconnect
    (kbd "r")   #'ecloud-account-list-restart
    (kbd "l")   #'ecloud-account-list-show-log
    (kbd "g")   #'ecloud-account-list-refresh
    (kbd "q")   #'quit-window))

(defun ecloud-account-list--get-account-at-point ()
  "Get the account name at the current line.

Parses the current line to extract the account name. The line format
is expected to be:
  [*] ACCOUNT-NAME STATUS PORT STARTED

Returns:
  Symbol - The account name at point
  nil if no account is found on the current line

This function is used by interactive commands in the account list buffer
to determine which account to operate on."
  (save-excursion
    (beginning-of-line)
    ;; Skip the marker column and whitespace
    (when (re-search-forward "^\\s-*\\*?\\s-*\\([a-zA-Z0-9_-]+\\)" (line-end-position) t)
      (intern (match-string 1)))))

(defun ecloud-account-list-switch ()
  "Switch to the account at point in the account list buffer.

Reads the account name from the current line and calls
`ecloud-account-switch' to switch to that account. After switching,
refreshes the buffer to show the updated current account marker.

Signals an error if no account is found at point."
  (interactive)
  (let ((account-name (ecloud-account-list--get-account-at-point)))
    (if account-name
        (progn
          (ecloud-account-switch account-name)
          (ecloud-account-list-refresh))
      (error "No account at point"))))

(defun ecloud-account-list-connect ()
  "Connect to the account at point in the account list buffer.

Reads the account name from the current line and calls
`ecloud-account-connect' to start its server process. After connecting,
refreshes the buffer to show the updated status.

Signals an error if no account is found at point."
  (interactive)
  (let ((account-name (ecloud-account-list--get-account-at-point)))
    (if account-name
        (progn
          (ecloud-account-connect account-name)
          (ecloud-account-list-refresh))
      (error "No account at point"))))

(defun ecloud-account-list-disconnect ()
  "Disconnect from the account at point in the account list buffer.

Reads the account name from the current line and calls
`ecloud-account-disconnect' to stop its server process. After disconnecting,
refreshes the buffer to show the updated status.

Signals an error if no account is found at point."
  (interactive)
  (let ((account-name (ecloud-account-list--get-account-at-point)))
    (if account-name
        (progn
          (ecloud-account-disconnect account-name)
          (ecloud-account-list-refresh))
      (error "No account at point"))))

(defun ecloud-account-list-restart ()
  "Restart the account at point in the account list buffer.

Reads the account name from the current line and calls
`ecloud-account-restart' to restart its server process. After restarting,
refreshes the buffer to show the updated status.

Signals an error if no account is found at point."
  (interactive)
  (let ((account-name (ecloud-account-list--get-account-at-point)))
    (if account-name
        (progn
          (ecloud-account-restart account-name)
          (ecloud-account-list-refresh))
      (error "No account at point"))))

(defun ecloud-account-list-show-log ()
  "Show the process log buffer for the account at point.

Reads the account name from the current line and calls
`ecloud-account-show-process-buffer' to display its process output
buffer. This is useful for viewing server logs and debugging issues.

Signals an error if no account is found at point or if the account
has no process buffer (not running)."
  (interactive)
  (let ((account-name (ecloud-account-list--get-account-at-point)))
    (if account-name
        (ecloud-account-show-process-buffer account-name)
      (error "No account at point"))))

(defun ecloud-account-list-refresh ()
  "Refresh the account list buffer.

Regenerates the buffer content to show the current status of all
accounts. This is useful after performing operations that change
account states (connect, disconnect, switch, restart).

The cursor position is preserved if possible."
  (interactive)
  (let ((account-at-point (ecloud-account-list--get-account-at-point))
        (inhibit-read-only t))
    (erase-buffer)
    (ecloud-account-list--insert-content)
    ;; Try to restore cursor position to the same account
    (when account-at-point
      (goto-char (point-min))
      (when (re-search-forward (format "^\\s-*\\*?\\s-*%s\\b" 
                                      (symbol-name account-at-point))
                              nil t)
        (beginning-of-line)))))

(defun ecloud-account-list--insert-content ()
  "Insert the account list content into the current buffer.

This is a helper function that generates the tabular display of all
accounts with their status information. It is called by
`ecloud-account-list-processes' and `ecloud-account-list-refresh'.

The table includes columns for:
  - Marker (*) for current account
  - Account name
  - Status (running, stopped, starting, error)
  - Port number
  - Start time

The buffer is expected to be in `ecloud-account-list-mode' and
have `inhibit-read-only' set to t."
  (let ((accounts (ecloud-account-list))
        (current (ecloud-account-current)))
    
    ;; Insert header
    (insert (propertize "ECloud Accounts\n\n" 'face 'bold))
    
    (if (null accounts)
        (insert "No accounts configured.\n\n"
                "To configure accounts, set the `ecloud-accounts' variable:\n"
                "  (setq ecloud-accounts\n"
                "        '((staging . \"/path/to/staging-service-account.json\")\n"
                "          (production . \"/path/to/production-service-account.json\")))\n")
      
      ;; Insert column headers
      (insert (format "%-3s %-20s %-12s %-8s %-20s\n"
                     "" "Account" "Status" "Port" "Started"))
      (insert (make-string 70 ?-) "\n")
      
      ;; Insert account rows
      (dolist (account-name accounts)
        (let* ((info (ecloud-account-status account-name))
               (status (plist-get info :status))
               (port (plist-get info :port))
               (start-time (plist-get info :start-time))
               (current-p (eq account-name current))
               (marker (if current-p "*" " "))
               (status-str (symbol-name status))
               (port-str (if port (number-to-string port) "-"))
               (time-str (if start-time
                            (format-time-string "%Y-%m-%d %H:%M:%S" start-time)
                          "-")))
          
          (insert (format "%-3s %-20s %-12s %-8s %-20s\n"
                         marker
                         (symbol-name account-name)
                         status-str
                         port-str
                         time-str))))
      
      ;; Insert footer with help
      (insert "\n")
      (insert (propertize "Key bindings:\n" 'face 'bold))
      (insert "  RET - Switch to account\n")
      (insert "  c   - Connect to account\n")
      (insert "  d   - Disconnect from account\n")
      (insert "  r   - Restart account\n")
      (insert "  l   - Show process log\n")
      (insert "  g   - Refresh buffer\n")
      (insert "  q   - Quit window\n"))))

(defun ecloud-account-list-processes ()
  "Display a buffer listing all ECloud account processes.

Creates or switches to the *ecloud-accounts* buffer and displays
a tabular view of all configured accounts with their current status.

The buffer shows:
  - Current account marked with *
  - Account name
  - Status (running, stopped, starting, error)
  - Port number (if running)
  - Start time (if running)

Interactive commands are available via key bindings to manage accounts
directly from the buffer. See `ecloud-account-list-mode' for details.

This function is useful for:
  - Viewing the status of all accounts at a glance
  - Quickly switching between accounts
  - Managing account connections (start, stop, restart)

Example:
  M-x ecloud-account-list-processes RET"
  (interactive)
  (let ((buffer (get-buffer-create "*ecloud-accounts*")))
    (with-current-buffer buffer
      (ecloud-account-list-mode)
      (let ((inhibit-read-only t))
        (erase-buffer)
        (ecloud-account-list--insert-content))
      (goto-char (point-min)))
    (display-buffer buffer)))

;;; Auto-Connect and Initialization

(defvar ecloud-account--migration-hint-shown nil
  "Flag to track if migration hint has been shown.
Used to display the hint only once per session.")

(defun ecloud-account--show-migration-hint (config)
  "Show migration hint message if using legacy configuration.

Arguments:
  CONFIG - The parsed configuration list

This function checks if the user is using legacy configuration
(GOOGLE_APPLICATION_CREDENTIALS or custom ecloud-server-url) and
displays a one-time hint about the new multi-account configuration
format.

Side effects:
  - Displays a message suggesting the new configuration format
  - Sets `ecloud-account--migration-hint-shown' to prevent repeated hints"
  (when (and (not ecloud-account--migration-hint-shown)
             (not ecloud-accounts)  ; User hasn't configured ecloud-accounts
             config)                ; But we have a config (from legacy sources)
    
    (let ((account-type (plist-get (car config) :type)))
      (cond
       ;; Using GOOGLE_APPLICATION_CREDENTIALS
       ((and (eq (plist-get (car config) :name) 'default)
             (eq account-type 'managed))
        (message "ECloud: Using GOOGLE_APPLICATION_CREDENTIALS. For multi-account support, consider setting `ecloud-accounts'")
        (message "ECloud: Example: (setq ecloud-accounts '((staging . \"/path/to/staging.json\") (prod . \"/path/to/prod.json\")))")
        (setq ecloud-account--migration-hint-shown t))
       
       ;; Using custom ecloud-server-url
       ((eq account-type 'external)
        (message "ECloud: Using custom ecloud-server-url. For multi-account support, consider setting `ecloud-accounts'")
        (setq ecloud-account--migration-hint-shown t))))))

(defun ecloud-account--auto-connect ()
  "Auto-connect to last used account if configured.

This function is called during initialization to automatically connect
to the last used account. It respects the `ecloud-auto-connect-last-account'
setting and handles various edge cases gracefully.

Behavior:
1. Checks if auto-connect is enabled via `ecloud-auto-connect-last-account'
2. Retrieves the last used account from `ecloud-account-last-used'
3. Validates that the account still exists in the current configuration
4. Attempts to connect to the account
5. If the account doesn't exist, logs a message with available accounts
6. Special case: If only one account is configured, auto-connects to it
   regardless of last-used setting (for single-account backward compatibility)

Returns:
  t if auto-connect was successful
  nil if auto-connect is disabled, no last-used account is set, or connection failed

Side effects:
  - May start a server process
  - Logs messages about auto-connect attempts
  - Errors are caught and logged (does not block Emacs startup)

This function is designed to be called from `ecloud-account-init' or
added to `after-init-hook'. It ensures a smooth startup experience by
automatically restoring the user's previous session."
  (when ecloud-auto-connect-last-account
    (let* ((config (ecloud-account--parse-config))
           (account-count (length config))
           (last-account ecloud-account-last-used))
      
      ;; Special case: Single account auto-connect (backward compatibility)
      ;; If there's only one account configured, auto-connect to it regardless
      ;; of last-used setting. This provides seamless single-account experience.
      (if (= account-count 1)
          (let ((single-account (plist-get (car config) :name)))
            (message "ECloud: Single account detected, auto-connecting to '%s'..." single-account)
            (condition-case err
                (progn
                  (ecloud-account-connect single-account)
                  (message "ECloud: Auto-connected to account '%s'" single-account)
                  t)
              (error
               (message "ECloud: Failed to auto-connect to account '%s': %s"
                       single-account (error-message-string err))
               nil)))
        
        ;; Multiple accounts: use last-used account
        (if (not last-account)
            ;; No last-used account, nothing to do
            (progn
              (message "ECloud: No last-used account configured")
              nil)
          
          ;; Check if the last-used account still exists in configuration
          (let ((account-exists (cl-find-if
                                (lambda (cfg)
                                  (eq (plist-get cfg :name) last-account))
                                config)))
            
            (if account-exists
                ;; Account exists, attempt to connect
                (progn
                  (message "ECloud: Auto-connecting to account '%s'..." last-account)
                  (condition-case err
                      (progn
                        (ecloud-account-connect last-account)
                        (message "ECloud: Auto-connected to account '%s'" last-account)
                        t)
                    (error
                     (message "ECloud: Failed to auto-connect to account '%s': %s"
                             last-account (error-message-string err))
                     nil)))
              
              ;; Account doesn't exist anymore, log message
              (progn
                (message "ECloud: Last-used account '%s' no longer exists in configuration"
                        last-account)
                (message "ECloud: Available accounts: %s"
                        (mapconcat (lambda (cfg)
                                    (symbol-name (plist-get cfg :name)))
                                  config ", "))
                nil))))))))

(defun ecloud-account-init ()
  "Initialize the ECloud account manager.

This function performs the following initialization steps:
1. Initializes the port pool from `ecloud-port-range'
2. Parses the account configuration
3. Shows migration hint if using legacy configuration
4. Calls auto-connect if enabled via `ecloud-auto-connect-last-account'

This function should be called once during Emacs initialization,
either explicitly or by adding it to `after-init-hook'.

Returns:
  t if initialization completed successfully
  nil if initialization failed

Side effects:
  - Initializes `ecloud-account--port-pool'
  - May start a server process (if auto-connect is enabled)
  - Logs initialization messages
  - Does not block Emacs startup (errors are caught and logged)

The function is designed to be idempotent - calling it multiple times
is safe and will reinitialize the port pool and configuration.

Example:
  ;; In your init.el or .emacs:
  (with-eval-after-load 'ecloud-account-manager
    (ecloud-account-init))
  
  ;; Or add to after-init-hook:
  (add-hook 'after-init-hook #'ecloud-account-init)"
  (condition-case err
      (progn
        (message "ECloud: Initializing account manager...")
        
        ;; Step 1: Initialize port pool
        (ecloud-account--init-port-pool)
        (message "ECloud: Port pool initialized (range: %d-%d)"
                (car ecloud-port-range)
                (cdr ecloud-port-range))
        
        ;; Step 2: Parse configuration
        (let ((config (ecloud-account--parse-config)))
          (if config
              (progn
                (message "ECloud: Found %d configured account%s: %s"
                        (length config)
                        (if (= (length config) 1) "" "s")
                        (mapconcat (lambda (cfg)
                                    (symbol-name (plist-get cfg :name)))
                                  config ", "))
                
                ;; Step 3: Show migration hint if using legacy configuration
                (ecloud-account--show-migration-hint config))
            (message "ECloud: No accounts configured")))
        
        ;; Step 4: Auto-connect if enabled
        (when ecloud-auto-connect-last-account
          (ecloud-account--auto-connect))
        
        (message "ECloud: Account manager initialized")
        t)
    
    (error
     (message "ECloud: Failed to initialize account manager: %s"
             (error-message-string err))
     nil)))

;;; Module Provide

(provide 'ecloud-account-manager)

;;; ecloud-account-manager.el ends here
