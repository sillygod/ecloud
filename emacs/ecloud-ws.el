;;; ecloud-ws.el --- WebSocket client for ecloud -*- lexical-binding: t; -*-

;; Copyright (C) 2024

;; Author: ecloud
;; Keywords: tools, cloud

;;; Commentary:

;; WebSocket client for ecloud.
;; Requires `websocket` package.

;;; Code:

(require 'websocket)
(require 'json)
(require 'ecloud-rpc)

(defvar ecloud-ws-client nil "The active WebSocket client.")
(defvar ecloud-ws-client nil "The active WebSocket client.")
(defcustom ecloud-ws-url "ws://127.0.0.1:8765/ws" "WebSocket URL."
  :type 'string
  :group 'ecloud)

(defcustom ecloud-ws-auto-connect t
  "Whether to automatically connect to WebSocket on load."
  :type 'boolean
  :group 'ecloud)

(defun ecloud-ws-connect ()
  "Connect to the ECloud WebSocket server."
  (interactive)
  (when ecloud-ws-client
    (websocket-close ecloud-ws-client))
  
  (setq ecloud-ws-client
        (websocket-open
         ecloud-ws-url
         :on-message (lambda (_ws frame)
                       (ecloud-ws--handle-message (websocket-frame-text frame)))
         :on-close (lambda (_ws)
                     (setq ecloud-ws-client nil)
                     (message "ECloud WebSocket disconnected"))
         :on-error (lambda (_ws err)
                     (message "ECloud WebSocket error: %s" err))))
  (message "ECloud WebSocket connected!"))

(defun ecloud-ws-disconnect ()
  "Disconnect from the ECloud WebSocket server."
  (interactive)
  (when ecloud-ws-client
    (websocket-close ecloud-ws-client)
    (setq ecloud-ws-client nil)
    (message "ECloud WebSocket disconnected")))

(defun ecloud-ws--handle-message (msg-text)
  "Handle incoming WebSocket message MSG-TEXT."
  (let* ((json-object-type 'plist)
         (json-array-type 'list)
         (json-key-type 'keyword)
         (msg (json-read-from-string msg-text))
         (type (plist-get msg :type))
         (data (plist-get msg :data)))
    
    (cond
     ((string= type "sql_proxy_started")
      (ecloud-ws--on-sql-proxy-event "sql_proxy_started" data))
     ((string= type "sql_proxy_stopped")
      (ecloud-ws--on-sql-proxy-event "sql_proxy_stopped" data))
     ((string-prefix-p "gcs_" type)
      (ecloud-ws--on-gcs-event type data))
     ((string-prefix-p "gar_" type)
      (ecloud-ws--on-gar-event type data))
     (t (message "Unknown ECloud event: %s" type)))))

;; Event Handlers

(defun ecloud-ws--on-sql-proxy-event (type data)
  "Handle proxy event TYPE with DATA."
  (run-hook-with-args 'ecloud-sql-event-hook type data))

(defun ecloud-ws--on-gcs-event (type data)
  "Handle GCS event TYPE with DATA."
  (run-hook-with-args 'ecloud-gcs-event-hook type data))

(defun ecloud-ws--on-gar-event (type data)
  "Handle GAR event TYPE with DATA."
  (run-hook-with-args 'ecloud-gar-event-hook type data))

;; Hooks

(defvar ecloud-sql-event-hook nil
  "Hook run when SQL instance/proxy state changes.
Functions are called with (TYPE DATA).")

(defvar ecloud-gcs-event-hook nil
  "Hook run when GCS state changes.
Functions are called with (TYPE DATA).")

(defvar ecloud-gar-event-hook nil
  "Hook run when GAR state changes.
Functions are called with (TYPE DATA).")

(provide 'ecloud-ws)

;; Auto-connect
(when ecloud-ws-auto-connect
  (condition-case nil
      (ecloud-ws-connect)
    (error (message "ECloud WebSocket: Could not connect automatically."))))
;;; ecloud-ws.el ends here
