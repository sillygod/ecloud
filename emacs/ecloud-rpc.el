;;; ecloud-rpc.el --- JSON-RPC connection for ecloud -*- lexical-binding: t; -*-

;; Copyright (C) 2024

;; Author: ecloud
;; Keywords: tools, cloud

;;; Commentary:

;; JSON-RPC connection management for ecloud.
;; Uses url.el to communicate with the FastAPI server over HTTP.

;;; Code:

(require 'json)
(require 'url)
(require 'url-http)

;;; User customization

(defgroup ecloud-rpc nil
  "ECloud JSON-RPC settings."
  :group 'ecloud)

(defcustom ecloud-server-url "http://127.0.0.1:8765/jsonrpc"
  "URL of the ecloud JSON-RPC server endpoint."
  :type 'string
  :group 'ecloud-rpc)

(defcustom ecloud-request-timeout 30
  "Timeout in seconds for JSON-RPC requests."
  :type 'integer
  :group 'ecloud-rpc)

;;; Internal state

(defvar ecloud-rpc--request-id 0
  "Counter for JSON-RPC request IDs.")

;;; Helper functions

(defun ecloud-rpc--next-id ()
  "Generate the next request ID."
  (cl-incf ecloud-rpc--request-id))

(defun ecloud-rpc--build-request (method params)
  "Build a JSON-RPC 2.0 request for METHOD with PARAMS."
  (let ((request (list :jsonrpc "2.0"
                       :id (ecloud-rpc--next-id)
                       :method method)))
    (when params
      (setq request (plist-put request :params params)))
    request))

(defun ecloud-rpc--parse-response (response-string)
  "Parse RESPONSE-STRING as JSON-RPC response.
Returns the result on success, signals an error on failure."
  (let* ((json-object-type 'plist)
         (json-array-type 'list)
         (json-key-type 'keyword)
         (response (json-read-from-string response-string))
         (error-obj (plist-get response :error))
         (result (plist-get response :result)))
    (if error-obj
        (error "JSON-RPC Error %d: %s"
               (plist-get error-obj :code)
               (plist-get error-obj :message))
      result)))

;;; Public API

(defun ecloud-rpc-request (method &optional params)
  "Send a synchronous JSON-RPC request.
METHOD is the method name (string).
PARAMS is an optional plist of parameters.
Returns the result on success, signals an error on failure."
  (let* ((url-request-method "POST")
         (url-request-extra-headers
          '(("Content-Type" . "application/json")))
         (request (ecloud-rpc--build-request method params))
         (url-request-data (encode-coding-string
                           (json-encode request)
                           'utf-8))
         (buffer (url-retrieve-synchronously
                  ecloud-server-url
                  nil nil
                  ecloud-request-timeout)))
    (unless buffer
      (error "Failed to connect to ecloud server at %s" ecloud-server-url))
    (unwind-protect
        (with-current-buffer buffer
          ;; Skip HTTP headers
          (goto-char (point-min))
          (re-search-forward "^\r?\n" nil t)
          (let ((response-body (buffer-substring-no-properties
                               (point) (point-max))))
            (ecloud-rpc--parse-response response-body)))
      (kill-buffer buffer))))

(defun ecloud-rpc-request-async (method callback &optional params error-callback)
  "Send an asynchronous JSON-RPC request.
METHOD is the method name (string).
CALLBACK is called with the result on success.
PARAMS is an optional plist of parameters.
ERROR-CALLBACK is called with error message on failure."
  (let* ((url-request-method "POST")
         (url-request-extra-headers
          '(("Content-Type" . "application/json")))
         (request (ecloud-rpc--build-request method params))
         (url-request-data (encode-coding-string
                           (json-encode request)
                           'utf-8)))
    (url-retrieve
     ecloud-server-url
     (lambda (status callback error-callback)
       (if-let ((err (plist-get status :error)))
           (if error-callback
               (funcall error-callback (format "Connection error: %s" err))
             (message "ECloud error: %s" err))
         (goto-char (point-min))
         (re-search-forward "^\r?\n" nil t)
         (let ((response-body (buffer-substring-no-properties
                              (point) (point-max))))
           (condition-case err
               (funcall callback (ecloud-rpc--parse-response response-body))
             (error
              (if error-callback
                  (funcall error-callback (error-message-string err))
                (message "ECloud error: %s" (error-message-string err))))))))
     (list callback error-callback)
     t  ; silent
     t  ; inhibit cookies
     )))

;;; Convenience wrappers for common operations

(defun ecloud-rpc-ping ()
  "Ping the server to check connectivity."
  (ecloud-rpc-request "ping"))

(defun ecloud-rpc-list-buckets ()
  "List all accessible GCS buckets."
  (ecloud-rpc-request "list_buckets"))

(defun ecloud-rpc-list-objects (bucket &optional prefix)
  "List objects in BUCKET with optional PREFIX."
  (let ((params (list :bucket bucket)))
    (when prefix
      (setq params (plist-put params :prefix prefix)))
    (ecloud-rpc-request "list_objects" params)))

(defun ecloud-rpc-download (bucket object-path local-path)
  "Download OBJECT-PATH from BUCKET to LOCAL-PATH."
  (ecloud-rpc-request "download_object"
                      (list :bucket bucket
                            :object_path object-path
                            :local_path local-path)))

(defun ecloud-rpc-upload (bucket object-path local-path)
  "Upload LOCAL-PATH to OBJECT-PATH in BUCKET."
  (ecloud-rpc-request "upload_object"
                      (list :bucket bucket
                            :object_path object-path
                            :local_path local-path)))

(defun ecloud-rpc-delete (bucket object-path)
  "Delete OBJECT-PATH from BUCKET."
  (ecloud-rpc-request "delete_object"
                      (list :bucket bucket
                            :object_path object-path)))

(defun ecloud-rpc-create-folder (bucket folder-path)
  "Create FOLDER-PATH in BUCKET."
  (ecloud-rpc-request "create_folder"
                      (list :bucket bucket
                            :folder_path folder-path)))

(provide 'ecloud-rpc)
;;; ecloud-rpc.el ends here
