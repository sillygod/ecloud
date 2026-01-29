;;; ecloud.el --- Emacs interface to Google Cloud Storage -*- lexical-binding: t; -*-

;; Copyright (C) 2024

;; Author: ecloud
;; Version: 0.1.0
;; Package-Requires: ((emacs "27.1"))
;; Keywords: tools, cloud, gcs, google
;; URL: https://github.com/user/ecloud

;;; Commentary:

;; ECloud provides an Emacs interface to Google Cloud Storage (GCS)
;; via a FastAPI JSON-RPC server.
;;
;; Features:
;; - Browse GCS buckets and objects in a dired-like interface
;; - Upload and download files
;; - Create and delete objects and folders
;; - Copy GCS URLs to clipboard
;; - Multi-account support for managing multiple GCP projects
;;
;; Quick Start:
;;
;; Single Account Mode (Backward Compatible):
;;
;; 1. Start the ecloud server:
;;    $ cd /path/to/ecloud/server
;;    $ export GOOGLE_APPLICATION_CREDENTIALS=/path/to/service-account.json
;;    $ uvicorn main:app --port 8765
;;
;; 2. Load this package:
;;    (add-to-list 'load-path "/path/to/ecloud/emacs")
;;    (require 'ecloud)
;;
;; 3. Browse GCS:
;;    M-x ecloud-browse
;;
;; Multi-Account Mode (Recommended):
;;
;; 1. Configure accounts in your init file:
;;    (setq ecloud-accounts
;;          '((staging . "/path/to/staging-service-account.json")
;;            (production . "/path/to/production-service-account.json")
;;            (dev . "/path/to/dev-service-account.json")))
;;
;; 2. Load this package:
;;    (add-to-list 'load-path "/path/to/ecloud/emacs")
;;    (require 'ecloud)
;;
;; 3. Switch between accounts:
;;    M-x ecloud-account-switch
;;
;; 4. View all accounts:
;;    M-x ecloud-account-list-processes
;;
;; The account manager will automatically:
;; - Start server processes for each account
;; - Allocate unique ports (default range: 8765-8774)
;; - Monitor server health
;; - Reconnect on failures
;; - Remember your last used account
;;
;; Backward Compatibility:
;; - If ecloud-accounts is not set, ECloud will use GOOGLE_APPLICATION_CREDENTIALS
;;   environment variable or the existing ecloud-server-url configuration
;; - All existing commands work without modification
;;
;; Key bindings in ecloud-browser:
;;   RET   - Enter bucket/folder
;;   ^     - Go up one level
;;   r     - Refresh
;;   d     - Download file at point
;;   u     - Upload file to current location
;;   D     - Delete file at point
;;   +     - Create folder
;;   c     - Copy gs:// URL
;;   q     - Quit

;;; Code:

(require 'ecloud-rpc)
(require 'ecloud-notify)
(require 'ecloud-browser)
(require 'ecloud-commands)
(require 'ecloud-gar)
(require 'ecloud-ips)
(require 'ecloud-compute)
(require 'ecloud-sql)
(require 'ecloud-k8s)
(require 'ecloud-ws)
(require 'ecloud-transient)
(require 'ecloud-account-manager)

;;; User customization

(defgroup ecloud nil
  "Emacs interface to Google Cloud Storage."
  :group 'tools
  :prefix "ecloud-")

;;; Autoloads

;;;###autoload
(autoload 'ecloud-menu "ecloud-transient"
  "Open the main ECloud menu." t)

;;;###autoload
(autoload 'ecloud-browse "ecloud-browser"
  "Open the ECloud browser." t)

;;;###autoload
(autoload 'ecloud-gar-browse "ecloud-gar"
  "Open the ECloud Artifact Registry browser." t)

;;;###autoload
(autoload 'ecloud-ips-list "ecloud-ips"
  "Open the ECloud IP addresses browser." t)

;;;###autoload
(autoload 'ecloud-compute-list "ecloud-compute"
  "Open the ECloud Compute instances browser." t)

;;;###autoload
(autoload 'ecloud-sql-list "ecloud-sql"
  "Open the ECloud SQL instances browser." t)

;;;###autoload
(autoload 'ecloud-k8s-list "ecloud-k8s"
  "Open the ECloud Kubernetes browser." t)

;;;###autoload
(autoload 'ecloud-ws-connect "ecloud-ws"
  "Connect to the ECloud WebSocket server." t)

;;;###autoload
(autoload 'ecloud-ws-disconnect "ecloud-ws"
  "Disconnect from the ECloud WebSocket server." t)

;;;###autoload
(autoload 'ecloud-download-file "ecloud-commands"
  "Download a file from GCS." t)

;;;###autoload
(autoload 'ecloud-upload-file "ecloud-commands"
  "Upload a local file to GCS." t)

;;;###autoload
(autoload 'ecloud-upload-buffer "ecloud-commands"
  "Upload current buffer to GCS." t)

;;;###autoload
(autoload 'ecloud-delete-object "ecloud-commands"
  "Delete an object from GCS." t)

;;;###autoload
(autoload 'ecloud-server-status "ecloud-commands"
  "Check the status of the ecloud server." t)

;;;###autoload
(autoload 'ecloud-copy-gs-url "ecloud-commands"
  "Copy a GCS object URL to clipboard." t)

;;;###autoload
(autoload 'ecloud-account-switch "ecloud-account-manager"
  "Switch to a different GCP account." t)

;;;###autoload
(autoload 'ecloud-account-list-processes "ecloud-account-manager"
  "Display a list of all account processes." t)

;;; Initialization

;; Initialize account manager on load
(with-eval-after-load 'ecloud-account-manager
  (ecloud-account-init))

(provide 'ecloud)
;;; ecloud.el ends here
