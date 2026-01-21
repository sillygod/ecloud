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
;;
;; Quick Start:
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
(require 'ecloud-browser)
(require 'ecloud-commands)
(require 'ecloud-gar)
(require 'ecloud-ips)
(require 'ecloud-compute)
(require 'ecloud-sql)

;;; User customization

(defgroup ecloud nil
  "Emacs interface to Google Cloud Storage."
  :group 'tools
  :prefix "ecloud-")

;;; Autoloads

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

(provide 'ecloud)
;;; ecloud.el ends here
