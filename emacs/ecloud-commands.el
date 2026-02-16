;;; ecloud-commands.el --- Additional commands for ecloud -*- lexical-binding: t; -*-

;; Copyright (C) 2024

;; Author: jing
;; Keywords: tools, cloud

;;; Commentary:

;; Additional interactive commands for ecloud that work outside the browser.

;;; Code:

(require 'ecloud-rpc)

;;; Interactive commands

;;;###autoload
(defun ecloud-download-file (bucket object-path local-path)
  "Download a file from GCS.
BUCKET is the bucket name.
OBJECT-PATH is the full path to the object in GCS.
LOCAL-PATH is where to save the file locally."
  (interactive
   (let* ((buckets-response (ecloud-rpc-list-buckets))
          (bucket-names (mapcar (lambda (b) (plist-get b :name))
                               (plist-get buckets-response :buckets)))
          (bucket (completing-read "Bucket: " bucket-names nil t))
          (objects-response (ecloud-rpc-list-objects bucket))
          (object-names (mapcar (lambda (o) (plist-get o :name))
                               (plist-get objects-response :objects)))
          (object-path (completing-read "Object: " object-names nil t))
          (filename (file-name-nondirectory object-path))
          (local-path (read-file-name "Save to: " nil filename)))
     (list bucket object-path local-path)))
  (message "Downloading %s from %s..." object-path bucket)
  (condition-case err
      (let ((result (ecloud-rpc-download bucket object-path local-path)))
        (message "Downloaded %s (%s bytes)"
                 object-path
                 (plist-get result :size)))
    (error (message "Download failed: %s" (error-message-string err)))))

;;;###autoload
(defun ecloud-upload-file (local-path bucket object-path)
  "Upload a local file to GCS.
LOCAL-PATH is the file to upload.
BUCKET is the target bucket.
OBJECT-PATH is the destination path in GCS."
  (interactive
   (let* ((local-path (read-file-name "File to upload: " nil nil t))
          (buckets-response (ecloud-rpc-list-buckets))
          (bucket-names (mapcar (lambda (b) (plist-get b :name))
                               (plist-get buckets-response :buckets)))
          (bucket (completing-read "Bucket: " bucket-names nil t))
          (filename (file-name-nondirectory local-path))
          (object-path (read-string "Destination path: " filename)))
     (list local-path bucket object-path)))
  (message "Uploading %s to %s/%s..." local-path bucket object-path)
  (condition-case err
      (let ((result (ecloud-rpc-upload bucket object-path local-path)))
        (message "Uploaded to gs://%s/%s (%s bytes)"
                 bucket
                 object-path
                 (plist-get result :size)))
    (error (message "Upload failed: %s" (error-message-string err)))))

;;;###autoload
(defun ecloud-upload-buffer (bucket object-path)
  "Upload current buffer to GCS.
BUCKET is the target bucket.
OBJECT-PATH is the destination path in GCS."
  (interactive
   (let* ((buckets-response (ecloud-rpc-list-buckets))
          (bucket-names (mapcar (lambda (b) (plist-get b :name))
                               (plist-get buckets-response :buckets)))
          (bucket (completing-read "Bucket: " bucket-names nil t))
          (filename (or (buffer-file-name)
                       (buffer-name)))
          (object-path (read-string "Destination path: "
                                    (file-name-nondirectory filename))))
     (list bucket object-path)))
  (let ((temp-file (make-temp-file "ecloud-upload-")))
    (unwind-protect
        (progn
          (write-region (point-min) (point-max) temp-file nil 'quiet)
          (message "Uploading buffer to %s/%s..." bucket object-path)
          (condition-case err
              (let ((result (ecloud-rpc-upload bucket object-path temp-file)))
                (message "Uploaded to gs://%s/%s (%s bytes)"
                         bucket
                         object-path
                         (plist-get result :size)))
            (error (message "Upload failed: %s" (error-message-string err)))))
      (delete-file temp-file))))

;;;###autoload
(defun ecloud-delete-object (bucket object-path)
  "Delete an object from GCS.
BUCKET is the bucket name.
OBJECT-PATH is the full path to the object."
  (interactive
   (let* ((buckets-response (ecloud-rpc-list-buckets))
          (bucket-names (mapcar (lambda (b) (plist-get b :name))
                               (plist-get buckets-response :buckets)))
          (bucket (completing-read "Bucket: " bucket-names nil t))
          (objects-response (ecloud-rpc-list-objects bucket))
          (object-names (mapcar (lambda (o) (plist-get o :name))
                               (plist-get objects-response :objects)))
          (object-path (completing-read "Object to delete: " object-names nil t)))
     (list bucket object-path)))
  (when (yes-or-no-p (format "Delete gs://%s/%s? " bucket object-path))
    (message "Deleting %s from %s..." object-path bucket)
    (condition-case err
        (progn
          (ecloud-rpc-delete bucket object-path)
          (message "Deleted gs://%s/%s" bucket object-path))
      (error (message "Delete failed: %s" (error-message-string err))))))

;;;###autoload
(defun ecloud-server-status ()
  "Check the status of the ecloud server."
  (interactive)
  (condition-case err
      (let ((result (ecloud-rpc-ping)))
        (message "ECloud server is running (version: %s)"
                 (plist-get result :version)))
    (error (message "ECloud server is not responding: %s"
                   (error-message-string err)))))

;;;###autoload
(defun ecloud-copy-gs-url ()
  "Interactively select an object and copy its gs:// URL."
  (interactive)
  (let* ((buckets-response (ecloud-rpc-list-buckets))
         (bucket-names (mapcar (lambda (b) (plist-get b :name))
                              (plist-get buckets-response :buckets)))
         (bucket (completing-read "Bucket: " bucket-names nil t))
         (objects-response (ecloud-rpc-list-objects bucket))
         (object-names (mapcar (lambda (o) (plist-get o :name))
                              (plist-get objects-response :objects)))
         (object-path (completing-read "Object: " object-names nil t))
         (url (format "gs://%s/%s" bucket object-path)))
    (kill-new url)
    (message "Copied: %s" url)))

(provide 'ecloud-commands)
;;; ecloud-commands.el ends here
