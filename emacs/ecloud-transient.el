;;; ecloud-transient.el --- Transient menus for ECloud -*- lexical-binding: t; -*-

;; Copyright (C) 2024

;; Author: jing
;; Keywords: tools, cloud, transient
;; Package-Requires: ((emacs "28.1") (transient "0.3.0"))

;;; Commentary:

;; This module provides a unified Transient menu interface for ECloud,
;; offering hierarchical menus for accessing all Google Cloud Platform
;; services and operations.
;;
;; The Transient library (built into Emacs 28+) provides a Magit-like
;; menu system with:
;; - Hierarchical navigation (main menu → service submenus → actions)
;; - Visual display of available options and keybindings
;; - Context-aware menus showing current state (e.g., connected cluster)
;;
;; Usage:
;;   M-x ecloud-menu    - Open the main ECloud menu
;;
;; The menu system maintains backward compatibility - all existing
;; ecloud-* commands remain available and can be called directly.

;;; Code:

(require 'transient)

;; Forward declarations for commands used in menus
(declare-function ecloud-browse "ecloud-browser")
(declare-function ecloud-k8s-list "ecloud-k8s")
(declare-function ecloud-k8s-switch-to-pods "ecloud-k8s")
(declare-function ecloud-k8s-switch-to-services "ecloud-k8s")
(declare-function ecloud-k8s-switch-to-ingresses "ecloud-k8s")
(declare-function ecloud-k8s-switch-to-deployments "ecloud-k8s")
(declare-function ecloud-k8s-switch-to-namespaces "ecloud-k8s")
(declare-function ecloud-k8s-helm-list "ecloud-k8s")
(declare-function ecloud-k8s-connect "ecloud-k8s")
(declare-function ecloud-k8s-disconnect "ecloud-k8s")
(declare-function ecloud-compute-list "ecloud-compute")
(declare-function ecloud-sql-list "ecloud-sql")
(declare-function ecloud-gar-browse "ecloud-gar")
(declare-function ecloud-ips-list "ecloud-ips")
(declare-function ecloud-cloud-run-list "ecloud-cloud-run")
(declare-function ecloud-scheduler-list "ecloud-scheduler")
(declare-function ecloud-account-switch "ecloud-account-manager")
(declare-function ecloud-account-list-processes "ecloud-account-manager")
(declare-function ecloud-account-current "ecloud-account-manager")

;; Forward declarations for variables
(defvar ecloud-k8s--current-cluster)

;;; Transient Library Availability Check

(defun ecloud-transient--check-availability ()
  "Check if Transient library is available and functional.
Signal an error with helpful instructions if not available."
  (unless (featurep 'transient)
    (error "Transient library not available. Please upgrade to Emacs 28+ or install the transient package from MELPA/ELPA. You can still use individual ecloud-* commands without the menu interface")))

;; Perform availability check when loading this module
(ecloud-transient--check-availability)

;;; Customization

(defgroup ecloud-transient nil
  "Transient menu interface for ECloud."
  :group 'ecloud
  :prefix "ecloud-transient-")

(defcustom ecloud-transient-show-help t
  "Whether to show help text in transient menus."
  :type 'boolean
  :group 'ecloud-transient)

;;; Main Menu

;;;###autoload
(transient-define-prefix ecloud-menu ()
  "ECloud main menu - Manage Google Cloud Platform resources.

This is the unified entry point for all ECloud functionality.
Select a service to access its features, or use individual
ecloud-* commands directly for quick access."
  [:description
   (lambda ()
     (let ((current-account (when (fboundp 'ecloud-account-current)
                             (ecloud-account-current))))
       (if current-account
           (format "Google Cloud Platform - Account: %s" (symbol-name current-account))
         "Google Cloud Platform")))
   :class transient-columns
   ["Storage & Data"
    ("b" "Browse GCS" ecloud-browse)
    ("s" "Cloud SQL" ecloud-sql-list)]
   ["Compute & Containers"
    ("c" "Compute Engine" ecloud-compute-list)
    ("k" "Kubernetes (GKE)" ecloud-k8s-menu)
    ("r" "Cloud Run" ecloud-cloud-run-list)
    ("S" "Cloud Scheduler" ecloud-scheduler-list)]
   ["Networking & Registry"
    ("i" "IP Addresses" ecloud-ips-list)
    ("a" "Artifact Registry" ecloud-gar-browse)]
   ["Account Management"
    ("A" "Switch Account" ecloud-account-switch)
    ("L" "List Accounts" ecloud-account-list-processes)]]
  ["Actions"
   ("q" "Quit" transient-quit-one)])

;;; Kubernetes Submenu

;;;###autoload
(transient-define-prefix ecloud-k8s-menu ()
  "Kubernetes (GKE) management menu.

Provides access to all Kubernetes resources and cluster operations.
Shows current cluster connection status in the menu header."
  [:description
   (lambda ()
     (if (and (boundp 'ecloud-k8s--current-cluster) ecloud-k8s--current-cluster)
         (format "Kubernetes - Connected: %s (%s)"
                 (or (plist-get ecloud-k8s--current-cluster :name) "unknown")
                 (or (plist-get ecloud-k8s--current-cluster :location) "unknown"))
       "Kubernetes - Not connected"))
   :class transient-columns
   ["Resources"
    ("p" "Pods" ecloud-k8s-switch-to-pods)
    ("s" "Services" ecloud-k8s-switch-to-services)
    ("i" "Ingresses" ecloud-k8s-switch-to-ingresses)
    ("d" "Deployments" ecloud-k8s-switch-to-deployments)
    ("n" "Namespaces" ecloud-k8s-switch-to-namespaces)
    ("h" "Helm Releases" ecloud-k8s-helm-list)]
   ["Cluster Actions"
    ("c" "Connect to cluster" ecloud-k8s-list)
    ("D" "Disconnect" ecloud-k8s-disconnect)]]
  ["Navigation"
   ("q" "Back to main menu" ecloud-menu)
   ("Q" "Quit" transient-quit-one)])

(provide 'ecloud-transient)
;;; ecloud-transient.el ends here
