;; -*- lexical-binding: t -*-
(message "[init.el] Initializing")

;; Increase gc to 500MB for quick & easy startup
(setq gc-cons-threshold (* 1000 1024 1024))

;; https://emacs.stackexchange.com/questions/34342/is-there-any-downside-to-setting-gc-cons-threshold-very-high-and-collecting-ga
;; GC when idling. Also see below.
(run-with-idle-timer 30 t (lambda () (garbage-collect)))

;; Bootstrap straight.el package manager
(message "[init.el] Bootstrapping straight.el...")
(defvar bootstrap-version)
(let ((bootstrap-file
       (expand-file-name "straight/repos/straight.el/bootstrap.el" user-emacs-directory))
      (bootstrap-version 5))
  (unless (file-exists-p bootstrap-file)
    (message "Installing straight.el...")
    (with-current-buffer
        (url-retrieve-synchronously
         "https://raw.githubusercontent.com/raxod502/straight.el/develop/install.el"
         'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (condition-case err
      (load bootstrap-file nil 'nomessage)
    (error
     (message "Failed to load straight.el: %s" err)
     (error "Cannot continue without straight.el"))))

(message "[init.el] Installing core packages...")
;; Install and configure use-package via straight.el
(straight-use-package 'use-package)
(straight-use-package 'bind-key)

;; Configure use-package to use straight.el
(setq use-package-always-ensure t)
(setq use-package-verbose t)
(setq use-package-ensure-function 'straight-use-package)

;; Disable asking for confirmation for the following commands
(put 'narrow-to-region 'disabled nil)
(put 'downcase-region 'disabled nil)
(put 'upcase-region 'disabled nil)
(put 'erase-buffer 'disabled nil)

;; Set the file encoding based on os
(cond
 ((eq system-type 'darwin)
  ;; macOS-specific settings
  (prefer-coding-system 'utf-8-unix)
  (setq-default buffer-file-coding-system 'utf-8-unix)
  (setq coding-system-for-read 'utf-8-unix
        coding-system-for-write 'utf-8-unix))
 ((eq system-type 'windows-nt)
  ;; Message when Windows is detected
  (message "Windows is not supported."))
 (t
  ;; Default settings for other systems (e.g., Ubuntu/Linux)
  (prefer-coding-system 'utf-8-unix)
  (setq-default buffer-file-coding-system 'utf-8-unix)
  (setq coding-system-for-read 'utf-8-unix
        coding-system-for-write 'utf-8-unix)))

;; Ensure org-mode is loaded before using org functions
(message "[init.el] Loading org-mode...")
(require 'org)

;; Function that compiles org to el
(defun org-to-el (&optional org-file)
  "Export the specified Org file (or current buffer if none specified) to Emacs Lisp."
  (interactive)
  (let* ((org-file (or org-file (buffer-file-name)))
         (output-file (concat (file-name-sans-extension org-file) ".el")))
    (if (file-exists-p output-file)
        (message "%s already exists. No need to export." output-file)
      (with-current-buffer (find-file-noselect org-file)
        (org-babel-tangle)
        (write-region (point-min) (point-max) output-file)
        (message "Exported Org file to %s" output-file)))))

(message "[init.el] Checking README.el status...")
;; Check if README.el exists, if not, generate it from README.org
(let ((readme-org (concat user-emacs-directory "README.org"))
      (readme-el (concat user-emacs-directory "README.el")))
  (if (file-exists-p readme-el)
      (message "README.el found, no generation needed")
    (message "README.el not found, generating from README.org...")
    (condition-case err
        (org-to-el readme-org)
      (error
       (message "Failed to generate README.el: %s" err)
       (message "Please ensure org-mode is available")))))
(message "[init.el] README.el check complete")

;; Load README.org - my Emacs configuration
(message "[init.el] Loading README")
(condition-case err
    (org-babel-load-file (concat user-emacs-directory "README.org"))
  (error
   (message "Failed to load README.org: %s" err)
   (message "Please check the file for syntax errors")))
(message "[init.el] Finished initialization")