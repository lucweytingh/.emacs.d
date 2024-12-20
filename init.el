(message "[init.el] Initializing")

;; Increase gc to 500MB for quick & easy startup
(setq gc-cons-threshold (* 1000 1024 1024))

;; https://emacs.stackexchange.com/questions/34342/is-there-any-downside-to-setting-gc-cons-threshold-very-high-and-collecting-ga
;; GC when idling. Also see below.
(run-with-idle-timer 30 t (lambda () (garbage-collect)))

(require 'package)

(setq package-archives '(("melpa" . "https://melpa.org/packages/")
                 ("gnu"          . "https://elpa.gnu.org/packages/")
                ("melpa"        . "https://stable.melpa.org/packages/")
                 ))

(package-initialize)

(eval-after-load 'gnutls
  '(add-to-list 'gnutls-trustfiles "/etc/ssl/cert.pem"))

(eval-when-compile
  (require 'use-package))

(require 'bind-key)
(setq use-package-always-ensure t)

;; add straight as package manager
(defvar bootstrap-version)
(let ((bootstrap-file
       (expand-file-name "straight/repos/straight.el/bootstrap.el" user-emacs-directory))
      (bootstrap-version 5))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
        (url-retrieve-synchronously
         "https://raw.githubusercontent.com/raxod502/straight.el/develop/install.el"
         'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))

(straight-use-package 'use-package)



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

(message "[init.el] Check if README.org exists")
;; Check if README.el exists, if not, run org-to-el with README.org
(let ((readme-org (concat user-emacs-directory "README.org"))
      (readme-el (concat user-emacs-directory "README.el")))
  (unless (file-exists-p readme-el)
    (org-to-el readme-org)))
(message "[init.el] README.org exists")
;; from protesilaos/dotfiles
;; I create an "el" version of my Org configuration file as a final step
;; before closing down Emacs.  This is done to load the latest version
;; of my code upon startup.
;;
;; Also helps with initialisation times.  Not that I care too much about
;; those… Hence why I no longer bother with deferring package loading
;; either by default or on a case-by-case basis.
(defun load-config-org-or-el (fname)
  (let* ((conf (concat user-emacs-directory fname))
         (el (concat conf ".el"))
         (org (concat conf ".org")))
    (if (file-exists-p el)
        (load-file el)
      (use-package org-mode :straight (:type built-in))
      (org-babel-load-file org))))

;; Load README.org - my Emacs configuration
(message "[init.el] Loading README")
(org-babel-load-file (concat user-emacs-directory "README.org"))
(message "[init.el] Finished inititalization")
;; (load-config-org-or-el "README")
