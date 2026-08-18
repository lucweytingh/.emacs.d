;;; herdr.el --- Native Emacs overview for the herdr agent runtime -*- lexical-binding: t; -*-

;; herdr (https://herdr.dev) is a persistent terminal multiplexer for coding
;; agents: each agent (e.g. Claude) runs as a CLI process in a herdr "pane"
;; (a PTY) on a background server, which tracks status and exposes a socket API.
;;
;; This gives a tabulated overview of the running agents and lets you attach to
;; one full-screen in an `eat' buffer -- the real, interactive terminal, not a
;; polling reimplementation.  `herdr agent attach' scopes to a single pane, so
;; it renders cleanly (unlike embedding the whole multiplexer TUI).
;;
;; Requires `our-herdr' (or `herdr') on PATH and a running herdr session.
;; `M-x herdr-status' opens the overview.

;;; Code:

(require 'tabulated-list)
(require 'json)
(require 'subr-x)
(require 'term)

(declare-function eat-make "eat")

(defgroup herdr nil
  "Native overview for the herdr agent runtime."
  :group 'tools)

(defcustom herdr-executable (or (executable-find "our-herdr") "herdr")
  "Path to the herdr CLI binary."
  :type 'string :group 'herdr)

(defcustom herdr-auto-refresh-seconds 2
  "Seconds between overview auto-refreshes, or nil to disable."
  :type '(choice (const :tag "Off" nil) number) :group 'herdr)

;;;; Socket API

(defun herdr--call (&rest args)
  "Run the herdr CLI with ARGS; return the parsed `result' object.
Signal a `user-error' on a CLI {\"error\":...} reply or non-zero exit."
  (with-temp-buffer
    (let ((status (apply #'call-process herdr-executable nil t nil args)))
      (goto-char (point-min))
      (let* ((obj (condition-case _
                      (json-parse-buffer :object-type 'alist :array-type 'list
                                         :null-object nil :false-object nil)
                    (error
                     (user-error "herdr: unparseable reply (exit %s): %s"
                                 status (string-trim (buffer-string))))))
             (err (alist-get 'error obj)))
        (when err
          (user-error "herdr: %s" (or (alist-get 'message err) err)))
        (unless (eq status 0)
          (user-error "herdr exited with status %s" status))
        (alist-get 'result obj)))))

(defun herdr--rows ()
  "Return one plist per pane from the snapshot, agents merged in.
Keys: :target :ws :name :kind :status :cwd."
  (let* ((snap (alist-get 'snapshot (herdr--call "api" "snapshot")))
         (labels (mapcar (lambda (w)
                           (cons (alist-get 'workspace_id w) (alist-get 'label w)))
                         (alist-get 'workspaces snap)))
         (agents (make-hash-table :test 'equal)))
    (dolist (a (alist-get 'agents snap))
      (puthash (alist-get 'pane_id a) a agents))
    (mapcar
     (lambda (p)
       (let* ((pid (alist-get 'pane_id p))
              (a (gethash pid agents)))
         (list :target pid
               :ws (or (alist-get (alist-get 'workspace_id p) labels nil nil #'equal) "")
               :name (or (and a (alist-get 'name a)) "")
               :kind (or (and a (alist-get 'agent a)) "shell")
               :status (or (alist-get 'agent_status p) "unknown")
               :cwd (or (alist-get 'foreground_cwd p) (alist-get 'cwd p) ""))))
     (alist-get 'panes snap))))

(defun herdr--status-face (status)
  "Face for agent STATUS string."
  (pcase status
    ("working" 'success)
    ("blocked" 'warning)
    ("idle" 'font-lock-keyword-face)
    (_ 'shadow)))

;;;; Overview

(defvar-local herdr--timer nil "Buffer-local auto-refresh timer.")

(defvar-keymap herdr-list-mode-map
  :doc "Keymap for `herdr-list-mode'."
  "RET" #'herdr-attach
  "g"   #'herdr-refresh
  "s"   #'herdr-send
  "f"   #'herdr-focus
  "+"   #'herdr-start-agent)

(define-derived-mode herdr-list-mode tabulated-list-mode "Herdr"
  "Overview of herdr panes/agents.  RET attaches to the agent at point."
  (setq tabulated-list-format
        [("Agent" 16 t) ("Kind" 8 t) ("Status" 9 t) ("Directory" 48 t)]
        tabulated-list-padding 1
        tabulated-list-entries #'herdr--list-entries)
  (tabulated-list-init-header))

(defun herdr--list-entries ()
  "Build `tabulated-list' entries from `herdr--rows'."
  (mapcar
   (lambda (r)
     (let ((status (plist-get r :status)))
       (list (plist-get r :target)
             (vector (let ((n (plist-get r :name)))
                       (if (string-empty-p n) (plist-get r :target) n))
                     (plist-get r :kind)
                     (propertize status 'face (herdr--status-face status))
                     (abbreviate-file-name (plist-get r :cwd))))))
   (herdr--rows)))

(defun herdr-refresh ()
  "Refresh the overview."
  (interactive)
  (tabulated-list-print t))

(defun herdr--install-timer ()
  "Auto-refresh this overview buffer per `herdr-auto-refresh-seconds'."
  (when (and herdr-auto-refresh-seconds (not herdr--timer))
    (let ((buf (current-buffer)))
      (setq herdr--timer
            (run-with-timer
             herdr-auto-refresh-seconds herdr-auto-refresh-seconds
             (lambda ()
               (if (buffer-live-p buf)
                   (with-current-buffer buf (ignore-errors (herdr-refresh)))
                 (ignore-errors (cancel-timer herdr--timer))))))
      (add-hook 'kill-buffer-hook
                (lambda () (when herdr--timer (cancel-timer herdr--timer)))
                nil t))))

;;;###autoload
(defun herdr-status ()
  "Open the herdr overview: agents, status, and directories."
  (interactive)
  (with-current-buffer (get-buffer-create "*herdr*")
    (herdr-list-mode)
    (herdr-refresh)
    (herdr--install-timer)
    (pop-to-buffer (current-buffer))))

;;;; Actions

(defun herdr--target ()
  "Pane id (agent target) on the current overview line."
  (or (tabulated-list-get-id) (user-error "No herdr agent on this line")))

(defun herdr-attach (target)
  "Attach to herdr TARGET full-screen in a terminal buffer.
Uses `eat' when available, else `term'.  Detach with the herdr detach key
\(C-b q by default); the pane keeps running on the server."
  (interactive (list (herdr--target)))
  (let ((name (format "herdr:%s" target))
        (args (list "agent" "attach" "--takeover" target)))
    (pop-to-buffer
     (if (require 'eat nil t)
         (apply #'eat-make name herdr-executable nil args)
       (let ((buf (apply #'make-term name herdr-executable nil args)))
         (with-current-buffer buf (term-mode) (term-char-mode) buf))))))

(defun herdr-send (target text)
  "Send literal TEXT to herdr TARGET without attaching."
  (interactive (let ((tgt (herdr--target)))
                 (list tgt (read-string (format "Send to %s: " tgt)))))
  (herdr--call "agent" "send" target text)
  (message "herdr: sent to %s" target))

(defun herdr-focus (target)
  "Focus herdr TARGET in the running session."
  (interactive (list (herdr--target)))
  (herdr--call "agent" "focus" target)
  (message "herdr: focused %s" target))

(defun herdr-start-agent (name command)
  "Start a new agent NAME running COMMAND (split on whitespace)."
  (interactive (list (read-string "Agent name: ")
                     (read-string "Command: " "claude")))
  (apply #'herdr--call "agent" "start" name "--"
         (split-string-and-unquote command))
  (when (derived-mode-p 'herdr-list-mode) (herdr-refresh))
  (message "herdr: started %s" name))

(provide 'herdr)
;;; herdr.el ends here
