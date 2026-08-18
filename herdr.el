;;; herdr.el --- Native Emacs interface to the herdr agent runtime -*- lexical-binding: t; -*-

;; Drives a running herdr server through its CLI/socket API instead of
;; embedding herdr's TUI in a terminal (which renders poorly).  Gives a
;; tabulated dashboard of panes/agents plus read/send/attach/focus actions.
;;
;; Requires the `our-herdr' (or `herdr') binary on PATH and a running herdr
;; session.  Start one with `M-x herdr-status' once herdr is running.

;;; Code:

(require 'tabulated-list)
(require 'ansi-color)
(require 'json)
(require 'subr-x)
(require 'term)

(defgroup herdr nil
  "Native interface to the herdr agent runtime."
  :group 'tools)

(defcustom herdr-executable (or (executable-find "our-herdr") "herdr")
  "Path to the herdr CLI binary."
  :type 'string :group 'herdr)

(defcustom herdr-auto-refresh-seconds 2
  "Seconds between auto-refreshes of herdr buffers, or nil to disable."
  :type '(choice (const :tag "Off" nil) number) :group 'herdr)

(defcustom herdr-output-lines 300
  "Number of scrollback lines to fetch when viewing pane output."
  :type 'integer :group 'herdr)

;;;; Core API

(defun herdr--call (&rest args)
  "Run the herdr CLI with ARGS, returning the parsed `result' object.
Signal a `user-error' on a CLI-level {\"error\":...} reply or non-zero exit."
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

(defun herdr--snapshot ()
  "Return the current session snapshot as an alist."
  (alist-get 'snapshot (herdr--call "api" "snapshot")))

(defun herdr--panes ()
  "Return pane alists from the snapshot, each with a `ws_label' added."
  (let* ((snap (herdr--snapshot))
         (labels (mapcar (lambda (w)
                           (cons (alist-get 'workspace_id w) (alist-get 'label w)))
                         (alist-get 'workspaces snap))))
    (mapcar (lambda (p)
              (cons (cons 'ws_label
                          (alist-get (alist-get 'workspace_id p) labels
                                     nil nil #'equal))
                    p))
            (alist-get 'panes snap))))

(defun herdr--status-face (status)
  "Face for agent STATUS string."
  (pcase status
    ("working" 'success)
    ("blocked" 'warning)
    (_ 'shadow)))

;;;; Dashboard

(defvar-local herdr--timer nil "Buffer-local auto-refresh timer.")

(defun herdr--install-timer (refresh-fn)
  "Auto-run REFRESH-FN in the current buffer per `herdr-auto-refresh-seconds'."
  (when (and herdr-auto-refresh-seconds (not herdr--timer))
    (let ((buf (current-buffer)))
      (setq herdr--timer
            (run-with-timer
             herdr-auto-refresh-seconds herdr-auto-refresh-seconds
             (lambda ()
               (if (buffer-live-p buf)
                   (with-current-buffer buf (ignore-errors (funcall refresh-fn)))
                 (ignore-errors (cancel-timer herdr--timer))))))
      (add-hook 'kill-buffer-hook
                (lambda () (when herdr--timer (cancel-timer herdr--timer)))
                nil t))))

(defvar-keymap herdr-list-mode-map
  :doc "Keymap for `herdr-list-mode'."
  "g"   #'herdr-refresh
  "RET" #'herdr-view-output
  "s"   #'herdr-send
  "f"   #'herdr-focus
  "a"   #'herdr-attach
  "+"   #'herdr-start-agent)

(define-derived-mode herdr-list-mode tabulated-list-mode "Herdr"
  "Major mode for the herdr pane/agent dashboard."
  (setq tabulated-list-format
        [("WS" 10 t) ("Pane" 8 t) ("Status" 9 t) ("Directory" 50 t)]
        tabulated-list-padding 1
        tabulated-list-entries #'herdr--list-entries)
  (tabulated-list-init-header))

(defun herdr--list-entries ()
  "Build `tabulated-list' entries from the current panes."
  (mapcar
   (lambda (p)
     (let ((status (or (alist-get 'agent_status p) "unknown")))
       (list (alist-get 'pane_id p)
             (vector (or (alist-get 'ws_label p) "")
                     (or (alist-get 'pane_id p) "")
                     (propertize status 'face (herdr--status-face status))
                     (abbreviate-file-name
                      (or (alist-get 'foreground_cwd p) (alist-get 'cwd p) ""))))))
   (herdr--panes)))

(defun herdr-refresh ()
  "Refresh the herdr dashboard."
  (interactive)
  (tabulated-list-print t))

;;;###autoload
(defun herdr-status ()
  "Open the herdr dashboard listing panes and agent status."
  (interactive)
  (with-current-buffer (get-buffer-create "*herdr*")
    (herdr-list-mode)
    (herdr-refresh)
    (herdr--install-timer #'herdr-refresh)
    (pop-to-buffer (current-buffer))))

;;;; Per-pane actions

(defun herdr--target ()
  "The pane id on the current dashboard line."
  (or (tabulated-list-get-id) (user-error "No herdr pane on this line")))

(defun herdr-send (target text)
  "Send TEXT to herdr TARGET (literal input, no trailing newline appended)."
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

(defun herdr-attach (target)
  "Attach to herdr TARGET in a dedicated `term' buffer.
A single pane renders fine in `term'; only the full multiplexer TUI does not."
  (interactive (list (herdr--target)))
  (let ((buf (make-term (format "herdr-attach:%s" target)
                        herdr-executable nil "agent" "attach" target)))
    (with-current-buffer buf (term-mode) (term-char-mode))
    (switch-to-buffer buf)))

;;;; Pane output viewer

(defvar-local herdr--output-target nil "Pane id shown in this output buffer.")

(define-derived-mode herdr-output-mode special-mode "Herdr-Out"
  "View a herdr pane's recent output, refreshed in place.")

(defun herdr--output-refresh ()
  "Re-read the target pane's output into the current buffer."
  (let* ((res (herdr--call "agent" "read" herdr--output-target
                           "--lines" (number-to-string herdr-output-lines)
                           "--format" "ansi"))
         (text (alist-get 'text (alist-get 'read res)))
         (at-end (>= (point) (point-max)))
         (inhibit-read-only t))
    (erase-buffer)
    (insert (ansi-color-apply (or text "")))
    (when at-end (goto-char (point-max)))))

(defun herdr-view-output ()
  "Open a live-refreshing view of the pane output at point."
  (interactive)
  (let ((target (herdr--target)))
    (with-current-buffer (get-buffer-create (format "*herdr:%s*" target))
      (herdr-output-mode)
      (setq herdr--output-target target)
      (herdr--output-refresh)
      (herdr--install-timer #'herdr--output-refresh)
      (pop-to-buffer (current-buffer)))))

(provide 'herdr)
;;; herdr.el ends here
