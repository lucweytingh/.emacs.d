;;; herdr-tests.el --- Tests for herdr.el -*- lexical-binding: t; -*-

;; Run:
;;   emacs -Q --batch -l lisp/herdr.el -l lisp/herdr-tests.el -f ert-run-tests-batch-and-exit
;;
;; Most tests use a mock `herdr' script (no server needed).  The live test
;; skips itself when no herdr server is reachable.

;;; Code:

(require 'ert)
(require 'herdr)

(defconst herdr-tests--snapshot-json
  (concat
   "{\"id\":\"cli:api:snapshot\",\"result\":{\"snapshot\":{"
   "\"agents\":[{\"agent\":\"claude\",\"agent_status\":\"idle\",\"name\":\"bricks\","
   "\"pane_id\":\"w1:p2\",\"cwd\":\"/tmp\",\"workspace_id\":\"w1\"}],"
   "\"panes\":[{\"pane_id\":\"w1:p1\",\"agent_status\":\"unknown\",\"cwd\":\"/tmp\",\"workspace_id\":\"w1\"},"
   "{\"pane_id\":\"w1:p2\",\"agent_status\":\"idle\",\"cwd\":\"/tmp\",\"workspace_id\":\"w1\"}],"
   "\"workspaces\":[{\"workspace_id\":\"w1\",\"label\":\"pattaya\"}]},"
   "\"type\":\"session_snapshot\"}}")
  "Canned snapshot: one claude agent (bricks) in w1:p2, plain shell in w1:p1.")

(defmacro herdr-tests--with-mock (reply &rest body)
  "Run BODY with `herdr-executable' bound to a script that prints REPLY.
The script appends each argv line to a file; its path is bound to `record'."
  (declare (indent 1))
  `(let* ((dir (make-temp-file "herdr-mock" t))
          (record (expand-file-name "argv" dir))
          (script (expand-file-name "herdr" dir))
          (herdr-executable script))
     (unwind-protect
         (progn
           (with-temp-file script
             (insert "#!/bin/sh\n"
                     "printf '%s\\n' \"$@\" >> " (shell-quote-argument record) "\n"
                     "cat <<'EOF'\n" ,reply "\nEOF\n"))
           (set-file-modes script #o755)
           ,@body)
       (delete-directory dir t))))

(defun herdr-tests--recorded (record)
  "Argv lines the mock binary received, as a list."
  (with-temp-buffer
    (insert-file-contents record)
    (split-string (buffer-string) "\n" t)))

(ert-deftest herdr-rows-merges-agents-into-panes ()
  "Overview rows show agent name/kind; agentless panes fall back to shell."
  (herdr-tests--with-mock herdr-tests--snapshot-json
    (let ((rows (herdr--rows)))
      (should (equal (herdr-tests--recorded record) '("api" "snapshot")))
      (should (= (length rows) 2))
      (let ((shell (car rows)) (agent (cadr rows)))
        (should (equal (plist-get shell :target) "w1:p1"))
        (should (equal (plist-get shell :kind) "shell"))
        (should (equal (plist-get shell :name) ""))
        (should (equal (plist-get agent :target) "w1:p2"))
        (should (equal (plist-get agent :name) "bricks"))
        (should (equal (plist-get agent :kind) "claude"))
        (should (equal (plist-get agent :status) "idle"))
        (should (equal (plist-get agent :ws) "pattaya"))))))

(ert-deftest herdr-list-entries-names-over-pane-ids ()
  "The rendered table shows the agent name, not the bare pane id."
  (herdr-tests--with-mock herdr-tests--snapshot-json
    (let ((entries (herdr--list-entries)))
      (should (equal (aref (cadr (car entries)) 0) "w1:p1")) ; no name -> id
      (should (equal (aref (cadr (cadr entries)) 0) "bricks")))))

(ert-deftest herdr-call-error-reply-signals-user-error ()
  "A CLI {\"error\":...} reply becomes a readable user-error."
  (herdr-tests--with-mock
      "{\"error\":{\"code\":\"agent_not_found\",\"message\":\"agent target x not found\"},\"id\":\"i\"}"
    (let ((err (should-error (herdr--call "agent" "get" "x") :type 'user-error)))
      (should (string-match-p "not found" (cadr err))))))

(ert-deftest herdr-attach-arg-order-target-before-takeover ()
  "Regression: attach must pass `agent attach TARGET --takeover'.
--takeover before the target made the CLI reject it as `unknown option'."
  (herdr-tests--with-mock "{}"
    (let (spawn-args)
      (cl-letf (((symbol-function 'require) (lambda (&rest _) nil)) ; force term path
                ((symbol-function 'make-term)
                 (lambda (_name program _startfile &rest switches)
                   (setq spawn-args (cons program switches))
                   (generate-new-buffer " *herdr-test-term*")))
                ((symbol-function 'term-mode) #'ignore)
                ((symbol-function 'term-char-mode) #'ignore)
                ((symbol-function 'pop-to-buffer) #'identity))
        (herdr-attach "w1:p3")
        (should (equal (cdr spawn-args)
                       '("agent" "attach" "w1:p3" "--takeover")))))))

(ert-deftest herdr-send-passes-literal-text ()
  "Send hands the target and literal text through to the CLI."
  (herdr-tests--with-mock "{\"id\":\"i\",\"result\":{}}"
    (herdr-send "w1:p2" "hello world")
    (should (equal (herdr-tests--recorded record)
                   '("agent" "send" "w1:p2" "hello world")))))

(ert-deftest herdr-live-cli-accepts-attach-arg-order ()
  "Against a real server, our attach argv parses (fails as not-found, not
as unknown-option).  Skipped when no herdr server is reachable."
  (let ((herdr-executable (or (executable-find "our-herdr")
                              (executable-find "herdr"))))
    (skip-unless (and herdr-executable
                      (ignore-errors (herdr--call "api" "snapshot"))))
    (let ((err (should-error
                (herdr--call "agent" "attach" "herdr-tests-no-such-agent" "--takeover")
                :type 'user-error)))
      (should (string-match-p "not found" (cadr err)))
      (should-not (string-match-p "unknown option" (cadr err))))))

(ert-deftest herdr-cycle-target-orders-and-wraps ()
  "C-M-n/C-M-p cycle agents in stable order and wrap at the ends."
  (herdr-tests--with-mock
      (concat "{\"id\":\"i\",\"result\":{\"snapshot\":{\"agents\":["
              "{\"pane_id\":\"w1:p4\"},{\"pane_id\":\"w1:p2\"},{\"pane_id\":\"w1:p3\"}],"
              "\"panes\":[],\"workspaces\":[]},\"type\":\"session_snapshot\"}}")
    (should (equal (herdr--cycle-target "w1:p2" 1) "w1:p3"))
    (should (equal (herdr--cycle-target "w1:p3" 1) "w1:p4"))
    (should (equal (herdr--cycle-target "w1:p4" 1) "w1:p2"))   ; wrap forward
    (should (equal (herdr--cycle-target "w1:p2" -1) "w1:p4"))  ; wrap backward
    ;; unknown current (e.g. TUI focused on a non-agent pane) -> first agent
    (should (equal (herdr--cycle-target "w1:p9" 1) "w1:p2"))))

(ert-deftest herdr-eat-spawn-displays-buffer-before-exec ()
  "Regression: the buffer must be shown in a window BEFORE eat-exec runs.
eat sizes the terminal at exec from the displaying window; exec-then-display
left the TUI at eat's 80x24 default (tiny corner render) until a resize."
  (let ((eat-dir (expand-file-name "~/.emacs.d/straight/build/eat")))
    (when (file-directory-p eat-dir) (add-to-list 'load-path eat-dir)))
  (skip-unless (require 'eat nil t))
  (let (window-at-exec)
    (cl-letf (((symbol-function 'eat-exec)
               (lambda (buffer &rest _)
                 (setq window-at-exec (get-buffer-window buffer)))))
      (unwind-protect
          (save-window-excursion
            (herdr--eat-spawn "herdr-spawn-test" nil)
            (should (windowp window-at-exec)))
        (kill-buffer "*herdr-spawn-test*")))))

(ert-deftest herdr-tui-requires-eat ()
  "Without eat available, `herdr-tui' fails with a readable error."
  (cl-letf (((symbol-function 'require) (lambda (&rest _) nil)))
    (should-error (herdr-tui) :type 'user-error)))

(ert-deftest herdr-tui-live-char-mode-passes-emacs-chords ()
  "The TUI buffer is in char mode so C-x/C-s reach herdr's Emacs layer.
Skipped without eat or a reachable herdr server."
  (let ((eat-dir (expand-file-name "~/.emacs.d/straight/build/eat")))
    (when (file-directory-p eat-dir) (add-to-list 'load-path eat-dir)))
  (let ((herdr-executable (or (executable-find "our-herdr")
                              (executable-find "herdr"))))
    (skip-unless (and herdr-executable
                      (require 'eat nil t)
                      (ignore-errors (herdr--call "api" "snapshot"))))
    (save-window-excursion
      (herdr-tui)
      (unwind-protect
          (with-current-buffer "*herdr-tui*"
            (sleep-for 2)
            (should (bound-and-true-p eat--char-mode))
            (should (eq (key-binding (kbd "C-x")) 'eat-self-input))
            (should (eq (key-binding (kbd "C-s")) 'eat-self-input))
            (should (eq (key-binding (kbd "C-M-m")) 'eat-semi-char-mode))
            ;; agent cycling overrides eat's pass-through, nothing else
            (should (eq (key-binding (kbd "C-M-n")) 'herdr-next-agent))
            (should (eq (key-binding (kbd "C-M-p")) 'herdr-prev-agent))
            ;; M-x stays Emacs's, not herdr's palette
            (should (eq (key-binding (kbd "M-x")) 'execute-extended-command))
            ;; latency raised so full ratatui frames render per redisplay
            (should (>= eat-minimum-latency 0.033))
            (should (get-buffer-process (current-buffer))))
        (let ((proc (get-buffer-process "*herdr-tui*")))
          (when proc (delete-process proc)))
        (kill-buffer "*herdr-tui*")))))

(provide 'herdr-tests)
;;; herdr-tests.el ends here
