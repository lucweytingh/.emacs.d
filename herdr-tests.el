;;; herdr-tests.el --- Tests for herdr.el -*- lexical-binding: t; -*-

;; Run:
;;   emacs -Q --batch -l herdr.el -l herdr-tests.el -f ert-run-tests-batch-and-exit
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

(provide 'herdr-tests)
;;; herdr-tests.el ends here
