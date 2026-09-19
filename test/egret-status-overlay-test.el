;;; egret-status-overlay-test.el --- Tests for inline pass/fail overlays  -*- lexical-binding: t; -*-

;;; Commentary:

;; ERT tests for `egret--parse-test-statuses', `egret--apply-status-overlays',
;; and `egret-status-overlay-mode'.  The parser fixtures use a verbatim
;; sample captured from a real `go test -v' run (table-driven subtest
;; failure rolled up into its parent, plain pass, and skip), not a
;; synthesized guess.  A real end-to-end run (see the migration plan)
;; confirms `egret--maybe-refresh-status-overlays' wiring separately.

;;; Code:

(require 'ert)
(require 'egret)

(defconst egret-status-test--sample-output
  "=== RUN   TestTable
=== RUN   TestTable/ok_case
=== RUN   TestTable/bad_case
    status_test.go:16: boom
--- FAIL: TestTable (0.00s)
    --- PASS: TestTable/ok_case (0.00s)
    --- FAIL: TestTable/bad_case (0.00s)
=== RUN   TestPlainPass
--- PASS: TestPlainPass (0.00s)
=== RUN   TestSkipped
    status_test.go:25: skip reason
--- SKIP: TestSkipped (0.00s)
FAIL
FAIL	egretstatus	0.158s
FAIL
"
  "Verbatim `go test -v' output, table-driven fail rolled into parent.")

(ert-deftest egret-test-parse-test-statuses ()
  (should (equal '(("TestTable" . fail)
                    ("TestPlainPass" . pass)
                    ("TestSkipped" . skip))
                  (egret--parse-test-statuses egret-status-test--sample-output))))

(ert-deftest egret-test-parse-test-statuses-ignores-indented-subtest-lines ()
  "Only unindented top-level lines are considered function-level."
  (let ((statuses (egret--parse-test-statuses egret-status-test--sample-output)))
    (should-not (assoc "TestTable/ok_case" statuses))
    (should-not (assoc "TestTable/bad_case" statuses))))

(ert-deftest egret-test-apply-status-overlays-marks-pass-and-fail-not-skip ()
  (with-temp-buffer
    (insert "package sample\n\n"
            "func TestTable(t *testing.T) {}\n\n"
            "func TestPlainPass(t *testing.T) {}\n\n"
            "func TestSkipped(t *testing.T) {}\n")
    (go-ts-mode)
    (egret--apply-status-overlays
     (egret--parse-test-statuses egret-status-test--sample-output))
    (should (= 2 (length egret--status-overlays)))
    (let* ((faces (mapcar (lambda (ov) (overlay-get ov 'face)) egret--status-overlays)))
      (should (member 'egret-status-fail-face faces))
      (should (member 'egret-status-pass-face faces))
      (should-not (member nil faces)))))

(ert-deftest egret-test-apply-status-overlays-clears-previous ()
  (with-temp-buffer
    (insert "package sample\n\nfunc TestPlainPass(t *testing.T) {}\n")
    (go-ts-mode)
    (egret--apply-status-overlays '(("TestPlainPass" . fail)))
    (should (= 1 (length egret--status-overlays)))
    (egret--apply-status-overlays '(("TestPlainPass" . pass)))
    (should (= 1 (length egret--status-overlays)))
    (should (eq 'egret-status-pass-face
                (overlay-get (car egret--status-overlays) 'face)))))

(ert-deftest egret-test-status-overlay-mode-clears-on-disable ()
  (with-temp-buffer
    (insert "package sample\n\nfunc TestPlainPass(t *testing.T) {}\n")
    (go-ts-mode)
    (egret--apply-status-overlays '(("TestPlainPass" . pass)))
    (should egret--status-overlays)
    (egret-status-overlay-mode 1)
    (egret-status-overlay-mode -1)
    (should (null egret--status-overlays))))

(ert-deftest egret-test-maybe-refresh-status-overlays-noop-when-mode-disabled ()
  (with-temp-buffer
    (insert "package sample\n\nfunc TestPlainPass(t *testing.T) {}\n")
    (go-ts-mode)
    (egret-status-overlay-mode -1) ; explicit: disabled
    (let ((src (current-buffer)))
      (with-temp-buffer
        (rename-buffer egret--buffer-name t)
        (insert "--- PASS: TestPlainPass (0.00s)\n")
        (egret--maybe-refresh-status-overlays src)))
    (should (null egret--status-overlays))))

(ert-deftest egret-test-maybe-refresh-status-overlays-applies-when-mode-enabled ()
  (with-temp-buffer
    (insert "package sample\n\nfunc TestPlainPass(t *testing.T) {}\n")
    (go-ts-mode)
    (egret-status-overlay-mode 1)
    (let ((src (current-buffer)))
      (with-temp-buffer
        (rename-buffer egret--buffer-name t)
        (insert "--- PASS: TestPlainPass (0.00s)\n")
        (egret--maybe-refresh-status-overlays src))
      (should (= 1 (length egret--status-overlays)))
      (should (eq 'egret-status-pass-face
                  (overlay-get (car egret--status-overlays) 'face))))))

(provide 'egret-status-overlay-test)

;;; egret-status-overlay-test.el ends here
