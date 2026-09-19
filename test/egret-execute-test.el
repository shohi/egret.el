;;; egret-execute-test.el --- Tests for egret.el's execution layer  -*- lexical-binding: t; -*-

;;; Commentary:

;; ERT tests for the deterministic parts of egret's execution layer:
;; argument/prefix-arg resolution and command building.  Actually
;; spawning `go test' is exercised manually (see the migration plan),
;; not in this batch-friendly suite.

;;; Code:

(require 'ert)
(require 'egret)

(ert-deftest egret-test-get-arguments-no-prefix ()
  (let ((current-prefix-arg nil)
        (egret-history nil))
    (should (string= "-run 'X' ." (egret--get-arguments "-run 'X' ." 'egret-history)))))

(ert-deftest egret-test-get-arguments-numeric-prefix ()
  (let ((current-prefix-arg 5)
        (egret-history nil))
    (should (string= "-count=5 -run 'X' ."
                      (egret--get-arguments "-run 'X' ." 'egret-history)))))

(ert-deftest egret-test-get-arguments-dash-prefix-reuses-history ()
  (let ((current-prefix-arg '-)
        (egret-history '("-run 'Last' .")))
    (should (string= "-run 'Last' ."
                      (egret--get-arguments "-run 'X' ." 'egret-history)))))

(ert-deftest egret-test-get-arguments-double-prefix-reuses-history ()
  (let ((current-prefix-arg '(16))
        (egret-history '("-run 'Last' .")))
    (should (string= "-run 'Last' ."
                      (egret--get-arguments "-run 'X' ." 'egret-history)))))

(ert-deftest egret-test-get-arguments-prompts-with-single-prefix ()
  (let ((current-prefix-arg '(4))
        (egret-history nil)
        (prompted nil))
    (cl-letf (((symbol-function 'read-shell-command)
               (lambda (prompt default history)
                 (setq prompted (list prompt default history))
                 "-run 'Edited' .")))
      (should (string= "-run 'Edited' ."
                        (egret--get-arguments "-run 'X' ." 'egret-history)))
      (should (equal prompted '("go test args: " "-run 'X' ." egret-history))))))

(ert-deftest egret-test-build-command-plain ()
  (let ((current-prefix-arg nil)
        (egret-history nil)
        (egret-test-args nil)
        (egret-verbose nil))
    (should (string= "go test -run '^TestFoo$' ."
                      (egret--build-command "^TestFoo$")))))

(ert-deftest egret-test-build-command-verbose ()
  (let ((current-prefix-arg nil)
        (egret-history nil)
        (egret-test-args nil)
        (egret-verbose t))
    (should (string= "go test -v -run '^TestFoo$' ."
                      (egret--build-command "^TestFoo$")))))

(ert-deftest egret-test-build-command-extra-args ()
  (let ((current-prefix-arg nil)
        (egret-history nil)
        (egret-test-args "-race")
        (egret-verbose nil))
    (should (string= "go test -race -run '^TestFoo$' ."
                      (egret--build-command "^TestFoo$")))))

(ert-deftest egret-test-build-command-verbose-and-extra-args ()
  (let ((current-prefix-arg nil)
        (egret-history nil)
        (egret-test-args "-race")
        (egret-verbose t))
    (should (string= "go test -v -race -run '^TestFoo$' ."
                      (egret--build-command "^TestFoo$")))))

(ert-deftest egret-test-cleanup-erases-buffer ()
  (let ((buf (generate-new-buffer " *egret-cleanup-test*")))
    (unwind-protect
        (progn
          (with-current-buffer buf (insert "stale output"))
          (egret--cleanup (buffer-name buf))
          (with-current-buffer buf
            (should (= (point-min) (point-max)))))
      (when (buffer-live-p buf) (kill-buffer buf)))))

(provide 'egret-execute-test)

;;; egret-execute-test.el ends here
