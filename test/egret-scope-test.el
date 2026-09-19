;;; egret-scope-test.el --- Tests for egret.el's file/package/project/last scopes  -*- lexical-binding: t; -*-

;;; Commentary:

;; ERT tests for `egret-run-file', `egret-run-package',
;; `egret-run-project', and `egret-run-last'.  All stub `egret--start'
;; (the single low-level entry point that would otherwise spawn a real
;; process) to capture the resolved command instead of running it.

;;; Code:

(require 'ert)
(require 'cl-lib)
(require 'egret)

(defconst egret-scope-test--dir
  (file-name-directory (or load-file-name buffer-file-name default-directory)))

(defconst egret-scope-test--fixtures-dir
  (expand-file-name "fixtures/" egret-scope-test--dir))

(defmacro egret-scope-test--capturing-start (captured-var &rest body)
  "Run BODY with `egret--start' stubbed to set CAPTURED-VAR instead
of actually starting a compilation process."
  (declare (indent 1))
  `(let (,captured-var)
     (cl-letf (((symbol-function 'egret--start)
                (lambda (command) (setq ,captured-var command))))
       ,@body)
     ,captured-var))

(ert-deftest egret-test-file-test-names ()
  (with-temp-buffer
    (insert-file-contents (expand-file-name "detect_test.go" egret-scope-test--fixtures-dir))
    (go-ts-mode)
    (should (equal '("TestPlain" "TestTable" "TestFooSuite")
                    (egret--file-test-names)))))

(ert-deftest egret-test-run-file-unions-all-names ()
  (with-temp-buffer
    (insert-file-contents (expand-file-name "detect_test.go" egret-scope-test--fixtures-dir))
    (go-ts-mode)
    (let* ((current-prefix-arg nil)
           (egret-history nil)
           (egret-test-args nil)
           (egret-verbose nil)
           (captured (egret-scope-test--capturing-start c (egret-run-file))))
      (should (string= "go test -run '^TestPlain$|^TestTable$|^TestFooSuite$' ."
                        captured)))))

(ert-deftest egret-test-run-package-uses-dot ()
  (let* ((current-prefix-arg nil)
         (egret-history nil)
         (egret-test-args nil)
         (egret-verbose nil)
         (captured (egret-scope-test--capturing-start c (egret-run-package))))
    (should (string= "go test ." captured))))

(ert-deftest egret-test-run-project-joins-packages-excludes-vendor ()
  (let* ((current-prefix-arg nil)
         (egret-history nil)
         (egret-test-args nil)
         (egret-verbose nil))
    (cl-letf (((symbol-function 'shell-command-to-string)
               (lambda (_cmd) "example.com/foo\nexample.com/foo/vendor/bar\nexample.com/foo/baz\n")))
      (let ((captured (egret-scope-test--capturing-start c (egret-run-project))))
        (should (string= "go test example.com/foo example.com/foo/baz" captured))))))

(ert-deftest egret-test-run-project-errors-when-no-packages ()
  (cl-letf (((symbol-function 'shell-command-to-string) (lambda (_cmd) "")))
    (should-error (egret-run-project) :type 'user-error)))

(ert-deftest egret-test-run-last-replays-verbatim ()
  (let ((egret-last-command "go test -run '^TestFoo$' ."))
    (let ((captured (egret-scope-test--capturing-start c (egret-run-last))))
      (should (string= "go test -run '^TestFoo$' ." captured)))))

(ert-deftest egret-test-run-last-errors-when-empty ()
  (let ((egret-last-command nil))
    (should-error (egret-run-last) :type 'user-error)))

(provide 'egret-scope-test)

;;; egret-scope-test.el ends here
