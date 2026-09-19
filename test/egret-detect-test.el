;;; egret-detect-test.el --- Tests for egret.el's detection layer  -*- lexical-binding: t; -*-

;;; Commentary:

;; ERT tests for the tree-sitter detection core in egret.el.  Requires
;; the Go tree-sitter grammar to be installed
;; (`M-x treesit-install-language-grammar RET go').
;;
;; Run with:
;;   emacs -Q --batch -L .. -l ert -l egret.el -l egret-detect-test.el \
;;     -f ert-run-tests-batch-and-exit

;;; Code:

(require 'ert)
(require 'egret)

(defconst egret-test--dir
  (file-name-directory (or load-file-name buffer-file-name default-directory))
  "Directory containing this test file.")

(defconst egret-test--fixtures-dir
  (expand-file-name "fixtures/" egret-test--dir)
  "Directory containing egret's tree-sitter fixture files.")

(defmacro egret-test--with-fixture (filename &rest body)
  "Visit FILENAME (relative to the fixtures dir) into a temp buffer.
Enables `go-ts-mode' and runs BODY with point at `point-min'."
  (declare (indent 1))
  `(with-temp-buffer
     (insert-file-contents (expand-file-name ,filename egret-test--fixtures-dir))
     (go-ts-mode)
     (goto-char (point-min))
     ,@body))

(defun egret-test--goto-substring (needle)
  "Move point to the middle of the first occurrence of NEEDLE."
  (goto-char (point-min))
  (search-forward needle)
  (backward-char (/ (length needle) 2)))

(ert-deftest egret-test-plain-function ()
  (egret-test--with-fixture "detect_test.go"
    (egret-test--goto-substring "t.Log(\"plain\")")
    (should (string= "TestPlain" (egret--test-function-name-at-point)))
    (should (string= "^TestPlain$" (egret--run-pattern-at-point)))
    (should (string= "^TestPlain$" (egret--enclosing-run-target-at-point)))))

(ert-deftest egret-test-subtest-case-one ()
  (egret-test--with-fixture "detect_test.go"
    (egret-test--goto-substring "case one")
    (should (string= "case one" (egret--subtest-name-at-point)))
    (should (string= "TestTable" (egret--test-function-name-at-point)))
    (should (string= "^TestTable$/^case_one$" (egret--run-pattern-at-point)))
    (should (string= "^TestTable$" (egret--enclosing-run-target-at-point)))))

(ert-deftest egret-test-subtest-case-two ()
  (egret-test--with-fixture "detect_test.go"
    (egret-test--goto-substring "case two")
    (should (string= "case two" (egret--subtest-name-at-point)))
    (should (string= "^TestTable$/^case_two$" (egret--run-pattern-at-point)))))

(ert-deftest egret-test-table-function-header ()
  "Point on the enclosing function, outside any subtest literal."
  (egret-test--with-fixture "detect_test.go"
    (egret-test--goto-substring "func TestTable")
    (should (null (egret--subtest-name-at-point)))
    (should (string= "^TestTable$" (egret--run-pattern-at-point)))))

(ert-deftest egret-test-suite-method ()
  (egret-test--with-fixture "detect_test.go"
    (egret-test--goto-substring "_ = s")
    (should (equal '("TestFoo" . "TestBar") (egret--suite-method-info-at-point)))
    (should (string= "^TestFoo$/^TestBar$" (egret--run-pattern-at-point)))
    (should (string= "^TestFoo$" (egret--enclosing-run-target-at-point)))))

(ert-deftest egret-test-not-in-test-function ()
  "Point outside any function should signal a `user-error'."
  (egret-test--with-fixture "detect_test.go"
    (goto-char (point-min))
    (should-error (egret--run-pattern-at-point) :type 'user-error)))

(ert-deftest egret-test-subtest-name-normalization ()
  (should (string= "case_one" (egret--normalize-subtest-name "case one")))
  (should (string= (regexp-quote "a.b") (egret--normalize-subtest-name "a.b"))))

(provide 'egret-detect-test)

;;; egret-detect-test.el ends here
