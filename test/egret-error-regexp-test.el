;;; egret-error-regexp-test.el --- Tests for egret's next-error regexps  -*- lexical-binding: t; -*-

;;; Commentary:

;; Regression tests for `egret-compilation-error-regexp-alist-alist',
;; using verbatim sample lines captured from a real `go test' run
;; (Go 1.27, testify v1.12.1), not synthesized guesses.  Caught two
;; real bugs during manual end-to-end verification:
;;
;; - `egret-testing' assumed a literal tab before the file:line;
;;   current `go test' indents with spaces instead.
;; - `egret-testify' assumed the old "Location:\t<file>:<line>"
;;   format; current testify prints "Error Trace:\t<file>:<line>"
;;   instead.

;;; Code:

(require 'ert)
(require 'egret)

(defun egret-error-regexp-test--match (symbol line)
  "Match LINE against the regexp for SYMBOL in
`egret-compilation-error-regexp-alist-alist'.  Return (FILE . LINE-NO)
on match, or nil."
  (let* ((spec (alist-get symbol egret-compilation-error-regexp-alist-alist))
         (regexp (nth 0 spec))
         (file-group (nth 1 spec))
         (line-group (nth 2 spec)))
    (when (string-match regexp line)
      (cons (match-string file-group line)
            (match-string line-group line)))))

(ert-deftest egret-test-error-regexp-testing-space-indented ()
  "Current `go test' indents with spaces, not a tab."
  (should (equal '("main_test.go" . "9")
                  (egret-error-regexp-test--match
                   'egret-testing "    main_test.go:9: boom"))))

(ert-deftest egret-test-error-regexp-testing-tab-indented-still-works ()
  "Older Go versions/tools may still emit a literal tab; keep matching it."
  (should (equal '("main_test.go" . "9")
                  (egret-error-regexp-test--match
                   'egret-testing "\tmain_test.go:9: boom"))))

(ert-deftest egret-test-error-regexp-testify-error-trace ()
  "Current testify (v1.12.1) prints \"Error Trace:\", not \"Location:\"."
  (should (equal '("/tmp/proj/testify_test.go" . "10")
                  (egret-error-regexp-test--match
                   'egret-testify
                   "        \tError Trace:\t/tmp/proj/testify_test.go:10"))))

(ert-deftest egret-test-error-regexp-gopanic ()
  (should (equal '("/usr/local/go/src/runtime/panic.go" . "859")
                  (egret-error-regexp-test--match
                   'egret-gopanic
                   "\t/usr/local/go/src/runtime/panic.go:859 +0x120"))))

(ert-deftest egret-test-error-regexp-compile ()
  "`go test' build-failure line, relative path with leading \"./\"."
  (should (equal '("./compileerr_test.go" . "6")
                  (egret-error-regexp-test--match
                   'egret-compile
                   "./compileerr_test.go:6:2: undefined: undefinedFunc"))))

(ert-deftest egret-test-error-regexp-linkage ()
  (should (equal '("./foo_test.go" . "174")
                  (egret-error-regexp-test--match
                   'egret-linkage
                   "./foo_test.go:174: undefined: foo.Symbol"))))

(provide 'egret-error-regexp-test)

;;; egret-error-regexp-test.el ends here
