;;; egret-region-test.el --- Tests for egret-dwim's region support  -*- lexical-binding: t; -*-

;;; Commentary:

;; ERT tests for `egret-dwim' when a region is active: it should run
;; every top-level test function the region overlaps, ignoring
;; subtest/suite context.  Stubs `egret--start' to capture the
;; resolved command instead of actually starting a process.

;;; Code:

(require 'ert)
(require 'cl-lib)
(require 'egret)

(defconst egret-region-test--dir
  (file-name-directory (or load-file-name buffer-file-name default-directory)))

(defconst egret-region-test--fixtures-dir
  (expand-file-name "fixtures/" egret-region-test--dir))

(defmacro egret-region-test--with-fixture (&rest body)
  (declare (indent 0))
  `(with-temp-buffer
     (insert-file-contents (expand-file-name "detect_test.go" egret-region-test--fixtures-dir))
     (go-ts-mode)
     ,@body))

(defmacro egret-region-test--capturing-start (captured-var &rest body)
  (declare (indent 1))
  `(let (,captured-var)
     (cl-letf (((symbol-function 'egret--start)
                (lambda (command) (setq ,captured-var command))))
       ,@body)
     ,captured-var))

(defun egret-region-test--goto-substring (needle)
  (goto-char (point-min))
  (search-forward needle)
  (backward-char (/ (length needle) 2)))

(ert-deftest egret-test-dwim-region-unions-covered-functions ()
  (egret-region-test--with-fixture
   (let* ((current-prefix-arg nil) (egret-history nil)
          (egret-test-args nil) (egret-verbose nil))
     ;; Select from the start of TestPlain through inside TestTable,
     ;; which should cover both but not FooSuite/TestFooSuite.
     (goto-char (point-min))
     (search-forward "func TestPlain")
     (push-mark (match-beginning 0))
     (egret-region-test--goto-substring "case one")
     (activate-mark)
     (let ((captured (egret-region-test--capturing-start c (egret-dwim))))
       (should (string= "go test -run '^TestPlain$|^TestTable$' ." captured))))))

(ert-deftest egret-test-dwim-region-single-function ()
  (egret-region-test--with-fixture
   (let* ((current-prefix-arg nil) (egret-history nil)
          (egret-test-args nil) (egret-verbose nil))
     (goto-char (point-min))
     (search-forward "func TestPlain")
     (push-mark (match-beginning 0))
     (goto-char (match-end 0))
     (activate-mark)
     (let ((captured (egret-region-test--capturing-start c (egret-dwim))))
       (should (string= "go test -run '^TestPlain$' ." captured))))))

(ert-deftest egret-test-dwim-region-errors-when-no-tests-covered ()
  (egret-region-test--with-fixture
   ;; Select the leading comment/package/import block, before any
   ;; function declaration.
   (goto-char (point-min))
   (push-mark (point-min))
   (search-forward "func TestPlain")
   (goto-char (match-beginning 0))
   (activate-mark)
   (should-error (egret-dwim) :type 'user-error)))

(ert-deftest egret-test-dwim-without-region-still-uses-point ()
  (egret-region-test--with-fixture
   (deactivate-mark)
   (egret-region-test--goto-substring "case one")
   (let* ((current-prefix-arg nil) (egret-history nil)
          (egret-test-args nil) (egret-verbose nil)
          (captured (egret-region-test--capturing-start c (egret-dwim))))
     (should (string= "go test -run '^TestTable$/^case_one$' ." captured)))))

(provide 'egret-region-test)

;;; egret-region-test.el ends here
