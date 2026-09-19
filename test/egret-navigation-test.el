;;; egret-navigation-test.el --- Tests for egret.el's navigation/imenu  -*- lexical-binding: t; -*-

;;; Commentary:

;; ERT tests for `egret-next-subtest', `egret-prev-subtest',
;; `egret-show-info', and the imenu index builder.  Reuses the
;; detect_test.go fixture (TestPlain / TestTable with two subtests /
;; FooSuite+TestBar / TestFooSuite).

;;; Code:

(require 'ert)
(require 'cl-lib)
(require 'egret)

(defconst egret-nav-test--dir
  (file-name-directory (or load-file-name buffer-file-name default-directory)))

(defconst egret-nav-test--fixtures-dir
  (expand-file-name "fixtures/" egret-nav-test--dir))

(defmacro egret-nav-test--with-fixture (&rest body)
  (declare (indent 0))
  `(with-temp-buffer
     (insert-file-contents (expand-file-name "detect_test.go" egret-nav-test--fixtures-dir))
     (go-ts-mode)
     ,@body))

(defun egret-nav-test--goto-substring (needle)
  (goto-char (point-min))
  (search-forward needle)
  (backward-char (/ (length needle) 2)))

(ert-deftest egret-test-next-subtest-from-function-header ()
  (egret-nav-test--with-fixture
   (egret-nav-test--goto-substring "func TestTable")
   (egret-next-subtest)
   (should (looking-at-p "{name: \"case one\""))))

(ert-deftest egret-test-next-subtest-advances-and-wraps ()
  (egret-nav-test--with-fixture
   (egret-nav-test--goto-substring "case one")
   (egret-next-subtest)
   (should (looking-at-p "{name: \"case two\""))
   (egret-next-subtest)
   (should (looking-at-p "{name: \"case one\"")) ; wraps around
   ))

(ert-deftest egret-test-prev-subtest-retreats-and-wraps ()
  (egret-nav-test--with-fixture
   (egret-nav-test--goto-substring "case two")
   (egret-prev-subtest)
   (should (looking-at-p "{name: \"case one\""))
   (egret-prev-subtest)
   (should (looking-at-p "{name: \"case two\""))))

(ert-deftest egret-test-next-subtest-errors-outside-test-function ()
  (egret-nav-test--with-fixture
   (goto-char (point-min))
   (should-error (egret-next-subtest) :type 'user-error)))

(ert-deftest egret-test-next-subtest-errors-when-none ()
  (egret-nav-test--with-fixture
   (egret-nav-test--goto-substring "t.Log(\"plain\")")
   (should-error (egret-next-subtest) :type 'user-error)))

(ert-deftest egret-test-show-info-plain-function ()
  (egret-nav-test--with-fixture
   (egret-nav-test--goto-substring "t.Log(\"plain\")")
   (let (msg)
     (cl-letf (((symbol-function 'message) (lambda (fmt &rest args) (setq msg (apply #'format fmt args)))))
       (egret-show-info))
     (should (string= "Egret: test TestPlain" msg)))))

(ert-deftest egret-test-show-info-subtest ()
  (egret-nav-test--with-fixture
   (egret-nav-test--goto-substring "case one")
   (let (msg)
     (cl-letf (((symbol-function 'message) (lambda (fmt &rest args) (setq msg (apply #'format fmt args)))))
       (egret-show-info))
     (should (string= "Egret: test TestTable, subtest \"case one\"" msg)))))

(ert-deftest egret-test-show-info-suite-method ()
  (egret-nav-test--with-fixture
   (egret-nav-test--goto-substring "_ = s")
   (let (msg)
     (cl-letf (((symbol-function 'message) (lambda (fmt &rest args) (setq msg (apply #'format fmt args)))))
       (egret-show-info))
     (should (string= "Egret: suite method TestBar (run via TestFoo)" msg)))))

(ert-deftest egret-test-show-info-outside-test-function ()
  (egret-nav-test--with-fixture
   (goto-char (point-min))
   (let (msg)
     (cl-letf (((symbol-function 'message) (lambda (fmt &rest args) (setq msg (apply #'format fmt args)))))
       (egret-show-info))
     (should (string= "Egret: not inside a test function" msg)))))

(ert-deftest egret-test-imenu-create-index ()
  (egret-nav-test--with-fixture
   (let ((index (egret--imenu-create-index)))
     (should (equal '("TestPlain" "TestTable" "TestTable::case_one" "TestTable::case_two" "TestFooSuite")
                      (mapcar #'car index))))))

(ert-deftest egret-test-imenu-index-sets-local-vars ()
  (egret-nav-test--with-fixture
   (egret-imenu-index)
   (should (eq imenu-create-index-function #'egret--imenu-create-index))
   (should imenu-auto-rescan)))

(provide 'egret-navigation-test)

;;; egret-navigation-test.el ends here
