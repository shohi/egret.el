;;; egret-coverage-test.el --- Tests for egret.el's coverage commands  -*- lexical-binding: t; -*-

;;; Commentary:

;; ERT tests for `egret-coverage' and `egret-coverage-show-html'.
;; Stubs `egret--start' / `call-process' / `browse-url-of-file' so no
;; real process is spawned; a real end-to-end run is done manually
;; (see the migration plan) instead of here.

;;; Code:

(require 'ert)
(require 'cl-lib)
(require 'egret)

(defmacro egret-cov-test--capturing-start (captured-var &rest body)
  "Run BODY with `egret--start' stubbed to set CAPTURED-VAR."
  (declare (indent 1))
  `(let (,captured-var)
     (cl-letf (((symbol-function 'egret--start)
                (lambda (command &optional _on-success) (setq ,captured-var command))))
       ,@body)
     ,captured-var))

(ert-deftest egret-test-coverage-builds-coverprofile-command ()
  (let* ((default-directory (file-name-as-directory (make-temp-file "egret-cov" t)))
         (current-prefix-arg nil) (egret-history nil)
         (egret-test-args nil) (egret-verbose nil)
         (egret-coverage-file "cover.out")
         (egret--last-coverage-file nil))
    (unwind-protect
        (let ((captured (egret-cov-test--capturing-start c (egret-coverage))))
          (should (string= (format "go test --coverprofile=%s ."
                                    (shell-quote-argument
                                     (expand-file-name "cover.out" default-directory)))
                            captured))
          (should (string= (expand-file-name "cover.out" default-directory)
                            egret--last-coverage-file)))
      (delete-directory default-directory t))))

(ert-deftest egret-test-coverage-show-html-errors-without-profile ()
  (let* ((default-directory (file-name-as-directory (make-temp-file "egret-cov" t)))
         (egret-coverage-file "cover.out")
         (egret--last-coverage-file nil))
    (unwind-protect
        (should-error (egret-coverage-show-html) :type 'user-error)
      (delete-directory default-directory t))))

(ert-deftest egret-test-coverage-show-html-generates-and-opens ()
  (let* ((dir (file-name-as-directory (make-temp-file "egret-cov" t)))
         (profile (expand-file-name "cover.out" dir))
         (opened nil) (cover-args nil))
    (unwind-protect
        (progn
          (with-temp-file profile (insert "mode: set\n"))
          (cl-letf (((symbol-function 'call-process)
                     (lambda (_prog &rest args) (setq cover-args args) 0))
                    ((symbol-function 'browse-url-of-file)
                     (lambda (file) (setq opened file))))
            (let ((egret--last-coverage-file profile))
              (egret-coverage-show-html)))
          (should (string= (expand-file-name "cover.html" dir) opened))
          (should (member (format "-html=%s" profile) cover-args)))
      (delete-directory dir t))))

(ert-deftest egret-test-coverage-show-html-errors-on-tool-failure ()
  (let* ((dir (file-name-as-directory (make-temp-file "egret-cov" t)))
         (profile (expand-file-name "cover.out" dir)))
    (unwind-protect
        (progn
          (with-temp-file profile (insert "mode: set\n"))
          (cl-letf (((symbol-function 'call-process)
                     (lambda (&rest _args) 1)))
            (let ((egret--last-coverage-file profile))
              (should-error (egret-coverage-show-html)))))
      (delete-directory dir t))))

(provide 'egret-coverage-test)

;;; egret-coverage-test.el ends here
