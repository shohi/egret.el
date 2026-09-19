;;; egret-safe-local-test.el --- Tests for egret's safe-local-variable declarations  -*- lexical-binding: t; -*-

;;; Commentary:

;; Ensures the customizables meant to be set per-project via
;; `.dir-locals.el' are recognized as safe, so `hack-local-variables'
;; applies them without prompting.

;;; Code:

(require 'ert)
(require 'egret)

(ert-deftest egret-test-customizables-are-safe-local ()
  (dolist (case '((egret-subtest-field-name . "desc")
                   (egret-test-args . nil)
                   (egret-test-args . "-race")
                   (egret-verbose . t)
                   (egret-verbose . nil)
                   (egret-bench-args . "-benchmem")
                   (egret-fuzz-args . "-fuzztime=5s")
                   (egret-coverage-file . "c.out")))
    (should (safe-local-variable-p (car case) (cdr case)))))

(ert-deftest egret-test-unsafe-values-rejected ()
  ;; Sanity check the predicates aren't accidentally permissive.
  (should-not (safe-local-variable-p 'egret-subtest-field-name 42))
  (should-not (safe-local-variable-p 'egret-verbose "yes"))
  (should-not (safe-local-variable-p 'egret-test-args 42)))

(provide 'egret-safe-local-test)

;;; egret-safe-local-test.el ends here
