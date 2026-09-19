;;; egret-transient-test.el --- Tests for egret's transient menu  -*- lexical-binding: t; -*-

;;; Commentary:

;; ERT tests for `egret-transient'.  Transient prefixes are mostly
;; declarative UI layout, not logic, so these just confirm it loaded
;; correctly and every suffix command it references actually exists
;; and is interactive -- the thing most likely to silently break
;; (typo'd command name, renamed command left stale in the menu).

;;; Code:

(require 'ert)
(require 'egret)

(defconst egret-transient-test--expected-commands
  '(egret-dwim egret-run-function egret-run-file egret-run-package
    egret-run-project egret-run-last egret-run-benchmark
    egret-run-file-benchmarks egret-run-project-benchmarks egret-run-fuzz
    egret-coverage egret-coverage-show-html egret-coverage-overlay-mode
    egret-next-subtest egret-prev-subtest egret-imenu-goto egret-show-info)
  "Every command `egret-transient' is expected to surface.")

(ert-deftest egret-test-transient-defined-as-prefix ()
  (should (fboundp 'egret-transient))
  (should (get 'egret-transient 'transient--prefix)))

(ert-deftest egret-test-transient-suffix-commands-exist-and-interactive ()
  (dolist (cmd egret-transient-test--expected-commands)
    (should (fboundp cmd))
    (should (commandp cmd))))

(ert-deftest egret-test-transient-bound-in-mode-map ()
  (should (eq (lookup-key egret-mode-map (kbd "C-c C-t C-t")) 'egret-transient)))

(provide 'egret-transient-test)

;;; egret-transient-test.el ends here
