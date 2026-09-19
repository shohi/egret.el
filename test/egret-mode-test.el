;;; egret-mode-test.el --- Tests for egret.el's minor mode wiring  -*- lexical-binding: t; -*-

;;; Commentary:

;; ERT tests for `egret-mode-map' bindings, `egret-imenu-goto', and
;; `egret--maybe-enable' (the predicate behind `egret-global-mode').

;;; Code:

(require 'ert)
(require 'cl-lib)
(require 'egret)

(ert-deftest egret-test-mode-map-bindings ()
  (let ((cases '(("C-c C-t t" . egret-dwim)
                  ("C-c C-t T" . egret-run-function)
                  ("C-c C-t f" . egret-run-file)
                  ("C-c C-t p" . egret-run-package)
                  ("C-c C-t P" . egret-run-project)
                  ("C-c C-t l" . egret-run-last)
                  ("C-c C-t b" . egret-run-benchmark)
                  ("C-c C-t B" . egret-run-project-benchmarks)
                  ("C-c C-t c" . egret-coverage)
                  ("C-c C-t C" . egret-coverage-show-html)
                  ("C-c C-t n" . egret-next-subtest)
                  ("C-c C-t N" . egret-prev-subtest)
                  ("C-c C-t m" . egret-imenu-goto)
                  ("C-c C-t i" . egret-show-info))))
    (dolist (case cases)
      (should (eq (lookup-key egret-mode-map (kbd (car case))) (cdr case))))))

(ert-deftest egret-test-mode-map-no-binding-for-fuzz-or-file-benchmarks ()
  ;; Deliberately unbound, M-x only.
  (should-not (lookup-key egret-mode-map (kbd "C-c C-t z")))
  (dolist (key '("C-c C-t u" "C-c C-t F"))
    (should-not (eq (lookup-key egret-mode-map (kbd key)) 'egret-run-fuzz))
    (should-not (eq (lookup-key egret-mode-map (kbd key)) 'egret-run-file-benchmarks))))

(ert-deftest egret-test-imenu-goto-sets-up-index-then-calls-imenu ()
  (with-temp-buffer
    (let (called)
      (cl-letf (((symbol-function 'imenu)
                 (lambda () (interactive) (setq called t))))
        (egret-imenu-goto))
      (should (eq imenu-create-index-function #'egret--imenu-create-index))
      (should called))))

(ert-deftest egret-test-maybe-enable-in-go-ts-mode ()
  (with-temp-buffer
    (go-ts-mode)
    (egret--maybe-enable)
    (should egret-mode)))

(ert-deftest egret-test-maybe-enable-not-in-other-modes ()
  (with-temp-buffer
    (fundamental-mode)
    (egret--maybe-enable)
    (should-not egret-mode)))

(provide 'egret-mode-test)

;;; egret-mode-test.el ends here
