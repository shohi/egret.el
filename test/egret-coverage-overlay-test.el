;;; egret-coverage-overlay-test.el --- Tests for inline coverage overlays  -*- lexical-binding: t; -*-

;;; Commentary:

;; ERT tests for `egret-coverage-overlay-show'/-clear/-mode and the
;; profile-parsing helpers.  Stubs `shell-command-to-string' (used to
;; resolve the file's Go import path) so no real `go list' call is
;; needed for the pure-parsing tests; a real end-to-end check with a
;; real Go module is done manually (see the migration plan).

;;; Code:

(require 'ert)
(require 'cl-lib)
(require 'egret)

(ert-deftest egret-test-parse-coverage-profile-matches-key ()
  (let* ((dir (file-name-as-directory (make-temp-file "egret-covparse" t)))
         (profile (expand-file-name "cover.out" dir)))
    (unwind-protect
        (progn
          (with-temp-file profile
            (insert "mode: set\n"
                    "example.com/mod/pkg/other.go:1.1,2.1 1 1\n"
                    "example.com/mod/pkg/file.go:3.9,5.2 2 1\n"
                    "example.com/mod/pkg/file.go:6.1,6.10 1 0\n"))
          (should (equal '((3 9 5 2 1) (6 1 6 10 0))
                          (egret--parse-coverage-profile
                           profile "example.com/mod/pkg/file.go"))))
      (delete-directory dir t))))

(ert-deftest egret-test-coverage-profile-file-key ()
  (let ((buffer-file-name "/tmp/mod/pkg/file.go"))
    (cl-letf (((symbol-function 'shell-command-to-string)
               (lambda (_cmd) "example.com/mod/pkg\n")))
      (should (string= "example.com/mod/pkg/file.go"
                        (egret--coverage-profile-file-key))))))

(ert-deftest egret-test-coverage-profile-file-key-nil-on-go-list-error ()
  (let ((buffer-file-name "/tmp/mod/pkg/file.go"))
    (cl-letf (((symbol-function 'shell-command-to-string)
               (lambda (_cmd) "go: cannot find module\n")))
      (should-not (egret--coverage-profile-file-key)))))

(ert-deftest egret-test-coverage-overlay-show-applies-faces ()
  (let* ((dir (file-name-as-directory (make-temp-file "egret-covshow" t)))
         (profile (expand-file-name "cover.out" dir))
         (file (expand-file-name "file.go" dir)))
    (unwind-protect
        (progn
          (with-temp-file file
            (insert "package pkg\n"        ; line 1
                    "func A() int {\n"     ; line 2
                    "\treturn 1\n"         ; line 3 (covered)
                    "}\n"                  ; line 4
                    "func B() int {\n"     ; line 5
                    "\treturn 2\n"         ; line 6 (uncovered)
                    "}\n"))                ; line 7
          (with-temp-file profile
            (insert "mode: set\n"
                    ;; whole line 3 ("\treturn 1", 9 chars: col 1..10)
                    "example.com/mod/pkg/file.go:3.1,3.10 1 1\n"
                    ;; whole line 6 ("\treturn 2", 9 chars: col 1..10)
                    "example.com/mod/pkg/file.go:6.1,6.10 1 0\n"))
          (with-current-buffer (find-file-noselect file)
            (cl-letf (((symbol-function 'egret--coverage-profile-file-key)
                       (lambda () "example.com/mod/pkg/file.go")))
              (egret-coverage-overlay-show profile))
            (should (= 2 (length egret--coverage-overlays)))
            (let* ((line3-pos (save-excursion (goto-char (point-min)) (forward-line 2) (point)))
                   (line6-pos (save-excursion (goto-char (point-min)) (forward-line 5) (point)))
                   (ovs-at-3 (overlays-at line3-pos))
                   (ovs-at-6 (overlays-at line6-pos)))
              (should (memq 'egret-coverage-covered-face
                            (mapcar (lambda (ov) (overlay-get ov 'face)) ovs-at-3)))
              (should (memq 'egret-coverage-uncovered-face
                            (mapcar (lambda (ov) (overlay-get ov 'face)) ovs-at-6))))
            (egret-coverage-overlay-clear)
            (should (null egret--coverage-overlays))
            (kill-buffer)))
      (delete-directory dir t))))

(ert-deftest egret-test-coverage-overlay-show-errors-without-profile ()
  (with-temp-buffer
    (let ((egret--last-coverage-file "/nonexistent/cover.out")
          (egret-coverage-file "cover.out"))
      (should-error (egret-coverage-overlay-show) :type 'user-error))))

(ert-deftest egret-test-coverage-overlay-mode-toggles ()
  (let* ((dir (file-name-as-directory (make-temp-file "egret-covmode" t)))
         (profile (expand-file-name "cover.out" dir))
         (file (expand-file-name "file.go" dir)))
    (unwind-protect
        (progn
          (with-temp-file file (insert "package pkg\nfunc A() { }\n"))
          (with-temp-file profile
            (insert "mode: set\nexample.com/mod/pkg/file.go:2.11,2.13 1 1\n"))
          (with-current-buffer (find-file-noselect file)
            (cl-letf (((symbol-function 'egret--coverage-profile-file-key)
                       (lambda () "example.com/mod/pkg/file.go"))
                      ((symbol-function 'egret--last-coverage-file) profile))
              (let ((egret--last-coverage-file profile))
                (egret-coverage-overlay-mode 1)
                (should egret--coverage-overlays)
                (egret-coverage-overlay-mode -1)
                (should (null egret--coverage-overlays))))
            (kill-buffer)))
      (delete-directory dir t))))

(provide 'egret-coverage-overlay-test)

;;; egret-coverage-overlay-test.el ends here
