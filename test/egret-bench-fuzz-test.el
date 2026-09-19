;;; egret-bench-fuzz-test.el --- Tests for egret.el's benchmark/fuzz commands  -*- lexical-binding: t; -*-

;;; Commentary:

;; ERT tests for `egret-run-benchmark', `egret-run-file-benchmarks',
;; `egret-run-project-benchmarks', and `egret-run-fuzz'.  Stubs
;; `egret--start' to capture the resolved command instead of actually
;; starting a process.

;;; Code:

(require 'ert)
(require 'cl-lib)
(require 'egret)

(defconst egret-bf-test--dir
  (file-name-directory (or load-file-name buffer-file-name default-directory)))

(defconst egret-bf-test--fixtures-dir
  (expand-file-name "fixtures/" egret-bf-test--dir))

(defmacro egret-bf-test--capturing-start (captured-var &rest body)
  "Run BODY with `egret--start' stubbed to set CAPTURED-VAR."
  (declare (indent 1))
  `(let (,captured-var)
     (cl-letf (((symbol-function 'egret--start)
                (lambda (command &optional _on-success) (setq ,captured-var command))))
       ,@body)
     ,captured-var))

(defun egret-bf-test--goto-substring (needle)
  (goto-char (point-min))
  (search-forward needle)
  (backward-char (/ (length needle) 2)))

(defmacro egret-bf-test--with-fixture (&rest body)
  (declare (indent 0))
  `(with-temp-buffer
     (insert-file-contents (expand-file-name "bench_fuzz_test.go" egret-bf-test--fixtures-dir))
     (go-ts-mode)
     ,@body))

(ert-deftest egret-test-benchmark-name-at-point ()
  (egret-bf-test--with-fixture
   (egret-bf-test--goto-substring "b.N")
   (should (string= "BenchmarkOne" (egret--benchmark-name-at-point)))))

(ert-deftest egret-test-fuzz-name-at-point ()
  (egret-bf-test--with-fixture
   (egret-bf-test--goto-substring "_ = in")
   (should (string= "FuzzThing" (egret--fuzz-name-at-point)))))

(ert-deftest egret-test-file-benchmark-names ()
  (egret-bf-test--with-fixture
   (should (equal '("BenchmarkOne" "BenchmarkTwo")
                   (egret--file-function-names "Benchmark")))))

(ert-deftest egret-test-run-benchmark-at-point ()
  (egret-bf-test--with-fixture
   (egret-bf-test--goto-substring "bench-one")
   (let* ((current-prefix-arg nil) (egret-history nil)
          (egret-test-args nil) (egret-verbose nil) (egret-bench-args nil)
          (captured (egret-bf-test--capturing-start c (egret-run-benchmark))))
     (should (string= "go test -run=- -bench '^BenchmarkOne$'" captured)))))

(ert-deftest egret-test-run-benchmark-with-extra-args ()
  (egret-bf-test--with-fixture
   (egret-bf-test--goto-substring "bench-one")
   (let* ((current-prefix-arg nil) (egret-history nil)
          (egret-test-args nil) (egret-verbose nil) (egret-bench-args "-benchmem")
          (captured (egret-bf-test--capturing-start c (egret-run-benchmark))))
     (should (string= "go test -benchmem -run=- -bench '^BenchmarkOne$'" captured)))))

(ert-deftest egret-test-run-file-benchmarks-unions-all ()
  (egret-bf-test--with-fixture
   (let* ((current-prefix-arg nil) (egret-history nil)
          (egret-test-args nil) (egret-verbose nil) (egret-bench-args nil)
          (captured (egret-bf-test--capturing-start c (egret-run-file-benchmarks))))
     (should (string= "go test -run=- -bench '^BenchmarkOne$|^BenchmarkTwo$'" captured)))))

(ert-deftest egret-test-run-project-benchmarks-joins-packages ()
  (let* ((current-prefix-arg nil) (egret-history nil)
         (egret-test-args nil) (egret-verbose nil) (egret-bench-args nil))
    (cl-letf (((symbol-function 'shell-command-to-string)
               (lambda (_cmd) "example.com/foo\nexample.com/foo/baz\n")))
      (let ((captured (egret-bf-test--capturing-start c (egret-run-project-benchmarks))))
        (should (string= "go test -run=- -bench '.' example.com/foo example.com/foo/baz"
                          captured))))))

(ert-deftest egret-test-run-fuzz-at-point ()
  (egret-bf-test--with-fixture
   (egret-bf-test--goto-substring "fuzz-thing")
   (let* ((current-prefix-arg nil) (egret-history nil)
          (egret-test-args nil) (egret-verbose nil) (egret-fuzz-args nil)
          (captured (egret-bf-test--capturing-start c (egret-run-fuzz))))
     (should (string= "go test -run=- -fuzz '^FuzzThing$'" captured)))))

(ert-deftest egret-test-run-benchmark-errors-outside-benchmark ()
  (egret-bf-test--with-fixture
   (goto-char (point-min))
   (should-error (egret-run-benchmark) :type 'user-error)))

(ert-deftest egret-test-run-fuzz-errors-outside-fuzz ()
  (egret-bf-test--with-fixture
   (goto-char (point-min))
   (should-error (egret-run-fuzz) :type 'user-error)))

(provide 'egret-bench-fuzz-test)

;;; egret-bench-fuzz-test.el ends here
