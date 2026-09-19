;;; egret.el --- Run Go tests and subtests with tree-sitter  -*- lexical-binding: t; -*-

;; Copyright (C) 2026  Shohi

;; Author: Shohi
;; URL: https://github.com/shohi/egret.el
;; Version: 0.2.0
;; Package-Requires: ((emacs "31.1"))
;; Keywords: languages, go, tests, tools

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; egret.el runs Go tests and table-driven subtests using tree-sitter for
;; accurate syntax parsing.  See dev/egret/.agent/plans/migration-plan.org
;; (or the equivalent local plan, if present) for the full design and
;; phased build order.
;;
;; Given point in a `go-ts-mode' buffer, egret works out which `go test
;; -run' pattern corresponds to the enclosing test function, table-driven
;; subtest, or testify-style suite method (the detection layer), then
;; runs `go test' for that pattern in a dedicated compilation buffer
;; (the execution layer):
;;
;;   M-x egret-dwim            ; run the test/subtest/suite-method at point
;;   M-x egret-run-function    ; run the whole enclosing test, ignore subtest
;;
;; `egret-mode' is a minor mode; enabling it is not yet required to use
;; the commands above (keymap/hook wiring lands in a later phase).

;;; Code:

(require 'compile)
(require 'treesit)
(require 'subr-x)
(require 'pcase)

(defgroup egret nil
  "Run Go tests and subtests with tree-sitter."
  :group 'tools
  :prefix "egret-")

(defcustom egret-subtest-field-name "name"
  "Struct field name used to identify a subtest case in table-driven tests.
Typically \"name\", but could be \"description\", \"testName\", etc.,
depending on project convention."
  :type 'string
  :group 'egret)

;;; Detection (private)

(defun egret--enclosing-defun-node (&optional pos)
  "Return the nearest defun-like tree-sitter node enclosing POS.
POS defaults to point.  Only `function_declaration' and
`method_declaration' nodes are considered; returns nil if neither
encloses POS."
  (let ((node (treesit-node-at (or pos (point)))))
    (while (and node
                (not (member (treesit-node-type node)
                              '("function_declaration" "method_declaration"))))
      (setq node (treesit-node-parent node)))
    node))

(defun egret--defun-node-name (node)
  "Return the Go identifier name of NODE, or nil.
NODE must be a `function_declaration' or `method_declaration' node."
  (when node
    (let ((name-node (treesit-node-child-by-field-name node "name")))
      (when name-node (treesit-node-text name-node t)))))

(defun egret--receiver-type-name (node)
  "Return the receiver type identifier of `method_declaration' NODE.
Strips pointer indirection, e.g. a receiver of `*FooSuite' returns
\"FooSuite\".  Returns nil if NODE is not a method declaration."
  (when (and node (string= (treesit-node-type node) "method_declaration"))
    (let ((receiver-node (treesit-node-child-by-field-name node "receiver")))
      (when receiver-node
        (let ((type-node (treesit-search-subtree receiver-node "type_identifier")))
          (when type-node (treesit-node-text type-node t)))))))

(defun egret--prefixed-function-name-at-point (prefix &optional pos)
  "Return the enclosing plain Go function name at POS starting with PREFIX.
Return nil if none does.  Only matches `function_declaration' nodes
\(not `method_declaration'; see `egret--suite-method-info-at-point'
for those)."
  (let ((node (egret--enclosing-defun-node pos)))
    (when (and node (string= (treesit-node-type node) "function_declaration"))
      (let ((name (egret--defun-node-name node)))
        (when (and name (string-prefix-p prefix name))
          name)))))

(defun egret--test-function-name-at-point (&optional pos)
  "Return the enclosing Go test function name at POS, or nil.
Only matches plain `function_declaration' nodes whose name starts
with \"Test\".  Suite methods are handled separately by
`egret--suite-method-info-at-point'."
  (egret--prefixed-function-name-at-point "Test" pos))

(defun egret--benchmark-name-at-point (&optional pos)
  "Return the enclosing Go benchmark function name at POS, or nil."
  (egret--prefixed-function-name-at-point "Benchmark" pos))

(defun egret--fuzz-name-at-point (&optional pos)
  "Return the enclosing Go fuzz target function name at POS, or nil."
  (egret--prefixed-function-name-at-point "Fuzz" pos))

(defun egret--suite-method-info-at-point (&optional pos)
  "Return (ENTRY . METHOD) for a testify-style suite method at POS.
Matches methods declared on a receiver type whose name ends in
\"Suite\", e.g. `func (s *FooSuite) TestBar() {...}'.  ENTRY is the
top-level test function expected to run the suite (\"Test\" + the
receiver type name with its \"Suite\" suffix stripped); METHOD is the
method name at POS.  Returns nil if POS is not inside such a method."
  (let ((node (egret--enclosing-defun-node pos)))
    (when (and node (string= (treesit-node-type node) "method_declaration"))
      (let ((receiver (egret--receiver-type-name node))
            (method (egret--defun-node-name node)))
        (when (and receiver (string-suffix-p "Suite" receiver))
          (let* ((base (string-remove-suffix "Suite" receiver))
                 (entry (if (string-prefix-p "Test" base)
                            base
                          (concat "Test" base))))
            (cons entry method)))))))

(defun egret--keyed-element-key-text (node)
  "Return the text of the `key' field of `keyed_element' NODE, or nil."
  (let ((key (treesit-node-child-by-field-name node "key")))
    (when key (treesit-node-text key t))))

(defun egret--go-string-literal-value (node)
  "If NODE is a Go quoted string literal, return its unquoted text.
NODE is typically the `literal_element' wrapping a `keyed_element'
value field; its raw text includes the surrounding quotes (either
double-quoted or backtick-quoted), which this strips.  Returns nil if
NODE's text is not a quoted string.  Escape sequences inside
double-quoted strings are not unescaped."
  (let ((text (treesit-node-text node t)))
    (when (and (> (length text) 1)
               (member (substring text 0 1) '("\"" "`"))
               (string= (substring text 0 1) (substring text -1)))
      (substring text 1 -1))))

(defun egret--subtest-name-at-point (&optional pos)
  "Return the table-driven subtest name enclosing POS, or nil.
Walks up from POS through enclosing `literal_value' nodes (innermost
first), looking for a `keyed_element' child whose key matches
`egret-subtest-field-name' and whose value is a string literal."
  (let ((node (treesit-node-at (or pos (point)))))
    (catch 'egret--found
      (while node
        (when (string= (treesit-node-type node) "literal_value")
          (dolist (child (treesit-node-children node))
            (when (and (string= (treesit-node-type child) "keyed_element")
                       (equal (egret--keyed-element-key-text child)
                              egret-subtest-field-name))
              (let* ((value-node (treesit-node-child-by-field-name child "value"))
                     (value (and value-node
                                 (egret--go-string-literal-value value-node))))
                (when value (throw 'egret--found value))))))
        (setq node (treesit-node-parent node)))
      nil)))

(defun egret--subtest-run-name (name)
  "Turn raw subtest NAME into the identifier `go test' uses for it.
`go test' replaces whitespace in subtest names with underscores when
generating the runnable subtest name."
  (replace-regexp-in-string "[[:space:]]+" "_" name))

(defun egret--normalize-subtest-name (name)
  "Turn raw subtest NAME into the form `go test -run' expects.
\(see `egret--subtest-run-name'); the result is regexp-quoted so it
can be embedded in a `-run' pattern."
  (regexp-quote (egret--subtest-run-name name)))

(defun egret--all-subtests-in-defun (defun-node)
  "Return (POS . NAME) for every table-driven subtest inside DEFUN-NODE.
Sorted by POS.  NAME is the raw, un-normalized subtest name text.
Visits every `literal_value' node in DEFUN-NODE's subtree (see
`treesit-search-subtree'); the predicate always returns nil so the
traversal never stops early, and matches are collected as a
side effect instead of via the search's own return value."
  (let (result)
    (treesit-search-subtree
     defun-node
     (lambda (node)
       (when (string= (treesit-node-type node) "literal_value")
         (catch 'egret--matched
           (dolist (child (treesit-node-children node))
             (when (string= (treesit-node-type child) "keyed_element")
               (when (equal (egret--keyed-element-key-text child)
                            egret-subtest-field-name)
                 (let* ((value-node (treesit-node-child-by-field-name child "value"))
                        (value (and value-node
                                    (egret--go-string-literal-value value-node))))
                   (when value
                     (push (cons (treesit-node-start node) value) result)
                     (throw 'egret--matched t))))))))
       nil)
     t)
    (sort result (lambda (a b) (< (car a) (car b))))))

(defun egret--current-subtest-index (subtests pos)
  "Return the 0-based index of the last SUBTESTS entry at or before POS.
SUBTESTS is a list as returned by `egret--all-subtests-in-defun'.
Returns nil if POS is before every entry."
  (let (found (i 0))
    (dolist (entry subtests found)
      (when (>= pos (car entry))
        (setq found i))
      (setq i (1+ i)))))

(defun egret--run-pattern-at-point (&optional pos)
  "Return a `go test -run' pattern string for the context at POS.
Prefers, in order: a table-driven subtest, a testify suite method,
then a plain test function.  Signals a `user-error' if none apply."
  (let* ((pos (or pos (point)))
         (subtest (egret--subtest-name-at-point pos)))
    (cond
     (subtest
      (let ((func (egret--test-function-name-at-point pos)))
        (unless func
          (user-error "Egret: not inside a test function"))
        (format "^%s$/^%s$" func (egret--normalize-subtest-name subtest))))
     ((egret--suite-method-info-at-point pos)
      (pcase-let ((`(,entry . ,method) (egret--suite-method-info-at-point pos)))
        (format "^%s$/^%s$" entry method)))
     ((egret--test-function-name-at-point pos)
      (format "^%s$" (egret--test-function-name-at-point pos)))
     (t (user-error "Egret: not inside a test function")))))

(defun egret--enclosing-run-target-at-point (&optional pos)
  "Return a `go test -run' pattern for the whole enclosing test at POS.
Like `egret--run-pattern-at-point', but always targets the entire
enclosing test function or suite entry, ignoring any subtest context."
  (let* ((pos (or pos (point)))
         (suite (egret--suite-method-info-at-point pos)))
    (cond
     (suite (format "^%s$" (car suite)))
     ((egret--test-function-name-at-point pos)
      (format "^%s$" (egret--test-function-name-at-point pos)))
     (t (user-error "Egret: not inside a test function")))))

;;; Execution (private)

(defcustom egret-test-args nil
  "Extra arguments to pass to every `go test' invocation."
  :type '(choice (const :tag "None" nil) string)
  :group 'egret)

(defcustom egret-verbose nil
  "Non-nil to always pass -v to `go test'."
  :type 'boolean
  :group 'egret)

(defvar egret-history nil
  "History list for `go test' command arguments.")

(defvar egret-last-command nil
  "Last `go test' shell command egret ran.")

(defconst egret--buffer-name "*Egret Test*"
  "Name of egret's dedicated test-output buffer.")

(defface egret-ok-face
  '((t (:foreground "#00ff00")))
  "Face for passing test output lines."
  :group 'egret)

(defface egret-error-face
  '((t (:foreground "#ff0000")))
  "Face for failing test output lines."
  :group 'egret)

(defface egret-warning-face
  '((t (:foreground "#eeee00")))
  "Face for warning test output lines."
  :group 'egret)

(defface egret-pointer-face
  '((t (:foreground "#ff00ff")))
  "Face for the `^~~~' pointer lines under a failing assertion."
  :group 'egret)

(defface egret-standard-face
  '((t (:foreground "#ffa500")))
  "Face for informational test output lines."
  :group 'egret)

(defconst egret-font-lock-keywords
  '(("error\\:" . 'egret-error-face)
    ("testing: warning:.*" . 'egret-warning-face)
    ("^\s*\\^\\~*\s*$" . 'egret-pointer-face)
    ("^\s*Compilation.*" . 'egret-standard-face)
    ("^\s*go test.*" . 'egret-standard-face)
    (".*undefined.*" . 'egret-warning-face)
    ("^\s*FAIL.*" . 'egret-error-face)
    ("^\s*--- FAIL:.*" . 'egret-error-face)
    ("^\s*=== RUN.*" . 'egret-ok-face)
    ("^\s*--- PASS.*" . 'egret-ok-face)
    ("^\s*PASS.*" . 'egret-ok-face)
    ("^\s*ok.*" . 'egret-ok-face))
  "Minimal highlighting expressions for `egret-compilation-mode'.")

(defvar egret-compilation-error-regexp-alist-alist
  '((egret-testing . ("^\t\\([[:alnum:]-_/.]+\\.go\\):\\([0-9]+\\): .*$" 1 2))
    (egret-testify . ("^\tLocation:\t\\([[:alnum:]-_/.]+\\.go\\):\\([0-9]+\\)$" 1 2))
    (egret-gopanic . ("^\t\\([[:alnum:]-_/.]+\\.go\\):\\([0-9]+\\) \\+0x\\(?:[0-9a-f]+\\)" 1 2))
    (egret-compile . ("^\\([[:alnum:]-_/.]+\\.go\\):\\([0-9]+\\):\\(?:\\([0-9]+\\):\\)? .*$" 1 2 3))
    (egret-linkage . ("^\\([[:alnum:]-_/.]+\\.go\\):\\([0-9]+\\): undefined: .*$" 1 2)))
  "Alist of values for `egret-compilation-error-regexp-alist'.
See also `compilation-error-regexp-alist-alist'.")

(defcustom egret-compilation-error-regexp-alist
  '(egret-testing egret-testify egret-gopanic egret-compile egret-linkage)
  "Specifies how `next-error' matches errors in `go test' output.
Covers stdlib `testing' failures, `testify' assertion locations,
panics, compiler errors, and linker errors.  See also
`compilation-error-regexp-alist'."
  :type '(repeat (choice (symbol :tag "Predefined symbol")
                          (sexp :tag "Error specification")))
  :group 'egret)

(defvar egret-compilation-mode-map
  (let ((map (make-sparse-keymap)))
    (set-keymap-parent map compilation-mode-map)
    map)
  "Keymap for `egret-compilation-mode'.")

(define-derived-mode egret-compilation-mode compilation-mode "Egret-Test"
  "Major mode for egret's `go test' output buffer."
  (setq-local truncate-lines t)
  (setq-local compilation-error-regexp-alist-alist
              egret-compilation-error-regexp-alist-alist)
  (setq-local compilation-error-regexp-alist
              egret-compilation-error-regexp-alist)
  (font-lock-add-keywords nil egret-font-lock-keywords))

(defun egret--get-arguments (defaults history)
  "Resolve `go test' arguments, honoring `current-prefix-arg'.
DEFAULTS is used with no prefix argument.  A numeric prefix injects a
`-count=N' flag.  `C-u -' or a double prefix reuses the most recent
HISTORY entry.  A single or triple prefix prompts for arguments (using
HISTORY for completion)."
  (pcase current-prefix-arg
    ('nil defaults)
    ((pred integerp) (format "-count=%d %s" current-prefix-arg defaults))
    ((or '- '(16)) (car (symbol-value history)))
    ((or '(4) '(64)) (read-shell-command "go test args: " defaults history))))

(defun egret--build-command-from-args (args)
  "Return the full `go test' shell command for raw ARGS.
ARGS is a complete `go test' argument string, such as a -run pattern
target (\".\"), a bare \".\", or package paths joined by spaces.
Honors `egret-test-args', `egret-verbose', and `current-prefix-arg'
\(see `egret--get-arguments')."
  (when egret-test-args
    (setq args (concat egret-test-args " " args)))
  (when egret-verbose
    (setq args (concat "-v " args)))
  (concat "go test " (egret--get-arguments args 'egret-history)))

(defun egret--build-command (pattern)
  "Return the full `go test' shell command for -run PATTERN.
\(see `egret--build-command-from-args')."
  (egret--build-command-from-args (format "-run '%s' ." pattern)))

(defun egret--cleanup (buffer-name)
  "Delete any live process in BUFFER-NAME and erase it."
  (when (get-buffer buffer-name)
    (when (get-buffer-process buffer-name)
      (delete-process buffer-name))
    (with-current-buffer buffer-name
      (let ((inhibit-read-only t))
        (erase-buffer)))))

(defun egret--finished-sentinel (process event)
  "Run `compilation-sentinel', then report completion.
PROCESS and EVENT are as passed to any process sentinel."
  (compilation-sentinel process event)
  (when (equal event "finished\n")
    (message "Egret: test run finished.")))

(defun egret--start (command)
  "Start COMMAND, a complete shell command, in `egret--buffer-name'."
  (setq egret-last-command command)
  (egret--cleanup egret--buffer-name)
  (compilation-start command 'egret-compilation-mode
                      (lambda (_mode-name) egret--buffer-name))
  (set-process-sentinel (get-buffer-process egret--buffer-name)
                         #'egret--finished-sentinel))

(defun egret--run-args (args)
  "Run `go test' with raw ARGS (see `egret--build-command-from-args')."
  (egret--start (egret--build-command-from-args args)))

(defun egret--run (pattern)
  "Run `go test' for -run PATTERN in `egret--buffer-name'."
  (egret--run-args (format "-run '%s' ." pattern)))

(defun egret--file-function-names (prefix)
  "Return top-level function names in the current buffer starting with PREFIX.
Names are returned in source order.  Only plain `function_declaration'
nodes are considered, so testify suite methods (which are
`method_declaration' nodes, and are not runnable via `-run' on their
own) are excluded; a suite's top-level TestXxx entry function is
included like any other test function."
  (delq nil
        (mapcar (lambda (node)
                  (when (string= (treesit-node-type node) "function_declaration")
                    (let ((name (egret--defun-node-name node)))
                      (when (and name (string-prefix-p prefix name))
                        name))))
                (treesit-node-children (treesit-buffer-root-node) t))))

(defun egret--file-test-names ()
  "Return top-level Test* function names in the current buffer.
\(see `egret--file-function-names')."
  (egret--file-function-names "Test"))

(defun egret--project-packages ()
  "Return the list of package import paths in the current Go module.
Uses `go list ./...' relative to `default-directory', excluding
vendored packages."
  (seq-remove (lambda (s) (string-match-p "/vendor/" s))
              (split-string (shell-command-to-string "go list ./...") "\n" t)))

(defcustom egret-bench-args nil
  "Extra arguments to pass to every `go test -bench' invocation."
  :type '(choice (const :tag "None" nil) string)
  :group 'egret)

(defcustom egret-fuzz-args nil
  "Extra arguments to pass to every `go test -fuzz' invocation."
  :type '(choice (const :tag "None" nil) string)
  :group 'egret)

(defun egret--flagged-run-args (flag pattern extra-args)
  "Return raw `go test' args to run FLAG on PATTERN.
FLAG is \"-bench\" or \"-fuzz\".  Ordinary tests are disabled via
\"-run=-\"; EXTRA-ARGS, if non-nil, is prepended to the arguments.
PATTERN is single-quoted, matching `egret--build-command' -- a bare
\"$\" anchor is otherwise misinterpreted by shells such as fish."
  (let ((opts "-run=-"))
    (when extra-args
      (setq opts (concat extra-args " " opts)))
    (concat opts " " flag " '" pattern "'")))

(defcustom egret-coverage-file "cover.out"
  "File name for the `go test' coverage profile.
Relative to `default-directory' unless given as an absolute path."
  :type 'string
  :group 'egret)

(defvar egret--last-coverage-file nil
  "Absolute path of the most recent coverage profile `egret-coverage' wrote.")

;;; Commands

;;;###autoload
(defun egret-dwim ()
  "Run the test, table-driven subtest, or suite method at point.
Detects, in order of precedence, a table-driven subtest, a testify
suite method, or a plain test function, then runs `go test' for it."
  (interactive)
  (egret--run (egret--run-pattern-at-point)))

;;;###autoload
(defun egret-run-function ()
  "Run the whole enclosing test function or suite entry at point.
Like `egret-dwim', but always targets the whole enclosing test,
ignoring any subtest context."
  (interactive)
  (egret--run (egret--enclosing-run-target-at-point)))

;;;###autoload
(defun egret-run-file ()
  "Run every top-level test function declared in the current file."
  (interactive)
  (let ((names (egret--file-test-names)))
    (unless names
      (user-error "Egret: no test functions found in this file"))
    (egret--run (mapconcat (lambda (name) (format "^%s$" name)) names "|"))))

;;;###autoload
(defun egret-run-package ()
  "Run all tests in the current package (this file's directory)."
  (interactive)
  (egret--run-args "."))

;;;###autoload
(defun egret-run-project ()
  "Run tests for every package in the current Go module."
  (interactive)
  (let ((packages (egret--project-packages)))
    (unless packages
      (user-error "Egret: no packages found (not in a Go module?)"))
    (egret--run-args (string-join packages " "))))

;;;###autoload
(defun egret-run-last ()
  "Re-run the most recent `go test' command egret started.
Note: the egret test buffer is a `compilation-mode' buffer, so
pressing \\`g' (`revert-buffer') in it re-runs the same command too;
this command is for invoking a re-run from elsewhere."
  (interactive)
  (unless egret-last-command
    (user-error "Egret: no previous test run"))
  (egret--start egret-last-command))

;;;###autoload
(defun egret-run-benchmark ()
  "Run the benchmark function at point.
Ordinary tests are disabled for this run (\"-run=-\")."
  (interactive)
  (let ((name (egret--benchmark-name-at-point)))
    (unless name
      (user-error "Egret: not inside a benchmark function"))
    (egret--run-args (egret--flagged-run-args
                       "-bench" (format "^%s$" name) egret-bench-args))))

;;;###autoload
(defun egret-run-file-benchmarks ()
  "Run every benchmark function declared in the current file.
Ordinary tests are disabled for this run (\"-run=-\")."
  (interactive)
  (let ((names (egret--file-function-names "Benchmark")))
    (unless names
      (user-error "Egret: no benchmark functions found in this file"))
    (egret--run-args
     (egret--flagged-run-args
      "-bench" (mapconcat (lambda (name) (format "^%s$" name)) names "|")
      egret-bench-args))))

;;;###autoload
(defun egret-run-project-benchmarks ()
  "Run every benchmark across every package in the current Go module.
Ordinary tests are disabled for this run (\"-run=-\")."
  (interactive)
  (let ((packages (egret--project-packages)))
    (unless packages
      (user-error "Egret: no packages found (not in a Go module?)"))
    (egret--run-args
     (format "%s %s"
             (egret--flagged-run-args "-bench" "." egret-bench-args)
             (string-join packages " ")))))

;;;###autoload
(defun egret-run-fuzz ()
  "Run the fuzz target at point.
Ordinary tests are disabled for this run (\"-run=-\").  `go test'
only supports fuzzing a single target per invocation, so unlike
benchmarks there is no file- or project-wide fuzz command."
  (interactive)
  (let ((name (egret--fuzz-name-at-point)))
    (unless name
      (user-error "Egret: not inside a fuzz function"))
    (egret--run-args (egret--flagged-run-args
                       "-fuzz" (format "^%s$" name) egret-fuzz-args))))

;;;###autoload
(defun egret-coverage ()
  "Run `go test' with coverage for the current package.
Writes the profile to `egret-coverage-file' (via `--coverprofile'),
under `default-directory'.  Use `egret-coverage-show-html' afterwards
to view it."
  (interactive)
  (let ((file (expand-file-name egret-coverage-file)))
    (setq egret--last-coverage-file file)
    (egret--run-args (format "--coverprofile=%s ." (shell-quote-argument file)))))

;;;###autoload
(defun egret-coverage-show-html ()
  "Open an HTML report for the most recent `egret-coverage' profile.
Generates it via \"go tool cover -html\" next to the profile, then
opens it with `browse-url-of-file'."
  (interactive)
  (let* ((profile (or egret--last-coverage-file (expand-file-name egret-coverage-file))))
    (unless (file-exists-p profile)
      (user-error "Egret: no coverage profile found at %s" profile))
    (let* ((html (concat (file-name-sans-extension profile) ".html"))
           (exit (call-process "go" nil nil nil "tool" "cover"
                                (format "-html=%s" profile)
                                (format "-o=%s" html))))
      (if (zerop exit)
          (browse-url-of-file html)
        (user-error "Egret: `go tool cover' failed (exit %d)" exit)))))

;;;###autoload
(defun egret-show-info ()
  "Show the test/subtest/suite context egret detects at point."
  (interactive)
  (let ((subtest (egret--subtest-name-at-point))
        (func (egret--test-function-name-at-point))
        (suite (egret--suite-method-info-at-point)))
    (cond
     (suite
      (message "Egret: suite method %s (run via %s)" (cdr suite) (car suite)))
     ((and func subtest)
      (message "Egret: test %s, subtest %S" func subtest))
     (func
      (message "Egret: test %s" func))
     (t
      (message "Egret: not inside a test function")))))

;;;###autoload
(defun egret-next-subtest ()
  "Move point to the next table-driven subtest in the enclosing test.
Wraps around to the first subtest past the last one."
  (interactive)
  (let ((node (egret--enclosing-defun-node)))
    (unless (and node (string= (treesit-node-type node) "function_declaration"))
      (user-error "Egret: not inside a test function"))
    (let ((subtests (egret--all-subtests-in-defun node)))
      (unless subtests
        (user-error "Egret: no subtests found in this test function"))
      (let* ((current (egret--current-subtest-index subtests (point)))
             (next (cond ((null current) 0)
                         ((>= current (1- (length subtests))) 0)
                         (t (1+ current))))
             (entry (nth next subtests)))
        (goto-char (car entry))
        (message "Egret: subtest %S (%d/%d)"
                 (cdr entry) (1+ next) (length subtests))))))

;;;###autoload
(defun egret-prev-subtest ()
  "Move point to the previous table-driven subtest in the enclosing test.
Wraps around to the last subtest before the first one."
  (interactive)
  (let ((node (egret--enclosing-defun-node)))
    (unless (and node (string= (treesit-node-type node) "function_declaration"))
      (user-error "Egret: not inside a test function"))
    (let ((subtests (egret--all-subtests-in-defun node)))
      (unless subtests
        (user-error "Egret: no subtests found in this test function"))
      (let* ((current (egret--current-subtest-index subtests (point)))
             (prev (cond ((null current) (1- (length subtests)))
                         ((<= current 0) (1- (length subtests)))
                         (t (1- current))))
             (entry (nth prev subtests)))
        (goto-char (car entry))
        (message "Egret: subtest %S (%d/%d)"
                 (cdr entry) (1+ prev) (length subtests))))))

(defun egret--imenu-create-index ()
  "Create an imenu index of test functions and their subtests.
Subtest entries are named \"TestName::subtest_name\"."
  (let (index)
    (dolist (node (treesit-node-children (treesit-buffer-root-node) t))
      (when (string= (treesit-node-type node) "function_declaration")
        (let ((name (egret--defun-node-name node)))
          (when (and name (string-prefix-p "Test" name))
            (push (cons name (treesit-node-start node)) index)
            (dolist (entry (egret--all-subtests-in-defun node))
              (push (cons (format "%s::%s" name (egret--subtest-run-name (cdr entry)))
                          (car entry))
                    index))))))
    (nreverse index)))

;;;###autoload
(defun egret-imenu-index ()
  "Enable imenu support for tests and subtests in the current buffer.
Sets `imenu-create-index-function' buffer-locally to
`egret--imenu-create-index'.  Call this interactively, or add it to
`go-ts-mode-hook'."
  (interactive)
  (setq-local imenu-create-index-function #'egret--imenu-create-index)
  (setq-local imenu-auto-rescan t))

;;;###autoload
(defun egret-imenu-goto ()
  "Jump to a test or subtest in the current buffer via `imenu'.
Enables egret's imenu index (`egret-imenu-index') first if it is not
already active."
  (interactive)
  (unless (eq imenu-create-index-function #'egret--imenu-create-index)
    (egret-imenu-index))
  (call-interactively #'imenu))

;;; Minor mode

(defvar-keymap egret-mode-map
  :doc "Keymap for `egret-mode'.
Deliberately shadows built-in `go-ts-mode-map's C-c C-t t/f/p with
egret's richer DWIM/scope equivalents; see the migration plan for the
rationale.  egret-run-fuzz and egret-run-file-benchmarks have no
binding by design (less common, M-x only)."
  "C-c C-t t" #'egret-dwim
  "C-c C-t T" #'egret-run-function
  "C-c C-t f" #'egret-run-file
  "C-c C-t p" #'egret-run-package
  "C-c C-t P" #'egret-run-project
  "C-c C-t l" #'egret-run-last
  "C-c C-t b" #'egret-run-benchmark
  "C-c C-t B" #'egret-run-project-benchmarks
  "C-c C-t c" #'egret-coverage
  "C-c C-t C" #'egret-coverage-show-html
  "C-c C-t n" #'egret-next-subtest
  "C-c C-t N" #'egret-prev-subtest
  "C-c C-t m" #'egret-imenu-goto
  "C-c C-t i" #'egret-show-info)

;;;###autoload
(define-minor-mode egret-mode
  "Minor mode for running Go tests with tree-sitter."
  :lighter " Egret"
  :keymap egret-mode-map
  :group 'egret)

(defun egret--maybe-enable ()
  "Enable `egret-mode' if the current buffer is a `go-ts-mode' buffer.
Uses `derived-mode-p' rather than `eq' against `major-mode', so any
future mode derived from `go-ts-mode' is covered too.  Intended for
`egret-global-mode'."
  (when (derived-mode-p 'go-ts-mode)
    (egret-mode 1)))

;;;###autoload
(define-globalized-minor-mode egret-global-mode egret-mode
  egret--maybe-enable
  :group 'egret)

(provide 'egret)

;;; egret.el ends here
