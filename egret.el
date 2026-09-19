;;; egret.el --- Run Go tests and subtests with tree-sitter  -*- lexical-binding: t; -*-

;; Copyright (C) 2026  Shohi

;; Author: Shohi
;; URL: https://github.com/shohi/egret.el
;; Version: 0.1.0
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
;; This file currently implements the tree-sitter detection core: given
;; point in a `go-ts-mode' buffer, work out which `go test -run' pattern
;; corresponds to the enclosing test function, table-driven subtest, or
;; testify-style suite method.  Actual test execution lands in a later
;; phase; for now `egret-dwim' and `egret-run-function' only report the
;; pattern they would use.

;;; Code:

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

(defun egret--test-function-name-at-point (&optional pos)
  "Return the enclosing Go test function name at POS, or nil.
Only matches plain `function_declaration' nodes whose name starts
with \"Test\".  Suite methods are handled separately by
`egret--suite-method-info-at-point'."
  (let ((node (egret--enclosing-defun-node pos)))
    (when (and node (string= (treesit-node-type node) "function_declaration"))
      (let ((name (egret--defun-node-name node)))
        (when (and name (string-prefix-p "Test" name))
          name)))))

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

(defun egret--normalize-subtest-name (name)
  "Turn raw subtest NAME into the form `go test -run' expects.
`go test' replaces whitespace in subtest names with underscores when
generating the runnable subtest name; the result is then
regexp-quoted so it can be embedded in a `-run' pattern."
  (regexp-quote (replace-regexp-in-string "[[:space:]]+" "_" name)))

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

;;; Commands

;;;###autoload
(defun egret-dwim ()
  "Report the `go test -run' pattern egret would use at point.
Detects, in order of precedence, a table-driven subtest, a testify
suite method, or a plain test function.  Actual execution lands in a
later phase; for now this only reports what would run."
  (interactive)
  (let ((pattern (egret--run-pattern-at-point)))
    (message "egret: go test -run '%s'" pattern)
    pattern))

;;;###autoload
(defun egret-run-function ()
  "Report the `go test -run' pattern for the whole test at point.
Like `egret-dwim', but always targets the whole enclosing test
function or suite entry, ignoring any subtest context."
  (interactive)
  (let ((pattern (egret--enclosing-run-target-at-point)))
    (message "egret: go test -run '%s'" pattern)
    pattern))

;;; Minor mode

;;;###autoload
(define-minor-mode egret-mode
  "Minor mode for running Go tests with tree-sitter."
  :lighter " Egret"
  :group 'egret)

(provide 'egret)

;;; egret.el ends here
