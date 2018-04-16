;;; subr-x.el --- extra Lisp functions  -*- lexical-binding:t -*-

;; Copyright (C) 2013-2018 Free Software Foundation, Inc.

;; Maintainer: emacs-devel@gnu.org
;; Keywords: convenience
;; Package: emacs

;; This file is part of GNU Emacs.

;; GNU Emacs is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; GNU Emacs is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; Less commonly used functions that complement basic APIs, often implemented in
;; C code (like hash-tables and strings), and are not eligible for inclusion
;; in subr.el.

;; Do not document these functions in the lispref.
;; https://lists.gnu.org/r/emacs-devel/2014-01/msg01006.html

;; NB If you want to use this library, it's almost always correct to use:
;; (eval-when-compile (require 'subr-x))

;;; Code:

(eval-when-compile (require 'cl-lib))


(defmacro internal--thread-argument (first? &rest forms)
  "Internal implementation for `thread-first' and `thread-last'.
When Argument FIRST? is non-nil argument is threaded first, else
last.  FORMS are the expressions to be threaded."
  (pcase forms
    (`(,x (,f . ,args) . ,rest)
     `(internal--thread-argument
       ,first? ,(if first? `(,f ,x ,@args) `(,f ,@args ,x)) ,@rest))
    (`(,x ,f . ,rest) `(internal--thread-argument ,first? (,f ,x) ,@rest))
    (_ (car forms))))

(defmacro thread-first (&rest forms)
  "Thread FORMS elements as the first argument of their successor.
Example:
    (thread-first
      5
      (+ 20)
      (/ 25)
      -
      (+ 40))
Is equivalent to:
    (+ (- (/ (+ 5 20) 25)) 40)
Note how the single `-' got converted into a list before
threading."
  (declare (indent 1)
           (debug (form &rest [&or symbolp (sexp &rest form)])))
  `(internal--thread-argument t ,@forms))

(defmacro thread-last (&rest forms)
  "Thread FORMS elements as the last argument of their successor.
Example:
    (thread-last
      5
      (+ 20)
      (/ 25)
      -
      (+ 40))
Is equivalent to:
    (+ 40 (- (/ 25 (+ 20 5))))
Note how the single `-' got converted into a list before
threading."
  (declare (indent 1) (debug thread-first))
  `(internal--thread-argument nil ,@forms))

(defsubst internal--listify (elt)
  "Wrap ELT in a list if it is not one.
If ELT is of the form ((EXPR)), listify (EXPR) with a dummy symbol."
  (cond
   ((symbolp elt) (list elt elt))
   ((null (cdr elt))
    (list (make-symbol "s") (car elt)))
   (t elt)))

(defsubst internal--check-binding (binding)
  "Check BINDING is properly formed."
  (when (> (length binding) 2)
    (signal
     'error
     (cons "`let' bindings can have only one value-form" binding)))
  binding)

(defsubst internal--build-binding-value-form (binding prev-var)
  "Build the conditional value form for BINDING using PREV-VAR."
  (let ((var (car binding)))
    `(,var (and ,prev-var ,(cadr binding)))))

(defun internal--build-binding (binding prev-var)
  "Check and build a single BINDING with PREV-VAR."
  (thread-first
      binding
    internal--listify
    internal--check-binding
    (internal--build-binding-value-form prev-var)))

(defun internal--build-bindings (bindings)
  "Check and build conditional value forms for BINDINGS."
  (let ((prev-var t))
    (mapcar (lambda (binding)
              (let ((binding (internal--build-binding binding prev-var)))
                (setq prev-var (car binding))
                binding))
            bindings)))

(defmacro if-let* (varlist then &rest else)
  "Bind variables according to VARLIST and eval THEN or ELSE.
This is like `if-let' but doesn't handle a VARLIST of the form
\(SYMBOL SOMETHING) specially."
  (declare (indent 2)
           (debug ((&rest [&or symbolp (symbolp form) (form)])
                   form body)))
  (if varlist
      `(let* ,(setq varlist (internal--build-bindings varlist))
         (if ,(caar (last varlist))
             ,then
           ,@else))
    `(let* () ,then)))

(defmacro when-let* (varlist &rest body)
  "Bind variables according to VARLIST and conditionally eval BODY.
This is like `when-let' but doesn't handle a VARLIST of the form
\(SYMBOL SOMETHING) specially."
  (declare (indent 1) (debug if-let*))
  (list 'if-let* varlist (macroexp-progn body)))

(defmacro and-let* (varlist &rest body)
  "Bind variables according to VARLIST and conditionally eval BODY.
Like `when-let*', except if BODY is empty and all the bindings
are non-nil, then the result is non-nil."
  (declare (indent 1)
           (debug ((&rest [&or symbolp (symbolp form) (form)])
                   body)))
  (let (res)
    (if varlist
        `(let* ,(setq varlist (internal--build-bindings varlist))
           (if ,(setq res (caar (last varlist)))
               ,@(or body `(,res))))
      `(let* () ,@(or body '(t))))))

(defmacro if-let (spec then &rest else)
  "Bind variables according to SPEC and eval THEN or ELSE.
Each binding is evaluated in turn, and evaluation stops if a
binding value is nil.  If all are non-nil, the value of THEN is
returned, or the last form in ELSE is returned.

Each element of SPEC is a list (SYMBOL VALUEFORM) which binds
SYMBOL to the value of VALUEFORM.  An element can additionally be
of the form (VALUEFORM), which is evaluated and checked for nil;
i.e. SYMBOL can be omitted if only the test result is of
interest.  It can also be of the form SYMBOL, then the binding of
SYMBOL is checked for nil.

As a special case, a SPEC of the form \(SYMBOL SOMETHING) is
interpreted like \((SYMBOL SOMETHING)). This exists for backward
compatibility with the old syntax that accepted only one
binding."
  (declare (indent 2)
           (debug ([&or (&rest [&or symbolp (symbolp form) (form)])
                        (symbolp form)]
                   form body)))
  (when (and (<= (length spec) 2)
             (not (listp (car spec))))
    ;; Adjust the single binding case
    (setq spec (list spec)))
  (list 'if-let* spec then (macroexp-progn else)))

(defmacro when-let (spec &rest body)
  "Bind variables according to SPEC and conditionally eval BODY.
Each binding is evaluated in turn, and evaluation stops if a
binding value is nil.  If all are non-nil, the value of the last
form in BODY is returned.

The variable list SPEC is the same as in `if-let'."
  (declare (indent 1) (debug if-let))
  (list 'if-let spec (macroexp-progn body)))

(defsubst hash-table-empty-p (hash-table)
  "Check whether HASH-TABLE is empty (has 0 elements)."
  (zerop (hash-table-count hash-table)))

(defsubst hash-table-keys (hash-table)
  "Return a list of keys in HASH-TABLE."
  (cl-loop for k being the hash-keys of hash-table collect k))

(defsubst hash-table-values (hash-table)
  "Return a list of values in HASH-TABLE."
  (cl-loop for v being the hash-values of hash-table collect v))

(defsubst string-empty-p (string)
  "Check whether STRING is empty."
  (string= string ""))

(defsubst string-join (strings &optional separator)
  "Join all STRINGS using SEPARATOR."
  (mapconcat 'identity strings separator))

(define-obsolete-function-alias 'string-reverse 'reverse "25.1")

(defsubst string-trim-left (string &optional regexp)
  "Trim STRING of leading string matching REGEXP.

REGEXP defaults to \"[ \\t\\n\\r]+\"."
  (if (string-match (concat "\\`\\(?:" (or  regexp "[ \t\n\r]+")"\\)") string)
      (replace-match "" t t string)
    string))

(defsubst string-trim-right (string &optional regexp)
  "Trim STRING of trailing string matching REGEXP.

REGEXP defaults to  \"[ \\t\\n\\r]+\"."
  (if (string-match (concat "\\(?:" (or regexp "[ \t\n\r]+") "\\)\\'") string)
      (replace-match "" t t string)
    string))

(defsubst string-trim (string &optional trim-left trim-right)
  "Trim STRING of leading and trailing strings matching TRIM-LEFT and TRIM-RIGHT.

TRIM-LEFT and TRIM-RIGHT default to \"[ \\t\\n\\r]+\"."
  (string-trim-left (string-trim-right string trim-right) trim-left))

(defsubst string-blank-p (string)
  "Check whether STRING is either empty or only whitespace."
  (string-match-p "\\`[ \t\n\r]*\\'" string))

(defsubst string-remove-prefix (prefix string)
  "Remove PREFIX from STRING if present."
  (if (string-prefix-p prefix string)
      (substring string (length prefix))
    string))

(defsubst string-remove-suffix (suffix string)
  "Remove SUFFIX from STRING if present."
  (if (string-suffix-p suffix string)
      (substring string 0 (- (length string) (length suffix)))
    string))

(cl-defstruct (prop-match)
  beginning end value)

(defun text-property-search-forward (property &optional value predicate
                                              not-immediate)
  "Search for the next region that has text property PROPERTY set to VALUE.
If not found, the return value is nil.  If found, point will be
placed at the end of the region and an object describing the
match is returned.

PREDICATE is called with two values.  The first is the VALUE
parameter.  The second is the value of PROPERTY.  This predicate
should return non-nil if there is a match.

Some convenience values for PREDICATE can also be used.  `t'
means the same as `equal'.  `nil' means almost the same as \"not
equal\", but will also end the match if the value of PROPERTY
changes.  See the manual for extensive examples.

If `not-immediate', if the match is under point, it will not be
returned, but instead the next instance is returned, if any.

The return value (if a match is made) is a `prop-match'
structure.  The accessor avaliable are
`prop-match-beginning'/`prop-match-end' (which are the region in
the buffer that's matching, and `prop-match-value', which is the
value of PROPERTY at the start of the region."
  (interactive
   (list
    (let ((string (completing-read "Search for property: " obarray)))
      (when (> (length string) 0)
        (intern string obarray)))))
  ;; We're standing in the property we're looking for, so find the
  ;; end.
  (if (and (text-property--match-p value (get-text-property (point) property)
                                   predicate)
           (not not-immediate))
      (text-property--find-end (point) property value predicate)
    (let ((origin (point))
          (ended nil)
          pos)
      ;; Fix the next candidate.
      (while (not ended)
        (setq pos (next-single-property-change (point) property))
        (if (not pos)
            (progn
              (goto-char origin)
              (setq ended t))
          (goto-char pos)
          (if (text-property--match-p value (get-text-property (point) property)
                                      predicate)
              (setq ended
                    (text-property--find-end (point) property value predicate))
            ;; Skip past this section of non-matches.
            (setq pos (next-single-property-change (point) property))
            (unless pos
              (goto-char origin)
              (setq ended t)))))
      (and (not (eq ended t))
           ended))))

(defun text-property--find-end (start property value predicate)
  (let (end)
    (if (and value
             (null predicate))
        ;; This is the normal case: We're looking for areas where the
        ;; values aren't, so we aren't interested in sub-areas where the
        ;; property has different values, all non-matching value.
        (let ((ended nil))
          (while (not ended)
            (setq end (next-single-property-change (point) property))
            (if (not end)
                (progn
                  (goto-char (point-max))
                  (setq end (point)
                        ended t))
              (goto-char end)
              (unless (text-property--match-p
                       value (get-text-property (point) property) predicate)
                (setq ended t)))))
      ;; End this at the first place the property changes value.
      (setq end (next-single-property-change
                 (point) property nil (point-max)))
      (goto-char end))
    (make-prop-match :beginning start
                     :end end
                     :value (get-text-property start property))))

(defun text-property--match-p (value prop-value predicate)
  (cond
   ((eq predicate t)
    (setq predicate #'equal))
   ((eq predicate nil)
    (setq predicate (lambda (val p-val)
                      (not (equal val p-val))))))
  (funcall predicate value prop-value))

(provide 'subr-x)

;;; subr-x.el ends here
