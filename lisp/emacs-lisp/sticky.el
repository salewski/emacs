;;; sticky.el --- Sticky storage for variables  -*- lexical-binding: t; -*-

;; Copyright (C) 2021 Free Software Foundation, Inc.

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

;;

;;; Code:

(require 'cl-lib)
(require 'eieio)
(require 'sqlite)

(defmacro define-sticky-variable (name initial-value &optional doc
                                       &rest args)
  "Make NAME into a sticky variable initialized from INITIAL-VALUE.
DOC should be a doc string, and ARGS are keywords as applicable to
`make-sticky'."
  (declare (indent defun))
  `(defvar ,name
     (make-sticky :key ',name
                  :initial-value ,initial-value
                  ,@args)
     ,@(list doc)))

(cl-defstruct (sticky
               (:constructor nil)
               (:constructor sticky--create)
               (:conc-name sticky--))
  "A persistent variable that will live across Emacs invocations."
  key
  (initial-value nil)
  package
  (synchronized t)
  (cached-value :)
  (cached-sequence 0))

(cl-defun make-sticky (&key key initial-value package)
  "Create a sticky object."
  (unless key
    (error "No key for the sticky object"))
  (unless package
    (setq package (intern (replace-regexp-in-string "-.*" ""
                                                    (symbol-name key)))))
  (sticky--create
   :key key
   :initial-value initial-value
   :package package))

(defvar sticky--db nil)

(defun sticky--ensure-db ()
  (unless sticky--db
    (setq sticky--db
          (sqlite-open (expand-file-name "sticky.sqlite" "~/.emacs.d/"))))
  (with-sqlite-transaction sticky--db
    (unless (sqlite-select
             sticky--db
             "select name from sqlite_master where type='table' and name='sticky'")
      ;; Create the table.
      (sqlite-execute
       sticky--db
       "create table sticky (package text not null, key text not null, sequence number not null, value text not null)")
      (sqlite-execute
       sticky--db
       "create unique index sticky_idx on sticky (package, key)"))))

(defun sticky-value (object)
  "Return the value of the sticky OBJECT."
  (if (or (null user-init-file)
          (not (sqlite-available-p)))
      ;; If we don't have storage, then just return the value from the
      ;; object.
      (if (eq (sticky--cached-value object) :)
          (sticky--initial-value object)
        (sticky--cached-value object))
    ;; We have storage, so we update from storage.
    (sticky--ensure-db)
    (let ((id (list (symbol-name (sticky--package object))
                    (symbol-name (sticky--key object)))))
      (cond
       ;; We have no value yet; check the database.
       ((eq (sticky--cached-value object) :)
        (let ((stored
               (car
                (sqlite-select
                 sticky--db
                 "select value, sequence from sticky where package = ? and key = ?"
                 id))))
          (if stored
              (let ((value (car (read-from-string (car stored)))))
                (setf (sticky--cached-value object) value
                      (sticky--cached-sequence object) (cadr stored))
                value)
            ;; Nothing; return the initial value.
            (sticky--initial-value object))))
       ;; We have a value, but we want to update in case some other
       ;; Emacs instance has updated.
       ((sticky--synchronized object)
        (let ((stored
               (car
                (sqlite-select
                 sticky--db
                 "select value, sequence from sticky where sequence > 0 package = ? and key = ?"
                 (cons (sticky--cached-sequence object) id)))))
          (if stored
              (let ((value (read-from-string (caar stored))))
                (setf (sticky--cached-value object) value
                      (sticky--cached-sequence object) (cadar stored))
                value)
            ;; Nothing, return the cached value.
            (sticky--cached-value object))))
       ;; Just return the cached value.
       (t
        (sticky--cached-value object))))))

(defun sticky--set-value (object value)
  (if (or (null user-init-file)
          (not (sqlite-available-p)))
      ;; We have no backend, so just store the value.
      (setf (sticky--cached-value object) value)
    ;; We have a backend.
    (sticky--ensure-db)
    (with-sqlite-transaction sticky--db
      (let* ((id (list (symbol-name (sticky--package object))
                       (symbol-name (sticky--key object))))
             (old-sequence
              (caar
               (sqlite-select
                sticky--db
                "select sequence from sticky where package = ? and key = ?" id))))
        (if old-sequence
            (progn
              (setf (sticky--cached-sequence object) (1+ old-sequence))
              (sqlite-execute
               sticky--db
               "update sticky set value = ?, sequence = ? where package = ? and key = ?"
               (cons (prin1-to-string value)
                     (cons (sticky--cached-sequence object)
                           id))))
          (cl-incf (sticky--cached-sequence object))
          (sqlite-execute
           sticky--db
           "insert into sticky (package, key, sequence, value) values (?, ?, ?, ?)"
           (nconc id (list (sticky--cached-sequence object)
                           (prin1-to-string value)))))
        (setf (sticky--cached-value object) value)))))

(gv-define-simple-setter sticky-value sticky--set-value)

;; (define-sticky-variable foo 'bar)
;; (sticky-value foo)
;; (sticky--set-value foo 'zot)
;; (setf (sticky-value foo) 'gazonk)

(provide 'sticky)

;;; sticky.el ends here
