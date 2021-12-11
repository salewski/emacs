;;; multisession.el --- Multisession storage for variables  -*- lexical-binding: t; -*-

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

(defcustom multisession-database-file
  (expand-file-name "multisession.sqlite3" user-emacs-directory)
  "File to store multisession variables."
  :type 'file
  :version "29.1"
  :group 'files)

(defmacro define-multisession-variable (name initial-value &optional doc
                                             &rest args)
  "Make NAME into a multisession variable initialized from INITIAL-VALUE.
DOC should be a doc string, and ARGS are keywords as applicable to
`make-multisession'."
  (declare (indent defun))
  `(defvar ,name
     (make-multisession :key ',name
                        :initial-value ,initial-value
                        ,@args)
     ,@(list doc)))

(cl-defstruct (multisession
               (:constructor nil)
               (:constructor multisession--create)
               (:conc-name multisession--))
  "A persistent variable that will live across Emacs invocations."
  key
  (initial-value nil)
  package
  (synchronized t)
  (cached-value :)
  (cached-sequence 0))

(cl-defun make-multisession (&key key initial-value package)
  "Create a multisession object."
  (unless key
    (error "No key for the multisession object"))
  (unless package
    (setq package (intern (replace-regexp-in-string "-.*" ""
                                                    (symbol-name key)))))
  (multisession--create
   :key key
   :initial-value initial-value
   :package package))

(defvar multisession--db nil)

(defun multisession--ensure-db ()
  (unless multisession--db
    (setq multisession--db (sqlite-open multisession-database-file)))
  (with-sqlite-transaction multisession--db
    (unless (sqlite-select
             multisession--db
             "select name from sqlite_master where type='table' and name='multisession'")
      ;; Create the table.
      (sqlite-execute multisession--db "PRAGMA auto_vacuum = FULL")
      (sqlite-execute
       multisession--db
       "create table multisession (package text not null, key text not null, sequence number not null default 1, value text not null)")
      (sqlite-execute
       multisession--db
       "create unique index multisession_idx on multisession (package, key)"))))

(defun multisession-value (object)
  "Return the value of the multisession OBJECT."
  (if (or (null user-init-file)
          (not (sqlite-available-p)))
      ;; If we don't have storage, then just return the value from the
      ;; object.
      (if (eq (multisession--cached-value object) :)
          (multisession--initial-value object)
        (multisession--cached-value object))
    ;; We have storage, so we update from storage.
    (multisession--ensure-db)
    (let ((id (list (symbol-name (multisession--package object))
                    (symbol-name (multisession--key object)))))
      (cond
       ;; We have no value yet; check the database.
       ((eq (multisession--cached-value object) :)
        (let ((stored
               (car
                (sqlite-select
                 multisession--db
                 "select value, sequence from multisession where package = ? and key = ?"
                 id))))
          (if stored
              (let ((value (car (read-from-string (car stored)))))
                (setf (multisession--cached-value object) value
                      (multisession--cached-sequence object) (cadr stored))
                value)
            ;; Nothing; return the initial value.
            (multisession--initial-value object))))
       ;; We have a value, but we want to update in case some other
       ;; Emacs instance has updated.
       ((multisession--synchronized object)
        (let ((stored
               (car
                (sqlite-select
                 multisession--db
                 "select value, sequence from multisession where sequence > 0 package = ? and key = ?"
                 (cons (multisession--cached-sequence object) id)))))
          (if stored
              (let ((value (read-from-string (caar stored))))
                (setf (multisession--cached-value object) value
                      (multisession--cached-sequence object) (cadar stored))
                value)
            ;; Nothing, return the cached value.
            (multisession--cached-value object))))
       ;; Just return the cached value.
       (t
        (multisession--cached-value object))))))

(defun multisession--set-value (object value)
  (if (or (null user-init-file)
          (not (sqlite-available-p)))
      ;; We have no backend, so just store the value.
      (setf (multisession--cached-value object) value)
    ;; We have a backend.
    (multisession--ensure-db)
    (with-sqlite-transaction multisession--db
      (let ((id (list (symbol-name (multisession--package object))
                      (symbol-name (multisession--key object))))
            (pvalue (prin1-to-string value)))
        (sqlite-execute
         multisession--db
         "insert into multisession(package, key, sequence, value) values(?, ?, 1, ?) on conflict(package, key) do update set sequence = sequence + 1, value = ?"
         (append id (list pvalue pvalue)))
        (setf (multisession--cached-sequence object)
              (caar (sqlite-select
                     multisession--db
                     "select sequence from multisession where package = ? and key = ?"
                     id)))
        (setf (multisession--cached-value object) value)))))

(gv-define-simple-setter multisession-value multisession--set-value)

;; (define-multisession-variable foo 'bar)
;; (multisession-value foo)
;; (multisession--set-value foo 'zot)
;; (setf (multisession-value foo) 'gazonk)

(defvar-keymap multisession-edit-mode-map
  "d" #'multisession-delete-value)

(define-derived-mode multisession-edit-mode special-mode "Multisession"
  "This mode lists all elements in the \"multisession\" database."
  :interactive nil
  (buffer-disable-undo)
  (setq-local buffer-read-only t))

;;;###autoload
(defun list-multisession-values ()
  "List all values in the \"multisession\" database."
  (interactive)
  (multisession--ensure-db)
  (pop-to-buffer (get-buffer-create "*Multisession*"))
  (let ((inhibit-read-only t))
    (erase-buffer)
    (cl-loop for (package key value) in (sqlite-select
                                         multisession--db
                                         "select package, key, value from multisession order by package, key")
             do (insert (propertize (format "%s %s %s\n"
                                            package key value)
                                    'multisession--id (list package key))))
    (goto-char (point-min)))
  (multisession-edit-mode))

(defun multisession-delete-value (id)
  "Delete the value at point."
  (interactive (list (get-text-property (point) 'multisession--id)) multisession-edit-mode)
  (unless id
    (error "No value on the current line"))
  (sqlite-execute multisession--db "delete from multisession where package = ? and key = ?"
                  id)
  (let ((inhibit-read-only t))
    (beginning-of-line)
    (delete-region (point) (progn (forward-line 1) (point)))))

(provide 'multisession)

;;; multisession.el ends here
