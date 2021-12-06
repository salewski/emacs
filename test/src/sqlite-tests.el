;;; sqlite-tests.el --- Tests for sqlite.el  -*- lexical-binding: t; -*-

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

(require 'ert)
(require 'ert-x)

(ert-deftest sqlite-select ()
  (skip-unless (sqlite-available-p))
  (let ((db (sqlite-open)))
    (should (eq (type-of db) 'sqlite))
    (should (sqlitep db))
    (should-not (sqlitep 'foo))

    (should
     (zerop
      (sqlite-execute
       db "create table if not exists test1 (col1 text, col2 integer, col3 float, col4 blob)")))

    (should-error
     (sqlite-execute
      db
      "insert into test1 (col1, col2, col3, col4) values ('foo', 2, 9.45, 'bar', 'zot')"))

    (should
     (=
      (sqlite-execute
       db
       "insert into test1 (col1, col2, col3, col4) values ('foo', 2, 9.45, 'bar')")
      1))

    (should
     (equal
      (sqlite-select  db "select * from test1")
      '(("col1" "col2" "col3" "col4") ("foo" 2 9.45 "bar"))))))

;; (setq db (sqlite-open))

(ert-deftest sqlite-set ()
  (skip-unless (sqlite-available-p))
  (let ((db (sqlite-open))
        set)
    (should
     (zerop
      (sqlite-execute
       db "create table if not exists test1 (col1 text, col2 integer)")))

    (should
     (=
      (sqlite-execute
       db "insert into test1 (col1, col2) values ('foo', 1)")
      1))
    (should
     (=
      (sqlite-execute
       db "insert into test1 (col1, col2) values ('bar', 2)")
      1))

    (setq set (sqlite-select db "select * from test1" nil t))
    (should (sqlitep set))
    (should (sqlite-more-p set))
    (should
     (equal (sqlite-next set)
            '("foo" 1)))
    (should
     (equal (sqlite-next set)
            '("bar" 2)))
    (should-not (sqlite-next set))
    (should-not (sqlite-more-p set))))

(ert-deftest sqlite-chars ()
  (skip-unless (sqlite-available-p))
  (let ((db (sqlite-open)))
    (sqlite-execute
     db "create table if not exists test2 (col1 text, col2 integer)")
    (sqlite-execute
     db "insert into test2 (col1, col2) values ('fóo', 3)")
    (sqlite-execute
     db "insert into test2 (col1, col2) values ('fo', 4)")
    (should
     (equal (sqlite-select db "select * from test2")
            '(("col1" "col2") ("fóo" 3) ("fo" 4))))))

;;; sqlite-tests.el ends here
