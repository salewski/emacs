;;; text-property-search.el --- search for text properties  -*- lexical-binding:t -*-

;; Copyright (C) 2018 Free Software Foundation, Inc.

;; Author: Lars Magne Ingebrigtsen <larsi@gnus.org>
;; Keywords: convenience

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

;;; Code:

(eval-when-compile (require 'cl-lib))

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
  (text-property--search #'next-single-property-change #'point-max
                         property value predicate not-immediate
                         (point)))

(defun text-property-search-backward (property &optional value predicate
                                               not-immediate)
  "Search for the next region that has text property PROPERTY set to VALUE.
See `text-property-search-forward' for further documentation."
  (interactive
   (list
    (let ((string (completing-read "Search for property: " obarray)))
      (when (> (length string) 0)
        (intern string obarray)))))
  (let ((match
         (text-property--search #'text-property--previous-change #'point-min
                                property value predicate not-immediate
                                (max (1- (point)) (point-min)))))
    (when match
      ;; We have to exchange beginning and end since everything's
      ;; backwards when searching backwards.  Also adjust the end
      ;; point to the correct place.
      (cl-rotatef (prop-match-beginning match) (prop-match-end match))
      (setf (prop-match-beginning match) (1+ (prop-match-beginning match)))
      (setf (prop-match-end match) (1+ (prop-match-end match))))
    match))

(defun text-property--previous-change (position prop &optional object limit)
  (when-let ((pos (previous-single-property-change position prop
                                                   object limit)))
    (max (1- pos) (point-min))))

(defun text-property--search (next-func extreme-func
                                        property value predicate not-immediate
                                        start)
  ;; We're standing in the property we're looking for, so find the
  ;; end.
  (if (and (text-property--match-p value (get-text-property start property)
                                   predicate)
           (not not-immediate))
      (text-property--find-end (point) property value predicate
                               next-func extreme-func)
    (let ((origin (point))
          (ended nil)
          pos)
      ;; Fix the next candidate.
      (while (not ended)
        (setq pos (funcall next-func (point) property))
        (if (not pos)
            (progn
              (goto-char origin)
              (setq ended t))
          (goto-char pos)
          (if (text-property--match-p value (get-text-property (point) property)
                                      predicate)
              (setq ended
                    (text-property--find-end (point) property value predicate
                                             next-func extreme-func))
            ;; Skip past this section of non-matches.
            (setq pos (funcall next-func (point) property))
            (unless pos
              (goto-char origin)
              (setq ended t)))))
      (and (not (eq ended t))
           ended))))

(defun text-property--find-end (start property value predicate
                                      next-func extreme-func)
  (let (end)
    (if (and value
             (null predicate))
        ;; This is the normal case: We're looking for areas where the
        ;; values aren't, so we aren't interested in sub-areas where the
        ;; property has different values, all non-matching value.
        (let ((ended nil))
          (while (not ended)
            (setq end (funcall next-func (point) property))
            (if (not end)
                (progn
                  (goto-char (funcall extreme-func))
                  (setq end (point)
                        ended t))
              (goto-char end)
              (unless (text-property--match-p
                       value (get-text-property (point) property) predicate)
                (setq ended t)))))
      ;; End this at the first place the property changes value.
      (setq end (funcall next-func (point) property nil
                         (funcall extreme-func)))
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

(provide 'text-property-search)
