;;; org-inc-review.el --- Review strategy for topics and items in Org-inc -*- lexical-binding: t; -*-

;; Copyright (C) 2025 Bohong Huang

;; This file is not part of GNU Emacs.

;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; This package provides functionality for review interaction and implements
;; an interleaved review strategy for topics and items in Org-inc.

;;; Code:

(require 'cl-lib)
(require 'cl-generic)

(require 'org-srs-time)
(require 'org-srs-table)
(require 'org-srs-query)
(require 'org-srs-item)
(require 'org-srs-review)
(require 'org-srs-review-rate)
(require 'org-srs-review-strategy)

(require 'org-inc-item)

(defun org-inc-item-review-count (&rest args)
  (let ((args (or args (org-srs-item-at-point))))
    (org-srs-item-with-current args
      (cl-loop with type = (org-inc-item-args-type args)
               initially (org-srs-table-goto-starred-line)
               for timestamp = (progn (forward-line -1) (org-srs-table-field 'timestamp))
               until (string-empty-p timestamp)
               while (org-srs-time-today-p (org-srs-timestamp-time timestamp))
               count (cl-case type
                       (topic (member (org-srs-table-ensure-read-field (org-srs-table-field 'action)) '(:review :dismiss :transform)))
                       (t t))))))

(cl-defmethod org-srs-review-strategy-items ((type (eql 'done)) (_strategy (eql 'rotate)) &rest args)
  (apply #'org-srs-review-strategy-items type 'union (mapcar #'cdr args)))

(cl-defmethod org-srs-review-strategy-items ((type (eql 'todo)) (_strategy (eql 'rotate)) &rest args)
  (cl-some
   (apply-partially #'org-srs-review-strategy-items type)
   (cl-mapl
    (lambda (cons) (setf (car cons) (cdar cons)))
    (cl-stable-sort
     (cl-loop for (repeat . strategy) in args
              for review-count = (cl-loop for item in (org-srs-review-strategy-items 'done strategy)
                                          sum (org-srs-item-with-current item (org-inc-item-review-count)))
              collect (cons (truncate review-count repeat) strategy))
     #'< :key #'car))))

(cl-defun org-srs-query-predicate-review-count (&optional (count 1))
  (lambda () (>= (org-inc-item-review-count) count)))

(cl-defmethod org-srs-review-strategy-items ((_type (eql 'todo)) (_strategy (eql 'topic)) &rest _args)
  (defvar org-srs-review-source)
  (defvar org-srs-review-strategy-due-predicate)
  (org-srs-query `(and ,org-srs-review-strategy-due-predicate topic (not suspended)) org-srs-review-source))

(cl-defmethod org-srs-review-strategy-items ((_type (eql 'done)) (_strategy (eql 'topic)) &rest _args)
  (defvar org-srs-review-source)
  (org-srs-query `(and topic (not suspended) (review-count 1)) org-srs-review-source))

(defvar org-inc-continue)

(defun org-inc-continue (&rest args)
  "Continue the current review process.

ARGS are optional arguments specifying the current review item.
If there is no ongoing review session, start a new one."
  (interactive)
  (let ((org-inc-continue (if (boundp 'org-inc-continue) org-inc-continue t))
        (interactivep (called-interactively-p 'any)))
    (if-let ((item (or args (bound-and-true-p org-srs-review-item))))
        (cl-multiple-value-bind (entry-beginning entry-end)
            (save-excursion
              (goto-char (apply #'org-srs-item-marker item))
              (cl-values (org-srs-entry-beginning-position) (org-srs-entry-end-position)))
          (let ((before-first-heading-p (save-excursion
                                          (and (goto-char entry-end) (org-before-first-heading-p)
                                               (goto-char entry-beginning) (org-before-first-heading-p)))))
            (if (or (buffer-narrowed-p)
                    (and (= (org-srs-entry-beginning-position) (point-min))
                         (= (org-srs-entry-end-position t) (point-max)))
                    before-first-heading-p)
                (progn
                  (cl-assert (org-srs-reviewing-p))
                  (cl-case (org-inc-item-args-type item)
                    (topic (org-srs-property-let ((org-srs-schedule-step-learning-steps nil)
                                                  (org-srs-schedule-step-relearning-steps nil))
                             (funcall
                              (if interactivep #'call-interactively #'funcall)
                              #'org-srs-item-confirm-command)))
                    (t (if-let ((command (org-srs-item-confirm-pending-p)))
                           (if (and interactivep (commandp command))
                               (call-interactively command)
                             (apply command args))
                         (cl-assert interactivep)
                         (if (<= entry-beginning (point) entry-end)
                             (org-srs-item-with-current item
                               (org-srs-review-rate
                                (let ((choices (cl-loop for rating in org-srs-review-ratings
                                                        for name = (string-remove-prefix ":" (symbol-name rating))
                                                        collect (list (aref name 0) (capitalize name)))))
                                  (nth (cl-position (read-multiple-choice "Choose rating: " choices) choices) org-srs-review-ratings))))
                           (goto-char entry-beginning)
                           (unless before-first-heading-p
                             (org-narrow-to-subtree)))))))
              (apply #'org-srs-item-goto item)
              (org-narrow-to-subtree))))
      (cl-assert interactivep)
      (org-srs-property-let ((org-srs-review-strategy '(rotate (4 . (sort (or #1=(difference due topic) #2=(difference reviewing topic) (ahead #2#) (ahead #1#)) priority))
                                                               (1 . (sort (ahead topic) priority)))))
        (if current-prefix-arg
            (call-interactively #'org-srs-review-start)
          (org-srs-review-start org-inc-directory))))))

(defalias 'org-inc-abort 'org-srs-review-quit)

(provide 'org-inc-review)
;;; org-inc-review.el ends here
