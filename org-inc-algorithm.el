;;; org-inc-algorithm.el --- Incremental learning algorithm for Org-inc -*- lexical-binding: t; -*-

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

;; This package implements a SuperMemo-like incremental learning algorithm
;; for Org-mode, allowing for efficient management of learning intervals
;; and priorities.

;;; Code:

(require 'cl-lib)
(require 'cl-generic)

(require 'org-srs-algorithm)
(require 'org-srs-time)

(defvar org-inc-a-factor 1.2)

(cl-defun org-inc-a-factor (&optional (x org-inc-a-factor))
  (min (max x 1.2) 6.9))

(defun org-inc-a-factor-parent (x)
  (let ((a 0.05) (b 0.95))
    (+ a (* b x))))

(defun org-inc-a-factor-child (x)
  (let ((a 2.65204663673051)
        (b -2.08925219374674)
        (c 4.1555228033838e-07)
        (d -31401387039204.6))
    (+ (/ (- a d) (1+ (expt (/ x c) b))) d)))

(defun org-inc-a-factor-review (x)
  (let ((a -0.20) (b 1.18))
    (+ a (* b x))))

(cl-defun org-inc-a-factor-topic-initial-interval (&optional (a-factor 0.0))
  (let ((a 0.77) (b 2.9))
    (round (+ a (* b a-factor)))))

(defun org-inc-priority-scale (interval-scale)
  (let* ((a -0.0107) (b 1.056) (c -0.115)
         (x interval-scale))
    (+ a (* b x) (* c x x))))

(defun org-inc-topic-child-priority (priority)
  (cl-check-type priority float)
  (* priority (/ 2.0 3.0)))

(cl-deftype org-inc-action ()
  '(member :create :extract :review :postpone :dismiss))

(cl-defmethod org-srs-algorithm-repeat ((_ (eql 'topic)) (_args null))
  `((a-factor . ,org-inc-a-factor) (action . :create)))

(declare-function org-inc-topic-last-review-timestamp "org-inc")

(cl-defmethod org-srs-algorithm-repeat ((_ (eql 'topic)) (args list))
  (cl-destructuring-bind (review-timestamp due-timestamp last-review-timestamp)
      (cl-loop for (name . value) in args when (eq name 'timestamp) collect value)
    (setf last-review-timestamp (org-inc-topic-last-review-timestamp))
    (cl-assert (org-srs-timestamp< last-review-timestamp due-timestamp))
    (let* ((a-factor (alist-get 'a-factor args))
           (last-optimal-interval (round (org-srs-timestamp-difference due-timestamp last-review-timestamp) (* 60 60 24)))
           (optimal-interval (ceiling (* last-optimal-interval a-factor))))
      `((timestamp . ,(org-srs-timestamp+ review-timestamp optimal-interval :day))
        (a-factor . ,(org-inc-a-factor-review a-factor))
        (priority-scale . ,(org-inc-priority-scale (/ optimal-interval last-optimal-interval)))
        (action . :review)))))

(provide 'org-inc-algorithm)
;;; org-inc-algorithm.el ends here
