;;; org-inc-priority.el --- Priority management for Org-inc -*- lexical-binding: t; -*-

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

;; This package provides functionality for managing relative priorities
;; and performing priority operations in Org-inc.

;;; Code:

(require 'cl-lib)
(require 'cl-generic)

(require 'org-srs-property)
(require 'org-srs-query)
(require 'org-srs-review-cache)

(defun org-inc-f64-u64le (f64)
  (cl-assert (not (isnan f64)))
  (cl-destructuring-bind (fraction . exponent) (frexp (abs f64))
    (cl-assert (<= fraction 1.0))
    (let ((minusp (cl-minusp f64))
          (biased-exponent 0)
          (mantissa 0))
      (unless (zerop fraction)
        (setf biased-exponent (+ exponent 1022)
              mantissa (truncate (ldexp fraction 53))))
      (let* ((sign-bit (if minusp #x8000000000000000 0))
             (exponent (ash (logand biased-exponent #x7FF) 52))
             (mantissa (logand mantissa (1- (ash 1 52)))))
        (logior sign-bit exponent mantissa)))))

(defun org-inc-u64le-f64 (u64le)
  (let* ((msb0 (logand (ash u64le -56) #xFF))
         (msb1 (logand (ash u64le -48) #xFF))
         (minusp (= #x80 (logand msb0 #x80)))
         (exponent (logand (logior (ash msb0 4) (ash msb1 -4)) #x7FF))
         (mantissa (logior #x10000000000000 (logand u64le #xFFFFFFFFFFFFF)))
         (result (cond
                  ((/= #x7FF exponent) (ldexp (ldexp mantissa -53) (- exponent 1022)))
                  ((= #x10000000000000 mantissa) 1.0e+INF)
                  (t 0.0e+NaN))))
    (if minusp (- result) result)))

(defconst org-inc-priority-min (org-inc-f64-u64le 0.0))
(defconst org-inc-priority-max (org-inc-f64-u64le 1.0))

(defun org-inc-priority-between (a b)
  (org-inc-f64-u64le (/ (+ (org-inc-u64le-f64 a) (org-inc-u64le-f64 b)) 2.0)))

(defun org-inc-priority-percentage (current total)
  (/ (float current) (float (max 1 (1- total)))))

(defun org-inc-priority-default ()
  (if-let ((priorities (org-inc-priorities)))
      (org-inc-priority-between org-inc-priority-min (car (cl-first priorities)))
    (org-inc-f64-u64le 0.5)))

(defvar org-inc-priority-default #'org-inc-priority-default)

(defconst org-inc-priority-regexp (rx (+ "*") (+ blank) (? "COMMENT" (+ blank)) (group "[#" (group (+ digit)) "]" (+ blank)) (* not-newline)))

(defun org-inc-priority (&optional value)
  "Set the priority for the current Org-inc item.
  
VALUE is the new priority value, which should be a float between 0.0 and 1.0.
PRIORITIES is the list of current items' priorities sorted from high to low."
  (interactive (list (float (read-number "Priority [0.0-1.0]: " 0.5))))
  (if value
      (org-inc-priority-set value)
    (save-excursion
      (org-back-to-heading)
      (when (re-search-forward org-inc-priority-regexp (line-end-position) t)
        (string-to-number (match-string 2))))))

(defun \(setf\ org-inc-priority\) (value)
  (save-excursion
    (org-back-to-heading)
    (if (re-search-forward org-inc-priority-regexp (line-end-position) t)
        (if value
            (replace-match (prin1-to-string value) t t nil 2)
          (replace-match "" t t nil 1))
      (cl-assert (looking-at org-complex-heading-regexp))
      (goto-char (or (match-beginning 4) (match-end 3) (match-end 2) (match-end 1)))
      (unless (match-beginning 4) (forward-char 1))
      (insert (format "[#%d] " value)))))

(cl-defun org-inc-priority-previous (&optional (current (org-inc-priority)))
  (org-with-wide-buffer
   (cl-loop initially (goto-char (point-min))
            for priority = org-inc-priority-min then (string-to-number (match-string-no-properties 2))
            when (< priority current)
            maximize priority
            while (re-search-forward org-inc-priority-regexp nil t))))

(cl-defun org-inc-priority-next (&optional (current (org-inc-priority)))
  (org-with-wide-buffer
   (cl-loop initially (goto-char (point-min))
            for priority = org-inc-priority-max then (string-to-number (match-string-no-properties 2))
            when (> priority current)
            minimize priority
            while (re-search-forward org-inc-priority-regexp nil t))))

(defcustom org-inc-directory (expand-file-name "org-inc/" org-directory)
  "The root directory for Org-inc files."
  :group 'org-inc
  :type 'directory)

(cl-defun org-inc-priorities (&optional (source org-inc-directory))
  (org-with-wide-buffer
   (let ((entries nil))
     (org-srs-property-let ((org-srs-review-cache-p nil))
       (org-srs-query
        (lambda ()
          (save-excursion
            (org-back-to-heading)
            (when (re-search-forward org-inc-priority-regexp (pos-eol) t)
              (let ((priority (string-to-number (match-string-no-properties 2)))
                    (position (copy-marker (pos-bol))))
                (unless (eql (car (cl-first entries)) priority)
                  (push (cons priority position) entries))))))
        source))
     (cl-sort entries #'< :key #'car))))

(cl-defun org-inc-priority-balance (&optional (priorities (org-inc-priorities)))
  (cl-loop with step = (/ 1.0 (1+ (length priorities)))
           for (nil . marker) in priorities
           for priority from step by step
           do (with-current-buffer (marker-buffer marker)
                (org-with-wide-buffer
                 (goto-char marker)
                 (setf (org-inc-priority) (org-inc-f64-u64le priority))))))

(defun org-inc-priority-set (value &optional priorities)
  (cl-etypecase value
    (float (let* ((priorities (or priorities (org-inc-priorities)))
                  (length (length priorities))
                  (nth (truncate (* (1- length) (min (max value 0.0) 1.0)))))
             (cl-destructuring-bind (start &optional (end (list org-inc-priority-max)) &rest args)
                 (if (zerop nth) (cons (list org-inc-priority-min) priorities) (nthcdr nth priorities))
               (let* ((left (car start))
                      (right (car end))
                      (mid (org-inc-priority-between left right)))
                 (cl-assert (/= left mid))
                 (cl-assert (/= mid right))
                 (cl-assert (/= left right))
                 (setf (org-inc-priority) mid)))))
    (integer (setf (org-inc-priority)
                   (cl-loop for previous = org-inc-priority-min then (if (= current value) previous current)
                            for (current . nil) in (org-inc-priorities)
                            when (< value current)
                            return (org-inc-priority-between previous current)
                            finally (cl-return (org-inc-priority-between current org-inc-priority-min)))))))

(provide 'org-inc-priority)
;;; org-inc-priority.el ends here
