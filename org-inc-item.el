;;; org-inc-item.el --- Item facilities for Org-inc -*- lexical-binding: t; -*-

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

;; This package provides facilities for item operations and access within
;; Org-inc.

;;; Code:

(require 'cl-lib)

(require 'org-srs-time)
(require 'org-srs-item)
(require 'org-srs-log)
(require 'org-srs-table)
(require 'org-srs-query)
(require 'org-srs-review)
(require 'org-srs-review-cache)

(require 'org-inc-priority)

(cl-defun org-inc-item-args-type (&optional (item (cl-multiple-value-list (org-srs-item-at-point))))
  (car (ensure-list (cl-first item))))

(cl-defun org-inc-log-new-record (&rest args &key (timestamp (org-srs-item-due-timestamp)) &allow-other-keys)
  (org-srs-table-goto-starred-line)
  (org-srs-table-with-temp-buffer
    (cl-loop for (name . value) in (save-excursion (forward-line -1) (org-srs-table-current-line))
             do (setf (org-srs-table-field name) (cl-case name (timestamp (org-srs-timestamp-now)) (t (prin1-to-string value t)))))
    (cl-loop for (name . value) in (org-srs-log-plist-alist args)
             do (setf name (intern (string-remove-prefix ":" (symbol-name name))))
             unless (eq name 'timestamp)
             do (setf (org-srs-table-field name) (prin1-to-string value t)))
    (org-srs-table-forward-star)
    (org-srs-table-goto-starred-line))
  (setf (org-srs-item-due-timestamp) timestamp))

(defun org-inc-read-item-type ()
  (read (completing-read "Item type: " (org-srs-item-types) nil t nil nil '("topic"))))

(defun org-inc-entry-items ()
  (org-srs-property-let ((org-srs-review-cache-p nil))
    (org-srs-query '(and) (cons (org-entry-beginning-position) (org-entry-end-position)))))

(defconst org-inc-dismiss-timestamp "2999-12-31T23:59:59Z")

(defalias 'org-inc-create 'org-srs-item-create)

;;;###autoload
(defun org-inc-dismiss ()
  "Dismiss the current Org-inc item."
  (interactive)
  (save-excursion
    (cl-loop for item in (org-inc-entry-items)
             do (org-srs-item-with-current item
                  (unless (org-srs-timestamp= (org-srs-item-due-timestamp) org-inc-dismiss-timestamp)
                    (apply #'org-inc-log-new-record
                           :timestamp org-inc-dismiss-timestamp
                           (cl-case (org-inc-item-args-type)
                             (topic (list :action :dismiss))
                             (t))))))
    (org-srs-log-hide-drawer))
  (setf (org-inc-priority) nil)
  (when (org-srs-reviewing-p)
    (org-srs-review-next)))

;;;###autoload
(cl-defun org-inc-postpone (&optional (time '(1 :day)))
  "Postpone the current Org-inc item by TIME."
  (interactive (list (read-from-minibuffer "Interval: " (prin1-to-string '(1 :day)) nil t)))
  (cl-assert (org-srs-reviewing-p))
  (org-srs-item-with-current org-srs-review-item
    (cl-case (org-inc-item-args-type)
      (topic (org-inc-log-new-record :action :postpone))))
  (org-srs-review-postpone time))

(provide 'org-inc-item)
;;; org-inc-item.el ends here
