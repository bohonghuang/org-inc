;;; org-inc-topic.el --- Replication of SuperMemo's topic concept in Org-inc -*- lexical-binding: t; -*-

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

;; This package replicates the topic concept in SuperMemo, with
;; support for text extraction and topic-specific operations in
;; review.

;;; Code:

(require 'cl-lib)
(require 'cl-generic)

(require 'org-srs-time)
(require 'org-srs-property)
(require 'org-srs-table)
(require 'org-srs-embed)
(require 'org-srs-review)
(require 'org-srs-item)
(require 'org-srs-algorithm)

(require 'org-inc-algorithm)
(require 'org-inc-priority)
(require 'org-inc-item)

(cl-defun org-inc-topic-a-factor ()
  (org-srs-item-with-current ('topic)
    (cl-loop initially (org-srs-table-goto-starred-line)
             for a-factor = (org-srs-table-ensure-read-field (org-srs-table-field 'a-factor))
             when a-factor return a-factor
             do (forward-line -1)
             until (org-at-table-hline-p))))

(cl-defun org-inc-topic-last-review-timestamp ()
  (save-excursion
    (cl-assert (org-at-table-p))
    (cl-loop initially (org-srs-table-goto-starred-line)
             for action = (org-srs-table-ensure-read-field (org-srs-table-field 'action))
             when (member action '(:create :review))
             return (org-srs-table-field 'timestamp)
             do (forward-line -1)
             until (org-at-table-hline-p))))

(cl-defun org-inc-topic-scale-priority (&optional (scale 2.0))
  (let ((priority (org-inc-priority))
        (priorities (org-inc-priorities)))
    (org-inc-priority-set
     (* (org-inc-priority-percentage
         (cl-position priority priorities :key #'car) (length priorities))
        scale)
     priorities)))

(cl-defmethod org-srs-item-review ((type (eql 'topic)) &rest args)
  (let ((entry-beginning (copy-marker (org-entry-beginning-position)))
        (entry-end (copy-marker (1- (org-entry-end-position)))))
    (org-srs-item-narrow)
    (org-srs-item-add-hook-once
     'org-srs-item-after-confirm-hook
     (lambda ()
       (when (org-srs-reviewing-p)
         (if (<= entry-beginning (point) entry-end)
             (when (and (or (not (boundp 'org-inc-continue)) (symbol-value 'org-inc-continue))
                        (not (boundp 'org-srs-reviewing-p)))
               (condition-case nil
                   (if (and (org-srs-item-cloze-collect (org-entry-beginning-position) (org-entry-end-position)) (y-or-n-p "Transform this topic to cloze deletions?"))
                       (org-inc-transform 'cloze)
                     (let ((args (org-srs-review-rate nil)))
                       (org-with-wide-buffer
                        (goto-char entry-beginning)
                        (org-inc-topic-scale-priority (alist-get 'priority-scale args)))))
                 (quit (apply #'org-srs-item-review type args))))
           (goto-char entry-beginning)
           (apply #'org-srs-item-review type args))))
     90)
    (apply (org-srs-item-confirm) type args)))

(defun org-srs-query-predicate-topic ()
  (lambda () (eq (org-inc-item-args-type) 'topic)))

(defun org-inc-topic-review-timestamps ()
  (cl-values (org-srs-timestamp-now) (org-srs-item-due-timestamp) (org-inc-topic-last-review-timestamp)))

(cl-defmethod org-srs-item-new ((_type (eql 'topic)) &rest _args)
  (org-srs-property-let ((org-srs-algorithm 'topic)
                         (org-srs-item-confirm 'org-inc-continue))
    (cl-call-next-method)
    (setf (org-srs-property-plist) (list :algorithm (org-srs-algorithm) :item-confirm (org-srs-item-confirm))
          (org-srs-item-due-timestamp) (org-srs-timestamp+ (org-srs-item-due-timestamp) (org-inc-a-factor-topic-initial-interval) :day)
          (org-inc-priority) org-inc-priority-max)
    (org-inc-priority-set 0.0)))

(defconst org-inc-extract-regexp (rx "@@inc:[[" (group (+? not-newline)) "][@@" (group (*? anychar)) "@@inc:]]@@"))

(defmacro org-inc-with-embed-cloze-vars (&rest body)
  (declare (indent 0))
  `(progn
     (defvar org-srs-embed-cloze-tag)
     (defvar org-srs-embed-cloze-brackets)
     (defvar org-srs-embed-cloze-overlay-category)
     (let ((org-srs-embed-cloze-tag "inc")
           (org-srs-embed-cloze-brackets '(?\[ ?\]))
           (org-srs-embed-cloze-overlay-category 'org-inc-extract))
       ,@body)))

(cl-defun org-inc-extract-bounds ()
  (org-inc-with-embed-cloze-vars
    (org-srs-embed-cloze-bounds (point) (org-entry-beginning-position) (org-entry-end-position))))

(defun org-inc-extract-delete ()
  (goto-char (car (org-inc-extract-bounds)))
  (cl-assert (looking-at org-inc-extract-regexp))
  (replace-match (match-string 2) t t))

(cl-defun org-inc-extract-1 (&optional (type (org-inc-read-item-type)))
  (org-inc-with-embed-cloze-vars
    (let ((text (buffer-substring-no-properties (region-beginning) (region-end))))
      (cl-multiple-value-bind (id marker)
          (save-mark-and-excursion
            (let* ((level (org-current-level))
                   (priority (or (org-inc-priority)
                                 (org-inc-priority-between
                                  (cl-loop for (priority) in (org-inc-priorities)
                                           maximize priority)
                                  org-inc-priority-max)))
                   (a-factor (org-inc-topic-a-factor))
                   (a-factor-child (org-inc-a-factor-child a-factor))
                   (a-factor-parent (org-inc-a-factor-parent a-factor))
                   (timestamp (org-srs-item-with-current ('topic)
                                (prog2 (org-srs-table-goto-starred-line)
                                    (if (org-srs-timestamp= (org-srs-table-field 'timestamp) org-inc-dismiss-timestamp)
                                        (org-srs-timestamp+ (org-srs-timestamp-now) 1 :day)
                                      (alist-get
                                       'timestamp
                                       (org-srs-algorithm-repeat
                                        'topic
                                        (cons (cons 'a-factor a-factor)
                                              (mapcar (apply-partially #'cons 'timestamp)
                                                      (cl-multiple-value-list (org-inc-topic-review-timestamps)))))))
                                  (org-inc-log-new-record :action :extract :a-factor a-factor-parent)))))
              (org-end-of-subtree t t)
              (org-insert-heading nil t (1+ level))
              (let ((marker (point-marker)))
                (org-return-and-maybe-indent)
                (insert text)
                (cl-values
                 (let ((org-inc-a-factor a-factor-child))
                   (prog1 (org-id-get-create)
                     (org-srs-item-new type)
                     (setf (org-srs-item-due-timestamp)
                           (cl-case type
                             (topic (org-srs-timestamp+ (org-srs-timestamp-now) (org-inc-a-factor-topic-initial-interval a-factor) :day))
                             (t timestamp)))
                     (let ((priorities (org-inc-priorities)))
                       (org-inc-priority-set
                        (org-inc-topic-child-priority
                         (org-inc-priority-percentage
                          (or (cl-position priority priorities :key #'car) (1- (length priorities)))
                          (length priorities)))
                        priorities))
                     (org-srs-log-hide-drawer)))
                 marker))))
        (org-srs-embed-cloze (region-beginning) (region-end) nil (concat "id:" id))
        (cl-case type
          (topic)
          (t (goto-char marker) (end-of-line)))))))

;;;###autoload
(defun org-inc-extract ()
  "Perform text extraction based on the current context.

If the region is active, extract the text from the region.
If called with a `\\[universal-argument]`, delete the text extraction
and associated subtree.
If the point is within an existing text extraction, jump to the
associated entry."
  (interactive)
  (require 'org-srs)
  (cond
   ((region-active-p) (org-inc-extract-1))
   (t (when-let ((position (car (org-inc-extract-bounds))))
        (goto-char position)
        (cl-assert (looking-at org-inc-extract-regexp))
        (cl-destructuring-bind (&optional (arg 1)) current-prefix-arg
          (cl-etypecase arg
            ((integer 4)
             (save-excursion
               (org-link-open-from-string (match-string 1))
               (org-cut-subtree))
             (org-inc-extract-delete))
            ((integer 1)
             (org-link-open-from-string (match-string 1)))))))))

;;;###autoload
(defun org-inc-transform (type)
  "Transform the current Org-inc topic to the item of TYPE in place.

TYPE is the new type to which the current item will be converted."
  (interactive (list (org-inc-read-item-type)))
  (cl-destructuring-bind (item) (org-inc-entry-items)
    (org-srs-item-with-current item
      (cl-ecase (org-inc-item-args-type)
        (topic
         (cl-assert (not (eq type 'topic)))
         (org-srs-table-goto-starred-line)
         (let* ((args (cl-acons
                       'a-factor (save-excursion (forward-line -1) (org-srs-table-ensure-read-field (org-srs-table-field 'a-factor)))
                       (mapcar (apply-partially #'cons 'timestamp)
                               (cl-multiple-value-list (org-inc-topic-review-timestamps)))))
                (args (if (org-srs-time>= (org-srs-time-today) (org-srs-time-today (org-srs-item-due-time)))
                          (org-srs-algorithm-repeat 'topic args)
                        (cl-acons 'timestamp (org-srs-item-due-timestamp) args)))
                (timestamp (alist-get 'timestamp args))
                (a-factor (alist-get 'a-factor args)))
           (org-inc-log-new-record :action :transform :timestamp timestamp :a-factor a-factor)
           (setf (org-srs-item-due-timestamp) org-inc-dismiss-timestamp)
           (org-srs-item-new type)
           (setf (org-srs-item-due-timestamp) timestamp)))))
    (org-srs-log-hide-drawer))
  (when (org-srs-reviewing-p)
    (org-srs-review-next)))

(provide 'org-inc-topic)
;;; org-inc-topic.el ends here
