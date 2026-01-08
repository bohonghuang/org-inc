;;; org-inc-overlay.el --- Overlay management for Org-inc -*- lexical-binding: t; -*-

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

;; This package provides functionality for visualizing and managing
;; overlays of priorities and text extracts.

;;; Code:

(require 'cl-lib)
(require 'cl-generic)

(require 'org-srs-embed)

(require 'org-inc-priority)
(require 'org-inc-topic)

(cl-defun org-inc-extract-put-overlays (&optional (start (point-min)) (end (point-max)))
  (org-inc-with-embed-cloze-vars (org-srs-embed-put-cloze-overlays start end)))

(cl-defun org-inc-extract-remove-overlays (&optional (start (point-min)) (end (point-max)))
  (org-inc-with-embed-cloze-vars (org-srs-embed-remove-cloze-overlays start end)))

(cl-defun org-inc-priority-put-overlays (&optional (start (point-min)) (end (point-max)))
  (save-excursion
    (cl-loop with entries = (org-inc-priorities)
             with length = (length entries)
             for (nil . marker) in entries
             for index from 0
             when (eq (marker-buffer marker) (current-buffer))
             when (<= start marker (1- end))
             do
             (goto-char marker)
             (re-search-forward org-inc-priority-regexp (pos-eol))
             (let ((overlay (make-overlay (match-beginning 2) (match-end 2))))
               (overlay-put overlay 'category 'org-inc-priority)
               (overlay-put overlay 'invisible nil)
               (overlay-put overlay 'display (format "%.1f%%" (* (org-inc-priority-percentage index length) 100.0)))))))

(cl-defun org-inc-priority-remove-overlays (&optional (start (point-min)) (end (point-max)))
  (remove-overlays start end 'category 'org-inc-priority))

(defvar org-inc-overlay-mode)

(cl-defun org-inc-priority-update-overlays (&rest args)
  (org-with-wide-buffer
   (apply #'org-inc-priority-remove-overlays args)
   (when org-inc-overlay-mode
     (apply #'org-inc-priority-put-overlays args))))

(defun org-inc-update-overlays ()
  (org-inc-extract-update-overlays)
  (org-inc-priority-update-overlays))

(cl-defmethod org-srs-item-new-interactively :after ((_type (eql 'topic)) &rest _args)
  (org-inc-priority-update-overlays))

(cl-defun org-inc-extract-update-overlays (&optional
                                           (start (save-excursion (org-srs-entry-end-of-meta-data t) (point)))
                                           (end (org-srs-entry-end-position)))
  (org-inc-extract-remove-overlays start end)
  (when org-inc-overlay-mode
    (org-inc-extract-put-overlays start end)))

(cl-defun org-inc-remove-overlays (&optional (start (point-min)) (end (point-max)))
  (org-inc-extract-remove-overlays start end)
  (org-inc-priority-remove-overlays start end))

(cl-defun org-inc-put-overlays (&optional (start (point-min)) (end (point-max)))
  (org-inc-extract-put-overlays start end)
  (org-inc-priority-put-overlays start end))

;;;###autoload
(define-minor-mode org-inc-overlay-mode
  "Minor mode for visualizing Org-inc entries and priorities using overlays."
  :group 'org-inc
  (cl-assert (derived-mode-p 'org-mode))
  (if org-inc-overlay-mode
      (org-inc-put-overlays)
    (org-inc-remove-overlays)))

(define-advice org-inc-dismiss (:after (&rest _) org-inc-overlay)
  (when (called-interactively-p 'any)
    (org-inc-update-overlays)))

(define-advice org-inc-priority (:after (&rest _) org-inc-overlay)
  (when (called-interactively-p 'any)
    (org-inc-update-overlays)))

(define-advice org-inc-priority-balance (:after (&rest _) org-inc-overlay)
  (when (called-interactively-p 'any)
    (org-inc-update-overlays)))

(cl-defmethod org-srs-item-review :before ((_ (eql 'topic)) &rest _)
  (org-srs-item-add-hook-once 'org-srs-review-after-rate-hook #'org-inc-update-overlays))

(define-advice org-inc-extract-1 (:around (fun &rest args) org-inc-overlay)
  (let ((marker (point-marker)))
    (prog1 (apply fun args)
      (save-excursion
        (goto-char marker)
        (org-inc-update-overlays)))))

(define-advice org-inc-extract-delete (:after (&rest _) org-inc-overlay)
  (org-inc-update-overlays))

(provide 'org-inc-overlay)
;;; org-inc-overlay.el ends here
