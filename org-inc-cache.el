;;; org-inc-cache.el --- Priority caching mechanism for Org-inc -*- lexical-binding: t; -*-

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

;; This package implements a caching mechanism to enhance the
;; performance of Org-inc, allowing for fast querying of priority
;; data.

;;; Code:

(require 'cl-lib)
(require 'cl-generic)

(require 'org-srs-query)

(require 'org-inc-priority)

(defvar org-inc-priority-cache nil)

(defun org-inc-priority-cache-before-change (beg end)
  (when (eq major-mode 'org-mode)
    (save-excursion
      (cl-loop with cache = (or org-inc-priority-cache (cl-return))
               initially (goto-char end) (setf end (pos-eol)) (goto-char beg) (beginning-of-line)
               while (re-search-forward org-inc-priority-regexp end t)
               do (remhash (string-to-number (match-string-no-properties 2)) cache)))))

(defun org-inc-priority-cache-after-change (beg end _len)
  (when (eq major-mode 'org-mode)
    (save-excursion
      (cl-loop with cache = (or org-inc-priority-cache (cl-return))
               initially (goto-char end) (setf end (pos-eol)) (goto-char beg) (beginning-of-line)
               while (re-search-forward org-inc-priority-regexp end t)
               do (setf (gethash (string-to-number (match-string-no-properties 2)) cache) (copy-marker (pos-bol)))))))

(defvar org-inc-cache-mode)

(define-advice org-inc-priorities (:around (fun &rest args) org-inc-cache)
  (if org-inc-cache-mode
      (if args
          (apply fun args)
        (if org-inc-priority-cache
            (cl-sort
             (org-srs-query-with-loop
               (cl-loop for priority being the hash-key of org-inc-priority-cache using (hash-value marker)
                        collect (cons priority marker)))
             #'< :key #'car)
          (cl-loop with cache = (setf org-inc-priority-cache (make-hash-table :test #'eql))
                   for (priority . marker) in (apply fun args)
                   do (setf (gethash priority cache) marker)
                   finally (cl-return (apply #'org-inc-priorities args)))))
    (apply fun args)))

(defun org-inc-priority-cache-clear ()
  (setf org-inc-priority-cache nil))

;;;###autoload
(define-minor-mode org-inc-cache-mode
  "Minor mode for caching priority changes in Org-inc."
  :group 'org-inc
  (if org-inc-cache-mode
      (progn
        (add-hook 'before-change-functions #'org-inc-priority-cache-before-change nil t)
        (add-hook 'after-change-functions #'org-inc-priority-cache-after-change nil t)
        (add-hook 'kill-buffer-hook #'org-inc-priority-cache-clear nil t))
    (remove-hook 'before-change-functions #'org-inc-priority-cache-before-change t)
    (remove-hook 'after-change-functions #'org-inc-priority-cache-after-change t)
    (remove-hook 'kill-buffer-hook #'org-inc-priority-cache-clear t)))

(provide 'org-inc-cache)
;;; org-inc-cache.el ends here
