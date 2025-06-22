;;; org-inc.el --- Incremental learning in Org-mode -*- lexical-binding: t; -*-

;; Copyright (C) 2025 Bohong Huang

;; Author: Bohong Huang <bohonghuang@qq.com>
;; Maintainer: Bohong Huang <bohonghuang@qq.com>
;; Version: 1.0
;; Package-Requires: ((emacs "27.1") (org "9.6") (org-srs "1.0"))
;; URL: https://github.com/bohonghuang/org-srs
;; Keywords: tools

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

;; This package implements SuperMemo-like incremental learning in
;; Org-mode, leveraging Org-srs.

;;; Code:

(defgroup org-inc nil
  "Incremental learning inside Org-mode."
  :group 'org
  :prefix "org-inc"
  :link '(url-link "https://github.com/bohonghuang/org-inc"))

(require 'org-inc-algorithm)
(require 'org-inc-cache)
(require 'org-inc-item)
(require 'org-inc-overlay)
(require 'org-inc-priority)
(require 'org-inc-review)
(require 'org-inc-topic)

(provide 'org-inc)
;;; org-inc.el ends here
