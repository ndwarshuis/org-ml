;;; org-ml-dev.el --- Initialize org-ml.el development -*- lexical-binding: t; -*-

;; Copyright (C) 2020 Nathan Dwarshuis

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;;; Code:

(defvar org-ml-dev-path
  (directory-file-name (file-name-directory load-file-name))
  "Path to development directory.")

(defvar org-ml-dev-root-path
  (directory-file-name (file-name-directory org-ml-dev-path))
  "Path to root directory.")

(add-to-list 'load-path org-ml-dev-root-path)
(add-to-list 'load-path org-ml-dev-path)

(require 'org-ml)

(defconst org-ml-dev-defined-names nil
  "Alist of all functions/macros defined in `org-ml.el'.
The two cells in the alist are 'private' and 'public'.")

(mapatoms
 (lambda (x)
   (when (and (fboundp x) (s-starts-with-p "org-ml-" (symbol-name x)))
     (push x org-ml-dev-defined-names))))

(setq org-ml-dev-defined-names
      (--group-by
       (if (s-starts-with-p "org-ml--" (symbol-name it)) 'private 'public)
       org-ml-dev-defined-names))

;; set up standard org environment

(defmacro org-ml--with-org-env (&rest body)
  "Execute BODY in a standardized Org-mode buffer."
  `(let ((org-tags-column 20)
         (org-todo-keywords '((sequence "TODO" "DONE")))
         (org-archive-tag "ARCHIVE")
         (org-lowest-priority ?C)
         (org-highest-priority ?A)
         (org-list-allow-alphabetical nil)
         (org-log-into-drawer "LOGBOOK"))
     (with-temp-buffer
       (org-mode)
       ,@body)))

(provide 'org-ml-dev-init)
;;; org-ml-dev.el ends here
