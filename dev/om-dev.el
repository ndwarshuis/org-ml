;;; om-dev.el --- Initialize om.el development -*- lexical-binding: t; -*-

;; Copyright (C) 2019 Nathan Dwarshuis

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

(defvar om-dev-path
  (directory-file-name (file-name-directory load-file-name))
  "Path to development directory.")

(defvar om-dev-root-path
  (directory-file-name (file-name-directory om-dev-path))
  "Path to root directory.")

(add-to-list 'load-path om-dev-root-path)
(add-to-list 'load-path om-dev-path)

(require 'om)

(defconst om-dev-defined-names nil
  "Alist of all functions/macros defined in `om.el'.
The two cells in the alist are 'private' and 'public'.")

(mapatoms
 (lambda (x)
   (when (and (fboundp x) (s-starts-with-p "om-" (symbol-name x)))
     (push x om-dev-defined-names))))

(setq om-dev-defined-names
      (--group-by
       (if (s-starts-with-p "om--" (symbol-name it)) 'private 'public)
       om-dev-defined-names))

(provide 'om-dev-init)
;;; om-dev.el ends here
