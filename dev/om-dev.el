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

(provide 'om-dev-init)
;;; om-dev.el ends here
