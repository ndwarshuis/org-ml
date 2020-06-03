;;; org-ml-dev.el --- Initialize om.el development -*- lexical-binding: t; -*-

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

;; This is code used for interactive development and editing, and is
;; required because of the insanity introduced with custom macro
;; definitions and such

;;; Code:

;; override the default for this function so it recognizes my new
;; macros
(defun checkdoc--next-docstring ()
  "When looking at a definition with a doc string, find it.
Move to the next doc string after point, and return t.  When not
looking at a definition containing a doc string, return nil and
don't move point."
  (pcase (save-excursion (condition-case nil
                             (read (current-buffer))
                           ;; Conservatively skip syntax errors.
                           (invalid-read-syntax)))
    ;; added my macro definitions here
    (`(,(or 'org-ml--defun-node 'org-ml--defun-node* 'org-ml--defun-kw 'org-ml--defun
            'org-ml--defun* org-ml--defun-nocheck* 'defun 'defvar 'defcustom
            'defmacro 'defconst 'defsubst 'defadvice)
       ,(pred symbolp)
       ;; Require an initializer, i.e. ignore single-argument `defvar'
       ;; forms, which never have a doc string.
       ,_ . ,_)
     (down-list)
     ;; Skip over function or macro name, symbol to be defined, and
     ;; initializer or argument list.
     (forward-sexp 3)
     (skip-chars-forward " \n\t")
     t)))

(provide 'org-ml-dev-env.el)
;;; org-ml-dev-env.el ends here
