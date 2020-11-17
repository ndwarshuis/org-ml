;;; org-ml-test-common.el --- Common Test functions -*- lexical-binding: t; -*-

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

(require 's)
(require 'dash)
(require 'buttercup)

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

(defun example-to-should (actual sym expected)
  (let ((expected
         (if (eq (and (listp expected) (car expected)) :result)
             (s-join "\n" (cdr expected))
           expected)))
    (cond ((eq sym '=>)
           `(expect ,actual :to-equal ,expected))
          ;; this will only work with defexamples-content
          ((eq sym '$>)
           `(expect (progn ,actual (s-trim (buffer-string))) :to-equal ,expected))
          ;; TODO I never use this?
          ((eq sym '~>)
           `(should (approx-equal ,actual ,expected)))
          ((eq sym '!!>)
           `(should-error (eval ',actual) :type ',expected))
          (t
           (error "Invalid test case: %S" `(,actual ,sym ,expected))))))

(defmacro defexamples (cmd &rest examples)
  (let ((tests (->> examples
                    (remove :begin-hidden)
                    (remove :end-hidden)
                    (-partition 3)
                    (--map (apply #'example-to-should it)))))
    (when tests
      `(it ,(format "%S" cmd) (org-ml--with-org-env ,@tests)))))

(defmacro defexamples-content (cmd _docstring &rest args)
  (cl-flet*
      ((make-test-form
        (test contents)
        `(org-ml--with-org-env
          (when ,contents (insert ,contents))
          (goto-char (point-min))
          ,test))
       (make-tests
        (list)
        (let ((contents (->> (car list) (-drop 1) (s-join "\n")))
              (tests
               (->> (-drop 1 list)
                    (--remove (eq (and (listp it) (car it)) :comment))
                    (-partition 3)
                    (--map (apply #'example-to-should it)))))
          (--map (make-test-form it contents) tests))))
    (let ((body
           (->> args
                (remove :begin-hidden)
                (remove :end-hidden)
                (-partition-before-pred
                 (lambda (it) (eq (and (listp it) (car it)) :buffer)))
                (-mapcat #'make-tests))))
      (when body
        `(it ,(format "%S" cmd) ,@body)))))

(defmacro def-example-subgroup (title _subtitle &rest specs)
  (when specs
    `(describe ,title ,@specs)))

(defmacro def-example-group (title _subtitle &rest specs)
  (when specs
    `(describe ,title ,@specs)))

(provide 'org-ml-test-common)
;;; org-ml-test-common.el ends here
