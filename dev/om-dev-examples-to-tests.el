;;; om-dev-examples-to-tests.el --- Extract om.el's tests from examples.el

;; Copyright (C) 2015 Free Software Foundation, Inc.

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

;; FIXME: Lots of duplication with examples-to-info.el.

;;; Code:

(require 'ert)
(require 'dash)

(defun example-to-should (actual sym expected)
  (let ((expected
         (if (eq (and (listp expected) (car expected)) :result)
             (s-join "\n" (cdr expected))
           expected)))
    (cond ((eq sym '=>)
           `(should (equal ,actual ,expected)))
          ;; this will only work with defexamples-content
          ((eq sym '$>)
           `(should (equal (progn ,actual (s-trim (buffer-string))) ,expected)))
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
    `(ert-deftest ,cmd () (om--with-org-env ,@tests))))

(defmacro defexamples-content (cmd _docstring &rest args)
  (cl-flet
      ((make-test
        (list)
        (let ((contents (->> (car list) (-drop 1) (s-join "\n")))
              (tests
               (->> (-drop 1 list)
                    (--remove (eq (and (listp it) (car it)) :comment))
                    (-partition 3)
                    (--map (apply #'example-to-should it)))))
          `(om--with-org-env
            (when ,contents (insert ,contents))
            (goto-char (point-min))
            ,@tests))))
    (let ((body
           (->> args
                (remove :begin-hidden)
                (remove :end-hidden)
                (-partition-before-pred
                 (lambda (it) (eq (and (listp it) (car it)) :buffer)))
                (-map #'make-test))))
      `(ert-deftest ,cmd () ,@body))))

(defun def-example-subgroup (&rest _)) ; ignore

(defun def-example-group (&rest _)) ; ignore

(provide 'om-dev-examples-to-tests)
;;; om-dev-examples-to-tests.el ends here
