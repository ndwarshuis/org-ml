;;; org-ml-bench-fw.el --- Benchmark framework for org-ml -*- lexical-binding: t; -*-

;; Copyright (C) 2020 Free Software Foundation, Inc.

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

;; This framework provides a macro `org-ml-defbench' which is used to define
;; the default benchmarks as part of this library, and can also be used by end
;; users to define their own benchmarks.

;;; Code:

(require 's)
(require 'dash)
(require 'org-ml)

(defvar org-ml-benchmarks '()
  "All defined benchmarks in the org-ml benchmark suite.")

(defun org-ml-bench-get-sys-info ()
  (let ((cpumodel (if (eq system-type 'gnu/linux)
                    (->> (shell-command-to-string "lscpu | grep 'Model name:'")
                         (s-chop-prefix "Model name:")
                         (s-trim))
                    "unknown"))
        (memtotal (if (eq system-type 'gnu/linux)
                    (->> (shell-command-to-string "grep MemTotal /proc/meminfo")
                         (s-chop-prefix "MemTotal:")
                         (s-trim))
                    "unknown"))
        (org-ver (org-version))
        (emacs-ver (s-replace "\n" "" (emacs-version))))
    (s-join "\n"
            (list
             (format "CPU:           %s" cpumodel)
             (format "Total Memory:  %s" memtotal)
             (format "Org Version:   %s" org-ver)
             (format "Emacs Version: %s" emacs-ver)))))

(defmacro org-ml-bench-time-call (&rest body)
  `(let ((start-time (float-time)))
     ,@body
     (- (float-time) start-time)))

(defmacro org-ml-bench-with-org-file (repeated-pattern n &rest body)
  (declare (indent 2))
  `(let ((inhibit-message t))
     (with-temp-buffer
       (org-mode)
       (insert (s-repeat ,n ,repeated-pattern))
       (goto-char (point-min))
       (garbage-collect)
       (let ((time (org-ml-bench-time-call ,@body))
             (res (buffer-string)))
         (list res time)))))

(defun org-ml-bench-compare (repeated-pattern n form1 form2)
  (declare (indent 2))
  `(-let (((res1 time1) (org-ml-bench-with-org-file ,repeated-pattern ,n ,form1))
          ((res2 time2) (org-ml-bench-with-org-file ,repeated-pattern ,n ,form2)))
     (unless (equal res1 res2)
       (print "WARNING: forms produced different buffer strings")
       (print (cadr (s-lines res1)))
       (print (cadr (s-lines res2))))
     (list time1 time2)))

(defun org-ml-bench-format-result-row (title n time1 time2)
  (format "| %-40s | %6s | %10.5f | %10.5f | %10.2f |" title n time1 time2
          (/ time2 time1)))

(defmacro org-ml-defbench (title n pattern form1 form2)
  "Define a benchmark.

TITLE is a short string that will be used to identify the
benchmark (uniqueness isn't enforced but makes sense). FORM1 and
FORM2 are the two forms to be compared; by convention the first
is a function composed of built-in org commands and the second is
one composed of org-ml commands. FORM1 and FORM2 will be applied
to a buffer with PATTERN repeated N times. Note the both forms
will only be called once and thus must contain the code for
iterating across PATTERN as desired.

Calling `org-ml-bench-run' will execute all benchmarks in the
order they are defined with this macro."
  (declare (indent 2))
  (let ((p (format "%s\n" (if (listp pattern)
                              (s-join "\n" (eval pattern))
                            pattern))))
    `(add-to-list 'org-ml-benchmarks
                  (lambda ()
                    (print (format "Starting benchmark: %s" ,title))
                    (-let (((time1 time2)
                            ,(org-ml-bench-compare p n form1 form2)))
                      (org-ml-bench-format-result-row ,title ,n time1 time2))))))

(defun org-ml-bench-run ()
  "Run and print all defined benchmarks."
  (let ((test-rows (--map (funcall it) (reverse org-ml-benchmarks))))
    (print
     (s-join "\n"
             (append
              (list
               ""
               (org-ml-bench-get-sys-info)
               ""
               (format "| %-40s | %6s | %10s | %10s | %10s |"
                       "Test name" "N" "Native" "org-ml" "X Increase"))
              test-rows
              (list ""))))))

(provide 'org-ml-bench-fw)
;;; org-ml-bench-fw.el ends here
