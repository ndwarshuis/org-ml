;;; org-ml-benchmarks.el --- Benchmarks for org-ml -*- lexical-binding: t; -*-

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

;;; Code:

(require 'org-ml-bench-fw)

(org-ml-defbench "read TODO" "* TODO headline\n" 10000
  (let ((next t))
    (while next
      (org-get-todo-state)
      (setq next (outline-next-heading))))
  
  (->> (org-ml-get-headlines)
       (--map (org-ml-get-property :todo-keyword it))))

(org-ml-defbench "TODO -> DONE" "* TODO headline\n" 1000
  (let ((org-log-done 'time)
        (org-todo-keywords '((sequence "TODO" "|" "DONE")))
        (org-adapt-indentation nil)
        (next t))
    (while next
      (org-todo 'done)
      (setq next (outline-next-heading))))

  (let ((planning (->> (org-ml-unixtime-to-time-long (float-time))
                       (org-ml-build-planning! :closed))))
    (org-ml-do-headlines*
      (->> (org-ml-set-property :todo-keyword "DONE" it)
           (org-ml-headline-set-planning planning)))))

(org-ml-defbench "tag headline" "* headline\n" 1000
  (let ((next t))
    (while next
      (org-set-tags '("A" "B" "C"))
      (setq next (outline-next-heading))))

  (org-ml-do-headlines*
    (org-ml-set-property :tags '("A" "B" "C") it)))

(org-ml-defbench "schedule headline" "* headline\n" 1000
  (let ((org-adapt-indentation nil)
        (next t))
    (while next
      (org-schedule nil "2000-01-01")
      (setq next (outline-next-heading))))

  (let ((pl (org-ml-build-planning! :scheduled '(2000 1 1))))
    (org-ml-do-headlines*
      (org-ml-headline-set-planning pl it))))

(org-ml-defbench "reschedule headline" "* headline\nSCHEDULED: <2020-01-01 Wed>\n" 1000
  (let ((org-adapt-indentation nil)
        (next t))
    (while next
      (->> (org-get-scheduled-time (point))
           (float-time)
           ;; shift up one day
           (+ (* 24 60 60))
           (format-time-string "%Y-%m-%d")
           (org-schedule nil))
      (setq next (outline-next-heading))))

  (org-ml-do-headlines*
    (org-ml-headline-map-planning*
      (org-ml-map-property* :scheduled (org-ml-timestamp-shift 1 'day it) it) it)))

(org-ml-defbench "set headline effort" "* headline\n" 1000
  (let ((org-adapt-indentation nil)
        (next t))
    (while next
      (org-set-property "Effort" "0:05")
      (setq next (outline-next-heading))))

  (org-ml-do-headlines*
    (org-ml-headline-set-node-property "Effort" "0:05" it)))

(org-ml-defbench "set checkboxes" "* headline [0/0]\n- [ ] one\n- [ ] two\n" 1000
  (let ((org-adapt-indentation nil)
        (next t))
    (while next
      (org-toggle-checkbox)
      (setq next (outline-next-heading))))

  (org-ml-do-headlines*
    (->> (org-ml-match-map '(section plain-list item) #'org-ml-item-toggle-checkbox it)
         (org-ml-headline-update-item-statistics))))

(org-ml-defbench "insert headline text" "* headline\n" 2500
  (let ((org-adapt-indentation nil)
        (next t))
    (while next
      (save-excursion
        (org-end-of-subtree)
        (insert "\nsome text"))
      (setq next (outline-next-heading))))

  (let ((para (org-ml-build-paragraph! "some text")))
    (org-ml-do-headlines*
      (org-ml-headline-set-section (list para) it))))

(org-ml-defbench "set headline effort/TODO/scheduled" "* headline\n" 1000
  (let ((org-log-done 'time)
        (org-todo-keywords '((sequence "TODO" "|" "DONE")))
        (org-adapt-indentation nil)
        (next t))
    (while next
      (org-schedule nil "2000-01-01")
      (org-set-property "Effort" "0:05")
      (org-todo 'todo)
      (setq next (outline-next-heading))))

  (let ((pl (org-ml-build-planning! :scheduled '(2000 1 1))))
    (org-ml-do-headlines*
      (->> (org-ml-set-property :todo-keyword "TODO" it)
           (org-ml-headline-set-node-property "Effort" "0:05")
           (org-ml-headline-set-planning pl)))))

(provide 'org-ml-benchmarks)
;;; org-ml-benchmarks.el ends here
