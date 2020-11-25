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

(org-ml-defbench "read TODO" "\n* TODO headline" 10000
  (while (outline-next-heading)
    (org-get-todo-state))
  
  (->> (org-ml-parse-this-buffer)
       (--drop-while (not (org-ml-is-type 'headline it)))
       (--map (org-ml-get-property :todo-keyword it))))

(org-ml-defbench "TODO -> DONE" "\n* TODO headline" 1000
  (let ((org-log-done 'time)
        (org-todo-keywords '((sequence "TODO" "|" "DONE")))
        (org-adapt-indentation nil))
    (while (outline-next-heading)
      (org-todo 'done)))

  (let ((planning (->> (org-ml-unixtime-to-time-long (float-time))
                       (org-ml-build-planning! :closed))))
    (org-ml-do-headlines*
      (->> (org-ml-set-property :todo-keyword "DONE" it)
           (org-ml-headline-set-planning planning)))))

(org-ml-defbench "tag headline" "* headline\n" 1000
  (progn
    (org-set-tags '("A" "B" "C"))
    (while (outline-next-heading)
      (org-set-tags '("A" "B" "C"))))

  (org-ml-do-headlines*
    (org-ml-set-property :tags '("A" "B" "C") it)))

(org-ml-defbench "schedule headline" "* headline\n" 1000
  (let ((org-adapt-indentation nil))
    (org-schedule nil "2000-01-01")
    (while (outline-next-heading)
      (org-schedule nil "2000-01-01")))

  (let ((pl (org-ml-build-planning! :scheduled '(2000 1 1))))
    (org-ml-do-headlines*
      (org-ml-headline-set-planning pl it))))

(org-ml-defbench "set headline effort" "* headline\n" 1000
  (let ((org-adapt-indentation nil))
    (org-set-property "Effort" "0:05")
    (while (outline-next-heading)
      (org-set-property "Effort" "0:05")))

  (org-ml-do-headlines*
    (org-ml-headline-set-node-property "Effort" "0:05" it)))

(org-ml-defbench "set checkboxes" "* headline [0/0]\n- [ ] one\n- [ ] two\n" 1000
  (let ((org-adapt-indentation nil))
    (org-toggle-checkbox)
    (while (outline-next-heading)
      (org-toggle-checkbox)))

  (org-ml-do-headlines*
    (->> (org-ml-match-map '(section plain-list item) #'org-ml-item-toggle-checkbox it)
         (org-ml-headline-update-item-statistics))))

(org-ml-defbench "insert headline text" "* headline\n" 2500
  (let ((org-adapt-indentation nil))
    (save-excursion
      (org-end-of-subtree)
      (insert "\nsome text"))
    (while (outline-next-heading)
      (save-excursion
        (org-end-of-subtree)
        (insert "\nsome text"))))

  (let ((para (org-ml-build-paragraph! "some text")))
    (org-ml-do-headlines*
      (org-ml-headline-set-section (list para) it))))

(org-ml-defbench "headline effort/TODO/scheduled" "\n* headline" 1000
  (let ((org-log-done 'time)
        (org-todo-keywords '((sequence "TODO" "|" "DONE")))
        (org-adapt-indentation nil))
    (while (outline-next-heading)
      (org-schedule nil "2000-01-01")
      (org-set-property "Effort" "0:05")
      (org-todo 'todo)))

  (let ((pl (org-ml-build-planning! :scheduled '(2000 1 1))))
    (org-ml-do-headlines*
      (->> (org-ml-set-property :todo-keyword "TODO" it)
           (org-ml-headline-set-node-property "Effort" "0:05")
           (org-ml-headline-set-planning pl)))))

(provide 'org-ml-benchmarks)
;;; org-ml-benchmarks.el ends here
