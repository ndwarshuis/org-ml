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
        (org-adapt-indentation nil)
        (org-log-into-drawer t))
    (while (outline-next-heading)
      (org-todo 'done)))

  (let ((planning (->> (org-ml-unixtime-to-time-long (float-time))
                       (org-ml-build-planning! :closed))))
    (org-ml-do-headlines*
      (->> (org-ml-set-property :todo-keyword "DONE" it)
           (org-ml-headline-set-planning planning)))))

(provide 'org-ml-benchmarks)
;;; org-ml-benchmarks.el ends here
