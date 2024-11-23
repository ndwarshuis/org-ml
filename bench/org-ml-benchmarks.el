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

(org-ml-defbench "read TODO" 10000
  "* TODO headline"
  (let ((next t))
    (while next
      (org-get-todo-state)
      (setq next (outline-next-heading))))
  
  (->> (org-ml-parse-headlines 'all)
       (--map (org-ml-get-property :todo-keyword it))))

(org-ml-defbench "read SCHEDULED epoch time" 2500
  (list "* TODO headline"
        "SCHEDULED: <2020-01-01 Tue>")
  (let ((next t))
    (while next
      (org-2ft (org-entry-get (point) "SCHEDULED"))
      (setq next (outline-next-heading))))
  
  (--each (org-ml-parse-headlines 'all)
    (->> (plist-get (org-ml-headline-get-planning it) :scheduled)
         (org-ml-timelist-to-unixtime))))

(org-ml-defbench "TODO -> DONE" 1000
  "* TODO headline"
  (let ((org-log-done 'time)
        (org-todo-keywords '((sequence "TODO" "|" "DONE")))
        (org-adapt-indentation nil)
        (next t))
    (while next
      (org-todo 'done)
      (setq next (outline-next-heading))))

  (let ((planning `(:closed ,(org-ml-unixtime-to-timelist t (float-time)))))
    (org-ml-wrap-impure
     (org-ml-update-headlines* 'all
       (->> (org-ml-set-property :todo-keyword "DONE" it)
            (org-ml-headline-set-planning planning))))))

(org-ml-defbench "demote headlines" 2500
  "* headline"
  (let ((org-adapt-indentation nil)
        (next t))
    (while next
      (org-do-demote)
      (setq next (outline-next-heading))))

  (org-ml-wrap-impure
   (org-ml-update-headlines* 'all
     (org-ml-shift-property :level 1 it))))

(org-ml-defbench "demote subtrees" 2500
  (list "* headline"
        "** subheadline")
  (let ((org-adapt-indentation nil)
        (next t))
    (while next
      (org-demote)
      (setq next (outline-next-heading))))

  (org-ml-wrap-impure
   (org-ml-update-subtrees* 'all
     (org-ml--headline-subtree-shift-level 1 it))))

(org-ml-defbench "tag headline" 1000
  "* headline"
  (let ((next t))
    (while next
      (org-set-tags '("A" "B" "C"))
      (setq next (outline-next-heading))))

  (org-ml-wrap-impure
   (org-ml-update-headlines* 'all
     (org-ml-set-property :tags '("A" "B" "C") it))))

;; TODO a better test for this would be to put a logbook underneath the
;; planning ts since in that case we need to also parse the logbook
(org-ml-defbench "schedule headline" 1000
  (list "* headline"
        ":LOGGING:"
        "- Note taken on [2024-08-07 Wed 20:07] \\"
        "thingy"
        ":END:")
  (let ((org-adapt-indentation nil)
        (next t))
    (while next
      (org-schedule nil "2000-01-01")
      (setq next (outline-next-heading))))

  (let ((pl '(:scheduled (2000 1 1))))
    (org-ml-wrap-impure
     (org-ml-update-supercontents* 'all
       (org-ml-supercontents-set-planning pl it)))))

(org-ml-defbench "schedule headline (memoized)" 1000
  (list "* headline"
        ":LOGGING:"
        "- Note taken on [2024-08-07 Wed 20:07] \\"
        "thingy"
        ":END:")
  (let ((org-adapt-indentation nil)
        (next t))
    (while next
      (org-schedule nil "2000-01-01")
      (setq next (outline-next-heading))))

  (let ((pl '(:scheduled (2000 1 1))))
    (let ((org-ml-memoize-shorthand-builders t))
      (org-ml-wrap-impure
       (org-ml-update-supercontents* 'all
         (org-ml-supercontents-set-planning pl it))))))

(org-ml-defbench "reschedule headline" 1000
  (list "* headline"
        "SCHEDULED: <2020-01-01 Wed>"
        ":LOGGING:"
        "- Note taken on [2024-08-07 Wed 20:07] \\"
        "thingy"
        ":END:")
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

  (org-ml-wrap-impure
   (org-ml-update-supercontents* 'all
     (org-ml-supercontents-set-planning
      (list :scheduled
            (org-ml-timelist-shift
             1 'day
             (plist-get (org-ml-supercontents-get-planning it) :scheduled)))
      it))))

(org-ml-defbench "reschedule headline (memoized)" 1000
  (list "* headline"
        "SCHEDULED: <2020-01-01 Wed>"
        ":LOGGING:"
        "- Note taken on [2024-08-07 Wed 20:07] \\"
        "thingy"
        ":END:")
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

  (org-ml-wrap-impure
   (let ((org-ml-memoize-shorthand-builders t))
     (org-ml-update-supercontents* 'all
       (org-ml-supercontents-set-planning
        (list :scheduled
              (org-ml-timelist-shift
               1 'day
               (plist-get (org-ml-supercontents-get-planning it) :scheduled)))
        it)))))

(org-ml-defbench "set headline effort" 1000
  (list "* headline"
        ":LOGGING:"
        "- Note taken on [2024-08-07 Wed 20:07] \\"
        "thingy"
        ":END:")
  (let ((org-adapt-indentation nil)
        (next t))
    (while next
      (org-set-property "Effort" "0:05")
      (setq next (outline-next-heading))))

  (org-ml-wrap-impure
   (org-ml-update-supercontents* 'all
     (org-ml-supercontents-set-node-properties '(("Effort" "0:05")) it))))

(org-ml-defbench "set headline effort (memoized)" 1000
  (list "* headline"
        ":LOGGING:"
        "- Note taken on [2024-08-07 Wed 20:07] \\"
        "thingy"
        ":END:")
  (let ((org-adapt-indentation nil)
        (next t))
    (while next
      (org-set-property "Effort" "0:05")
      (setq next (outline-next-heading))))

  (org-ml-wrap-impure
   (let ((org-ml-memoize-shorthand-builders t))
     (org-ml-update-supercontents* 'all
       (org-ml-supercontents-set-node-properties '(("Effort" "0:05")) it)))))

(org-ml-defbench "insert headline text" 2500
  "* headline"
  (let ((org-adapt-indentation nil)
        (next t))
    (while next
      (save-excursion
        (org-end-of-subtree)
        (insert "\n~some text~"))
      (setq next (outline-next-heading))))

  (org-ml-wrap-impure
   (org-ml-update-supercontents* 'all
     (org-ml-supercontents-set-contents
      nil (list (org-ml-build-paragraph! "~some text~")) it))))

(org-ml-defbench "insert headline text (memoized)" 2500
  "* headline"
  (let ((org-adapt-indentation nil)
        (next t))
    (while next
      (save-excursion
        (org-end-of-subtree)
        (insert "\n~some text~"))
      (setq next (outline-next-heading))))

  (org-ml-wrap-impure
   (let ((org-ml-memoize-shorthand-builders t))
     (org-ml-update-supercontents* 'all
       (org-ml-supercontents-set-contents
        nil (list (org-ml-build-paragraph! "~some text~")) it)))))

(org-ml-defbench "set checkboxes" 1000
  (list "* headline [0/0]"
        "- [ ] one"
        "- [ ] two")
  (let ((org-adapt-indentation nil)
        (next t))
    (while next
      (org-toggle-checkbox)
      (setq next (outline-next-heading))))

  (let ((org-ml-memoize-match-patterns 'compiled))
    (org-ml-update-headlines* 'all
      (org-ml->> (org-ml-match-map '(section plain-list item) #'org-ml-item-toggle-checkbox it)
        (org-ml-headline-update-item-statistics)))))

(org-ml-defbench "set headline effort/TODO/scheduled" 1000
  "* headline"
  (let ((org-log-done 'time)
        (org-todo-keywords '((sequence "TODO" "|" "DONE")))
        (org-adapt-indentation nil)
        (next t))
    (while next
      (org-schedule nil "2000-01-01")
      (org-set-property "Effort" "0:05")
      (org-todo 'todo)
      (setq next (outline-next-heading))))

  (let ((pl '(:scheduled (2000 1 1))))
    (let ((org-ml-memoize-shorthand-builders t))
      (org-ml-update-headlines* 'all
        (org-ml->> (org-ml-set-property :todo-keyword "TODO" it)
          (org-ml-headline-set-node-property "Effort" "0:05")
          (org-ml-headline-set-planning pl))))))

(provide 'org-ml-benchmarks)
;;; org-ml-benchmarks.el ends here
