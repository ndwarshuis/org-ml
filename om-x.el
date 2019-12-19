;;; om.el --- Org Mode Functional API -*- lexical-binding: t; -*-

;; Copyright (C) 2019  Nathan Dwarshuis

;; Author: Nathan Dwarshuis <natedwarshuis@gmail.com>
;; Keywords: org-mode, outlines
;; Homepage: https://github.com/ndwarshuis/org-x
;; Package-Requires: ((emacs "25") (dash "2.15"))
;; Version: 0.0.1

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

;; (require 'om-elem)
(require 'dash)
(require 'org)

;;; timestamp value extractors

;; TODO, maybe make this also take a point so that the current
;; buffer is implied
;; TODO this is basically org-with-point-at
(defmacro om--with-marker (marker &rest body)
  "Execute BODY after setting point and buffer to MARKER."
  ;; seems like there should be an easier way to do this...
  `(with-current-buffer (marker-buffer marker)
     (save-excursion
       (goto-char (marker-position marker))
       ,@body)))

(defun om--get-date-property (timestamp-property)
  "Get TIMESTAMP-PROPERTY on current heading and convert to a number.
If it does not have a date, it will return nil."
  (let ((ts (org-entry-get nil timestamp-property)))
        (when ts (round (org-2ft ts)))))

(defun om--this-timestamp (prop)
  "Get the timestamp under the current headline for PROP.
PROP is a string of the property to obtain, and is one of
\"TIMESTAMP\", \"TIMESTAMP_IA\", \"SCHEDULED\", \"DEADLINE\", or
\"CLOSED\"."
  (if (member prop '("TIMESTAMP" "TIMESTAMP_IA" "SCHEDULED" "DEADLINE"
                     "CLOSED"))
      (org-entry-get nil prop)
    (error "Invalid property: %s" prop)))

;; TODO make the reverse of this...
(defun om-timestamp-to-unixtime (timestamp)
  ;; TODO maybe make this freak if given nil?
  (-when-let (u (-some-> timestamp org-2ft round))
    ;; assume that org-2ft will spit out a 0 if given garbage
    (if (< 0 u) u (error "Invalid timestamp: %s" timestamp))))

(defun om--this-unixtime (prop)
  (-some-> prop om--this-timestamp om-timestamp-to-unixtime))

;; TODO are these all necessary?
(defun om-this-active-timestamp ()
  (om--this-timestamp "TIMESTAMP"))

(defun om-this-inactive-timestamp ()
  (om--this-timestamp "TIMESTAMP_IA"))

(defun om-this-scheduled-timestamp ()
  (om--this-timestamp "SCHEDULED"))

(defun om-this-deadline-timestamp ()
  (om--this-timestamp "DEADLINE"))

(defun om-this-closed-timestamp ()
  (om--this-timestamp "CLOSED"))

(defun om-this-active-unixtime ()
  (om--this-unixtime "TIMESTAMP"))

(defun om-this-inactive-unixtime ()
  (om--this-unixtime "TIMESTAMP_IA"))

(defun om-this-scheduled-unixtime ()
  (om--this-unixtime "SCHEDULED"))

(defun om-this-deadline-unixtime ()
  (om--this-unixtime "DEADLINE"))

(defun om-this-closed-unixtime ()
  (om--this-unixtime "CLOSED"))

(defun om-marker-active-timestamp (marker)
  (om--with-marker
   marker
   (om--this-timestamp "TIMESTAMP")))

(defun om-marker-inactive-timestamp (marker)
  (om--with-marker
   marker
   (om--this-timestamp "TIMESTAMP_IA")))

(defun om-marker-scheduled-timestamp (marker)
  (om--with-marker
   marker
   (om--this-timestamp "SCHEDULED")))

(defun om-marker-deadline-timestamp (marker)
  (om--with-marker
   marker
   (om--this-timestamp "DEADLINE")))

(defun om-marker-closed-timestamp (marker)
  (om--with-marker
   marker
   (om--this-timestamp "CLOSED")))

(defun om-marker-active-unixtime (marker)
  (om--with-marker
   marker
   (om--this-unixtime "TIMESTAMP")))

(defun om-marker-inactive-unixtime (marker)
  (om--with-marker
   marker
   (om--this-unixtime "TIMESTAMP_IA")))

(defun om-marker-scheduled-unixtime (marker)
  (om--with-marker
   marker
   (om--this-unixtime "SCHEDULED")))

(defun om-marker-deadline-unixtime (marker)
  (om--with-marker
   marker
   (om--this-unixtime "DEADLINE")))

(defun om-marker-closed-unixtime (marker)
  (om--with-marker
   marker
   (om--this-unixtime "CLOSED")))

;;; timestamp predicates

(defun om--compare-timestamps (timestamp-fun
                                     &optional ref-time future)
  "Returns the timestamp (from TIMESTAMP-FUN on the current heading) 
if timestamp is futher back in time compared to a REF-TIME (default to 
0 which is now, where negative is past and positive is future). If the 
FUTURE flag is t, returns timestamp if it is in the future compared 
to REF-TIME. Returns nil if no timestamp is found."
  (let* ((timestamp (funcall timestamp-fun))
        (ref-time (or ref-time 0)))
    (if (and timestamp
             (if future
                 (> (- timestamp (float-time)) ref-time)
               (<= (- timestamp (float-time)) ref-time)))
        timestamp)))

(defun om-this-is-timestamped-p ()
  "Get active timestamp of current heading."
  (om--get-date-property "TIMESTAMP"))

(defun om-this-is-scheduled-p ()
  "Get scheduled timestamp of current heading."
  (om--get-date-property "SCHEDULED"))

(defun om-this-is-deadlined-p ()
  "Get deadline timestamp of current heading."
  (om--get-date-property "DEADLINE"))

(defun om-this-is-closed-p ()
  "Get closed timestamp of current heading."
  (om--get-date-property "CLOSED"))

;; (defun om-is-stale-heading-p (&optional ts-prop)
;;   "Return timestamp for TS-PROP (TIMESTAMP by default) if current heading is stale."
;;   (om--compare-timestamps
;;    (lambda () (let ((ts (org-entry-get nil (or ts-prop "TIMESTAMP"))))
;;            (when (and ts (not (cl-find ?+ ts))) (org-2ft ts))))))

;; (defun om-is-fresh-heading-p ()
;;   "Return timestamp if current heading is fresh."
;;   (om--compare-timestamps 'om-is-timestamped-heading-p nil t))


;; task-level testing

(defun om-is-todoitem-p ()
  "Return todo keyword if heading has one."
  (let ((keyword (nth 2 (org-heading-components))))
    (if (member keyword org-todo-keywords-1)
        keyword)))

(defun om-is-project-p ()
  "Return todo keyword if heading has todoitem children."
  (and (om-headline-has-children 'om-is-todoitem-p) (om-is-todoitem-p)))

(defun om-is-task-p ()
  "Return todo keyword if heading has no todoitem children."
  (and (not (om-headline-has-children 'om-is-todoitem-p)) (om-is-todoitem-p)))

(defun om-is-project-task-p ()
  "Return todo keyword if heading has todoitem parents."
  (and (om-headline-has-parent 'om-is-todoitem-p) (om-is-task-p)))

(defun om-is-atomic-task-p ()
  "Return todo keyword if heading has no todoitem parents or children."
  (and (not (om-headline-has-parent 'om-is-todoitem-p)) (om-is-task-p)))

;; (defun om-task-status ()
;;   "Return the status of the headline under point."
;;   (-when-let (kw (om-is-task-p))
;;     (cond 
;;      ((om-is-archivable-heading-p)
;;       :archivable)
;;      ((om-is-inert-p)
;;       :inert)
;;      ((and (member kw org-done-keywords) (not (om-is-closed-heading-p)))
;;       :done-unclosed)
;;      ((and (not (member kw org-done-keywords)) (om-is-closed-heading-p))
;;       :undone-closed)
;;      ((member kw org-done-keywords)
;;       :complete)
;;      (t :active))))

;; property testing

;; (defun om-is-periodical-heading-p ()
;;   "Return t if heading is a periodical."
;;   (equal "periodical" (org-entry-get nil "PARENT_TYPE" t)))

;; (defun om-is-iterator-heading-p ()
;;   "Return t if heading is an iterator."
;;   (equal "iterator" (org-entry-get nil "PARENT_TYPE" t)))

(defun om-is-habit-heading-p ()
  "Return t if heading is an iterator."
  (equal "habit" (org-entry-get nil "STYLE" t)))

(defun om-headline-has-effort-p ()
  "Return t if heading has an effort."
  (org-entry-get nil "Effort"))

;; (defun om-headline-has-context-p ()
;;   "Return t if heading has a context."
;;   (let ((tags (org-get-tags-at)))
;;     (or (> (length (om-filter-list-prefix "#" tags)) 0)
;;         (> (length (om-filter-list-prefix "@" tags)) 0))))

(defun om-headline-has-tag-p (tag)
  "Return t if heading has tag TAG."
  (member tag (org-get-tags-at)))

;; relational testing

(defun om-headline-has-children (heading-test)
  "Return t if heading has a child for whom HEADING-TEST is t."
  (let ((subtree-end (save-excursion (org-end-of-subtree t)))
        has-children previous-point)
    (save-excursion
      (setq previous-point (point))
      (outline-next-heading)
      (while (and (not has-children)
                  (< previous-point (point) subtree-end))
        (when (funcall heading-test)
          (setq has-children t))
        (setq previous-point (point))
        (org-forward-heading-same-level 1 t)))
    has-children))

(defun om-headline-has-parent (heading-test)
  "Return t if heading has parent for whom HEADING-TEST is t."
  (save-excursion (and (org-up-heading-safe) (funcall heading-test))))

(defun om-has-discontinuous-parent ()
  "Return t if heading has a non-todoitem parent which in turn has a todoitem parent."
  (let ((has-todoitem-parent)
        (has-non-todoitem-parent))
    (save-excursion
      (while (and (org-up-heading-safe)
                  (not has-todoitem-parent))
        (if (om-is-todoitem-p)
            (setq has-todoitem-parent t)
          (setq has-non-todoitem-parent t))))
    (and has-todoitem-parent has-non-todoitem-parent)))

;; skip functions

(defun om-skip-heading ()
  "Skip forward to next heading."
  (save-excursion (or (outline-next-heading) (point-max))))

(defun om-skip-subtree ()
  "Skip forward to next subtree."
  (save-excursion (or (org-end-of-subtree t) (point-max))))

(defun om-skip-children ()
  "Skip to the end of all subheadings on the current subheading level.
This implies that the current heading has a parent. If it doesn't, this
function will simply return the point of the next headline."
  (save-excursion
    (if (org-up-heading-safe)
        (om-skip-subtree)
      (om-skip-heading))))

(defun om-skip-headings-with-tags (pos-tags-list &optional neg-tags-list)
  "Skip headings that have tags in POS-TAGS-LIST and not in NEG-TAGS-LIST."
  (save-restriction
    (widen)
    (let ((heading-tags (org-get-tags-at)))
      (if (and (or (not pos-tags-list)
                   (cl-intersection pos-tags-list heading-tags :test 'equal))
               (not (cl-intersection neg-tags-list heading-tags :test 'equal)))
          (om-skip-heading)))))

;;; MISC FUNCTIONS

(defun om-now ()
  "Return list representing the current time without hours and minutes.
This is meant to be used as input for functions such as
`om-build-timestamp'."
  (->> (decode-time) (-select-by-indices '(3 4 5)) (reverse)))

(defun om-now-long ()
  "Return list representing the current time with hours and minutes.
This is meant to be used as input for functions such as
`om-build-timestamp'."
  (->> (decode-time) (-select-by-indices '(1 2 3 4 5)) (reverse)))


(provide 'om)
;;; om.el ends here
