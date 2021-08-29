;;; org-ml-cookbook.el --- Common patterns for org.el's  -*- lexical-binding: t -*-

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

;;; Code:

(require 's)
(require 'dash)
(require 'org-ml)

(defrecipe "Adding created time"
  "This will add a property called CREATED with a timestamp (which could be modified to hold the current time).."
  ("* headine")
  (let ((ts (org-ml-to-string (org-ml-build-timestamp! '(2020 1 1 0 0)))))
    (->> (org-ml-parse-this-headline)
         (org-ml-headline-set-node-property "CREATED" ts)
         (org-ml-to-string)))
  => (:result "* headline"
              ":PROPERTIES:"
              ":CREATED:  [2020-01-01 Wed 00:00]"
              ":END:"))

(provide 'org-ml-cookbook)
;;; org-ml-cookbook.el ends here
