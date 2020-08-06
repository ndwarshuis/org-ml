;;; org-ml-dev-doc.el --- Build documentation for org-ml

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

(require 'org-ml-dev-examples-to-docs)
(require 'org-ml-dev-examples)

(let ((public-syms (alist-get 'public org-ml-dev-defined-names))
      (example-syms (->> (-remove #'stringp org-ml-dev-examples-list)
                         (-map #'car))))
  (-some->> (-difference public-syms example-syms)
            (-map #'symbol-name)
            (--remove (s-ends-with? "*" it))
            (--remove (s-starts-with? "org-ml-update-this-" it))
            (--remove (s-starts-with? "org-ml-parse-this-" it))
            (--map (format "  %s" it))
            (s-join "\n")
            (format "The following functions don't have examples:\n%s")
            (print)))

;;; org-ml-dev-doc.el ends here
