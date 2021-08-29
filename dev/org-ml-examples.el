;;; org-ml-examples.el --- Examples for org.el's API  -*- lexical-binding: t -*-

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

(def-example-group "String Conversion"
  "Convert nodes to strings."

  ;; these are more thoroughly tested in `org-ml-test-internal.el'

  (defexamples org-ml-to-string
    (org-ml-to-string
     '(bold
       (:begin 1 :end 5 :parent nil :post-blank 0 :post-affiliated nil)
       "text"))
    => "*text*"
    (org-ml-to-string
     '(bold
       (:begin 1 :end 5 :parent nil :post-blank 3 :post-affiliated nil)
       "text"))
    => "*text*   "
    (org-ml-to-string nil) => "")

  (defexamples org-ml-to-trimmed-string
    (org-ml-to-trimmed-string
     '(bold
       (:begin 1 :end 5 :parent nil :post-blank 0 :post-affiliated nil)
       "text"))
    => "*text*"
    (org-ml-to-trimmed-string
     '(bold
       (:begin 1 :end 5 :parent nil :post-blank 3 :post-affiliated nil)
       "text"))
    => "*text*"
    (org-ml-to-trimmed-string nil) => "")

  (defexamples org-ml-from-string
    (org-ml-from-string 'bold "*text*")
    => '(bold
         (:begin 1 :end 7 :contents-begin 2 :contents-end 6 :post-blank 0 :parent nil)
         "text")

    (org-ml-from-string 'italic "*text*")
    => nil))

(def-example-group "Buffer Parsing"
  "Parse buffers to trees."

  ;; these are more thoroughly tested in `org-ml-dev-test.el'

  (defexamples-content org-ml-parse-object-at
    nil
    (:buffer "*text*")
    (->> (org-ml-parse-object-at 1)
         (car))
    => 'bold

    (:buffer "[2019-01-01 Tue]")
    (->> (org-ml-parse-object-at 1)
         (car))
    => 'timestamp

    (:buffer "- notme")
    (:comment "Return nil when parsing an element")
    (org-ml-parse-object-at 1)
    => nil)

  (defexamples-content org-ml-parse-element-at
    nil
    (:buffer "#+call: ktulu()")
    (->> (org-ml-parse-element-at 1)
         (car))
    => 'babel-call
    
    (:buffer "- plain-list")
    (:comment "Give the plain-list, not the item for this function")
    (->> (org-ml-parse-element-at 1)
         (car))
    => 'plain-list
    
    (:buffer "| R | A |"
             "| G | E |")
    (:comment "Return a table, not the table-row for this function")
    (->> (org-ml-parse-element-at 1)
         (car))
    => 'table)

  (defexamples-content org-ml-parse-table-row-at
    nil
    (:buffer "| bow | stroke |"
             "|-----+--------|"
             "| wob | ekorts |")
    (:comment "Return the row itself")
    (->> (org-ml-parse-table-row-at 1)
         (car))
    => 'table-row
    (->> (org-ml-parse-table-row-at 20)
         (car))
    => 'table-row
    (->> (org-ml-parse-table-row-at 40)
         (car))
    => 'table-row
    (:comment "Also return the row when not at beginning of line")
    (->> (org-ml-parse-table-row-at 5)
         (car))
    => 'table-row
    (:buffer "- bow and arrow choke")
    (:comment "Return nil if not a table-row")
    (->> (org-ml-parse-table-row-at 1)
         (car))
    => nil)

  (defexamples-content org-ml-parse-headline-at
    nil
    (:buffer "* headline")
    (:comment "Return the headline itself")
    (->> (org-ml-parse-headline-at 1)
         (org-ml-to-trimmed-string))
    => "* headline"
    (:buffer "* headline"
             "section crap")
    (:comment "Return headline and section")
    (->> (org-ml-parse-headline-at 1)
         (org-ml-to-trimmed-string))
    => (:result "* headline"
                "section crap")
    (:comment "Return headline when point is in the section")
    (->> (org-ml-parse-headline-at 12)
         (org-ml-to-trimmed-string))
    => (:result "* headline"
                "section crap")
    (:buffer "* headline"
             "section crap"
             "** not parsed")
    (:comment "Don't parse any subheadlines")
    (->> (org-ml-parse-headline-at 1)
         (org-ml-to-trimmed-string))
    => (:result "* headline"
                "section crap")
    (:buffer "nothing nowhere")
    (:comment "Return nil if not under a headline")
    (->> (org-ml-parse-headline-at 1)
         (org-ml-to-trimmed-string))
    => "")

  (defexamples-content org-ml-parse-subtree-at
    nil
    (:buffer "* headline")
    (:comment "Return the headline itself")
    (->> (org-ml-parse-subtree-at 1)
         (org-ml-to-trimmed-string))
    => "* headline"
    (:buffer "* headline"
             "section crap")
    (:comment "Return headline and section")
    (->> (org-ml-parse-subtree-at 1)
         (org-ml-to-trimmed-string))
    => (:result "* headline"
                "section crap")
    (:comment "Return headline when point is in the section")
    (->> (org-ml-parse-subtree-at 12)
         (org-ml-to-trimmed-string))
    => (:result "* headline"
                "section crap")
    (:buffer "* headline"
             "section crap"
             "** parsed")
    (:comment "Return all the subheadlines")
    (->> (org-ml-parse-subtree-at 1)
         (org-ml-to-trimmed-string))
    => (:result "* headline"
                "section crap"
                "** parsed")
    (:buffer "nothing nowhere")
    (:comment "Return nil if not under a headline")
    (->> (org-ml-parse-subtree-at 1)
         (org-ml-to-trimmed-string))
    => "")

  (defexamples-content org-ml-parse-item-at
    nil
    (:buffer "- item")
    (:comment "Return the item itself")
    (->> (org-ml-parse-item-at 1)
         (org-ml-to-trimmed-string))
    => "- item"
    (:comment "Also return the item when not at beginning of line")
    (->> (org-ml-parse-item-at 5)
         (org-ml-to-trimmed-string))
    => "- item"
    (:buffer "- item"
             "  - item 2")
    (:comment "Return item and its subitems")
    (->> (org-ml-parse-item-at 1)
         (org-ml-to-trimmed-string))
    => (:result "- item"
                "  - item 2")
    (:buffer "* not item")
    (:comment "Return nil if not an item")
    (->> (org-ml-parse-item-at 1)
         (org-ml-to-trimmed-string))
    => "")
  
  (defexamples-content org-ml-parse-section-at
    nil
    (:buffer "over headline"
             "* headline"
             "under headline")
    (:comment "Return the section above the headline")
    (->> (org-ml-parse-section-at 1)
         (org-ml-to-trimmed-string))
    => "over headline"
    (:comment "Return the section under headline")
    (->> (org-ml-parse-section-at 25)
         (org-ml-to-trimmed-string))
    => "under headline"
    (:buffer "* headline"
             "** subheadline")
    (:comment "Return nil if no section under headline")
    (->> (org-ml-parse-section-at 1)
         (org-ml-to-trimmed-string))
    => ""
    (:buffer "")
    (:comment "Return nil if no section at all")
    (->> (org-ml-parse-section-at 1)
         (org-ml-to-trimmed-string))
    => "")

  (defexamples-content org-ml-parse-this-toplevel-section
    nil
    (:buffer "over headline"
             "* headline"
             "under headline")
    (->> (org-ml-parse-this-toplevel-section)
         (org-ml-to-trimmed-string))
    => "over headline"
    (:buffer "* headline"
             "under headline")
    (->> (org-ml-parse-this-toplevel-section)
         (org-ml-to-trimmed-string))
    => "")

  (defexamples-content org-ml-this-buffer-has-headlines
    nil
    (:buffer "not headline"
             "* headline")
    (org-ml-this-buffer-has-headlines)
    => t
    (:buffer "not headline")
    (org-ml-this-buffer-has-headlines)
    => nil)

  (defexamples-content org-ml-parse-headlines
    nil
    (:buffer "not headline"
             "* one"
             "* two"
             "* three")
    (->> (org-ml-parse-headlines 'all)
         (-map #'org-ml-to-string)
         (s-join ""))
    => (:result "* one"
                "* two"
                "* three"
                "")
    (:buffer "not headline")
    (->> (org-ml-parse-headlines 'all)
         (-map #'org-ml-to-string)
         (s-join ""))
    => ""
    (:buffer "not headline"
             "* one"
             "** two"
             "*** three")
    (->> (org-ml-parse-headlines 'all)
         (-map #'org-ml-to-trimmed-string))
    => '("* one\n** two\n*** three" "** two\n*** three" "*** three")

    (:buffer "not headline"
             "* one"
             "* two"
             "* three")
    (->> (org-ml-parse-headlines 0)
         (-map #'org-ml-to-string)
         (s-join ""))
    => "* one\n"
    (->> (org-ml-parse-headlines '(0 1))
         (-map #'org-ml-to-string)
         (s-join ""))
    => (:result "* one"
                "* two\n")
    (->> (org-ml-parse-headlines [10 25])
         (-map #'org-ml-to-string)
         (s-join ""))
    => (:result "* one"
                "* two\n"))

  (defexamples-content org-ml-parse-subtrees
    nil
    (:buffer "not headline"
             "* one"
             "** _one"
             "* two"
             "** _two"
             "* three"
             "** _three")
    (->> (org-ml-parse-subtrees 'all)
         (-map #'org-ml-to-string)
         (s-join ""))
    => (:result "* one"
                "** _one"
                "* two"
                "** _two"
                "* three"
                "** _three\n")
    (:buffer "not headline")
    (->> (org-ml-parse-subtrees 'all)
         (-map #'org-ml-to-string)
         (s-join ""))
    => ""

    (:buffer "not headline"
             "* one"
             "** _one"
             "* two"
             "** _two"
             "* three"
             "** _three")
    (->> (org-ml-parse-subtrees 0)
         (-map #'org-ml-to-string)
         (s-join ""))
    => (:result "* one"
                "** _one\n")
    (->> (org-ml-parse-subtrees '(0 1))
         (-map #'org-ml-to-string)
         (s-join ""))
    => (:result "* one"
                "** _one"
                "* two"
                "** _two\n")
    (->> (org-ml-parse-subtrees [10 30])
         (-map #'org-ml-to-string)
         (s-join ""))
    => (:result "* one"
                "** _one"
                "* two"
                "** _two\n")))

(def-example-group "Building"
  "Build new nodes."

  (def-example-subgroup "Leaf Object Nodes"
    nil

    (defexamples org-ml-build-code
      (->> (org-ml-build-code "text")
           (org-ml-to-string))
      => "~text~")

    (defexamples org-ml-build-entity
      (->> (org-ml-build-entity "gamma")
           (org-ml-to-string))
      => "\\gamma")

    (defexamples org-ml-build-export-snippet
      (->> (org-ml-build-export-snippet "back" "value")
           (org-ml-to-string))
      => "@@back:value@@")

    (defexamples org-ml-build-inline-babel-call
      (->> (org-ml-build-inline-babel-call "name")
           (org-ml-to-string))
      => "call_name()"
      (->> (org-ml-build-inline-babel-call "name" :arguments '("n=4"))
           (org-ml-to-string))
      => "call_name(n=4)"
      (->> (org-ml-build-inline-babel-call "name" :inside-header '(:key val))
           (org-ml-to-string))
      => "call_name[:key val]()"
      (->> (org-ml-build-inline-babel-call "name" :end-header '(:key val))
           (org-ml-to-string))
      => "call_name()[:key val]")

    (defexamples org-ml-build-inline-src-block
      (->> (org-ml-build-inline-src-block "lang")
           (org-ml-to-string))
      
      => "src_lang{}"
      (->> (org-ml-build-inline-src-block "lang" :value "value")
           (org-ml-to-string))
      
      => "src_lang{value}"
      (->> (org-ml-build-inline-src-block "lang" :value "value" :parameters '(:key val))
           (org-ml-to-string))
      => "src_lang[:key val]{value}")

    (defexamples org-ml-build-line-break
      (->> (org-ml-build-line-break)
           (org-ml-to-string))
      => "\\\\\n")

    (defexamples org-ml-build-latex-fragment
      (->> (org-ml-build-latex-fragment "$2+2=5$")
           (org-ml-to-string))
      => "$2+2=5$")

    (defexamples org-ml-build-macro
      (->> (org-ml-build-macro "economics")
           (org-ml-to-string))
      => "{{{economics}}}"
      (->> (org-ml-build-macro "economics" :args '("s=d"))
           (org-ml-to-string))
      => "{{{economics(s=d)}}}")

    (defexamples org-ml-build-statistics-cookie
      (->> (org-ml-build-statistics-cookie '(nil))
           (org-ml-to-string))
      => "[%]"
      (->> (org-ml-build-statistics-cookie '(nil nil))
           (org-ml-to-string))
      => "[/]"
      (->> (org-ml-build-statistics-cookie '(50))
           (org-ml-to-string))
      => "[50%]"
      (->> (org-ml-build-statistics-cookie '(1 3))
           (org-ml-to-string))
      => "[1/3]")

    (defexamples org-ml-build-target
      (->> (org-ml-build-target "text")
           (org-ml-to-string))
      => "<<text>>")

    (defexamples org-ml-build-timestamp
      (->> (org-ml-build-timestamp 'inactive 2019 1 15 2019 1 15)
           (org-ml-to-string))
      => "[2019-01-15 Tue]"
      (->> (org-ml-build-timestamp 'active-range 2019 1 15 2019 1 16)
           (org-ml-to-string))
      => "<2019-01-15 Tue>--<2019-01-16 Wed>"
      (->> (org-ml-build-timestamp
            'inactive 2019 1 15 2019 1 15 :warning-type 'all
            :warning-unit 'day :warning-value 1)
           (org-ml-to-string))
      => "[2019-01-15 Tue -1d]")

    (defexamples org-ml-build-verbatim
      (->> (org-ml-build-verbatim "text")
           (org-ml-to-string))
      => "=text="))

  (def-example-subgroup "Branch Object Nodes"
    nil

    (defexamples org-ml-build-bold
      (->> (org-ml-build-bold "text")
           (org-ml-to-string))
      => "*text*")

    (defexamples org-ml-build-footnote-reference
      (->> (org-ml-build-footnote-reference)
           (org-ml-to-string))
      => "[fn:]"
      (->> (org-ml-build-footnote-reference :label "label")
           (org-ml-to-string))
      => "[fn:label]"
      (->> (org-ml-build-footnote-reference :label "label" "content")
           (org-ml-to-string))
      => "[fn:label:content]")

    (defexamples org-ml-build-italic
      (->> (org-ml-build-italic "text")
           (org-ml-to-string))
      => "/text/")

    (defexamples org-ml-build-link
      (->> (org-ml-build-link "target")
           (org-ml-to-string))
      => "[[target]]"
      (->> (org-ml-build-link "target" :type "file")
           (org-ml-to-string))
      => "[[file:target]]"
      (->> (org-ml-build-link "target" "desc")
           (org-ml-to-string))
      => "[[target][desc]]")

    (defexamples org-ml-build-radio-target
      (->> (org-ml-build-radio-target "text")
           (org-ml-to-string))
      => "<<<text>>>")

    (defexamples org-ml-build-strike-through
      (->> (org-ml-build-strike-through "text")
           (org-ml-to-string))
      => "+text+")

    (defexamples org-ml-build-superscript
      (->> (org-ml-build-superscript "text")
           (org-ml-to-string))
      => "^text")

    (defexamples org-ml-build-subscript
      (->> (org-ml-build-subscript "text")
           (org-ml-to-string))
      => "_text")

    (defexamples org-ml-build-table-cell
      (->> (org-ml-build-table-cell "text")
           (org-ml-to-string))
      => " text |")

    (defexamples org-ml-build-underline
      (->> (org-ml-build-underline "text")
           (org-ml-to-string))
      => "_text_"))

  (def-example-subgroup "Leaf Element Nodes"
    nil

    (defexamples org-ml-build-babel-call
      (->> (org-ml-build-babel-call "name")
           (org-ml-to-trimmed-string))
      => "#+call: name()"
      (->> (org-ml-build-babel-call "name" :arguments '("arg=x"))
           (org-ml-to-trimmed-string))
      => "#+call: name(arg=x)"
      (->> (org-ml-build-babel-call "name" :inside-header '(:key val))
           (org-ml-to-trimmed-string))
      => "#+call: name[:key val]()"
      (->> (org-ml-build-babel-call "name" :end-header '(:key val))
           (org-ml-to-trimmed-string))
      => "#+call: name() :key val")

    (defexamples org-ml-build-clock
      (->> (org-ml-build-clock (org-ml-build-timestamp! '(2019 1 1 0 0)))
           (org-ml-to-trimmed-string))
      => "CLOCK: [2019-01-01 Tue 00:00]"
      (->> (org-ml-build-timestamp! '(2019 1 1 0 0) :end '(2019 1 1 1 0))
           ;; TODO this is sloppy but also kinda a bad example anyways since
           ;; the shortcut function exists
           (org-ml-set-property :type 'inactive-range)
           (org-ml-build-clock)
           (org-ml-to-trimmed-string))
      => "CLOCK: [2019-01-01 Tue 00:00]--[2019-01-01 Tue 01:00] =>  1:00")

    (defexamples org-ml-build-comment
      ;; TODO there is a bug that makes a blank string return a
      ;; blank string (it should return a "# ")
      (->> (org-ml-build-comment "text")
           (org-ml-to-trimmed-string))
      => "# text"
      (->> (org-ml-build-comment "text\nless")
           (org-ml-to-trimmed-string))
      => "# text\n# less")

    (defexamples org-ml-build-comment-block
      (->> (org-ml-build-comment-block)
           (org-ml-to-trimmed-string))
      => (:result "#+begin_comment"
                  "#+end_comment")
      (->> (org-ml-build-comment-block :value "text")
           (org-ml-to-trimmed-string))
      => (:result "#+begin_comment"
                  "text"
                  "#+end_comment"))

    (defexamples org-ml-build-diary-sexp
      (->> (org-ml-build-diary-sexp)
           (org-ml-to-trimmed-string))
      => "%%()"
      (->> (org-ml-build-diary-sexp :value '(text))
           (org-ml-to-trimmed-string))
      => "%%(text)")

    (defexamples org-ml-build-example-block
      (->> (org-ml-build-example-block)
           (org-ml-to-trimmed-string))
      => (:result "#+begin_example"
                  "#+end_example")
      (->> (org-ml-build-example-block :value "text")
           (org-ml-to-trimmed-string))
      => (:result "#+begin_example"
                  "  text"
                  "#+end_example")
      (->> (org-ml-build-example-block :value "text" :switches '("switches"))
           (org-ml-to-trimmed-string))
      => (:result "#+begin_example switches"
                  "  text"
                  "#+end_example"))

    (defexamples org-ml-build-export-block
      (->> (org-ml-build-export-block "type" "value\n")
           (org-ml-to-trimmed-string))
      => (:result "#+begin_export type"
                  "value"
                  "#+end_export"))

    (defexamples org-ml-build-fixed-width
      (->> (org-ml-build-fixed-width "text")
           (org-ml-to-trimmed-string))
      => ": text")

    (defexamples org-ml-build-horizontal-rule
      (->> (org-ml-build-horizontal-rule)
           (org-ml-to-trimmed-string))
      => "-----")

    (defexamples org-ml-build-keyword
      (->> (org-ml-build-keyword "FILETAGS" "tmsu")
           (org-ml-to-trimmed-string))
      => "#+filetags: tmsu")

    (defexamples org-ml-build-latex-environment
      (->> (org-ml-build-latex-environment '("env" "text"))
           (org-ml-to-trimmed-string))
      => (:result "\\begin{env}"
                  "text"
                  "\\end{env}"))

    (defexamples org-ml-build-node-property
      (->> (org-ml-build-node-property "key" "val")
           (org-ml-to-trimmed-string))
      => ":key:      val")

    (defexamples org-ml-build-planning
      (->> (org-ml-build-planning :closed (org-ml-build-timestamp! '(2019 1 1) :active nil))
           (org-ml-to-trimmed-string))
      => "CLOSED: [2019-01-01 Tue]"
      (->> (org-ml-build-planning :scheduled (org-ml-build-timestamp! '(2019 1 1) :active t))
           (org-ml-to-trimmed-string))
      => "SCHEDULED: <2019-01-01 Tue>"
      (->> (org-ml-build-planning :deadline (org-ml-build-timestamp! '(2019 1 1) :active t))
           (org-ml-to-trimmed-string))
      => "DEADLINE: <2019-01-01 Tue>")

    (defexamples org-ml-build-src-block
      (->> (org-ml-build-src-block)
           (org-ml-to-trimmed-string))
      => (:result "#+begin_src"
                  "#+end_src")
      (->> (org-ml-build-src-block :value "body")
           (org-ml-to-trimmed-string))
      => (:result "#+begin_src"
                  "  body"
                  "#+end_src")
      (->> (org-ml-build-src-block :value "body" :language "emacs-lisp")
           (org-ml-to-trimmed-string))
      => (:result "#+begin_src emacs-lisp"
                  "  body"
                  "#+end_src")
      ;; TODO pretty sure this makes no sense...
      (->> (org-ml-build-src-block :value "body" :switches '("-n 20" "-r"))
           (org-ml-to-trimmed-string))
      => (:result "#+begin_src -n 20 -r"
                  "  body"
                  "#+end_src")
      ;; TODO and this...
      (->> (org-ml-build-src-block :value "body" :parameters '(:key val))
           (org-ml-to-trimmed-string))
      => (:result "#+begin_src :key val"
                  "  body"
                  "#+end_src")))

  (def-example-subgroup "Branch Element Nodes with Child Object Nodes"
    nil

    (defexamples org-ml-build-paragraph
      (->> (org-ml-build-paragraph "text")
           (org-ml-to-trimmed-string))
      => "text")

    (defexamples org-ml-build-table-row
      (->> (org-ml-build-table-cell "a")
           (org-ml-build-table-row)
           (org-ml-to-trimmed-string))
      => "| a |")

    ;; TODO should add a comment here to explain that newlines are necessary
    (defexamples org-ml-build-verse-block
      (->> (org-ml-build-verse-block "text\n")
           (org-ml-to-trimmed-string))
      => (:result "#+begin_verse"
                  "text"
                  "#+end_verse")))

  (def-example-subgroup "Branch Element Nodes with Child Element Nodes"
    nil

    (defexamples org-ml-build-org-data
      (->> (org-ml-build-headline :title '("dummy"))
        (org-ml-build-org-data)
        (org-ml-to-trimmed-string))
      => "* dummy")

    (defexamples org-ml-build-center-block
      (->> (org-ml-build-center-block)
           (org-ml-to-trimmed-string))
      => (:result "#+begin_center"
                  "#+end_center")
      (->> (org-ml-build-paragraph "text")
           (org-ml-build-center-block)
           (org-ml-to-trimmed-string))
      => (:result "#+begin_center"
                  "text"
                  "#+end_center"))

    (defexamples org-ml-build-drawer
      (->> (org-ml-build-drawer "NAME")
           (org-ml-to-trimmed-string))
      => (:result ":NAME:"
                  ":END:")
      (->> (org-ml-build-paragraph "text")
           (org-ml-build-drawer "NAME")
           (org-ml-to-trimmed-string))
      => (:result ":NAME:"
                  "text"
                  ":END:"))

    (defexamples org-ml-build-dynamic-block
      (->> (org-ml-build-dynamic-block "empty")
           (org-ml-to-trimmed-string))
      => (:result "#+begin: empty"
                  "#+end:")
      (->> (org-ml-build-comment "I'm in here")
           (org-ml-build-dynamic-block "notempty")
           (org-ml-to-trimmed-string))
      => (:result "#+begin: notempty"
                  "# I'm in here"
                  "#+end:"))

    (defexamples org-ml-build-footnote-definition
      (->> (org-ml-build-paragraph "footnote contents")
           (org-ml-build-footnote-definition "label")
           (org-ml-to-trimmed-string))
      => "[fn:label] footnote contents")

    (defexamples org-ml-build-headline
      (->> (org-ml-build-headline)
           (org-ml-to-trimmed-string))
      => "*"
      (->> (org-ml-build-headline :level 2 :title '("dummy") :tags '("tmsu"))
           (org-ml-to-trimmed-string))
      => "** dummy            :tmsu:"
      (->> (org-ml-build-headline :todo-keyword "TODO" :archivedp t
                                  :commentedp t :priority ?A)
           (org-ml-to-trimmed-string))
      => "* TODO COMMENT [#A]  :ARCHIVE:"
      :begin-hidden
      (->> (org-ml-build-headline :level 2)
           (org-ml-to-trimmed-string))
      => "**"
      (->> (org-ml-build-headline :title '("dummy"))
           (org-ml-to-trimmed-string))
      => "* dummy"
      (->> (org-ml-build-headline :tags '("tmsu"))
           (org-ml-to-trimmed-string))
      => "*                   :tmsu:"
      (->> (org-ml-build-headline :todo-keyword "DONE")
           (org-ml-to-trimmed-string))
      => "* DONE"
      (->> (org-ml-build-headline :priority ?A)
           (org-ml-to-trimmed-string))
      => "* [#A]"
      (->> (org-ml-build-headline :footnote-section-p t)
           (org-ml-to-trimmed-string))
      => "* Footnotes"
      (->> (org-ml-build-headline :commentedp t)
           (org-ml-to-trimmed-string))
      => "* COMMENT"
      (->> (org-ml-build-headline :archivedp t)
           (org-ml-to-trimmed-string))
      => "*                   :ARCHIVE:"
      :end-hidden)

    (defexamples org-ml-build-item
      (->> (org-ml-build-paragraph "item contents")
           (org-ml-build-item)
           (org-ml-to-trimmed-string))
      => "- item contents"
      (->> (org-ml-build-paragraph "item contents")
           (org-ml-build-item :bullet 1)
           (org-ml-to-trimmed-string))
      => "1. item contents"
      (->> (org-ml-build-paragraph "item contents")
           (org-ml-build-item :checkbox 'on)
           (org-ml-to-trimmed-string))
      => "- [X] item contents"
      (->> (org-ml-build-paragraph "item contents")
           (org-ml-build-item :tag '("tmsu"))
           (org-ml-to-trimmed-string))
      => "- tmsu :: item contents"
      (->> (org-ml-build-paragraph "item contents")
           (org-ml-build-item :counter 10)
           (org-ml-to-trimmed-string))
      => "- [@10] item contents")

    (defexamples org-ml-build-plain-list
      (->> (org-ml-build-paragraph "item contents")
           (org-ml-build-item)
           (org-ml-build-plain-list)
           (org-ml-to-trimmed-string))
      => "- item contents")

    (defexamples org-ml-build-property-drawer
      (->> (org-ml-build-property-drawer)
           (org-ml-to-trimmed-string))
      => (:result ":PROPERTIES:"
                  ":END:")
      (->> (org-ml-build-node-property "key" "val")
           (org-ml-build-property-drawer)
           (org-ml-to-trimmed-string))
      => (:result ":PROPERTIES:"
                  ":key:      val"
                  ":END:"))

    (defexamples org-ml-build-quote-block
      (->> (org-ml-build-quote-block)
           (org-ml-to-trimmed-string))
      => (:result "#+begin_quote"
                  "#+end_quote")
      (->> (org-ml-build-paragraph "quoted stuff")
           (org-ml-build-quote-block)
           (org-ml-to-trimmed-string))
      => (:result "#+begin_quote"
                  "quoted stuff"
                  "#+end_quote"))

    (defexamples org-ml-build-section
      (->> (org-ml-build-paragraph "text")
           (org-ml-build-section)
           (org-ml-to-trimmed-string))
      => "text")

    (defexamples org-ml-build-special-block
      (->> (org-ml-build-special-block "monad")
           (org-ml-to-trimmed-string))
      => (:result "#+begin_monad"
                  "#+end_monad")
      (->> (org-ml-build-comment "Launch missiles")
           (org-ml-build-special-block "monad")
           (org-ml-to-trimmed-string))
      => (:result "#+begin_monad"
                  "# Launch missiles"
                  "#+end_monad"))

    (defexamples org-ml-build-table
      (->> (org-ml-build-table-cell "cell")
           (org-ml-build-table-row)
           (org-ml-build-table)
           (org-ml-to-trimmed-string))
      => "| cell |"))

  (def-example-subgroup "Miscellaneous Builders"
    nil

    (defexamples-content org-ml-clone-node
      nil
      (:buffer "dolly")
      (let* ((node1 (org-ml-parse-this-element))
             (node2 (org-ml-clone-node node1)))
        (equal node1 node2))
      => t
      (let* ((node1 (org-ml-parse-this-element))
             (node2 (org-ml-clone-node node1)))
        (eq node1 node2))
      => nil)

    (defexamples-content org-ml-clone-node-n
      nil
      (:buffer "dolly")
      (-let* ((node1 (org-ml-parse-this-element))
              ((node2 node3) (org-ml-clone-node-n 2 node1)))
        (or (equal node1 node2)
            (equal node1 node3)
            (equal node2 node3)))
      => t
      (-let* ((node1 (org-ml-parse-this-element))
              ((node2 node3) (org-ml-clone-node-n 2 node1)))
        (or (eq node1 node2)
            (eq node1 node3)
            (eq node2 node3)))
      => nil)

    (defexamples org-ml-build-secondary-string!
      (->> (org-ml-build-secondary-string! "I'm plain")
           (-map #'org-ml-get-type))
      => '(plain-text)
      (->> (org-ml-build-secondary-string! "I'm *not* plain")
           (-map #'org-ml-get-type))
      => '(plain-text bold plain-text)
      (->> (org-ml-build-secondary-string! "* I'm not an object")
           (-map #'org-ml-get-type))
      !!> arg-type-error)

    (defexamples org-ml-build-table-row-hline
      (->>  (org-ml-build-table
             (org-ml-build-table-row
              (org-ml-build-table-cell "text"))
             (org-ml-build-table-row-hline))
            (org-ml-to-trimmed-string))
      => (:result "| text |"
                  "|------|"))

    (defexamples org-ml-build-timestamp-diary
      (->> (org-ml-build-timestamp-diary '(diary-float t 4 2))
           (org-ml-to-string))
      => "<%%(diary-float t 4 2)>"))

  (def-example-subgroup "Shorthand Builders"
    "Build nodes with more convenient/shorter syntax."

    (defexamples org-ml-build-timestamp!
      (->> (org-ml-build-timestamp! '(2019 1 1))
           (org-ml-to-string))
      => "[2019-01-01 Tue]"
      (->> (org-ml-build-timestamp! '(2019 1 1 12 0)
                                    :active t
                                    :warning '(all 1 day)
                                    :repeater '(cumulate 1 month))
           (org-ml-to-string))
      => "<2019-01-01 Tue 12:00 +1m -1d>"
      (->> (org-ml-build-timestamp! '(2019 1 1) :end '(2019 1 2))
           (org-ml-to-string))
      => "[2019-01-01 Tue]--[2019-01-02 Wed]")

    (defexamples org-ml-build-clock!
      (->> (org-ml-build-clock! '(2019 1 1))
           (org-ml-to-trimmed-string))
      => "CLOCK: [2019-01-01 Tue]"
      (->> (org-ml-build-clock! '(2019 1 1 12 0))
           (org-ml-to-trimmed-string))
      => "CLOCK: [2019-01-01 Tue 12:00]"
      (->> (org-ml-build-clock! '(2019 1 1 12 0) :end '(2019 1 1 13 0))
           (org-ml-to-trimmed-string))
      => "CLOCK: [2019-01-01 Tue 12:00]--[2019-01-01 Tue 13:00] =>  1:00")

    (defexamples org-ml-build-planning!
      (->> (org-ml-build-planning! :closed '(2019 1 1))
           (org-ml-to-trimmed-string))
      => "CLOSED: [2019-01-01 Tue]"
      (->> (org-ml-build-planning! :closed '(2019 1 1)
                                   :scheduled '(2018 1 1))
           (org-ml-to-trimmed-string))
      => "SCHEDULED: <2018-01-01 Mon> CLOSED: [2019-01-01 Tue]"
      (->> (org-ml-build-planning! :closed '(2019 1 1 &warning all 1 day &repeater cumulate 1 month))
           (org-ml-to-trimmed-string))
      => "CLOSED: [2019-01-01 Tue +1m -1d]")

    (defexamples org-ml-build-property-drawer!
      (->> (org-ml-build-property-drawer! '(key val))
           (org-ml-to-trimmed-string))
      => (:result ":PROPERTIES:"
                  ":key:      val"
                  ":END:"))

    (defexamples org-ml-build-headline!
      (->> (org-ml-build-headline! :title-text "really impressive title")
           (org-ml-to-trimmed-string))
      => "* really impressive title"
      (->> (org-ml-build-headline! :title-text "really impressive title"
                                   :statistics-cookie '(0 9000))
           (org-ml-to-trimmed-string))
      => "* really impressive title [0/9000]"
      (->> (org-ml-build-headline!
            :title-text "really impressive title"
            :section-children
            (list (org-ml-build-property-drawer! '(key val))
                  (org-ml-build-paragraph! "section text"))
            (org-ml-build-headline! :title-text "subhead"))
           (org-ml-to-trimmed-string))
      => (:result "* really impressive title"
                  ":PROPERTIES:"
                  ":key:      val"
                  ":END:"
                  "section text"
                  "** subhead"))

    (defexamples org-ml-build-item!
      (->> (org-ml-build-item!
            :bullet 1
            :tag "complicated *tag*"
            :paragraph "petulant /frenzy/"
            (org-ml-build-plain-list
             (org-ml-build-item! :bullet '- :paragraph "below")))
           (org-ml-to-trimmed-string))
      => (:result "1. complicated *tag* :: petulant /frenzy/"
                  "     - below"))

    (defexamples org-ml-build-paragraph!
      (->> (org-ml-build-paragraph! "stuff /with/ *formatting*" :post-blank 2)
           (org-ml-to-string))
      => (:result "stuff /with/ *formatting*"
                  ""
                  ""
                  "")
      (->> (org-ml-build-paragraph! "* stuff /with/ *formatting*")
           (org-ml-to-string))
      !!> arg-type-error)

    (defexamples org-ml-build-table-cell!
      (->> (org-ml-build-table-cell! "rage")
           (org-ml-to-trimmed-string))
      => "rage |"
      (->> (org-ml-build-table-cell! "*rage*")
           (org-ml-to-trimmed-string))
      => "*rage* |")

    (defexamples org-ml-build-table-row!
      (->> (org-ml-build-table-row! '("R" "A" "G" "E"))
           (org-ml-to-trimmed-string))
      => "| R | A | G | E |"
      (->> (org-ml-build-table-row! 'hline)
           (org-ml-to-trimmed-string))
      => "|-")

    (defexamples org-ml-build-table!
      (->> (org-ml-build-table! '("R" "A") '("G" "E"))
           (org-ml-to-trimmed-string))
      => (:result "| R | A |"
                  "| G | E |")
      (->> (org-ml-build-table! '("L" "O") 'hline '("V" "E"))
           (org-ml-to-trimmed-string))
      => (:result "| L | O |"
                  "|---+---|"
                  "| V | E |")))

  (def-example-subgroup "Logbook Item Builders"
    "Build item nodes for inclusion in headline logbooks"

    (defexamples org-ml-build-log-note
      (-> (- 1546300800 (car (current-time-zone)))
          (org-ml-build-log-note "noteworthy")
          (org-ml-to-trimmed-string))
      => (:result "- Note taken on [2019-01-01 Tue 00:00] \\\\"
                  "  noteworthy"))
    
    (defexamples org-ml-build-log-done
      (-> (- 1546300800 (car (current-time-zone)))
          (org-ml-build-log-done)
          (org-ml-to-trimmed-string))
      => (:result "- CLOSING NOTE [2019-01-01 Tue 00:00]")
      (-> (- 1546300800 (car (current-time-zone)))
          (org-ml-build-log-done "noteworthy")
          (org-ml-to-trimmed-string))
      => (:result "- CLOSING NOTE [2019-01-01 Tue 00:00] \\\\"
                  "  noteworthy"))

    (defexamples org-ml-build-log-refile
      (-> (- 1546300800 (car (current-time-zone)))
          (org-ml-build-log-refile)
          (org-ml-to-trimmed-string))
      => (:result "- Refiled on [2019-01-01 Tue 00:00]")
      (-> (- 1546300800 (car (current-time-zone)))
          (org-ml-build-log-refile "noteworthy")
          (org-ml-to-trimmed-string))
      => (:result "- Refiled on [2019-01-01 Tue 00:00] \\\\"
                  "  noteworthy"))

    (defexamples org-ml-build-log-state
      (-> (- 1546300800 (car (current-time-zone)))
          (org-ml-build-log-state "HOLD" "TODO")
          (org-ml-to-trimmed-string))
      => (:result "- State \"HOLD\"       from \"TODO\"       [2019-01-01 Tue 00:00]")
      (-> (- 1546300800 (car (current-time-zone)))
          (org-ml-build-log-state "HOLD" "TODO" "noteworthy")
          (org-ml-to-trimmed-string))
      => (:result "- State \"HOLD\"       from \"TODO\"       [2019-01-01 Tue 00:00] \\\\"
                  "  noteworthy"))

    (defexamples org-ml-build-log-deldeadline
      (-> (- 1546300800 (car (current-time-zone)))
          (org-ml-build-log-deldeadline (org-ml-build-timestamp! '(2019 1 2)))
          (org-ml-to-trimmed-string))
      => (:result "- Removed deadline, was \"[2019-01-02 Wed]\" on [2019-01-01 Tue 00:00]")
      (-> (- 1546300800 (car (current-time-zone)))
          (org-ml-build-log-deldeadline (org-ml-build-timestamp! '(2019 1 2)) "noteworthy")
          (org-ml-to-trimmed-string))
      => (:result "- Removed deadline, was \"[2019-01-02 Wed]\" on [2019-01-01 Tue 00:00] \\\\"
                  "  noteworthy"))

    (defexamples org-ml-build-log-delschedule
      (-> (- 1546300800 (car (current-time-zone)))
          (org-ml-build-log-delschedule (org-ml-build-timestamp! '(2019 1 2)))
          (org-ml-to-trimmed-string))
      => (:result "- Not scheduled, was \"[2019-01-02 Wed]\" on [2019-01-01 Tue 00:00]")
      (-> (- 1546300800 (car (current-time-zone)))
          (org-ml-build-log-delschedule (org-ml-build-timestamp! '(2019 1 2)) "noteworthy")
          (org-ml-to-trimmed-string))
      => (:result "- Not scheduled, was \"[2019-01-02 Wed]\" on [2019-01-01 Tue 00:00] \\\\"
                  "  noteworthy"))

    (defexamples org-ml-build-log-redeadline
      (-> (- 1546300800 (car (current-time-zone)))
          (org-ml-build-log-redeadline (org-ml-build-timestamp! '(2019 1 2)))
          (org-ml-to-trimmed-string))
      => (:result "- New deadline from \"[2019-01-02 Wed]\" on [2019-01-01 Tue 00:00]")
      (-> (- 1546300800 (car (current-time-zone)))
          (org-ml-build-log-redeadline (org-ml-build-timestamp! '(2019 1 2)) "noteworthy")
          (org-ml-to-trimmed-string))
      => (:result "- New deadline from \"[2019-01-02 Wed]\" on [2019-01-01 Tue 00:00] \\\\"
                  "  noteworthy"))

    (defexamples org-ml-build-log-reschedule
      (-> (- 1546300800 (car (current-time-zone)))
          (org-ml-build-log-reschedule (org-ml-build-timestamp! '(2019 1 2)))
          (org-ml-to-trimmed-string))
      => (:result "- Rescheduled from \"[2019-01-02 Wed]\" on [2019-01-01 Tue 00:00]")
      (-> (- 1546300800 (car (current-time-zone)))
          (org-ml-build-log-reschedule (org-ml-build-timestamp! '(2019 1 2)) "noteworthy")
          (org-ml-to-trimmed-string))
      => (:result "- Rescheduled from \"[2019-01-02 Wed]\" on [2019-01-01 Tue 00:00] \\\\"
                  "  noteworthy"))

    (defexamples org-ml-build-log-type
      (let ((org-log-note-headings '((test . "Changed %s from %S on %t by %u")))
            (ut (- 1546300800 (car (current-time-zone)))))
        (->> (org-ml-build-log-type 'test :unixtime ut :old "TODO" :new
                                    "DONE" :username "shadowbrokers"
                                    :note "We're coming for you")
             (org-ml-to-trimmed-string)))
      => (:result
          "- Changed \"DONE\" from \"TODO\" on [2019-01-01 Tue 00:00] by shadowbrokers \\\\"
          "  We're coming for you")
      :begin-hidden
      (let ((org-log-note-headings '((test . "My note is %t"))))
        (->> (- 1546300800 (car (current-time-zone)))
             (org-ml-build-log-type 'test :unixtime)
             (org-ml-to-trimmed-string)))
      => "- My note is [2019-01-01 Tue 00:00]"
      (let ((org-log-note-headings '((test . "My note is %T"))))
        (->> (- 1546300800 (car (current-time-zone)))
             (org-ml-build-log-type 'test :unixtime)
             (org-ml-to-trimmed-string)))
      => "- My note is <2019-01-01 Tue 00:00>"
      (let ((org-log-note-headings '((test . "My note is %d"))))
        (->> (- 1546300800 (car (current-time-zone)))
             (org-ml-build-log-type 'test :unixtime)
             (org-ml-to-trimmed-string)))
      => "- My note is [2019-01-01 Tue]"
      (let ((org-log-note-headings '((test . "My note is %D"))))
        (->> (- 1546300800 (car (current-time-zone)))
             (org-ml-build-log-type 'test :unixtime)
             (org-ml-to-trimmed-string)))
      => "- My note is <2019-01-01 Tue>"
      (let ((org-log-note-headings '((test . "My name is %u"))))
        (->> (org-ml-build-log-type 'test :username "slim")
             (org-ml-to-trimmed-string)))
      => "- My name is slim"
      (let ((org-log-note-headings '((test . "My name is %U"))))
        (->> (org-ml-build-log-type 'test :full-username "slimshady")
             (org-ml-to-trimmed-string)))
      => "- My name is slimshady"
      (let ((org-log-note-headings '((test . "My note is %S"))))
        (->> (org-ml-build-log-type 'test :old "DONE")
             (org-ml-to-trimmed-string)))
      => "- My note is \"DONE\""
      (let ((org-log-note-headings '((test . "My note is %S"))))
        (->> (org-ml-build-timestamp! '(2019 1 1 0 0))
             (org-ml-build-log-type 'test :old)
             (org-ml-to-trimmed-string)))
      => "- My note is \"[2019-01-01 Tue 00:00]\""
      (let ((org-log-note-headings '((test . "My note is %s"))))
        (->> (org-ml-build-log-type 'test :new "DONE")
             (org-ml-to-trimmed-string)))
      => "- My note is \"DONE\""
      (let ((org-log-note-headings '((test . "My note is %s"))))
        (->> (org-ml-build-timestamp! '(2019 1 1 0 0))
             (org-ml-build-log-type 'test :new)
             (org-ml-to-trimmed-string)))
      => "- My note is \"[2019-01-01 Tue 00:00]\""
      :end-hidden
      )
    ))

(def-example-group "Type Predicates"
  "Test node types."

  (defexamples-content org-ml-get-type
    nil
    (:buffer "*I'm emboldened*")
    (->> (org-ml-parse-this-object)
         (org-ml-get-type))
    => 'bold
    (:buffer "* I'm the headliner")
    (->> (org-ml-parse-this-element)
         (org-ml-get-type))
    => 'headline
    (:buffer "[2112-12-21 Wed]")
    (->> (org-ml-parse-this-object)
         (org-ml-get-type))
    => 'timestamp)

  (defexamples-content org-ml-is-type
    nil
    (:buffer "*ziltoid*")
    (->> (org-ml-parse-this-object)
         (org-ml-is-type 'bold))
    => t
    (->> (org-ml-parse-this-object)
         (org-ml-is-type 'italic))
    => nil)

  (defexamples-content org-ml-is-any-type
    nil
    (:buffer "*ziltoid*")
    (->> (org-ml-parse-this-object)
         (org-ml-is-any-type '(bold)))
    => t
    (->> (org-ml-parse-this-object)
         (org-ml-is-any-type '(bold italic)))
    => t
    (->> (org-ml-parse-this-object)
         (org-ml-is-any-type '(italic)))
    => nil)

  (defexamples-content org-ml-is-element
    nil
    (:buffer "*ziltoid*")
    (:comment "Parsing this text as an element node gives a paragraph node")
    (->> (org-ml-parse-this-element)
         (org-ml-is-element))
    => t
    (:comment "Parsing the same text as an object node gives a bold node")
    (->> (org-ml-parse-this-object)
         (org-ml-is-element))
    => nil)

  (defexamples-content org-ml-is-branch-node
    nil
    (:buffer "*ziltoid*")
    (:comment "Parsing this as an element node gives a paragraph node"
              "(a branch node)")
    (->> (org-ml-parse-this-element)
         (org-ml-is-branch-node))
    => t
    (:comment "Parsing this as an object node gives a bold node"
              "(also a branch node)")
    (->> (org-ml-parse-this-object)
         (org-ml-is-branch-node))
    => t
    (:buffer "~ziltoid~")
    (:comment "Parsing this as an object node gives a code node"
              "(not a branch node)")
    (->> (org-ml-parse-this-object)
         (org-ml-is-branch-node))
    => nil
    (:buffer "# ziltoid")
    (:comment "Parsing this as an element node gives a comment node"
              "(also not a branch node)")
    (->> (org-ml-parse-this-element)
         (org-ml-is-branch-node))
    => nil
    (:buffer "* I'm so great")
    (:comment "Parsing this as an element node gives a headline node"
              "(a branch node)")
    (->> (org-ml-parse-this-element)
         (org-ml-is-branch-node))
    => t)

  (defexamples-content org-ml-node-may-have-child-objects
    nil
    (:buffer "*ziltoid*")
    (:comment "Parsing this as an element node gives a paragraph node"
              "(can have child object nodes)")
    (->> (org-ml-parse-this-element)
         (org-ml-node-may-have-child-objects))
    => t
    (:comment "Parsing this as an object node gives a bold node"
              "(also can have child object nodes)")
    (->> (org-ml-parse-this-object)
         (org-ml-node-may-have-child-objects))
    => t
    (:buffer "~ziltoid~")
    (:comment "Parsing this as an object node gives a code node"
              "(not a branch node)")
    (->> (org-ml-parse-this-object)
         (org-ml-node-may-have-child-objects))
    => nil
    (:buffer "# ziltoid")
    (:comment "Parsing this as an element node gives a comment node"
              "(not a branch node)")
    (->> (org-ml-parse-this-element)
         (org-ml-node-may-have-child-objects))
    => nil
    (:buffer "* I'm so great")
    (:comment "Parsing this as an element node gives a headline node"
              "(can only have child element nodes)")
    (->> (org-ml-parse-this-element)
         (org-ml-node-may-have-child-objects))
    => nil)

  (defexamples-content org-ml-node-may-have-child-elements
    nil
    (:buffer "* I'm so great")
    (:comment "Parsing this as an element node gives a headline node"
              "(can have child element nodes)")
    (->> (org-ml-parse-this-element)
         (org-ml-node-may-have-child-elements))
    => t
    (:buffer "*ziltoid*")
    (:comment "Parsing this as an element node gives a paragraph node"
              "(can only have child object nodes)")
    (->> (org-ml-parse-this-element)
         (org-ml-node-may-have-child-elements))
    => nil
    (:buffer "# ziltoid")
    (:comment "Parsing this as an element node gives a comment node"
              "(not a branch node)")
    (->> (org-ml-parse-this-element)
         (org-ml-node-may-have-child-elements))
    => nil))

(def-example-group "Property Manipulation"
  "Set, get, and map properties of nodes."

  (def-example-subgroup "Generic"
    nil

    (defexamples-content org-ml-contains-point-p
      nil
      (:buffer "*findme*")
      (->> (org-ml-parse-this-object)
           (org-ml-contains-point-p 2))
      => t
      (->> (org-ml-parse-this-object)
           (org-ml-contains-point-p 10))
      => nil)

    (defexamples-content org-ml-set-property
      nil

      (:buffer "#+call: ktulu()")
      (->> (org-ml-parse-this-element)
           (org-ml-set-property :call "cthulhu")
           (org-ml-set-property :inside-header '(:cache no))
           (org-ml-set-property :arguments '("x=4"))
           (org-ml-set-property :end-header '(:exports results))
           (org-ml-to-trimmed-string))
      => "#+call: cthulhu[:cache no](x=4) :exports results"

      :begin-hidden
      (:buffer "CLOCK: [2019-01-01 Tue]")
      (->> (org-ml-parse-this-element)
           (org-ml-set-property
            :value (org-ml-build-timestamp! '(2019 1 1) :end '(2019 1 2)))
           (org-ml-to-trimmed-string))
      => "CLOCK: [2019-01-01 Tue]--[2019-01-02 Wed] => 24:00"

      (:buffer "~learn to~")
      (->> (org-ml-parse-this-object)
           (org-ml-set-property :value "why?")
           (org-ml-to-trimmed-string))
      => "~why?~"

      (:buffer "# not here")
      (->> (org-ml-parse-this-element)
           (org-ml-set-property :value "still not here")
           (org-ml-to-trimmed-string))
      => "# still not here"

      (:buffer "#+begin_comment"
               "not here"
               "#+end_comment")
      (->> (org-ml-parse-this-element)
           (org-ml-set-property :value "still not here")
           (org-ml-to-trimmed-string))
      => (:result "#+begin_comment"
                  "still not here"
                  "#+end_comment")

      (:buffer "%%(print :valueble)")
      (->> (org-ml-parse-this-element)
           (org-ml-set-property :value '(print :invaluble))
           (org-ml-to-trimmed-string))
      => "%%(print :invaluble)"

      (:buffer ":LOGBOOK:"
               ":END:")
      (->> (org-ml-parse-this-element)
           (org-ml-set-property :drawer-name "BOOKOFSOULS")
           (org-ml-to-trimmed-string))
      => (:result ":BOOKOFSOULS:"
                  ":END:")

      (:buffer "#+begin: blockhead"
               "#+end:")
      (->> (org-ml-parse-this-element)
           (org-ml-set-property :block-name "blockfoot")
           (org-ml-set-property :arguments '(:cache no))
           (org-ml-to-trimmed-string))
      => (:result "#+begin: blockfoot :cache no"
                  "#+end:")

      (:buffer "\\pi")
      (->> (org-ml-parse-this-object)
           (org-ml-set-property :name "gamma")
           (org-ml-set-property :use-brackets-p t)
           (org-ml-to-trimmed-string))
      => "\\gamma{}"

      ;; TODO test preserve indentation...
      (:buffer "#+begin_example"
               "#+end_example")
      (->> (org-ml-parse-this-element)
           (org-ml-set-property :switches '("-n"))
           (org-ml-set-property :value "example.com")
           (org-ml-to-trimmed-string))
      => (:buffer "#+begin_example -n"
                  "  example.com"
                  "#+end_example")

      (:buffer "#+begin_export latex"
               "#+end_export")
      (->> (org-ml-parse-this-element)
           (org-ml-set-property :type "domestic")
           (org-ml-set-property :value "bullets, bombs, and bigotry")
           (org-ml-to-trimmed-string))
      => (:buffer "#+begin_export domestic"
                  "bullets, bombs, and bigotry"
                  "#+end_export")

      (:buffer "@@back-end:value@@")
      (->> (org-ml-parse-this-object)
           (org-ml-set-property :back-end "latex")
           (org-ml-set-property :value "new-value")
           (org-ml-to-trimmed-string))
      => "@@latex:new-value@@"

      (:buffer ": fixed")
      (->> (org-ml-parse-this-element)
           (org-ml-set-property :value "unfixed")
           (org-ml-to-trimmed-string))
      => ": unfixed"

      (:buffer "[fn:whitelabel] society")
      (->> (org-ml-parse-this-element)
           (org-ml-set-property :label "blacklabel")
           (org-ml-to-trimmed-string))
      => "[fn:blacklabel] society"

      (:buffer "* dummy"
               "stuff")
      (->> (org-ml-parse-this-element)
           (org-ml-set-property :archivedp t)
           (org-ml-set-property :commentedp t)
           (org-ml-set-property :level 2)
           (org-ml-set-property :pre-blank 1)
           (org-ml-set-property :priority ?A)
           (org-ml-set-property :tags '("tmsu"))
           (org-ml-set-property :title '("smartie"))
           (org-ml-set-property :todo-keyword "TODO")
           (org-ml-to-trimmed-string))
      => (:result "** TODO COMMENT [#A] smartie :tmsu:ARCHIVE:"
                  ""
                  "stuff")
      :begin-hidden
      (->> (org-ml-parse-this-element)
           (org-ml-set-property :footnote-section-p t)
           (org-ml-to-trimmed-string))
      => (:result "* Footnotes"
                  "stuff")
      :end-hidden

      (:buffer "call_kthulu()")
      (->> (org-ml-parse-this-object)
           (org-ml-set-property :call "cthulhu")
           (org-ml-set-property :inside-header '(:cache no))
           (org-ml-set-property :arguments '("x=4"))
           (org-ml-set-property :end-header '(:exports results))
           (org-ml-to-trimmed-string))
      => "call_cthulhu[:cache no](x=4)[:exports results]"

      (:buffer "src_emacs{(print 'yeah-boi)}")
      (->> (org-ml-parse-this-object)
           (org-ml-set-property :language "python")
           (org-ml-set-property :parameters '(:cache no))
           (org-ml-set-property :value "print \"yeah boi\"")
           (org-ml-to-trimmed-string))
      => "src_python[:cache no]{print \"yeah boi\"}"
      :end-hidden

      (:buffer "- thing")
      (->> (org-ml-parse-this-item)
           (org-ml-set-property :bullet 1)
           (org-ml-set-property :checkbox 'on)
           (org-ml-set-property :counter 2)
           (org-ml-set-property :tag '("tmsu"))
           (org-ml-to-trimmed-string))
      => "1. [@2] [X] tmsu :: thing"

      :begin-hidden
      (:buffer "#+KEY: VAL")
      (->> (org-ml-parse-this-element)
           (org-ml-set-property :key "kee")
           (org-ml-set-property :value "vahl")
           (org-ml-to-trimmed-string))
      => "#+kee: vahl"

      ;; TODO this is stupid, who would ever do this?
      (:buffer "\begin{env}"
               "body"
               "\end{env}")
      (->> (org-ml-parse-this-element)
           (org-ml-set-property :value "\begin{vne}\nbody\end{vne}")
           (org-ml-to-trimmed-string))
      => (:buffer "\begin{vne}"
                  "body"
                  "\end{vne}")

      ;; TODO this is also stupid...
      (:buffer "$2+2=4$")
      (->> (org-ml-parse-this-object)
           (org-ml-set-property :value "$2+2=5$")
           (org-ml-to-trimmed-string))
      => "$2+2=5$"

      (:buffer "https://example.com")
      (->> (org-ml-parse-this-object)
           (org-ml-set-property :path "/dev/null")
           (org-ml-set-property :type "file")
           (org-ml-set-property :format 'bracket)
           (org-ml-to-trimmed-string))
      => "[[file:/dev/null]]"

      (:buffer "{{{economics}}}")
      (->> (org-ml-parse-this-object)
           (org-ml-set-property :key "freakonomics")
           (org-ml-set-property :args '("x=4" "y=2"))
           (org-ml-to-trimmed-string))
      => "{{{freakonomics(x=4,y=2)}}}"

      (:buffer "* dummy"
               ":PROPERTIES:"
               ":KEY: VAL"
               ":END:")
      (->> (org-ml-parse-this-headline)
           (org-ml-headline-get-node-properties)
           (-first-item)
           (org-ml-set-property :key "kee")
           (org-ml-set-property :value "vahl")
           (org-ml-to-trimmed-string))
      => ":kee:      vahl"

      (:buffer "* dummy"
               "CLOSED: <2019-01-01 Tue>")
      (->> (org-ml-parse-this-headline)
           (org-ml-headline-get-planning)
           (org-ml-set-property
            :closed (org-ml-build-timestamp! '(2019 1 2) :active nil))
           (org-ml-to-trimmed-string))
      => "CLOSED: [2019-01-02 Wed]"

      (:buffer "#+begin_special"
               "#+end_special")
      (->> (org-ml-parse-this-element)
           (org-ml-set-property :type "talent")
           (org-ml-to-trimmed-string))
      => (:result "#+begin_talent"
                  "#+end_talent")

      (:buffer "#+begin_src"
               "something amorphous"
               "#+end_src")
      (->> (org-ml-parse-this-element)
           (org-ml-set-property :language "emacs")
           (org-ml-set-property :value "(print 'hi)")
           (org-ml-set-property :parameters '(:cache no))
           (org-ml-set-property :switches '("-n"))
           ;; TODO test preserver indent
           (org-ml-to-trimmed-string))
      => (:result "#+begin_src emacs -n :cache no"
                  "  (print 'hi)"
                  "#+end_src")

      (:buffer "* dummy [50%]")
      (->> (org-ml-parse-this-headline)
           (org-ml-headline-get-statistics-cookie)
           (org-ml-set-property :value '(0 5))
           (org-ml-to-trimmed-string))
      => "[0/5]"

      (:buffer "sub_woofer")
      (->> (org-ml-parse-object-at 5)
           (org-ml-set-property :use-brackets-p t)
           (org-ml-to-trimmed-string))
      => "_{woofer}"

      (:buffer "super^woofer")
      (->> (org-ml-parse-object-at 7)
           (org-ml-set-property :use-brackets-p t)
           (org-ml-to-trimmed-string))
      => "^{woofer}"

      (:buffer "| a |")
      (->> (org-ml-parse-this-element)
           (org-ml-set-property :tblfm '("x=$2"))
           (org-ml-to-trimmed-string))
      => (:result "| a |"
                  "#+TBLFM: x=$2")

      (:buffer "<<found>>")
      (->> (org-ml-parse-this-object)
           (org-ml-set-property :value "lost")
           (org-ml-to-trimmed-string))
      => "<<lost>>"

      (:buffer "[2019-01-01 Tue]")
      (->> (org-ml-parse-this-object)
           (org-ml-set-property :year-start 2020)
           (org-ml-set-property :month-start 2)
           (org-ml-set-property :day-start 2)
           (org-ml-set-property :hour-start 12)
           (org-ml-set-property :minute-start 0)
           (org-ml-set-property :year-end 2020)
           (org-ml-set-property :month-end 2)
           (org-ml-set-property :day-end 3)
           (org-ml-set-property :hour-end 12)
           (org-ml-set-property :minute-end 0)
           (org-ml-set-property :type 'active-range)
           (org-ml-set-property :warning-type 'all)
           (org-ml-set-property :warning-unit 'day)
           (org-ml-set-property :warning-value 1)
           (org-ml-set-property :repeater-type 'cumulate)
           (org-ml-set-property :repeater-unit 'day)
           (org-ml-set-property :repeater-value 1)
           (org-ml-to-trimmed-string))
      => "<2020-02-02 Sun 12:00 +1d -1d>--<2020-02-03 Mon 12:00 +1d -1d>"

      (:buffer "=I am not a crook=")
      (->> (org-ml-parse-this-object)
           (org-ml-set-property :value "You totally are")
           (org-ml-to-trimmed-string))
      => "=You totally are="

      (:buffer "plain")
      (->> (org-ml-set-property :post-blank 1 "plain")
           (org-ml-to-string))
      => "plain "

      (:buffer "*not plain*")
      (->> (org-ml-parse-this-object)
           (org-ml-set-property :post-blank 1)
           (org-ml-to-string))
      => "*not plain* "

      ;; affiliated keywords

      (:buffer "short paragraph")
      (->> (org-ml-parse-this-element)
           (org-ml-set-property :name "foo")
           (org-ml-to-trimmed-string))
      => (:result "#+name: foo"
                  "short paragraph")
      (->> (org-ml-parse-this-element)
           (org-ml-set-property :attr_bar '("foo"))
           (org-ml-to-trimmed-string))
      => (:result "#+attr_bar: foo"
                  "short paragraph")
      (->> (org-ml-parse-this-element)
           (org-ml-set-property :header '((:k1 "h1") (:k2 "h2")))
           (org-ml-to-trimmed-string))
      => (:result "#+header: :k1 h1"
                  "#+header: :k2 h2"
                  "short paragraph")
      (->> (org-ml-parse-this-element)
           (org-ml-set-property :results '("bar" "foo"))
           (org-ml-to-trimmed-string))
      => (:result "#+results[bar]: foo"
                  "short paragraph")
      (->> (org-ml-parse-this-element)
           (org-ml-set-property :caption '("cap"))
           (org-ml-to-trimmed-string))
      => (:result "#+caption: cap"
                  "short paragraph")
      (->> (org-ml-parse-this-element)
           (org-ml-set-property :caption '(("foo" "cap")))
           (org-ml-to-trimmed-string))
      => (:result "#+caption[foo]: cap"
                  "short paragraph")
      (->> (org-ml-parse-this-element)
           (org-ml-set-property :caption '(("FOO" "CAP") ("foo" "cap")))
           (org-ml-to-trimmed-string))
      => (:result "#+caption[FOO]: CAP"
                  "#+caption[foo]: cap"
                  "short paragraph")
      
      (:buffer "#+caption: cap"
               "short paragraph")
      (->> (org-ml-parse-this-element)
           (org-ml-set-property :caption nil)
           (org-ml-to-trimmed-string))
      => "short paragraph"
      (:buffer "#+name: deleteme"
               "short paragraph")
      (->> (org-ml-parse-this-element)
           (org-ml-set-property :name nil)
           (org-ml-to-trimmed-string))
      => "short paragraph"

      :end-hidden

      (:buffer "* not valuable")
      (:comment "Throw error when setting a property that doesn't exist")
      (->> (org-ml-parse-this-headline)
           (org-ml-set-property :value "wtf")
           (org-ml-to-trimmed-string))
      !!> arg-type-error

      (:comment "Throw error when setting to an improper type")
      (->> (org-ml-parse-this-headline)
           (org-ml-set-property :title 666)
           (org-ml-to-trimmed-string))
      !!> arg-type-error)

    (defexamples-content org-ml-get-property
      nil

      (:buffer "#+call: ktulu(x=4) :exports results")
      (->> (org-ml-parse-this-element)
           (org-ml-get-property :call))
      => "ktulu"
      (->> (org-ml-parse-this-element)
           (org-ml-get-property :inside-header))
      => nil

      :begin-hidden

      (->> (org-ml-parse-this-element)
           (org-ml-get-property :arguments))
      => '("x=4")
      (->> (org-ml-parse-this-element)
           (org-ml-get-property :end-header))
      => '(:exports results)

      (:buffer "CLOCK: [2019-01-01 Tue]")
      (->> (org-ml-parse-this-element)
           (org-ml-get-property :value)
           (org-ml-to-string))
      => "[2019-01-01 Tue]"

      (:buffer "~learn to~")
      (->> (org-ml-parse-this-object)
           (org-ml-get-property :value))
      => "learn to"

      (:buffer "# not here")
      (->> (org-ml-parse-this-element)
           (org-ml-get-property :value))
      => "not here"

      (:buffer "#+begin_comment"
               "not here"
               "#+end_comment")
      (->> (org-ml-parse-this-element)
           (org-ml-get-property :value))
      => "not here"

      (:buffer "%%(print :hi)")
      (->> (org-ml-parse-this-element)
           (org-ml-get-property :value))
      => '(print :hi)

      (:buffer ":LOGBOOK:"
               ":END:")
      (->> (org-ml-parse-this-element)
           (org-ml-get-property :drawer-name))
      => "LOGBOOK"

      (:buffer "#+begin: blockhead :cache no"
               "#+end:")
      (->> (org-ml-parse-this-element)
           (org-ml-get-property :block-name))
      => "blockhead"
      (->> (org-ml-parse-this-element)
           (org-ml-get-property :arguments))
      => '(:cache no)

      (:buffer "\\pi{}")
      (->> (org-ml-parse-this-object)
           (org-ml-get-property :name))
      => "pi"
      (->> (org-ml-parse-this-object)
           (org-ml-get-property :use-brackets-p))
      => t

      ;; TODO test preserve indentation...
      => (:buffer "#+begin_example -n"
                  "example.com"
                  "#+end_example")
      (->> (org-ml-parse-this-element)
           (org-ml-get-property :switches))
      => '("-n")
      (->> (org-ml-parse-this-element)
           (org-ml-get-property :value))
      => "example.com"

      (:buffer "#+begin_export domestic"
               "bullets, bombs, and bigotry"
               "#+end_export")
      (->> (org-ml-parse-this-element)
           (org-ml-get-property :type))
      ;; TODO why capitalized?
      => "DOMESTIC"
      (->> (org-ml-parse-this-element)
           (org-ml-get-property :value))
      => "bullets, bombs, and bigotry\n"

      (:buffer "@@back-end:value@@")
      (->> (org-ml-parse-this-object)
           (org-ml-get-property :back-end))
      => "back-end"
      (->> (org-ml-parse-this-object)
           (org-ml-get-property :value))
      => "value"

      (:buffer ": fixed")
      (->> (org-ml-parse-this-element)
           (org-ml-get-property :value))
      => "fixed"

      (:buffer "[fn:blacklabel] society")
      (->> (org-ml-parse-this-element)
           (org-ml-get-property :label))
      => "blacklabel"

      ;; TODO the priority should be parsable after "COMMENT"
      (:buffer "** TODO [#A] COMMENT dummy     :tmsu:ARCHIVE:"
               ""
               "stuff")
      (->> (org-ml-parse-this-element)
           (org-ml-get-property :archivedp))
      => t
      (->> (org-ml-parse-this-element)
           (org-ml-get-property :commentedp))
      => t
      (->> (org-ml-parse-this-element)
           (org-ml-get-property :level))
      => 2
      (->> (org-ml-parse-this-element)
           (org-ml-get-property :pre-blank))
      => 1
      (->> (org-ml-parse-this-element)
           (org-ml-get-property :priority))
      => ?A
      (->> (org-ml-parse-this-element)
           (org-ml-get-property :tags))
      => '("tmsu")
      (->> (org-ml-parse-this-element)
           (org-ml-get-property :title))
      => '("dummy")
      (->> (org-ml-parse-this-element)
           (org-ml-get-property :todo-keyword))
      => "TODO"

      (:buffer "* Footnotes")
      (->> (org-ml-parse-this-element)
           (org-ml-get-property :footnote-section-p))
      => t

      (:buffer "call_ktulu[:cache no](x=4)[:exports results]")
      (->> (org-ml-parse-this-object)
           (org-ml-get-property :call))
      => "ktulu"
      (->> (org-ml-parse-this-object)
           (org-ml-get-property :inside-header))
      =>  '(:cache no)
      (->> (org-ml-parse-this-object)
           (org-ml-get-property :arguments))
      => '("x=4")
      (->> (org-ml-parse-this-object)
           (org-ml-get-property :end-header))
      => '(:exports results)

      (:buffer "src_python[:cache no]{print \"yeah boi\"}")
      (->> (org-ml-parse-this-object)
           (org-ml-get-property :language))
      => "python"
      (->> (org-ml-parse-this-object)
           (org-ml-get-property :parameters))
      => '(:cache no)
      (->> (org-ml-parse-this-object)
           (org-ml-get-property :value))
      => "print \"yeah boi\""

      (:buffer "- [@2] [X] tmsu :: thing")
      (->> (org-ml-parse-this-item)
           (org-ml-get-property :bullet))
      => '-
      (->> (org-ml-parse-this-item)
           (org-ml-get-property :checkbox))
      => 'on
      (->> (org-ml-parse-this-item)
           (org-ml-get-property :counter))
      => 2
      (->> (org-ml-parse-this-item)
           (org-ml-get-property :tag))
      => '("tmsu")

      (:buffer "#+KEY: VAL")
      (->> (org-ml-parse-this-element)
           (org-ml-get-property :key))
      => "KEY"
      (->> (org-ml-parse-this-element)
           (org-ml-get-property :value))
      => "VAL"

      (:buffer "\begin{env}"
               "body"
               "\end{env}")
      (->> (org-ml-parse-this-element)
           (org-ml-get-property :value))
      => (:buffer "\begin{env}"
                  "body"
                  "\end{env}")

      (:buffer "$2+2=4$")
      (->> (org-ml-parse-this-object)
           (org-ml-get-property :value))
      => "$2+2=4$"

      (:buffer "[[file:/dev/null]]")
      (->> (org-ml-parse-this-object)
           (org-ml-get-property :path))
      => "/dev/null"
      (->> (org-ml-parse-this-object)
           (org-ml-get-property :type))
      => "file"
      (->> (org-ml-parse-this-object)
           (org-ml-get-property :format))
      => 'bracket
      
      (:buffer "{{{economics(x=4,y=2)}}}")
      (->> (org-ml-parse-this-object)
           (org-ml-get-property :key))
      => "economics"
      (->> (org-ml-parse-this-object)
           (org-ml-get-property :args))
      => '("x=4" "y=2")

      (:buffer "* dummy"
               ":PROPERTIES:"
               ":KEY: VAL"
               ":END:")
      (->> (org-ml-parse-this-headline)
           (org-ml-headline-get-node-properties)
           (-first-item)
           (org-ml-get-property :key))
      => "KEY"
      (->> (org-ml-parse-this-headline)
           (org-ml-headline-get-node-properties)
           (-first-item)
           (org-ml-get-property :value))
      => "VAL"

      (:buffer "* dummy"
               "CLOSED: [2019-01-01 Tue]")
      (->> (org-ml-parse-this-headline)
           (org-ml-headline-get-planning)
           (org-ml-get-property :closed)
           (org-ml-to-string))
      => "[2019-01-01 Tue]"

      (:buffer "#+BEGIN_special"
               "#+END_special")
      (->> (org-ml-parse-this-element)
           (org-ml-get-property :type))
      => "special"

      (:buffer "#+begin_src emacs -n :cache no"
               "  (print 'hi)"
               "#+end_src")
      (->> (org-ml-parse-this-element)
           (org-ml-get-property :language))
      => "emacs"
      (->> (org-ml-parse-this-element)
           (org-ml-get-property :value))
      ;; TODO why indented?
      => "  (print 'hi)"
      (->> (org-ml-parse-this-element)
           (org-ml-get-property :parameters))
      => '(:cache no)
      (->> (org-ml-parse-this-element)
           (org-ml-get-property :switches))
      => '("-n")

      (:buffer "* dummy [50%]")
      (->> (org-ml-parse-this-headline)
           (org-ml-headline-get-statistics-cookie)
           (org-ml-get-property :value))
      => '(50)

      (:buffer "sub_{woofer}")
      (->> (org-ml-parse-object-at 6)
           (org-ml-get-property :use-brackets-p))
      => t

      (:buffer "super_{woofer}")
      (->> (org-ml-parse-object-at 8)
           (org-ml-get-property :use-brackets-p))
      => t

      (:buffer "| a |"
               "#+TBLFM: x=$2")
      (->> (org-ml-parse-this-element)
           (org-ml-get-property :tblfm))
      => '("x=$2")

      (:buffer "<<found>>")
      (->> (org-ml-parse-this-object)
           (org-ml-get-property :value))
      => "found"

      (:buffer "<2020-02-02 Sun 12:00 +1d -1d>--<2020-02-03 Mon 12:00 +1d -1d>")
      (->> (org-ml-parse-this-object)
           (org-ml-get-property :year-start))
      => 2020
      (->> (org-ml-parse-this-object)
           (org-ml-get-property :month-start))
      => 2
      (->> (org-ml-parse-this-object)
           (org-ml-get-property :day-start))
      => 2
      (->> (org-ml-parse-this-object)
           (org-ml-get-property :hour-start))
      => 12
      (->> (org-ml-parse-this-object)
           (org-ml-get-property :minute-start))
      => 0
      (->> (org-ml-parse-this-object)
           (org-ml-get-property :year-end))
      => 2020
      (->> (org-ml-parse-this-object)
           (org-ml-get-property :month-end))
      => 2
      (->> (org-ml-parse-this-object)
           (org-ml-get-property :day-end))
      => 3
      (->> (org-ml-parse-this-object)
           (org-ml-get-property :hour-end))
      => 12
      (->> (org-ml-parse-this-object)
           (org-ml-get-property :minute-end))
      => 0
      (->> (org-ml-parse-this-object)
           (org-ml-get-property :type))
      => 'active-range
      (->> (org-ml-parse-this-object)
           (org-ml-get-property :warning-type))
      => 'all
      (->> (org-ml-parse-this-object)
           (org-ml-get-property :warning-unit))
      => 'day
      (->> (org-ml-parse-this-object)
           (org-ml-get-property :warning-value))
      => 1
      (->> (org-ml-parse-this-object)
           (org-ml-get-property :repeater-type))
      => 'cumulate
      (->> (org-ml-parse-this-object)
           (org-ml-get-property :repeater-unit))
      => 'day
      (->> (org-ml-parse-this-object)
           (org-ml-get-property :repeater-value))
      => 1

      (:buffer "=I am not a crook=")
      (->> (org-ml-parse-this-object)
           (org-ml-get-property :value))
      => "I am not a crook"

      (:buffer "*postable* ")
      (->> (org-ml-parse-this-object)
           (org-ml-get-property :post-blank))
      => 1

      (:buffer "/*child*/")
      (->> (org-ml-parse-this-object)
           (org-ml-get-children)
           (car)
           (org-ml-get-property :parent)
           (org-ml-to-trimmed-string))
      => "/*child*/"

      (:buffer "/16-chars-long/")
      (->> (org-ml-parse-this-object)
           (org-ml-get-property :begin))
      => 1
      (->> (org-ml-parse-this-object)
           (org-ml-get-property :contents-begin))
      => 2
      (->> (org-ml-parse-this-object)
           (org-ml-get-property :contents-end))
      => 15
      (->> (org-ml-parse-this-object)
           (org-ml-get-property :end))
      => 16

      ;; affiliated keywords

      (:buffer "#+name: name"
               "#+attr_foo: bar"
               "#+attr_foo: BAR"
               "#+plot: poo"
               "#+caption: koo"
               "#+caption[COO]: KOO"
               "#+results[hash]: res"
               "#+header: :k1 h1"
               "#+header: :k2 h2"
               "#+begin_src"
               "echo test for echo"
               "#+end_src")
      (->> (org-ml-parse-this-element)
           (org-ml-get-property :name))
      => "name"
      (->> (org-ml-parse-this-element)
           (org-ml-get-property :plot))
      => "poo"
      (->> (org-ml-parse-this-element)
           (org-ml-get-property :attr_foo))
      ;; TODO why are these reversed?
      => '("BAR" "bar")
      (->> (org-ml-parse-this-element)
           (org-ml-get-property :header))
      => '((:k1 "h1") (:k2 "h2"))
      (->> (org-ml-parse-this-element)
           (org-ml-get-property :results))
      => '("hash" "res")
      (->> (org-ml-parse-this-element)
           (org-ml-get-property :caption))
      => '("koo" ("COO" "KOO"))
      
      :end-hidden

      (:buffer "* not arguable")
      (:comment "Throw error when requesting a property that doesn't exist")
      (->> (org-ml-parse-this-headline)
           (org-ml-get-property :value))
      !!> arg-type-error)

    (defexamples-content org-ml-map-property
      nil

      :begin-hidden

      (:buffer "#+call: ktulu()")
      (->> (org-ml-parse-this-element)
           (org-ml-map-property :call #'s-upcase)
           (org-ml-to-trimmed-string))
      => "#+call: KTULU()"

      (:buffer "CLOCK: [2019-01-01 Tue 12:00]")
      (->> (org-ml-parse-this-element)
           (org-ml-map-property* :value (org-ml-timestamp-shift-end 1 'hour it))
           (org-ml-to-trimmed-string))
      => "CLOCK: [2019-01-01 Tue 12:00]--[2019-01-01 Tue 13:00] =>  1:00"

      :end-hidden
      
      (:buffer "~learn to~")
      (->> (org-ml-parse-this-object)
           (org-ml-map-property :value #'s-upcase)
           (org-ml-to-trimmed-string))
      => "~LEARN TO~"
      (:comment "Throw error if property doesn't exist")
      (->> (org-ml-parse-this-object)
           (org-ml-map-property :title #'s-upcase)
           (org-ml-to-trimmed-string))
      !!> arg-type-error
      (:comment "Throw error if function doesn't return proper type")
      (->> (org-ml-parse-this-object)
           (org-ml-map-property* :value (if it 1 0))
           (org-ml-to-trimmed-string))
      !!> arg-type-error

      :begin-hidden

      (:buffer "# not here")
      (->> (org-ml-parse-this-element)
           (org-ml-map-property :value #'s-upcase)
           (org-ml-to-trimmed-string))
      => "# NOT HERE"

      (:buffer "#+begin_comment"
               "not here"
               "#+end_comment")
      (->> (org-ml-parse-this-element)
           (org-ml-map-property :value #'s-upcase)
           (org-ml-to-trimmed-string))
      => (:result "#+begin_comment"
                  "NOT HERE"
                  "#+end_comment")

      (:buffer "%%(diary-float t 1 -1)")
      (->> (org-ml-parse-this-element)
           (org-ml-map-property :value (org-ml--map-last* (+ 2 it) it))
           (org-ml-to-trimmed-string))
      => (:buffer "%%(diary-float t 1 1)")

      (:buffer ":LOGBOOK:"
               ":END:")
      (->> (org-ml-parse-this-element)
           (org-ml-map-property :drawer-name #'s-capitalize)
           (org-ml-to-trimmed-string))
      => (:result ":Logbook:"
                  ":END:")

      (:buffer "#+begin: blockhead"
               "#+end:")
      (->> (org-ml-parse-this-element)
           (org-ml-map-property :block-name #'s-upcase)
           (org-ml-to-trimmed-string))
      => (:result "#+begin: BLOCKHEAD"
                  "#+end:")

      ;; TODO add entity

      (:buffer "#+begin_example"
               "example.com"
               "#+end_example")
      (->> (org-ml-parse-this-element)
           (org-ml-map-property* :value (concat "https://" it))
           (org-ml-to-trimmed-string))
      => (:result "#+begin_example"
                  "  https://example.com"
                  "#+end_example")

      (:buffer "#+begin_export domestic"
               "bullets, bombs, and bigotry"
               "#+end_export")
      (->> (org-ml-parse-this-element)
           (org-ml-map-property :type #'s-upcase)
           (org-ml-map-property :value #'s-upcase)
           (org-ml-to-trimmed-string))
      => (:result "#+begin_export DOMESTIC"
                  "BULLETS, BOMBS, AND BIGOTRY"
                  "#+end_export")

      (:buffer "@@back-end:value@@")
      (->> (org-ml-parse-this-object)
           (org-ml-map-property :back-end #'s-upcase)
           (org-ml-map-property :value #'s-upcase)
           (org-ml-to-trimmed-string))
      => "@@BACK-END:VALUE@@"

      (:buffer ": fixed")
      (->> (org-ml-parse-this-element)
           (org-ml-map-property :value #'s-upcase)
           (org-ml-to-trimmed-string))
      => ": FIXED"

      (:buffer "[fn:blacklabel] society")
      (->> (org-ml-parse-this-element)
           (org-ml-map-property :label #'s-upcase)
           (org-ml-to-trimmed-string))
      => "[fn:BLACKLABEL] society"

      (:buffer "* headline")
      (->> (org-ml-parse-this-headline)
           (org-ml-map-property* :title (-map #'s-upcase it))
           (org-ml-to-trimmed-string))
      => "* HEADLINE"

      (:buffer "call_ktulu()")
      (->> (org-ml-parse-this-object)
           (org-ml-map-property :call #'s-upcase)
           (org-ml-to-trimmed-string))
      => "call_KTULU()"

      (:buffer "src_python{print \"hi\"}")
      (->> (org-ml-parse-this-object)
           (org-ml-map-property* :value (s-replace-regexp "\".*\"" #'s-upcase it))
           (org-ml-to-trimmed-string))
      => "src_python{print \"HI\"}"

      (:buffer "- tag :: thing")
      (->> (org-ml-parse-this-item)
           (org-ml-map-property :tag (lambda (it) (-map #'s-upcase it)))
           (org-ml-to-trimmed-string))
      => "- TAG :: thing"

      (:buffer "#+key: VAL")
      (->> (org-ml-parse-this-element)
           (org-ml-map-property :key (-partial #'s-prepend "OM_"))
           (org-ml-map-property :value (-partial #'s-prepend "OM_"))
           (org-ml-to-trimmed-string))
      => "#+om_key: OM_VAL"

      ;; TODO add examples for latex frag/env

      (:buffer "[[https://downloadmoreram.org][legit]]")
      (->> (org-ml-parse-this-object)
           (org-ml-map-property* :path (s-replace ".org" ".com" it))
           (org-ml-to-trimmed-string))
      => "[[https://downloadmoreram.com][legit]]"

      (:buffer "{{{economics}}}")
      (->> (org-ml-parse-this-object)
           (org-ml-map-property :key #'s-upcase)
           (org-ml-to-trimmed-string))
      => "{{{ECONOMICS}}}"

      (:buffer "* dummy"
               ":PROPERTIES:"
               ":KEY: VAL"
               ":END:")
      (->> (org-ml-parse-this-headline)
           (org-ml-headline-get-node-properties)
           (-first-item)
           (org-ml-map-property :key (-partial #'s-prepend "OM_"))
           (org-ml-map-property :value (-partial #'s-prepend "OM_"))
           (org-ml-to-trimmed-string))
      => ":OM_KEY:   OM_VAL"

      ;; TODO add example for planning

      (:buffer "#+begin_special"
               "#+end_special")
      (->> (org-ml-parse-this-element)
           (org-ml-map-property :type #'s-upcase)
           (org-ml-to-trimmed-string))
      => (:result "#+begin_SPECIAL"
                  "#+end_SPECIAL")

      ;; TODO add example for src block

      ;; TODO add example for statistics cookie

      (:buffer "<<found>>")
      (->> (org-ml-parse-this-object)
           (org-ml-map-property :value #'s-upcase)
           (org-ml-to-trimmed-string))
      => "<<FOUND>>"

      (:buffer "=I am not a crook=")
      (->> (org-ml-parse-this-object)
           (org-ml-map-property :value #'s-upcase)
           (org-ml-to-trimmed-string))
      => "=I AM NOT A CROOK="
      :end-hidden)

    (defexamples-content org-ml-toggle-property
      nil

      (:buffer "\\pi")
      (->> (org-ml-parse-this-object)
           (org-ml-toggle-property :use-brackets-p)
           (org-ml-to-trimmed-string))
      => "\\pi{}"

      ;; TODO test src/example block preserve indent

      :begin-hidden
      
      (:buffer "* headline")
      (->> (org-ml-parse-this-headline)
           (org-ml-toggle-property :archivedp)
           (org-ml-to-trimmed-string))
      => "* headline          :ARCHIVE:"
      (->> (org-ml-parse-this-headline)
           (org-ml-toggle-property :commentedp)
           (org-ml-to-trimmed-string))
      => "* COMMENT headline"
      (->> (org-ml-parse-this-headline)
           (org-ml-toggle-property :footnote-section-p)
           (org-ml-to-trimmed-string))
      => "* Footnotes"


      (:buffer "sub_woofer")
      (->> (org-ml-parse-object-at 5)
           (org-ml-toggle-property :use-brackets-p)
           (org-ml-to-trimmed-string))
      => "_{woofer}"

      (:buffer "super^woofer")
      (->> (org-ml-parse-object-at 7)
           (org-ml-toggle-property :use-brackets-p)
           (org-ml-to-trimmed-string))
      => "^{woofer}"

      :end-hidden

      (:buffer "- [ ] nope")
      (:comment "Throw an error when trying to toggle a non-boolean property")
      (->> (org-ml-parse-this-item)
           (org-ml-toggle-property :checkbox)
           (org-ml-to-trimmed-string))
      !!> arg-type-error)

    (defexamples-content org-ml-shift-property
      nil

      (:buffer "* no priorities")
      (:comment "Do nothing if there is nothing to shift.")
      (->> (org-ml-parse-this-headline)
           (org-ml-shift-property :priority 1)
           (org-ml-to-trimmed-string))
      => "* no priorities"

      (:buffer "* [#A] priorities")
      (->> (org-ml-parse-this-headline)
           (org-ml-shift-property :priority -1)
           (org-ml-to-trimmed-string))
      => "* [#B] priorities"
      (:comment "Wrap priority around when crossing the min or max")
      (->> (org-ml-parse-this-headline)
           (org-ml-shift-property :priority 1)
           (org-ml-to-trimmed-string))
      => "* [#C] priorities"

      :begin-hidden

      (->> (org-ml-parse-this-headline)
           (org-ml-shift-property :priority -2)
           (org-ml-to-trimmed-string))
      => "* [#C] priorities"

      :end-hidden

      (:buffer "* TODO or not todo")
      (:comment "Throw error when shifting an unshiftable property")
      (->> (org-ml-parse-this-headline)
           (org-ml-shift-property :todo-keyword 1)
           (org-ml-to-string))
      !!> arg-type-error

      :begin-hidden

      (:buffer "*bold*")
      (->> (org-ml-parse-this-object)
           (org-ml-shift-property :post-blank 1)
           (org-ml-to-string))
      => "*bold* "
      (->> (org-ml-parse-this-object)
           (org-ml-shift-property :post-blank -1)
           (org-ml-to-string))
      => "*bold*"

      (:buffer "1. thing")
      (->> (org-ml-parse-this-item)
           (org-ml-shift-property :counter 1)
           (org-ml-to-trimmed-string))
      => "1. thing"

      (:buffer "1. [@1] thing")
      (->> (org-ml-parse-this-item)
           (org-ml-shift-property :counter 1)
           (org-ml-to-trimmed-string))
      => "1. [@2] thing"
      (->> (org-ml-parse-this-item)
           (org-ml-shift-property :counter -1)
           (org-ml-to-trimmed-string))
      => "1. [@1] thing"

      (:buffer "* noob level")
      (->> (org-ml-parse-this-headline)
           (org-ml-shift-property :level 1)
           (org-ml-to-trimmed-string))
      => "** noob level"

      (:comment "Do nothing when final value is less than one.")
      (->> (org-ml-parse-this-headline)
           (org-ml-shift-property :level -1)
           (org-ml-to-trimmed-string))
      => "* noob level"

      (:buffer "* headline"
               "stuff")
      (->> (org-ml-parse-this-headline)
           (org-ml-shift-property :pre-blank 1)
           (org-ml-to-trimmed-string))
      => (:result "* headline"
                  ""
                  "stuff")
      (->> (org-ml-parse-this-headline)
           (org-ml-shift-property :pre-blank -1)
           (org-ml-to-trimmed-string))
      => (:result "* headline"
                  "stuff")
      :end-hidden)

    (defexamples-content org-ml-insert-into-property
      nil

      (:buffer "#+call: ktulu(y=1)")
      (->> (org-ml-parse-this-element)
           (org-ml-insert-into-property :arguments 0 "x=4")
           (org-ml-to-trimmed-string))
      => "#+call: ktulu(x=4,y=1)"

      (:comment "Do nothing if the string is already in the list")
      (->> (org-ml-parse-this-element)
           (org-ml-insert-into-property :arguments 0 "y=1")
           (org-ml-to-trimmed-string))
      => "#+call: ktulu(y=1)"

      (:comment "Throw error when inserting into a property that is not a list of strings")
      (->> (org-ml-parse-this-element)
           (org-ml-insert-into-property :end-header 0 "html")
           (org-ml-to-trimmed-string))
      !!> arg-type-error

      :begin-hidden

      (:buffer "* headline          :tag1:")
      (->> (org-ml-parse-this-headline)
           (org-ml-insert-into-property :tags 0 "tag0")
           (org-ml-to-trimmed-string))
      => "* headline          :tag0:tag1:"

      (:buffer "#+begin_example -n"
               "#+end_example")
      (->> (org-ml-parse-this-element)
           (org-ml-insert-into-property :switches -1 "-r")
           (org-ml-to-trimmed-string))
      => (:result "#+begin_example -n -r"
                  "#+end_example")

      (:buffer "call_ktulu(y=1)")
      (->> (org-ml-parse-this-object)
           (org-ml-insert-into-property :arguments 0 "x=4")
           (org-ml-to-trimmed-string))
      => "call_ktulu(x=4,y=1)"

      (:buffer "{{{economics(x=4)}}}")
      (->> (org-ml-parse-this-object)
           (org-ml-insert-into-property :args 0 "z=2")
           (org-ml-to-trimmed-string))
      => "{{{economics(z=2,x=4)}}}"
      
      (:buffer "#+begin_src emacs-lisp -n"
               "#+end_src")
      (->> (org-ml-parse-this-element)
           (org-ml-insert-into-property :switches -1 "-r")
           (org-ml-to-trimmed-string))
      => (:result "#+begin_src emacs-lisp -n -r"
                  "#+end_src")

      (:buffer "| a |"
               "#+TBLFM: x=$2")
      (->> (org-ml-parse-this-element)
           (org-ml-insert-into-property :tblfm -1 "y=$3")
           (org-ml-to-trimmed-string))
      => (:result "| a |"
                  "#+TBLFM: y=$3"
                  "#+TBLFM: x=$2")
      :end-hidden)

    (defexamples-content org-ml-remove-from-property
      nil

      (:buffer "#+call: ktulu(y=1)")
      (->> (org-ml-parse-this-element)
           (org-ml-remove-from-property :arguments "y=1")
           (org-ml-to-trimmed-string))
      => "#+call: ktulu()"

      (:comment "Do nothing if the string does not exist")
      (->> (org-ml-parse-this-element)
           (org-ml-remove-from-property :arguments "d=666")
           (org-ml-to-trimmed-string))
      => "#+call: ktulu(y=1)"

      (:comment "Throw error when removing from property that is not a string list")
      (->> (org-ml-parse-this-element)
           (org-ml-remove-from-property :end-header ":results")
           (org-ml-to-trimmed-string))
      !!> arg-type-error

      :begin-hidden

      (:buffer "* headline       :tag1:")
      (->> (org-ml-parse-this-headline)
           (org-ml-remove-from-property :tags "tag1")
           (org-ml-to-trimmed-string))
      => "* headline"

      (:buffer "#+begin_example -n"
               "#+end_example")
      (->> (org-ml-parse-this-element)
           (org-ml-remove-from-property :switches "-n")
           (org-ml-to-trimmed-string))
      => (:result "#+begin_example"
                  "#+end_example")

      (:buffer "call_ktulu(y=1)")
      (->> (org-ml-parse-this-object)
           (org-ml-remove-from-property :arguments "y=1")
           (org-ml-to-trimmed-string))
      => "call_ktulu()"

      (:buffer "{{{economics(x=4)}}}")
      (->> (org-ml-parse-this-object)
           (org-ml-remove-from-property :args "x=4")
           (org-ml-to-trimmed-string))
      => "{{{economics}}}"
      
      (:buffer "#+begin_src emacs-lisp -n"
               "#+end_src")
      (->> (org-ml-parse-this-element)
           (org-ml-remove-from-property :switches "-n")
           (org-ml-to-trimmed-string))
      => (:result "#+begin_src emacs-lisp"
                  "#+end_src")

      (:buffer "| a |"
               "#+TBLFM: x=$2")
      (->> (org-ml-parse-this-element)
           (org-ml-remove-from-property :tblfm "x=$2")
           (org-ml-to-trimmed-string))
      => "| a |"
      :end-header)

    (defexamples-content org-ml-plist-put-property
      nil

      (:buffer "#+call: ktulu[:cache no]()")
      (->> (org-ml-parse-this-element)
           (org-ml-plist-put-property :end-header :results 'html)
           (org-ml-to-trimmed-string))
      => "#+call: ktulu[:cache no]() :results html"
      (:comment "Change the value of key if it already is present")
      (->> (org-ml-parse-this-element)
           (org-ml-plist-put-property :inside-header :cache 'yes)
           (org-ml-to-trimmed-string))
      => "#+call: ktulu[:cache yes]()"
      (:comment "Do nothing if the key and value already exist")
      (->> (org-ml-parse-this-element)
           (org-ml-plist-put-property :inside-header :cache 'no)
           (org-ml-to-trimmed-string))
      => "#+call: ktulu[:cache no]()"
      (:comment "Throw error if setting property that isn't a plist")
      (->> (org-ml-parse-this-element)
           (org-ml-plist-put-property :arguments :cache 'no)
           (org-ml-to-trimmed-string))
      !!> arg-type-error

      :begin-hidden

      (:buffer "#+begin: blockhead :format \"[%s]\""
               "#+end:")
      (->> (org-ml-parse-this-element)
           (org-ml-plist-put-property :arguments :format "<%s>")
           (org-ml-to-trimmed-string))
      => (:result "#+begin: blockhead :format \"<%s>\""
                  "#+end:")

      (:buffer "call_ktulu[:cache no]()")
      (->> (org-ml-parse-this-object)
           (org-ml-plist-put-property :inside-header :cache 'yes)
           (org-ml-plist-put-property :end-header :results 'html)
           (org-ml-to-trimmed-string))
      => "call_ktulu[:cache yes]()[:results html]"

      (:buffer "src_emacs-lisp[:exports results]{}")
      (->> (org-ml-parse-this-object)
           (org-ml-plist-put-property :parameters :exports 'both)
           (org-ml-to-trimmed-string))
      => "src_emacs-lisp[:exports both]{}"

      (:buffer "#+begin_src emacs-lisp -n :exports results"
               "#+end_src")
      (->> (org-ml-parse-this-element)
           (org-ml-plist-put-property :parameters :exports 'both)
           (org-ml-to-trimmed-string))
      => (:result "#+begin_src emacs-lisp -n :exports both"
                  "#+end_src")
      :end-hidden)

    (defexamples-content org-ml-plist-remove-property
      nil

      (:buffer "#+call: ktulu() :results html")
      (->> (org-ml-parse-this-element)
           (org-ml-plist-remove-property :end-header :results)
           (org-ml-to-trimmed-string))
      => "#+call: ktulu()"
      (:comment "Do nothing if the key is not present")
      (->> (org-ml-parse-this-element)
           (org-ml-plist-remove-property :inside-header :cache)
           (org-ml-to-trimmed-string))
      => "#+call: ktulu() :results html"
      (:comment "Throw error if trying to remove key from non-plist property")
      (->> (org-ml-parse-this-element)
           (org-ml-plist-remove-property :arguments :cache)
           (org-ml-to-trimmed-string))
      !!> arg-type-error

      :begin-hidden

      (:buffer "#+begin: blockhead :format \"[%s]\""
               "#+end:")
      (->> (org-ml-parse-this-element)
           (org-ml-plist-remove-property :arguments :format)
           (org-ml-to-trimmed-string))
      => (:result "#+begin: blockhead"
                  "#+end:")

      (:buffer "call_ktulu[:cache no]()[:results html]")
      (->> (org-ml-parse-this-object)
           (org-ml-plist-remove-property :inside-header :cache)
           (org-ml-plist-remove-property :end-header :results)
           (org-ml-to-trimmed-string))
      => "call_ktulu()"

      (:buffer "src_emacs-lisp[:exports results]{}")
      (->> (org-ml-parse-this-object)
           (org-ml-plist-remove-property :parameters :exports)
           (org-ml-to-trimmed-string))
      => "src_emacs-lisp{}"

      (:buffer "#+begin_src emacs-lisp -n :exports results"
               "#+end_src")
      (->> (org-ml-parse-this-element)
           (org-ml-plist-remove-property :parameters :exports)
           (org-ml-to-trimmed-string))
      => (:result "#+begin_src emacs-lisp -n"
                  "#+end_src")
      :end-hidden)

    ;; (defexamples-content org-ml-property-is-nil-p
    ;;   nil
    ;;   (:buffer "* TODO dummy")
    ;;   (->> (org-ml-parse-this-headline)
    ;;        (org-ml-property-is-nil-p :todo-keyword))
    ;;   => nil
    ;;   (->> (org-ml-parse-this-headline)
    ;;        (org-ml-property-is-nil-p :commentedp))
    ;;   => t)

    ;; (defexamples-content org-ml-property-is-non-nil-p
    ;;   nil
    ;;   (:buffer "* TODO dummy")
    ;;   (->> (org-ml-parse-this-headline)
    ;;        (org-ml-property-is-non-nil-p :todo-keyword))
    ;;   => t
    ;;   (->> (org-ml-parse-this-headline)
    ;;        (org-ml-property-is-non-nil-p :commentedp))
    ;;   => nil)

    ;; (defexamples-content org-ml-property-is-eq-p
    ;;   nil
    ;;   (:buffer "* [#A] dummy")
    ;;   (->> (org-ml-parse-this-headline)
    ;;        (org-ml-property-is-eq-p :priority ?A))
    ;;   => t
    ;;   (->> (org-ml-parse-this-headline)
    ;;        (org-ml-property-is-eq-p :priority ?B))
    ;;   => nil)

    ;; (defexamples-content org-ml-property-is-equal-p
    ;;   nil
    ;;   (:buffer "* TODO dummy")
    ;;   (->> (org-ml-parse-this-headline)
    ;;        (org-ml-property-is-equal-p :todo-keyword "TODO"))
    ;;   => t
    ;;   (->> (org-ml-parse-this-headline)
    ;;        (org-ml-property-is-equal-p :todo-keyword "DONE"))
    ;;   => nil)

    ;; (defexamples-content org-ml-property-is-predicate-p
    ;;   nil
    ;;   (:buffer "* this is a dummy")
    ;;   (->> (org-ml-parse-this-headline)
    ;;        (org-ml-property-is-predicate-p*
    ;;         :title (s-contains? "dummy" (car it))))
    ;;   => t)

    (defexamples-content org-ml-get-properties
      nil

      (:buffer "call_ktulu[:cache no](x=4)[:exports results]")
      (->> (org-ml-parse-this-object)
           (org-ml-get-properties '(:call :inside-header :arguments :end-header)))
      => '("ktulu" (:cache no) ("x=4") (:exports results)))

    (defexamples-content org-ml-get-all-properties
      nil

      (:buffer "*bold*")
      (--> (org-ml-parse-this-object)
           (org-ml-get-all-properties it)
           (plist-put it :parent nil))
      => '(:begin 1 :end 7 :contents-begin 2 :contents-end 6 :post-blank 0 :parent nil))

    (defexamples-content org-ml-set-properties
      nil
      
      (:buffer "- thing")
      (->> (org-ml-parse-this-item)
           (org-ml-set-properties (list :bullet 1
                                        :checkbox 'on
                                        :counter 2
                                        :tag '("tmsu")))
           (org-ml-to-trimmed-string))
      => "1. [@2] [X] tmsu :: thing"

      (:buffer "- plain")
      (->> (org-ml-parse-this-element)
           (org-ml-set-properties (list :name "plain name"
                                        :attr_XXX '("tmsu")))
           (org-ml-to-trimmed-string))
      => (:result "#+name: plain name"
                  "#+attr_xxx: tmsu"
                  "- plain"))

    (defexamples-content org-ml-map-properties
      nil

      (:buffer "#+KEY: VAL")
      (->> (org-ml-parse-this-element)
           (org-ml-map-properties
            (list :key (-partial #'s-prepend "OM_")
                  :value (-partial #'s-prepend "OM_")))
           (org-ml-to-trimmed-string))
      => "#+om_key: OM_VAL"
      ;; TODO this makes the document parser puke
      ;; (:comment "Throw error if any of the properties are invalid")
      ;; (->> (org-ml-parse-this-element)
      ;;      (org-ml-map-properties*
      ;;       (:title (s-prepend "OM_" it) :value (s-prepend "OM_" it)))
      ;;      (org-ml-to-trimmed-string))
      ;; !!> error
      )

    (defexamples-content org-ml-get-parents
      nil
      (:buffer "* one"
               "** two"
               "*** three")
      (->> (org-ml-parse-this-subtree)
           (org-ml-get-parents)
           (--map (org-ml-get-property :begin it)))
      => '(1)
      (->> (org-ml-parse-this-subtree)
           (org-ml-headline-get-subheadlines)
           (car)
           (org-ml-headline-get-subheadlines)
           (car)
           (org-ml-get-parents)
           (--map (org-ml-get-property :begin it)))
      => '(1 7 14))

    (defexamples-content org-ml-remove-parent
      nil
      (:buffer "one")
      (:comment "This is actually a paragraph node, but parsing the object"
                "will directly return a plain-text node with the :parent"
                "pointing to the paragraph")
      (->> (org-ml-parse-this-object)
           (org-ml-remove-parent))
      => "one"

    (defexamples-content org-ml-remove-parents
      nil
      (:buffer "one")
      (->> (org-ml-parse-this-element)
           (org-ml-remove-parents))
      => '(paragraph
           (:begin 1 :end 4 :contents-begin 1 :contents-end 4 :post-blank 0 :post-affiliated 1 :parent nil)
           "one"))

      (:buffer "* headline")
      (->> (org-ml-parse-this-element)
           (org-ml-remove-parents))
      => '(headline
           (:raw-value "headline" :begin 1 :end 11 :pre-blank 0 :contents-begin nil :contents-end nil :level 1 :priority nil :tags nil :todo-keyword nil :todo-type nil :post-blank 0 :footnote-section-p nil :archivedp nil :commentedp nil :post-affiliated 1 :title ("headline") :parent nil))

      (:buffer "- tag :: thingy")
      (->> (org-ml-parse-this-item)
           (org-ml-remove-parents))
      => '(item
           (:bullet "- " :begin 1 :end 16 :contents-begin 10 :contents-end 16 :checkbox nil :counter nil :structure ((1 0 "- " nil nil "tag" 16)) :pre-blank 0 :post-blank 0 :post-affiliated 1 :tag ("tag") :parent nil)
           (paragraph
            (:begin 10 :end 16 :contents-begin 10 :contents-end 16 :post-blank 0 :post-affiliated 10 :parent nil)
            "thingy"))

      :begin-hidden

      ;; for some reason the timestamps in planning don't have parents
      (:buffer "* headline"
               "SCHEDULED: <2021-08-27 Fri>")
      (->> (org-ml-parse-this-element)
           (org-ml-headline-get-planning)
           (org-ml-get-property :scheduled)
           ;; (org-ml-remove-parents)
           (org-ml-get-property :parent)
      => nil)

      ;; same thing with clocks...
      (:buffer "* headline"
               "CLOCK: [2021-08-27 Fri 23:31]--[2021-08-28 Sat 00:45] =>  1:14")
      (->> (org-ml-parse-this-element)
           (org-ml-headline-get-logbook-clocks nil)
           (car)
           (org-ml-get-property :value)
           (org-ml-get-property :parent)
      => nil)

      :end-hidden
    ))

  (def-example-subgroup "Clock"
    nil

    (defexamples-content org-ml-clock-is-running
      nil
      (:buffer "CLOCK: [2019-01-01 Tue 00:00]")
      (->> (org-ml-parse-this-element)
           (org-ml-clock-is-running))
      => t
      (:buffer "CLOCK: [2019-01-01 Tue 00:00]--[2019-01-02 Wed 00:00] => 24:00")
      (->> (org-ml-parse-this-element)
           (org-ml-clock-is-running))
      => nil))

  (def-example-subgroup "Entity"
    nil

    (defexamples-content org-ml-entity-get-replacement
      nil
      (:buffer "\\pi{}")
      (->> (org-ml-parse-this-object)
           (org-ml-entity-get-replacement :latex))
      => "\\pi"
      (->> (org-ml-parse-this-object)
           (org-ml-entity-get-replacement :latex-math-p))
      => t
      (->> (org-ml-parse-this-object)
           (org-ml-entity-get-replacement :html))
      => "&pi;"
      (->> (org-ml-parse-this-object)
           (org-ml-entity-get-replacement :ascii))
      => "pi"
      (->> (org-ml-parse-this-object)
           (org-ml-entity-get-replacement :latin1))
      => "pi"
      (->> (org-ml-parse-this-object)
           (org-ml-entity-get-replacement :utf-8))
      => "π"))

  (def-example-subgroup "Headline"
    nil

    (defexamples-content org-ml-headline-set-title!
      nil
      (:buffer "* really impressive title")
      (->> (org-ml-parse-this-headline)
           (org-ml-headline-set-title! "really *impressive* title" '(2 3))
           (org-ml-to-trimmed-string))
      => "* really *impressive* title [2/3]")

    (defexamples-content org-ml-headline-is-done
      nil
      (:buffer "* TODO darn")
      (->> (org-ml-parse-this-headline)
           (org-ml-headline-is-done))
      => nil
      (:buffer "* DONE yay")
      (->> (org-ml-parse-this-headline)
           (org-ml-headline-is-done))
      => t)

    (defexamples-content org-ml-headline-has-tag
      nil
      (:buffer "* dummy")
      (->> (org-ml-parse-this-headline)
           (org-ml-headline-has-tag "tmsu"))
      => nil
      (:buffer "* dummy                  :tmsu:")
      (->> (org-ml-parse-this-headline)
           (org-ml-headline-has-tag "tmsu"))
      => t)

    (defexamples-content org-ml-headline-get-statistics-cookie
      nil
      (:buffer "* statistically significant [10/10]")
      (->> (org-ml-parse-this-headline)
           (org-ml-headline-get-statistics-cookie)
           (org-ml-to-string))
      => "[10/10]"
      (:buffer "* not statistically significant")
      (->> (org-ml-parse-this-headline)
           (org-ml-headline-get-statistics-cookie))
      => nil)

    ;; TODO add the shortcut version title setter

    )


  ;; TODO add inlinetask

  (def-example-subgroup "Item"
    nil

    ;; TODO add shortcut tag setter

    (defexamples-content org-ml-item-toggle-checkbox
      nil
      (:buffer "- [ ] one")
      (->> (org-ml-parse-this-item)
           (org-ml-item-toggle-checkbox)
           (org-ml-to-trimmed-string))
      => "- [X] one"
      (:buffer "- [-] one")
      (:comment "Ignore trans state checkboxes")
      (->> (org-ml-parse-this-item)
           (org-ml-item-toggle-checkbox)
           (org-ml-to-trimmed-string))
      => "- [-] one"
      (:buffer "- one")
      (:comment "Do nothing if there is no checkbox")
      (->> (org-ml-parse-this-item)
           (org-ml-item-toggle-checkbox)
           (org-ml-to-trimmed-string))
      => "- one"))

  (def-example-subgroup "Planning"
    nil

    (defexamples-content org-ml-planning-set-timestamp!
      nil
      (:buffer "* dummy"
               "CLOSED: [2019-01-01 Tue]")
      (:comment "Change an existing timestamp in planning")
      (->> (org-ml-parse-this-headline)
           (org-ml-headline-get-planning)
           (org-ml-planning-set-timestamp!
            :closed '(2019 1 2 &warning all 1 day &repeater cumulate 2 month))
           (org-ml-to-trimmed-string))
      => "CLOSED: [2019-01-02 Wed +2m -1d]"
      (:comment "Add a new timestamp and remove another")
      (->> (org-ml-parse-this-headline)
           (org-ml-headline-get-planning)
           (org-ml-planning-set-timestamp!
            :deadline '(2112 1 1))
           (org-ml-planning-set-timestamp!
            :closed nil)
           (org-ml-to-trimmed-string))
      => "DEADLINE: <2112-01-01 Fri>"))

  (def-example-subgroup "Statistics Cookie"
    nil
    (defexamples-content org-ml-statistics-cookie-is-complete
      nil
      (:buffer "* statistically significant [10/10]")
      (->> (org-ml-parse-this-headline)
           (org-ml-headline-get-statistics-cookie)
           (org-ml-statistics-cookie-is-complete))
      => t
      (:buffer "* statistically significant [1/10]")
      (->> (org-ml-parse-this-headline)
           (org-ml-headline-get-statistics-cookie)
           (org-ml-statistics-cookie-is-complete))
      => nil
      (:buffer "* statistically significant [100%]")
      (->> (org-ml-parse-this-headline)
           (org-ml-headline-get-statistics-cookie)
           (org-ml-statistics-cookie-is-complete))
      => t
      (:buffer "* statistically significant [33%]")
      (->> (org-ml-parse-this-headline)
           (org-ml-headline-get-statistics-cookie)
           (org-ml-statistics-cookie-is-complete))
      => nil))

  ;; TODO add these
  (def-example-subgroup "Timestamp (Auxiliary)"
    "Functions to work with timestamp data"
    
    (defexamples-content org-ml-time-is-long
      nil)

    (defexamples-content org-ml-time-to-unixtime
      nil)

    (defexamples-content org-ml-unixtime-to-time-long
      nil)

    (defexamples-content org-ml-unixtime-to-time-short
      nil))

  (def-example-subgroup "Timestamp (Standard)"
    nil

    (defexamples-content org-ml-timestamp-get-start-time
      nil
      (:buffer "[2019-01-01 Tue]")
      (->> (org-ml-parse-this-object)
           (org-ml-timestamp-get-start-time))
      => '(2019 1 1 nil nil)
      (:buffer "[2019-01-01 Tue]--[2019-01-02 Wed]")
      (->> (org-ml-parse-this-object)
           (org-ml-timestamp-get-start-time))
      => '(2019 1 1 nil nil)
      (:buffer "[2019-01-01 Tue 00:00-12:00]")
      (->> (org-ml-parse-this-object)
           (org-ml-timestamp-get-start-time))
      => '(2019 1 1 0 0))

    (defexamples-content org-ml-timestamp-get-end-time
      nil
      (:buffer "[2019-01-01 Tue]")
      (->> (org-ml-parse-this-object)
           (org-ml-timestamp-get-end-time))
      => nil
      (:buffer "[2019-01-01 Tue]--[2019-01-02 Wed]")
      (->> (org-ml-parse-this-object)
           (org-ml-timestamp-get-end-time))
      => '(2019 1 2 nil nil)
      (:buffer "[2019-01-01 Tue]--[2019-01-01 Tue]")
      (->> (org-ml-parse-this-object)
           (org-ml-timestamp-get-end-time))
      => '(2019 1 1 nil nil)
      (:buffer "[2019-01-01 Tue 00:00-12:00]")
      (->> (org-ml-parse-this-object)
           (org-ml-timestamp-get-end-time))
      => '(2019 1 1 12 0))

    (defexamples-content org-ml-timestamp-get-range
      nil
      (:buffer "[2019-01-01 Tue]")
      (->> (org-ml-parse-this-object)
           (org-ml-timestamp-get-range))
      => 0
      (:buffer "[2019-01-01 Tue]--[2019-01-02 Wed]")
      (->> (org-ml-parse-this-object)
           (org-ml-timestamp-get-range))
      => 86400
      (:buffer "[2019-01-01 Tue 00:00-12:00]")
      (->> (org-ml-parse-this-object)
           (org-ml-timestamp-get-range))
      => 43200)

    (defexamples-content org-ml-timestamp-is-active
      nil
      (:buffer "<2019-01-01 Tue>")
      (->> (org-ml-parse-this-object)
           (org-ml-timestamp-is-active))
      => t
      (:buffer "[2019-01-01 Tue]")
      (->> (org-ml-parse-this-object)
           (org-ml-timestamp-is-active))
      => nil)

    (defexamples-content org-ml-timestamp-is-ranged
      nil
      (:buffer "[2019-01-01 Tue]--[2019-01-02 Wed]")
      (->> (org-ml-parse-this-object)
           (org-ml-timestamp-is-ranged))
      => t
      (:buffer "[2019-01-01 Tue 00:00-12:00]")
      (->> (org-ml-parse-this-object)
           (org-ml-timestamp-is-ranged))
      => t
      (:buffer "[2019-01-01 Tue]")
      (->> (org-ml-parse-this-object)
           (org-ml-timestamp-is-ranged))
      => nil)

    (defexamples-content org-ml-timestamp-range-contains-p
      nil
      (:buffer "[2019-01-01 Tue 00:00]")
      (let ((ut (org-ml-time-to-unixtime '(2019 1 1 0 0))))
        (->> (org-ml-parse-this-object)
             (org-ml-timestamp-range-contains-p ut)))
      => t
      (let ((ut (org-ml-time-to-unixtime '(2019 1 1 0 30))))
        (->> (org-ml-parse-this-object)
             (org-ml-timestamp-range-contains-p ut)))
      => nil
      (:buffer "[2019-01-01 Tue 00:00-01:00]")
      (let ((ut (org-ml-time-to-unixtime '(2019 1 1 0 30))))
        (->> (org-ml-parse-this-object)
             (org-ml-timestamp-range-contains-p ut)))
      => t)

    (defexamples-content org-ml-timestamp-set-collapsed
      nil
      (:buffer "[2019-01-01 Tue 12:00-13:00]")
      (->> (org-ml-parse-this-object)
           (org-ml-timestamp-set-collapsed nil)
           (org-ml-to-trimmed-string))
      => "[2019-01-01 Tue 12:00]--[2019-01-01 Tue 13:00]"
      (:buffer "[2019-01-01 Tue 12:00-13:00]")
      (->> (org-ml-parse-this-object)
           (org-ml-timestamp-set-collapsed nil)
           (org-ml-timestamp-set-collapsed t)
           (org-ml-to-trimmed-string))
      => "[2019-01-01 Tue 12:00-13:00]"
      (:buffer "[2019-01-01 Tue 12:00]")
      (->> (org-ml-parse-this-object)
           (org-ml-timestamp-set-collapsed nil)
           (org-ml-to-trimmed-string))
      => "[2019-01-01 Tue 12:00]"
      (:buffer "[2019-01-01 Tue]--[2019-01-02 Wed]")
      (->> (org-ml-parse-this-object)
           (org-ml-timestamp-set-collapsed nil)
           (org-ml-to-trimmed-string))
      => "[2019-01-01 Tue]--[2019-01-02 Wed]")

    (defexamples-content org-ml-timestamp-get-warning
      nil
      (:buffer "[2019-01-01 Tue 12:00]")
      (->> (org-ml-parse-this-object)
           (org-ml-timestamp-get-warning))
      => '(nil nil nil)
      (:buffer "[2019-01-01 Tue 12:00 -1d]")
      (->> (org-ml-parse-this-object)
           (org-ml-timestamp-get-warning))
      => '(all 1 day))

    (defexamples-content org-ml-timestamp-set-warning
      nil
      (:buffer "[2019-01-01 Tue 12:00]")
      (->> (org-ml-parse-this-object)
           (org-ml-timestamp-set-warning '(nil nil nil))
           (org-ml-to-string))
      => "[2019-01-01 Tue 12:00]"
      (->> (org-ml-parse-this-object)
           (org-ml-timestamp-set-warning '(all 1 day))
           (org-ml-to-string))
      => "[2019-01-01 Tue 12:00 -1d]"
      (:buffer "[2019-01-01 Tue 12:00]")
      (->> (org-ml-parse-this-object)
           (org-ml-timestamp-set-warning nil)
           (org-ml-to-string))
      => "[2019-01-01 Tue 12:00]"
      (->> (org-ml-parse-this-object)
           (org-ml-timestamp-set-warning '(all 1 year))
           (org-ml-to-string))
      => "[2019-01-01 Tue 12:00 -1y]")

    (defexamples-content org-ml-timestamp-map-warning
      nil
      (:buffer "[2019-01-01 Tue 12:00 -1d]")
      (->> (org-ml-parse-this-object)
           (org-ml-timestamp-map-warning* (-let (((y v u) it)) `(,y ,(1+ v) ,u)))
           (org-ml-to-string))
      => "[2019-01-01 Tue 12:00 -2d]")

    (defexamples-content org-ml-timestamp-get-repeater
      nil
      (:buffer "[2019-01-01 Tue 12:00]")
      (->> (org-ml-parse-this-object)
           (org-ml-timestamp-get-repeater))
      => '(nil nil nil)
      (:buffer "[2019-01-01 Tue 12:00 +1d]")
      (->> (org-ml-parse-this-object)
           (org-ml-timestamp-get-repeater))
      => '(cumulate 1 day)
      (:buffer "[2019-01-01 Tue 12:00 +1d/3d]")
      (->> (org-ml-parse-this-object)
           (org-ml-timestamp-get-repeater))
      => '(cumulate 1 day)
      (:buffer "[2019-01-01 Tue 12:00 +1d/3d]")
      (let ((org-ml-parse-habits t))
        (->> (org-ml-parse-this-object)
             (org-ml-timestamp-get-repeater)))
      => '(cumulate 1 day 3 day))

    (defexamples-content org-ml-timestamp-set-repeater
      nil
      (:buffer "[2019-01-01 Tue 12:00]")
      (->> (org-ml-parse-this-object)
           (org-ml-timestamp-set-repeater nil)
           (org-ml-to-string))
      => "[2019-01-01 Tue 12:00]"
      (->> (org-ml-parse-this-object)
           (org-ml-timestamp-set-repeater '(restart 1 day))
           (org-ml-to-string))
      => "[2019-01-01 Tue 12:00 .+1d]"
      (let ((org-ml-parse-habits t))
        (->> (org-ml-parse-this-object)
             (org-ml-timestamp-set-repeater '(restart 1 day nil nil))
             (org-ml-to-string)))
      => "[2019-01-01 Tue 12:00 .+1d]"
      (let ((org-ml-parse-habits t))
        (->> (org-ml-parse-this-object)
             (org-ml-timestamp-set-repeater '(restart 1 day 3 day))
             (org-ml-to-string)))
      => "[2019-01-01 Tue 12:00 .+1d/3d]"
      :begin-hidden
      (:buffer "[2019-01-01 Tue 12:00 .+1d]")
      (->> (org-ml-parse-this-object)
           (org-ml-timestamp-set-repeater nil)
           (org-ml-to-string))
      => "[2019-01-01 Tue 12:00]"
      (->> (org-ml-parse-this-object)
           (org-ml-timestamp-set-repeater '(cumulate 1 day))
           (org-ml-to-string))
      => "[2019-01-01 Tue 12:00 +1d]"
      (let ((org-ml-parse-habits t))
        (->> (org-ml-parse-this-object)
             (org-ml-timestamp-set-repeater '(cumulate 1 day nil nil))
             (org-ml-to-string)))
      => "[2019-01-01 Tue 12:00 +1d]"
      (let ((org-ml-parse-habits t))
        (->> (org-ml-parse-this-object)
             (org-ml-timestamp-set-repeater '(cumulate 1 day 3 day))
             (org-ml-to-string)))
      => "[2019-01-01 Tue 12:00 +1d/3d]"
      (:buffer "[2019-01-01 Tue 12:00 .+1d/3d]")
      (->> (org-ml-parse-this-object)
           (org-ml-timestamp-set-repeater nil)
           (org-ml-to-string))
      => "[2019-01-01 Tue 12:00]"
      (->> (org-ml-parse-this-object)
           (org-ml-timestamp-set-repeater '(cumulate 1 day))
           (org-ml-to-string))
      => "[2019-01-01 Tue 12:00 +1d]"
      (let ((org-ml-parse-habits t))
        (->> (org-ml-parse-this-object)
             (org-ml-timestamp-set-repeater '(cumulate 1 day nil nil))
             (org-ml-to-string)))
      => "[2019-01-01 Tue 12:00 +1d]"
      (let ((org-ml-parse-habits t))
        (->> (org-ml-parse-this-object)
             (org-ml-timestamp-set-repeater '(cumulate 1 day 2 day))
             (org-ml-to-string)))
      => "[2019-01-01 Tue 12:00 +1d/2d]"
      (->> (org-ml-parse-this-object)
           (org-ml-timestamp-set-repeater '(cumulate 1 day nil nil))
           (org-ml-to-string))
      !!> error
      (let ((org-ml-parse-habits t))
        (->> (org-ml-parse-this-object)
             (org-ml-timestamp-set-repeater '(cumulate 1 day))
             (org-ml-to-string)))
      !!> error
      :end-hidden)

    (defexamples-content org-ml-timestamp-map-repeater
      nil
      (:buffer "[2019-01-01 Tue 12:00 +1d]")
      (->> (org-ml-parse-this-object)
           (org-ml-timestamp-map-repeater* (-let (((y v u) it)) `(,y ,(1+ v) ,u)))
           (org-ml-to-string))
      => "[2019-01-01 Tue 12:00 +2d]")

    (defexamples-content org-ml-timestamp-set-start-time
      nil
      (:buffer "[2019-01-02 Wed]")
      (:comment "If not a range this will turn into a range by moving only the start time.")
      (->> (org-ml-parse-this-object)
           (org-ml-timestamp-set-start-time '(2019 1 1))
           (org-ml-to-trimmed-string))
      => "[2019-01-01 Tue]--[2019-01-02 Wed]"
      (:comment "Set a different time with different precision.")
      (->> (org-ml-parse-this-object)
           (org-ml-timestamp-set-start-time '(2019 1 1 10 0))
           (org-ml-to-trimmed-string))
      => "[2019-01-01 Tue 10:00]--[2019-01-02 Wed]"
      (:buffer "[2019-01-02 Wed 12:00]")
      (:comment "If not a range and set within a day, use short format")
      (->> (org-ml-parse-this-object)
           (org-ml-timestamp-set-start-time '(2019 1 1 0 0))
           (org-ml-to-trimmed-string))
      => "[2019-01-01 Tue 00:00-12:00]"
      :begin-hidden
      (:buffer "[2019-01-02 Wed 12:00 +1d]")
      (->> (org-ml-parse-this-object)
           (org-ml-timestamp-set-start-time '(2019 1 1 0 0))
           (org-ml-to-trimmed-string))
      => "[2019-01-01 Tue 00:00-12:00 +1d]"
      ;; disabling habit parser will obliterate the habit bit
      (:buffer "[2019-01-02 Wed 12:00 +1d/3d]")
      (->> (org-ml-parse-this-object)
           (org-ml-timestamp-set-start-time '(2019 1 1 0 0))
           (org-ml-to-trimmed-string))
      => "[2019-01-01 Tue 00:00-12:00 +1d]"
      ;; and enabling will preserve it
      (:buffer "[2019-01-02 Wed 12:00 +1d/3d]")
      (let ((org-ml-parse-habits t))
        (->> (org-ml-parse-this-object)
             (org-ml-timestamp-set-start-time '(2019 1 1 0 0))
             (org-ml-to-trimmed-string)))
      => "[2019-01-01 Tue 00:00-12:00 +1d/3d]"
      :end-hidden)

    (defexamples-content org-ml-timestamp-set-end-time
      nil
      (:buffer "[2019-01-01 Tue]")
      (:comment "Add the end time")
      (->> (org-ml-parse-this-object)
           (org-ml-timestamp-set-end-time '(2019 1 2))
           (org-ml-to-trimmed-string))
      => "[2019-01-01 Tue]--[2019-01-02 Wed]"
      (:buffer "[2019-01-01 Tue]--[2019-01-02 Wed]")
      (:comment "Remove the end time")
      (->> (org-ml-parse-this-object)
           (org-ml-timestamp-set-end-time nil)
           (org-ml-to-trimmed-string))
      => "[2019-01-01 Tue]"
      (:buffer "[2019-01-01 Tue 12:00]")
      (:comment "Use short range format")
      (->> (org-ml-parse-this-object)
           (org-ml-timestamp-set-end-time '(2019 1 1 13 0))
           (org-ml-to-trimmed-string))
      => "[2019-01-01 Tue 12:00-13:00]")

    (defexamples-content org-ml-timestamp-set-single-time
      nil
      (:buffer "[2019-01-01 Tue]")
      (:comment "Don't make a range")
      (->> (org-ml-parse-this-object)
           (org-ml-timestamp-set-single-time '(2019 1 2))
           (org-ml-to-trimmed-string))
      => "[2019-01-02 Wed]"
      (:buffer "[2019-01-01 Tue]--[2019-01-02 Wed]")
      (:comment "Output is not a range despite input being ranged")
      (->> (org-ml-parse-this-object)
           (org-ml-timestamp-set-single-time '(2019 1 3))
           (org-ml-to-trimmed-string))
      => "[2019-01-03 Thu]")

    (defexamples-content org-ml-timestamp-set-double-time
      nil
      (:buffer "[2019-01-01 Tue]")
      (:comment "Make a range")
      (->> (org-ml-parse-this-object)
           (org-ml-timestamp-set-double-time '(2019 1 2) '(2019 1 3))
           (org-ml-to-trimmed-string))
      => "[2019-01-02 Wed]--[2019-01-03 Thu]"
      (:buffer "[2019-01-01 Tue]--[2019-01-03 Wed]")
      (->> (org-ml-parse-this-object)
           (org-ml-timestamp-set-double-time '(2019 1 4) '(2019 1 5))
           (org-ml-to-trimmed-string))
      => "[2019-01-04 Fri]--[2019-01-05 Sat]"
      (:buffer "[2019-01-01 Tue]--[2019-01-03 Wed]")
      (->> (org-ml-parse-this-object)
           (org-ml-timestamp-set-double-time '(2019 1 1 0 0) '(2019 1 1 1 0))
           (org-ml-to-trimmed-string))
      => "[2019-01-01 Tue 00:00-01:00]")

    (defexamples-content org-ml-timestamp-set-range
      nil
      (:buffer "[2019-01-01 Tue]")
      (:comment "Use days as the unit for short format")
      (->> (org-ml-parse-this-object)
           (org-ml-timestamp-set-range 1)
           (org-ml-to-trimmed-string))
      => "[2019-01-01 Tue]--[2019-01-02 Wed]"
      (:buffer "[2019-01-01 Tue 00:00]")
      (:comment "Use minutes as the unit for long format")
      (->> (org-ml-parse-this-object)
           (org-ml-timestamp-set-range 3)
           (org-ml-to-trimmed-string))
      => "[2019-01-01 Tue 00:00-00:03]"
      (:buffer "[2019-01-01 Tue]--[2019-01-03 Wed]")
      (:comment "Set range to 0 to remove end time")
      (->> (org-ml-parse-this-object)
           (org-ml-timestamp-set-range 0)
           (org-ml-to-trimmed-string))
      => "[2019-01-01 Tue]")

    (defexamples-content org-ml-timestamp-set-active
      nil
      (:buffer "[2019-01-01 Tue]")
      (->> (org-ml-parse-this-object)
           (org-ml-timestamp-set-active t)
           (org-ml-to-trimmed-string))
      => "<2019-01-01 Tue>"
      (:buffer "<2019-01-01 Tue>")
      (->> (org-ml-parse-this-object)
           (org-ml-timestamp-set-active nil)
           (org-ml-to-trimmed-string))
      => "[2019-01-01 Tue]")

    (defexamples-content org-ml-timestamp-shift
      nil
      (:buffer "[2019-01-01 Tue 12:00]")
      (:comment "Change each unit, and wrap around to the next unit as needed.")
      (->> (org-ml-parse-this-object)
           (org-ml-timestamp-shift 30 'minute)
           (org-ml-to-trimmed-string))
      => "[2019-01-01 Tue 12:30]"
      (->> (org-ml-parse-this-object)
           (org-ml-timestamp-shift 13 'month)
           (org-ml-to-trimmed-string))
      => "[2020-02-01 Sat 12:00]"
      :begin-hidden
      (->> (org-ml-parse-this-object)
           (org-ml-timestamp-shift 60 'minute)
           (org-ml-to-trimmed-string))
      => "[2019-01-01 Tue 13:00]"
      (->> (org-ml-parse-this-object)
           (org-ml-timestamp-shift 1 'hour)
           (org-ml-to-trimmed-string))
      => "[2019-01-01 Tue 13:00]"
      (->> (org-ml-parse-this-object)
           (org-ml-timestamp-shift 1 'day)
           (org-ml-to-trimmed-string))
      => "[2019-01-02 Wed 12:00]"
      (->> (org-ml-parse-this-object)
           (org-ml-timestamp-shift 31 'day)
           (org-ml-to-trimmed-string))
      => "[2019-02-01 Fri 12:00]"
      (->> (org-ml-parse-this-object)
           (org-ml-timestamp-shift 1 'month)
           (org-ml-to-trimmed-string))
      => "[2019-02-01 Fri 12:00]"
      (->> (org-ml-parse-this-object)
           (org-ml-timestamp-shift 1 'year)
           (org-ml-to-trimmed-string))
      => "[2020-01-01 Wed 12:00]"
      (->> (org-ml-parse-this-object)
           (org-ml-timestamp-shift 0 'year)
           (org-ml-to-trimmed-string))
      => "[2019-01-01 Tue 12:00]"
      :end-hidden
      (:buffer "[2019-01-01 Tue]")
      (:comment "Error when shifting hour/minute in short format")
      (->> (org-ml-parse-this-object)
           (org-ml-timestamp-shift 30 'minute)
           (org-ml-to-trimmed-string))
      !!> arg-type-error
      :begin-hidden
      (->> (org-ml-parse-this-object)
           (org-ml-timestamp-shift 30 'hour)
           (org-ml-to-trimmed-string))
      !!> arg-type-error
      :end-hidden)

    (defexamples-content org-ml-timestamp-shift-start
      nil
      (:buffer "[2019-01-01 Tue 12:00]")
      (:comment "If not a range, change start time and leave implicit end time.")
      (->> (org-ml-parse-this-object)
           (org-ml-timestamp-shift-start -1 'year)
           (org-ml-to-trimmed-string))
      => "[2018-01-01 Mon 12:00]--[2019-01-01 Tue 12:00]"
      (->> (org-ml-parse-this-object)
           (org-ml-timestamp-shift-start -1 'hour)
           (org-ml-to-trimmed-string))
      => "[2019-01-01 Tue 11:00-12:00]"
      (:buffer "[2019-01-01 Tue]--[2019-01-03 Thu]")
      (:comment "Change only start time if a range")
      (->> (org-ml-parse-this-object)
           (org-ml-timestamp-shift-start 1 'day)
           (org-ml-to-trimmed-string))
      => "[2019-01-02 Wed]--[2019-01-03 Thu]")

    (defexamples-content org-ml-timestamp-shift-end
      nil
      (:buffer "[2019-01-01 Tue]")
      (:comment "Shift implicit end time if not a range.")
      (->> (org-ml-parse-this-object)
           (org-ml-timestamp-shift-end 1 'day)
           (org-ml-to-trimmed-string))
      => "[2019-01-01 Tue]--[2019-01-02 Wed]"
      (:buffer "[2019-01-01 Tue]--[2019-01-02 Wed]")
      (:comment "Move only the second time if a range.")
      (->> (org-ml-parse-this-object)
           (org-ml-timestamp-shift-end 1 'day)
           (org-ml-to-trimmed-string))
      => "[2019-01-01 Tue]--[2019-01-03 Thu]")

    (defexamples-content org-ml-timestamp-toggle-active
      nil
      (:buffer "[2019-01-01 Tue]")
      (->> (org-ml-parse-this-object)
           (org-ml-timestamp-toggle-active)
           (org-ml-to-trimmed-string))
      => "<2019-01-01 Tue>"
      :begin-hidden
      (->> (org-ml-parse-this-object)
           (org-ml-timestamp-toggle-active)
           (org-ml-timestamp-toggle-active)
           (org-ml-to-trimmed-string))
      => "[2019-01-01 Tue]"
      :end-hidden
      (:buffer "<2019-01-01 Tue>--<2019-01-02 Wed>")
      (->> (org-ml-parse-this-object)
           (org-ml-timestamp-toggle-active)
           (org-ml-to-trimmed-string))
      => "[2019-01-01 Tue]--[2019-01-02 Wed]"
      :begin-hidden
      (->> (org-ml-parse-this-object)
           (org-ml-timestamp-toggle-active)
           (org-ml-timestamp-toggle-active)
           (org-ml-to-trimmed-string))
      => "<2019-01-01 Tue>--<2019-01-02 Wed>"
      :end-hidden)

    (defexamples-content org-ml-timestamp-truncate
      nil
      (:buffer "[2019-01-01 Tue]--[2019-01-02 Wed]")
      (->> (org-ml-parse-this-object)
           (org-ml-timestamp-truncate)
           (org-ml-to-trimmed-string))
      => "[2019-01-01 Tue]--[2019-01-02 Wed]"
      (:buffer "[2019-01-01 Tue 12:00]--[2019-01-02 Wed 13:00]")
      (->> (org-ml-parse-this-object)
           (org-ml-timestamp-truncate)
           (org-ml-to-trimmed-string))
      => "[2019-01-01 Tue]--[2019-01-02 Wed]")

    (defexamples-content org-ml-timestamp-truncate-start
      nil
      (:buffer "[2019-01-01 Tue 12:00]")
      (->> (org-ml-parse-this-object)
           (org-ml-timestamp-truncate-start)
           (org-ml-to-trimmed-string))
      => "[2019-01-01 Tue]"
      (:buffer "[2019-01-01 Tue 12:00]--[2019-01-02 Wed 12:00]")
      (->> (org-ml-parse-this-object)
           (org-ml-timestamp-truncate-start)
           (org-ml-to-trimmed-string))
      => "[2019-01-01 Tue]--[2019-01-02 Wed 12:00]"
      (:buffer "[2019-01-01 Tue]")
      (->> (org-ml-parse-this-object)
           (org-ml-timestamp-truncate-start)
           (org-ml-to-trimmed-string))
      => "[2019-01-01 Tue]")

    (defexamples-content org-ml-timestamp-truncate-end
      nil
      (:buffer "[2019-01-01 Tue]--[2019-01-02 Wed]")
      (->> (org-ml-parse-this-object)
           (org-ml-timestamp-truncate-end)
           (org-ml-to-trimmed-string))
      => "[2019-01-01 Tue]--[2019-01-02 Wed]"
      (:buffer "[2019-01-01 Tue 12:00]--[2019-01-02 Wed 13:00]")
      (->> (org-ml-parse-this-object)
           (org-ml-timestamp-truncate-end)
           (org-ml-to-trimmed-string))
      => "[2019-01-01 Tue 12:00]--[2019-01-02 Wed]"
      (:buffer "[2019-01-01 Tue 12:00]")
      (->> (org-ml-parse-this-object)
           (org-ml-timestamp-truncate-end)
           (org-ml-to-trimmed-string))
      => "[2019-01-01 Tue 12:00]"))

  (def-example-subgroup "Timestamp (diary)"
    nil

    (defexamples-content org-ml-timestamp-diary-set-value
      nil
      (:buffer "<%%(diary-float t 4 2)>")
      (->> (org-ml-parse-this-object)
           (org-ml-timestamp-diary-set-value '(diary-float 1 3 2))
           (org-ml-to-string))
      => "<%%(diary-float 1 3 2)>"))

  (def-example-subgroup "Affiliated Keywords"
    nil

    (defexamples-content org-ml-get-affiliated-keyword
      nil
      (:buffer "#+name: name"
               "#+attr_foo: bar"
               "#+attr_foo: BAR"
               "#+plot: poo"
               "#+results[hash]: res"
               "#+header: h1"
               "#+begin_src"
               "echo test for echo"
               "#+end_src")
      (:comment "Simply return NAME and PLOT")
      (->> (org-ml-parse-this-element)
           (org-ml-get-affiliated-keyword :name))
      => "name"
      (->> (org-ml-parse-this-element)
           (org-ml-get-affiliated-keyword :plot))
      => "poo"
      (:comment "Attribute FOO has multiple entries so return a list of all")
      (->> (org-ml-parse-this-element)
           (org-ml-get-affiliated-keyword :attr_foo))
      ;; TODO why are these reversed?
      => '("BAR" "bar")
      (:comment "HEADER may have multiple values so return a singleton list")
      (->> (org-ml-parse-this-element)
           (org-ml-get-affiliated-keyword :header))
      => '("h1")
      (:comment "RESULTS returns a cons cell with the optional part")
      (->> (org-ml-parse-this-element)
           (org-ml-get-affiliated-keyword :results))
      => '("res" . "hash")
      )

    (defexamples-content org-ml-set-affiliated-keyword
      nil

      (:buffer "short paragraph")
      (->> (org-ml-parse-this-element)
           (org-ml-set-affiliated-keyword :name "foo")
           (org-ml-to-trimmed-string))
      => (:result "#+name: foo"
                  "short paragraph")
      (->> (org-ml-parse-this-element)
           (org-ml-set-affiliated-keyword :attr_bar '("foo"))
           (org-ml-to-trimmed-string))
      => (:result "#+attr_bar: foo"
                  "short paragraph")
      (->> (org-ml-parse-this-element)
           (org-ml-set-affiliated-keyword :header '("h1" "h2"))
           (org-ml-to-trimmed-string))
      => (:result "#+header: h2"
                  "#+header: h1"
                  "short paragraph")
      (->> (org-ml-parse-this-element)
           (org-ml-set-affiliated-keyword :results '("foo" . "bar"))
           (org-ml-to-trimmed-string))
      => (:result "#+results[bar]: foo"
                  "short paragraph")

      (:buffer "#+name: deleteme"
               "short paragraph")
      (->> (org-ml-parse-this-element)
           (org-ml-set-affiliated-keyword :name nil)
           (org-ml-to-trimmed-string))
      => "short paragraph")

    (defexamples-content org-ml-map-affiliated-keyword
      nil

      (:buffer "#+name: foo"
               "short paragraph")
      (->> (org-ml-parse-this-element)
           (org-ml-map-affiliated-keyword :name #'upcase)
           (org-ml-to-trimmed-string))
      => (:result "#+name: FOO"
                  "short paragraph")

      (:buffer "#+header: foo"
               "short paragraph")
      (->> (org-ml-parse-this-element)
           (org-ml-map-affiliated-keyword* :header (cons "bar" it))
           (org-ml-to-trimmed-string))
      => (:result "#+header: foo"
                  "#+header: bar"
                  "short paragraph"))

    (defexamples-content org-ml-set-caption!
      nil

      (:buffer "short paragraph")
      (->> (org-ml-parse-this-element)
           (org-ml-set-caption! "cap")
           (org-ml-to-trimmed-string))
      => (:result "#+caption: cap"
                  "short paragraph")
      (->> (org-ml-parse-this-element)
           (org-ml-set-caption! '("foo" "cap"))
           (org-ml-to-trimmed-string))
      => (:result "#+caption[foo]: cap"
                  "short paragraph")
      (->> (org-ml-parse-this-element)
           (org-ml-set-caption! '("foo" "cap"))
           (org-ml-to-trimmed-string))
      => (:result "#+caption[foo]: cap"
                  "short paragraph")
      (->> (org-ml-parse-this-element)
           (org-ml-set-caption! '(("foo" "cap") ("FOO" "CAP")))
           (org-ml-to-trimmed-string))
      => (:result "#+caption[FOO]: CAP"
                  "#+caption[foo]: cap"
                  "short paragraph")
      
      (:buffer "#+caption: cap"
               "short paragraph")
      (->> (org-ml-parse-this-element)
           (org-ml-set-caption! nil)
           (org-ml-to-trimmed-string))
      => "short paragraph")))

(def-example-group "Branch/Child Manipulation"
  "Set, get, and map the children of branch nodes."

  (def-example-subgroup "Polymorphic"
    nil

    (defexamples-content org-ml-children-contain-point
      nil
      (:buffer "* headline"
               "findme")
      (->> (org-ml-parse-this-headline)
           (org-ml-children-contain-point 2))
      => nil
      (->> (org-ml-parse-this-headline)
           (org-ml-children-contain-point 15))
      => t)

    (defexamples-content org-ml-get-children
      nil

      (:buffer "/this/ is a *paragraph*")
      (:comment "Return child nodes for branch nodes")
      (->> (org-ml-parse-this-element)
           (org-ml-get-children)
           (-map #'org-ml-get-type))
      => '(italic plain-text bold)

      (:buffer "* headline")
      (:comment "Return nil if no children")
      (->> (org-ml-parse-this-subtree)
           (org-ml-get-children)
           (-map #'org-ml-get-type))
      => nil

      ;; (:buffer "#+call: ktulu()")
      ;; (:comment "Throw error when attempting to get contents of a non-branch node")
      ;; (->> (org-ml-parse-this-element)
      ;;      (org-ml-get-children)
      ;;      (-map #'org-ml-get-type))
      ;; !!> arg-type-error

      :begin-hidden

      (:buffer "* headline"
               "stuff"
               "** subheadline")
      (:comment "Return child element nodes")
      (->> (org-ml-parse-this-subtree)
           (org-ml-get-children)
           (-map #'org-ml-get-type))
      => '(section headline)

      (:buffer "| a | b |")
      (->> (org-ml-parse-this-table-row)
           (org-ml-get-children)
           (-map #'org-ml-get-type))
      => '(table-cell table-cell)

      (:buffer "#+begin_verse"
               "verse /666/"
               "#+end_verse")
      (->> (org-ml-parse-this-element)
           (org-ml-get-children)
           (-map #'org-ml-get-type))
      ;; plain-text for the newline at the end...I think
      => '(plain-text italic plain-text)

      (:buffer "#+begin_center"
               "paragraph thing"
               "#+end_center")
      (->> (org-ml-parse-this-element)
           (org-ml-get-children)
           (-map #'org-ml-get-type))
      => '(paragraph)

      (:buffer ":LOGBOOK:"
               "- log entry"
               "CLOCK: [2019-01-01 Tue]"
               ":END:")
      (->> (org-ml-parse-this-element)
           (org-ml-get-children)
           (-map #'org-ml-get-type))
      => '(plain-list clock)

      (:buffer "[fn:1] bigfoot")
      (->> (org-ml-parse-this-element)
           (org-ml-get-children)
           (-map #'org-ml-get-type))
      => '(paragraph)

      (:buffer "- item"
               "  - subitem")
      (->> (org-ml-parse-this-element)
           (org-ml-get-children)
           (-map #'org-ml-get-type))
      => '(item)
      (->> (org-ml-parse-this-item)
           (org-ml-get-children)
           (-map #'org-ml-get-type))
      => '(paragraph plain-list)

      (:buffer "* dummy"
               ":PROPERTIES:"
               ":ONE: one"
               ":TWO: two"
               ":END:")
      (->> (org-ml-parse-this-headline)
           (org-ml-headline-get-node-properties)
           (-map #'org-ml-get-type))
      => '(node-property node-property)

      (:buffer "#+begin_quote"
               "no pity for the majority"
               "#+end_quote")
      (->> (org-ml-parse-this-element)
           (org-ml-get-children)
           (-map #'org-ml-get-type))
      => '(paragraph)

      ;; (:buffer "* dummy"
      ;;           "stuff")
      ;; (->> (org-ml-parse-this-headline)
      ;;      (org-ml-headline-get-section)
      ;;      (org-ml-get-children)
      ;;      (-map #'org-ml-get-type))
      ;; => '(paragraph)

      (:buffer "| a |"
               "| b |")
      (->> (org-ml-parse-this-element)
           (org-ml-get-children)
           (-map #'org-ml-get-type))
      => '(table-row table-row)

      :end-hidden)

    (defexamples-content org-ml-set-children
      nil

      (:buffer "/this/ is a *paragraph*")
      (:comment "Set children for branch object")
      (->> (org-ml-parse-this-element)
           (org-ml-set-children (list "this is lame"))
           (org-ml-to-trimmed-string))
      => "this is lame"

      (:buffer "* headline")
      (:comment "Set children for branch element nodes")
      (->> (org-ml-parse-this-subtree)
           (org-ml-set-children (list (org-ml-build-headline! :title-text "only me" :level 2)))
           (org-ml-to-trimmed-string))
      => (:result "* headline"
                  "** only me")

      ;; (:buffer "#+call: ktulu()")
      ;; (:comment "Throw error when attempting to set children of a non-branch nodes")
      ;; (->> (org-ml-parse-this-element)
      ;;      (org-ml-set-children "nil by mouth")
      ;;      (org-ml-to-trimmed-string))
      ;; !!> arg-type-error

      :begin-hidden

      ;; TODO add hidden tests

      :end-hidden)

    (defexamples-content org-ml-map-children
      nil

      (:buffer "/this/ is a *paragraph*")
      (->> (org-ml-parse-this-element)
           (org-ml-map-children
             (lambda (objs) (append objs (list " ...yeah"))))
           (org-ml-to-trimmed-string))
      => "/this/ is a *paragraph* ...yeah"

      (:buffer "* headline"
               "** subheadline")
      (->> (org-ml-parse-this-subtree)
           (org-ml-map-children* (--map (org-ml-shift-property :level 1 it) it))
           (org-ml-to-trimmed-string))
      => (:result "* headline"
                  "*** subheadline")

      ;; (:buffer "#+call: ktulu()")
      ;; (:comment "Throw error when attempting to map children of a non-branch node")
      ;; (->> (org-ml-parse-this-element)
      ;;      (org-ml-map-children #'ignore)
      ;;      (org-ml-to-trimmed-string))
      ;; !!> arg-type-error

      :begin-hidden

      ;; TODO add hidden tests

      :end-hidden)

    
    (defexamples-content org-ml-is-childless
      nil
      (:buffer "* dummy"
               "filled with useless knowledge")
      (->> (org-ml-parse-this-headline)
           (org-ml-is-childless))
      => nil
      (:buffer "* dummy")
      (->> (org-ml-parse-this-headline)
           (org-ml-is-childless))
      => t
      ;; (:buffer "#+call: ktulu()")
      ;; (:comment "Throw error when attempting to determine if non-branch node is empty")
      ;; (->> (org-ml-parse-this-element)
      ;;      (org-ml-is-childless))
      ;; !!> arg-type-error
      ))

  (def-example-subgroup "Object Nodes"
    nil

    (defexamples-content org-ml-unwrap
      nil
      (:buffer "_1 *2* 3 */4/* 5 /6/_")
      (:comment "Remove the outer underline formatting")
      (->> (org-ml-parse-this-object)
           (org-ml-unwrap)
           (apply #'org-ml-build-paragraph)
           (org-ml-to-trimmed-string))
      => "1 *2* 3 */4/* 5 /6/")
    
    (defexamples-content org-ml-unwrap-types-deep
      nil
      (:buffer "_1 *2* 3 */4/* 5 /6/_")
      (:comment "Remove bold formatting at any level")
      (->> (org-ml-parse-this-object)
           (org-ml-unwrap-types-deep '(bold))
           (apply #'org-ml-build-paragraph)
           (org-ml-to-trimmed-string))
      => "_1 2 3 /4/ 5 /6/_")

    (defexamples-content org-ml-unwrap-deep
      nil
      (:buffer "_1 *2* 3 */4/* 5 /6/_")
      (:comment "Remove all formatting")
      (->> (org-ml-parse-this-object)
           (org-ml-unwrap-deep)
           (apply #'org-ml-build-paragraph)
           (org-ml-to-trimmed-string))
      => "1 2 3 4 5 6"))

  (def-example-subgroup "Secondary Strings"
    nil

    (defexamples-content org-ml-flatten
      nil
      (:buffer "This (1 *2* 3 */4/* 5 /6/) is randomly formatted")
      (:comment "Remove first level of formatting")
      (->> (org-ml-parse-this-element)
           (org-ml-map-children #'org-ml-flatten)
           (org-ml-to-trimmed-string))
      => "This (1 2 3 /4/ 5 6) is randomly formatted")

    (defexamples-content org-ml-flatten-types-deep
      nil
      (:buffer "This (1 *2* 3 */4/* 5 /6/) is randomly formatted")
      (:comment "Remove italic formatting at any level")
      (->> (org-ml-parse-this-element)
           (org-ml-map-children* (org-ml-flatten-types-deep '(italic) it))
           (org-ml-to-trimmed-string))
      => "This (1 *2* 3 *4* 5 6) is randomly formatted")

    (defexamples-content org-ml-flatten-deep
      nil
      (:buffer "This (1 *2* 3 */4/* 5 /6/) is randomly formatted")
      (:comment "Remove italic formatting at any level")
      (->> (org-ml-parse-this-element)
           (org-ml-map-children #'org-ml-flatten-deep)
           (org-ml-to-trimmed-string))
      => "This (1 2 3 4 5 6) is randomly formatted"))

  (def-example-subgroup "Item"
    nil

    (defexamples-content org-ml-item-get-paragraph
      nil
      (:buffer "- one")
      (->> (org-ml-parse-this-item)
           (org-ml-item-get-paragraph))
      => '("one")
      (:buffer "- ")
      (->> (org-ml-parse-this-item)
           (org-ml-item-get-paragraph))
      => nil)

    (defexamples-content org-ml-item-set-paragraph
      nil
      (:buffer "- one")
      (->> (org-ml-parse-this-item)
           (org-ml-item-set-paragraph '("two"))
           (org-ml-to-string))
      => "- two\n"
      (:buffer "- one")
      (->> (org-ml-parse-this-item)
           (org-ml-item-set-paragraph nil)
           (org-ml-to-string))
      => "- \n")

    (defexamples-content org-ml-item-map-paragraph
      nil
      (:buffer "- one")
      (->> (org-ml-parse-this-item)
           (org-ml-item-map-paragraph* (-map #'upcase it))
           (org-ml-to-string))
      => "- ONE\n"))

  (def-example-subgroup "Headline"
    nil

    (defexamples-content org-ml-headline-get-section
      nil
      (:buffer "* headline 1"
               "sectional stuff"
               "** headline 2"
               "** headline 3")
      (->> (org-ml-parse-this-subtree)
           (org-ml-headline-get-section)
           (-map #'org-ml-to-trimmed-string))
      => '("sectional stuff")
      (:buffer "* headline 1"
               "** headline 2"
               "** headline 3")
      (->> (org-ml-parse-this-subtree)
           (org-ml-headline-get-section)
           (org-ml-to-trimmed-string))
      => "")

    (defexamples-content org-ml-headline-set-section
      nil
      (:buffer "* headline")
      (->> (org-ml-parse-this-subtree)
           (org-ml-headline-set-section (list (org-ml-build-paragraph! "x-section")))
           (org-ml-to-trimmed-string))
      => (:result "* headline"
                  "x-section")
      (:buffer "* headline"
               "x-section")
      (->> (org-ml-parse-this-subtree)
           (org-ml-headline-set-section (list (org-ml-build-paragraph! "x-guard")))
           (org-ml-to-trimmed-string))
      => (:result "* headline"
                  "x-guard")
      (->> (org-ml-parse-this-subtree)
           (org-ml-headline-set-section nil)
           (org-ml-to-trimmed-string))
      => "* headline")

    (defexamples-content org-ml-headline-map-section
      nil
      (:buffer "* headline"
               "x-section")
      (->> (org-ml-parse-this-subtree)
           (org-ml-headline-map-section* (cons (org-ml-build-planning! :closed '(2019 1 1)) it))
           (org-ml-to-trimmed-string))
      => (:result "* headline"
                  "CLOSED: [2019-01-01 Tue]"
                  "x-section"))

    (defexamples-content org-ml-headline-get-subheadlines
      nil
      (:buffer "* headline 1"
               "sectional stuff"
               "** headline 2"
               "** headline 3")
      (->> (org-ml-parse-this-subtree)
           (org-ml-headline-get-subheadlines)
           (-map #'org-ml-to-trimmed-string))
      => '("** headline 2" "** headline 3")
      (:buffer "* headline 1"
               "sectional stuff")
      (->> (org-ml-parse-this-subtree)
           (org-ml-headline-get-subheadlines)
           (-map #'org-ml-to-trimmed-string))
      => nil)

    (defexamples-content org-ml-headline-set-subheadlines
      nil
      (:buffer "* headline 1"
               "sectional stuff"
               "** headline 2"
               "** headline 3")
      (->> (org-ml-parse-this-subtree)
           (org-ml-headline-set-subheadlines (list (org-ml-build-headline! :level 2 :title-text "headline x")))
           (org-ml-to-trimmed-string))
      => (:result "* headline 1"
                  "sectional stuff"
                  "** headline x")
      (->> (org-ml-parse-this-subtree)
           (org-ml-headline-set-subheadlines nil)
           (org-ml-to-trimmed-string))
      => (:result "* headline 1"
                  "sectional stuff"))

    (defexamples-content org-ml-headline-map-subheadlines
      nil
      (:buffer "* headline 1"
               "** headline 2"
               "** headline 3")
      (->> (org-ml-parse-this-subtree)
           (org-ml-headline-map-subheadlines* (--map (org-ml-set-property :todo-keyword "TODO" it) it))
           (org-ml-to-trimmed-string))
      => (:result "* headline 1"
                  "** TODO headline 2"
                  "** TODO headline 3")))

  (def-example-subgroup "Headline (metadata)"
    nil

    (defexamples-content org-ml-headline-get-planning
      nil
      (:buffer "* headline"
               "CLOSED: [2019-01-01 Tue]")
      (->> (org-ml-parse-this-headline)
           (org-ml-headline-get-planning)
           (org-ml-to-trimmed-string))
      => "CLOSED: [2019-01-01 Tue]"
      (:buffer "* headline")
      (->> (org-ml-parse-this-headline)
           (org-ml-headline-get-planning)
           (org-ml-to-trimmed-string))
      => "")

    (defexamples-content org-ml-headline-set-planning
      nil
      (:buffer "* headline")
      (->> (org-ml-parse-this-headline)
           (org-ml-headline-set-planning (org-ml-build-planning! :closed '(2019 1 1)))
           (org-ml-to-trimmed-string))
      => (:result "* headline"
                  "CLOSED: [2019-01-01 Tue]")
      (:buffer "* headline"
               "CLOSED: [2019-01-01 Tue]")
      (->> (org-ml-parse-this-headline)
           (org-ml-headline-set-planning (org-ml-build-planning! :scheduled '(2019 1 1)))
           (org-ml-to-trimmed-string))
      => (:result "* headline"
                  "SCHEDULED: <2019-01-01 Tue>")
      (:buffer "* headline"
               "CLOSED: [2019-01-01 Tue]")
      (->> (org-ml-parse-this-headline)
           (org-ml-headline-set-planning nil)
           (org-ml-to-trimmed-string))
      => "* headline"
      :begin-hidden
      (:buffer "* headline"
               ""
               "rest")
      (->> (org-ml-parse-this-headline)
           (org-ml-headline-set-planning (org-ml-build-planning! :scheduled '(2019 1 1)))
           (org-ml-to-trimmed-string))
      => (:result "* headline"
                  "SCHEDULED: <2019-01-01 Tue>"
                  ""
                  "rest")
      (:buffer "* headline"
               "SCHEDULED: <2019-01-01 Tue>"
               ""
               "rest")
      (->> (org-ml-parse-this-headline)
           (org-ml-headline-set-planning nil)
           (org-ml-to-trimmed-string))
      => (:result "* headline"
                  ""
                  "rest")
      :end-hidden
      )

    (defexamples-content org-ml-headline-map-planning
      nil
      (:buffer "* headline"
               "CLOSED: [2019-01-01 Tue]")
      (->> (org-ml-parse-this-headline)
           (org-ml-headline-map-planning*
             (org-ml-map-property* :closed
               (org-ml-timestamp-shift 1 'day it) it))
           (org-ml-to-trimmed-string))
      => (:result "* headline"
                  "CLOSED: [2019-01-02 Wed]"))

    (defexamples-content org-ml-headline-get-node-properties
      nil
      (:buffer "* headline"
               ":PROPERTIES:"
               ":Effort:   1:00"
               ":ID:       minesfake"
               ":END:")
      (->> (org-ml-parse-this-headline)
           (org-ml-headline-get-node-properties)
           (-map #'org-ml-to-trimmed-string))
      => '(":Effort:   1:00" ":ID:       minesfake")
      (:buffer "* headline")
      (->> (org-ml-parse-this-headline)
           (org-ml-headline-get-node-properties)
           (-map #'org-ml-to-trimmed-string))
      => nil
      :begin-hidden
      (:buffer "* headline"
               "CLOSED: <2019-01-01 Tue>"
               ":PROPERTIES:"
               ":Effort:   1:00"
               ":ID:       minesfake"
               ":END:")
      (->> (org-ml-parse-this-headline)
           (org-ml-headline-get-node-properties)
           (-map #'org-ml-to-trimmed-string))
      => '(":Effort:   1:00" ":ID:       minesfake")
      :end-hidden)

    (defexamples-content org-ml-headline-set-node-properties
      nil
      (:buffer "* headline"
               ":PROPERTIES:"
               ":Effort:   1:00"
               ":ID:       minesfake"
               ":END:")
      (->> (org-ml-parse-this-headline)
           (org-ml-headline-set-node-properties
            (--map (apply #'org-ml-build-node-property it)
                   '(("Effort" "0:01") ("ID" "easy"))))
           (org-ml-to-trimmed-string))
      => (:result "* headline"
                  ":PROPERTIES:"
                  ":Effort:   0:01"
                  ":ID:       easy"
                  ":END:")
      (->> (org-ml-parse-this-headline)
           (org-ml-headline-set-node-properties nil)
           (org-ml-to-trimmed-string))
      => "* headline"
      :begin-hidden
      (:buffer "* headline"
               "CLOSED: <2019-01-01 Tue>"
               ":PROPERTIES:"
               ":Effort:   1:00"
               ":ID:       minesfake"
               ":END:")
      (->> (org-ml-parse-this-headline)
           (org-ml-headline-set-node-properties
            (--map (apply #'org-ml-build-node-property it)
                   '(("Effort" "0:01") ("ID" "easy"))))
           (org-ml-to-trimmed-string))
      => (:result "* headline"
                  "CLOSED: <2019-01-01 Tue>"
                  ":PROPERTIES:"
                  ":Effort:   0:01"
                  ":ID:       easy"
                  ":END:")
      (->> (org-ml-parse-this-headline)
           (org-ml-headline-set-node-properties nil)
           (org-ml-to-trimmed-string))
      => (:result "* headline"
                  "CLOSED: <2019-01-01 Tue>")
      (:buffer "* headline"
               ""
               "section")
      (->> (org-ml-parse-this-headline)
           (org-ml-headline-set-node-properties (list (org-ml-build-node-property "New" "world man")))
           (org-ml-to-trimmed-string))
      => (:result "* headline"
                  ":PROPERTIES:"
                  ":New:      world man"
                  ":END:"
                  ""
                  "section")
      (:buffer "* headline"
               "CLOSED: <2019-01-01 Tue>"
               ""
               "section")
      (->> (org-ml-parse-this-headline)
           (org-ml-headline-set-node-properties (list (org-ml-build-node-property "New" "world man")))
           (org-ml-to-trimmed-string))
      => (:result "* headline"
                  "CLOSED: <2019-01-01 Tue>"
                  ":PROPERTIES:"
                  ":New:      world man"
                  ":END:"
                  ""
                  "section")
      (:buffer "* headline"
               "CLOSED: <2019-01-01 Tue>"
               ":PROPERTIES:"
               ":Effort:   0:01"
               ":ID:       easy"
               ":END:"
               ""
               "section")
      (->> (org-ml-parse-this-headline)
           (org-ml-headline-set-node-properties nil)
           (org-ml-to-trimmed-string))
      => (:result "* headline"
                  "CLOSED: <2019-01-01 Tue>"
                  ""
                  "section"))

    (defexamples-content org-ml-headline-map-node-properties
      nil
      (:buffer "* headline"
               ":PROPERTIES:"
               ":Effort:   1:00"
               ":ID:       minesfake"
               ":END:")
      (->> (org-ml-parse-this-headline)
           (org-ml-headline-map-node-properties*
             (cons (org-ml-build-node-property "New" "world man") it))
           (org-ml-to-trimmed-string))
      => (:result "* headline"
                  ":PROPERTIES:"
                  ":New:      world man"
                  ":Effort:   1:00"
                  ":ID:       minesfake"
                  ":END:")
      ;; assume this will work with planning in front because
      ;; we already tested the get/set base functions
      )

    ;; assume these will work when planning is in front since
    ;; they are all based on the -map-properties (plural) functions
    ;; and these have already been tested
    (defexamples-content org-ml-headline-get-node-property
      nil
      (:buffer "* headline"
               ":PROPERTIES:"
               ":ID:       fake"
               ":END:")
      (->> (org-ml-parse-this-headline)
           (org-ml-headline-get-node-property "ID"))
      => "fake"

      (:buffer "* headline"
               ":PROPERTIES:"
               ":ID:       fake"
               ":END:")
      (->> (org-ml-parse-this-headline)
           (org-ml-headline-get-node-property "READ_ID"))
      => nil)

    (defexamples-content org-ml-headline-set-node-property
      nil
      (:buffer "* headline"
               ":PROPERTIES:"
               ":ID:       fake"
               ":END:")
      (->> (org-ml-parse-this-headline)
           (org-ml-headline-set-node-property "ID" "real")
           (org-ml-to-trimmed-string))
      => (:result "* headline"
                  ":PROPERTIES:"
                  ":ID:       real"
                  ":END:")
      (:buffer "* headline")
      (->> (org-ml-parse-this-headline)
           (org-ml-headline-set-node-property "ID" "real")
           (org-ml-to-trimmed-string))
      => (:result "* headline"
                  ":PROPERTIES:"
                  ":ID:       real"
                  ":END:")
      (->> (org-ml-parse-this-headline)
           (org-ml-headline-set-node-property "ID" nil)
           (org-ml-to-trimmed-string))
      => "* headline"
     (:buffer "* headline"
              ":PROPERTIES:"
              ":ID:       real"
              ":END:")
      (->> (org-ml-parse-this-headline)
           (org-ml-headline-set-node-property "ID" nil)
           (org-ml-to-trimmed-string))
      => "* headline")

    (defexamples-content org-ml-headline-map-node-property
      nil
      (:buffer "* headline"
               ":PROPERTIES:"
               ":ID:       fake"
               ":END:")
      (->> (org-ml-parse-this-headline)
           (org-ml-headline-map-node-property "ID" #'s-upcase)
           (org-ml-to-trimmed-string))
      => (:result "* headline"
                  ":PROPERTIES:"
                  ":ID:       FAKE"
                  ":END:")))

  (def-example-subgroup "Headline (logbook and contents)"
    nil


    (defexamples-content org-ml-headline-get-supercontents
      nil
      
      (:buffer "* headline"
               "CLOSED: [2019-01-01 Tue 00:00]"
               ":PROPERTIES:"
               ":Effort: 0:30"
               ":END:"
               ":LOGGING:"
               "- Note taken on [2018-12-31 Mon 00:00] \\\\"
               "  log note"
               ":END:"
               ":CLOCKING:"
               "CLOCK: [2019-01-01 Tue 00:00]"
               ":END:"
               "contents")

      (let ((config (list :log-into-drawer "LOGGING"
                          :clock-into-drawer "CLOCKING")))
        (->> (org-ml-parse-this-headline)
             (org-ml-headline-get-supercontents config)
             (org-ml-supercontents-get-logbook)
             (org-ml-logbook-get-items)
             (-map #'org-ml-to-trimmed-string)))
      => '("- Note taken on [2018-12-31 Mon 00:00] \\\\\n  log note")

      (let ((config (list :log-into-drawer "LOGGING"
                          :clock-into-drawer "CLOCKING")))
        (->> (org-ml-parse-this-headline)
             (org-ml-headline-get-supercontents config)
             (org-ml-supercontents-get-logbook)
             (org-ml-logbook-get-clocks)
             (-map #'org-ml-to-trimmed-string)))
      => '("CLOCK: [2019-01-01 Tue 00:00]")

      (let ((config (list :log-into-drawer "LOGGING"
                          :clock-into-drawer "CLOCKING")))
        (->> (org-ml-parse-this-headline)
             (org-ml-headline-get-supercontents config)
             (org-ml-supercontents-get-logbook)
             (alist-get :unknown)
             (-map #'org-ml-to-trimmed-string)))
      => nil

      (let ((config (list :log-into-drawer "LOGGING"
                          :clock-into-drawer "CLOCKING")))
        (->> (org-ml-parse-this-headline)
             (org-ml-headline-get-supercontents config)
             (org-ml-supercontents-get-contents)
             (-map #'org-ml-to-trimmed-string)))
      => '("contents"))

    (defexamples-content org-ml-headline-set-supercontents
      nil
      
      (:buffer "* headline"
               "CLOSED: [2019-01-01 Tue 00:00]"
               ":PROPERTIES:"
               ":Effort:    0:30"
               ":END:"
               ":LOGGING:"
               "- Note taken on [2018-12-31 Mon 00:00] \\\\"
               "  log note"
               ":END:"
               ":CLOCKING:"
               "CLOCK: [2019-01-01 Tue 00:00]"
               ":END:"
               "contents")

      (let ((config (list :log-into-drawer "LOGGING"
                          :clock-into-drawer "CLOCKING")))
        (->> (org-ml-parse-this-headline)
             (org-ml-headline-set-supercontents
              config `((:logbook nil) (:contents ,(org-ml-build-paragraph! "new contents"))))
             (org-ml-to-trimmed-string)))
      => (:result "* headline"
                  "CLOSED: [2019-01-01 Tue 00:00]"
                  ":PROPERTIES:"
                  ":Effort:   0:30"
                  ":END:"
                  "new contents"))

    (defexamples-content org-ml-headline-map-supercontents
      nil
      
      (:buffer "* headline"
               "CLOSED: [2019-01-01 Tue 00:00]"
               ":PROPERTIES:"
               ":Effort:    0:30"
               ":END:"
               ":LOGGING:"
               "- Note taken on [2018-12-31 Mon 00:00] \\\\"
               "  log note"
               ":END:"
               ":CLOCKING:"
               "CLOCK: [2019-01-01 Tue 00:00]"
               ":END:"
               "contents")

      (let ((config (list :log-into-drawer "LOGGING"
                          :clock-into-drawer "CLOCKING")))
        (->> (org-ml-parse-this-headline)
             (org-ml-headline-map-supercontents*
                 config (org-ml-supercontents-map-contents*
                          (cons (org-ml-build-paragraph! "new contents") it) it))
             (org-ml-to-trimmed-string)))
      => (:result "* headline"
                  "CLOSED: [2019-01-01 Tue 00:00]"
                  ":PROPERTIES:"
                  ":Effort:   0:30"
                  ":END:"
                  ":LOGGING:"
                  "- Note taken on [2018-12-31 Mon 00:00] \\\\"
                  "  log note"
                  ":END:"
                  ":CLOCKING:"
                  "CLOCK: [2019-01-01 Tue 00:00]"
                  ":END:"
                  "new contents"
                  "contents"))

    (defexamples-content org-ml-headline-get-logbook-items
      nil
      (:buffer "* headline"
               "CLOSED: [2019-01-01 Tue 00:00]"
               ":PROPERTIES:"
               ":Effort: 0:30"
               ":END:"
               ":LOGGING:"
               "- Note taken on [2018-12-31 Mon 00:00] \\\\"
               "  log note"
               ":END:"
               ":CLOCKING:"
               "CLOCK: [2019-01-01 Tue 00:00]"
               ":END:"
               "contents")
      (let ((config (list :log-into-drawer "LOGGING"
                          :clock-into-drawer "CLOCKING")))
        (->> (org-ml-parse-this-headline)
             (org-ml-headline-get-logbook-items config)
             (-map #'org-ml-to-trimmed-string)))
      => '("- Note taken on [2018-12-31 Mon 00:00] \\\\\n  log note"))

    (defexamples-content org-ml-headline-set-logbook-items
      nil
      (:buffer "* headline"
               "CLOSED: [2019-01-01 Tue 00:00]"
               ":PROPERTIES:"
               ":Effort: 0:30"
               ":END:"
               ":LOGGING:"
               "- Note taken on [2018-12-31 Mon 00:00] \\\\"
               "  log note"
               ":END:"
               ":CLOCKING:"
               "CLOCK: [2019-01-01 Tue 00:00]"
               ":END:"
               "contents")
      (let ((config (list :log-into-drawer "LOGGING"
                          :clock-into-drawer "CLOCKING")))
        (->> (org-ml-parse-this-headline)
             (org-ml-headline-set-logbook-items config nil)
             (org-ml-to-trimmed-string)))
      =>  (:result "* headline"
                   "CLOSED: [2019-01-01 Tue 00:00]"
                   ":PROPERTIES:"
                   ":Effort:   0:30"
                   ":END:"
                   ":CLOCKING:"
                   "CLOCK: [2019-01-01 Tue 00:00]"
                   ":END:"
                   "contents")
      :begin-hidden
      (:buffer "* headline"
               "CLOSED: [2019-01-01 Tue 00:00]"
               ":PROPERTIES:"
               ":Effort: 0:30"
               ":END:"
               ":LOGGING:"
               "- Note taken on [2018-12-31 Mon 00:00] \\\\"
               "  log note"
               ":END:"
               ""
               "contents")
      (let ((config (list :log-into-drawer "LOGGING"
                          :clock-into-drawer "CLOCKING")))
        (->> (org-ml-parse-this-headline)
             (org-ml-headline-set-logbook-items config nil)
             (org-ml-to-trimmed-string)))
      =>  (:result "* headline"
                   "CLOSED: [2019-01-01 Tue 00:00]"
                   ":PROPERTIES:"
                   ":Effort:   0:30"
                   ":END:"
                   ""
                   "contents")
      :end-hidden)

    (defexamples-content org-ml-headline-map-logbook-items
      nil
      (:buffer "* headline"
               "CLOSED: [2019-01-01 Tue 00:00]"
               ":PROPERTIES:"
               ":Effort: 0:30"
               ":END:"
               ":LOGGING:"
               "- Note taken on [2018-12-31 Mon 00:00] \\\\"
               "  log note"
               ":END:"
               ":CLOCKING:"
               "CLOCK: [2019-01-01 Tue 00:00]"
               ":END:"
               "contents")
      (let ((config (list :log-into-drawer "LOGGING"
                          :clock-into-drawer "CLOCKING")))
        (->> (org-ml-parse-this-headline)
             (org-ml-headline-map-logbook-items* config
               (--map (org-ml-map-children*
                        (--map (org-ml-map-children*
                                 (--map-when (org-ml-is-type 'plain-text it)
                                             (upcase it)
                                             it)
                                 it)
                               it)
                        it)
                      it))
             (org-ml-to-trimmed-string)))
      =>  (:result "* headline"
                   "CLOSED: [2019-01-01 Tue 00:00]"
                   ":PROPERTIES:"
                   ":Effort:   0:30"
                   ":END:"
                   ":LOGGING:"
                   "- NOTE TAKEN ON [2018-12-31 Mon 00:00] \\\\"
                   "  LOG NOTE"
                   ":END:"
                   ":CLOCKING:"
                   "CLOCK: [2019-01-01 Tue 00:00]"
                   ":END:"
                   "contents"))

    (defexamples-content org-ml-headline-get-logbook-clocks
      nil
      (:buffer "* headline"
               "CLOSED: [2019-01-01 Tue 00:00]"
               ":PROPERTIES:"
               ":Effort: 0:30"
               ":END:"
               ":LOGGING:"
               "- Note taken on [2018-12-31 Mon 00:00] \\\\"
               "  log note"
               ":END:"
               ":CLOCKING:"
               "CLOCK: [2019-01-01 Tue 00:00]"
               ":END:"
               "contents")
      (let ((config (list :log-into-drawer "LOGGING"
                          :clock-into-drawer "CLOCKING")))
        (->> (org-ml-parse-this-headline)
             (org-ml-headline-get-logbook-clocks config)
             (-map #'org-ml-to-trimmed-string)))
      => '("CLOCK: [2019-01-01 Tue 00:00]"))

    (defexamples-content org-ml-headline-set-logbook-clocks
      nil
      (:buffer "* headline"
               "CLOSED: [2019-01-01 Tue 00:00]"
               ":PROPERTIES:"
               ":Effort: 0:30"
               ":END:"
               ":LOGGING:"
               "- Note taken on [2018-12-31 Mon 00:00] \\\\"
               "  log note"
               ":END:"
               ":CLOCKING:"
               "CLOCK: [2019-01-01 Tue 00:00]"
               ":END:"
               "contents")
      (let ((config (list :log-into-drawer "LOGGING"
                          :clock-into-drawer "CLOCKING")))
        (->> (org-ml-parse-this-headline)
             (org-ml-headline-set-logbook-clocks config nil)
             (org-ml-to-trimmed-string)))
      =>  (:result "* headline"
                   "CLOSED: [2019-01-01 Tue 00:00]"
                   ":PROPERTIES:"
                   ":Effort:   0:30"
                   ":END:"
                   ":LOGGING:"
                   "- Note taken on [2018-12-31 Mon 00:00] \\\\"
                   "  log note"
                   ":END:"
                   "contents"))

    (defexamples-content org-ml-headline-map-logbook-clocks
      nil
      (:buffer "* headline"
               "CLOSED: [2019-01-01 Tue 00:00]"
               ":PROPERTIES:"
               ":Effort: 0:30"
               ":END:"
               ":LOGGING:"
               "- Note taken on [2018-12-31 Mon 00:00] \\\\"
               "  log note"
               ":END:"
               ":CLOCKING:"
               "CLOCK: [2019-01-01 Tue 00:00]"
               ":END:"
               "contents")
      (let ((config (list :log-into-drawer "LOGGING"
                          :clock-into-drawer "CLOCKING")))
        (->> (org-ml-parse-this-headline)
             (org-ml-headline-map-logbook-clocks* config
               (--map (org-ml-map-property* :value
                        (org-ml-timestamp-shift 1 'day it)
                        it)
                      it))
             (org-ml-to-trimmed-string)))
      =>  (:result "* headline"
                   "CLOSED: [2019-01-01 Tue 00:00]"
                   ":PROPERTIES:"
                   ":Effort:   0:30"
                   ":END:"
                   ":LOGGING:"
                   "- Note taken on [2018-12-31 Mon 00:00] \\\\"
                   "  log note"
                   ":END:"
                   ":CLOCKING:"
                   "CLOCK: [2019-01-02 Wed 00:00]"
                   ":END:"
                   "contents"))

    (defexamples-content org-ml-headline-get-contents
      nil
      (:buffer "* headline")
      (->> (org-ml-parse-this-headline)
           (org-ml-headline-get-contents
            (list :log-into-drawer t
                  :clock-into-drawer t
                  :clock-out-notes t))
           (-map #'org-ml-to-trimmed-string))
      => nil

      (:buffer "* headline"
               "something")
      (->> (org-ml-parse-this-headline)
           (org-ml-headline-get-contents
            (list :log-into-drawer t
                  :clock-into-drawer t
                  :clock-out-notes t))
           (-map #'org-ml-to-trimmed-string))
      => '("something")

      (:buffer "* headline"
               "CLOSED: [2019-01-01 Tue 00:00]"
               ":LOGBOOK:"
               "- Note taken on [2018-12-31 Mon 00:00] \\\\"
               "  log note"
               "CLOCK: [2019-01-01 Tue 00:00]"
               ":END:"
               ""
               "- not log")
      (->> (org-ml-parse-this-headline)
           (org-ml-headline-get-contents
            (list :log-into-drawer t
                  :clock-into-drawer t
                  :clock-out-notes t))
           (-map #'org-ml-to-trimmed-string))
      => '("- not log")

      (:buffer "* headline"
               "CLOSED: [2019-01-01 Tue 00:00]"
               ":LOGGING:"
               "- Note taken on [2018-12-31 Mon 00:00] \\\\"
               "  log note"
               ":END:"
               ":CLOCKING:"
               "CLOCK: [2019-01-01 Tue 00:00]"
               ":END:"
               ""
               "- not log")

      (->> (org-ml-parse-this-headline)
           (org-ml-headline-get-contents
            (list :log-into-drawer "LOGGING"
                  :clock-into-drawer "CLOCKING"))
           (-map #'org-ml-to-trimmed-string))
      => '("- not log"))

    (defexamples-content org-ml-headline-set-contents
      nil

      (:buffer "* headline")
      (->> (org-ml-parse-this-headline)
           (org-ml-headline-set-contents
            (list :log-into-drawer t
                  :clock-into-drawer t
                  :clock-out-notes t)
            (list (org-ml-build-paragraph! "I'm new")))
           (org-ml-to-trimmed-string))
      => (:result "* headline"
                  "I'm new")

      (:buffer "* headline"
               "something")
      (->> (org-ml-parse-this-headline)
           (org-ml-headline-set-contents
            (list :log-into-drawer t
                  :clock-into-drawer t
                  :clock-out-notes t)
            (list (org-ml-build-paragraph! "I'm new")))
           (org-ml-to-trimmed-string))
      => (:result "* headline"
                  "I'm new")

      (:buffer "* headline"
               ":LOGBOOK:"
               "- Note taken on [2018-12-31 Mon 00:00] \\\\"
               "  log1"
               ":END:"
               "something")
      (->> (org-ml-parse-this-headline)
           (org-ml-headline-set-contents
            (list :log-into-drawer t
                  :clock-into-drawer t
                  :clock-out-notes t)
            (list (org-ml-build-paragraph! "I'm new")))
           (org-ml-to-trimmed-string))
      => (:result "* headline"
                  ":LOGBOOK:"
                  "- Note taken on [2018-12-31 Mon 00:00] \\\\"
                  "  log1"
                  ":END:"
                  "I'm new")

      (:buffer "* headline"
               ":LOGBOOK:"
               "- Note taken on [2018-12-31 Mon 00:00] \\\\"
               "  log1"
               ":END:"
               "something")
      (->> (org-ml-parse-this-headline)
           (org-ml-headline-set-contents
            (list :log-into-drawer t
                  :clock-into-drawer t
                  :clock-out-notes t)
            nil)
           (org-ml-to-trimmed-string))
      => (:result "* headline"
                  ":LOGBOOK:"
                  "- Note taken on [2018-12-31 Mon 00:00] \\\\"
                  "  log1"
                  ":END:"))

    (defexamples-content org-ml-headline-map-contents
      nil

      (:buffer "* headline"
               "something")
      (->> (org-ml-parse-this-headline)
           (org-ml-headline-map-contents*
               (list :log-into-drawer t
                     :clock-into-drawer t
                     :clock-out-notes t)
             (cons (org-ml-build-paragraph! "I'm new") it))
           (org-ml-to-trimmed-string))
      => (:result "* headline"
                  "I'm new"
                  "something"))

    (defexamples-content org-ml-headline-logbook-append-item
      nil
      (:buffer "* headline")
      (let ((ut (- 1546300800 (car (current-time-zone)))))
        (->> (org-ml-parse-this-headline)
             (org-ml-headline-logbook-append-item
              
              (list :log-into-drawer t
                    :clock-into-drawer t
                    :clock-out-notes t)
              (org-ml-build-log-note ut "new note"))
             (org-ml-to-trimmed-string)))
      => (:result "* headline"
                  ":LOGBOOK:"
                  "- Note taken on [2019-01-01 Tue 00:00] \\\\"
                  "  new note"
                  ":END:")

      (:buffer "* headline"
               ":LOGBOOK:"
               "- Note taken on [2018-12-31 Mon 00:00] \\\\"
               "  old note"
               ":END:")
      (let ((ut (- 1546300800 (car (current-time-zone)))))
        (->> (org-ml-parse-this-headline)
             (org-ml-headline-logbook-append-item
              
              (list :log-into-drawer t
                    :clock-into-drawer t
                    :clock-out-notes t)
              (org-ml-build-log-note ut "new note"))
             (org-ml-to-trimmed-string)))
      => (:result "* headline"
                  ":LOGBOOK:"
                  "- Note taken on [2019-01-01 Tue 00:00] \\\\"
                  "  new note"
                  "- Note taken on [2018-12-31 Mon 00:00] \\\\"
                  "  old note"
                  ":END:")

      (:buffer "* headline"
               ":LOGGING:"
               "- Note taken on [2018-12-31 Mon 00:00] \\\\"
               "  old note"
               ":END:"
               ":CLOCKING:"
               "CLOCK: [2112-01-01 Fri]"
               ":END:")
      (let ((ut (- 1546300800 (car (current-time-zone)))))
        (->> (org-ml-parse-this-headline)
             (org-ml-headline-logbook-append-item
              (list :log-into-drawer "LOGGING"
                    :clock-into-drawer "CLOCKING")
              (org-ml-build-log-note ut "new note"))
             (org-ml-to-trimmed-string)))
      => (:result "* headline"
                  ":LOGGING:"
                  "- Note taken on [2019-01-01 Tue 00:00] \\\\"
                  "  new note"
                  "- Note taken on [2018-12-31 Mon 00:00] \\\\"
                  "  old note"
                  ":END:"
                  ":CLOCKING:"
                  "CLOCK: [2112-01-01 Fri]"
                  ":END:"))

    (defexamples-content org-ml-headline-logbook-append-open-clock
      nil

      (:buffer "* headline")
      (->> (org-ml-parse-this-headline)
           (org-ml-headline-logbook-append-open-clock
            
            (list :log-into-drawer t
                  :clock-into-drawer t
                  :clock-out-notes t)
            (- 1546300800 (car (current-time-zone))))
           (org-ml-to-trimmed-string))
      => (:result "* headline"
                  ":LOGBOOK:"
                  "CLOCK: [2019-01-01 Tue 00:00]"
                  ":END:")

      (:buffer "* headline"
               ":LOGBOOK:"
               "- note taken on [2018-12-30 Sun 00:00]"
               ":END:")
      (->> (org-ml-parse-this-headline)
           (org-ml-headline-logbook-append-open-clock
            
            (list :log-into-drawer t
                  :clock-into-drawer t
                  :clock-out-notes t)
            (- 1546300800 (car (current-time-zone))))
           (org-ml-to-trimmed-string))
      => (:result "* headline"
                  ":LOGBOOK:"
                  "CLOCK: [2019-01-01 Tue 00:00]"
                  "- note taken on [2018-12-30 Sun 00:00]"
                  ":END:")

      (:buffer "* headline"
               ":LOGGING:"
               "- note taken on [2018-12-30 Sun 00:00]"
               ":END:")
      (->> (org-ml-parse-this-headline)
           (org-ml-headline-logbook-append-open-clock
            (list :log-into-drawer "LOGGING"
                  :clock-into-drawer "CLOCKING")
            (- 1546300800 (car (current-time-zone))))
           (org-ml-to-trimmed-string))
      => (:result "* headline"
                  ":LOGGING:"
                  "- note taken on [2018-12-30 Sun 00:00]"
                  ":END:"
                  ":CLOCKING:"
                  "CLOCK: [2019-01-01 Tue 00:00]"
                  ":END:"))

    (defexamples-content org-ml-headline-logbook-close-open-clock
      nil
      (:buffer "* headline"
               ":LOGBOOK:"
               "- note taken on [2018-12-30 Sun 00:00]"
               ":END:")
      (->> (org-ml-parse-this-headline)
           (org-ml-headline-logbook-close-open-clock
            (list :log-into-drawer t
                  :clock-into-drawer t
                  :clock-out-notes t)
            (- 1546300800 (car (current-time-zone))) nil)
           (org-ml-to-trimmed-string))
      => (:result "* headline"
                  ":LOGBOOK:"
                  "- note taken on [2018-12-30 Sun 00:00]"
                  ":END:")

      (:buffer "* headline"
               ":LOGBOOK:"
               "CLOCK: [2018-12-31 Mon 00:00]"
               "- note taken on [2018-12-30 Sun 00:00]"
               ":END:")
      (->> (org-ml-parse-this-headline)
           (org-ml-headline-logbook-close-open-clock
            (list :log-into-drawer t
                  :clock-into-drawer t
                  :clock-out-notes t)
            (- 1546300800 (car (current-time-zone))) nil)
           (org-ml-to-trimmed-string))
      => (:result "* headline"
                  ":LOGBOOK:"
                  "CLOCK: [2018-12-31 Mon 00:00]--[2019-01-01 Tue 00:00] => 24:00"
                  "- note taken on [2018-12-30 Sun 00:00]"
                  ":END:")

      (->> (org-ml-parse-this-headline)
           (org-ml-headline-logbook-close-open-clock
            (list :log-into-drawer t
                  :clock-into-drawer t
                  :clock-out-notes t)
            (- 1546300800 (car (current-time-zone))) "new note")
           (org-ml-to-trimmed-string))
      => (:result "* headline"
                  ":LOGBOOK:"
                  "CLOCK: [2018-12-31 Mon 00:00]--[2019-01-01 Tue 00:00] => 24:00"
                  "- new note"
                  "- note taken on [2018-12-30 Sun 00:00]"
                  ":END:")

      (:buffer "* headline"
               ":LOGGING:"
               "- note taken on [2018-12-30 Sun 00:00]"
               ":END:"
               ":CLOCKING:"
               "CLOCK: [2018-12-31 Mon 00:00]"
               ":END:")
      (->> (org-ml-parse-this-headline)
           (org-ml-headline-logbook-close-open-clock
            '(:log-into-drawer "LOGGING" :clock-into-drawer "CLOCKING" :clock-out-notes t) (- 1546300800 (car (current-time-zone))) nil)
           (org-ml-to-trimmed-string))
      => (:result "* headline"
                  ":LOGGING:"
                  "- note taken on [2018-12-30 Sun 00:00]"
                  ":END:"
                  ":CLOCKING:"
                  "CLOCK: [2018-12-31 Mon 00:00]--[2019-01-01 Tue 00:00] => 24:00"
                  ":END:"))

    (defexamples-content org-ml-headline-logbook-convert-config
      nil
      (:buffer "* headline"
               "CLOCK: [2018-12-31 Mon 00:00]--[2019-01-01 Tue 00:00] => 24:00"
               "- note taken on [2018-12-30 Sun 00:00]")
      (->> (org-ml-parse-this-headline)
           (org-ml-headline-logbook-convert-config nil
                                                   (list :log-into-drawer t :clock-into-drawer t))
           (org-ml-to-trimmed-string))
      => (:result "* headline"
                  ":LOGBOOK:"
                  "CLOCK: [2018-12-31 Mon 00:00]--[2019-01-01 Tue 00:00] => 24:00"
                  "- note taken on [2018-12-30 Sun 00:00]"
                  ":END:")

      (->> (org-ml-parse-this-headline)
           (org-ml-headline-logbook-convert-config nil
                                                   (list :log-into-drawer "LOGGING" :clock-into-drawer "CLOCKING"))
           (org-ml-to-trimmed-string))
      => (:result "* headline"
                  ":LOGGING:"
                  "- note taken on [2018-12-30 Sun 00:00]"
                  ":END:"
                  ":CLOCKING:"
                  "CLOCK: [2018-12-31 Mon 00:00]--[2019-01-01 Tue 00:00] => 24:00"
                  ":END:")

      (:buffer "* headline"
               ":LOGBOOK:"
               "CLOCK: [2018-12-31 Mon 00:00]--[2019-01-01 Tue 00:00] => 24:00"
               "- note taken on [2018-12-30 Sun 00:00]"
               ":END:")
      (->> (org-ml-parse-this-headline)
           (org-ml-headline-logbook-convert-config
            (list :log-into-drawer t
                  :clock-into-drawer t)
            (list :log-into-drawer "LOGGING"
                  :clock-into-drawer "CLOCKING"))
           (org-ml-to-trimmed-string))
      => (:result "* headline"
                  ":LOGGING:"
                  "- note taken on [2018-12-30 Sun 00:00]"
                  ":END:"
                  ":CLOCKING:"
                  "CLOCK: [2018-12-31 Mon 00:00]--[2019-01-01 Tue 00:00] => 24:00"
                  ":END:")))

  (def-example-subgroup "Headline (misc)"
    nil

    (defexamples-content org-ml-headline-get-path
      nil
      (:buffer "* one"
               "** two"
               "*** three")
      (->> (org-ml-parse-this-subtree)
           (org-ml-headline-get-path))
      => '("one")
      (->> (org-ml-parse-this-subtree)
           (org-ml-headline-get-subheadlines)
           (car)
           (org-ml-headline-get-subheadlines)
           (car)
           (org-ml-headline-get-path))
      => '("one" "two" "three"))

    (defexamples-content org-ml-headline-update-item-statistics
      nil
      (:buffer "* statistically significant [/]"
               "- irrelevant data"
               "- [ ] good data"
               "- [X] bad data")
      (->> (org-ml-parse-this-headline)
           (org-ml-headline-update-item-statistics)
           (org-ml-to-trimmed-string))
      => (:result "* statistically significant [1/2]"
                  "- irrelevant data"
                  "- [ ] good data"
                  "- [X] bad data")

      :begin-hidden
      (:buffer "* statistically significant [%]"
               "- irrelevant data"
               "- [ ] good data"
               "- [X] bad data")
      (->> (org-ml-parse-this-headline)
           (org-ml-headline-update-item-statistics)
           (org-ml-to-trimmed-string))
      => (:result "* statistically significant [50%]"
                  "- irrelevant data"
                  "- [ ] good data"
                  "- [X] bad data")
      :end-hidden

      (:buffer "* statistically significant"
               "- irrelevant data"
               "- [ ] good data"
               "- [X] bad data")
      (:comment "Do nothing if nothing to update")
      (->> (org-ml-parse-this-headline)
           (org-ml-headline-update-item-statistics)
           (org-ml-to-trimmed-string))
      => (:result "* statistically significant"
                  "- irrelevant data"
                  "- [ ] good data"
                  "- [X] bad data"))

    (defexamples-content org-ml-headline-update-todo-statistics
      nil
      (:buffer "* statistically significant [/]"
               "** irrelevant data"
               "** TODO good data"
               "** DONE bad data")
      (->> (org-ml-parse-this-subtree)
           (org-ml-headline-update-todo-statistics)
           (org-ml-to-trimmed-string))
      => (:result "* statistically significant [1/2]"
                  "** irrelevant data"
                  "** TODO good data"
                  "** DONE bad data")

      :begin-hidden
      (:buffer "* statistically significant [%]"
               "** irrelevant data"
               "** TODO good data"
               "** DONE bad data")
      (->> (org-ml-parse-this-subtree)
           (org-ml-headline-update-todo-statistics)
           (org-ml-to-trimmed-string))
      => (:result "* statistically significant [50%]"
                  "** irrelevant data"
                  "** TODO good data"
                  "** DONE bad data")
      :end-hidden

      (:buffer "* statistically significant"
               "** irrelevant data"
               "** TODO good data"
               "** DONE bad data")
      (:comment "Do nothing if nothing to update")
      (->> (org-ml-parse-this-subtree)
           (org-ml-headline-update-todo-statistics)
           (org-ml-to-trimmed-string))
      => (:result "* statistically significant"
                  "** irrelevant data"
                  "** TODO good data"
                  "** DONE bad data"))

    (defexamples-content org-ml-headline-demote-subheadline
      nil
      (:buffer "* one"
               "** two"
               "** three"
               "*** four")
      (->> (org-ml-parse-element-at 1)
           (org-ml-headline-demote-subheadline 0)
           (org-ml-to-trimmed-string))
      !!> error
      (->> (org-ml-parse-element-at 1)
           (org-ml-headline-demote-subheadline 1)
           (org-ml-to-trimmed-string))
      => (:result "* one"
                  "** two"
                  "*** three"
                  "*** four")

      :begin-hidden

      ;; make sure this works with whitespace

      ;; (:buffer "* one"
      ;;          ""
      ;;          "** two"
      ;;          ""
      ;;          "** three"
      ;;          ""
      ;;          "*** four")
      ;; (->> (org-ml-parse-element-at 1)
      ;;      (org-ml-headline-demote-subheadline 1)
      ;;      (org-ml-to-trimmed-string))
      ;; => (:result "* one"
      ;;             ""
      ;;             "** two"
      ;;             ""
      ;;             "*** three"
      ;;             ""
      ;;             "*** four")

      (:buffer "* one"
               "something"
               ""
               "** two"
               "something"
               ""
               "** three"
               "something"
               ""
               "*** four")
      (->> (org-ml-parse-element-at 1)
           (org-ml-headline-demote-subheadline 1)
           (org-ml-to-trimmed-string))
      => (:result "* one"
                  "something"
                  ""
                  "** two"
                  "something"
                  ""
                  "*** three"
                  "something"
                  ""
                  "*** four")

      :end-hidden)

    (defexamples-content org-ml-headline-demote-subtree
      nil
      (:buffer "* one"
               "** two"
               "** three"
               "*** four")
      (->> (org-ml-parse-element-at 1)
           (org-ml-headline-demote-subtree 1)
           (org-ml-to-trimmed-string))
      => (:result "* one"
                  "** two"
                  "*** three"
                  "**** four"))

    (defexamples-content org-ml-headline-promote-subheadline
      nil
      (:buffer "* one"
               "** two"
               "** three"
               "*** four"
               "*** four"
               "*** four")
      (->> (org-ml-parse-element-at 1)
           (org-ml-headline-promote-subheadline 1 1)
           (org-ml-to-trimmed-string))
      => (:result "* one"
                  "** two"
                  "** three"
                  "*** four"
                  "** four"
                  "*** four"))

    (defexamples-content org-ml-headline-promote-all-subheadlines
      nil
      (:buffer "* one"
               "** two"
               "** three"
               "*** four"
               "*** four"
               "*** four")
      (->> (org-ml-parse-element-at 1)
           (org-ml-headline-promote-all-subheadlines 1)
           (org-ml-to-trimmed-string))
      => (:result "* one"
                  "** two"
                  "** three"
                  "** four"
                  "** four"
                  "** four")))

  ;; (def-example-subgroup "Item"
  ;;   nil

  ;; (defexamples-content org-ml-item-get-level
  ;;   nil
  ;;   (:buffer "- one"
  ;;             "  - two"
  ;;             "    - three")
  ;;   (->> (org-ml-parse-this-item)
  ;;        (org-ml-)))

  ;; (defexamples-content org-ml-item-get-sublist
  ;;   nil
  ;;   (:buffer "- one"
  ;;             "  - two"
  ;;             "  - three"
  ;;             "- four")
  ;;   (->> (org-ml-parse-this-item)
  ;;        (org-ml-item-get-sublist)
  ;;        (org-ml-to-trimmed-string))
  ;;   => (:result "- two"
  ;;               "- three")
  ;;   (:buffer "- one"
  ;;             "- two")
  ;;   (->> (org-ml-parse-this-item)
  ;;        (org-ml-item-get-sublist)
  ;;        (org-ml-to-trimmed-string))
  ;;   => "")

  ;; (defexamples-content org-ml-item-get-paragraph
  ;;   nil
  ;;   (:buffer "- one")
  ;;   (->> (org-ml-parse-this-item)
  ;;        (org-ml-item-get-paragraph)
  ;;        (org-ml-to-trimmed-string))
  ;;   => "one"
  ;;   (:buffer "- [ ] one")
  ;;   (->> (org-ml-parse-this-item)
  ;;        (org-ml-item-get-paragraph)
  ;;        (org-ml-to-trimmed-string))
  ;;   => "one"
  ;;   (:buffer "- tmsu :: one")
  ;;   (->> (org-ml-parse-this-item)
  ;;        (org-ml-item-get-paragraph)
  ;;        (org-ml-to-trimmed-string))
  ;;   => "one"
  ;;   (:buffer "- tmsu ::")
  ;;   (->> (org-ml-parse-this-item)
  ;;        (org-ml-item-get-paragraph)
  ;;        (org-ml-to-trimmed-string))
  ;;   => ""))

  (def-example-subgroup "Plain List"
    nil
    
    (defexamples-content org-ml-plain-list-set-type
      nil
      (:buffer "- [ ] one"
               "- [X] two")
      (->> (org-ml-parse-this-element)
           (org-ml-plain-list-set-type 'ordered)
           (org-ml-to-trimmed-string))
      => (:result "1. [ ] one"
                  "2. [X] two")
      (:buffer "1. [ ] one"
               "2. [X] two")
      (->> (org-ml-parse-this-element)
           (org-ml-plain-list-set-type 'unordered)
           (org-ml-to-trimmed-string))
      => (:result "- [ ] one"
                  "- [X] two"))

    (defexamples-content org-ml-plain-list-indent-item
      nil
      (:buffer "- one"
               "- two"
               "  - three"
               "- four")
      (:comment "It makes no sense to indent the first item")
      (->> (org-ml-parse-element-at 1)
           (org-ml-plain-list-indent-item 0)
           (org-ml-to-trimmed-string))
      !!> error
      (->> (org-ml-parse-element-at 1)
           (org-ml-plain-list-indent-item 1)
           (org-ml-to-trimmed-string))
      => (:result "- one"
                  "  - two"
                  "  - three"
                  "- four")
      (->> (org-ml-parse-element-at 1)
           (org-ml-plain-list-indent-item 2)
           (org-ml-to-trimmed-string))
      => (:result "- one"
                  "- two"
                  "  - three"
                  "  - four"))

    (defexamples-content org-ml-plain-list-indent-item-tree
      nil
      (:buffer "- one"
               "- two"
               "  - three"
               "- four")
      (->> (org-ml-parse-element-at 1)
           (org-ml-plain-list-indent-item-tree 1)
           (org-ml-to-trimmed-string))
      => (:result "- one"
                  "  - two"
                  "    - three"
                  "- four"))

    (defexamples-content org-ml-plain-list-outdent-item
      nil
      (:buffer "- one"
               "- two"
               "  - three"
               "  - three"
               "  - three"
               "- four")
      (->> (org-ml-parse-element-at 1)
           (org-ml-plain-list-outdent-item 1 0)
           (org-ml-to-trimmed-string))
      => (:result "- one"
                  "- two"
                  "- three"
                  "  - three"
                  "  - three"
                  "- four")
      (->> (org-ml-parse-element-at 1)
           (org-ml-plain-list-outdent-item 1 1)
           (org-ml-to-trimmed-string))
      => (:result "- one"
                  "- two"
                  "  - three"
                  "- three"
                  "  - three"
                  "- four")
      (->> (org-ml-parse-element-at 1)
           (org-ml-plain-list-outdent-item 2 1)
           (org-ml-to-trimmed-string))
      => (:result "- one"
                  "- two"
                  "  - three"
                  "  - three"
                  "  - three"
                  "- four"))
    
    (defexamples-content org-ml-plain-list-outdent-all-items
      nil
      (:buffer "- one"
               "- two"
               "  - three"
               "  - three"
               "  - three"
               "- four")
      (->> (org-ml-parse-element-at 1)
           (org-ml-plain-list-outdent-all-items 1)
           (org-ml-to-trimmed-string))
      => (:result "- one"
                  "- two"
                  "- three"
                  "- three"
                  "- three"
                  "- four")
      (->> (org-ml-parse-element-at 1)
           (org-ml-plain-list-outdent-all-items 2)
           (org-ml-to-trimmed-string))
      => (:result "- one"
                  "- two"
                  "  - three"
                  "  - three"
                  "  - three"
                  "- four")))

  (def-example-subgroup "Table"
    nil

    (defexamples-content org-ml-table-get-cell
      nil
      (:buffer "| 1 | 2 | 3 |"
               "|---+---+---|"
               "| a | b | c |")
      (->> (org-ml-parse-this-element)
           (org-ml-table-get-cell 0 0)
           (org-ml-get-children)
           (car))
      => "1"
      (->> (org-ml-parse-this-element)
           (org-ml-table-get-cell 1 1)
           (org-ml-get-children)
           (car))
      => "b"
      (->> (org-ml-parse-this-element)
           (org-ml-table-get-cell -1 -1)
           (org-ml-get-children)
           (car))
      => "c"
      :begin-hidden
      (->> (org-ml-parse-this-element)
           (org-ml-table-get-cell 0 3)
           (org-ml-get-children)
           (car))
      !!> arg-type-error
      :end-hidden)

    (defexamples-content org-ml-table-delete-column
      nil
      (:buffer "| a | b |"
               "|---+---|"
               "| c | d |")
      (->> (org-ml-parse-this-element)
           (org-ml-table-delete-column 0)
           (org-ml-to-trimmed-string))
      => (:result "| b |"
                  "|---|"
                  "| d |")
      (->> (org-ml-parse-this-element)
           (org-ml-table-delete-column 1)
           (org-ml-to-trimmed-string))
      => (:result "| a |"
                  "|---|"
                  "| c |")
      (->> (org-ml-parse-this-element)
           (org-ml-table-delete-column -1)
           (org-ml-to-trimmed-string))
      => (:result "| a |"
                  "|---|"
                  "| c |"))

    (defexamples-content org-ml-table-delete-row
      nil
      (:buffer "| a | b |"
               "|---+---|"
               "| c | d |")
      (->> (org-ml-parse-this-element)
           (org-ml-table-delete-row 0)
           (org-ml-to-trimmed-string))
      => (:result "|---+---|"
                  "| c | d |")
      (->> (org-ml-parse-this-element)
           (org-ml-table-delete-row 1)
           (org-ml-to-trimmed-string))
      => (:result "| a | b |"
                  "| c | d |")
      (->> (org-ml-parse-this-element)
           (org-ml-table-delete-row -1)
           (org-ml-to-trimmed-string))
      => (:result "| a | b |"
                  "|---+---|"))

    (defexamples-content org-ml-table-insert-column!
      nil
      (:buffer "| a | b |"
               "|---+---|"
               "| c | d |")
      (->> (org-ml-parse-this-element)
           (org-ml-table-insert-column! 1 '("x" "y"))
           (org-ml-to-trimmed-string))
      => (:result "| a | x | b |"
                  "|---+---+---|"
                  "| c | y | d |")
      (->> (org-ml-parse-this-element)
           (org-ml-table-insert-column! -1 '("x" "y"))
           (org-ml-to-trimmed-string))
      => (:result "| a | b | x |"
                  "|---+---+---|"
                  "| c | d | y |"))

    (defexamples-content org-ml-table-insert-row!
      nil
      (:buffer "| a | b |"
               "|---+---|"
               "| c | d |")
      (->> (org-ml-parse-this-element)
           (org-ml-table-insert-row! 1 '("x" "y"))
           (org-ml-to-trimmed-string))
      => (:result "| a | b |"
                  "| x | y |"
                  "|---+---|"
                  "| c | d |")
      (->> (org-ml-parse-this-element)
           (org-ml-table-insert-row! 2 '("x" "y"))
           (org-ml-to-trimmed-string))
      => (:result "| a | b |"
                  "|---+---|"
                  "| x | y |"
                  "| c | d |")
      (->> (org-ml-parse-this-element)
           (org-ml-table-insert-row! -1 '("x" "y"))
           (org-ml-to-trimmed-string))
      => (:result "| a | b |"
                  "|---+---|"
                  "| c | d |"
                  "| x | y |"))

    (defexamples-content org-ml-table-replace-cell!
      nil
      (:buffer "| 1 | 2 |"
               "|---+---|"
               "| a | b |")
      (->> (org-ml-parse-this-element)
           (org-ml-table-replace-cell! 0 0 "2")
           (org-ml-to-trimmed-string))
      => (:result "| 2 | 2 |"
                  "|---+---|"
                  "| a | b |")
      (->> (org-ml-parse-this-element)
           (org-ml-table-replace-cell! 0 0 nil)
           (org-ml-to-trimmed-string))
      => (:result "|   | 2 |"
                  "|---+---|"
                  "| a | b |")
      (->> (org-ml-parse-this-element)
           (org-ml-table-replace-cell! -1 -1 "B")
           (org-ml-to-trimmed-string))
      => (:result "| 1 | 2 |"
                  "|---+---|"
                  "| a | B |"))

    (defexamples-content org-ml-table-replace-column!
      nil
      (:buffer "| a | b |"
               "|---+---|"
               "| c | d |")
      (->> (org-ml-parse-this-element)
           (org-ml-table-replace-column! 0 '("A" "B"))
           (org-ml-to-trimmed-string))
      => (:result "| A | b |"
                  "|---+---|"
                  "| B | d |")
      (->> (org-ml-parse-this-element)
           (org-ml-table-replace-column! 0 nil)
           (org-ml-to-trimmed-string))
      => (:result "|   | b |"
                  "|---+---|"
                  "|   | d |")
      (->> (org-ml-parse-this-element)
           (org-ml-table-replace-column! -1 '("A" "B"))
           (org-ml-to-trimmed-string))
      => (:result "| a | A |"
                  "|---+---|"
                  "| c | B |"))

    (defexamples-content org-ml-table-replace-row!
      nil
      (:buffer "| a | b |"
               "|---+---|"
               "| c | d |")
      (->> (org-ml-parse-this-element)
           (org-ml-table-replace-row! 0 '("A" "B"))
           (org-ml-to-trimmed-string))
      => (:result "| A | B |"
                  "|---+---|"
                  "| c | d |")
      (->> (org-ml-parse-this-element)
           (org-ml-table-replace-row! 0 nil)
           (org-ml-to-trimmed-string))
      => (:result "|   |   |"
                  "|---+---|"
                  "| c | d |")
      (->> (org-ml-parse-this-element)
           (org-ml-table-replace-row! -1 '("A" "B"))
           (org-ml-to-trimmed-string))
      => (:result "| a | b |"
                  "|---+---|"
                  "| A | B |"))))

(def-example-group "Node Matching"
  "Use pattern-matching to selectively perform operations on nodes in trees."

  (defexamples-content org-ml-match
    nil

    (:buffer "* headline 1"
             "** TODO headline 2"
             "stuff"
             "- item 1"
             "- item 2"
             "- item 3"
             "** DONE headline 3"
             "- item 4"
             "- item 5"
             "- item 6"
             "** TODO COMMENT headline 4"
             "- item 7"
             "- item 8"
             "- item 9")
    (:comment "Match items (excluding the first) in headlines that"
              "are marked \"TODO\" and not commented."
              "The :many keyword matches the section and plain-list"
              "nodes holding the items.")
    (->> (org-ml-parse-this-subtree)
         (org-ml-match '((:and (:todo-keyword "TODO") (:commentedp nil))
                         :any *
                         (:and item (> 0))))
         (-map #'org-ml-to-trimmed-string))
    => '("- item 2" "- item 3")

    (:buffer "*one* *two* *three* *four* *five* *six*")
    (:comment "Return all bold nodes")
    (->> (org-ml-parse-this-element)
         (org-ml-match '(bold))
         (-map #'org-ml-to-trimmed-string))
    => '("*one*" "*two*" "*three*" "*four*" "*five*" "*six*")
    (:comment "Return first bold node")
    (->> (org-ml-parse-this-element)
         (org-ml-match '(:first bold))
         (-map #'org-ml-to-trimmed-string))
    => '("*one*")
    (:comment "Return last bold node")
    (->> (org-ml-parse-this-element)
         (org-ml-match '(:last bold))
         (-map #'org-ml-to-trimmed-string))
    => '("*six*")
    (:comment "Return a select bold node")
    (->> (org-ml-parse-this-element)
         (org-ml-match '(:nth 2 bold))
         (-map #'org-ml-to-trimmed-string))
    => '("*three*")
    (:comment "Return a sublist of matched bold nodes")
    (->> (org-ml-parse-this-element)
         (org-ml-match '(:sub 1 3 bold))
         (-map #'org-ml-to-trimmed-string))
    => '("*two*" "*three*" "*four*")

    :begin-hidden

    ;; Test all atomic and compound condition combinations here.
    ;; These tests ensure that:
    ;; - `org-ml--match-make-condition-form' is correct for all VALID
    ;;   condition combinations (the error cases are tested in
    ;;   `org-ml-test.el')
    ;; - the single and multiple condition paths in
    ;;   `org-ml--match-make-inner-pattern-form' are correct
    
    (:buffer "* one"
             "** TODO two"
             "2"
             "** COMMENT three"
             "3"
             "** four"
             "4"
             "** DONE five"
             "5")
    
    ;; type
    (->> (org-ml-parse-this-subtree)
         (org-ml-match '(headline section))
         (--map (org-ml-to-trimmed-string it)))
    => '("2" "3" "4" "5")
    (->> (org-ml-parse-this-subtree)
         (org-ml-match '(headline table))
         (--map (org-ml-to-trimmed-string it)))
    => nil

    ;; index
    (->> (org-ml-parse-this-subtree)
         (org-ml-match '(0 section))
         (--map (org-ml-to-trimmed-string it)))
    => '("2")
    (->> (org-ml-parse-this-subtree)
         (org-ml-match '(-1 section))
         (--map (org-ml-to-trimmed-string it)))
    => '("5")
    (->> (org-ml-parse-this-subtree)
         (org-ml-match '(4 section))
         (--map (org-ml-to-trimmed-string it)))
    => nil
    (->> (org-ml-parse-this-subtree)
         (org-ml-match '(-5 section))
         (--map (org-ml-to-trimmed-string it)))
    => nil

    ;; relative index
    (->> (org-ml-parse-this-subtree)
         (org-ml-match '((> 0) section))
         (--map (org-ml-to-trimmed-string it)))
    => '("3" "4" "5")
    (->> (org-ml-parse-this-subtree)
         (org-ml-match '((>= 1) section))
         (--map (org-ml-to-trimmed-string it)))
    => '("3" "4" "5")
    (->> (org-ml-parse-this-subtree)
         (org-ml-match '((<= -2) section))
         (--map (org-ml-to-trimmed-string it)))
    => '("2" "3" "4")
    (->> (org-ml-parse-this-subtree)
         (org-ml-match '((< -1) section))
         (--map (org-ml-to-trimmed-string it)))
    => '("2" "3" "4")
    (->> (org-ml-parse-this-subtree)
         (org-ml-match '((< 0) section))
         (--map (org-ml-to-trimmed-string it)))
    => nil
    (->> (org-ml-parse-this-subtree)
         (org-ml-match '((> 3) section))
         (--map (org-ml-to-trimmed-string it)))
    => nil
    (->> (org-ml-parse-this-subtree)
         (org-ml-match '((> -1) section))
         (--map (org-ml-to-trimmed-string it)))
    => nil
    (->> (org-ml-parse-this-subtree)
         (org-ml-match '((< -4) section))
         (--map (org-ml-to-trimmed-string it)))
    => nil

    ;; properties
    (->> (org-ml-parse-this-subtree)
         (org-ml-match '((:todo-keyword "TODO") section))
         (--map (org-ml-to-trimmed-string it)))
    => '("2")
    (->> (org-ml-parse-this-subtree)
         (org-ml-match '((:todo-keyword nil) section))
         (--map (org-ml-to-trimmed-string it)))
    => '("3" "4")
    (->> (org-ml-parse-this-subtree)
         (org-ml-match '((:todo-keyword "DONE") section))
         (--map (org-ml-to-trimmed-string it)))
    => '("5")

    ;; pred
    (->> (org-ml-parse-this-subtree)
         (org-ml-match '((:pred org-ml-headline-is-done) section))
         (--map (org-ml-to-trimmed-string it)))
    => '("5")
    (->> (org-ml-parse-this-subtree)
         (org-ml-match '((:pred stringp) section)) ; silly but proves my point
         (--map (org-ml-to-trimmed-string it)))
    => nil

    ;; :not
    (->> (org-ml-parse-this-subtree)
         (org-ml-match '((:not (:todo-keyword nil)) section))
         (--map (org-ml-to-trimmed-string it)))
    => '("2" "5")
    (->> (org-ml-parse-this-subtree)
         (org-ml-match '((:not headline) section))
         (--map (org-ml-to-trimmed-string it)))
    => nil
    
    ;; :and
    (->> (org-ml-parse-this-subtree)
         (org-ml-match '((:and (< 2) (:todo-keyword nil)) section))
         (--map (org-ml-to-trimmed-string it)))
    => '("3")
    (->> (org-ml-parse-this-subtree)
         (org-ml-match '((:and (:archivedp t) (:todo-keyword nil)) section))
         (--map (org-ml-to-trimmed-string it)))
    => nil

    ;; :or
    (->> (org-ml-parse-this-subtree)
         (org-ml-match '((:or (:todo-keyword "DONE") (:todo-keyword "TODO")) section))
         (--map (org-ml-to-trimmed-string it)))
    => '("2" "5")
    (->> (org-ml-parse-this-subtree)
         (org-ml-match '((:or (:archivedp t) (:todo-keyword "NEXT")) section))
         (--map (org-ml-to-trimmed-string it)))
    => nil
    (->> (org-ml-parse-this-subtree)
         (org-ml-match '((:or (:todo-keyword "DONE") (:todo-keyword "TODO")) section))
         (--map (org-ml-to-trimmed-string it)))
    => '("2" "5")

    ;; Test the remaining paths of `org-ml--match-make-inner-pattern-form'
    ;; These test cases ensure that:
    ;; - the :any + condition path is correct
    ;; - the condition + :any path is correct
    ;; - the * path is correct
    ;; - the + path is correct
    ;; - the ordering of each above path is correct (assumed because the tests
    ;;   contain nodes with multiple children that have a defined order to be
    ;;   preserved)
    ;;
    ;; Note that all error cases are tested in `org-ml-test.el'
    ;;
    ;; Also note that we assume `org-ml--match-make-condition-form' is
    ;; independent of `org-ml--match-make-inner-pattern-form' which
    ;; liberates us from testing all predicate patterns again below.

    ;; :any (first)
    (:buffer "*_1_* */2/* _*3*_ _/4/_ /*5*/ /_6_/")

    (->> (org-ml-parse-this-element)
         (org-ml-match '(:any (:or bold italic)))
         (--map (org-ml-to-trimmed-string it)))
    
    ;; :any (last)
    => '("/2/" "*3*" "/4/" "*5*")
    (->> (org-ml-parse-this-element)
         (org-ml-match '((:or bold italic) :any))
         (--map (org-ml-to-trimmed-string it)))
    => '("_1_" "/2/" "*5*" "_6_")

    ;; *
    (:buffer "* one"
             "- 1"
             "- 2"
             "  - 3"
             "** two"
             "- 4"
             "- 5"
             "  - 6"
             "** three"
             "- 7"
             "- 8"
             "  - 9")
    (->> (org-ml-parse-this-element)
         (org-ml-match '(:any * item))
         (--map (org-ml-to-trimmed-string it)))
    => '("- 1" "- 2\n  - 3" "- 3" "- 4" "- 5\n  - 6" "- 6" "- 7"
         "- 8\n  - 9" "- 9")
    (->> (org-ml-parse-this-element)
         (org-ml-match '(section plain-list :any * item))
         (--map (org-ml-to-trimmed-string it)))
    => '("- 1" "- 2\n  - 3" "- 3")

    ;; + and ?
    (:buffer "* one"
             "** two"
             "*** three"
             "** four"
             "*** five"
             "**** six")
    (->> (org-ml-parse-this-element)
         (org-ml-match '(headline +))
         (--map (org-ml-to-trimmed-string it)))
    => '("** two\n*** three" "*** three" "** four\n*** five\n**** six"
         "*** five\n**** six" "**** six")
    (->> (org-ml-parse-this-element)
         (org-ml-match '(headline + headline))
         (--map (org-ml-to-trimmed-string it)))
    => '("*** three" "*** five\n**** six" "**** six")
    (->> (org-ml-parse-this-element)
         (org-ml-match '(headline headline \?))
         (--map (org-ml-to-trimmed-string it)))
    => '("** two\n*** three" "** four\n*** five\n**** six" "*** three"
         "*** five\n**** six")
    (->> (org-ml-parse-this-element)
         (org-ml-match '(headline headline \? headline))
         (--map (org-ml-to-trimmed-string it)))
    => '("*** three" "*** five\n**** six" "**** six")

    ;; alternation
    (:buffer "* one"
             "** two"
             "*a* /_b_/"
             "*** three"
             "*c* /_d_/")
    (->> (org-ml-parse-this-element)
         (org-ml-match '(headline section paragraph (bold | italic underline)))
         (--map (org-ml-to-trimmed-string it)))
    => '("*a*" "_b_")
    (->> (org-ml-parse-this-element)
         (org-ml-match '(headline (nil | headline) section paragraph (bold | italic underline)))
         (--map (org-ml-to-trimmed-string it)))
    => '("*a*" "_b_" "*c*" "_d_")

    ;; slicer tests are not here, see `org-ml-test.el'

    :end-hidden)

  (defexamples-content org-ml-match-delete
    nil
    (:buffer "* headline one"
             "** headline two"
             "** headline three"
             "** headline four")
    (:comment "Selectively delete headlines")
    (->> (org-ml-parse-this-subtree)
         (org-ml-match-delete '(headline))
         (org-ml-to-trimmed-string))
    => "* headline one"
    (->> (org-ml-parse-this-subtree)
         (org-ml-match-delete '(:first headline))
         (org-ml-to-trimmed-string))
    => (:result "* headline one"
                "** headline three"
                "** headline four")
    (->> (org-ml-parse-this-subtree)
         (org-ml-match-delete '(:last headline))
         (org-ml-to-trimmed-string))
    => (:result "* headline one"
                "** headline two"
                "** headline three"))

  (defexamples-content org-ml-match-extract
    nil
    (:buffer "pull me /under/")
    (--> (org-ml-parse-this-element)
         (org-ml-match-extract '(:any * italic) it)
         (cons (-map #'org-ml-to-trimmed-string (car it))
               (org-ml-to-trimmed-string (cdr it))))
    => '(("/under/") . "pull me"))

  (defexamples-content org-ml-match-map
    nil

    (:buffer "* headline one"
             "** TODO headline two"
             "** headline three"
             "** headline four")

    (:comment "Selectively mark headlines as DONE")
    (->> (org-ml-parse-this-subtree)
         (org-ml-match-map '(headline)
           (lambda (it) (org-ml-set-property :todo-keyword "DONE" it)))
         (org-ml-to-trimmed-string))
    => (:result "* headline one"
                "** DONE headline two"
                "** DONE headline three"
                "** DONE headline four")
    (->> (org-ml-parse-this-subtree)
         (org-ml-match-map* '(:first headline)
           (org-ml-set-property :todo-keyword "DONE" it))
         (org-ml-to-trimmed-string))
    => (:result "* headline one"
                "** DONE headline two"
                "** headline three"
                "** headline four")
    (->> (org-ml-parse-this-subtree)
         (org-ml-match-map '(:last headline)
           (-partial #'org-ml-set-property :todo-keyword "DONE"))
         (org-ml-to-trimmed-string))
    => (:result "* headline one"
                "** TODO headline two"
                "** headline three"
                "** DONE headline four"))

  ;; (:buffer "* headline"
  ;;          ":PROPERTIES:"
  ;;          ":Effort:   0:30"
  ;;          ":END:")
  ;; (:comment "Match the literal property-drawer node and map the"
  ;;           "node-property inside if the property-drawer exists")
  ;; (let ((hl (org-ml-parse-this-headline)))
  ;;   (-if-let (pd (org-ml-headline-get-property-drawer hl))
  ;;       (->> hl
  ;;            (org-ml-match-map* `(,pd node-property)
  ;;                           (org-ml-set-property :value "1:30" it))
  ;;            (org-ml-to-trimmed-string))
  ;;     (print "...or do something else if no drawer")))
  ;; => (:result "* headline"
  ;;             ":PROPERTIES:"
  ;;             ":Effort:   1:30"
  ;;             ":END:"))
  
  (defexamples-content org-ml-match-mapcat
    nil

    (:buffer "* one"
             "** two")
    (->> (org-ml-parse-this-subtree)
         (org-ml-match-mapcat* '(:first headline)
           (list (org-ml-build-headline! :title-text "1.5" :level 2) it))
         (org-ml-to-trimmed-string))
    => (:result "* one"
                "** 1.5"
                "** two"))

  (defexamples-content org-ml-match-replace
    nil
    (:buffer "*1* 2 *3* 4 *5* 6 *7* 8 *9* 10")
    (->> (org-ml-parse-this-element)
         (org-ml-match-replace '(:any * bold)
           (org-ml-build-bold :post-blank 1 "0"))
         (org-ml-to-trimmed-string))
    => "*0* 2 *0* 4 *0* 6 *0* 8 *0* 10")

  (defexamples-content org-ml-match-insert-before
    nil
    (:buffer "* one"
             "** two"
             "** three")
    (->> (org-ml-parse-this-subtree)
         (org-ml-match-insert-before '(headline)
           (org-ml-build-headline! :title-text "new" :level 2))
         (org-ml-to-trimmed-string))
    => (:result "* one"
                "** new"
                "** two"
                "** new"
                "** three"))

  (defexamples-content org-ml-match-insert-after
    nil
    (:buffer "* one"
             "** two"
             "** three")
    (->> (org-ml-parse-this-subtree)
         (org-ml-match-insert-after '(headline)
           (org-ml-build-headline! :title-text "new" :level 2))
         (org-ml-to-trimmed-string))
    => (:result "* one"
                "** two"
                "** new"
                "** three"
                "** new"))

  (defexamples-content org-ml-match-insert-within
    nil
    (:buffer "* one"
             "** two"
             "** three")
    (->> (org-ml-parse-this-subtree)
         (org-ml-match-insert-within '(headline) 0
           (org-ml-build-headline! :title-text "new" :level 3))
         (org-ml-to-trimmed-string))
    => (:result "* one"
                "** two"
                "*** new"
                "** three"
                "*** new")
    (:comment "The nil pattern denotes top-level element")
    (->> (org-ml-parse-this-subtree)
         (org-ml-match-insert-within nil 1
           (org-ml-build-headline! :title-text "new" :level 2))
         (org-ml-to-trimmed-string))
    => (:result "* one"
                "** two"
                "** new"
                "** three"))

  (defexamples-content org-ml-match-splice
    nil
    (:buffer "* one"
             "** two"
             "** three")
    (let ((L (list
              (org-ml-build-headline! :title-text "new0" :level 2)
              (org-ml-build-headline! :title-text "new1" :level 2))))
      (->> (org-ml-parse-this-subtree)
           (org-ml-match-splice '(0) L)
           (org-ml-to-trimmed-string)))
    => (:result "* one"
                "** new0"
                "** new1"
                "** three"))

  (defexamples-content org-ml-match-splice-before
    nil
    (:buffer "* one"
             "** two"
             "** three")
    (let ((L (list
              (org-ml-build-headline! :title-text "new0" :level 2)
              (org-ml-build-headline! :title-text "new1" :level 2))))
      (->> (org-ml-parse-this-subtree)
           (org-ml-match-splice-before '(0) L)
           (org-ml-to-trimmed-string)))
    => (:result "* one"
                "** new0"
                "** new1"
                "** two"
                "** three"))

  (defexamples-content org-ml-match-splice-after
    nil
    (:buffer "* one"
             "** two"
             "** three")
    (let ((L (list
              (org-ml-build-headline! :title-text "new0" :level 2)
              (org-ml-build-headline! :title-text "new1" :level 2))))
      (->> (org-ml-parse-this-subtree)
           (org-ml-match-splice-after '(0) L)
           (org-ml-to-trimmed-string)))
    => (:result "* one"
                "** two"
                "** new0"
                "** new1"
                "** three"))

  (defexamples-content org-ml-match-splice-within
    nil
    (:buffer "* one"
             "** two"
             "** three"
             "*** four")
    (let ((L (list
              (org-ml-build-headline! :title-text "new0" :level 3)
              (org-ml-build-headline! :title-text "new1" :level 3))))
      (->> (org-ml-parse-this-subtree)
           (org-ml-match-splice-within '(headline) 0 L)
           (org-ml-to-trimmed-string)))
    => (:result "* one"
                "** two"
                "*** new0"
                "*** new1"
                "** three"
                "*** new0"
                "*** new1"
                "*** four")
    (let ((L (list
              (org-ml-build-headline! :title-text "new0" :level 2)
              (org-ml-build-headline! :title-text "new1" :level 2))))
      (->> (org-ml-parse-this-subtree)
           (org-ml-match-splice-within nil 1 L)
           (org-ml-to-trimmed-string)))
    => (:result "* one"
                "** two"
                "** new0"
                "** new1"
                "** three"
                "*** four"))

  (defexamples-content org-ml-match-do
    nil))

(def-example-group "Buffer Side Effects"
  "Map node manipulations into buffers."

  (def-example-subgroup "Insert"
    nil

    (defexamples-content org-ml-insert
      nil
      (:buffer "* one"
               "")
      (:comment "Insert single node")
      (->> (org-ml-build-headline! :title-text "two")
           (org-ml-insert (point-max)))
      $> (:result "* one"
                  "* two")
      (:comment "Insert multiple nodes")
      (->> (org-ml-build-headline! :title-text "two")
           (list (org-ml-build-headline! :title-text "more"))
           (org-ml-insert (point-max)))
      $> (:result "* one"
                  "* more"
                  "* two")

      (:buffer "a *game* or a /boy/")
      (->> (org-ml-build-paragraph! "we don't care if you're")
           (org-ml-insert (point-min)))
      $> (:result "we don't care if you're"
                  "a *game* or a /boy/"))

    (defexamples-content org-ml-insert-tail
      nil
      :begin-hidden
      (:buffer "* one"
               "")
      (:comment "Insert single node")
      (->> (org-ml-build-headline! :title-text "two")
           (org-ml-insert-tail (point-max)))
      $> (:result "* one"
                  "* two")
      (:comment "Insert multiple nodes")
      (->> (org-ml-build-headline! :title-text "two")
           (list (org-ml-build-headline! :title-text "more"))
           (org-ml-insert (point-max)))
      $> (:result "* one"
                  "* more"
                  "* two")

      (:buffer "a *game* or a /boy/")
      (->> (org-ml-build-paragraph! "we don't care if you're")
           (org-ml-insert-tail (point-min)))
      $> (:result "we don't care if you're"
                  "a *game* or a /boy/")
      :end-hidden))

  (def-example-subgroup "Update"
    nil

    (defexamples-content org-ml-update
      nil
      
      (:buffer "* TODO win grammy")
      (->> (org-ml-parse-this-headline)
           (org-ml-update
             (lambda (hl) (org-ml-set-property :todo-keyword "DONE" hl))))
      $> "* DONE win grammy"

      (:buffer "* win grammy [0/0]"
               "- [ ] write punk song"
               "- [ ] get new vocalist"
               "- [ ] sell 2 singles")
      (->> (org-ml-parse-this-headline)
           (org-ml-update*
             (->> (org-ml-match-map '(:any * item) #'org-ml-item-toggle-checkbox it)
                  (org-ml-headline-update-item-statistics))))
      $> (:result "* win grammy [3/3]"
                  "- [X] write punk song"
                  "- [X] get new vocalist"
                  "- [X] sell 2 singles"))

    (defexamples-content org-ml-update-object-at
      nil
      (:buffer "[[http://example.com][desc]]")
      (org-ml-update-object-at* (point)
        (org-ml-set-property :path "//buymoreram.com" it))
      $> "[[http://buymoreram.com][desc]]")

    (defexamples-content org-ml-update-element-at
      nil
      (:buffer "#+call: ktulu()")
      (org-ml-update-element-at* (point)
        (org-ml-set-properties 
         (list :call "cthulhu"
               :inside-header '(:cache no)
               :arguments '("x=4")
               :end-header '(:results html))
         it))
      $> "#+call: cthulhu[:cache no](x=4) :results html")

    (defexamples-content org-ml-update-table-row-at
      nil
      (:buffer "| a | b |")
      (org-ml-update-table-row-at* (point)
        (org-ml-map-children* (cons (org-ml-build-table-cell! "0") it) it))
      $> "| 0 | a | b |")

    (defexamples-content org-ml-update-item-at
      nil
      (:buffer "- [ ] thing")
      (org-ml-update-item-at* (point)
        (org-ml-item-toggle-checkbox it))
      $> "- [X] thing")

    (defexamples-content org-ml-update-headline-at
      nil
      (:buffer "* TODO might get done"
               "* DONE no need to update")
      (org-ml-update-headline-at* (point)
        (org-ml-set-property :todo-keyword "DONE" it))
      $> (:result "* DONE might get done"
                  "* DONE no need to update"))

    (defexamples-content org-ml-update-subtree-at
      nil
      (:buffer "* one"
               "** two"
               "** three"
               "* not updated")
      (org-ml-update-subtree-at* (point)
        (org-ml-headline-demote-subheadline 1 it))
      $> (:result "* one"
                  "** two"
                  "*** three"
                  "* not updated"))

    (defexamples-content org-ml-update-section-at
      nil
      (:buffer "#+key1: VAL1"
               "#+key2: VAL2"
               "* irrelevant headline")
      (:comment "Update the top buffer section before the headlines start")
      (org-ml-update-section-at* (point)
        (org-ml-map-children* (--map (org-ml-map-property :value #'s-downcase it) it) it))
      $> (:result "#+key1: val1"
                  "#+key2: val2"
                  "* irrelevant headline"))

    (defexamples-content org-ml-update-headlines
      nil
      (:buffer "* one"
               "* two"
               "* three")
      (org-ml-update-headlines* 0
        (org-ml-set-property :todo-keyword "DONE" it))
      $> (:result "* DONE one"
                  "* two"
                  "* three")
      (org-ml-update-headlines* '(0 1)
        (org-ml-set-property :todo-keyword "DONE" it))
      $> (:result "* DONE one"
                  "* DONE two"
                  "* three")
      (org-ml-update-headlines* [2 nil]
        (org-ml-set-property :todo-keyword "DONE" it))
      $> (:result "* one"
                  "* DONE two"
                  "* DONE three")
      (org-ml-update-headlines* [2 10]
        (org-ml-set-property :todo-keyword "DONE" it))
      $> (:result "* one"
                  "* DONE two"
                  "* three")

      (:buffer "* one"
               "* two"
               "* three")
      (org-ml-update-headlines* 'all
        (org-ml-set-property :todo-keyword "DONE" it))
      $> (:result "* DONE one"
                  "* DONE two"
                  "* DONE three"))

    (defexamples-content org-ml-update-subtrees
      nil
      (:buffer "* one [/]"
               "** DONE _one"
               "* two [/]"
               "** DONE _one"
               "* three [/]"
               "** DONE _one")
      (org-ml-update-subtrees* 0
        (org-ml-headline-update-todo-statistics))
      $> (:buffer "* one [1/1]"
                  "** DONE _one"
                  "* two [/]"
                  "** DONE _one"
                  "* three [/]"
                  "** DONE _one")
      (org-ml-update-subtrees* '(0 1)
        (org-ml-headline-update-todo-statistics))
      $> (:buffer "* one [1/1]"
                  "** DONE _one"
                  "* two [1/1]"
                  "** DONE _one"
                  "* three [/]"
                  "** DONE _one")
      (org-ml-update-subtrees* [2 nil]
        (org-ml-headline-update-todo-statistics))
      $> (:buffer "* one [/]"
                  "** DONE _one"
                  "* two [1/1]"
                  "** DONE _one"
                  "* three [1/1]"
                  "** DONE _one")
      (org-ml-update-subtrees* [nil 5]
        (org-ml-headline-update-todo-statistics))
      $> (:buffer "* one [1/1]"
                  "** DONE _one"
                  "* two [/]"
                  "** DONE _one"
                  "* three [/]"
                  "** DONE _one")

      (:buffer "* one [/]"
               "** DONE _one"
               "** DONE _two"
               "* two [/]"
               "** DONE _one"
               "** DONE _two")
      (org-ml-do-subtrees* 'all
        (org-ml-headline-update-todo-statistics))
      $> (:buffer "* one [2/2]"
                  "** DONE _one"
                  "** DONE _two"
                  "* two [2/2]"
                  "** DONE _one"
                  "** DONE _two")))

  (def-example-subgroup "Misc"
    nil

    (defexamples-content org-ml-fold
      nil)

    (defexamples-content org-ml-unfold
      nil)))

(provide 'org-ml-examples)
;;; org-ml-examples.el ends here
