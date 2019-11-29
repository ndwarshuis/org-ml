;;; examples.el --- Examples/tests for om.el's API  -*- lexical-binding: t -*-

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

;; Only the first three examples per function are shown in the docs,
;; so make those good.

;;; Code:

;; (require 'om)
(require 's)

(def-example-group "Element parsers"
  "parse shit"

  (defexamples-content om-elem-parse-object-at
    nil
    (:content "*text*")
    (->> (om-elem-parse-object-at 1)
         (om-elem-get-type))
    => 'bold

    :begin-hidden
    (:content "~text~")
    (->> (om-elem-parse-object-at 1)
         (om-elem-get-type))
    => 'code
    ;; TODO add entity
    ;; TODO add export snippet
    (:content "[fn:1:text]")
    (->> (om-elem-parse-object-at 1)
         (om-elem-get-type))
    => 'footnote-reference
    (:content "call_name()")
    (->> (om-elem-parse-object-at 1)
         (om-elem-get-type))
    => 'inline-babel-call
    (:content "src_emacs{}")
    (->> (om-elem-parse-object-at 1)
         (om-elem-get-type))
    => 'inline-src-block
    (:content "/text/")
    (->> (om-elem-parse-object-at 1)
         (om-elem-get-type))
    => 'italic
    (:content "\\\\")
    (->> (om-elem-parse-object-at 1)
         (om-elem-get-type))
    => 'line-break
    ;; TODO add latex frag
    (:content "[[path][desc]]")
    (->> (om-elem-parse-object-at 1)
         (om-elem-get-type))
    => 'link
    (:content "{{{macro}}}")
    (->> (om-elem-parse-object-at 1)
         (om-elem-get-type))
    => 'macro
    (:content "<<<text>>>")
    (->> (om-elem-parse-object-at 1)
         (om-elem-get-type))
    => 'radio-target
    (:content "[1/2]")
    (->> (om-elem-parse-object-at 1)
         (om-elem-get-type))
    => 'statistics-cookie
    (:content "+text+")
    (->> (om-elem-parse-object-at 1)
         (om-elem-get-type))
    => 'strike-through
    ;; TODO this is confusing for docs
    (:content "a_b")
    (->> (om-elem-parse-object-at 3)
         (om-elem-get-type))
    => 'subscript
    (:content "a^b")
    (->> (om-elem-parse-object-at 3)
         (om-elem-get-type))
    => 'superscript
    (:content "| a |")
    (->> (om-elem-parse-object-at 2)
         (om-elem-get-type))
    => 'table-cell
    (:content "<<text>>")
    (->> (om-elem-parse-object-at 1)
         (om-elem-get-type))
    => 'target
    :end-hidden

    (:content "[2019-01-01 Tue]")
    (->> (om-elem-parse-object-at 1)
         (om-elem-get-type))
    => 'timestamp

    :begin-hidden
    (:content "_text_")
    (->> (om-elem-parse-object-at 1)
         (om-elem-get-type))
    => 'underline
    (:content "=text=")
    (->> (om-elem-parse-object-at 1)
         (om-elem-get-type))
    => 'verbatim
    :end-hidden

    (:content "- notme")
    (:comment "Return nil when parsing an element")
    (om-elem-parse-object-at 1)
    => nil)

  (defexamples-content om-elem-parse-element-at
    nil
    (:content "#+CALL: ktulu()")
    (->> (om-elem-parse-element-at 1)
         (om-elem-get-type))
    => 'babel-call

    :begin-hidden
    (:content "#+BEGIN_CENTER"
              "#+END_CENTER")
    (->> (om-elem-parse-element-at 1)
         (om-elem-get-type))
    => 'center-block
    (:content "CLOCK: [2019-01-01 Tue]")
    (->> (om-elem-parse-element-at 1)
         (om-elem-get-type))
    => 'clock
    (:content "# oops I looked")
    (->> (om-elem-parse-element-at 1)
         (om-elem-get-type))
    => 'comment
    (:content "#+BEGIN_COMMENT"
              "oops I looked again"
              "#+END_COMMENT")
    (->> (om-elem-parse-element-at 1)
         (om-elem-get-type))
    => 'comment-block
    (:content "%%(diary of a madman)")
    (->> (om-elem-parse-element-at 1)
         (om-elem-get-type))
    => 'diary-sexp
    (:content ":DRAWER:"
              "- underwear"
              "- savings account"
              ":END:")
    (->> (om-elem-parse-element-at 1)
         (om-elem-get-type))
    => 'drawer
    (:content "#+BEGIN countdown"
              "#+END")
    (->> (om-elem-parse-element-at 1)
         (om-elem-get-type))
    => 'dynamic-block
    (:content "#+BEGIN_EXAMPLE"
              "#+END_EXAMPLE")
    (->> (om-elem-parse-element-at 1)
         (om-elem-get-type))
    => 'example-block
    (:content "#+BEGIN_EXPORT latex"
              "#+END_EXPORT")
    (->> (om-elem-parse-element-at 1)
         (om-elem-get-type))
    => 'export-block
    (:content ": mini mini mini")
    (->> (om-elem-parse-element-at 1)
         (om-elem-get-type))
    => 'fixed-width
    (:content "[fn:1]")
    (->> (om-elem-parse-element-at 1)
         (om-elem-get-type))
    => 'footnote-definition
    (:content "* murder, young girl killed"
              "* desperate shooting at echo's hill")
    (->> (om-elem-parse-element-at 1)
         (om-elem-get-type))
    => 'headline
    (:content "-----")
    (->> (om-elem-parse-element-at 1)
         (om-elem-get-type))
    => 'horizontal-rule
    ;; TODO add inlinetask
    (:content "#+QUOTE: unquote")
    (->> (om-elem-parse-element-at 1)
         (om-elem-get-type))
    => 'keyword
    ;; TODO add latex env
    (:content "* headline"
              ":PROPERTIES:"
              ":key: val"
              ":END:")
    (->> (om-elem-parse-element-at 25)
         (om-elem-get-type))
    => 'node-property
    (:content "Just for the record"
              "The weather today is slightly sarcastic with a good chance of"
              "A. Indifference and B. disinterest in what the critics say")
    (->> (om-elem-parse-element-at 1)
         (om-elem-get-type))
    => 'paragraph
    :end-hidden
    
    (:content "- plain-list")
    (:comment "Give the plain-list, not the item for this function")
    (->> (om-elem-parse-element-at 1)
         (om-elem-get-type))
    => 'plain-list

    :begin-hidden
    (:content "* deadhead"
              "DEADLINE: [2019-01-01 Tue]")
    (->> (om-elem-parse-element-at 12)
         (om-elem-get-type))
    => 'planning
    (:content "* headline"
              ":PROPERTIES:"
              ":END:")
    (->> (om-elem-parse-element-at 12)
         (om-elem-get-type))
    => 'property-drawer
    (:content "#+BEGIN_QUOTE"
              "Oh glorious cheeseburger, we bow to thee"
              "The secrets of the universe are between the buns"
              "#+END_QUOTE")
    (->> (om-elem-parse-element-at 12)
         (om-elem-get-type))
    => 'quote-block
    (:content "#+begin_dot dot.png"
              "#+end_dot")
    (->> (om-elem-parse-element-at 1)
         (om-elem-get-type))
    => 'special-block
    (:content "#+BEGIN_SRC emacs"
              "(launch-missiles)"
              "#+END_SRC")
    (->> (om-elem-parse-element-at 1)
         (om-elem-get-type))
    => 'src-block
    :end-hidden
    
    (:content "| R | A |"
              "| G | E |")
    (:comment "Return a table, not the table-row for this function")
    (->> (om-elem-parse-element-at 1)
         (om-elem-get-type))
    => 'table

    :begin-hidden
    (:content "#+BEGIN_VERSE"
              "#+END_VERSE")
    (->> (om-elem-parse-element-at 1)
         (om-elem-get-type))
    => 'verse-block
    :end-hidden)

  (defexamples-content om-elem-parse-headline-at
    nil
    (:content "* headline")
    (:comment "Return the headline itself")
    (->> (om-elem-parse-headline-at 1)
         (om-elem-to-trimmed-string))
    => "* headline"
    (:content "* headline"
              "section crap")
    (:comment "Return headline and section")
    (->> (om-elem-parse-headline-at 1)
         (om-elem-to-trimmed-string))
    => (:result "* headline"
                "section crap")
    (:comment "Return headline when point is in the section")
    (->> (om-elem-parse-headline-at 12)
         (om-elem-to-trimmed-string))
    => (:result "* headline"
                "section crap")
    (:content "* headline"
              "section crap"
              "** not parsed")
    (:comment "Don't parse any subheadlines")
    (->> (om-elem-parse-headline-at 1)
         (om-elem-to-trimmed-string))
    => (:result "* headline"
                "section crap")
    (:content "nothing nowhere")
    (:comment "Return nil if not under a headline")
    (->> (om-elem-parse-headline-at 1)
         (om-elem-to-trimmed-string))
    => "")

  (defexamples-content om-elem-parse-subtree-at
    nil
    (:content "* headline")
    (:comment "Return the headline itself")
    (->> (om-elem-parse-subtree-at 1)
         (om-elem-to-trimmed-string))
    => "* headline"
    (:content "* headline"
              "section crap")
    (:comment "Return headline and section")
    (->> (om-elem-parse-subtree-at 1)
         (om-elem-to-trimmed-string))
    => (:result "* headline"
                "section crap")
    (:comment "Return headline when point is in the section")
    (->> (om-elem-parse-subtree-at 12)
         (om-elem-to-trimmed-string))
    => (:result "* headline"
                "section crap")
    (:content "* headline"
              "section crap"
              "** parsed")
    (:comment "Return all the subheadlines")
    (->> (om-elem-parse-subtree-at 1)
         (om-elem-to-trimmed-string))
    => (:result "* headline"
                "section crap"
                "** parsed")
    (:content "nothing nowhere")
    (:comment "Return nil if not under a headline")
    (->> (om-elem-parse-subtree-at 1)
         (om-elem-to-trimmed-string))
    => "")

  (defexamples-content om-elem-parse-item-at
    nil
    (:content "- item")
    (:comment "Return the item itself")
    (->> (om-elem-parse-item-at 1)
         (om-elem-to-trimmed-string))
    => "- item"
    (:comment "Also return the item when not at beginning of line")
    (->> (om-elem-parse-item-at 5)
         (om-elem-to-trimmed-string))
    => "- item"
    (:content "- item"
              "  - item 2")
    (:comment "Return item and its subitems")
    (->> (om-elem-parse-item-at 1)
         (om-elem-to-trimmed-string))
    => (:result "- item"
                "  - item 2")
    (:content "* not item")
    (:comment "Return nil if not an item")
    (->> (om-elem-parse-item-at 1)
         (om-elem-to-trimmed-string))
    => "")

  (defexamples-content om-elem-parse-table-row-at
    nil
    (:content "| bow | stroke |")
    (:comment "Return the row itself")
    (->> (om-elem-parse-table-row-at 1)
         (om-elem-to-trimmed-string))
    => "| bow | stroke |"
    (:comment "Also return the row when not at beginning of line")
    (->> (om-elem-parse-table-row-at 5)
         (om-elem-to-trimmed-string))
    => "| bow | stroke |"
    (:content "- bow and arrow choke")
    (:comment "Return nil if not a table-row")
    (->> (om-elem-parse-table-row-at 1)
         (om-elem-to-trimmed-string))
    => "")
  
  (defexamples-content om-elem-parse-section-at
    nil
    (:content "over headline"
              "* headline"
              "under headline")
    (:comment "Return the section above the headline")
    (->> (om-elem-parse-section-at 1)
         (om-elem-to-trimmed-string))
    => "over headline"
    (:comment "Return the section under headline")
    (->> (om-elem-parse-section-at 25)
         (om-elem-to-trimmed-string))
    => "under headline"
    (:content "* headline"
              "** subheadline")
    (:comment "Return nil if no section under headline")
    (->> (om-elem-parse-section-at 1)
         (om-elem-to-trimmed-string))
    => ""
    (:content "")
    (:comment "Return nil if no section at all")
    (->> (om-elem-parse-section-at 1)
         (om-elem-to-trimmed-string))
    => ""))

(def-example-group "Printing functions"
  "Functions to nicely print elements"

  (defexamples om-elem-to-string
    (om-elem-to-string
     '(bold
       (:begin 1 :end 5 :parent nil :post-blank 0 :post-affiliated nil)
       "text"))
    => "*text*"
    (om-elem-to-string
     '(bold
       (:begin 1 :end 5 :parent nil :post-blank 3 :post-affiliated nil)
       "text"))
    => "*text*   "
    (om-elem-to-string nil) => "")

  (defexamples om-elem-to-trimmed-string
    (om-elem-to-trimmed-string
     '(bold
       (:begin 1 :end 5 :parent nil :post-blank 0 :post-affiliated nil)
       "text"))
    => "*text*"
    (om-elem-to-trimmed-string
     '(bold
       (:begin 1 :end 5 :parent nil :post-blank 3 :post-affiliated nil)
       "text"))
    => "*text*"
    (om-elem-to-trimmed-string nil) => "")

  )

(def-example-group "Builder Functions"
  "Functions that build elements and objects"

  (def-example-subgroup "Object Builders"
    "build objects"

    (defexamples om-elem-build-code
      (->> (om-elem-build-code "text")
           (om-elem-to-trimmed-string)) => "~text~")

    ;; TODO add entity

    (defexamples om-elem-build-inline-babel-call
      (->> (om-elem-build-inline-babel-call "name")
           (om-elem-to-trimmed-string)) => "call_name()"
      (->> (om-elem-build-inline-babel-call "name" :arguments '("n=4"))
           (om-elem-to-trimmed-string)) => "call_name(n=4)"
      (->> (om-elem-build-inline-babel-call "name" :inside-header '(:key val))
           (om-elem-to-trimmed-string)) => "call_name[:key val]()"
      (->> (om-elem-build-inline-babel-call "name" :end-header '(:key val))
           (om-elem-to-trimmed-string)) => "call_name()[:key val]")

    (defexamples om-elem-build-inline-src-block
      (->> (om-elem-build-inline-src-block "lang" "value")
           (om-elem-to-trimmed-string)) => "src_lang{value}"
      (->> (om-elem-build-inline-src-block "lang" "value" :parameters '(:key val))
           (om-elem-to-trimmed-string)) => "src_lang[:key val]{value}")

    (defexamples om-elem-build-line-break
      (->> (om-elem-build-line-break)
           (om-elem-to-trimmed-string)) => "\\\\")

    (defexamples om-elem-build-statistics-cookie
      (->> (om-elem-build-statistics-cookie '(nil))
           (om-elem-to-trimmed-string)) => "[%]"
      (->> (om-elem-build-statistics-cookie '(50))
           (om-elem-to-trimmed-string)) => "[50%]"
      (->> (om-elem-build-statistics-cookie '(1 3))
           (om-elem-to-trimmed-string)) => "[1/3]"
      (->> (om-elem-build-statistics-cookie '(nil nil))
           (om-elem-to-trimmed-string)) => "[/]")

    (defexamples om-elem-build-target
      (->> (om-elem-build-target "text")
           (om-elem-to-trimmed-string)) => "<<text>>")

    (defexamples om-elem-build-timestamp
      (->> (om-elem-build-timestamp 'inactive '(2019 1 15))
           (om-elem-to-trimmed-string)) => "[2019-01-15 Tue]"
      (->> (om-elem-build-timestamp 'inactive '(2019 1 15 12 30))
           (om-elem-to-trimmed-string)) => "[2019-01-15 Tue 12:30]"
      (->> (om-elem-build-timestamp 'inactive '(2019 1 15) :end '(2020 1 1))
           (om-elem-to-trimmed-string)) => "[2019-01-15 Tue]--[2020-01-01 Wed]")

    (defexamples om-elem-build-verbatim
      (->> (om-elem-build-verbatim "text")
           (om-elem-to-trimmed-string)) => "=text="))

  (def-example-subgroup "Recursive Object Builders"
    "build recursive objects"

    (defexamples om-elem-build-bold
      (->> (om-elem-build-bold "text")
           (om-elem-to-trimmed-string)) => "*text*")

    (defexamples om-elem-build-footnote-reference
      (->> (om-elem-build-footnote-reference)
           (om-elem-to-trimmed-string)) => "[fn:]"
      (->> (om-elem-build-footnote-reference :label "label")
           (om-elem-to-trimmed-string)) => "[fn:label]"
      (->> (om-elem-build-footnote-reference :label "label" "content")
           (om-elem-to-trimmed-string)) => "[fn:label:content]")

    (defexamples om-elem-build-italic
      (->> (om-elem-build-italic "text")
           (om-elem-to-trimmed-string)) => "/text/")

    (defexamples om-elem-build-link
      (->> (om-elem-build-link "target")
           (om-elem-to-trimmed-string)) => "[[target]]"
      (->> (om-elem-build-link "target" :type "file")
           (om-elem-to-trimmed-string)) => "[[file:target]]"
      (->> (om-elem-build-link "target" "desc")
           (om-elem-to-trimmed-string)) => "[[target][desc]]")

    (defexamples om-elem-build-radio-target
      (->> (om-elem-build-radio-target "text")
           (om-elem-to-trimmed-string)) => "<<<text>>>")

    (defexamples om-elem-build-strike-through
      (->> (om-elem-build-strike-through "text")
           (om-elem-to-trimmed-string)) => "+text+")

    (defexamples om-elem-build-superscript
      (->> (om-elem-build-superscript "text")
           (om-elem-to-trimmed-string)) => "^text")

    (defexamples om-elem-build-subscript
      (->> (om-elem-build-subscript "text")
           (om-elem-to-trimmed-string)) => "_text")

    (defexamples om-elem-build-table-cell
      (->> (om-elem-build-table-cell "text")
           (om-elem-build-table-row)
           (om-elem-to-trimmed-string)) => "| text |")

    (defexamples om-elem-build-underline
      (->> (om-elem-build-underline "text")
           (om-elem-to-trimmed-string)) => "_text_"))

  (def-example-subgroup "Elements"
    "build elements"

    (defexamples om-elem-build-babel-call
      (->> (om-elem-build-babel-call "name")
           (om-elem-to-trimmed-string)) => "#+CALL: name()"
      (->> (om-elem-build-babel-call "name" :arguments '("arg=x"))
           (om-elem-to-trimmed-string)) => "#+CALL: name(arg=x)"
      (->> (om-elem-build-babel-call "name" :inside-header '(:key val))
           (om-elem-to-trimmed-string)) => "#+CALL: name[:key val]()"
      (->> (om-elem-build-babel-call "name" :end-header '(:key val))
           (om-elem-to-trimmed-string)) => "#+CALL: name() :key val")

    (defexamples om-elem-build-clock
      (->> (om-elem-build-clock '(2019 1 1 0 0))
           (om-elem-to-trimmed-string))
      => "CLOCK: [2019-01-01 Tue 00:00]"
      (->> (om-elem-build-clock '(2019 1 1 0 0) :end '(2019 1 1 1 0))
           (om-elem-to-trimmed-string))
      => "CLOCK: [2019-01-01 Tue 00:00]--[2019-01-01 Tue 01:00] =>  1:00")

    (defexamples om-elem-build-comment
      (->> (om-elem-build-comment "text")
           (om-elem-to-trimmed-string)) => "# text")

    (defexamples om-elem-build-comment-block
      (->> (om-elem-build-comment-block "text")
           (om-elem-to-trimmed-string)) => (:result "#+BEGIN_COMMENT"
                                                    "text"
                                                    "#+END_COMMENT"))

    (defexamples om-elem-build-diary-sexp
      (->> (om-elem-build-diary-sexp '(text))
           (om-elem-to-trimmed-string)) => "%%(text)")

    (defexamples om-elem-build-example-block
      (->> (om-elem-build-example-block "text")
           (om-elem-to-trimmed-string)) => (:result "#+BEGIN_EXAMPLE"
                                                    "text"
                                                    "#+END_EXAMPLE")
      (->> (om-elem-build-example-block "text" :switches '("switches"))
           (om-elem-to-trimmed-string)) => (:result "#+BEGIN_EXAMPLE switches"
                                                    "text"
                                                    "#+END_EXAMPLE"))

    (defexamples om-elem-build-export-block
      (->> (om-elem-build-export-block "type" "value\n")
           (om-elem-to-trimmed-string)) => (:result "#+BEGIN_EXPORT type"
                                                    "value"
                                                    "#+END_EXPORT"))

    (defexamples om-elem-build-fixed-width
      (->> (om-elem-build-fixed-width "text")
           (om-elem-to-trimmed-string)) => ": text")

    (defexamples om-elem-build-horizontal-rule
      (->> (om-elem-build-horizontal-rule)
           (om-elem-to-trimmed-string)) => "-----")

    (defexamples om-elem-build-keyword
      (->> (om-elem-build-keyword "FILETAGS" "tmsu")
           (om-elem-to-trimmed-string)) => "#+FILETAGS: tmsu")

    (defexamples om-elem-build-latex-environment
      (->> (om-elem-build-latex-environment "env" "text")
           (om-elem-to-trimmed-string)) => (:result "\\begin{env}"
                                                    "text"
                                                    "\\end{env}"))

    (defexamples om-elem-build-node-property
      (->> (om-elem-build-node-property "key" "val")
           (om-elem-to-trimmed-string)) => ":key:      val")

    (defexamples om-elem-build-planning
      (->> (om-elem-build-planning :closed '(2019 1 1))
           (om-elem-to-trimmed-string)) => "CLOSED: [2019-01-01 Tue]"
      (->> (om-elem-build-planning :scheduled '(2019 1 1))
           (om-elem-to-trimmed-string)) => "SCHEDULED: [2019-01-01 Tue]"
      (->> (om-elem-build-planning :deadline '(2019 1 1))
           (om-elem-to-trimmed-string)) => "DEADLINE: [2019-01-01 Tue]")

    (defexamples om-elem-build-src-block
      (->> (om-elem-build-src-block "body")
           (om-elem-to-trimmed-string)) => (:result "#+BEGIN_SRC"
                                                    "  body"
                                                    "#+END_SRC")
      (->> (om-elem-build-src-block "body" :language "emacs-lisp")
           (om-elem-to-trimmed-string)) => (:result "#+BEGIN_SRC emacs-lisp"
                                                    "  body"
                                                    "#+END_SRC")
      ;; TODO pretty sure this makes no sense...
      (->> (om-elem-build-src-block "body" :switches '("-n 20" "-r"))
           (om-elem-to-trimmed-string)) => (:result "#+BEGIN_SRC -n 20 -r"
                                                    "  body"
                                                    "#+END_SRC")
      ;; TODO and this...
      (->> (om-elem-build-src-block "body" :parameters '(:key val))
           (om-elem-to-trimmed-string)) => (:result "#+BEGIN_SRC :key val"
                                                    "  body"
                                                    "#+END_SRC"))

    (defexamples om-elem-build-table-row-hline
      (->>  (om-elem-build-table
             (om-elem-build-table-row
              (om-elem-build-table-cell "text"))
             (om-elem-build-table-row-hline))
            (om-elem-to-trimmed-string)) => (:result "| text |"
                                                     "|------|")))

  (def-example-subgroup "Object Containers"
    "build object containers"
    ;; TODO add paragraph
    ;; TODO add table-row
    ;; TODO add verse-block
    )

  (def-example-group "Greater Elements"
    "Build greater elements"

    (defexamples om-elem-build-center-block
      (->> (om-elem-build-paragraph "text")
           (om-elem-build-center-block)
           (om-elem-to-trimmed-string)) => (:result "#+BEGIN_CENTER"
                                                    "text"
                                                    "#+END_CENTER"))

    (defexamples om-elem-build-drawer
      (->> (om-elem-build-paragraph "text")
           (om-elem-build-drawer "NAME")
           (om-elem-to-trimmed-string)) => (:result ":NAME:"
                                                    "text"
                                                    ":END:"))

    (defexamples om-elem-build-footnote-definition
      (->> (om-elem-build-paragraph "footnote contents")
           (om-elem-build-footnote-definition "label")
           (om-elem-to-trimmed-string)) => "[fn:label] footnote contents")

    ;; TODO, need tags and archive example, cumbersome since these outputs
    ;; have lots of whitespace
    (defexamples om-elem-build-headline
      (->> (om-elem-build-headline)
           (om-elem-to-trimmed-string)) => "*"
      (->> (om-elem-build-headline :title '("dummy"))
           (om-elem-to-trimmed-string)) => "* dummy"
      (->> (om-elem-build-headline :title '("dummy") :level 3)
           (om-elem-to-trimmed-string)) => "*** dummy"
      (->> (om-elem-build-headline :title '("dummy") :todo-keyword "DONE")
           (om-elem-to-trimmed-string)) => "* DONE dummy"
      (->> (om-elem-build-headline :title '("dummy") :priority ?A)
           (om-elem-to-trimmed-string)) => "* [#A] dummy"
      (->> (om-elem-build-headline :title '("dummy") :footnote-section-p t)
           (om-elem-to-trimmed-string)) => "* Footnotes"
      (->> (om-elem-build-headline :title '("dummy") :commentedp t)
           (om-elem-to-trimmed-string)) => "* COMMENT dummy")

    (defexamples om-elem-build-item
      (->> (om-elem-build-paragraph "item contents")
           (om-elem-build-item)
           (om-elem-to-trimmed-string)) => "- item contents"
      (->> (om-elem-build-paragraph "item contents")
           (om-elem-build-item :bullet 1)
           (om-elem-to-trimmed-string)) => "1. item contents"
      (->> (om-elem-build-paragraph "item contents")
           (om-elem-build-item :checkbox 'on)
           (om-elem-to-trimmed-string)) => "- [X] item contents"
      (->> (om-elem-build-paragraph "item contents")
           (om-elem-build-item :tag '("tmsu"))
           (om-elem-to-trimmed-string)) => "- tmsu :: item contents"
      (->> (om-elem-build-paragraph "item contents")
           (om-elem-build-item :counter 10)
           (om-elem-to-trimmed-string)) => "- [@10] item contents")

    (defexamples om-elem-build-plain-list
      (->> (om-elem-build-paragraph "item contents")
           (om-elem-build-item)
           (om-elem-build-plain-list)
           (om-elem-to-trimmed-string)) => "- item contents")

    (defexamples om-elem-build-property-drawer
      (->> (om-elem-build-node-property "key" "val")
           (om-elem-build-property-drawer)
           (om-elem-to-trimmed-string)) => (:result ":PROPERTIES:"
                                                    ":key:      val"
                                                    ":END:"))

    (defexamples om-elem-build-quote-block
      (->> (om-elem-build-paragraph "quoted stuff")
           (om-elem-build-quote-block)
           (om-elem-to-trimmed-string)) => (:result "#+BEGIN_QUOTE"
                                                    "quoted stuff"
                                                    "#+END_QUOTE"))

    (defexamples om-elem-build-section
      (->> (om-elem-build-paragraph "text")
           (om-elem-build-section)
           (om-elem-to-trimmed-string)) => "text")

    (defexamples om-elem-build-table
      (->> (om-elem-build-table-cell "cell")
           (om-elem-build-table-row)
           (om-elem-build-table)
           (om-elem-to-trimmed-string)) => "| cell |")))

(def-example-group "Type Predicate functions"
  "Testing types of elements"

  ;; (defexamples-content om-elem-contains-point-p
  ;;   nil
  ;;   (:content "* headline 1"
  ;;             "* headline 2")
  ;;   (:comment "The headline is parsed from 'point-min'")
  ;;   (->> (om-elem-parse-this-headline)
  ;;        (om-elem-contains-point-p (point-min)))
  ;;   => t
  ;;   (->> (om-elem-parse-this-headline)
  ;;        (om-elem-contains-point-p (point-max)))
  ;;   => nil)

  ;; (defexamples-content om-elem-contents-contains-point-p
  ;;   nil
  ;;   (:content "* this is a dummy"
  ;;             "filled with nonsense")
  ;;   (->> (om-elem-parse-this-headline)
  ;;        (om-elem-contents-contains-point-p (point-min)))
  ;;   => nil
  ;;   (->> (om-elem-parse-this-headline)
  ;;        (om-elem-contents-contains-point-p (point-max)))
  ;;   => t)

  ;; (defexamples-content om-elem-is-type-p
  ;;   nil
  ;;   (:content "*ziltoid*")
  ;;   (->> (om-elem-parse-this-object)
  ;;        (om-elem-is-type-p 'bold))
  ;;   => t
  ;;   (->> (om-elem-parse-this-object)
  ;;        (om-elem-is-type-p 'italic))
  ;;   => nil)

  ;; (defexamples-content om-elem-is-any-type-p
  ;;   nil
  ;;   (:content "*ziltoid*")
  ;;   (->> (om-elem-parse-this-object)
  ;;        (om-elem-is-any-type-p '(bold)))
  ;;   => t
  ;;   (->> (om-elem-parse-this-object)
  ;;        (om-elem-is-any-type-p '(bold italic)))
  ;;   => t
  ;;   (->> (om-elem-parse-this-object)
  ;;        (om-elem-is-any-type-p '(italic)))
  ;;   => nil)
  )

(def-example-group "Property functions"
  "Functions to get, set, and map properties."

  (def-example-subgroup "Generic"
    nil

    (defexamples-content om-elem-set-property
      "Set property PROP to VALUE in ELEM."

      (:content "#+CALL: ktulu()")
      (->> (om-elem-parse-this-element)
           (om-elem-set-property :call "cthulhu")
           (om-elem-set-property :inside-header '(:cache no))
           (om-elem-set-property :arguments '("x=4"))
           (om-elem-set-property :end-header '(:exports results))
           (om-elem-to-trimmed-string))
      => "#+CALL: cthulhu[:cache no](x=4) :exports results"

      :begin-hidden
      (:content "CLOCK: [2019-01-01 Tue]")
      (->> (om-elem-parse-this-element)
           (om-elem-set-property
            :value (om-elem-build-timestamp
                    'inactive '(2019 1 1) :end '(2019 1 2)))
           (om-elem-to-trimmed-string))
      => "CLOCK: [2019-01-01 Tue]--[2019-01-02 Wed] => 24:00"

      (:content "~learn to~")
      (->> (om-elem-parse-this-object)
           (om-elem-set-property :value "why?")
           (om-elem-to-trimmed-string))
      => "~why?~"

      (:content "# not here")
      (->> (om-elem-parse-this-element)
           (om-elem-set-property :value "still not here")
           (om-elem-to-trimmed-string))
      => "# still not here"

      (:content "#+BEGIN_COMMENT"
                "not here"
                "#+END_COMMENT")
      (->> (om-elem-parse-this-element)
           (om-elem-set-property :value "still not here")
           (om-elem-to-trimmed-string))
      => (:result "#+BEGIN_COMMENT"
                  "still not here"
                  "#+END_COMMENT")

      ;; TODO add diary-sexp

      (:content ":LOGBOOK:"
                ":END:")
      (->> (om-elem-parse-this-element)
           (om-elem-set-property :drawer-name "BOOKOFSOULS")
           (om-elem-to-trimmed-string))
      => (:result ":BOOKOFSOULS:"
                  ":END:")

      (:content "#+BEGIN: blockhead"
                "#+END:")
      (->> (om-elem-parse-this-element)
           (om-elem-set-property :block-name "blockfoot")
           (om-elem-set-property :arguments '(:cache no))
           (om-elem-to-trimmed-string))
      => (:result "#+BEGIN: blockfoot :cache no"
                  "#+END:")

      (:content "\\pi")
      (->> (om-elem-parse-this-object)
           (om-elem-set-property :name "gamma")
           (om-elem-set-property :use-brackets-p t)
           (om-elem-to-trimmed-string))
      => "\\gamma{}"

      ;; TODO test preserve indentation...
      (:content "#+BEGIN_EXAMPLE"
                "#+END_EXAMPLE")
      (->> (om-elem-parse-this-element)
           (om-elem-set-property :switches '("-n"))
           (om-elem-set-property :value "example.com")
           (om-elem-to-trimmed-string))
      => (:content "#+BEGIN_EXAMPLE -n"
                   "example.com"
                   "#+END_EXAMPLE")

      (:content "#+BEGIN_EXPORT latex"
                "#+END_EXPORT")
      (->> (om-elem-parse-this-element)
           (om-elem-set-property :type "domestic")
           (om-elem-set-property :value "bullets, bombs, and bigotry")
           (om-elem-to-trimmed-string))
      => (:content "#+BEGIN_EXPORT domestic"
                   "bullets, bombs, and bigotry"
                   "#+END_EXPORT")

      (:content "@@back-end:value@@")
      (->> (om-elem-parse-this-object)
           (om-elem-set-property :back-end "latex")
           (om-elem-set-property :value "new-value")
           (om-elem-to-trimmed-string))
      => "@@latex:new-value@@"

      (:content ": fixed")
      (->> (om-elem-parse-this-element)
           (om-elem-set-property :value "unfixed")
           (om-elem-to-trimmed-string))
      => ": unfixed"

      (:content "[fn:whitelabel] society")
      (->> (om-elem-parse-this-element)
           (om-elem-set-property :label "blacklabel")
           (om-elem-to-trimmed-string))
      => "[fn:blacklabel] society"

      ;; TODO test footnote section
      (:content "* dummy"
                "stuff")
      (->> (om-elem-parse-this-element)
           (om-elem-set-property :archivedp t)
           (om-elem-set-property :commentedp t)
           (om-elem-set-property :level 2)
           (om-elem-set-property :pre-blank 1)
           (om-elem-set-property :priority ?A)
           (om-elem-set-property :tags '("tmsu"))
           (om-elem-set-property :title '("smartie"))
           (om-elem-set-property :todo-keyword "TODO")
           (om-elem-to-trimmed-string))
      => (:result "** TODO COMMENT [#A] smartie                                   :tmsu:ARCHIVE:"
                  ""
                  "stuff")

      (:content "call_kthulu()")
      (->> (om-elem-parse-this-object)
           (om-elem-set-property :call "cthulhu")
           (om-elem-set-property :inside-header '(:cache no))
           (om-elem-set-property :arguments '("x=4"))
           (om-elem-set-property :end-header '(:exports results))
           (om-elem-to-trimmed-string))
      => "call_cthulhu[:cache no](x=4)[:exports results]"

      (:content "src_emacs{(print 'yeah-boi)}")
      (->> (om-elem-parse-this-object)
           (om-elem-set-property :language "python")
           (om-elem-set-property :parameters '(:cache no))
           (om-elem-set-property :value "print \"yeah boi\"")
           (om-elem-to-trimmed-string))
      => "src_python[:cache no]{print \"yeah boi\"}"
      :end-hidden

      (:content "- thing")
      (->> (om-elem-parse-this-item)
           (om-elem-set-property :bullet 1)
           (om-elem-set-property :checkbox 'on)
           (om-elem-set-property :counter 2)
           (om-elem-set-property :tag '("tmsu"))
           (om-elem-to-trimmed-string))
      => "1. [@2] [X] tmsu :: thing"

      :begin-hidden
      (:content "#+KEY: VAL")
      (->> (om-elem-parse-this-element)
           (om-elem-set-property :key "kee")
           (om-elem-set-property :value "vahl")
           (om-elem-to-trimmed-string))
      => "#+kee: vahl"

      ;; this is stupid, who would ever do this?
      (:content "\begin{env}"
                "body"
                "\end{env}")
      (->> (om-elem-parse-this-element)
           (om-elem-set-property :value "\begin{vne}\nbody\end{vne}")
           (om-elem-to-trimmed-string))
      => (:content "\begin{vne}"
                   "body"
                   "\end{vne}")

      ;; TODO this is also stupid...
      (:content "$2+2=4$")
      (->> (om-elem-parse-this-object)
           (om-elem-set-property :value "$2+2=5$")
           (om-elem-to-trimmed-string))
      => "$2+2=5$"

      (:content "https://example.com")
      (->> (om-elem-parse-this-object)
           (om-elem-set-property :path "/dev/null")
           (om-elem-set-property :type "file")
           (om-elem-set-property :format 'bracket)
           (om-elem-to-trimmed-string))
      => "[[file:/dev/null]]"

      (:content "{{{economics}}}")
      (->> (om-elem-parse-this-object)
           (om-elem-set-property :key "freakonomics")
           (om-elem-set-property :args '("x=4" "y=2"))
           (om-elem-to-trimmed-string))
      => "{{{freakonomics(x=4,y=2)}}}"

      (:content "* dummy"
                ":PROPERTIES:"
                ":KEY: VAL"
                ":END:")
      ;; TODO need public function
      (->> (om-elem-parse-this-headline)
           (om-elem--headline-get-node-properties)
           (-first-item)
           (om-elem-set-property :key "kee")
           (om-elem-set-property :value "vahl")
           (om-elem-to-trimmed-string))
      => ":kee:      vahl"

      (:content "* dummy"
                "CLOSED: [2019-01-01 Tue]")
      ;; TODO need public function
      (->> (om-elem-parse-this-headline)
           (om-elem--headline-get-planning)
           (om-elem-set-property
            :closed (om-elem-build-timestamp 'inactive '(2019 1 2)))
           (om-elem-to-trimmed-string))
      => "CLOSED: [2019-01-02 Wed]"

      (:content "#+BEGIN_special"
                "#+END_special")
      (->> (om-elem-parse-this-element)
           (om-elem-set-property :type "talent")
           (om-elem-to-trimmed-string))
      => (:result "#+BEGIN_talent"
                  "#+END_talent")

      (:content "#+BEGIN_SRC"
                "something amorphous"
                "#+END_SRC")
      (->> (om-elem-parse-this-element)
           (om-elem-set-property :language "emacs")
           (om-elem-set-property :value "(print 'hi)")
           (om-elem-set-property :parameters '(:cache no))
           (om-elem-set-property :switches '("-n"))
           ;; TODO test preserver indent
           (om-elem-to-trimmed-string))
      => (:result "#+BEGIN_SRC emacs -n :cache no"
                  "  (print 'hi)"
                  "#+END_SRC")

      (:content "* dummy [50%]")
      (->> (om-elem-parse-this-headline)
           (om-elem--headline-get-statistics-cookie)
           (om-elem-set-property :value '(0 5))
           (om-elem-to-trimmed-string))
      => "[0/5]"

      (:content "sub_woofer")
      (->> (om-elem-parse-object-at 5)
           (om-elem-set-property :use-brackets-p t)
           (om-elem-to-trimmed-string))
      => "_{woofer}"

      (:content "super^woofer")
      (->> (om-elem-parse-object-at 7)
           (om-elem-set-property :use-brackets-p t)
           (om-elem-to-trimmed-string))
      => "^{woofer}"

      (:content "| a |")
      (->> (om-elem-parse-this-element)
           (om-elem-set-property :tblfm '("x=$2"))
           (om-elem-to-trimmed-string))
      => (:result "| a |"
                  "#+TBLFM: x=$2")

      (:content "<<found>>")
      (->> (om-elem-parse-this-object)
           (om-elem-set-property :value "lost")
           (om-elem-to-trimmed-string))
      => "<<lost>>"

      (:content "[2019-01-01 Tue]")
      (->> (om-elem-parse-this-object)
           (om-elem-set-property :year-start 2020)
           (om-elem-set-property :month-start 2)
           (om-elem-set-property :day-start 2)
           (om-elem-set-property :hour-start 12)
           (om-elem-set-property :minute-start 0)
           (om-elem-set-property :year-end 2020)
           (om-elem-set-property :month-end 2)
           (om-elem-set-property :day-end 3)
           (om-elem-set-property :hour-end 12)
           (om-elem-set-property :minute-end 0)
           (om-elem-set-property :type 'active-range)
           (om-elem-set-property :warning-type 'all)
           (om-elem-set-property :warning-unit 'day)
           (om-elem-set-property :warning-value 1)
           (om-elem-set-property :repeater-type 'cumulate)
           (om-elem-set-property :repeater-unit 'day)
           (om-elem-set-property :repeater-value 1)
           (om-elem-to-trimmed-string))
      => "<2020-02-02 Sun 12:00 +1d -1d>--<2020-02-03 Mon 12:00 +1d -1d>"

      (:content "=I am not a crook=")
      (->> (om-elem-parse-this-object)
           (om-elem-set-property :value "You totally are")
           (om-elem-to-trimmed-string))
      => "=You totally are="
      :end-hidden

      ;; TODO add post-blank

      (:content "* not valuable")
      (:comment "Throw error when setting a property that doesn't exist")
      (->> (om-elem-parse-this-headline)
           (om-elem-set-property :value "wtf")
           (om-elem-to-trimmed-string))
      !!> error)

    (defexamples-content om-elem-get-property
      nil

      (:content "#+CALL: ktulu[:cache no](x=4) :exports results")
      (->> (om-elem-parse-this-element)
           (om-elem-get-property :call))
      => "ktulu"
      (->> (om-elem-parse-this-element)
           (om-elem-get-property :inside-header))
      => '(:cache no)
      (->> (om-elem-parse-this-element)
           (om-elem-get-property :arguments))
      => '("x=4")
      (->> (om-elem-parse-this-element)
           (om-elem-get-property :end-header))
      => '(:exports results)

      :begin-hidden
      (:content "CLOCK: [2019-01-01 Tue]")
      (->> (om-elem-parse-this-element)
           (om-elem-get-property :value)
           (om-elem-to-string))
      => "[2019-01-01 Tue]"

      (:content "~learn to~")
      (->> (om-elem-parse-this-object)
           (om-elem-get-property :value))
      => "learn to"

      (:content "# not here")
      (->> (om-elem-parse-this-element)
           (om-elem-get-property :value))
      => "not here"

      (:content "#+BEGIN_COMMENT"
                "not here"
                "#+END_COMMENT")
      (->> (om-elem-parse-this-element)
           (om-elem-get-property :value))
      => "not here"

      ;; TODO add diary-sexp

      (:content ":LOGBOOK:"
                ":END:")
      (->> (om-elem-parse-this-element)
           (om-elem-get-property :drawer-name))
      => "LOGBOOK"

      (:content "#+BEGIN: blockhead :cache no"
                  "#+END:")
      (->> (om-elem-parse-this-element)
           (om-elem-get-property :block-name))
      => "blockhead"
      (->> (om-elem-parse-this-element)
           (om-elem-get-property :arguments))
      => '(:cache no)

      (:content "\\pi{}")
      (->> (om-elem-parse-this-object)
           (om-elem-get-property :name))
      => "pi"
      (->> (om-elem-parse-this-object)
           (om-elem-get-property :use-brackets-p))
      => t

      ;; TODO test preserve indentation...
      => (:content "#+BEGIN_EXAMPLE -n"
                   "example.com"
                   "#+END_EXAMPLE")
      (->> (om-elem-parse-this-element)
           (om-elem-get-property :switches))
      => '("-n")
      (->> (om-elem-parse-this-element)
           (om-elem-get-property :value))
      => "example.com"

      (:content "#+BEGIN_EXPORT domestic"
                "bullets, bombs, and bigotry"
                "#+END_EXPORT")
      (->> (om-elem-parse-this-element)
           (om-elem-get-property :type))
      ;; TODO why capitalized?
      => "DOMESTIC"
      (->> (om-elem-parse-this-element)
           (om-elem-get-property :value))
      => "bullets, bombs, and bigotry\n"

      (:content "@@back-end:value@@")
      (->> (om-elem-parse-this-object)
           (om-elem-get-property :back-end))
      => "back-end"
      (->> (om-elem-parse-this-object)
           (om-elem-get-property :value))
      => "value"

      (:content ": fixed")
      (->> (om-elem-parse-this-element)
           (om-elem-get-property :value))
      => "fixed"

      (:content "[fn:blacklabel] society")
      (->> (om-elem-parse-this-element)
           (om-elem-get-property :label))
      => "blacklabel"

      ;; TODO test footnote section
      ;; TODO the priority should be parsable after "COMMENT"
      (:content "** TODO [#A] COMMENT dummy                                   :tmsu:ARCHIVE:"
                ""
                "stuff")
      (->> (om-elem-parse-this-element)
           (om-elem-get-property :archivedp))
      => '("ARCHIVE")
      (->> (om-elem-parse-this-element)
           (om-elem-get-property :commentedp))
      => 21
      (->> (om-elem-parse-this-element)
           (om-elem-get-property :level))
      => 2
      (->> (om-elem-parse-this-element)
           (om-elem-get-property :pre-blank))
      => 1
      (->> (om-elem-parse-this-element)
           (om-elem-get-property :priority))
      => ?A
      (->> (om-elem-parse-this-element)
           (om-elem-get-property :tags))
      => '("tmsu")
      (->> (om-elem-parse-this-element)
           (om-elem-get-property :title))
      => '("dummy")
      (->> (om-elem-parse-this-element)
           (om-elem-get-property :todo-keyword))
      => "TODO"

      (:content "call_ktulu[:cache no](x=4)[:exports results]")
      (->> (om-elem-parse-this-object)
           (om-elem-get-property :call))
      => "ktulu"
      (->> (om-elem-parse-this-object)
           (om-elem-get-property :inside-header))
      =>  '(:cache no)
      (->> (om-elem-parse-this-object)
           (om-elem-get-property :arguments))
      => '("x=4")
      (->> (om-elem-parse-this-object)
           (om-elem-get-property :end-header))
      => '(:exports results)

      (:content "src_python[:cache no]{print \"yeah boi\"}")
      (->> (om-elem-parse-this-object)
           (om-elem-get-property :language))
      => "python"
      (->> (om-elem-parse-this-object)
           (om-elem-get-property :parameters))
      => '(:cache no)
      (->> (om-elem-parse-this-object)
           (om-elem-get-property :value))
      => "print \"yeah boi\""

      (:content "- [@2] [X] tmsu :: thing")
      (->> (om-elem-parse-this-item)
           (om-elem-get-property :bullet))
      => '-
      (->> (om-elem-parse-this-item)
           (om-elem-get-property :checkbox))
      => 'on
      (->> (om-elem-parse-this-item)
           (om-elem-get-property :counter))
      => 2
      (->> (om-elem-parse-this-item)
           (om-elem-get-property :tag))
      => '("tmsu")

      (:content "#+KEY: VAL")
      (->> (om-elem-parse-this-element)
           (om-elem-get-property :key))
      => "KEY"
      (->> (om-elem-parse-this-element)
           (om-elem-get-property :value))
      => "VAL"

      ;; this is stupid, who would ever do this?
      (:content "\begin{env}"
                "body"
                "\end{env}")
      (->> (om-elem-parse-this-element)
           (om-elem-get-property :value))
      => (:content "\begin{env}"
                   "body"
                   "\end{env}")

      ;; TODO this is also stupid...
      (:content "$2+2=4$")
      (->> (om-elem-parse-this-object)
           (om-elem-get-property :value))
      => "$2+2=4$"
      :end-hidden

      (:content "[[file:/dev/null]]")
      (->> (om-elem-parse-this-object)
           (om-elem-get-property :path))
      => "/dev/null"
      (->> (om-elem-parse-this-object)
           (om-elem-get-property :type))
      => "file"
      (->> (om-elem-parse-this-object)
           (om-elem-get-property :format))
      => 'bracket
      
      :begin-hidden
      (:content "{{{economics(x=4,y=2)}}}")
      (->> (om-elem-parse-this-object)
           (om-elem-get-property :key))
      => "economics"
      (->> (om-elem-parse-this-object)
           (om-elem-get-property :args))
      => '("x=4" "y=2")

      (:content "* dummy"
                ":PROPERTIES:"
                ":KEY: VAL"
                ":END:")
      ;; TODO need public function
      (->> (om-elem-parse-this-headline)
           (om-elem--headline-get-node-properties)
           (-first-item)
           (om-elem-get-property :key))
      => "KEY"
      (->> (om-elem-parse-this-headline)
           (om-elem--headline-get-node-properties)
           (-first-item)
           (om-elem-get-property :value))
      => "VAL"

      (:content "* dummy"
                "CLOSED: [2019-01-01 Tue]")
      ;; TODO need public function
      (->> (om-elem-parse-this-headline)
           (om-elem--headline-get-planning)
           (om-elem-get-property :closed)
           (om-elem-to-string))
      => "[2019-01-01 Tue]"

      (:content "#+BEGIN_special"
                "#+END_special")
      (->> (om-elem-parse-this-element)
           (om-elem-get-property :type))
      => "special"

      (:content "#+BEGIN_SRC emacs -n :cache no"
                "  (print 'hi)"
                "#+END_SRC")
      (->> (om-elem-parse-this-element)
           (om-elem-get-property :language))
      => "emacs"
      (->> (om-elem-parse-this-element)
           (om-elem-get-property :value))
      ;; TODO why indented?
      => "  (print 'hi)"
      (->> (om-elem-parse-this-element)
           (om-elem-get-property :parameters))
      => '(:cache no)
      (->> (om-elem-parse-this-element)
           (om-elem-get-property :switches))
      => '("-n")

      (:content "* dummy [50%]")
      (->> (om-elem-parse-this-headline)
           (om-elem--headline-get-statistics-cookie)
           (om-elem-get-property :value))
      => '(50)

      (:content "sub_{woofer}")
      (->> (om-elem-parse-object-at 6)
           (om-elem-get-property :use-brackets-p))
      => 6

      (:content "super_{woofer}")
      (->> (om-elem-parse-object-at 8)
           (om-elem-get-property :use-brackets-p))
      => 8

      (:content "| a |"
                "#+TBLFM: x=$2")
      (->> (om-elem-parse-this-element)
           (om-elem-get-property :tblfm))
      => '("x=$2")

      (:content "<<found>>")
      (->> (om-elem-parse-this-object)
           (om-elem-get-property :value))
      => "found"

      (:content "<2020-02-02 Sun 12:00 +1d -1d>--<2020-02-03 Mon 12:00 +1d -1d>")
      (->> (om-elem-parse-this-object)
           (om-elem-get-property :year-start))
      => 2020
      (->> (om-elem-parse-this-object)
           (om-elem-get-property :month-start))
      => 2
      (->> (om-elem-parse-this-object)
           (om-elem-get-property :day-start))
      => 2
      (->> (om-elem-parse-this-object)
           (om-elem-get-property :hour-start))
      => 12
      (->> (om-elem-parse-this-object)
           (om-elem-get-property :minute-start))
      => 0
      (->> (om-elem-parse-this-object)
           (om-elem-get-property :year-end))
      => 2020
      (->> (om-elem-parse-this-object)
           (om-elem-get-property :month-end))
      => 2
      (->> (om-elem-parse-this-object)
           (om-elem-get-property :day-end))
      => 3
      (->> (om-elem-parse-this-object)
           (om-elem-get-property :hour-end))
      => 12
      (->> (om-elem-parse-this-object)
           (om-elem-get-property :minute-end))
      => 0
      (->> (om-elem-parse-this-object)
           (om-elem-get-property :type))
      => 'active-range
      (->> (om-elem-parse-this-object)
           (om-elem-get-property :warning-type))
      => 'all
      (->> (om-elem-parse-this-object)
           (om-elem-get-property :warning-unit))
      => 'day
      (->> (om-elem-parse-this-object)
           (om-elem-get-property :warning-value))
      => 1
      (->> (om-elem-parse-this-object)
           (om-elem-get-property :repeater-type))
      => 'cumulate
      (->> (om-elem-parse-this-object)
           (om-elem-get-property :repeater-unit))
      => 'day
      (->> (om-elem-parse-this-object)
           (om-elem-get-property :repeater-value))
      => 1

      (:content "=I am not a crook=")
      (->> (om-elem-parse-this-object)
           (om-elem-get-property :value))
      => "I am not a crook"
      :end-hidden

      ;; TODO add post-blank

      (:content "* not arguable")
      (:comment "Throw error when requesting a property that doesn't exist")
      (->> (om-elem-parse-this-headline)
           (om-elem-get-property :value))
      !!> error)

    (defexamples-content om-elem-map-property
      nil

      (:content "#+CALL: ktulu()")
      (->> (om-elem-parse-this-element)
           (om-elem-map-property :call #'s-upcase)
           (om-elem-to-trimmed-string))
      => "#+CALL: KTULU()"

      :begin-hidden

      ;; TODO add clock

      (:content "~learn to~")
      (->> (om-elem-parse-this-object)
           (om-elem-map-property :value #'s-upcase)
           (om-elem-to-trimmed-string))
      => "~LEARN TO~"

      (:content "# not here")
      (->> (om-elem-parse-this-element)
           (om-elem-map-property :value #'s-upcase)
           (om-elem-to-trimmed-string))
      => "# NOT HERE"

      (:content "#+BEGIN_COMMENT"
                "not here"
                "#+END_COMMENT")
      (->> (om-elem-parse-this-element)
           (om-elem-map-property :value #'s-upcase)
           (om-elem-to-trimmed-string))
      => (:result "#+BEGIN_COMMENT"
                  "NOT HERE"
                  "#+END_COMMENT")

      ;; TODO add diary-sexp

      (:content ":LOGBOOK:"
                ":END:")
      (->> (om-elem-parse-this-element)
           (om-elem-map-property :drawer-name #'s-capitalize)
           (om-elem-to-trimmed-string))
      => (:result ":Logbook:"
                  ":END:")

      (:content "#+BEGIN: blockhead"
                "#+END:")
      (->> (om-elem-parse-this-element)
           (om-elem-map-property :block-name #'s-upcase)
           (om-elem-to-trimmed-string))
      => (:result "#+BEGIN: BLOCKHEAD"
                  "#+END:")

      :end-hidden

      ;; TODO add entity

      (:content "#+BEGIN_EXAMPLE"
                "example.com"
                "#+END_EXAMPLE")
      (->> (om-elem-parse-this-element)
           (om-elem-map-property* :value (concat "https://" it))
           (om-elem-to-trimmed-string))
      => (:result "#+BEGIN_EXAMPLE"
                  "https://example.com"
                  "#+END_EXAMPLE")

      :begin-hidden

      (:content "#+BEGIN_EXPORT domestic"
                "bullets, bombs, and bigotry"
                "#+END_EXPORT")
      (->> (om-elem-parse-this-element)
           (om-elem-map-property :type #'s-upcase)
           (om-elem-map-property :value #'s-upcase)
           (om-elem-to-trimmed-string))
      => (:result "#+BEGIN_EXPORT DOMESTIC"
                   "BULLETS, BOMBS, AND BIGOTRY"
                   "#+END_EXPORT")

      (:content "@@back-end:value@@")
      (->> (om-elem-parse-this-object)
           (om-elem-map-property :back-end #'s-upcase)
           (om-elem-map-property :value #'s-upcase)
           (om-elem-to-trimmed-string))
      => "@@BACK-END:VALUE@@"

      (:content ": fixed")
      (->> (om-elem-parse-this-element)
           (om-elem-map-property :value #'s-upcase)
           (om-elem-to-trimmed-string))
      => ": FIXED"

      (:content "[fn:blacklabel] society")
      (->> (om-elem-parse-this-element)
           (om-elem-map-property :label #'s-upcase)
           (om-elem-to-trimmed-string))
      => "[fn:BLACKLABEL] society"

      ;; TODO add example for headline

      (:content "call_ktulu()")
      (->> (om-elem-parse-this-object)
           (om-elem-map-property :call #'s-upcase)
           (om-elem-to-trimmed-string))
      => "call_KTULU()"

      ;; TODO add example for inline src block

      (:content "- tag :: thing")
      (->> (om-elem-parse-this-item)
           (om-elem-map-property :tag (lambda (it) (-map #'s-upcase it)))
           (om-elem-to-trimmed-string))
      => "- TAG :: thing"

      (:content "#+KEY: VAL")
      (->> (om-elem-parse-this-element)
           (om-elem-map-property :key (-partial #'s-prepend "OM_"))
           (om-elem-map-property :value (-partial #'s-prepend "OM_"))
           (om-elem-to-trimmed-string))
      => "#+OM_KEY: OM_VAL"

      ;; TODO add examples for latex frag/env

      ;; TODO add example for link

      (:content "{{{economics}}}")
      (->> (om-elem-parse-this-object)
           (om-elem-map-property :key #'s-upcase)
           (om-elem-to-trimmed-string))
      => "{{{ECONOMICS}}}"

      (:content "* dummy"
                ":PROPERTIES:"
                ":KEY: VAL"
                ":END:")
      ;; TODO need public function
      (->> (om-elem-parse-this-headline)
           (om-elem--headline-get-node-properties)
           (-first-item)
           (om-elem-map-property :key (-partial #'s-prepend "OM_"))
           (om-elem-map-property :value (-partial #'s-prepend "OM_"))
           (om-elem-to-trimmed-string))
      => ":OM_KEY:   OM_VAL"

      ;; TODO add example for planning

      (:content "#+BEGIN_special"
                "#+END_special")
      (->> (om-elem-parse-this-element)
           (om-elem-map-property :type #'s-upcase)
           (om-elem-to-trimmed-string))
      => (:result "#+BEGIN_SPECIAL"
                  "#+END_SPECIAL")

      ;; TODO add example for src block

      ;; TODO add example for statistics cookie

      (:content "<<found>>")
      (->> (om-elem-parse-this-object)
           (om-elem-map-property :value #'s-upcase)
           (om-elem-to-trimmed-string))
      => "<<FOUND>>"

      (:content "=I am not a crook=")
      (->> (om-elem-parse-this-object)
           (om-elem-map-property :value #'s-upcase)
           (om-elem-to-trimmed-string))
      => "=I AM NOT A CROOK="
      :end-hidden

      (:content "~code~")
      (:comment "Throw error if property doesn't exist")
      (->> (om-elem-parse-this-object)
           (om-elem-map-property :title #'s-upcase)
           (om-elem-to-trimmed-string))
      !!> error
      (:comment "Throw error if function doesn't return proper type")
      (->> (om-elem-parse-this-object)
           (om-elem-map-property* :value (if it 1 0))
           (om-elem-to-trimmed-string))
      !!> error)

    (defexamples-content om-elem-toggle-property
      nil

      (:content "\\pi")
      (->> (om-elem-parse-this-object)
           (om-elem-toggle-property :use-brackets-p)
           (om-elem-to-trimmed-string))
      => "\\pi{}"

      ;; TODO test src/example block preserve indent
      
      (:content "* headline")
      (->> (om-elem-parse-this-headline)
           (om-elem-toggle-property :archivedp)
           (om-elem-to-trimmed-string))
      => "* headline                                                          :ARCHIVE:"
      (->> (om-elem-parse-this-headline)
           (om-elem-toggle-property :commentedp)
           (om-elem-to-trimmed-string))
      => "* COMMENT headline"
      (->> (om-elem-parse-this-headline)
           (om-elem-toggle-property :footnote-section-p)
           (om-elem-to-trimmed-string))
      => "* Footnotes"

      :begin-hidden

      (:content "sub_woofer")
      (->> (om-elem-parse-object-at 5)
           (om-elem-toggle-property :use-brackets-p)
           (om-elem-to-trimmed-string))
      => "_{woofer}"

      (:content "super^woofer")
      (->> (om-elem-parse-object-at 7)
           (om-elem-toggle-property :use-brackets-p)
           (om-elem-to-trimmed-string))
      => "^{woofer}"

      :end-hidden

      (:content "- [ ] nope")
      (:comment "Throw an error when trying to toggle a non-boolean property")
      (->> (om-elem-parse-this-item)
           (om-elem-toggle-property :checkbox)
           (om-elem-to-trimmed-string))
      !!> error)

    (defexamples-content om-elem-shift-property
      ;; TODO need to ensure that the min/max priorities are always the same
      nil

      (:content "* no priorities")
      (:comment "Do nothing if there is nothing to shift.")
      (->> (om-elem-parse-this-headline)
           (om-elem-shift-property :priority 1)
           (om-elem-to-trimmed-string))
      => "* no priorities"

      (:content "* [#A] priorities")
      (->> (om-elem-parse-this-headline)
           (om-elem-shift-property :priority -1)
           (om-elem-to-trimmed-string))
      => "* [#B] priorities"
      (->> (om-elem-parse-this-headline)
           (om-elem-shift-property :priority -2)
           (om-elem-to-trimmed-string))
      => "* [#C] priorities"
      (:comment "Wrap priority around when crossing the min or max")
      (->> (om-elem-parse-this-headline)
           (om-elem-shift-property :priority 1)
           (om-elem-to-trimmed-string))
      => "* [#C] priorities"

      (:content "* TODO or not todo")
      (:comment "Throw error when shifting an unshiftable property")
      (->> (om-elem-parse-this-headline)
           (om-elem-shift-property :todo-keyword 1)
           (om-elem-to-string))
      !!> error

      :begin-hidden

      (:content "*bold*")
      (->> (om-elem-parse-this-object)
           (om-elem-shift-property :post-blank 1)
           (om-elem-to-string))
      => "*bold* "
      (->> (om-elem-parse-this-object)
           (om-elem-shift-property :post-blank -1)
           (om-elem-to-string))
      => "*bold*"

      (:content "1. thing")
      (->> (om-elem-parse-this-item)
           (om-elem-shift-property :counter 1)
           (om-elem-to-trimmed-string))
      => "1. thing"

      (:content "1. [@1] thing")
      (->> (om-elem-parse-this-item)
           (om-elem-shift-property :counter 1)
           (om-elem-to-trimmed-string))
      => "1. [@2] thing"
      (->> (om-elem-parse-this-item)
           (om-elem-shift-property :counter -1)
           (om-elem-to-trimmed-string))
      => "1. [@1] thing"

      (:content "* noob level")
      (->> (om-elem-parse-this-headline)
           (om-elem-shift-property :level 1)
           (om-elem-to-trimmed-string))
      => "** noob level"

      (:comment "Do nothing when final value is less than one.")
      (->> (om-elem-parse-this-headline)
           (om-elem-shift-property :level -1)
           (om-elem-to-trimmed-string))
      => "* noob level"

      (:content "* headline"
                "stuff")
      (->> (om-elem-parse-this-headline)
           (om-elem-shift-property :pre-blank 1)
           (om-elem-to-trimmed-string))
      => (:result "* headline"
                  ""
                  "stuff")
      (->> (om-elem-parse-this-headline)
           (om-elem-shift-property :pre-blank -1)
           (om-elem-to-trimmed-string))
      => (:result "* headline"
                  "stuff")
      :end-hidden)

    (defexamples-content om-elem-insert-into-property
      nil

      (:content "#+CALL: ktulu(y=1)")
      (->> (om-elem-parse-this-element)
           (om-elem-insert-into-property :arguments 0 "x=4")
           (om-elem-to-trimmed-string))
      => "#+CALL: ktulu(x=4,y=1)"

      (:comment "Do nothing if the string is already in the list")
      (->> (om-elem-parse-this-element)
           (om-elem-insert-into-property :arguments 0 "y=1")
           (om-elem-to-trimmed-string))
      => "#+CALL: ktulu(y=1)"

      (:comment "Throw error when inserting into a property that is not a list of strings")
      (->> (om-elem-parse-this-element)
           (om-elem-insert-into-property :end-header 0 "html")
           (om-elem-to-trimmed-string))
      !!> error

      (:content "* headline       :tag1:")
      (->> (om-elem-parse-this-headline)
           (om-elem-insert-into-property :tags 0 "tag0")
           (om-elem-to-trimmed-string))
      => "* headline                                                        :tag0:tag1:"

      :begin-hidden

      (:content "#+BEGIN_EXAMPLE -n"
                "#+END_EXAMPLE")
      (->> (om-elem-parse-this-element)
           (om-elem-insert-into-property :switches -1 "-r")
           (om-elem-to-trimmed-string))
      => (:result "#+BEGIN_EXAMPLE -n -r"
                  "#+END_EXAMPLE")


      (:content "call_ktulu(y=1)")
      (->> (om-elem-parse-this-object)
           (om-elem-insert-into-property :arguments 0 "x=4")
           (om-elem-to-trimmed-string))
      => "call_ktulu(x=4,y=1)"

      (:content "{{{economics(x=4)}}}")
      (->> (om-elem-parse-this-object)
           (om-elem-insert-into-property :args 0 "z=2")
           (om-elem-to-trimmed-string))
      => "{{{economics(z=2,x=4)}}}"
      
      (:content "#+BEGIN_SRC emacs-lisp -n"
                "#+END_SRC")
      (->> (om-elem-parse-this-element)
           (om-elem-insert-into-property :switches -1 "-r")
           (om-elem-to-trimmed-string))
      => (:result "#+BEGIN_SRC emacs-lisp -n -r"
                  "#+END_SRC")

      (:content "| a |"
                "#+TBLFM: x=$2")
      (->> (om-elem-parse-this-element)
           (om-elem-insert-into-property :tblfm -1 "y=$3")
           (om-elem-to-trimmed-string))
      => (:result "| a |"
                  "#+TBLFM: y=$3"
                  "#+TBLFM: x=$2")
      :end-hidden)

    (defexamples-content om-elem-remove-from-property
      nil

      (:content "#+CALL: ktulu(y=1)")
      (->> (om-elem-parse-this-element)
           (om-elem-remove-from-property :arguments "y=1")
           (om-elem-to-trimmed-string))
      => "#+CALL: ktulu()"

      (:comment "Do nothing if the string does not exist")
      (->> (om-elem-parse-this-element)
           (om-elem-remove-from-property :arguments "d=666")
           (om-elem-to-trimmed-string))
      => "#+CALL: ktulu(y=1)"

      (:comment "Throw error when removing from property that is not a string list")
      (->> (om-elem-parse-this-element)
           (om-elem-remove-from-property :end-header ":results")
           (om-elem-to-trimmed-string))
      !!> error

      (:content "* headline       :tag1:")
      (->> (om-elem-parse-this-headline)
           (om-elem-remove-from-property :tags "tag1")
           (om-elem-to-trimmed-string))
      => "* headline"

      :begin-hidden

      (:content "#+BEGIN_EXAMPLE -n"
                "#+END_EXAMPLE")
      (->> (om-elem-parse-this-element)
           (om-elem-remove-from-property :switches "-n")
           (om-elem-to-trimmed-string))
      => (:result "#+BEGIN_EXAMPLE"
                  "#+END_EXAMPLE")

      (:content "call_ktulu(y=1)")
      (->> (om-elem-parse-this-object)
           (om-elem-remove-from-property :arguments "y=1")
           (om-elem-to-trimmed-string))
      => "call_ktulu()"

      (:content "{{{economics(x=4)}}}")
      (->> (om-elem-parse-this-object)
           (om-elem-remove-from-property :args "x=4")
           (om-elem-to-trimmed-string))
      => "{{{economics}}}"
      
      (:content "#+BEGIN_SRC emacs-lisp -n"
                "#+END_SRC")
      (->> (om-elem-parse-this-element)
           (om-elem-remove-from-property :switches "-n")
           (om-elem-to-trimmed-string))
      => (:result "#+BEGIN_SRC emacs-lisp"
                  "#+END_SRC")

      (:content "| a |"
                "#+TBLFM: x=$2")
      (->> (om-elem-parse-this-element)
           (om-elem-remove-from-property :tblfm "x=$2")
           (om-elem-to-trimmed-string))
      => "| a |"
      :end-header)

    (defexamples-content om-elem-plist-put-property
      nil

      (:content "#+CALL: ktulu[:cache no]()")
      (->> (om-elem-parse-this-element)
           (om-elem-plist-put-property :end-header :results 'html)
           (om-elem-to-trimmed-string))
      => "#+CALL: ktulu[:cache no]() :results html"
      (:comment "Change the value of key if it already is present")
      (->> (om-elem-parse-this-element)
           (om-elem-plist-put-property :inside-header :cache 'yes)
           (om-elem-to-trimmed-string))
      => "#+CALL: ktulu[:cache yes]()"
      (:comment "Do nothing if the key and value already exist")
      (->> (om-elem-parse-this-element)
           (om-elem-plist-put-property :inside-header :cache 'no)
           (om-elem-to-trimmed-string))
      => "#+CALL: ktulu[:cache no]()"
      (:comment "Throw error if setting property that isn't a plist")
      (->> (om-elem-parse-this-element)
           (om-elem-plist-put-property :arguments :cache 'no)
           (om-elem-to-trimmed-string))
      !!> error

      :begin-hidden

      (:content "#+BEGIN: blockhead :format \"[%s]\""
                "#+END:")
      (->> (om-elem-parse-this-element)
           (om-elem-plist-put-property :arguments :format "<%s>")
           (om-elem-to-trimmed-string))
      => (:result "#+BEGIN: blockhead :format \"<%s>\""
                  "#+END:")

      (:content "call_ktulu[:cache no]()")
      (->> (om-elem-parse-this-object)
           (om-elem-plist-put-property :inside-header :cache 'yes)
           (om-elem-plist-put-property :end-header :results 'html)
           (om-elem-to-trimmed-string))
      => "call_ktulu[:cache yes]()[:results html]"

      (:content "src_emacs-lisp[:exports results]{}")
      (->> (om-elem-parse-this-object)
           (om-elem-plist-put-property :parameters :exports 'both)
           (om-elem-to-trimmed-string))
      => "src_emacs-lisp[:exports both]{}"

      (:content "#+BEGIN_SRC emacs-lisp -n :exports results"
                "#+END_SRC")
      (->> (om-elem-parse-this-element)
           (om-elem-plist-put-property :parameters :exports 'both)
           (om-elem-to-trimmed-string))
      => (:result "#+BEGIN_SRC emacs-lisp -n :exports both"
                  "#+END_SRC")
      :end-hidden)

    (defexamples-content om-elem-plist-remove-property
      nil

      (:content "#+CALL: ktulu() :results html")
      (->> (om-elem-parse-this-element)
           (om-elem-plist-remove-property :end-header :results)
           (om-elem-to-trimmed-string))
      => "#+CALL: ktulu()"
      (:comment "Do nothing if the key is not present")
      (->> (om-elem-parse-this-element)
           (om-elem-plist-remove-property :inside-header :cache)
           (om-elem-to-trimmed-string))
      => "#+CALL: ktulu() :results html"
      (:comment "Throw error if trying to remove key from non-plist property")
      (->> (om-elem-parse-this-element)
           (om-elem-plist-remove-property :arguments :cache)
           (om-elem-to-trimmed-string))
      !!> error

      :begin-hidden

      (:content "#+BEGIN: blockhead :format \"[%s]\""
                "#+END:")
      (->> (om-elem-parse-this-element)
           (om-elem-plist-remove-property :arguments :format)
           (om-elem-to-trimmed-string))
      => (:result "#+BEGIN: blockhead"
                  "#+END:")

      (:content "call_ktulu[:cache no]()[:results html]")
      (->> (om-elem-parse-this-object)
           (om-elem-plist-remove-property :inside-header :cache)
           (om-elem-plist-remove-property :end-header :results)
           (om-elem-to-trimmed-string))
      => "call_ktulu()"

      (:content "src_emacs-lisp[:exports results]{}")
      (->> (om-elem-parse-this-object)
           (om-elem-plist-remove-property :parameters :exports)
           (om-elem-to-trimmed-string))
      => "src_emacs-lisp{}"

      (:content "#+BEGIN_SRC emacs-lisp -n :exports results"
                "#+END_SRC")
      (->> (om-elem-parse-this-element)
           (om-elem-plist-remove-property :parameters :exports)
           (om-elem-to-trimmed-string))
      => (:result "#+BEGIN_SRC emacs-lisp -n"
                  "#+END_SRC")
      :end-hidden)

    ;; (defexamples-content om-elem-property-is-nil-p
    ;;   nil
    ;;   (:content "* TODO dummy")
    ;;   (->> (om-elem-parse-this-headline)
    ;;        (om-elem-property-is-nil-p :todo-keyword))
    ;;   => nil
    ;;   (->> (om-elem-parse-this-headline)
    ;;        (om-elem-property-is-nil-p :commentedp))
    ;;   => t)

    ;; (defexamples-content om-elem-property-is-non-nil-p
    ;;   nil
    ;;   (:content "* TODO dummy")
    ;;   (->> (om-elem-parse-this-headline)
    ;;        (om-elem-property-is-non-nil-p :todo-keyword))
    ;;   => t
    ;;   (->> (om-elem-parse-this-headline)
    ;;        (om-elem-property-is-non-nil-p :commentedp))
    ;;   => nil)

    ;; (defexamples-content om-elem-property-is-eq-p
    ;;   nil
    ;;   (:content "* [#A] dummy")
    ;;   (->> (om-elem-parse-this-headline)
    ;;        (om-elem-property-is-eq-p :priority ?A))
    ;;   => t
    ;;   (->> (om-elem-parse-this-headline)
    ;;        (om-elem-property-is-eq-p :priority ?B))
    ;;   => nil)

    ;; (defexamples-content om-elem-property-is-equal-p
    ;;   nil
    ;;   (:content "* TODO dummy")
    ;;   (->> (om-elem-parse-this-headline)
    ;;        (om-elem-property-is-equal-p :todo-keyword "TODO"))
    ;;   => t
    ;;   (->> (om-elem-parse-this-headline)
    ;;        (om-elem-property-is-equal-p :todo-keyword "DONE"))
    ;;   => nil)

    ;; (defexamples-content om-elem-property-is-predicate-p
    ;;   nil
    ;;   (:content "* this is a dummy")
    ;;   (->> (om-elem-parse-this-headline)
    ;;        (om-elem-property-is-predicate-p*
    ;;         :title (s-contains? "dummy" (car it))))
    ;;   => t)


  (def-example-subgroup "Clock"
    nil

    ;; TODO add get/set/shift duration
    ;; TODO add get/set/shift start/end/single/double time

    (defexamples-content om-elem-clock-is-running-p
      nil
      (:content "CLOCK: [2019-01-01 Tue 00:00]")
      (->> (om-elem-parse-this-element)
           (om-elem-clock-is-running-p))
      => t
      (:content "CLOCK: [2019-01-01 Tue 00:00]--[2019-01-02 Wed 00:00] => 24:00")
      (->> (om-elem-parse-this-element)
           (om-elem-clock-is-running-p))
      => nil))

  (def-example-subgroup "Headline"
    nil

    (defexamples-content om-elem-headline-is-done-p
      nil
      (:content "* TODO darn")
      (->> (om-elem-parse-this-headline)
           (om-elem-headline-is-done-p))
      => nil
      (:content "* DONE yay")
      (->> (om-elem-parse-this-headline)
           (om-elem-headline-is-done-p))
      => t)

    (defexamples-content om-elem-headline-is-archived-p
      nil
      (:content "* dummy")
      (->> (om-elem-parse-this-headline)
           (om-elem-headline-is-archived-p))
      => nil
      (:content "* dummy                                                             :ARCHIVE:")
      (->> (om-elem-parse-this-headline)
           (om-elem-headline-is-archived-p))
      => t)

    (defexamples-content om-elem-headline-is-commented-p
      nil
      (:content "* dummy")
      (->> (om-elem-parse-this-headline)
           (om-elem-headline-is-commented-p))
      => nil
      (:content "* COMMENT dummy")
      (->> (om-elem-parse-this-headline)
           (om-elem-headline-is-commented-p))
      => t)

    (defexamples-content om-elem-headline-has-tag-p
      nil
      (:content "* dummy")
      (->> (om-elem-parse-this-headline)
           (om-elem-headline-has-tag-p "tmsu"))
      => nil
      (:content "* dummy                                                             :tmsu:")
      (->> (om-elem-parse-this-headline)
           (om-elem-headline-has-tag-p "tmsu"))
      => t)

    ;; TODO add the shortcut version title setter

    )


  ;; TODO add inlinetask

  (def-example-subgroup "Item"
    nil

    ;; TODO add shortcut tag setter

    (defexamples-content om-elem-item-is-unchecked-p
      nil
      (:content "- one"
                "- [ ] two"
                "- [X] three"
                "- [-] four")
      (->> (om-elem-parse-this-element)
           (om-elem--get-contents)
           (-map #'om-elem-item-is-unchecked-p))
      => '(nil t nil nil))

    (defexamples-content om-elem-item-is-checked-p
      nil
      (:content "- one"
                "- [ ] two"
                "- [X] three"
                "- [-] four")
      (->> (om-elem-parse-this-element)
           (om-elem--get-contents)
           (-map #'om-elem-item-is-checked-p))
      => '(nil nil t nil))

    (defexamples-content om-elem-item-is-trans-p
      nil
      (:content "- one"
                "- [ ] two"
                "- [X] three"
                "- [-] four")
      (->> (om-elem-parse-this-element)
           (om-elem--get-contents)
           (-map #'om-elem-item-is-trans-p))
      => '(nil nil nil t))
    
    (defexamples-content om-elem-item-toggle-checkbox
      nil
      (:content "- [ ] one")
      (->> (om-elem-parse-this-item)
           (om-elem-item-toggle-checkbox)
           (om-elem-to-trimmed-string))
      => "- [X] one"
      (->> (om-elem-parse-this-item)
           (om-elem-item-toggle-checkbox)
           (om-elem-item-toggle-checkbox)
           (om-elem-to-trimmed-string))
      => "- [ ] one"
      (:content "- [-] one")
      (->> (om-elem-parse-this-item)
           (om-elem-item-toggle-checkbox)
           (om-elem-to-trimmed-string))
      => "- [-] one"
      (:content "- one")
      (->> (om-elem-parse-this-item)
           (om-elem-item-toggle-checkbox)
           (om-elem-to-trimmed-string))
      => "- one")
    )

  (def-example-subgroup "Statistics Cookie"
    nil
    ;; TODO add predicate for complete/empty
    )

  (def-example-subgroup "Timestamp"
    nil

    ;; TODO add get start/end for timestamp and unixtime
    (defexamples-content om-elem-timestamp-get-start-timestamp
      nil
      (:content "[2019-01-01 Tue]")
      (->> (om-elem-parse-this-object)
           (om-elem-timestamp-get-start-timestamp)
           (om-elem-to-trimmed-string))
      => "[2019-01-01 Tue]"
      (:content "[2019-01-01 Tue]--[2019-01-02 Wed]")
      (->> (om-elem-parse-this-object)
           (om-elem-timestamp-get-start-timestamp)
           (om-elem-to-trimmed-string))
      => "[2019-01-01 Tue]"
      (:content "[2019-01-01 Tue 00:00-12:00]")
      (->> (om-elem-parse-this-object)
           (om-elem-timestamp-get-start-timestamp)
           (om-elem-to-trimmed-string))
      => "[2019-01-01 Tue 00:00]")

    (defexamples-content om-elem-timestamp-get-end-timestamp
      nil
      (:content "[2019-01-01 Tue]")
      (->> (om-elem-parse-this-object)
           (om-elem-timestamp-get-end-timestamp)
           (om-elem-to-trimmed-string))
      => ""
      (:content "[2019-01-01 Tue]--[2019-01-02 Wed]")
      (->> (om-elem-parse-this-object)
           (om-elem-timestamp-get-end-timestamp)
           (om-elem-to-trimmed-string))
      => "[2019-01-02 Wed]"
      (:content "[2019-01-01 Tue 00:00-12:00]")
      (->> (om-elem-parse-this-object)
           (om-elem-timestamp-get-end-timestamp)
           (om-elem-to-trimmed-string))
      => "[2019-01-01 Tue 12:00]")

    (defexamples-content om-elem-timestamp-is-active-p
      nil
      (:content "<2019-01-01 Tue>")
      (->> (om-elem-parse-this-object)
           (om-elem-timestamp-is-active-p))
      => t
      (:content "[2019-01-01 Tue]")
      (->> (om-elem-parse-this-object)
           (om-elem-timestamp-is-active-p))
      => nil)

    (defexamples-content om-elem-timestamp-is-inactive-p
      nil
      (:content "[2019-01-01 Tue]")
      (->> (om-elem-parse-this-object)
           (om-elem-timestamp-is-inactive-p))
      => t
      (:content "<2019-01-01 Tue>")
      (->> (om-elem-parse-this-object)
           (om-elem-timestamp-is-inactive-p))
      => nil)

    (defexamples-content om-elem-timestamp-is-ranged-p
      nil
      (:content "[2019-01-01 Tue]--[2019-01-02 Wed]")
      (->> (om-elem-parse-this-object)
           (om-elem-timestamp-is-ranged-p))
      => t
      (:content "[2019-01-01 Tue 00:00-12:00]")
      (->> (om-elem-parse-this-object)
           (om-elem-timestamp-is-ranged-p))
      => t
      (:content "[2019-01-01 Tue]")
      (->> (om-elem-parse-this-object)
           (om-elem-timestamp-is-ranged-p))
      => nil)

    ;; TODO add set unixtime and range
    (defexamples-content om-elem-timestamp-set-start-time
      nil
      (:content "[2019-01-02 Wed]")
      (:comment "If not a range this will turn into a range by moving only the start time.")
      (->> (om-elem-parse-this-object)
           (om-elem-timestamp-set-start-time '(2019 1 1))
           (om-elem-to-trimmed-string))
      => "[2019-01-01 Tue]--[2019-01-02 Wed]"
      (:comment "Set a different time with different precision.")
      (->> (om-elem-parse-this-object)
           (om-elem-timestamp-set-start-time '(2019 1 1 10 0))
           (om-elem-to-trimmed-string))
      => "[2019-01-01 Tue 10:00]--[2019-01-02 Wed]")

    (defexamples-content om-elem-timestamp-set-end-time
      nil
      (:content "[2019-01-01 Tue]")
      (:comment "Add the end time")
      (->> (om-elem-parse-this-object)
           (om-elem-timestamp-set-end-time '(2019 1 2))
           (om-elem-to-trimmed-string))
      => "[2019-01-01 Tue]--[2019-01-02 Wed]"
      (:content "[2019-01-01 Tue]--[2019-01-02 Wed]")
      (:comment "Remove the end time")
      (->> (om-elem-parse-this-object)
           (om-elem-timestamp-set-end-time nil)
           (om-elem-to-trimmed-string))
      => "[2019-01-01 Tue]")

    (defexamples-content om-elem-timestamp-set-type
      nil
      (:content "[2019-01-01 Tue]")
      (->> (om-elem-parse-this-object)
           (om-elem-timestamp-set-type 'active)
           (om-elem-to-trimmed-string))
      => "<2019-01-01 Tue>")

    (defexamples-content om-elem-timestamp-shift
      nil
      (:content "[2019-01-01 Tue 12:00]")
      (:comment "Change each unit, and wrap around to the next unit as needed.")
      (->> (om-elem-parse-this-object)
           (om-elem-timestamp-shift 30 'minute)
           (om-elem-to-trimmed-string))
      => "[2019-01-01 Tue 12:30]"
      (->> (om-elem-parse-this-object)
           (om-elem-timestamp-shift 60 'minute)
           (om-elem-to-trimmed-string))
      => "[2019-01-01 Tue 13:00]"
      (->> (om-elem-parse-this-object)
           (om-elem-timestamp-shift 1 'hour)
           (om-elem-to-trimmed-string))
      => "[2019-01-01 Tue 13:00]"
      (->> (om-elem-parse-this-object)
           (om-elem-timestamp-shift 1 'day)
           (om-elem-to-trimmed-string))
      => "[2019-01-02 Wed 12:00]"
      (->> (om-elem-parse-this-object)
           (om-elem-timestamp-shift 31 'day)
           (om-elem-to-trimmed-string))
      => "[2019-02-01 Fri 12:00]"
      (->> (om-elem-parse-this-object)
           (om-elem-timestamp-shift 1 'month)
           (om-elem-to-trimmed-string))
      => "[2019-02-01 Fri 12:00]"
      (->> (om-elem-parse-this-object)
           (om-elem-timestamp-shift 13 'month)
           (om-elem-to-trimmed-string))
      => "[2020-02-01 Sat 12:00]"
      (->> (om-elem-parse-this-object)
           (om-elem-timestamp-shift 1 'year)
           (om-elem-to-trimmed-string))
      => "[2020-01-01 Wed 12:00]"
      (->> (om-elem-parse-this-object)
           (om-elem-timestamp-shift 0 'year)
           (om-elem-to-trimmed-string))
      => "[2019-01-01 Tue 12:00]"
      (:content "[2019-01-01 Tue]")
      (:comment "Error when shifting hour/minute in short format")
      (->> (om-elem-parse-this-object)
           (om-elem-timestamp-shift 30 'minute)
           (om-elem-to-trimmed-string))
      !!> error
      (->> (om-elem-parse-this-object)
           (om-elem-timestamp-shift 30 'hour)
           (om-elem-to-trimmed-string))
      !!> error)

    (defexamples-content om-elem-timestamp-shift-start
      nil
      (:content "[2019-01-01 Tue 12:00]")
      (:comment "If not a range, change start time and leave implicit end time.")
      (->> (om-elem-parse-this-object)
           (om-elem-timestamp-shift-start -1 'year)
           (om-elem-to-trimmed-string))
      => "[2018-01-01 Mon 12:00]--[2019-01-01 Tue 12:00]"
      (:content "[2019-01-01 Tue]--[2019-01-03 Thu]")
      (:comment "Change only start time if a range")
      (->> (om-elem-parse-this-object)
           (om-elem-timestamp-shift-start 1 'day)
           (om-elem-to-trimmed-string))
      => "[2019-01-02 Wed]--[2019-01-03 Thu]")

    (defexamples-content om-elem-timestamp-shift-end
      nil
      (:content "[2019-01-01 Tue]")
      (:comment "Shift implicit end time if not a range.")
      (->> (om-elem-parse-this-object)
           (om-elem-timestamp-shift-end 1 'day)
           (om-elem-to-trimmed-string))
      => "[2019-01-01 Tue]--[2019-01-02 Wed]"
      (:content "[2019-01-01 Tue]--[2019-01-02 Wed]")
      (:comment "Move only the second time if a range.")
      (->> (om-elem-parse-this-object)
           (om-elem-timestamp-shift-end 1 'day)
           (om-elem-to-trimmed-string))
      => "[2019-01-01 Tue]--[2019-01-03 Thu]")

    (defexamples-content om-elem-timestamp-toggle-active
      nil
      (:content "[2019-01-01 Tue]")
      (->> (om-elem-parse-this-object)
           (om-elem-timestamp-toggle-active)
           (om-elem-to-trimmed-string))
      => "<2019-01-01 Tue>"
      (->> (om-elem-parse-this-object)
           (om-elem-timestamp-toggle-active)
           (om-elem-timestamp-toggle-active)
           (om-elem-to-trimmed-string))
      => "[2019-01-01 Tue]"
      (:content "[2019-01-01 Tue]--[2019-01-02 Wed]")
      (->> (om-elem-parse-this-object)
           (om-elem-timestamp-toggle-active)
           (om-elem-to-trimmed-string))
      => "<2019-01-01 Tue>--<2019-01-02 Wed>"
      (->> (om-elem-parse-this-object)
           (om-elem-timestamp-toggle-active)
           (om-elem-timestamp-toggle-active)
           (om-elem-to-trimmed-string))
      => "[2019-01-01 Tue]--[2019-01-02 Wed]"))
  ))

(def-example-group "Content Modification Functions"
  "Manipulate the contents of containers"

  (def-example-subgroup "Generic"
    nil

    ;; (defexamples-content om-elem-parent-get-headline
    ;;   nil
    ;;   (:content "* headline 1"
    ;;             "section stuff")
    ;;   (:comment "Find the headline from the section element")
    ;;   (--> (om-elem-parse-this-subtree)
    ;;        (om-elem-find-first it 'section)
    ;;        (om-elem-parent-get-headline it)
    ;;        (om-elem-to-trimmed-string it))
    ;;   => "* headline 1\nsection stuff"
    ;;   (:comment "The toplevel headline has no headline parent, so return nil")
    ;;   (-> (om-elem-parse-this-subtree)
    ;;       (om-elem-parent-get-headline))
    ;;   => "")
    
    ;; (defexamples-content om-elem-is-empty-p
    ;;   nil
    ;;   (:content "* dummy\nfilled with useless knowledge")
    ;;   (->> (om-elem-parse-this-headline)
    ;;        (om-elem-is-empty-p))
    ;;   => nil
    ;;   (:content "* dummy")
    ;;   (->> (om-elem-parse-this-headline)
    ;;        (om-elem-is-empty-p))
    ;;   => t)
    )

  (def-example-subgroup "Headline"
    nil

    (defexamples-content om-elem-headline-is-scheduled-p
      nil
      (:content "* lazy")
      (->> (om-elem-parse-this-headline)
           (om-elem-headline-is-scheduled-p))
      => nil
      (:content "* proactive"
                "SCHEDULED: [2019-01-01 Tue]")
      (->> (om-elem-parse-this-headline)
           (om-elem-headline-is-scheduled-p))
      => t)

    (defexamples-content om-elem-headline-is-deadlined-p
      nil
      (:content "* lazy")
      (->> (om-elem-parse-this-headline)
           (om-elem-headline-is-deadlined-p))
      => nil
      (:content "* proactive"
                "DEADLINE: [2019-01-01 Tue]")
      (->> (om-elem-parse-this-headline)
           (om-elem-headline-is-deadlined-p))
      => t)
    
    (defexamples-content om-elem-headline-is-closed-p
      nil
      (:content "* lazy")
      (->> (om-elem-parse-this-headline)
           (om-elem-headline-is-closed-p))
      => nil
      (:content "* proactive"
                "CLOSED: [2019-01-01 Tue]")
      (->> (om-elem-parse-this-headline)
           (om-elem-headline-is-closed-p))
      => t)

    (defexamples-content om-elem-headline-get-subheadlines
      nil
      (:content "* headline 1"
                "sectional stuff"
                "** headline 2"
                "** headline 3")
      (->> (om-elem-parse-this-subtree)
           (om-elem-headline-get-subheadlines)
           (-map #'om-elem-to-trimmed-string))
      => '("** headline 2" "** headline 3")
      (:content "* headline 1"
                "sectional stuff")
      (->> (om-elem-parse-this-subtree)
           (om-elem-headline-get-subheadlines)
           (-map #'om-elem-to-trimmed-string))
      => nil)

    (defexamples-content om-elem-headline-get-section
      nil
      (:content "* headline 1"
                "sectional stuff"
                "** headline 2"
                "** headline 3")
      (->> (om-elem-parse-this-subtree)
           (om-elem-headline-get-section)
           (om-elem-to-trimmed-string))
      => "sectional stuff"
      (:content "* headline 1"
                "** headline 2"
                "** headline 3")
      (->> (om-elem-parse-this-subtree)
           (om-elem-headline-get-section)
           (om-elem-to-trimmed-string))
      => "")

    (defexamples-content om-elem-headline-get-drawer
      nil
      (:content "* headline 1"
                ":LOGBOOK:"
                "- random note"
                ":END:"
                "rest of the section"
                "** headline 2")
      (->> (om-elem-parse-this-subtree)
           (om-elem-headline-get-drawer "LOGBOOK")
           (om-elem-to-trimmed-string))
      => (:result ":LOGBOOK:"
                  "- random note"
                  ":END:")
      (->> (om-elem-parse-this-subtree)
           (om-elem-headline-get-drawer "OTHER")
           (om-elem-to-trimmed-string))
      => "")

    ;; (defexamples-content om-elem-headline-get-path
    ;;   nil
    ;;   (:content "* one"
    ;;             "** two"
    ;;             "*** three")
    ;;   (--> (om-elem-parse-this-subtree)
    ;;        (om-elem-find-first it 'headline)
    ;;        (om-elem-headline-get-path it)
    ;;        (om-elem-to-trimmed-string it))
    ;;   => '("one"))

    (defexamples-content om-elem-headline-indent-subheadline
      nil
      (:content "* one"
                "** two"
                "** three"
                "*** four")
      (->> (om-elem-parse-element-at 1)
           (om-elem-headline-indent-subheadline 0)
           (om-elem-to-trimmed-string))
      !!> error
      (->> (om-elem-parse-element-at 1)
           (om-elem-headline-indent-subheadline 1)
           (om-elem-to-trimmed-string))
      => (:result "* one"
                  "** two"
                  "*** three"
                  "*** four"))

    (defexamples-content om-elem-headline-indent-subtree
      nil
      (:content "* one"
                "** two"
                "** three"
                "*** four")
      (->> (om-elem-parse-element-at 1)
           (om-elem-headline-indent-subtree 1)
           (om-elem-to-trimmed-string))
      => (:result "* one"
                  "** two"
                  "*** three"
                  "**** four"))

    (defexamples-content om-elem-headline-unindent-subheadline
      nil
      (:content "* one"
                "** two"
                "** three"
                "*** four"
                "*** four"
                "*** four")
      (->> (om-elem-parse-element-at 1)
           (om-elem-headline-unindent-subheadline 1 1)
           (om-elem-to-trimmed-string))
      => (:result "* one"
                  "** two"
                  "** three"
                  "*** four"
                  "** four"
                  "*** four"))

    (defexamples-content om-elem-headline-unindent-subtree
      nil
      (:content "* one"
                "** two"
                "** three"
                "*** four"
                "*** four"
                "*** four")
      (->> (om-elem-parse-element-at 1)
           (om-elem-headline-unindent-subtree 1)
           (om-elem-to-trimmed-string))
      => (:result "* one"
                  "** two"
                  "** three"
                  "** four"
                  "** four"
                  "** four")))

  (def-example-subgroup "Item"
    nil

    ;; (defexamples-content om-elem-item-get-level
    ;;   nil
    ;;   (:content "- one"
    ;;             "  - two"
    ;;             "    - three")
    ;;   (->> (om-elem-parse-this-item)
    ;;        (om-elem-)))

    (defexamples-content om-elem-item-get-sublist
      nil
      (:content "- one"
                "  - two"
                "  - three"
                "- four")
      (->> (om-elem-parse-this-item)
           (om-elem-item-get-sublist)
           (om-elem-to-trimmed-string))
      => (:result "- two"
                  "- three")
      (:content "- one"
                "- two")
      (->> (om-elem-parse-this-item)
           (om-elem-item-get-sublist)
           (om-elem-to-trimmed-string))
      => "")

    (defexamples-content om-elem-item-get-paragraph
      nil
      (:content "- one")
      (->> (om-elem-parse-this-item)
           (om-elem-item-get-paragraph)
           (om-elem-to-trimmed-string))
      => "one"
      (:content "- [ ] one")
      (->> (om-elem-parse-this-item)
           (om-elem-item-get-paragraph)
           (om-elem-to-trimmed-string))
      => "one"
      (:content "- tmsu :: one")
      (->> (om-elem-parse-this-item)
           (om-elem-item-get-paragraph)
           (om-elem-to-trimmed-string))
      => "one"
      (:content "- tmsu ::")
      (->> (om-elem-parse-this-item)
           (om-elem-item-get-paragraph)
           (om-elem-to-trimmed-string))
      => ""))

  (def-example-subgroup "Plain List"
    nil
    
    (defexamples-content om-elem-plain-list-set-type
      nil
      (:content "- [ ] one"
                "- [X] two")
      (->> (om-elem-parse-this-element)
           (om-elem-plain-list-set-type 'ordered)
           (om-elem-to-trimmed-string))
      => (:result "1. [ ] one"
                  "2. [X] two")
      (:content "1. [ ] one"
                "2. [X] two")
      (->> (om-elem-parse-this-element)
           (om-elem-plain-list-set-type '-)
           (om-elem-to-trimmed-string))
      => (:result "- [ ] one"
                  "- [X] two"))

    (defexamples-content om-elem-plain-list-indent-item
      nil
      (:content "- one"
                "- two"
                "  - three"
                "- four")
      (:comment "It makes no sense to indent the first item")
      (->> (om-elem-parse-element-at 1)
           (om-elem-plain-list-indent-item 0)
           (om-elem-to-trimmed-string))
      !!> error
      (->> (om-elem-parse-element-at 1)
           (om-elem-plain-list-indent-item 1)
           (om-elem-to-trimmed-string))
      => (:result "- one"
                  "  - two"
                  "  - three"
                  "- four")
      (->> (om-elem-parse-element-at 1)
           (om-elem-plain-list-indent-item 2)
           (om-elem-to-trimmed-string))
      => (:result "- one"
                  "- two"
                  "  - three"
                  "  - four"))

    (defexamples-content om-elem-plain-list-indent-item-tree
      nil
      (:content "- one"
                "- two"
                "  - three"
                "- four")
      (->> (om-elem-parse-element-at 1)
           (om-elem-plain-list-indent-item-tree 1)
           (om-elem-to-trimmed-string))
      => (:result "- one"
                  "  - two"
                  "    - three"
                  "- four"))

    (defexamples-content om-elem-plain-list-unindent-item
      nil
      (:content "- one"
                "- two"
                "  - three"
                "  - three"
                "  - three"
                "- four")
      (->> (om-elem-parse-element-at 1)
           (om-elem-plain-list-unindent-item 1 0)
           (om-elem-to-trimmed-string))
      => (:result "- one"
                  "- two"
                  "- three"
                  "  - three"
                  "  - three"
                  "- four")
      (->> (om-elem-parse-element-at 1)
           (om-elem-plain-list-unindent-item 1 1)
           (om-elem-to-trimmed-string))
      => (:result "- one"
                  "- two"
                  "  - three"
                  "- three"
                  "  - three"
                  "- four")
      (->> (om-elem-parse-element-at 1)
           (om-elem-plain-list-unindent-item 2 1)
           (om-elem-to-trimmed-string))
      => (:result "- one"
                  "- two"
                  "  - three"
                  "  - three"
                  "  - three"
                  "- four"))
  
    (defexamples-content om-elem-plain-list-unindent-items
      nil
      (:content "- one"
                "- two"
                "  - three"
                "  - three"
                "  - three"
                "- four")
      (->> (om-elem-parse-element-at 1)
           (om-elem-plain-list-unindent-items 1)
           (om-elem-to-trimmed-string))
      => (:result "- one"
                  "- two"
                  "- three"
                  "- three"
                  "- three"
                  "- four")
      (->> (om-elem-parse-element-at 1)
           (om-elem-plain-list-unindent-items 2)
           (om-elem-to-trimmed-string))
      => (:result "- one"
                  "- two"
                  "  - three"
                  "  - three"
                  "  - three"
                  "- four")))

  (def-example-subgroup "Table"
    nil

    (defexamples-content om-elem-table-get-cell
      nil
      (:content "| 1 | 2 | 3 |"
                "|---+---+---|"
                "| a | b | c |")
      (->> (om-elem-parse-this-element)
           (om-elem-table-get-cell 0 0)
           (om-elem--get-contents)
           (car))
      => "1"
      (->> (om-elem-parse-this-element)
           (om-elem-table-get-cell 1 0)
           (om-elem--get-contents)
           (car))
      => "a"
      (->> (om-elem-parse-this-element)
           (om-elem-table-get-cell 0 2)
           (om-elem--get-contents)
           (car))
      => "3"
      (->> (om-elem-parse-this-element)
           (om-elem-table-get-cell 0 3)
           (om-elem--get-contents)
           (car))
      !!> error)

    (defexamples-content om-elem-table-delete-column
      nil
      (:content "| a | b |"
                "|---+---|"
                "| c | d |")
      (->> (om-elem-parse-element-at 1)
           (om-elem-table-delete-column 0)
           (om-elem-to-trimmed-string))
      => (:result "| b |"
                  "|---|"
                  "| d |")
      (->> (om-elem-parse-element-at 1)
           (om-elem-table-delete-column 1)
           (om-elem-to-trimmed-string))
      => (:result "| a |"
                  "|---|"
                  "| c |")
      (->> (om-elem-parse-element-at 1)
           (om-elem-table-delete-column -1)
           (om-elem-to-trimmed-string))
      => (:result "| a |"
                  "|---|"
                  "| c |"))

    (defexamples-content om-elem-table-delete-row
      nil
      (:content "| a | b |"
                "|---+---|"
                "| c | d |")
      (->> (om-elem-parse-element-at 1)
           (om-elem-table-delete-row 0)
           (om-elem-to-trimmed-string))
      => (:result "|---+---|"
                  "| c | d |")
      (->> (om-elem-parse-element-at 1)
           (om-elem-table-delete-row 1)
           (om-elem-to-trimmed-string))
      => (:result "| a | b |"
                  "| c | d |")
      (->> (om-elem-parse-element-at 1)
           (om-elem-table-delete-row -1)
           (om-elem-to-trimmed-string))
      => (:result "| a | b |"
                  "|---+---|"))

    (defexamples-content om-elem-table-insert-column
      nil
      (:content "| a | b |"
                "|---+---|"
                "| c | d |")
      (->> (om-elem-parse-element-at 1)
           (om-elem-table-insert-column
            1
            (list
             (om-elem-build-table-cell "x")
             (om-elem-build-table-cell "y")))
           (om-elem-to-trimmed-string))
      => (:result "| a | x | b |"
                  "|---+---+---|"
                  "| c | y | d |")
      (->> (om-elem-parse-element-at 1)
           (om-elem-table-insert-column
            -1
            (list
             (om-elem-build-table-cell "x")
             (om-elem-build-table-cell "y")))
           (om-elem-to-trimmed-string))
      => (:result "| a | b | x |"
                  "|---+---+---|"
                  "| c | d | y |"))

    (defexamples-content om-elem-table-insert-column!
      nil
      (:content "| a | b |"
                "|---+---|"
                "| c | d |")
      (->> (om-elem-parse-element-at 1)
           (om-elem-table-insert-column! 1 '("x" "y"))
           (om-elem-to-trimmed-string))
      => (:result "| a | x | b |"
                  "|---+---+---|"
                  "| c | y | d |")
      (->> (om-elem-parse-element-at 1)
           (om-elem-table-insert-column! -1 '("x" "y"))
           (om-elem-to-trimmed-string))
      => (:result "| a | b | x |"
                  "|---+---+---|"
                  "| c | d | y |"))

    (defexamples-content om-elem-table-insert-row
      nil
      (:content "| a | b |"
                "|---+---|"
                "| c | d |")
      (->> (om-elem-parse-element-at 1)
           (om-elem-table-insert-row
            1
            (list
             (om-elem-build-table-cell "x")
             (om-elem-build-table-cell "y")))
           (om-elem-to-trimmed-string))
      => (:result "| a | b |"
                  "| x | y |"
                  "|---+---|"
                  "| c | d |")
      (->> (om-elem-parse-element-at 1)
           (om-elem-table-insert-row
            2
            (list
             (om-elem-build-table-cell "x")
             (om-elem-build-table-cell "y")))
           (om-elem-to-trimmed-string))
      => (:result "| a | b |"
                  "|---+---|"
                  "| x | y |"
                  "| c | d |")
      (->> (om-elem-parse-element-at 1)
           (om-elem-table-insert-row
            -1
            (list
             (om-elem-build-table-cell "x")
             (om-elem-build-table-cell "y")))
           (om-elem-to-trimmed-string))
      => (:result "| a | b |"
                  "|---+---|"
                  "| c | d |"
                  "| x | y |"))

    (defexamples-content om-elem-table-insert-row!
      nil
      (:content "| a | b |"
                "|---+---|"
                "| c | d |")
      (->> (om-elem-parse-element-at 1)
           (om-elem-table-insert-row! 1 '("x" "y"))
           (om-elem-to-trimmed-string))
      => (:result "| a | b |"
                  "| x | y |"
                  "|---+---|"
                  "| c | d |")
      (->> (om-elem-parse-element-at 1)
           (om-elem-table-insert-row! 2 '("x" "y"))
           (om-elem-to-trimmed-string))
      => (:result "| a | b |"
                  "|---+---|"
                  "| x | y |"
                  "| c | d |")
      (->> (om-elem-parse-element-at 1)
           (om-elem-table-insert-row! -1 '("x" "y"))
           (om-elem-to-trimmed-string))
      => (:result "| a | b |"
                  "|---+---|"
                  "| c | d |"
                  "| x | y |"))))

(def-example-group "Element matching functions"
  "match shit"

  (defexamples-content om-elem-find
    "docstring"

    (:content "* headline one"
              "** TODO headline two"
              "** COMMENT headline three"
              "** headline four")

    (:comment "Use a symbol to match a type, in this case all "
              "headlines.")
    (->> (om-elem-parse-this-subtree)
         (om-elem-find '(headline))
         (--map (om-elem-to-trimmed-string it)))
    => '("** TODO headline two"
         "** COMMENT headline three"
         "** headline four")

    (:comment "Use integers specify the index to return. Negative "
              "integers count from the end. Out of range integers "
              "return nil")
    (->> (om-elem-parse-this-subtree)
         (om-elem-find '(1))
         (--map (om-elem-to-trimmed-string it)))
    => '("** COMMENT headline three")
    (->> (om-elem-parse-this-subtree)
         (om-elem-find '(-1))
         (--map (om-elem-to-trimmed-string it)))
    => '("** headline four")
    (->> (om-elem-parse-this-subtree)
         (om-elem-find '(3))
         (--map (om-elem-to-trimmed-string it)))
    => nil

    (:comment "Use a two-membered list with an operator and an "
              "integer to match a range of indices. Allowed "
              "operators are <, >, <=, and and >=.")
    (->> (om-elem-parse-this-subtree)
         (om-elem-find '((> 0)))
         (--map (om-elem-to-trimmed-string it)))
    => '("** COMMENT headline three" "** headline four")
    (->> (om-elem-parse-this-subtree)
         (om-elem-find '((>= 1)))
         (--map (om-elem-to-trimmed-string it)))
    => '("** COMMENT headline three" "** headline four")

    (:comment "Use a plist to match based on an elements properties.")
    (->> (om-elem-parse-this-subtree)
         (om-elem-find '((:todo-keyword "TODO")))
         (--map (om-elem-to-trimmed-string it)))
    => '("** TODO headline two")
    (->> (om-elem-parse-this-subtree)
         (om-elem-find '((:todo-keyword nil)))
         (--map (om-elem-to-trimmed-string it)))
    => '("** COMMENT headline three" "** headline four")
    (->> (om-elem-parse-this-subtree)
         (om-elem-find '((:todo-keyword "DONE")))
         (--map (om-elem-to-trimmed-string it)))
    => nil

    (:content "* headline one"
              "this is *text1* of *text2*"
              "** headline two"
              "here is more *text3*"
              "*** headline three"
              "and here is even more *text4* and *text5*"
              "**** headline 4")
    (:comment "Specify multiple levels of matching using multiple "
              "queries.")
    (->> (om-elem-parse-this-subtree)
         (om-elem-find '(section paragraph bold))
         (--map (om-elem-to-trimmed-string it)))
    => '("*text1*" "*text2*")

    (:comment "Use the keyword :any as a wildcard to match any "
              "element at a particular level.")
    (->> (om-elem-parse-this-subtree)
         (om-elem-find '(:any :any bold))
         (--map (om-elem-to-trimmed-string it)))
    => '("*text1*" "*text2*")
    ;; TODO not sure why an empty string comes out here
    (->> (om-elem-parse-this-subtree)
         (om-elem-find '(section paragraph :any))
         (--map (om-elem-to-trimmed-string it)))
    => '("this is" "*text1*" "of" "*text2*" "")
    (->> (om-elem-parse-this-subtree)
         (om-elem-find '(:any bold))
         (--map (om-elem-to-trimmed-string it)))
    => nil

    (:comment "Use the keyword :many to match one or more levels "
              "of any element.")
    (->> (om-elem-parse-this-subtree)
         (om-elem-find '(:many bold))
         (--map (om-elem-to-trimmed-string it)))
    => '("*text1*" "*text2*" "*text3*" "*text4*" "*text5*")

    (:comment "Use the keyword :many! to match one or more levels, "
              "except unlike :many do not match within any elements "
              "that have already matched.")
    ;; TODO add predicate???
    (->> (om-elem-parse-this-subtree)
         (om-elem-find '(headline :many! headline))
         (--map (om-elem-to-trimmed-string it)))
    => '("*** headline three
and here is even more *text4* and *text5*
**** headline 4"))
  
  (defexamples-content om-elem-find-first
    nil
    (:content
     "* headline one"
     "** TODO headline two"
     "** COMMENT headline three"
     "** headline four")
    (:comment "Find the first subheadline")
    (->> (om-elem-parse-this-subtree)
         (om-elem-find-first '(headline))
         (om-elem-to-trimmed-string))
    => "** TODO headline two")

  (defexamples-content om-elem-find-last
    nil
    (:content "* headline one"
               "** TODO headline two"
               "** COMMENT headline three"
               "** headline four")
    (:comment "Find the last subheadline")
    (->> (om-elem-parse-this-subtree)
         (om-elem-find-last '(headline))
         (om-elem-to-trimmed-string))
    => "** headline four")

  (defexamples-content om-elem-delete
    nil
    (:content "* headline one"
               "** headline two"
               "** headline three"
               "** headline four")
    (:comment "Selectively delete headlines")
    (->> (om-elem-parse-this-subtree)
         (om-elem-delete '(headline))
         (om-elem-to-trimmed-string))
    => "* headline one"
    (->> (om-elem-parse-this-subtree)
         (om-elem-delete-first '(headline))
         (om-elem-to-trimmed-string))
    => (:result "* headline one"
                "** headline three"
                "** headline four")
    (->> (om-elem-parse-this-subtree)
         (om-elem-delete-last '(headline))
         (om-elem-to-trimmed-string))
    => (:result "* headline one"
                "** headline two"
                "** headline three"))

  ;; TODO add extract
  (defexamples-content om-elem-extract nil)

  (defexamples-content om-elem-map
    nil

    (:content "* headline one"
               "** TODO headline two"
               "** headline three"
               "** headline four")

    (:comment "Selectively mark headlines as DONE")
    (->> (om-elem-parse-this-subtree)
         (om-elem-map '(headline) (lambda (it) (om-elem-set-property :todo-keyword "DONE" it)))
         (om-elem-to-trimmed-string))
    => (:result "* headline one"
                "** DONE headline two"
                "** DONE headline three"
                "** DONE headline four")
    (->> (om-elem-parse-this-subtree)
         (om-elem-map-first* '(headline) (om-elem-set-property :todo-keyword "DONE" it))
         (om-elem-to-trimmed-string))
    => (:result "* headline one"
                "** DONE headline two"
                "** headline three"
                "** headline four")
    (->> (om-elem-parse-this-subtree)
         (om-elem-map-last '(headline) (-partial #'om-elem-set-property :todo-keyword "DONE"))
         (om-elem-to-trimmed-string))
    => (:result "* headline one"
                "** TODO headline two"
                "** headline three"
                "** DONE headline four"))
  
  ;; TODO add mapcat
  (defexamples-content om-elem-mapcat nil)

  ;; TODO add replace
  (defexamples-content om-elem-replace nil)

  ;; TOOD add insert-before
  (defexamples-content om-elem-insert-before nil)

  ;; TOOD add insert-after
  (defexamples-content om-elem-insert-after nil)

  ;; TOOD add insert-within
  (defexamples-content om-elem-insert-within nil)

  ;; TOOD add splice-before
  (defexamples-content om-elem-splice-before nil)

  ;; TOOD add splice-after
  (defexamples-content om-elem-splice-after nil)

  ;; TOOD add splice-within
  (defexamples-content om-elem-splice-within nil))
