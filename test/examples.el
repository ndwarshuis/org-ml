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

;;; Code:

;; (require 'om)
(require 's)

(def-example-group "Buffer Parsing"
  "Parse buffers to trees."

  (defexamples-content om-parse-object-at
    nil
    (:buffer "*text*")
    (->> (om-parse-object-at 1)
         (om-get-type))
    => 'bold

    :begin-hidden
    (:buffer "~text~")
    (->> (om-parse-object-at 1)
         (om-get-type))
    => 'code
    ;; TODO add entity
    ;; TODO add export snippet
    (:buffer "[fn:1:text]")
    (->> (om-parse-object-at 1)
         (om-get-type))
    => 'footnote-reference
    (:buffer "call_name()")
    (->> (om-parse-object-at 1)
         (om-get-type))
    => 'inline-babel-call
    (:buffer "src_emacs{}")
    (->> (om-parse-object-at 1)
         (om-get-type))
    => 'inline-src-block
    (:buffer "/text/")
    (->> (om-parse-object-at 1)
         (om-get-type))
    => 'italic
    (:buffer "\\\\")
    (->> (om-parse-object-at 1)
         (om-get-type))
    => 'line-break
    ;; TODO add latex frag
    (:buffer "[[path][desc]]")
    (->> (om-parse-object-at 1)
         (om-get-type))
    => 'link
    (:buffer "{{{macro}}}")
    (->> (om-parse-object-at 1)
         (om-get-type))
    => 'macro
    (:buffer "<<<text>>>")
    (->> (om-parse-object-at 1)
         (om-get-type))
    => 'radio-target
    (:buffer "[1/2]")
    (->> (om-parse-object-at 1)
         (om-get-type))
    => 'statistics-cookie
    (:buffer "+text+")
    (->> (om-parse-object-at 1)
         (om-get-type))
    => 'strike-through
    ;; TODO this is confusing for docs
    (:buffer "a_b")
    (->> (om-parse-object-at 3)
         (om-get-type))
    => 'subscript
    (:buffer "a^b")
    (->> (om-parse-object-at 3)
         (om-get-type))
    => 'superscript
    (:buffer "| a |")
    (->> (om-parse-object-at 2)
         (om-get-type))
    => 'table-cell
    (:buffer "<<text>>")
    (->> (om-parse-object-at 1)
         (om-get-type))
    => 'target
    :end-hidden

    (:buffer "[2019-01-01 Tue]")
    (->> (om-parse-object-at 1)
         (om-get-type))
    => 'timestamp

    :begin-hidden
    (:buffer "_text_")
    (->> (om-parse-object-at 1)
         (om-get-type))
    => 'underline
    (:buffer "=text=")
    (->> (om-parse-object-at 1)
         (om-get-type))
    => 'verbatim
    :end-hidden

    (:buffer "- notme")
    (:comment "Return nil when parsing an element")
    (om-parse-object-at 1)
    => nil)

  (defexamples-content om-parse-element-at
    nil
    (:buffer "#+CALL: ktulu()")
    (->> (om-parse-element-at 1)
         (om-get-type))
    => 'babel-call

    :begin-hidden
    (:buffer "#+BEGIN_CENTER"
             "#+END_CENTER")
    (->> (om-parse-element-at 1)
         (om-get-type))
    => 'center-block
    (:buffer "CLOCK: [2019-01-01 Tue]")
    (->> (om-parse-element-at 1)
         (om-get-type))
    => 'clock
    (:buffer "# oops I looked")
    (->> (om-parse-element-at 1)
         (om-get-type))
    => 'comment
    (:buffer "#+BEGIN_COMMENT"
             "oops I looked again"
             "#+END_COMMENT")
    (->> (om-parse-element-at 1)
         (om-get-type))
    => 'comment-block
    (:buffer "%%(diary of a madman)")
    (->> (om-parse-element-at 1)
         (om-get-type))
    => 'diary-sexp
    (:buffer ":DRAWER:"
             "- underwear"
             "- savings account"
             ":END:")
    (->> (om-parse-element-at 1)
         (om-get-type))
    => 'drawer
    (:buffer "#+BEGIN countdown"
             "#+END")
    (->> (om-parse-element-at 1)
         (om-get-type))
    => 'dynamic-block
    (:buffer "#+BEGIN_EXAMPLE"
             "#+END_EXAMPLE")
    (->> (om-parse-element-at 1)
         (om-get-type))
    => 'example-block
    (:buffer "#+BEGIN_EXPORT latex"
             "#+END_EXPORT")
    (->> (om-parse-element-at 1)
         (om-get-type))
    => 'export-block
    (:buffer ": mini mini mini")
    (->> (om-parse-element-at 1)
         (om-get-type))
    => 'fixed-width
    (:buffer "[fn:1]")
    (->> (om-parse-element-at 1)
         (om-get-type))
    => 'footnote-definition
    (:buffer "* murder, young girl killed"
             "* desperate shooting at echo's hill")
    (->> (om-parse-element-at 1)
         (om-get-type))
    => 'headline
    (:buffer "-----")
    (->> (om-parse-element-at 1)
         (om-get-type))
    => 'horizontal-rule
    ;; TODO add inlinetask
    (:buffer "#+QUOTE: unquote")
    (->> (om-parse-element-at 1)
         (om-get-type))
    => 'keyword
    ;; TODO add latex env
    (:buffer "* headline"
             ":PROPERTIES:"
             ":key: val"
             ":END:")
    (->> (om-parse-element-at 25)
         (om-get-type))
    => 'node-property
    (:buffer "Just for the record"
             "The weather today is slightly sarcastic with a good chance of"
             "A. Indifference and B. disinterest in what the critics say")
    (->> (om-parse-element-at 1)
         (om-get-type))
    => 'paragraph
    :end-hidden
    
    (:buffer "- plain-list")
    (:comment "Give the plain-list, not the item for this function")
    (->> (om-parse-element-at 1)
         (om-get-type))
    => 'plain-list

    :begin-hidden
    (:buffer "* deadhead"
             "DEADLINE: [2019-01-01 Tue]")
    (->> (om-parse-element-at 12)
         (om-get-type))
    => 'planning
    (:buffer "* headline"
             ":PROPERTIES:"
             ":END:")
    (->> (om-parse-element-at 12)
         (om-get-type))
    => 'property-drawer
    (:buffer "#+BEGIN_QUOTE"
             "Oh glorious cheeseburger, we bow to thee"
             "The secrets of the universe are between the buns"
             "#+END_QUOTE")
    (->> (om-parse-element-at 12)
         (om-get-type))
    => 'quote-block
    (:buffer "#+begin_dot dot.png"
             "#+end_dot")
    (->> (om-parse-element-at 1)
         (om-get-type))
    => 'special-block
    (:buffer "#+BEGIN_SRC emacs"
             "(launch-missiles)"
             "#+END_SRC")
    (->> (om-parse-element-at 1)
         (om-get-type))
    => 'src-block
    :end-hidden
    
    (:buffer "| R | A |"
             "| G | E |")
    (:comment "Return a table, not the table-row for this function")
    (->> (om-parse-element-at 1)
         (om-get-type))
    => 'table

    :begin-hidden
    (:buffer "#+BEGIN_VERSE"
             "#+END_VERSE")
    (->> (om-parse-element-at 1)
         (om-get-type))
    => 'verse-block
    :end-hidden)

  (defexamples-content om-parse-headline-at
    nil
    (:buffer "* headline")
    (:comment "Return the headline itself")
    (->> (om-parse-headline-at 1)
         (om-to-trimmed-string))
    => "* headline"
    (:buffer "* headline"
             "section crap")
    (:comment "Return headline and section")
    (->> (om-parse-headline-at 1)
         (om-to-trimmed-string))
    => (:result "* headline"
                "section crap")
    (:comment "Return headline when point is in the section")
    (->> (om-parse-headline-at 12)
         (om-to-trimmed-string))
    => (:result "* headline"
                "section crap")
    (:buffer "* headline"
             "section crap"
             "** not parsed")
    (:comment "Don't parse any subheadlines")
    (->> (om-parse-headline-at 1)
         (om-to-trimmed-string))
    => (:result "* headline"
                "section crap")
    (:buffer "nothing nowhere")
    (:comment "Return nil if not under a headline")
    (->> (om-parse-headline-at 1)
         (om-to-trimmed-string))
    => "")

  (defexamples-content om-parse-subtree-at
    nil
    (:buffer "* headline")
    (:comment "Return the headline itself")
    (->> (om-parse-subtree-at 1)
         (om-to-trimmed-string))
    => "* headline"
    (:buffer "* headline"
             "section crap")
    (:comment "Return headline and section")
    (->> (om-parse-subtree-at 1)
         (om-to-trimmed-string))
    => (:result "* headline"
                "section crap")
    (:comment "Return headline when point is in the section")
    (->> (om-parse-subtree-at 12)
         (om-to-trimmed-string))
    => (:result "* headline"
                "section crap")
    (:buffer "* headline"
             "section crap"
             "** parsed")
    (:comment "Return all the subheadlines")
    (->> (om-parse-subtree-at 1)
         (om-to-trimmed-string))
    => (:result "* headline"
                "section crap"
                "** parsed")
    (:buffer "nothing nowhere")
    (:comment "Return nil if not under a headline")
    (->> (om-parse-subtree-at 1)
         (om-to-trimmed-string))
    => "")

  (defexamples-content om-parse-item-at
    nil
    (:buffer "- item")
    (:comment "Return the item itself")
    (->> (om-parse-item-at 1)
         (om-to-trimmed-string))
    => "- item"
    (:comment "Also return the item when not at beginning of line")
    (->> (om-parse-item-at 5)
         (om-to-trimmed-string))
    => "- item"
    (:buffer "- item"
             "  - item 2")
    (:comment "Return item and its subitems")
    (->> (om-parse-item-at 1)
         (om-to-trimmed-string))
    => (:result "- item"
                "  - item 2")
    (:buffer "* not item")
    (:comment "Return nil if not an item")
    (->> (om-parse-item-at 1)
         (om-to-trimmed-string))
    => "")

  (defexamples-content om-parse-table-row-at
    nil
    (:buffer "| bow | stroke |")
    (:comment "Return the row itself")
    (->> (om-parse-table-row-at 1)
         (om-to-trimmed-string))
    => "| bow | stroke |"
    (:comment "Also return the row when not at beginning of line")
    (->> (om-parse-table-row-at 5)
         (om-to-trimmed-string))
    => "| bow | stroke |"
    (:buffer "- bow and arrow choke")
    (:comment "Return nil if not a table-row")
    (->> (om-parse-table-row-at 1)
         (om-to-trimmed-string))
    => "")
  
  (defexamples-content om-parse-section-at
    nil
    (:buffer "over headline"
             "* headline"
             "under headline")
    (:comment "Return the section above the headline")
    (->> (om-parse-section-at 1)
         (om-to-trimmed-string))
    => "over headline"
    (:comment "Return the section under headline")
    (->> (om-parse-section-at 25)
         (om-to-trimmed-string))
    => "under headline"
    (:buffer "* headline"
             "** subheadline")
    (:comment "Return nil if no section under headline")
    (->> (om-parse-section-at 1)
         (om-to-trimmed-string))
    => ""
    (:buffer "")
    (:comment "Return nil if no section at all")
    (->> (om-parse-section-at 1)
         (om-to-trimmed-string))
    => ""))

(def-example-group "String Conversion"
  "Convert nodes to strings."

  (defexamples om-to-string
    (om-to-string
     '(bold
       (:begin 1 :end 5 :parent nil :post-blank 0 :post-affiliated nil)
       "text"))
    => "*text*"
    (om-to-string
     '(bold
       (:begin 1 :end 5 :parent nil :post-blank 3 :post-affiliated nil)
       "text"))
    => "*text*   "
    (om-to-string nil) => "")

  (defexamples om-to-trimmed-string
    (om-to-trimmed-string
     '(bold
       (:begin 1 :end 5 :parent nil :post-blank 0 :post-affiliated nil)
       "text"))
    => "*text*"
    (om-to-trimmed-string
     '(bold
       (:begin 1 :end 5 :parent nil :post-blank 3 :post-affiliated nil)
       "text"))
    => "*text*"
    (om-to-trimmed-string nil) => ""))

(def-example-group "Building"
  "Build new nodes."

  (def-example-subgroup "Leaf Objects"
    nil

    (defexamples om-build-code
      (->> (om-build-code "text")
           (om-to-trimmed-string)) => "~text~")

    (defexamples om-build-entity
      (->> (om-build-entity "gamma")
           (om-to-trimmed-string)) => "\\gamma")

    (defexamples om-build-inline-babel-call
      (->> (om-build-inline-babel-call "name")
           (om-to-trimmed-string)) => "call_name()"
      (->> (om-build-inline-babel-call "name" :arguments '("n=4"))
           (om-to-trimmed-string)) => "call_name(n=4)"
      (->> (om-build-inline-babel-call "name" :inside-header '(:key val))
           (om-to-trimmed-string)) => "call_name[:key val]()"
      (->> (om-build-inline-babel-call "name" :end-header '(:key val))
           (om-to-trimmed-string)) => "call_name()[:key val]")

    (defexamples om-build-inline-src-block
      (->> (om-build-inline-src-block "lang" "value")
           (om-to-trimmed-string)) => "src_lang{value}"
      (->> (om-build-inline-src-block "lang" "value" :parameters '(:key val))
           (om-to-trimmed-string)) => "src_lang[:key val]{value}")

    (defexamples om-build-line-break
      (->> (om-build-line-break)
           (om-to-trimmed-string)) => "\\\\")

    (defexamples om-build-statistics-cookie
      (->> (om-build-statistics-cookie '(nil))
           (om-to-trimmed-string)) => "[%]"
      (->> (om-build-statistics-cookie '(50))
           (om-to-trimmed-string)) => "[50%]"
      (->> (om-build-statistics-cookie '(1 3))
           (om-to-trimmed-string)) => "[1/3]"
      (->> (om-build-statistics-cookie '(nil nil))
           (om-to-trimmed-string)) => "[/]")

    (defexamples om-build-target
      (->> (om-build-target "text")
           (om-to-trimmed-string)) => "<<text>>")

    (defexamples om-build-timestamp
      (->> (om-build-timestamp 'inactive 2019 1 15 2019 1 15)
           (om-to-trimmed-string))
      => "[2019-01-15 Tue]"
      (->> (om-build-timestamp 'active-range 2019 1 15 2019 1 16)
           (om-to-trimmed-string))
      => "<2019-01-15 Tue>--<2019-01-16 Wed>"
      (->> (om-build-timestamp
            'inactive 2019 1 15 2019 1 15 :warning-type 'all
            :warning-unit 'day :warning-value 1)
           (om-to-trimmed-string))
      => "[2019-01-15 Tue -1d]")

    (defexamples om-build-verbatim
      (->> (om-build-verbatim "text")
           (om-to-trimmed-string)) => "=text="))

  (def-example-subgroup "Branch Objects"
    nil

    (defexamples om-build-bold
      (->> (om-build-bold "text")
           (om-to-trimmed-string)) => "*text*")

    (defexamples om-build-footnote-reference
      (->> (om-build-footnote-reference)
           (om-to-trimmed-string)) => "[fn:]"
      (->> (om-build-footnote-reference :label "label")
           (om-to-trimmed-string)) => "[fn:label]"
      (->> (om-build-footnote-reference :label "label" "content")
           (om-to-trimmed-string)) => "[fn:label:content]")

    (defexamples om-build-italic
      (->> (om-build-italic "text")
           (om-to-trimmed-string)) => "/text/")

    (defexamples om-build-link
      (->> (om-build-link "target")
           (om-to-trimmed-string)) => "[[target]]"
      (->> (om-build-link "target" :type "file")
           (om-to-trimmed-string)) => "[[file:target]]"
      (->> (om-build-link "target" "desc")
           (om-to-trimmed-string)) => "[[target][desc]]")

    (defexamples om-build-radio-target
      (->> (om-build-radio-target "text")
           (om-to-trimmed-string)) => "<<<text>>>")

    (defexamples om-build-strike-through
      (->> (om-build-strike-through "text")
           (om-to-trimmed-string)) => "+text+")

    (defexamples om-build-superscript
      (->> (om-build-superscript "text")
           (om-to-trimmed-string)) => "^text")

    (defexamples om-build-subscript
      (->> (om-build-subscript "text")
           (om-to-trimmed-string)) => "_text")

    (defexamples om-build-table-cell
      (->> (om-build-table-cell "text")
           (om-build-table-row)
           (om-to-trimmed-string)) => "| text |")

    (defexamples om-build-underline
      (->> (om-build-underline "text")
           (om-to-trimmed-string)) => "_text_"))

  (def-example-subgroup "Leaf Elements"
    nil

    (defexamples om-build-babel-call
      (->> (om-build-babel-call "name")
           (om-to-trimmed-string)) => "#+CALL: name()"
      (->> (om-build-babel-call "name" :arguments '("arg=x"))
           (om-to-trimmed-string)) => "#+CALL: name(arg=x)"
      (->> (om-build-babel-call "name" :inside-header '(:key val))
           (om-to-trimmed-string)) => "#+CALL: name[:key val]()"
      (->> (om-build-babel-call "name" :end-header '(:key val))
           (om-to-trimmed-string)) => "#+CALL: name() :key val")

    (defexamples om-build-clock
      (->> (om-build-clock (om-build-timestamp! 'inactive '(2019 1 1 0 0)))
           (om-to-trimmed-string))
      => "CLOCK: [2019-01-01 Tue 00:00]"
      (->> (om-build-clock (om-build-timestamp! 'inactive '(2019 1 1 0 0) :end '(2019 1 1 1 0)))
           (om-to-trimmed-string))
      => "CLOCK: [2019-01-01 Tue 00:00]--[2019-01-01 Tue 01:00] =>  1:00")

    (defexamples om-build-comment
      (->> (om-build-comment "text")
           (om-to-trimmed-string)) => "# text")

    (defexamples om-build-comment-block
      (->> (om-build-comment-block "text")
           (om-to-trimmed-string)) => (:result "#+BEGIN_COMMENT"
           "text"
           "#+END_COMMENT"))

    (defexamples om-build-diary-sexp
      (->> (om-build-diary-sexp '(text))
           (om-to-trimmed-string)) => "%%(text)")

    (defexamples om-build-example-block
      (->> (om-build-example-block "text")
           (om-to-trimmed-string)) => (:result "#+BEGIN_EXAMPLE"
           "text"
           "#+END_EXAMPLE")
      (->> (om-build-example-block "text" :switches '("switches"))
           (om-to-trimmed-string)) => (:result "#+BEGIN_EXAMPLE switches"
           "text"
           "#+END_EXAMPLE"))

    (defexamples om-build-export-block
      (->> (om-build-export-block "type" "value\n")
           (om-to-trimmed-string)) => (:result "#+BEGIN_EXPORT type"
           "value"
           "#+END_EXPORT"))

    (defexamples om-build-fixed-width
      (->> (om-build-fixed-width "text")
           (om-to-trimmed-string)) => ": text")

    (defexamples om-build-horizontal-rule
      (->> (om-build-horizontal-rule)
           (om-to-trimmed-string)) => "-----")

    (defexamples om-build-keyword
      (->> (om-build-keyword "FILETAGS" "tmsu")
           (om-to-trimmed-string)) => "#+FILETAGS: tmsu")

    (defexamples om-build-latex-environment
      (->> (om-build-latex-environment '("env" "text"))
           (om-to-trimmed-string)) => (:result "\\begin{env}"
           "text"
           "\\end{env}"))

    (defexamples om-build-node-property
      (->> (om-build-node-property "key" "val")
           (om-to-trimmed-string)) => ":key:      val")

    (defexamples om-build-planning
      (->> (om-build-planning :closed (om-build-timestamp! 'inactive '(2019 1 1)))
           (om-to-trimmed-string)) => "CLOSED: [2019-01-01 Tue]"
      (->> (om-build-planning :scheduled (om-build-timestamp! 'inactive '(2019 1 1)))
           (om-to-trimmed-string)) => "SCHEDULED: [2019-01-01 Tue]"
      (->> (om-build-planning :deadline (om-build-timestamp! 'inactive '(2019 1 1)))
           (om-to-trimmed-string)) => "DEADLINE: [2019-01-01 Tue]")

    (defexamples om-build-src-block
      (->> (om-build-src-block "body")
           (om-to-trimmed-string)) => (:result "#+BEGIN_SRC"
           "  body"
           "#+END_SRC")
      (->> (om-build-src-block "body" :language "emacs-lisp")
           (om-to-trimmed-string)) => (:result "#+BEGIN_SRC emacs-lisp"
           "  body"
           "#+END_SRC")
      ;; TODO pretty sure this makes no sense...
      (->> (om-build-src-block "body" :switches '("-n 20" "-r"))
           (om-to-trimmed-string)) => (:result "#+BEGIN_SRC -n 20 -r"
           "  body"
           "#+END_SRC")
      ;; TODO and this...
      (->> (om-build-src-block "body" :parameters '(:key val))
           (om-to-trimmed-string)) => (:result "#+BEGIN_SRC :key val"
           "  body"
           "#+END_SRC"))

    (defexamples om-build-table-row-hline
      (->>  (om-build-table
             (om-build-table-row
              (om-build-table-cell "text"))
             (om-build-table-row-hline))
            (om-to-trimmed-string)) => (:result "| text |"
            "|------|")))

  (def-example-subgroup "Branch Elements with Child Objects"
    nil

    (defexamples om-build-paragraph
      (->> (om-build-paragraph "text")
           (om-to-trimmed-string))
      => "text")

    (defexamples om-build-table-row
      (->> (om-build-table-cell "a")
           (om-build-table-row)
           (om-to-trimmed-string))
      => "| a |")

    (defexamples om-build-verse-block
      ;; TODO why should I need to use newlines here?
      (->> (om-build-verse-block "text\n")
           (om-to-trimmed-string))
      => (:result "#+BEGIN_VERSE"
                  "text"
                  "#+END_VERSE")))

  (def-example-subgroup "Branch Elements with Child Elements"
    nil

    (defexamples om-build-center-block
      (->> (om-build-paragraph "text")
           (om-build-center-block)
           (om-to-trimmed-string)) => (:result "#+BEGIN_CENTER"
           "text"
           "#+END_CENTER"))

    (defexamples om-build-drawer
      (->> (om-build-paragraph "text")
           (om-build-drawer "NAME")
           (om-to-trimmed-string)) => (:result ":NAME:"
           "text"
           ":END:"))

    (defexamples om-build-footnote-definition
      (->> (om-build-paragraph "footnote contents")
           (om-build-footnote-definition "label")
           (om-to-trimmed-string)) => "[fn:label] footnote contents")

    ;; TODO, need tags and archive example, cumbersome since these outputs
    ;; have lots of whitespace
    (defexamples om-build-headline
      (->> (om-build-headline)
           (om-to-trimmed-string)) => "*"
      (->> (om-build-headline :title '("dummy"))
           (om-to-trimmed-string)) => "* dummy"
      (->> (om-build-headline :title '("dummy") :level 3)
           (om-to-trimmed-string)) => "*** dummy"
      (->> (om-build-headline :title '("dummy") :todo-keyword "DONE")
           (om-to-trimmed-string)) => "* DONE dummy"
      (->> (om-build-headline :title '("dummy") :priority ?A)
           (om-to-trimmed-string)) => "* [#A] dummy"
      (->> (om-build-headline :title '("dummy") :footnote-section-p t)
           (om-to-trimmed-string)) => "* Footnotes"
      (->> (om-build-headline :title '("dummy") :commentedp t)
           (om-to-trimmed-string)) => "* COMMENT dummy")

    (defexamples om-build-item
      (->> (om-build-paragraph "item contents")
           (om-build-item)
           (om-to-trimmed-string)) => "- item contents"
      (->> (om-build-paragraph "item contents")
           (om-build-item :bullet 1)
           (om-to-trimmed-string)) => "1. item contents"
      (->> (om-build-paragraph "item contents")
           (om-build-item :checkbox 'on)
           (om-to-trimmed-string)) => "- [X] item contents"
      (->> (om-build-paragraph "item contents")
           (om-build-item :tag '("tmsu"))
           (om-to-trimmed-string)) => "- tmsu :: item contents"
      (->> (om-build-paragraph "item contents")
           (om-build-item :counter 10)
           (om-to-trimmed-string)) => "- [@10] item contents")

    (defexamples om-build-plain-list
      (->> (om-build-paragraph "item contents")
           (om-build-item)
           (om-build-plain-list)
           (om-to-trimmed-string)) => "- item contents")

    (defexamples om-build-property-drawer
      (->> (om-build-node-property "key" "val")
           (om-build-property-drawer)
           (om-to-trimmed-string)) => (:result ":PROPERTIES:"
           ":key:      val"
           ":END:"))

    (defexamples om-build-quote-block
      (->> (om-build-paragraph "quoted stuff")
           (om-build-quote-block)
           (om-to-trimmed-string)) => (:result "#+BEGIN_QUOTE"
           "quoted stuff"
           "#+END_QUOTE"))

    (defexamples om-build-section
      (->> (om-build-paragraph "text")
           (om-build-section)
           (om-to-trimmed-string)) => "text")

    (defexamples om-build-table
      (->> (om-build-table-cell "cell")
           (om-build-table-row)
           (om-build-table)
           (om-to-trimmed-string)) => "| cell |")

    (def-example-subgroup "Miscellaneous Builders"
      nil

      (defexamples om-build-timestamp-diary-sexp
        (->> (om-build-timestamp-diary-sexp '(diary-float t 4 2))
             (om-to-string))
        => "<%%(diary-float t 4 2)>")

      (defexamples om-build-table-row-hline
        ;; TODO this example is pretty dumb
        (->> (om-build-table-row-hline)
             (om-to-trimmed-string))
        => "|-"))

    (def-example-subgroup "Shorthand Builders"
      "Build nodes with more convenient/shorter syntax."

      (defexamples om-build-timestamp!
        (->> (om-build-timestamp! 'inactive '(2019 1 1))
             (om-to-string))
        => "[2019-01-01 Tue]"
        (->> (om-build-timestamp! 'inactive '(2019 1 1 12 0)
                                       :warning '(all 1 day)
                                       :repeater '(cumulate 1 month))
             (om-to-string))
        => "[2019-01-01 Tue 12:00 +1m -1d]"
        (->> (om-build-timestamp! 'inactive '(2019 1 1)
                                       :end '(2019 1 2))
             (om-to-string))
        => "[2019-01-01 Tue]--[2019-01-02 Wed]")

      (defexamples om-build-clock!
        (->> (om-build-clock! '(2019 1 1))
             (om-to-trimmed-string))
        => "CLOCK: [2019-01-01 Tue]"
        (->> (om-build-clock! '(2019 1 1 12 0))
             (om-to-trimmed-string))
        => "CLOCK: [2019-01-01 Tue 12:00]"
        (->> (om-build-clock! '(2019 1 1 12 0) :end '(2019 1 1 13 0))
             (om-to-trimmed-string))
        ;; TODO why does this make two individual timestamps?
        => "CLOCK: [2019-01-01 Tue 12:00]--[2019-01-01 Tue 13:00] =>  1:00")

      (defexamples om-build-planning!
        (->> (om-build-planning! :closed '(2019 1 1))
             (om-to-trimmed-string))
        => "CLOSED: [2019-01-01 Tue]"
        (->> (om-build-planning! :closed '(2019 1 1)
                                      :scheduled '(2018 1 1))
             (om-to-trimmed-string))
        => "SCHEDULED: [2018-01-01 Mon] CLOSED: [2019-01-01 Tue]"
        (->> (om-build-planning! :closed '(2019 1 1 &warning all 1 day &repeater cumulate 1 month))
             (om-to-trimmed-string))
        => "CLOSED: [2019-01-01 Tue +1m -1d]")

      (defexamples om-build-property-drawer!
        (->> (om-build-property-drawer! '(key val))
             (om-to-trimmed-string))
        => (:result ":PROPERTIES:"
                    ":key:      val"
                    ":END:"))

      (defexamples om-build-headline!
        (->> (om-build-headline! :title-text "really impressive title")
             (om-to-trimmed-string))
        => "* really impressive title"
        (->> (om-build-headline! :title-text "really impressive title"
                                      :statistics-cookie '(0 9000))
             (om-to-trimmed-string))
        => "* really impressive title [0/9000]"
        (->> (om-build-headline!
              :title-text "really impressive title"
              :properties '((key val))
              :section-children (list (om-build-paragraph! "section text"))
              ;; TODO make levels make sense
              (om-build-headline! :level 2 :title-text "subhead"))
             (om-to-trimmed-string))
        => (:result "* really impressive title"
                    ":PROPERTIES:"
                    ":key:      val"
                    ":END:"
                    "section text"
                    "** subhead")
        )

      (defexamples om-build-item!
        (->> (om-build-item!
              ;; TODO (x) should make x)
              :bullet '(1)
              :tag "complicated *tag*"
              :paragraph "petulant /frenzy/"
              (om-build-item! :bullet '- :paragraph "below"))
             (om-to-trimmed-string))
        => (:result "1. complicated *tag* :: petulant /frenzy/"
                    "   - below"))

      (defexamples om-build-paragraph!
        (->> (om-build-paragraph! "stuff /with/ *formatting*" :post-blank 2)
             (om-to-string))
        => (:result "stuff /with/ *formatting*"
                    ""
                    ""
                    "")
        (->> (om-build-paragraph! "* stuff /with/ *formatting*")
             (om-to-string))
        !!> error)

      (defexamples om-build-table!
        (->> (om-build-table! '("R" "A") '("G" "E"))
             (om-to-trimmed-string))
        => (:result "| R | A |"
                    "| G | E |")
        (->> (om-build-table! '("L" "O") 'hline '("V" "E"))
             (om-to-trimmed-string))
        => (:result "| L | O |"
                    "|---+---|"
                    "| V | E |")))))

(def-example-group "Type Predicates"
  "Test node types."

  (defexamples-content om-is-type-p
    nil
    (:buffer "*ziltoid*")
    (->> (om-parse-this-object)
         (om-is-type-p 'bold))
    => t
    (->> (om-parse-this-object)
         (om-is-type-p 'italic))
    => nil)

  (defexamples-content om-is-any-type-p
    nil
    (:buffer "*ziltoid*")
    (->> (om-parse-this-object)
         (om-is-any-type-p '(bold)))
    => t
    (->> (om-parse-this-object)
         (om-is-any-type-p '(bold italic)))
    => t
    (->> (om-parse-this-object)
         (om-is-any-type-p '(italic)))
    => nil)

  (defexamples-content om-is-element-p
    nil
    (:buffer "*ziltoid*")
    (:comment "Parsing this text as an element gives a paragraph")
    (->> (om-parse-this-element)
         (om-is-element-p))
    => t
    (:comment "Parsing the same text as an object gives a bold object")
    (->> (om-parse-this-object)
         (om-is-element-p))
    => nil)

  (defexamples-content om-is-branch-node-p
    nil
    (:buffer "*ziltoid*")
    (:comment "Parsing this as an element gives a paragraph type (an object container).")
    (->> (om-parse-this-element)
         (om-is-branch-node-p))
    => t
    (:comment "Parsing this as an object gives a bold type (also an object container).")
    (->> (om-parse-this-object)
         (om-is-branch-node-p))
    => t
    (:buffer "~ziltoid~")
    (:comment "Parsing this as an object gives a code type (not a container).")
    (->> (om-parse-this-object)
         (om-is-branch-node-p))
    => nil
    (:buffer "# ziltoid")
    (:comment "Parsing this as an element gives a comment type (not a container).")
    (->> (om-parse-this-element)
         (om-is-branch-node-p))
    => nil
    (:buffer "* I'm so great")
    (:comment "Parsing this as an element gives a table (a greater element).")
    (->> (om-parse-this-element)
         (om-is-branch-node-p))
    => t)

  (defexamples-content om-node-may-have-child-objects-p
    nil
    (:buffer "*ziltoid*")
    (:comment "Parsing this as an element gives a paragraph type (an object container).")
    (->> (om-parse-this-element)
         (om-node-may-have-child-objects-p))
    => t
    (:comment "Parsing this as an object gives a bold type (also an object container).")
    (->> (om-parse-this-object)
         (om-node-may-have-child-objects-p))
    => t
    (:buffer "~ziltoid~")
    (:comment "Parsing this as an object gives a code type (not a container).")
    (->> (om-parse-this-object)
         (om-node-may-have-child-objects-p))
    => nil
    (:buffer "# ziltoid")
    (:comment "Parsing this as an element gives a comment type (not a container).")
    (->> (om-parse-this-element)
         (om-node-may-have-child-objects-p))
    => nil
    (:buffer "* I'm so great")
    (:comment "Parsing this as an element gives a table (a greater element).")
    (->> (om-parse-this-element)
         (om-node-may-have-child-objects-p))
    => nil)

  (defexamples-content om-node-may-have-child-elements-p
    nil
    (:buffer "* I'm so great")
    (:comment "Parsing this as an element gives a table (a greater element).")
    (->> (om-parse-this-element)
         (om-node-may-have-child-elements-p))
    => t
    (:buffer "*ziltoid*")
    (:comment "Parsing this as an element gives a paragraph type (not a greater element).")
    (->> (om-parse-this-element)
         (om-node-may-have-child-elements-p))
    => nil
    (:buffer "# ziltoid")
    (:comment "Parsing this as an element gives a comment type (not a container).")
    (->> (om-parse-this-element)
         (om-node-may-have-child-elements-p))
    => nil))

(def-example-group "Property Manipulation"
  "Set, get, and map properties of nodes."

  (def-example-subgroup "Generic"
    nil

    (defexamples-content om-contains-point-p
      nil
      (:buffer "*findme*")
      (->> (om-parse-this-object)
           (om-contains-point-p 2))
      => t
      (->> (om-parse-this-object)
           (om-contains-point-p 10))
      => nil)

    (defexamples-content om-set-property
      "Set property PROP to VALUE in ELEM."

      (:buffer "#+CALL: ktulu()")
      (->> (om-parse-this-element)
           (om-set-property :call "cthulhu")
           (om-set-property :inside-header '(:cache no))
           (om-set-property :arguments '("x=4"))
           (om-set-property :end-header '(:exports results))
           (om-to-trimmed-string))
      => "#+CALL: cthulhu[:cache no](x=4) :exports results"

      :begin-hidden
      (:buffer "CLOCK: [2019-01-01 Tue]")
      (->> (om-parse-this-element)
           (om-set-property
            :value (om-build-timestamp!
                    'inactive '(2019 1 1) :end '(2019 1 2)))
           (om-to-trimmed-string))
      => "CLOCK: [2019-01-01 Tue]--[2019-01-02 Wed] => 24:00"

      (:buffer "~learn to~")
      (->> (om-parse-this-object)
           (om-set-property :value "why?")
           (om-to-trimmed-string))
      => "~why?~"

      (:buffer "# not here")
      (->> (om-parse-this-element)
           (om-set-property :value "still not here")
           (om-to-trimmed-string))
      => "# still not here"

      (:buffer "#+BEGIN_COMMENT"
               "not here"
               "#+END_COMMENT")
      (->> (om-parse-this-element)
           (om-set-property :value "still not here")
           (om-to-trimmed-string))
      => (:result "#+BEGIN_COMMENT"
                  "still not here"
                  "#+END_COMMENT")

      ;; TODO add diary-sexp

      (:buffer ":LOGBOOK:"
               ":END:")
      (->> (om-parse-this-element)
           (om-set-property :drawer-name "BOOKOFSOULS")
           (om-to-trimmed-string))
      => (:result ":BOOKOFSOULS:"
                  ":END:")

      (:buffer "#+BEGIN: blockhead"
               "#+END:")
      (->> (om-parse-this-element)
           (om-set-property :block-name "blockfoot")
           (om-set-property :arguments '(:cache no))
           (om-to-trimmed-string))
      => (:result "#+BEGIN: blockfoot :cache no"
                  "#+END:")

      (:buffer "\\pi")
      (->> (om-parse-this-object)
           (om-set-property :name "gamma")
           (om-set-property :use-brackets-p t)
           (om-to-trimmed-string))
      => "\\gamma{}"

      ;; TODO test preserve indentation...
      (:buffer "#+BEGIN_EXAMPLE"
               "#+END_EXAMPLE")
      (->> (om-parse-this-element)
           (om-set-property :switches '("-n"))
           (om-set-property :value "example.com")
           (om-to-trimmed-string))
      => (:buffer "#+BEGIN_EXAMPLE -n"
                  "example.com"
                  "#+END_EXAMPLE")

      (:buffer "#+BEGIN_EXPORT latex"
               "#+END_EXPORT")
      (->> (om-parse-this-element)
           (om-set-property :type "domestic")
           (om-set-property :value "bullets, bombs, and bigotry")
           (om-to-trimmed-string))
      => (:buffer "#+BEGIN_EXPORT domestic"
                  "bullets, bombs, and bigotry"
                  "#+END_EXPORT")

      (:buffer "@@back-end:value@@")
      (->> (om-parse-this-object)
           (om-set-property :back-end "latex")
           (om-set-property :value "new-value")
           (om-to-trimmed-string))
      => "@@latex:new-value@@"

      (:buffer ": fixed")
      (->> (om-parse-this-element)
           (om-set-property :value "unfixed")
           (om-to-trimmed-string))
      => ": unfixed"

      (:buffer "[fn:whitelabel] society")
      (->> (om-parse-this-element)
           (om-set-property :label "blacklabel")
           (om-to-trimmed-string))
      => "[fn:blacklabel] society"

      ;; TODO test footnote section
      (:buffer "* dummy"
               "stuff")
      (->> (om-parse-this-element)
           (om-set-property :archivedp t)
           (om-set-property :commentedp t)
           (om-set-property :level 2)
           (om-set-property :pre-blank 1)
           (om-set-property :priority ?A)
           (om-set-property :tags '("tmsu"))
           (om-set-property :title '("smartie"))
           (om-set-property :todo-keyword "TODO")
           (om-to-trimmed-string))
      => (:result "** TODO COMMENT [#A] smartie                                   :tmsu:ARCHIVE:"
                  ""
                  "stuff")

      (:buffer "call_kthulu()")
      (->> (om-parse-this-object)
           (om-set-property :call "cthulhu")
           (om-set-property :inside-header '(:cache no))
           (om-set-property :arguments '("x=4"))
           (om-set-property :end-header '(:exports results))
           (om-to-trimmed-string))
      => "call_cthulhu[:cache no](x=4)[:exports results]"

      (:buffer "src_emacs{(print 'yeah-boi)}")
      (->> (om-parse-this-object)
           (om-set-property :language "python")
           (om-set-property :parameters '(:cache no))
           (om-set-property :value "print \"yeah boi\"")
           (om-to-trimmed-string))
      => "src_python[:cache no]{print \"yeah boi\"}"
      :end-hidden

      (:buffer "- thing")
      (->> (om-parse-this-item)
           (om-set-property :bullet 1)
           (om-set-property :checkbox 'on)
           (om-set-property :counter 2)
           (om-set-property :tag '("tmsu"))
           (om-to-trimmed-string))
      => "1. [@2] [X] tmsu :: thing"

      :begin-hidden
      (:buffer "#+KEY: VAL")
      (->> (om-parse-this-element)
           (om-set-property :key "kee")
           (om-set-property :value "vahl")
           (om-to-trimmed-string))
      => "#+kee: vahl"

      ;; this is stupid, who would ever do this?
      (:buffer "\begin{env}"
               "body"
               "\end{env}")
      (->> (om-parse-this-element)
           (om-set-property :value "\begin{vne}\nbody\end{vne}")
           (om-to-trimmed-string))
      => (:buffer "\begin{vne}"
                  "body"
                  "\end{vne}")

      ;; TODO this is also stupid...
      (:buffer "$2+2=4$")
      (->> (om-parse-this-object)
           (om-set-property :value "$2+2=5$")
           (om-to-trimmed-string))
      => "$2+2=5$"

      (:buffer "https://example.com")
      (->> (om-parse-this-object)
           (om-set-property :path "/dev/null")
           (om-set-property :type "file")
           (om-set-property :format 'bracket)
           (om-to-trimmed-string))
      => "[[file:/dev/null]]"

      (:buffer "{{{economics}}}")
      (->> (om-parse-this-object)
           (om-set-property :key "freakonomics")
           (om-set-property :args '("x=4" "y=2"))
           (om-to-trimmed-string))
      => "{{{freakonomics(x=4,y=2)}}}"

      (:buffer "* dummy"
               ":PROPERTIES:"
               ":KEY: VAL"
               ":END:")
      ;; TODO need public function
      (->> (om-parse-this-headline)
           (om--headline-get-node-properties)
           (-first-item)
           (om-set-property :key "kee")
           (om-set-property :value "vahl")
           (om-to-trimmed-string))
      => ":kee:      vahl"

      (:buffer "* dummy"
               "CLOSED: [2019-01-01 Tue]")
      ;; TODO need public function
      (->> (om-parse-this-headline)
           (om--headline-get-planning)
           (om-set-property
            :closed (om-build-timestamp! 'inactive '(2019 1 2)))
           (om-to-trimmed-string))
      => "CLOSED: [2019-01-02 Wed]"

      (:buffer "#+BEGIN_special"
               "#+END_special")
      (->> (om-parse-this-element)
           (om-set-property :type "talent")
           (om-to-trimmed-string))
      => (:result "#+BEGIN_talent"
                  "#+END_talent")

      (:buffer "#+BEGIN_SRC"
               "something amorphous"
               "#+END_SRC")
      (->> (om-parse-this-element)
           (om-set-property :language "emacs")
           (om-set-property :value "(print 'hi)")
           (om-set-property :parameters '(:cache no))
           (om-set-property :switches '("-n"))
           ;; TODO test preserver indent
           (om-to-trimmed-string))
      => (:result "#+BEGIN_SRC emacs -n :cache no"
                  "  (print 'hi)"
                  "#+END_SRC")

      (:buffer "* dummy [50%]")
      (->> (om-parse-this-headline)
           (om--headline-get-statistics-cookie)
           (om-set-property :value '(0 5))
           (om-to-trimmed-string))
      => "[0/5]"

      (:buffer "sub_woofer")
      (->> (om-parse-object-at 5)
           (om-set-property :use-brackets-p t)
           (om-to-trimmed-string))
      => "_{woofer}"

      (:buffer "super^woofer")
      (->> (om-parse-object-at 7)
           (om-set-property :use-brackets-p t)
           (om-to-trimmed-string))
      => "^{woofer}"

      (:buffer "| a |")
      (->> (om-parse-this-element)
           (om-set-property :tblfm '("x=$2"))
           (om-to-trimmed-string))
      => (:result "| a |"
                  "#+TBLFM: x=$2")

      (:buffer "<<found>>")
      (->> (om-parse-this-object)
           (om-set-property :value "lost")
           (om-to-trimmed-string))
      => "<<lost>>"

      (:buffer "[2019-01-01 Tue]")
      (->> (om-parse-this-object)
           (om-set-property :year-start 2020)
           (om-set-property :month-start 2)
           (om-set-property :day-start 2)
           (om-set-property :hour-start 12)
           (om-set-property :minute-start 0)
           (om-set-property :year-end 2020)
           (om-set-property :month-end 2)
           (om-set-property :day-end 3)
           (om-set-property :hour-end 12)
           (om-set-property :minute-end 0)
           (om-set-property :type 'active-range)
           (om-set-property :warning-type 'all)
           (om-set-property :warning-unit 'day)
           (om-set-property :warning-value 1)
           (om-set-property :repeater-type 'cumulate)
           (om-set-property :repeater-unit 'day)
           (om-set-property :repeater-value 1)
           (om-to-trimmed-string))
      => "<2020-02-02 Sun 12:00 +1d -1d>--<2020-02-03 Mon 12:00 +1d -1d>"

      (:buffer "=I am not a crook=")
      (->> (om-parse-this-object)
           (om-set-property :value "You totally are")
           (om-to-trimmed-string))
      => "=You totally are="
      :end-hidden

      ;; TODO add post-blank

      (:buffer "* not valuable")
      (:comment "Throw error when setting a property that doesn't exist")
      (->> (om-parse-this-headline)
           (om-set-property :value "wtf")
           (om-to-trimmed-string))
      !!> error)

    (defexamples-content om-set-properties
      nil
      
      (:buffer "- thing")
      (->> (om-parse-this-item)
           (om-set-properties (list :bullet 1
                                         :checkbox 'on
                                         :counter 2
                                         :tag '("tmsu")))
           (om-to-trimmed-string))
      => "1. [@2] [X] tmsu :: thing")

    (defexamples-content om-get-property
      nil

      (:buffer "#+CALL: ktulu[:cache no](x=4) :exports results")
      (->> (om-parse-this-element)
           (om-get-property :call))
      => "ktulu"
      (->> (om-parse-this-element)
           (om-get-property :inside-header))
      => '(:cache no)
      (->> (om-parse-this-element)
           (om-get-property :arguments))
      => '("x=4")
      (->> (om-parse-this-element)
           (om-get-property :end-header))
      => '(:exports results)

      :begin-hidden
      (:buffer "CLOCK: [2019-01-01 Tue]")
      (->> (om-parse-this-element)
           (om-get-property :value)
           (om-to-string))
      => "[2019-01-01 Tue]"

      (:buffer "~learn to~")
      (->> (om-parse-this-object)
           (om-get-property :value))
      => "learn to"

      (:buffer "# not here")
      (->> (om-parse-this-element)
           (om-get-property :value))
      => "not here"

      (:buffer "#+BEGIN_COMMENT"
               "not here"
               "#+END_COMMENT")
      (->> (om-parse-this-element)
           (om-get-property :value))
      => "not here"

      ;; TODO add diary-sexp

      (:buffer ":LOGBOOK:"
               ":END:")
      (->> (om-parse-this-element)
           (om-get-property :drawer-name))
      => "LOGBOOK"

      (:buffer "#+BEGIN: blockhead :cache no"
               "#+END:")
      (->> (om-parse-this-element)
           (om-get-property :block-name))
      => "blockhead"
      (->> (om-parse-this-element)
           (om-get-property :arguments))
      => '(:cache no)

      (:buffer "\\pi{}")
      (->> (om-parse-this-object)
           (om-get-property :name))
      => "pi"
      (->> (om-parse-this-object)
           (om-get-property :use-brackets-p))
      => t

      ;; TODO test preserve indentation...
      => (:buffer "#+BEGIN_EXAMPLE -n"
                  "example.com"
                  "#+END_EXAMPLE")
      (->> (om-parse-this-element)
           (om-get-property :switches))
      => '("-n")
      (->> (om-parse-this-element)
           (om-get-property :value))
      => "example.com"

      (:buffer "#+BEGIN_EXPORT domestic"
               "bullets, bombs, and bigotry"
               "#+END_EXPORT")
      (->> (om-parse-this-element)
           (om-get-property :type))
      ;; TODO why capitalized?
      => "DOMESTIC"
      (->> (om-parse-this-element)
           (om-get-property :value))
      => "bullets, bombs, and bigotry\n"

      (:buffer "@@back-end:value@@")
      (->> (om-parse-this-object)
           (om-get-property :back-end))
      => "back-end"
      (->> (om-parse-this-object)
           (om-get-property :value))
      => "value"

      (:buffer ": fixed")
      (->> (om-parse-this-element)
           (om-get-property :value))
      => "fixed"

      (:buffer "[fn:blacklabel] society")
      (->> (om-parse-this-element)
           (om-get-property :label))
      => "blacklabel"

      ;; TODO test footnote section
      ;; TODO the priority should be parsable after "COMMENT"
      (:buffer "** TODO [#A] COMMENT dummy                                   :tmsu:ARCHIVE:"
               ""
               "stuff")
      (->> (om-parse-this-element)
           (om-get-property :archivedp))
      => t
      (->> (om-parse-this-element)
           (om-get-property :commentedp))
      => t
      (->> (om-parse-this-element)
           (om-get-property :level))
      => 2
      (->> (om-parse-this-element)
           (om-get-property :pre-blank))
      => 1
      (->> (om-parse-this-element)
           (om-get-property :priority))
      => ?A
      (->> (om-parse-this-element)
           (om-get-property :tags))
      => '("tmsu")
      (->> (om-parse-this-element)
           (om-get-property :title))
      => '("dummy")
      (->> (om-parse-this-element)
           (om-get-property :todo-keyword))
      => "TODO"

      (:buffer "call_ktulu[:cache no](x=4)[:exports results]")
      (->> (om-parse-this-object)
           (om-get-property :call))
      => "ktulu"
      (->> (om-parse-this-object)
           (om-get-property :inside-header))
      =>  '(:cache no)
      (->> (om-parse-this-object)
           (om-get-property :arguments))
      => '("x=4")
      (->> (om-parse-this-object)
           (om-get-property :end-header))
      => '(:exports results)

      (:buffer "src_python[:cache no]{print \"yeah boi\"}")
      (->> (om-parse-this-object)
           (om-get-property :language))
      => "python"
      (->> (om-parse-this-object)
           (om-get-property :parameters))
      => '(:cache no)
      (->> (om-parse-this-object)
           (om-get-property :value))
      => "print \"yeah boi\""

      (:buffer "- [@2] [X] tmsu :: thing")
      (->> (om-parse-this-item)
           (om-get-property :bullet))
      => '-
      (->> (om-parse-this-item)
           (om-get-property :checkbox))
      => 'on
      (->> (om-parse-this-item)
           (om-get-property :counter))
      => 2
      (->> (om-parse-this-item)
           (om-get-property :tag))
      => '("tmsu")

      (:buffer "#+KEY: VAL")
      (->> (om-parse-this-element)
           (om-get-property :key))
      => "KEY"
      (->> (om-parse-this-element)
           (om-get-property :value))
      => "VAL"

      ;; this is stupid, who would ever do this?
      (:buffer "\begin{env}"
               "body"
               "\end{env}")
      (->> (om-parse-this-element)
           (om-get-property :value))
      => (:buffer "\begin{env}"
                  "body"
                  "\end{env}")

      ;; TODO this is also stupid...
      (:buffer "$2+2=4$")
      (->> (om-parse-this-object)
           (om-get-property :value))
      => "$2+2=4$"
      :end-hidden

      (:buffer "[[file:/dev/null]]")
      (->> (om-parse-this-object)
           (om-get-property :path))
      => "/dev/null"
      (->> (om-parse-this-object)
           (om-get-property :type))
      => "file"
      (->> (om-parse-this-object)
           (om-get-property :format))
      => 'bracket
      
      :begin-hidden
      (:buffer "{{{economics(x=4,y=2)}}}")
      (->> (om-parse-this-object)
           (om-get-property :key))
      => "economics"
      (->> (om-parse-this-object)
           (om-get-property :args))
      => '("x=4" "y=2")

      (:buffer "* dummy"
               ":PROPERTIES:"
               ":KEY: VAL"
               ":END:")
      ;; TODO need public function
      (->> (om-parse-this-headline)
           (om--headline-get-node-properties)
           (-first-item)
           (om-get-property :key))
      => "KEY"
      (->> (om-parse-this-headline)
           (om--headline-get-node-properties)
           (-first-item)
           (om-get-property :value))
      => "VAL"

      (:buffer "* dummy"
               "CLOSED: [2019-01-01 Tue]")
      ;; TODO need public function
      (->> (om-parse-this-headline)
           (om--headline-get-planning)
           (om-get-property :closed)
           (om-to-string))
      => "[2019-01-01 Tue]"

      (:buffer "#+BEGIN_special"
               "#+END_special")
      (->> (om-parse-this-element)
           (om-get-property :type))
      => "special"

      (:buffer "#+BEGIN_SRC emacs -n :cache no"
               "  (print 'hi)"
               "#+END_SRC")
      (->> (om-parse-this-element)
           (om-get-property :language))
      => "emacs"
      (->> (om-parse-this-element)
           (om-get-property :value))
      ;; TODO why indented?
      => "  (print 'hi)"
      (->> (om-parse-this-element)
           (om-get-property :parameters))
      => '(:cache no)
      (->> (om-parse-this-element)
           (om-get-property :switches))
      => '("-n")

      (:buffer "* dummy [50%]")
      (->> (om-parse-this-headline)
           (om--headline-get-statistics-cookie)
           (om-get-property :value))
      => '(50)

      (:buffer "sub_{woofer}")
      (->> (om-parse-object-at 6)
           (om-get-property :use-brackets-p))
      => t

      (:buffer "super_{woofer}")
      (->> (om-parse-object-at 8)
           (om-get-property :use-brackets-p))
      => t

      (:buffer "| a |"
               "#+TBLFM: x=$2")
      (->> (om-parse-this-element)
           (om-get-property :tblfm))
      => '("x=$2")

      (:buffer "<<found>>")
      (->> (om-parse-this-object)
           (om-get-property :value))
      => "found"

      (:buffer "<2020-02-02 Sun 12:00 +1d -1d>--<2020-02-03 Mon 12:00 +1d -1d>")
      (->> (om-parse-this-object)
           (om-get-property :year-start))
      => 2020
      (->> (om-parse-this-object)
           (om-get-property :month-start))
      => 2
      (->> (om-parse-this-object)
           (om-get-property :day-start))
      => 2
      (->> (om-parse-this-object)
           (om-get-property :hour-start))
      => 12
      (->> (om-parse-this-object)
           (om-get-property :minute-start))
      => 0
      (->> (om-parse-this-object)
           (om-get-property :year-end))
      => 2020
      (->> (om-parse-this-object)
           (om-get-property :month-end))
      => 2
      (->> (om-parse-this-object)
           (om-get-property :day-end))
      => 3
      (->> (om-parse-this-object)
           (om-get-property :hour-end))
      => 12
      (->> (om-parse-this-object)
           (om-get-property :minute-end))
      => 0
      (->> (om-parse-this-object)
           (om-get-property :type))
      => 'active-range
      (->> (om-parse-this-object)
           (om-get-property :warning-type))
      => 'all
      (->> (om-parse-this-object)
           (om-get-property :warning-unit))
      => 'day
      (->> (om-parse-this-object)
           (om-get-property :warning-value))
      => 1
      (->> (om-parse-this-object)
           (om-get-property :repeater-type))
      => 'cumulate
      (->> (om-parse-this-object)
           (om-get-property :repeater-unit))
      => 'day
      (->> (om-parse-this-object)
           (om-get-property :repeater-value))
      => 1

      (:buffer "=I am not a crook=")
      (->> (om-parse-this-object)
           (om-get-property :value))
      => "I am not a crook"
      :end-hidden

      ;; TODO add post-blank

      (:buffer "* not arguable")
      (:comment "Throw error when requesting a property that doesn't exist")
      (->> (om-parse-this-headline)
           (om-get-property :value))
      !!> error)

    (defexamples-content om-map-property
      nil

      (:buffer "#+CALL: ktulu()")
      (->> (om-parse-this-element)
           (om-map-property :call #'s-upcase)
           (om-to-trimmed-string))
      => "#+CALL: KTULU()"

      :begin-hidden

      ;; TODO add clock

      (:buffer "~learn to~")
      (->> (om-parse-this-object)
           (om-map-property :value #'s-upcase)
           (om-to-trimmed-string))
      => "~LEARN TO~"

      (:buffer "# not here")
      (->> (om-parse-this-element)
           (om-map-property :value #'s-upcase)
           (om-to-trimmed-string))
      => "# NOT HERE"

      (:buffer "#+BEGIN_COMMENT"
               "not here"
               "#+END_COMMENT")
      (->> (om-parse-this-element)
           (om-map-property :value #'s-upcase)
           (om-to-trimmed-string))
      => (:result "#+BEGIN_COMMENT"
                  "NOT HERE"
                  "#+END_COMMENT")

      ;; TODO add diary-sexp

      (:buffer ":LOGBOOK:"
               ":END:")
      (->> (om-parse-this-element)
           (om-map-property :drawer-name #'s-capitalize)
           (om-to-trimmed-string))
      => (:result ":Logbook:"
                  ":END:")

      (:buffer "#+BEGIN: blockhead"
               "#+END:")
      (->> (om-parse-this-element)
           (om-map-property :block-name #'s-upcase)
           (om-to-trimmed-string))
      => (:result "#+BEGIN: BLOCKHEAD"
                  "#+END:")

      :end-hidden

      ;; TODO add entity

      (:buffer "#+BEGIN_EXAMPLE"
               "example.com"
               "#+END_EXAMPLE")
      (->> (om-parse-this-element)
           (om-map-property* :value (concat "https://" it))
           (om-to-trimmed-string))
      => (:result "#+BEGIN_EXAMPLE"
                  "https://example.com"
                  "#+END_EXAMPLE")

      :begin-hidden

      (:buffer "#+BEGIN_EXPORT domestic"
               "bullets, bombs, and bigotry"
               "#+END_EXPORT")
      (->> (om-parse-this-element)
           (om-map-property :type #'s-upcase)
           (om-map-property :value #'s-upcase)
           (om-to-trimmed-string))
      => (:result "#+BEGIN_EXPORT DOMESTIC"
                  "BULLETS, BOMBS, AND BIGOTRY"
                  "#+END_EXPORT")

      (:buffer "@@back-end:value@@")
      (->> (om-parse-this-object)
           (om-map-property :back-end #'s-upcase)
           (om-map-property :value #'s-upcase)
           (om-to-trimmed-string))
      => "@@BACK-END:VALUE@@"

      (:buffer ": fixed")
      (->> (om-parse-this-element)
           (om-map-property :value #'s-upcase)
           (om-to-trimmed-string))
      => ": FIXED"

      (:buffer "[fn:blacklabel] society")
      (->> (om-parse-this-element)
           (om-map-property :label #'s-upcase)
           (om-to-trimmed-string))
      => "[fn:BLACKLABEL] society"

      ;; TODO add example for headline

      (:buffer "call_ktulu()")
      (->> (om-parse-this-object)
           (om-map-property :call #'s-upcase)
           (om-to-trimmed-string))
      => "call_KTULU()"

      ;; TODO add example for inline src block

      (:buffer "- tag :: thing")
      (->> (om-parse-this-item)
           (om-map-property :tag (lambda (it) (-map #'s-upcase it)))
           (om-to-trimmed-string))
      => "- TAG :: thing"

      (:buffer "#+KEY: VAL")
      (->> (om-parse-this-element)
           (om-map-property :key (-partial #'s-prepend "OM_"))
           (om-map-property :value (-partial #'s-prepend "OM_"))
           (om-to-trimmed-string))
      => "#+OM_KEY: OM_VAL"

      ;; TODO add examples for latex frag/env

      ;; TODO add example for link

      (:buffer "{{{economics}}}")
      (->> (om-parse-this-object)
           (om-map-property :key #'s-upcase)
           (om-to-trimmed-string))
      => "{{{ECONOMICS}}}"

      (:buffer "* dummy"
               ":PROPERTIES:"
               ":KEY: VAL"
               ":END:")
      ;; TODO need public function
      (->> (om-parse-this-headline)
           (om--headline-get-node-properties)
           (-first-item)
           (om-map-property :key (-partial #'s-prepend "OM_"))
           (om-map-property :value (-partial #'s-prepend "OM_"))
           (om-to-trimmed-string))
      => ":OM_KEY:   OM_VAL"

      ;; TODO add example for planning

      (:buffer "#+BEGIN_special"
               "#+END_special")
      (->> (om-parse-this-element)
           (om-map-property :type #'s-upcase)
           (om-to-trimmed-string))
      => (:result "#+BEGIN_SPECIAL"
                  "#+END_SPECIAL")

      ;; TODO add example for src block

      ;; TODO add example for statistics cookie

      (:buffer "<<found>>")
      (->> (om-parse-this-object)
           (om-map-property :value #'s-upcase)
           (om-to-trimmed-string))
      => "<<FOUND>>"

      (:buffer "=I am not a crook=")
      (->> (om-parse-this-object)
           (om-map-property :value #'s-upcase)
           (om-to-trimmed-string))
      => "=I AM NOT A CROOK="
      :end-hidden

      (:buffer "~code~")
      (:comment "Throw error if property doesn't exist")
      (->> (om-parse-this-object)
           (om-map-property :title #'s-upcase)
           (om-to-trimmed-string))
      !!> error
      (:comment "Throw error if function doesn't return proper type")
      (->> (om-parse-this-object)
           (om-map-property* :value (if it 1 0))
           (om-to-trimmed-string))
      !!> error)

    (defexamples-content om-map-properties
      nil

      (:buffer "#+KEY: VAL")
      (->> (om-parse-this-element)
           (om-map-properties
            (list :key (-partial #'s-prepend "OM_")
                  :value (-partial #'s-prepend "OM_")))
           (om-to-trimmed-string))
      => "#+OM_KEY: OM_VAL"

      ;; TODO spice this up...
      ;; TODO this makes the document parser puke for some reason
      ;; (->> (om-parse-this-element)
      ;;      (om-map-properties*
      ;;       (:key (s-prepend "OM_" it) :value (s-prepend "OM_" it)))
      ;;      (om-to-trimmed-string))
      ;; => "#+OM_KEY: OM_VAL"
      )

    (defexamples-content om-toggle-property
      nil

      (:buffer "\\pi")
      (->> (om-parse-this-object)
           (om-toggle-property :use-brackets-p)
           (om-to-trimmed-string))
      => "\\pi{}"

      ;; TODO test src/example block preserve indent
      
      (:buffer "* headline")
      (->> (om-parse-this-headline)
           (om-toggle-property :archivedp)
           (om-to-trimmed-string))
      => "* headline                                                          :ARCHIVE:"
      (->> (om-parse-this-headline)
           (om-toggle-property :commentedp)
           (om-to-trimmed-string))
      => "* COMMENT headline"
      (->> (om-parse-this-headline)
           (om-toggle-property :footnote-section-p)
           (om-to-trimmed-string))
      => "* Footnotes"

      :begin-hidden

      (:buffer "sub_woofer")
      (->> (om-parse-object-at 5)
           (om-toggle-property :use-brackets-p)
           (om-to-trimmed-string))
      => "_{woofer}"

      (:buffer "super^woofer")
      (->> (om-parse-object-at 7)
           (om-toggle-property :use-brackets-p)
           (om-to-trimmed-string))
      => "^{woofer}"

      :end-hidden

      (:buffer "- [ ] nope")
      (:comment "Throw an error when trying to toggle a non-boolean property")
      (->> (om-parse-this-item)
           (om-toggle-property :checkbox)
           (om-to-trimmed-string))
      !!> error)

    (defexamples-content om-shift-property
      ;; TODO need to ensure that the min/max priorities are always the same
      nil

      (:buffer "* no priorities")
      (:comment "Do nothing if there is nothing to shift.")
      (->> (om-parse-this-headline)
           (om-shift-property :priority 1)
           (om-to-trimmed-string))
      => "* no priorities"

      (:buffer "* [#A] priorities")
      (->> (om-parse-this-headline)
           (om-shift-property :priority -1)
           (om-to-trimmed-string))
      => "* [#B] priorities"
      (->> (om-parse-this-headline)
           (om-shift-property :priority -2)
           (om-to-trimmed-string))
      => "* [#C] priorities"
      (:comment "Wrap priority around when crossing the min or max")
      (->> (om-parse-this-headline)
           (om-shift-property :priority 1)
           (om-to-trimmed-string))
      => "* [#C] priorities"

      (:buffer "* TODO or not todo")
      (:comment "Throw error when shifting an unshiftable property")
      (->> (om-parse-this-headline)
           (om-shift-property :todo-keyword 1)
           (om-to-string))
      !!> error

      :begin-hidden

      (:buffer "*bold*")
      (->> (om-parse-this-object)
           (om-shift-property :post-blank 1)
           (om-to-string))
      => "*bold* "
      (->> (om-parse-this-object)
           (om-shift-property :post-blank -1)
           (om-to-string))
      => "*bold*"

      (:buffer "1. thing")
      (->> (om-parse-this-item)
           (om-shift-property :counter 1)
           (om-to-trimmed-string))
      => "1. thing"

      (:buffer "1. [@1] thing")
      (->> (om-parse-this-item)
           (om-shift-property :counter 1)
           (om-to-trimmed-string))
      => "1. [@2] thing"
      (->> (om-parse-this-item)
           (om-shift-property :counter -1)
           (om-to-trimmed-string))
      => "1. [@1] thing"

      (:buffer "* noob level")
      (->> (om-parse-this-headline)
           (om-shift-property :level 1)
           (om-to-trimmed-string))
      => "** noob level"

      (:comment "Do nothing when final value is less than one.")
      (->> (om-parse-this-headline)
           (om-shift-property :level -1)
           (om-to-trimmed-string))
      => "* noob level"

      (:buffer "* headline"
               "stuff")
      (->> (om-parse-this-headline)
           (om-shift-property :pre-blank 1)
           (om-to-trimmed-string))
      => (:result "* headline"
                  ""
                  "stuff")
      (->> (om-parse-this-headline)
           (om-shift-property :pre-blank -1)
           (om-to-trimmed-string))
      => (:result "* headline"
                  "stuff")
      :end-hidden)

    (defexamples-content om-insert-into-property
      nil

      (:buffer "#+CALL: ktulu(y=1)")
      (->> (om-parse-this-element)
           (om-insert-into-property :arguments 0 "x=4")
           (om-to-trimmed-string))
      => "#+CALL: ktulu(x=4,y=1)"

      (:comment "Do nothing if the string is already in the list")
      (->> (om-parse-this-element)
           (om-insert-into-property :arguments 0 "y=1")
           (om-to-trimmed-string))
      => "#+CALL: ktulu(y=1)"

      (:comment "Throw error when inserting into a property that is not a list of strings")
      (->> (om-parse-this-element)
           (om-insert-into-property :end-header 0 "html")
           (om-to-trimmed-string))
      !!> error

      (:buffer "* headline       :tag1:")
      (->> (om-parse-this-headline)
           (om-insert-into-property :tags 0 "tag0")
           (om-to-trimmed-string))
      => "* headline                                                        :tag0:tag1:"

      :begin-hidden

      (:buffer "#+BEGIN_EXAMPLE -n"
               "#+END_EXAMPLE")
      (->> (om-parse-this-element)
           (om-insert-into-property :switches -1 "-r")
           (om-to-trimmed-string))
      => (:result "#+BEGIN_EXAMPLE -n -r"
                  "#+END_EXAMPLE")


      (:buffer "call_ktulu(y=1)")
      (->> (om-parse-this-object)
           (om-insert-into-property :arguments 0 "x=4")
           (om-to-trimmed-string))
      => "call_ktulu(x=4,y=1)"

      (:buffer "{{{economics(x=4)}}}")
      (->> (om-parse-this-object)
           (om-insert-into-property :args 0 "z=2")
           (om-to-trimmed-string))
      => "{{{economics(z=2,x=4)}}}"
      
      (:buffer "#+BEGIN_SRC emacs-lisp -n"
               "#+END_SRC")
      (->> (om-parse-this-element)
           (om-insert-into-property :switches -1 "-r")
           (om-to-trimmed-string))
      => (:result "#+BEGIN_SRC emacs-lisp -n -r"
                  "#+END_SRC")

      (:buffer "| a |"
               "#+TBLFM: x=$2")
      (->> (om-parse-this-element)
           (om-insert-into-property :tblfm -1 "y=$3")
           (om-to-trimmed-string))
      => (:result "| a |"
                  "#+TBLFM: y=$3"
                  "#+TBLFM: x=$2")
      :end-hidden)

    (defexamples-content om-remove-from-property
      nil

      (:buffer "#+CALL: ktulu(y=1)")
      (->> (om-parse-this-element)
           (om-remove-from-property :arguments "y=1")
           (om-to-trimmed-string))
      => "#+CALL: ktulu()"

      (:comment "Do nothing if the string does not exist")
      (->> (om-parse-this-element)
           (om-remove-from-property :arguments "d=666")
           (om-to-trimmed-string))
      => "#+CALL: ktulu(y=1)"

      (:comment "Throw error when removing from property that is not a string list")
      (->> (om-parse-this-element)
           (om-remove-from-property :end-header ":results")
           (om-to-trimmed-string))
      !!> error

      (:buffer "* headline       :tag1:")
      (->> (om-parse-this-headline)
           (om-remove-from-property :tags "tag1")
           (om-to-trimmed-string))
      => "* headline"

      :begin-hidden

      (:buffer "#+BEGIN_EXAMPLE -n"
               "#+END_EXAMPLE")
      (->> (om-parse-this-element)
           (om-remove-from-property :switches "-n")
           (om-to-trimmed-string))
      => (:result "#+BEGIN_EXAMPLE"
                  "#+END_EXAMPLE")

      (:buffer "call_ktulu(y=1)")
      (->> (om-parse-this-object)
           (om-remove-from-property :arguments "y=1")
           (om-to-trimmed-string))
      => "call_ktulu()"

      (:buffer "{{{economics(x=4)}}}")
      (->> (om-parse-this-object)
           (om-remove-from-property :args "x=4")
           (om-to-trimmed-string))
      => "{{{economics}}}"
      
      (:buffer "#+BEGIN_SRC emacs-lisp -n"
               "#+END_SRC")
      (->> (om-parse-this-element)
           (om-remove-from-property :switches "-n")
           (om-to-trimmed-string))
      => (:result "#+BEGIN_SRC emacs-lisp"
                  "#+END_SRC")

      (:buffer "| a |"
               "#+TBLFM: x=$2")
      (->> (om-parse-this-element)
           (om-remove-from-property :tblfm "x=$2")
           (om-to-trimmed-string))
      => "| a |"
      :end-header)

    (defexamples-content om-plist-put-property
      nil

      (:buffer "#+CALL: ktulu[:cache no]()")
      (->> (om-parse-this-element)
           (om-plist-put-property :end-header :results 'html)
           (om-to-trimmed-string))
      => "#+CALL: ktulu[:cache no]() :results html"
      (:comment "Change the value of key if it already is present")
      (->> (om-parse-this-element)
           (om-plist-put-property :inside-header :cache 'yes)
           (om-to-trimmed-string))
      => "#+CALL: ktulu[:cache yes]()"
      (:comment "Do nothing if the key and value already exist")
      (->> (om-parse-this-element)
           (om-plist-put-property :inside-header :cache 'no)
           (om-to-trimmed-string))
      => "#+CALL: ktulu[:cache no]()"
      (:comment "Throw error if setting property that isn't a plist")
      (->> (om-parse-this-element)
           (om-plist-put-property :arguments :cache 'no)
           (om-to-trimmed-string))
      !!> error

      :begin-hidden

      (:buffer "#+BEGIN: blockhead :format \"[%s]\""
               "#+END:")
      (->> (om-parse-this-element)
           (om-plist-put-property :arguments :format "<%s>")
           (om-to-trimmed-string))
      => (:result "#+BEGIN: blockhead :format \"<%s>\""
                  "#+END:")

      (:buffer "call_ktulu[:cache no]()")
      (->> (om-parse-this-object)
           (om-plist-put-property :inside-header :cache 'yes)
           (om-plist-put-property :end-header :results 'html)
           (om-to-trimmed-string))
      => "call_ktulu[:cache yes]()[:results html]"

      (:buffer "src_emacs-lisp[:exports results]{}")
      (->> (om-parse-this-object)
           (om-plist-put-property :parameters :exports 'both)
           (om-to-trimmed-string))
      => "src_emacs-lisp[:exports both]{}"

      (:buffer "#+BEGIN_SRC emacs-lisp -n :exports results"
               "#+END_SRC")
      (->> (om-parse-this-element)
           (om-plist-put-property :parameters :exports 'both)
           (om-to-trimmed-string))
      => (:result "#+BEGIN_SRC emacs-lisp -n :exports both"
                  "#+END_SRC")
      :end-hidden)

    (defexamples-content om-plist-remove-property
      nil

      (:buffer "#+CALL: ktulu() :results html")
      (->> (om-parse-this-element)
           (om-plist-remove-property :end-header :results)
           (om-to-trimmed-string))
      => "#+CALL: ktulu()"
      (:comment "Do nothing if the key is not present")
      (->> (om-parse-this-element)
           (om-plist-remove-property :inside-header :cache)
           (om-to-trimmed-string))
      => "#+CALL: ktulu() :results html"
      (:comment "Throw error if trying to remove key from non-plist property")
      (->> (om-parse-this-element)
           (om-plist-remove-property :arguments :cache)
           (om-to-trimmed-string))
      !!> error

      :begin-hidden

      (:buffer "#+BEGIN: blockhead :format \"[%s]\""
               "#+END:")
      (->> (om-parse-this-element)
           (om-plist-remove-property :arguments :format)
           (om-to-trimmed-string))
      => (:result "#+BEGIN: blockhead"
                  "#+END:")

      (:buffer "call_ktulu[:cache no]()[:results html]")
      (->> (om-parse-this-object)
           (om-plist-remove-property :inside-header :cache)
           (om-plist-remove-property :end-header :results)
           (om-to-trimmed-string))
      => "call_ktulu()"

      (:buffer "src_emacs-lisp[:exports results]{}")
      (->> (om-parse-this-object)
           (om-plist-remove-property :parameters :exports)
           (om-to-trimmed-string))
      => "src_emacs-lisp{}"

      (:buffer "#+BEGIN_SRC emacs-lisp -n :exports results"
               "#+END_SRC")
      (->> (om-parse-this-element)
           (om-plist-remove-property :parameters :exports)
           (om-to-trimmed-string))
      => (:result "#+BEGIN_SRC emacs-lisp -n"
                  "#+END_SRC")
      :end-hidden)

    ;; (defexamples-content om-property-is-nil-p
    ;;   nil
    ;;   (:buffer "* TODO dummy")
    ;;   (->> (om-parse-this-headline)
    ;;        (om-property-is-nil-p :todo-keyword))
    ;;   => nil
    ;;   (->> (om-parse-this-headline)
    ;;        (om-property-is-nil-p :commentedp))
    ;;   => t)

    ;; (defexamples-content om-property-is-non-nil-p
    ;;   nil
    ;;   (:buffer "* TODO dummy")
    ;;   (->> (om-parse-this-headline)
    ;;        (om-property-is-non-nil-p :todo-keyword))
    ;;   => t
    ;;   (->> (om-parse-this-headline)
    ;;        (om-property-is-non-nil-p :commentedp))
    ;;   => nil)

    ;; (defexamples-content om-property-is-eq-p
    ;;   nil
    ;;   (:buffer "* [#A] dummy")
    ;;   (->> (om-parse-this-headline)
    ;;        (om-property-is-eq-p :priority ?A))
    ;;   => t
    ;;   (->> (om-parse-this-headline)
    ;;        (om-property-is-eq-p :priority ?B))
    ;;   => nil)

    ;; (defexamples-content om-property-is-equal-p
    ;;   nil
    ;;   (:buffer "* TODO dummy")
    ;;   (->> (om-parse-this-headline)
    ;;        (om-property-is-equal-p :todo-keyword "TODO"))
    ;;   => t
    ;;   (->> (om-parse-this-headline)
    ;;        (om-property-is-equal-p :todo-keyword "DONE"))
    ;;   => nil)

    ;; (defexamples-content om-property-is-predicate-p
    ;;   nil
    ;;   (:buffer "* this is a dummy")
    ;;   (->> (om-parse-this-headline)
    ;;        (om-property-is-predicate-p*
    ;;         :title (s-contains? "dummy" (car it))))
    ;;   => t)
    )

  (def-example-subgroup "Clock"
    nil

    ;; TODO add get/set/shift duration
    ;; TODO add get/set/shift start/end/single/double time

    (defexamples-content om-clock-is-running-p
      nil
      (:buffer "CLOCK: [2019-01-01 Tue 00:00]")
      (->> (om-parse-this-element)
           (om-clock-is-running-p))
      => t
      (:buffer "CLOCK: [2019-01-01 Tue 00:00]--[2019-01-02 Wed 00:00] => 24:00")
      (->> (om-parse-this-element)
           (om-clock-is-running-p))
      => nil))

  (defexamples-content om-clock-map-timestamp
    nil
    ;; TODO add more unit tests for this
    (:buffer "CLOCK: [2019-01-01 Tue 00:00]")
    (->> (om-parse-this-element)
         (om-clock-map-timestamp*
          (om-timestamp-shift 1 'day it))
         (om-to-trimmed-string))
    => "CLOCK: [2019-01-02 Wed 00:00]"
    )
  

  (def-example-subgroup "Headline"
    nil

    (defexamples-content om-headline-is-done-p
      nil
      (:buffer "* TODO darn")
      (->> (om-parse-this-headline)
           (om-headline-is-done-p))
      => nil
      (:buffer "* DONE yay")
      (->> (om-parse-this-headline)
           (om-headline-is-done-p))
      => t)

    (defexamples-content om-headline-is-archived-p
      nil
      (:buffer "* dummy")
      (->> (om-parse-this-headline)
           (om-headline-is-archived-p))
      => nil
      (:buffer "* dummy                                                             :ARCHIVE:")
      (->> (om-parse-this-headline)
           (om-headline-is-archived-p))
      => t)

    (defexamples-content om-headline-is-commented-p
      nil
      (:buffer "* dummy")
      (->> (om-parse-this-headline)
           (om-headline-is-commented-p))
      => nil
      (:buffer "* COMMENT dummy")
      (->> (om-parse-this-headline)
           (om-headline-is-commented-p))
      => t)

    (defexamples-content om-headline-has-tag-p
      nil
      (:buffer "* dummy")
      (->> (om-parse-this-headline)
           (om-headline-has-tag-p "tmsu"))
      => nil
      (:buffer "* dummy                                                             :tmsu:")
      (->> (om-parse-this-headline)
           (om-headline-has-tag-p "tmsu"))
      => t)

    (defexamples-content om-headline-get-statistics-cookie
      nil
      (:buffer "* statistically significant [10/10]")
      (->> (om-parse-this-headline)
           (om-headline-get-statistics-cookie)
           (om-to-string))
      => "[10/10]"
      (:buffer "* not statistically significant")
      (->> (om-parse-this-headline)
           (om-headline-get-statistics-cookie))
      => nil)

    ;; TODO add the shortcut version title setter

    )


  ;; TODO add inlinetask

  (def-example-subgroup "Item"
    nil

    ;; TODO add shortcut tag setter

    (defexamples-content om-item-is-unchecked-p
      nil
      (:buffer "- one"
               "- [ ] two"
               "- [X] three"
               "- [-] four")
      (->> (om-parse-this-element)
           (om--get-children)
           (-map #'om-item-is-unchecked-p))
      => '(nil t nil nil))

    (defexamples-content om-item-is-checked-p
      nil
      (:buffer "- one"
               "- [ ] two"
               "- [X] three"
               "- [-] four")
      (->> (om-parse-this-element)
           (om--get-children)
           (-map #'om-item-is-checked-p))
      => '(nil nil t nil))

    (defexamples-content om-item-is-trans-p
      nil
      (:buffer "- one"
               "- [ ] two"
               "- [X] three"
               "- [-] four")
      (->> (om-parse-this-element)
           (om--get-children)
           (-map #'om-item-is-trans-p))
      => '(nil nil nil t))
    
    (defexamples-content om-item-toggle-checkbox
      nil
      (:buffer "- [ ] one")
      (->> (om-parse-this-item)
           (om-item-toggle-checkbox)
           (om-to-trimmed-string))
      => "- [X] one"
      (->> (om-parse-this-item)
           (om-item-toggle-checkbox)
           (om-item-toggle-checkbox)
           (om-to-trimmed-string))
      => "- [ ] one"
      (:buffer "- [-] one")
      (->> (om-parse-this-item)
           (om-item-toggle-checkbox)
           (om-to-trimmed-string))
      => "- [-] one"
      (:buffer "- one")
      (->> (om-parse-this-item)
           (om-item-toggle-checkbox)
           (om-to-trimmed-string))
      => "- one")
    )

  (def-example-subgroup "Planning"
    nil

    (defexamples-content om-planning-set-timestamp
      nil
      (:buffer "* dummy"
               "CLOSED: [2019-01-01 Tue]")
      (:comment "Change an existing timestamp in planning")
      (->> (om-parse-this-headline)
           (om--headline-get-planning)
           (om-planning-set-timestamp
            :closed '(2019 1 2 &warning all 1 day &repeater cumulate 2 month))
           (om-to-trimmed-string))
      => "CLOSED: [2019-01-02 Wed +2m -1d]"
      (:comment "Add a new timestamp and remove another")
      (->> (om-parse-this-headline)
           (om--headline-get-planning)
           (om-planning-set-timestamp
            :deadline '(2112 1 1))
           (om-planning-set-timestamp
            :closed nil)
           (om-to-trimmed-string))
      => "DEADLINE: [2112-01-01 Fri]")

    (defexamples-content om-planning-map-timestamp
      nil
      (:buffer "* dummy"
               "CLOSED: [2019-01-01 Tue]")
      (:comment "Apply mapping function if timestamp exists")
      (->> (om-parse-this-headline)
           (om--headline-get-planning)
           (om-planning-map-timestamp*
            :closed (om-timestamp-shift 1 'day it))
           (om-to-trimmed-string))
      => "CLOSED: [2019-01-02 Wed]"
      (:comment "Do nothing if timestamp does not exist")
      (->> (om-parse-this-headline)
           (om--headline-get-planning)
           (om-planning-map-timestamp*
            :deadline (om-timestamp-shift 1 'day it))
           (om-to-trimmed-string))
      => "CLOSED: [2019-01-01 Tue]"
      (:comment "Throw error if new timestamp is not allowed")
      (->> (om-parse-this-headline)
           (om--headline-get-planning)
           (om-planning-map-timestamp
            :closed #'om-timestamp-toggle-active)
           (om-to-trimmed-string))
      !!> error))

  (def-example-subgroup "Statistics Cookie"
    nil
    (defexamples-content om-statistics-cookie-is-complete-p
      nil
      (:buffer "* statistically significant [10/10]")
      (->> (om-parse-this-headline)
           ;; TODO make public
           (om-headline-get-statistics-cookie)
           (om-statistics-cookie-is-complete-p))
      => t
      (:buffer "* statistically significant [1/10]")
      (->> (om-parse-this-headline)
           ;; TODO make public
           (om-headline-get-statistics-cookie)
           (om-statistics-cookie-is-complete-p))
      => nil
      (:buffer "* statistically significant [100%]")
      (->> (om-parse-this-headline)
           ;; TODO make public
           (om-headline-get-statistics-cookie)
           (om-statistics-cookie-is-complete-p))
      => t
      (:buffer "* statistically significant [33%]")
      (->> (om-parse-this-headline)
           ;; TODO make public
           (om-headline-get-statistics-cookie)
           (om-statistics-cookie-is-complete-p))
      => nil))

  (def-example-subgroup "Timestamp"
    nil

    (defexamples-content om-timestamp-get-start-time
      nil
      (:buffer "[2019-01-01 Tue]")
      (->> (om-parse-this-object)
           (om-timestamp-get-start-time))
      => '(2019 1 1 nil nil)
      (:buffer "[2019-01-01 Tue]--[2019-01-02 Wed]")
      (->> (om-parse-this-object)
           (om-timestamp-get-start-time))
      => '(2019 1 1 nil nil)
      (:buffer "[2019-01-01 Tue 00:00-12:00]")
      (->> (om-parse-this-object)
           (om-timestamp-get-start-time))
      => '(2019 1 1 0 0))

    (defexamples-content om-timestamp-get-end-time
      nil
      (:buffer "[2019-01-01 Tue]")
      (->> (om-parse-this-object)
           (om-timestamp-get-end-time))
      => nil
      (:buffer "[2019-01-01 Tue]--[2019-01-02 Wed]")
      (->> (om-parse-this-object)
           (om-timestamp-get-end-time))
      => '(2019 1 2 nil nil)
      (:buffer "[2019-01-01 Tue 00:00-12:00]")
      (->> (om-parse-this-object)
           (om-timestamp-get-end-time))
      => '(2019 1 1 12 0))

    (defexamples-content om-timestamp-is-active-p
      nil
      (:buffer "<2019-01-01 Tue>")
      (->> (om-parse-this-object)
           (om-timestamp-is-active-p))
      => t
      (:buffer "[2019-01-01 Tue]")
      (->> (om-parse-this-object)
           (om-timestamp-is-active-p))
      => nil)

    (defexamples-content om-timestamp-is-ranged-p
      nil
      (:buffer "[2019-01-01 Tue]--[2019-01-02 Wed]")
      (->> (om-parse-this-object)
           (om-timestamp-is-ranged-p))
      => t
      (:buffer "[2019-01-01 Tue 00:00-12:00]")
      (->> (om-parse-this-object)
           (om-timestamp-is-ranged-p))
      => t
      (:buffer "[2019-01-01 Tue]")
      (->> (om-parse-this-object)
           (om-timestamp-is-ranged-p))
      => nil)

    (defexamples-content om-timestamp-set-start-time
      nil
      (:buffer "[2019-01-02 Wed]")
      (:comment "If not a range this will turn into a range by moving only the start time.")
      (->> (om-parse-this-object)
           (om-timestamp-set-start-time '(2019 1 1))
           (om-to-trimmed-string))
      => "[2019-01-01 Tue]--[2019-01-02 Wed]"
      (:comment "Set a different time with different precision.")
      (->> (om-parse-this-object)
           (om-timestamp-set-start-time '(2019 1 1 10 0))
           (om-to-trimmed-string))
      => "[2019-01-01 Tue 10:00]--[2019-01-02 Wed]")

    (defexamples-content om-timestamp-set-end-time
      nil
      (:buffer "[2019-01-01 Tue]")
      (:comment "Add the end time")
      (->> (om-parse-this-object)
           (om-timestamp-set-end-time '(2019 1 2))
           (om-to-trimmed-string))
      => "[2019-01-01 Tue]--[2019-01-02 Wed]"
      (:buffer "[2019-01-01 Tue]--[2019-01-02 Wed]")
      (:comment "Remove the end time")
      (->> (om-parse-this-object)
           (om-timestamp-set-end-time nil)
           (om-to-trimmed-string))
      => "[2019-01-01 Tue]")

    (defexamples-content om-timestamp-set-single-time
      nil
      (:buffer "[2019-01-01 Tue]")
      (:comment "Don't make a range")
      (->> (om-parse-this-object)
           (om-timestamp-set-single-time '(2019 1 2))
           (om-to-trimmed-string))
      => "[2019-01-02 Wed]"
      (:buffer "[2019-01-01 Tue]--[2019-01-02 Wed]")
      (:comment "Output is not a range despite input being ranged")
      (->> (om-parse-this-object)
           (om-timestamp-set-single-time '(2019 1 3))
           (om-to-trimmed-string))
      => "[2019-01-03 Thu]")

    (defexamples-content om-timestamp-set-double-time
      nil
      (:buffer "[2019-01-01 Tue]")
      (:comment "Make a range")
      (->> (om-parse-this-object)
           (om-timestamp-set-double-time '(2019 1 2) '(2019 1 3))
           (om-to-trimmed-string))
      => "[2019-01-02 Wed]--[2019-01-03 Thu]"
      (:buffer "[2019-01-01 Tue]--[2019-01-03 Wed]")
      (:comment "Output is not a range despite input being ranged")
      (->> (om-parse-this-object)
           (om-timestamp-set-double-time '(2019 1 4) '(2019 1 5))
           (om-to-trimmed-string))
      => "[2019-01-04 Fri]--[2019-01-05 Sat]")

    (defexamples-content om-timestamp-set-range
      nil
      (:buffer "[2019-01-01 Tue]")
      (:comment "Use days as the unit for short format")
      (->> (om-parse-this-object)
           (om-timestamp-set-range 1)
           (om-to-trimmed-string))
      => "[2019-01-01 Tue]--[2019-01-02 Wed]"
      (:buffer "[2019-01-01 Tue 00:00]")
      (:comment "Use minutes as the unit for long format")
      (->> (om-parse-this-object)
           (om-timestamp-set-range 3)
           (om-to-trimmed-string))
      => "[2019-01-01 Tue 00:00]--[2019-01-01 Tue 00:03]"
      (:buffer "[2019-01-01 Tue]--[2019-01-03 Wed]")
      (:comment "Set range to 0 to remove end time")
      (->> (om-parse-this-object)
           (om-timestamp-set-range 0)
           (om-to-trimmed-string))
      => "[2019-01-01 Tue]")

    (defexamples-content om-timestamp-set-type
      nil
      (:buffer "[2019-01-01 Tue]")
      (->> (om-parse-this-object)
           (om-timestamp-set-type 'active)
           (om-to-trimmed-string))
      => "<2019-01-01 Tue>")

    (defexamples-content om-timestamp-shift
      nil
      (:buffer "[2019-01-01 Tue 12:00]")
      (:comment "Change each unit, and wrap around to the next unit as needed.")
      (->> (om-parse-this-object)
           (om-timestamp-shift 30 'minute)
           (om-to-trimmed-string))
      => "[2019-01-01 Tue 12:30]"
      (->> (om-parse-this-object)
           (om-timestamp-shift 60 'minute)
           (om-to-trimmed-string))
      => "[2019-01-01 Tue 13:00]"
      (->> (om-parse-this-object)
           (om-timestamp-shift 1 'hour)
           (om-to-trimmed-string))
      => "[2019-01-01 Tue 13:00]"
      (->> (om-parse-this-object)
           (om-timestamp-shift 1 'day)
           (om-to-trimmed-string))
      => "[2019-01-02 Wed 12:00]"
      (->> (om-parse-this-object)
           (om-timestamp-shift 31 'day)
           (om-to-trimmed-string))
      => "[2019-02-01 Fri 12:00]"
      (->> (om-parse-this-object)
           (om-timestamp-shift 1 'month)
           (om-to-trimmed-string))
      => "[2019-02-01 Fri 12:00]"
      (->> (om-parse-this-object)
           (om-timestamp-shift 13 'month)
           (om-to-trimmed-string))
      => "[2020-02-01 Sat 12:00]"
      (->> (om-parse-this-object)
           (om-timestamp-shift 1 'year)
           (om-to-trimmed-string))
      => "[2020-01-01 Wed 12:00]"
      (->> (om-parse-this-object)
           (om-timestamp-shift 0 'year)
           (om-to-trimmed-string))
      => "[2019-01-01 Tue 12:00]"
      (:buffer "[2019-01-01 Tue]")
      (:comment "Error when shifting hour/minute in short format")
      (->> (om-parse-this-object)
           (om-timestamp-shift 30 'minute)
           (om-to-trimmed-string))
      !!> error
      (->> (om-parse-this-object)
           (om-timestamp-shift 30 'hour)
           (om-to-trimmed-string))
      !!> error)

    (defexamples-content om-timestamp-shift-start
      nil
      (:buffer "[2019-01-01 Tue 12:00]")
      (:comment "If not a range, change start time and leave implicit end time.")
      (->> (om-parse-this-object)
           (om-timestamp-shift-start -1 'year)
           (om-to-trimmed-string))
      => "[2018-01-01 Mon 12:00]--[2019-01-01 Tue 12:00]"
      (:buffer "[2019-01-01 Tue]--[2019-01-03 Thu]")
      (:comment "Change only start time if a range")
      (->> (om-parse-this-object)
           (om-timestamp-shift-start 1 'day)
           (om-to-trimmed-string))
      => "[2019-01-02 Wed]--[2019-01-03 Thu]")

    (defexamples-content om-timestamp-shift-end
      nil
      (:buffer "[2019-01-01 Tue]")
      (:comment "Shift implicit end time if not a range.")
      (->> (om-parse-this-object)
           (om-timestamp-shift-end 1 'day)
           (om-to-trimmed-string))
      => "[2019-01-01 Tue]--[2019-01-02 Wed]"
      (:buffer "[2019-01-01 Tue]--[2019-01-02 Wed]")
      (:comment "Move only the second time if a range.")
      (->> (om-parse-this-object)
           (om-timestamp-shift-end 1 'day)
           (om-to-trimmed-string))
      => "[2019-01-01 Tue]--[2019-01-03 Thu]")

    (defexamples-content om-timestamp-toggle-active
      nil
      (:buffer "[2019-01-01 Tue]")
      (->> (om-parse-this-object)
           (om-timestamp-toggle-active)
           (om-to-trimmed-string))
      => "<2019-01-01 Tue>"
      (->> (om-parse-this-object)
           (om-timestamp-toggle-active)
           (om-timestamp-toggle-active)
           (om-to-trimmed-string))
      => "[2019-01-01 Tue]"
      (:buffer "[2019-01-01 Tue]--[2019-01-02 Wed]")
      (->> (om-parse-this-object)
           (om-timestamp-toggle-active)
           (om-to-trimmed-string))
      => "<2019-01-01 Tue>--<2019-01-02 Wed>"
      (->> (om-parse-this-object)
           (om-timestamp-toggle-active)
           (om-timestamp-toggle-active)
           (om-to-trimmed-string))
      => "[2019-01-01 Tue]--[2019-01-02 Wed]")))

(def-example-group "Branch/Child Manipulation"
  "Set, get, and map the children of branch nodes."

  (def-example-subgroup "Generic"
    nil

    (defexamples-content om-children-contain-point-p
      nil
      (:buffer "* headline"
               "findme")
      (->> (om-parse-this-headline)
           (om-children-contain-point-p 2))
      => nil
      (->> (om-parse-this-headline)
           (om-children-contain-point-p 15))
      => t)

    (defexamples-content om-get-children
      nil

      (:buffer "/this/ is a *paragraph*")
      (:comment "Return objects for object containers")
      (->> (om-parse-this-element)
           (om-get-children)
           (-map #'om-get-type))
      => '(italic plain-text bold)

      (:buffer "* headline"
               "stuff"
               "** subheadline")
      (:comment "Return elements for greater elements")
      (->> (om-parse-this-subtree)
           (om-get-children)
           (-map #'om-get-type))
      => '(section headline)

      (:buffer "#+CALL: ktulu()")
      (:comment "Throw error when attempting to get contents of a non-container")
      (->> (om-parse-this-element)
           (om-get-children)
           (-map #'om-get-type))
      !!> error

      :begin-hidden

      (:buffer "| a | b |")
      (->> (om-parse-this-table-row)
           (om-get-children)
           (-map #'om-get-type))
      => '(table-cell table-cell)

      (:buffer "#+BEGIN_VERSE"
               "verse /666/"
               "#+END_VERSE")
      (->> (om-parse-this-element)
           (om-get-children)
           (-map #'om-get-type))
      ;; plain-text for the newline at the end...I think
      => '(plain-text italic plain-text)

      (:buffer "#+BEGIN_CENTER"
               "paragraph thing"
               "#+END_CENTER")
      (->> (om-parse-this-element)
           (om-get-children)
           (-map #'om-get-type))
      => '(paragraph)

      (:buffer ":LOGBOOK:"
               "- log entry"
               "CLOCK: [2019-01-01 Tue]"
               ":END:")
      (->> (om-parse-this-element)
           (om-get-children)
           (-map #'om-get-type))
      => '(plain-list clock)

      (:buffer "[fn:1] bigfoot")
      (->> (om-parse-this-element)
           (om-get-children)
           (-map #'om-get-type))
      => '(paragraph)

      (:buffer "- item"
               "  - subitem")
      (->> (om-parse-this-element)
           (om-get-children)
           (-map #'om-get-type))
      => '(item)
      (->> (om-parse-this-item)
           (om-get-children)
           (-map #'om-get-type))
      => '(paragraph plain-list)

      (:buffer "* dummy"
               ":PROPERTIES:"
               ":ONE: one"
               ":TWO: two"
               ":END:")
      (->> (om-parse-this-headline)
           (om--headline-get-properties-drawer)
           (om-get-children)
           (-map #'om-get-type))
      => '(node-property node-property) 

      (:buffer "#+BEGIN_QUOTE"
               "no pity for the majority"
               "#+END_QUOTE")
      (->> (om-parse-this-element)
           (om-get-children)
           (-map #'om-get-type))
      => '(paragraph)

      ;; (:buffer "* dummy"
      ;;           "stuff")
      ;; (->> (om-parse-this-headline)
      ;;      (om-headline-get-section)
      ;;      (om-get-children)
      ;;      (-map #'om-get-type))
      ;; => '(paragraph)

      (:buffer "| a |"
               "| b |")
      (->> (om-parse-this-element)
           (om-get-children)
           (-map #'om-get-type))
      => '(table-row table-row)

      :end-hidden)

    (defexamples-content om-set-children
      nil

      (:buffer "/this/ is a *paragraph*")
      (:comment "Set children for object containers")
      (->> (om-parse-this-element)
           (om-set-children (list "this is lame"))
           (om-to-trimmed-string))
      => "this is lame"

      (:buffer "* headline"
               "stuff"
               "** subheadline")
      (:comment "Set children for greater elements")
      (->> (om-parse-this-subtree)
           (om-set-children (list (om-build-headline! :title-text "only me" :level 2)))
           (om-to-trimmed-string))
      => (:result "* headline"
                  "** only me")

      (:buffer "#+CALL: ktulu()")
      (:comment "Throw error when attempting to get children of a non-container")
      (->> (om-parse-this-element)
           (om-set-children "nil by mouth")
           (om-to-trimmed-string))
      !!> error

      :begin-hidden

      ;; TODO add hidden tests

      :end-hidden)

    (defexamples-content om-map-children
      nil

      (:buffer "/this/ is a *paragraph*")
      (->> (om-parse-this-element)
           (om-map-children
            (lambda (objs) (append objs (list " ...yeah"))))
           (om-to-trimmed-string))
      => "/this/ is a *paragraph* ...yeah"

      (:buffer "* headline"
               "** subheadline")
      (->> (om-parse-this-subtree)
           (om-map-children* (--map (om-shift-property :level 1 it) it))
           (om-to-trimmed-string))
      => (:result "* headline"
                  "*** subheadline")

      (:buffer "#+CALL: ktulu()")
      (:comment "Throw error when attempting to map children of a non-container")
      (->> (om-parse-this-element)
           (om-map-children #'ignore)
           (om-to-trimmed-string))
      !!> error

      :begin-hidden

      ;; TODO add hidden tests

      :end-hidden)

    
    (defexamples-content om-is-childless-p
      nil
      (:buffer "* dummy"
               "filled with useless knowledge")
      (->> (om-parse-this-headline)
           (om-is-childless-p))
      => nil
      (:buffer "* dummy")
      (->> (om-parse-this-headline)
           (om-is-childless-p))
      => t
      (:buffer "#+CALL: ktulu()")
      (:comment "Throw error when attempting to determine if non-container is empty")
      (->> (om-parse-this-element)
           (om-is-childless-p))
      !!> error))

  (def-example-subgroup "Headline"
    nil

    (defexamples-content om-headline-update-item-statistics
      nil
      (:buffer "* statistically significant [/]"
               "- irrelevant data"
               "- [ ] good data"
               "- [X] bad data")
      (->> (om-parse-this-headline)
           (om-headline-update-item-statistics)
           (om-to-trimmed-string))
      => (:result "* statistically significant [1/2]"
                  "- irrelevant data"
                  "- [ ] good data"
                  "- [X] bad data")

      (:buffer "* statistically significant [%]"
               "- irrelevant data"
               "- [ ] good data"
               "- [X] bad data")
      (->> (om-parse-this-headline)
           (om-headline-update-item-statistics)
           (om-to-trimmed-string))
      => (:result "* statistically significant [50%]"
                  "- irrelevant data"
                  "- [ ] good data"
                  "- [X] bad data")

      (:buffer "* statistically significant"
               "- irrelevant data"
               "- [ ] good data"
               "- [X] bad data")
      (->> (om-parse-this-headline)
           (om-headline-update-item-statistics)
           (om-to-trimmed-string))
      => (:result "* statistically significant"
                  "- irrelevant data"
                  "- [ ] good data"
                  "- [X] bad data"))

    (defexamples-content om-headline-update-todo-statistics
      nil
      (:buffer "* statistically significant [/]"
               "** irrelevant data"
               "** TODO good data"
               "** DONE bad data")
      (->> (om-parse-this-subtree)
           (om-headline-update-todo-statistics)
           (om-to-trimmed-string))
      => (:result "* statistically significant [1/2]"
                  "** irrelevant data"
                  "** TODO good data"
                  "** DONE bad data")

      (:buffer "* statistically significant [%]"
               "** irrelevant data"
               "** TODO good data"
               "** DONE bad data")
      (->> (om-parse-this-subtree)
           (om-headline-update-todo-statistics)
           (om-to-trimmed-string))
      => (:result "* statistically significant [50%]"
                  "** irrelevant data"
                  "** TODO good data"
                  "** DONE bad data")

      (:buffer "* statistically significant"
               "** irrelevant data"
               "** TODO good data"
               "** DONE bad data")
      (->> (om-parse-this-subtree)
           (om-headline-update-todo-statistics)
           (om-to-trimmed-string))
      => (:result "* statistically significant"
                  "** irrelevant data"
                  "** TODO good data"
                  "** DONE bad data"))

    ;; (defexamples-content om-headline-is-scheduled-p
    ;;   nil
    ;;   (:buffer "* lazy")
    ;;   (->> (om-parse-this-headline)
    ;;        (om-headline-is-scheduled-p))
    ;;   => nil
    ;;   (:buffer "* proactive"
    ;;             "SCHEDULED: [2019-01-01 Tue]")
    ;;   (->> (om-parse-this-headline)
    ;;        (om-headline-is-scheduled-p))
    ;;   => t)

    ;; (defexamples-content om-headline-is-deadlined-p
    ;;   nil
    ;;   (:buffer "* lazy")
    ;;   (->> (om-parse-this-headline)
    ;;        (om-headline-is-deadlined-p))
    ;;   => nil
    ;;   (:buffer "* proactive"
    ;;             "DEADLINE: [2019-01-01 Tue]")
    ;;   (->> (om-parse-this-headline)
    ;;        (om-headline-is-deadlined-p))
    ;;   => t)
    
    ;; (defexamples-content om-headline-is-closed-p
    ;;   nil
    ;;   (:buffer "* lazy")
    ;;   (->> (om-parse-this-headline)
    ;;        (om-headline-is-closed-p))
    ;;   => nil
    ;;   (:buffer "* proactive"
    ;;             "CLOSED: [2019-01-01 Tue]")
    ;;   (->> (om-parse-this-headline)
    ;;        (om-headline-is-closed-p))
    ;;   => t)

    ;; (defexamples-content om-headline-get-subheadlines
    ;;   nil
    ;;   (:buffer "* headline 1"
    ;;             "sectional stuff"
    ;;             "** headline 2"
    ;;             "** headline 3")
    ;;   (->> (om-parse-this-subtree)
    ;;        (om-headline-get-subheadlines)
    ;;        (-map #'om-to-trimmed-string))
    ;;   => '("** headline 2" "** headline 3")
    ;;   (:buffer "* headline 1"
    ;;             "sectional stuff")
    ;;   (->> (om-parse-this-subtree)
    ;;        (om-headline-get-subheadlines)
    ;;        (-map #'om-to-trimmed-string))
    ;;   => nil)

    ;; (defexamples-content om-headline-get-section
    ;;   nil
    ;;   (:buffer "* headline 1"
    ;;             "sectional stuff"
    ;;             "** headline 2"
    ;;             "** headline 3")
    ;;   (->> (om-parse-this-subtree)
    ;;        (om-headline-get-section)
    ;;        (om-to-trimmed-string))
    ;;   => "sectional stuff"
    ;;   (:buffer "* headline 1"
    ;;             "** headline 2"
    ;;             "** headline 3")
    ;;   (->> (om-parse-this-subtree)
    ;;        (om-headline-get-section)
    ;;        (om-to-trimmed-string))
    ;;   => "")

    ;; (defexamples-content om-headline-get-drawer
    ;;   nil
    ;;   (:buffer "* headline 1"
    ;;             ":LOGBOOK:"
    ;;             "- random note"
    ;;             ":END:"
    ;;             "rest of the section"
    ;;             "** headline 2")
    ;;   (->> (om-parse-this-subtree)
    ;;        (om-headline-get-drawer "LOGBOOK")
    ;;        (om-to-trimmed-string))
    ;;   => (:result ":LOGBOOK:"
    ;;               "- random note"
    ;;               ":END:")
    ;;   (->> (om-parse-this-subtree)
    ;;        (om-headline-get-drawer "OTHER")
    ;;        (om-to-trimmed-string))
    ;;   => "")

    ;; (defexamples-content om-headline-get-path
    ;;   nil
    ;;   (:buffer "* one"
    ;;             "** two"
    ;;             "*** three")
    ;;   (--> (om-parse-this-subtree)
    ;;        (om-match-first it 'headline)
    ;;        (om-headline-get-path it)
    ;;        (om-to-trimmed-string it))
    ;;   => '("one"))

    (defexamples-content om-headline-indent-subheadline
      nil
      (:buffer "* one"
               "** two"
               "** three"
               "*** four")
      (->> (om-parse-element-at 1)
           (om-headline-indent-subheadline 0)
           (om-to-trimmed-string))
      !!> error
      (->> (om-parse-element-at 1)
           (om-headline-indent-subheadline 1)
           (om-to-trimmed-string))
      => (:result "* one"
                  "** two"
                  "*** three"
                  "*** four"))

    (defexamples-content om-headline-indent-subtree
      nil
      (:buffer "* one"
               "** two"
               "** three"
               "*** four")
      (->> (om-parse-element-at 1)
           (om-headline-indent-subtree 1)
           (om-to-trimmed-string))
      => (:result "* one"
                  "** two"
                  "*** three"
                  "**** four"))

    (defexamples-content om-headline-unindent-subheadline
      nil
      (:buffer "* one"
               "** two"
               "** three"
               "*** four"
               "*** four"
               "*** four")
      (->> (om-parse-element-at 1)
           (om-headline-unindent-subheadline 1 1)
           (om-to-trimmed-string))
      => (:result "* one"
                  "** two"
                  "** three"
                  "*** four"
                  "** four"
                  "*** four"))

    (defexamples-content om-headline-unindent-subtree
      nil
      (:buffer "* one"
               "** two"
               "** three"
               "*** four"
               "*** four"
               "*** four")
      (->> (om-parse-element-at 1)
           (om-headline-unindent-subtree 1)
           (om-to-trimmed-string))
      => (:result "* one"
                  "** two"
                  "** three"
                  "** four"
                  "** four"
                  "** four")))

  ;; (def-example-subgroup "Item"
  ;;   nil

  ;; (defexamples-content om-item-get-level
  ;;   nil
  ;;   (:buffer "- one"
  ;;             "  - two"
  ;;             "    - three")
  ;;   (->> (om-parse-this-item)
  ;;        (om-)))

  ;; (defexamples-content om-item-get-sublist
  ;;   nil
  ;;   (:buffer "- one"
  ;;             "  - two"
  ;;             "  - three"
  ;;             "- four")
  ;;   (->> (om-parse-this-item)
  ;;        (om-item-get-sublist)
  ;;        (om-to-trimmed-string))
  ;;   => (:result "- two"
  ;;               "- three")
  ;;   (:buffer "- one"
  ;;             "- two")
  ;;   (->> (om-parse-this-item)
  ;;        (om-item-get-sublist)
  ;;        (om-to-trimmed-string))
  ;;   => "")

  ;; (defexamples-content om-item-get-paragraph
  ;;   nil
  ;;   (:buffer "- one")
  ;;   (->> (om-parse-this-item)
  ;;        (om-item-get-paragraph)
  ;;        (om-to-trimmed-string))
  ;;   => "one"
  ;;   (:buffer "- [ ] one")
  ;;   (->> (om-parse-this-item)
  ;;        (om-item-get-paragraph)
  ;;        (om-to-trimmed-string))
  ;;   => "one"
  ;;   (:buffer "- tmsu :: one")
  ;;   (->> (om-parse-this-item)
  ;;        (om-item-get-paragraph)
  ;;        (om-to-trimmed-string))
  ;;   => "one"
  ;;   (:buffer "- tmsu ::")
  ;;   (->> (om-parse-this-item)
  ;;        (om-item-get-paragraph)
  ;;        (om-to-trimmed-string))
  ;;   => ""))

  (def-example-subgroup "Plain List"
    nil
    
    (defexamples-content om-plain-list-set-type
      nil
      (:buffer "- [ ] one"
               "- [X] two")
      (->> (om-parse-this-element)
           (om-plain-list-set-type 'ordered)
           (om-to-trimmed-string))
      => (:result "1. [ ] one"
                  "2. [X] two")
      (:buffer "1. [ ] one"
               "2. [X] two")
      (->> (om-parse-this-element)
           (om-plain-list-set-type '-)
           (om-to-trimmed-string))
      => (:result "- [ ] one"
                  "- [X] two"))

    (defexamples-content om-plain-list-indent-item
      nil
      (:buffer "- one"
               "- two"
               "  - three"
               "- four")
      (:comment "It makes no sense to indent the first item")
      (->> (om-parse-element-at 1)
           (om-plain-list-indent-item 0)
           (om-to-trimmed-string))
      !!> error
      (->> (om-parse-element-at 1)
           (om-plain-list-indent-item 1)
           (om-to-trimmed-string))
      => (:result "- one"
                  "  - two"
                  "  - three"
                  "- four")
      (->> (om-parse-element-at 1)
           (om-plain-list-indent-item 2)
           (om-to-trimmed-string))
      => (:result "- one"
                  "- two"
                  "  - three"
                  "  - four"))

    (defexamples-content om-plain-list-indent-item-tree
      nil
      (:buffer "- one"
               "- two"
               "  - three"
               "- four")
      (->> (om-parse-element-at 1)
           (om-plain-list-indent-item-tree 1)
           (om-to-trimmed-string))
      => (:result "- one"
                  "  - two"
                  "    - three"
                  "- four"))

    (defexamples-content om-plain-list-unindent-item
      nil
      (:buffer "- one"
               "- two"
               "  - three"
               "  - three"
               "  - three"
               "- four")
      (->> (om-parse-element-at 1)
           (om-plain-list-unindent-item 1 0)
           (om-to-trimmed-string))
      => (:result "- one"
                  "- two"
                  "- three"
                  "  - three"
                  "  - three"
                  "- four")
      (->> (om-parse-element-at 1)
           (om-plain-list-unindent-item 1 1)
           (om-to-trimmed-string))
      => (:result "- one"
                  "- two"
                  "  - three"
                  "- three"
                  "  - three"
                  "- four")
      (->> (om-parse-element-at 1)
           (om-plain-list-unindent-item 2 1)
           (om-to-trimmed-string))
      => (:result "- one"
                  "- two"
                  "  - three"
                  "  - three"
                  "  - three"
                  "- four"))
    
    (defexamples-content om-plain-list-unindent-items
      nil
      (:buffer "- one"
               "- two"
               "  - three"
               "  - three"
               "  - three"
               "- four")
      (->> (om-parse-element-at 1)
           (om-plain-list-unindent-items 1)
           (om-to-trimmed-string))
      => (:result "- one"
                  "- two"
                  "- three"
                  "- three"
                  "- three"
                  "- four")
      (->> (om-parse-element-at 1)
           (om-plain-list-unindent-items 2)
           (om-to-trimmed-string))
      => (:result "- one"
                  "- two"
                  "  - three"
                  "  - three"
                  "  - three"
                  "- four")))

  (def-example-subgroup "Table"
    nil

    (defexamples-content om-table-get-cell
      nil
      (:buffer "| 1 | 2 | 3 |"
               "|---+---+---|"
               "| a | b | c |")
      (->> (om-parse-this-element)
           (om-table-get-cell 0 0)
           (om--get-children)
           (car))
      => "1"
      (->> (om-parse-this-element)
           (om-table-get-cell 1 1)
           (om--get-children)
           (car))
      => "b"
      (->> (om-parse-this-element)
           (om-table-get-cell -1 -1)
           (om--get-children)
           (car))
      => "c"
      :begin-hidden
      (->> (om-parse-this-element)
           (om-table-get-cell 0 3)
           (om--get-children)
           (car))
      !!> error
      :end-hidden)

    (defexamples-content om-table-replace-cell
      nil
      (:buffer "| 1 | 2 |"
               "|---+---|"
               "| a | b |")
      (->> (om-parse-this-element)
           (om-table-replace-cell
            0 0 (om-build-table-cell "2"))
           (om-to-trimmed-string))
      => (:result "| 2 | 2 |"
                  "|---+---|"
                  "| a | b |")
      (->> (om-parse-this-element)
           (om-table-replace-cell
            -1 -1 (om-build-table-cell "B"))
           (om-to-trimmed-string))
      => (:result "| 1 | 2 |"
                  "|---+---|"
                  "| a | B |"))

    (defexamples-content om-table-replace-cell!
      nil
      (:buffer "| 1 | 2 |"
               "|---+---|"
               "| a | b |")
      (->> (om-parse-this-element)
           (om-table-replace-cell! 0 0 "2")
           (om-to-trimmed-string))
      => (:result "| 2 | 2 |"
                  "|---+---|"
                  "| a | b |")
      (->> (om-parse-this-element)
           (om-table-replace-cell! -1 -1 "B")
           (om-to-trimmed-string))
      => (:result "| 1 | 2 |"
                  "|---+---|"
                  "| a | B |"))

    (defexamples-content om-table-clear-cell
      nil
      (:buffer "| 1 | 2 |"
               "|---+---|"
               "| a | b |")
      (->> (om-parse-this-element)
           (om-table-clear-cell 0 0)
           (om-to-trimmed-string))
      => (:result "|   | 2 |"
                  "|---+---|"
                  "| a | b |")
      (->> (om-parse-this-element)
           (om-table-clear-cell -1 -1)
           (om-to-trimmed-string))
      => (:result "| 1 | 2 |"
                  "|---+---|"
                  "| a |   |"))

    (defexamples-content om-table-delete-column
      nil
      (:buffer "| a | b |"
               "|---+---|"
               "| c | d |")
      (->> (om-parse-element-at 1)
           (om-table-delete-column 0)
           (om-to-trimmed-string))
      => (:result "| b |"
                  "|---|"
                  "| d |")
      (->> (om-parse-element-at 1)
           (om-table-delete-column 1)
           (om-to-trimmed-string))
      => (:result "| a |"
                  "|---|"
                  "| c |")
      (->> (om-parse-element-at 1)
           (om-table-delete-column -1)
           (om-to-trimmed-string))
      => (:result "| a |"
                  "|---|"
                  "| c |"))

    (defexamples-content om-table-replace-column
      nil
      (:buffer "| a | b |"
               "|---+---|"
               "| c | d |")
      (->> (om-parse-element-at 1)
           (om-table-replace-column
            0 (list
               (om-build-table-cell "A")
               (om-build-table-cell "B")))
           (om-to-trimmed-string))
      => (:result "| A | b |"
                  "|---+---|"
                  "| B | d |")
      (->> (om-parse-element-at 1)
           (om-table-replace-column
            -1 (list
                (om-build-table-cell "A")
                (om-build-table-cell "B")))
           (om-to-trimmed-string))
      => (:result "| a | A |"
                  "|---+---|"
                  "| c | B |"))

    (defexamples-content om-table-replace-column!
      nil
      (:buffer "| a | b |"
               "|---+---|"
               "| c | d |")
      (->> (om-parse-element-at 1)
           (om-table-replace-column! 0 '("A" "B"))
           (om-to-trimmed-string))
      => (:result "| A | b |"
                  "|---+---|"
                  "| B | d |")
      (->> (om-parse-element-at 1)
           (om-table-replace-column! -1 '("A" "B"))
           (om-to-trimmed-string))
      => (:result "| a | A |"
                  "|---+---|"
                  "| c | B |"))

    (defexamples-content om-table-clear-column
      nil
      (:buffer "| a | b |"
               "|---+---|"
               "| c | d |")
      (->> (om-parse-element-at 1)
           (om-table-clear-column 0)
           (om-to-trimmed-string))
      => (:result "|   | b |"
                  "|---+---|"
                  "|   | d |")
      (->> (om-parse-element-at 1)
           (om-table-clear-column -1)
           (om-to-trimmed-string))
      => (:result "| a |   |"
                  "|---+---|"
                  "| c |   |"))

    (defexamples-content om-table-replace-row
      nil
      (:buffer "| a | b |"
               "|---+---|"
               "| c | d |")
      (->> (om-parse-element-at 1)
           (om-table-replace-row
            0 (om-build-table-row
               (om-build-table-cell "A")
               (om-build-table-cell "B")))
           (om-to-trimmed-string))
      => (:result "| A | B |"
                  "|---+---|"
                  "| c | d |")
      (->> (om-parse-element-at 1)
           (om-table-replace-row
            -1 (om-build-table-row
                (om-build-table-cell "A")
                (om-build-table-cell "B")))
           (om-to-trimmed-string))
      => (:result "| a | b |"
                  "|---+---|"
                  "| A | B |"))

    (defexamples-content om-table-replace-row!
      nil
      (:buffer "| a | b |"
               "|---+---|"
               "| c | d |")
      (->> (om-parse-element-at 1)
           (om-table-replace-row! 0 '("A" "B"))
           (om-to-trimmed-string))
      => (:result "| A | B |"
                  "|---+---|"
                  "| c | d |")
      (->> (om-parse-element-at 1)
           (om-table-replace-row! -1 '("A" "B"))
           (om-to-trimmed-string))
      => (:result "| a | b |"
                  "|---+---|"
                  "| A | B |"))

    (defexamples-content om-table-clear-row
      nil
      (:buffer "| a | b |"
               "|---+---|"
               "| c | d |")
      (->> (om-parse-element-at 1)
           (om-table-clear-row 0)
           (om-to-trimmed-string))
      => (:result "|   |   |"
                  "|---+---|"
                  "| c | d |")
      (->> (om-parse-element-at 1)
           (om-table-clear-row -1)
           (om-to-trimmed-string))
      => (:result "| a | b |"
                  "|---+---|"
                  "|   |   |"))

    (defexamples-content om-table-delete-row
      nil
      (:buffer "| a | b |"
               "|---+---|"
               "| c | d |")
      (->> (om-parse-element-at 1)
           (om-table-delete-row 0)
           (om-to-trimmed-string))
      => (:result "|---+---|"
                  "| c | d |")
      (->> (om-parse-element-at 1)
           (om-table-delete-row 1)
           (om-to-trimmed-string))
      => (:result "| a | b |"
                  "| c | d |")
      (->> (om-parse-element-at 1)
           (om-table-delete-row -1)
           (om-to-trimmed-string))
      => (:result "| a | b |"
                  "|---+---|"))

    (defexamples-content om-table-insert-column
      nil
      (:buffer "| a | b |"
               "|---+---|"
               "| c | d |")
      (->> (om-parse-element-at 1)
           (om-table-insert-column
            1
            (list
             (om-build-table-cell "x")
             (om-build-table-cell "y")))
           (om-to-trimmed-string))
      => (:result "| a | x | b |"
                  "|---+---+---|"
                  "| c | y | d |")
      (->> (om-parse-element-at 1)
           (om-table-insert-column
            -1
            (list
             (om-build-table-cell "x")
             (om-build-table-cell "y")))
           (om-to-trimmed-string))
      => (:result "| a | b | x |"
                  "|---+---+---|"
                  "| c | d | y |"))

    (defexamples-content om-table-insert-column!
      nil
      (:buffer "| a | b |"
               "|---+---|"
               "| c | d |")
      (->> (om-parse-element-at 1)
           (om-table-insert-column! 1 '("x" "y"))
           (om-to-trimmed-string))
      => (:result "| a | x | b |"
                  "|---+---+---|"
                  "| c | y | d |")
      (->> (om-parse-element-at 1)
           (om-table-insert-column! -1 '("x" "y"))
           (om-to-trimmed-string))
      => (:result "| a | b | x |"
                  "|---+---+---|"
                  "| c | d | y |"))

    (defexamples-content om-table-insert-row
      nil
      (:buffer "| a | b |"
               "|---+---|"
               "| c | d |")
      (->> (om-parse-element-at 1)
           (om-table-insert-row
            1
            (om-build-table-row
             (om-build-table-cell "x")
             (om-build-table-cell "y")))
           (om-to-trimmed-string))
      => (:result "| a | b |"
                  "| x | y |"
                  "|---+---|"
                  "| c | d |")
      (->> (om-parse-element-at 1)
           (om-table-insert-row
            2
            (om-build-table-row
             (om-build-table-cell "x")
             (om-build-table-cell "y")))
           (om-to-trimmed-string))
      => (:result "| a | b |"
                  "|---+---|"
                  "| x | y |"
                  "| c | d |")
      (->> (om-parse-element-at 1)
           (om-table-insert-row
            -1
            (om-build-table-row
             (om-build-table-cell "x")
             (om-build-table-cell "y")))
           (om-to-trimmed-string))
      => (:result "| a | b |"
                  "|---+---|"
                  "| c | d |"
                  "| x | y |"))

    (defexamples-content om-table-insert-row!
      nil
      (:buffer "| a | b |"
               "|---+---|"
               "| c | d |")
      (->> (om-parse-element-at 1)
           (om-table-insert-row! 1 '("x" "y"))
           (om-to-trimmed-string))
      => (:result "| a | b |"
                  "| x | y |"
                  "|---+---|"
                  "| c | d |")
      (->> (om-parse-element-at 1)
           (om-table-insert-row! 2 '("x" "y"))
           (om-to-trimmed-string))
      => (:result "| a | b |"
                  "|---+---|"
                  "| x | y |"
                  "| c | d |")
      (->> (om-parse-element-at 1)
           (om-table-insert-row! -1 '("x" "y"))
           (om-to-trimmed-string))
      => (:result "| a | b |"
                  "|---+---|"
                  "| c | d |"
                  "| x | y |"))))

(def-example-group "Node Matching"
  "Use pattern-matching to selectively perform operations on nodes in trees."

  (defexamples-content om-match
    nil

    (:buffer "* headline one"
             "** TODO headline two"
             "** COMMENT headline three"
             "** headline four")

    (:comment "Use a symbol to match a type, in this case all "
              "headlines.")
    (->> (om-parse-this-subtree)
         (om-match '(headline))
         (--map (om-to-trimmed-string it)))
    => '("** TODO headline two"
         "** COMMENT headline three"
         "** headline four")

    (:comment "Use integers specify the index to return. Negative "
              "integers count from the end. Out of range integers "
              "return nil")
    (->> (om-parse-this-subtree)
         (om-match '(1))
         (--map (om-to-trimmed-string it)))
    => '("** COMMENT headline three")
    (->> (om-parse-this-subtree)
         (om-match '(-1))
         (--map (om-to-trimmed-string it)))
    => '("** headline four")
    (->> (om-parse-this-subtree)
         (om-match '(3))
         (--map (om-to-trimmed-string it)))
    => nil

    (:comment "Use a two-membered list with an operator and an "
              "integer to match a range of indices. Allowed "
              "operators are <, >, <=, and and >=.")
    (->> (om-parse-this-subtree)
         (om-match '((> 0)))
         (--map (om-to-trimmed-string it)))
    => '("** COMMENT headline three" "** headline four")
    (->> (om-parse-this-subtree)
         (om-match '((>= 1)))
         (--map (om-to-trimmed-string it)))
    => '("** COMMENT headline three" "** headline four")

    (:comment "Use a plist to match based on an elements properties.")
    (->> (om-parse-this-subtree)
         (om-match '((:todo-keyword "TODO")))
         (--map (om-to-trimmed-string it)))
    => '("** TODO headline two")
    (->> (om-parse-this-subtree)
         (om-match '((:todo-keyword nil)))
         (--map (om-to-trimmed-string it)))
    => '("** COMMENT headline three" "** headline four")
    (->> (om-parse-this-subtree)
         (om-match '((:todo-keyword "DONE")))
         (--map (om-to-trimmed-string it)))
    => nil

    (:buffer "* headline one"
             "this is *text1* of *text2*"
             "** headline two"
             "here is more *text3*"
             "*** headline three"
             "and here is even more *text4* and *text5*"
             "**** headline 4")
    (:comment "Specify multiple levels of matching using multiple "
              "queries.")
    (->> (om-parse-this-subtree)
         (om-match '(section paragraph bold))
         (--map (om-to-trimmed-string it)))
    => '("*text1*" "*text2*")

    (:comment "Use the keyword :any as a wildcard to match any "
              "element at a particular level.")
    (->> (om-parse-this-subtree)
         (om-match '(:any :any bold))
         (--map (om-to-trimmed-string it)))
    => '("*text1*" "*text2*")
    ;; TODO not sure why an empty string comes out here
    (->> (om-parse-this-subtree)
         (om-match '(section paragraph :any))
         (--map (om-to-trimmed-string it)))
    => '("this is" "*text1*" "of" "*text2*" "")
    (->> (om-parse-this-subtree)
         (om-match '(:any bold))
         (--map (om-to-trimmed-string it)))
    => nil

    (:comment "Use the keyword :many to match one or more levels "
              "of any element.")
    (->> (om-parse-this-subtree)
         (om-match '(:many bold))
         (--map (om-to-trimmed-string it)))
    => '("*text1*" "*text2*" "*text3*" "*text4*" "*text5*")

    (:comment "Use the keyword :many! to match one or more levels, "
              "except unlike :many do not match within any elements "
              "that have already matched.")
    ;; TODO add predicate???
    (->> (om-parse-this-subtree)
         (om-match '(headline :many! headline))
         (--map (om-to-trimmed-string it)))
    => '("*** headline three
and here is even more *text4* and *text5*
**** headline 4")

    (:buffer "* headline one"
             "** TODO headline two"
             "** COMMENT headline three"
             "** headline four")
    (:comment "Find the first subheadline")
    (->> (om-parse-this-subtree)
         (om-match '(:first headline))
         (car)
         (om-to-trimmed-string))
    => "** TODO headline two"
    
    (:buffer "* headline one"
             "** TODO headline two"
             "** COMMENT headline three"
             "** headline four")
    (:comment "Find the last subheadline")
    (->> (om-parse-this-subtree)
         (om-match '(:last headline))
         (car)
         (om-to-trimmed-string))
    => "** headline four")

  (defexamples-content om-match-delete
    nil
    (:buffer "* headline one"
             "** headline two"
             "** headline three"
             "** headline four")
    (:comment "Selectively delete headlines")
    (->> (om-parse-this-subtree)
         (om-match-delete '(headline))
         (om-to-trimmed-string))
    => "* headline one"
    (->> (om-parse-this-subtree)
         (om-match-delete '(:first headline))
         (om-to-trimmed-string))
    => (:result "* headline one"
                "** headline three"
                "** headline four")
    (->> (om-parse-this-subtree)
         (om-match-delete '(:last headline))
         (om-to-trimmed-string))
    => (:result "* headline one"
                "** headline two"
                "** headline three"))

  (defexamples-content om-match-extract
    nil
    (:buffer "pull me /under/")
    (--> (om-parse-this-element)
         (om-match-extract '(:many italic) it)
         (cons (-map #'om-to-trimmed-string (car it))
               (om-to-trimmed-string (cdr it))))
    => '(("/under/") . "pull me"))

  (defexamples-content om-match-map
    nil

    (:buffer "* headline one"
             "** TODO headline two"
             "** headline three"
             "** headline four")

    (:comment "Selectively mark headlines as DONE")
    (->> (om-parse-this-subtree)
         (om-match-map '(headline)
           (lambda (it) (om-set-property :todo-keyword "DONE" it)))
         (om-to-trimmed-string))
    => (:result "* headline one"
                "** DONE headline two"
                "** DONE headline three"
                "** DONE headline four")
    (->> (om-parse-this-subtree)
         (om-match-map* '(:first headline)
           (om-set-property :todo-keyword "DONE" it))
         (om-to-trimmed-string))
    => (:result "* headline one"
                "** DONE headline two"
                "** headline three"
                "** headline four")
    (->> (om-parse-this-subtree)
         (om-match-map '(:last headline)
           (-partial #'om-set-property :todo-keyword "DONE"))
         (om-to-trimmed-string))
    => (:result "* headline one"
                "** TODO headline two"
                "** headline three"
                "** DONE headline four"))
  
  (defexamples-content om-match-mapcat
    nil

    (:buffer "* one"
             "** two")
    (->> (om-parse-this-subtree)
         (om-match-mapcat* '(:first headline)
           (list (om-build-headline! :title-text "1.5" :level 2) it))
         (om-to-trimmed-string))
    => (:result "* one"
                "** 1.5"
                "** two"))

  (defexamples-content om-match-replace
    nil
    (:buffer "*1* 2 *3* 4 *5* 6 *7* 8 *9* 10")
    (->> (om-parse-this-element)
         (om-match-replace '(:many bold)
           (om-build-bold :post-blank 1 "0"))
         (om-to-trimmed-string))
    => "*0* 2 *0* 4 *0* 6 *0* 8 *0* 10")

  (defexamples-content om-match-insert-before
    nil
    (:buffer "* one"
             "** two"
             "** three")
    (->> (om-parse-this-subtree)
         (om-match-insert-before '(headline)
           (om-build-headline! :title-text "new" :level 2))
         (om-to-trimmed-string))
    => (:result "* one"
                "** new"
                "** two"
                "** new"
                "** three"))

  (defexamples-content om-match-insert-after
    nil
    (:buffer "* one"
             "** two"
             "** three")
    (->> (om-parse-this-subtree)
         (om-match-insert-after '(headline)
           (om-build-headline! :title-text "new" :level 2))
         (om-to-trimmed-string))
    => (:result "* one"
                "** two"
                "** new"
                "** three"
                "** new"))

  (defexamples-content om-match-insert-within
    nil
    (:buffer "* one"
             "** two"
             "** three")
    (->> (om-parse-this-subtree)
         (om-match-insert-within '(headline) 0
           (om-build-headline! :title-text "new" :level 3))
         (om-to-trimmed-string))
    => (:result "* one"
                "** two"
                "*** new"
                "** three"
                "*** new")
    (:comment "The nil pattern denotes top-level element")
    (->> (om-parse-this-subtree)
         (om-match-insert-within nil 1
           (om-build-headline! :title-text "new" :level 2))
         (om-to-trimmed-string))
    => (:result "* one"
                "** two"
                "** new"
                "** three"))

  ;; add splice

  (defexamples-content om-match-splice-before
    nil
    (:buffer "* one"
             "** two"
             "** three")
    (->> (om-parse-this-subtree)
         (om-match-splice-before '(0)
           (list
            (om-build-headline! :title-text "new0" :level 2)
            (om-build-headline! :title-text "new1" :level 2)))
         (om-to-trimmed-string))
    => (:result "* one"
                "** new0"
                "** new1"
                "** two"
                "** three"))

  (defexamples-content om-match-splice-after
    nil
    (:buffer "* one"
             "** two"
             "** three")
    (->> (om-parse-this-subtree)
         (om-match-splice-after '(0)
           (list
            (om-build-headline! :title-text "new0" :level 2)
            (om-build-headline! :title-text "new1" :level 2)))
         (om-to-trimmed-string))
    => (:result "* one"
                "** two"
                "** new0"
                "** new1"
                "** three"))

  (defexamples-content om-match-splice-within
    nil
    (:buffer "* one"
             "** two"
             "** three")
    (->> (om-parse-this-subtree)
         (om-match-splice-within nil 1
           (list
            (om-build-headline! :title-text "new0" :level 2)
            (om-build-headline! :title-text "new1" :level 2)))
         (om-to-trimmed-string))
    => (:result "* one"
                "** two"
                "** new0"
                "** new1"
                "** three")))

(def-example-group "Buffer Side Effects"
  "Map node manipulations into buffers."

  (def-example-subgroup "Insert"
    nil

    (defexamples-content om-insert
      nil
      (:buffer "* one"
               "")
      (->> (om-build-headline! :title-text "two")
           (om-insert (point-max)))
      $> (:result "* one"
                  "* two")

      (:buffer "a *game* or a /boy/")
      (->> (om-build-paragraph! "we don't care if you're")
           (om-insert (point-min)))
      $> (:result "we don't care if you're"
                  "a *game* or a /boy/"))

    (defexamples-content om-insert-tail
      nil
      :begin-hidden
      (:buffer "* one"
               "")
      (->> (om-build-headline! :title-text "two")
           (om-insert-tail (point-max)))
      $> (:result "* one"
                  "* two")

      (:buffer "a *game* or a /boy/")
      (->> (om-build-paragraph! "we don't care if you're")
           (om-insert-tail (point-min)))
      $> (:result "we don't care if you're"
                  "a *game* or a /boy/")
      :end-hidden))

  (def-example-subgroup "Update"
    nil

    (defexamples-content om-update
      nil
      
      (:buffer "* TODO win grammy")
      (->> (om-parse-this-headline)
           (om-update
            (lambda (hl) (om-set-property :todo-keyword "DONE" hl))))
      $> "* DONE win grammy"

      (:buffer "* win grammy [0/0]"
               "- [ ] write punk song"
               "- [ ] get new vocalist"
               "- [ ] sell 2 singles")
      (->> (om-parse-this-headline)
           (om-update*
             (->> (om-match-map '(:many item) #'om-item-toggle-checkbox it)
                  (om-headline-update-item-statistics))))
      $> (:result "* win grammy [3/3]"
                  "- [X] write punk song"
                  "- [X] get new vocalist"
                  "- [X] sell 2 singles")
      )

    (defexamples-content om-update-object-at
      nil
      (:buffer "[[http://example.com][desc]]")
      (om-update-object-at* (point)
        (om-set-property :path "//buymoreram.com" it))
      $> "[[http://buymoreram.com][desc]]")

    (defexamples-content om-update-element-at
      nil
      (:buffer "#+CALL: ktulu()")
      (om-update-element-at* (point)
        (om-set-properties 
         (list :call "cthulhu"
               :inside-header '(:cache no)
               :arguments '("x=4")
               :end-header '(:results html))
         it))
      $> "#+CALL: cthulhu[:cache no](x=4) :results html")

    (defexamples-content om-update-table-row-at
      nil
      (:buffer "| a | b |")
      (om-update-table-row-at* (point)
        (om-map-children* (cons (om-build-table-cell! "0") it) it))
      $> "| 0 | a | b |")

    (defexamples-content om-update-item-at
      nil
      (:buffer "- [ ] thing")
      (om-update-item-at* (point)
        (om-item-toggle-checkbox it))
      $> "- [X] thing")

    (defexamples-content om-update-headline-at
      nil
      (:buffer "* TODO might get done")
      (om-update-headline-at* (point)
        (om-set-property :todo-keyword "DONE" it))
      $> "* DONE might get done")

    (defexamples-content om-update-subtree-at
      nil
      (:buffer "* one"
               "** two"
               "** three")
      (om-update-subtree-at* (point)
        (om-headline-indent-subheadline 1 it))
      $> (:result "* one"
                  "** two"
                  "*** three")))

  ;; TODO should probably still test these

  ;; (defexamples-content om-update-this-object
  ;;   nil)

  ;; (defexamples-content om-update-this-element
  ;;   nil)
  
  ;; (defexamples-content om-update-this-table-row
  ;;   nil)

  ;; (defexamples-content om-update-this-item
  ;;   nil)

  ;; (defexamples-content om-update-this-headline
  ;;   nil)

  ;; (defexamples-content om-update-this-subtree
  ;;   nil))

  (def-example-subgroup "Misc"
    nil

    (defexamples-content om-fold
      nil)

    (defexamples-content om-unfold
      nil)))
