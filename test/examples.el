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

  (defexamples-content om-elem-parse-object-at
    nil
    (:buffer "*text*")
    (->> (om-elem-parse-object-at 1)
         (om-elem-get-type))
    => 'bold

    :begin-hidden
    (:buffer "~text~")
    (->> (om-elem-parse-object-at 1)
         (om-elem-get-type))
    => 'code
    ;; TODO add entity
    ;; TODO add export snippet
    (:buffer "[fn:1:text]")
    (->> (om-elem-parse-object-at 1)
         (om-elem-get-type))
    => 'footnote-reference
    (:buffer "call_name()")
    (->> (om-elem-parse-object-at 1)
         (om-elem-get-type))
    => 'inline-babel-call
    (:buffer "src_emacs{}")
    (->> (om-elem-parse-object-at 1)
         (om-elem-get-type))
    => 'inline-src-block
    (:buffer "/text/")
    (->> (om-elem-parse-object-at 1)
         (om-elem-get-type))
    => 'italic
    (:buffer "\\\\")
    (->> (om-elem-parse-object-at 1)
         (om-elem-get-type))
    => 'line-break
    ;; TODO add latex frag
    (:buffer "[[path][desc]]")
    (->> (om-elem-parse-object-at 1)
         (om-elem-get-type))
    => 'link
    (:buffer "{{{macro}}}")
    (->> (om-elem-parse-object-at 1)
         (om-elem-get-type))
    => 'macro
    (:buffer "<<<text>>>")
    (->> (om-elem-parse-object-at 1)
         (om-elem-get-type))
    => 'radio-target
    (:buffer "[1/2]")
    (->> (om-elem-parse-object-at 1)
         (om-elem-get-type))
    => 'statistics-cookie
    (:buffer "+text+")
    (->> (om-elem-parse-object-at 1)
         (om-elem-get-type))
    => 'strike-through
    ;; TODO this is confusing for docs
    (:buffer "a_b")
    (->> (om-elem-parse-object-at 3)
         (om-elem-get-type))
    => 'subscript
    (:buffer "a^b")
    (->> (om-elem-parse-object-at 3)
         (om-elem-get-type))
    => 'superscript
    (:buffer "| a |")
    (->> (om-elem-parse-object-at 2)
         (om-elem-get-type))
    => 'table-cell
    (:buffer "<<text>>")
    (->> (om-elem-parse-object-at 1)
         (om-elem-get-type))
    => 'target
    :end-hidden

    (:buffer "[2019-01-01 Tue]")
    (->> (om-elem-parse-object-at 1)
         (om-elem-get-type))
    => 'timestamp

    :begin-hidden
    (:buffer "_text_")
    (->> (om-elem-parse-object-at 1)
         (om-elem-get-type))
    => 'underline
    (:buffer "=text=")
    (->> (om-elem-parse-object-at 1)
         (om-elem-get-type))
    => 'verbatim
    :end-hidden

    (:buffer "- notme")
    (:comment "Return nil when parsing an element")
    (om-elem-parse-object-at 1)
    => nil)

  (defexamples-content om-elem-parse-element-at
    nil
    (:buffer "#+CALL: ktulu()")
    (->> (om-elem-parse-element-at 1)
         (om-elem-get-type))
    => 'babel-call

    :begin-hidden
    (:buffer "#+BEGIN_CENTER"
             "#+END_CENTER")
    (->> (om-elem-parse-element-at 1)
         (om-elem-get-type))
    => 'center-block
    (:buffer "CLOCK: [2019-01-01 Tue]")
    (->> (om-elem-parse-element-at 1)
         (om-elem-get-type))
    => 'clock
    (:buffer "# oops I looked")
    (->> (om-elem-parse-element-at 1)
         (om-elem-get-type))
    => 'comment
    (:buffer "#+BEGIN_COMMENT"
             "oops I looked again"
             "#+END_COMMENT")
    (->> (om-elem-parse-element-at 1)
         (om-elem-get-type))
    => 'comment-block
    (:buffer "%%(diary of a madman)")
    (->> (om-elem-parse-element-at 1)
         (om-elem-get-type))
    => 'diary-sexp
    (:buffer ":DRAWER:"
             "- underwear"
             "- savings account"
             ":END:")
    (->> (om-elem-parse-element-at 1)
         (om-elem-get-type))
    => 'drawer
    (:buffer "#+BEGIN countdown"
             "#+END")
    (->> (om-elem-parse-element-at 1)
         (om-elem-get-type))
    => 'dynamic-block
    (:buffer "#+BEGIN_EXAMPLE"
             "#+END_EXAMPLE")
    (->> (om-elem-parse-element-at 1)
         (om-elem-get-type))
    => 'example-block
    (:buffer "#+BEGIN_EXPORT latex"
             "#+END_EXPORT")
    (->> (om-elem-parse-element-at 1)
         (om-elem-get-type))
    => 'export-block
    (:buffer ": mini mini mini")
    (->> (om-elem-parse-element-at 1)
         (om-elem-get-type))
    => 'fixed-width
    (:buffer "[fn:1]")
    (->> (om-elem-parse-element-at 1)
         (om-elem-get-type))
    => 'footnote-definition
    (:buffer "* murder, young girl killed"
             "* desperate shooting at echo's hill")
    (->> (om-elem-parse-element-at 1)
         (om-elem-get-type))
    => 'headline
    (:buffer "-----")
    (->> (om-elem-parse-element-at 1)
         (om-elem-get-type))
    => 'horizontal-rule
    ;; TODO add inlinetask
    (:buffer "#+QUOTE: unquote")
    (->> (om-elem-parse-element-at 1)
         (om-elem-get-type))
    => 'keyword
    ;; TODO add latex env
    (:buffer "* headline"
             ":PROPERTIES:"
             ":key: val"
             ":END:")
    (->> (om-elem-parse-element-at 25)
         (om-elem-get-type))
    => 'node-property
    (:buffer "Just for the record"
             "The weather today is slightly sarcastic with a good chance of"
             "A. Indifference and B. disinterest in what the critics say")
    (->> (om-elem-parse-element-at 1)
         (om-elem-get-type))
    => 'paragraph
    :end-hidden
    
    (:buffer "- plain-list")
    (:comment "Give the plain-list, not the item for this function")
    (->> (om-elem-parse-element-at 1)
         (om-elem-get-type))
    => 'plain-list

    :begin-hidden
    (:buffer "* deadhead"
             "DEADLINE: [2019-01-01 Tue]")
    (->> (om-elem-parse-element-at 12)
         (om-elem-get-type))
    => 'planning
    (:buffer "* headline"
             ":PROPERTIES:"
             ":END:")
    (->> (om-elem-parse-element-at 12)
         (om-elem-get-type))
    => 'property-drawer
    (:buffer "#+BEGIN_QUOTE"
             "Oh glorious cheeseburger, we bow to thee"
             "The secrets of the universe are between the buns"
             "#+END_QUOTE")
    (->> (om-elem-parse-element-at 12)
         (om-elem-get-type))
    => 'quote-block
    (:buffer "#+begin_dot dot.png"
             "#+end_dot")
    (->> (om-elem-parse-element-at 1)
         (om-elem-get-type))
    => 'special-block
    (:buffer "#+BEGIN_SRC emacs"
             "(launch-missiles)"
             "#+END_SRC")
    (->> (om-elem-parse-element-at 1)
         (om-elem-get-type))
    => 'src-block
    :end-hidden
    
    (:buffer "| R | A |"
             "| G | E |")
    (:comment "Return a table, not the table-row for this function")
    (->> (om-elem-parse-element-at 1)
         (om-elem-get-type))
    => 'table

    :begin-hidden
    (:buffer "#+BEGIN_VERSE"
             "#+END_VERSE")
    (->> (om-elem-parse-element-at 1)
         (om-elem-get-type))
    => 'verse-block
    :end-hidden)

  (defexamples-content om-elem-parse-headline-at
    nil
    (:buffer "* headline")
    (:comment "Return the headline itself")
    (->> (om-elem-parse-headline-at 1)
         (om-elem-to-trimmed-string))
    => "* headline"
    (:buffer "* headline"
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
    (:buffer "* headline"
             "section crap"
             "** not parsed")
    (:comment "Don't parse any subheadlines")
    (->> (om-elem-parse-headline-at 1)
         (om-elem-to-trimmed-string))
    => (:result "* headline"
                "section crap")
    (:buffer "nothing nowhere")
    (:comment "Return nil if not under a headline")
    (->> (om-elem-parse-headline-at 1)
         (om-elem-to-trimmed-string))
    => "")

  (defexamples-content om-elem-parse-subtree-at
    nil
    (:buffer "* headline")
    (:comment "Return the headline itself")
    (->> (om-elem-parse-subtree-at 1)
         (om-elem-to-trimmed-string))
    => "* headline"
    (:buffer "* headline"
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
    (:buffer "* headline"
             "section crap"
             "** parsed")
    (:comment "Return all the subheadlines")
    (->> (om-elem-parse-subtree-at 1)
         (om-elem-to-trimmed-string))
    => (:result "* headline"
                "section crap"
                "** parsed")
    (:buffer "nothing nowhere")
    (:comment "Return nil if not under a headline")
    (->> (om-elem-parse-subtree-at 1)
         (om-elem-to-trimmed-string))
    => "")

  (defexamples-content om-elem-parse-item-at
    nil
    (:buffer "- item")
    (:comment "Return the item itself")
    (->> (om-elem-parse-item-at 1)
         (om-elem-to-trimmed-string))
    => "- item"
    (:comment "Also return the item when not at beginning of line")
    (->> (om-elem-parse-item-at 5)
         (om-elem-to-trimmed-string))
    => "- item"
    (:buffer "- item"
             "  - item 2")
    (:comment "Return item and its subitems")
    (->> (om-elem-parse-item-at 1)
         (om-elem-to-trimmed-string))
    => (:result "- item"
                "  - item 2")
    (:buffer "* not item")
    (:comment "Return nil if not an item")
    (->> (om-elem-parse-item-at 1)
         (om-elem-to-trimmed-string))
    => "")

  (defexamples-content om-elem-parse-table-row-at
    nil
    (:buffer "| bow | stroke |")
    (:comment "Return the row itself")
    (->> (om-elem-parse-table-row-at 1)
         (om-elem-to-trimmed-string))
    => "| bow | stroke |"
    (:comment "Also return the row when not at beginning of line")
    (->> (om-elem-parse-table-row-at 5)
         (om-elem-to-trimmed-string))
    => "| bow | stroke |"
    (:buffer "- bow and arrow choke")
    (:comment "Return nil if not a table-row")
    (->> (om-elem-parse-table-row-at 1)
         (om-elem-to-trimmed-string))
    => "")
  
  (defexamples-content om-elem-parse-section-at
    nil
    (:buffer "over headline"
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
    (:buffer "* headline"
             "** subheadline")
    (:comment "Return nil if no section under headline")
    (->> (om-elem-parse-section-at 1)
         (om-elem-to-trimmed-string))
    => ""
    (:buffer "")
    (:comment "Return nil if no section at all")
    (->> (om-elem-parse-section-at 1)
         (om-elem-to-trimmed-string))
    => ""))

(def-example-group "String Conversion"
  "Convert nodes to strings."

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
    (om-elem-to-trimmed-string nil) => ""))

(def-example-group "Building"
  "Build new nodes."

  (def-example-subgroup "Leaf Objects"
    nil

    (defexamples om-elem-build-code
      (->> (om-elem-build-code "text")
           (om-elem-to-trimmed-string)) => "~text~")

    (defexamples om-elem-build-entity
      (->> (om-elem-build-entity "gamma")
           (om-elem-to-trimmed-string)) => "\\gamma")

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
      (->> (om-elem-build-timestamp 'inactive 2019 1 15 2019 1 15)
           (om-elem-to-trimmed-string))
      => "[2019-01-15 Tue]"
      (->> (om-elem-build-timestamp 'active-range 2019 1 15 2019 1 16)
           (om-elem-to-trimmed-string))
      => "<2019-01-15 Tue>--<2019-01-16 Wed>"
      (->> (om-elem-build-timestamp
            'inactive 2019 1 15 2019 1 15 :warning-type 'all
            :warning-unit 'day :warning-value 1)
           (om-elem-to-trimmed-string))
      => "[2019-01-15 Tue -1d]")

    (defexamples om-elem-build-verbatim
      (->> (om-elem-build-verbatim "text")
           (om-elem-to-trimmed-string)) => "=text="))

  (def-example-subgroup "Branch Objects"
    nil

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

  (def-example-subgroup "Leaf Elements"
    nil

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
      (->> (om-elem-build-clock (om-elem-build-timestamp! 'inactive '(2019 1 1 0 0)))
           (om-elem-to-trimmed-string))
      => "CLOCK: [2019-01-01 Tue 00:00]"
      (->> (om-elem-build-clock (om-elem-build-timestamp! 'inactive '(2019 1 1 0 0) :end '(2019 1 1 1 0)))
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
      (->> (om-elem-build-latex-environment '("env" "text"))
           (om-elem-to-trimmed-string)) => (:result "\\begin{env}"
           "text"
           "\\end{env}"))

    (defexamples om-elem-build-node-property
      (->> (om-elem-build-node-property "key" "val")
           (om-elem-to-trimmed-string)) => ":key:      val")

    (defexamples om-elem-build-planning
      (->> (om-elem-build-planning :closed (om-elem-build-timestamp! 'inactive '(2019 1 1)))
           (om-elem-to-trimmed-string)) => "CLOSED: [2019-01-01 Tue]"
      (->> (om-elem-build-planning :scheduled (om-elem-build-timestamp! 'inactive '(2019 1 1)))
           (om-elem-to-trimmed-string)) => "SCHEDULED: [2019-01-01 Tue]"
      (->> (om-elem-build-planning :deadline (om-elem-build-timestamp! 'inactive '(2019 1 1)))
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

  (def-example-subgroup "Branch Elements with Child Objects"
    nil

    (defexamples om-elem-build-paragraph
      (->> (om-elem-build-paragraph "text")
           (om-elem-to-trimmed-string))
      => "text")

    (defexamples om-elem-build-table-row
      (->> (om-elem-build-table-cell "a")
           (om-elem-build-table-row)
           (om-elem-to-trimmed-string))
      => "| a |")

    (defexamples om-elem-build-verse-block
      ;; TODO why should I need to use newlines here?
      (->> (om-elem-build-verse-block "text\n")
           (om-elem-to-trimmed-string))
      => (:result "#+BEGIN_VERSE"
                  "text"
                  "#+END_VERSE")))

  (def-example-subgroup "Branch Elements with Child Elements"
    nil

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
           (om-elem-to-trimmed-string)) => "| cell |")

    (def-example-subgroup "Miscellaneous Builders"
      nil

      (defexamples om-elem-build-timestamp-diary-sexp
        (->> (om-elem-build-timestamp-diary-sexp '(diary-float t 4 2))
             (om-elem-to-string))
        => "<%%(diary-float t 4 2)>")

      (defexamples om-elem-build-table-row-hline
        ;; TODO this example is pretty dumb
        (->> (om-elem-build-table-row-hline)
             (om-elem-to-trimmed-string))
        => "|-"))

    (def-example-subgroup "Shorthand Builders"
      "Build nodes with more convenient/shorter syntax."

      (defexamples om-elem-build-timestamp!
        (->> (om-elem-build-timestamp! 'inactive '(2019 1 1))
             (om-elem-to-string))
        => "[2019-01-01 Tue]"
        (->> (om-elem-build-timestamp! 'inactive '(2019 1 1 12 0)
                                       :warning '(all 1 day)
                                       :repeater '(cumulate 1 month))
             (om-elem-to-string))
        => "[2019-01-01 Tue 12:00 +1m -1d]"
        (->> (om-elem-build-timestamp! 'inactive '(2019 1 1)
                                       :end '(2019 1 2))
             (om-elem-to-string))
        => "[2019-01-01 Tue]--[2019-01-02 Wed]")

      (defexamples om-elem-build-clock!
        (->> (om-elem-build-clock! '(2019 1 1))
             (om-elem-to-trimmed-string))
        => "CLOCK: [2019-01-01 Tue]"
        (->> (om-elem-build-clock! '(2019 1 1 12 0))
             (om-elem-to-trimmed-string))
        => "CLOCK: [2019-01-01 Tue 12:00]"
        (->> (om-elem-build-clock! '(2019 1 1 12 0) :end '(2019 1 1 13 0))
             (om-elem-to-trimmed-string))
        ;; TODO why does this make two individual timestamps?
        => "CLOCK: [2019-01-01 Tue 12:00]--[2019-01-01 Tue 13:00] =>  1:00")

      (defexamples om-elem-build-planning!
        (->> (om-elem-build-planning! :closed '(2019 1 1))
             (om-elem-to-trimmed-string))
        => "CLOSED: [2019-01-01 Tue]"
        (->> (om-elem-build-planning! :closed '(2019 1 1)
                                      :scheduled '(2018 1 1))
             (om-elem-to-trimmed-string))
        => "SCHEDULED: [2018-01-01 Mon] CLOSED: [2019-01-01 Tue]"
        (->> (om-elem-build-planning! :closed '(2019 1 1 &warning all 1 day &repeater cumulate 1 month))
             (om-elem-to-trimmed-string))
        => "CLOSED: [2019-01-01 Tue +1m -1d]")

      (defexamples om-elem-build-property-drawer!
        (->> (om-elem-build-property-drawer! '(key val))
             (om-elem-to-trimmed-string))
        => (:result ":PROPERTIES:"
                    ":key:      val"
                    ":END:"))

      (defexamples om-elem-build-headline!
        (->> (om-elem-build-headline! :title-text "really impressive title")
             (om-elem-to-trimmed-string))
        => "* really impressive title"
        (->> (om-elem-build-headline! :title-text "really impressive title"
                                      :statistics-cookie '(0 9000))
             (om-elem-to-trimmed-string))
        => "* really impressive title [0/9000]"
        (->> (om-elem-build-headline!
              :title-text "really impressive title"
              :properties '((key val))
              :section-children (list (om-elem-build-paragraph! "section text"))
              ;; TODO make levels make sense
              (om-elem-build-headline! :level 2 :title-text "subhead"))
             (om-elem-to-trimmed-string))
        => (:result "* really impressive title"
                    ":PROPERTIES:"
                    ":key:      val"
                    ":END:"
                    "section text"
                    "** subhead")
        )

      (defexamples om-elem-build-item!
        (->> (om-elem-build-item!
              ;; TODO (x) should make x)
              :bullet '(1)
              :tag "complicated *tag*"
              :paragraph "petulant /frenzy/"
              (om-elem-build-item! :bullet '- :paragraph "below"))
             (om-elem-to-trimmed-string))
        => (:result "1. complicated *tag* :: petulant /frenzy/"
                    "   - below"))

      (defexamples om-elem-build-paragraph!
        (->> (om-elem-build-paragraph! "stuff /with/ *formatting*" :post-blank 2)
             (om-elem-to-string))
        => (:result "stuff /with/ *formatting*"
                    ""
                    ""
                    "")
        (->> (om-elem-build-paragraph! "* stuff /with/ *formatting*")
             (om-elem-to-string))
        !!> error)

      (defexamples om-elem-build-table!
        (->> (om-elem-build-table! '("R" "A") '("G" "E"))
             (om-elem-to-trimmed-string))
        => (:result "| R | A |"
                    "| G | E |")
        (->> (om-elem-build-table! '("L" "O") 'hline '("V" "E"))
             (om-elem-to-trimmed-string))
        => (:result "| L | O |"
                    "|---+---|"
                    "| V | E |")))))

(def-example-group "Type Predicates"
  "Test node types."

  (defexamples-content om-elem-is-type-p
    nil
    (:buffer "*ziltoid*")
    (->> (om-elem-parse-this-object)
         (om-elem-is-type-p 'bold))
    => t
    (->> (om-elem-parse-this-object)
         (om-elem-is-type-p 'italic))
    => nil)

  (defexamples-content om-elem-is-any-type-p
    nil
    (:buffer "*ziltoid*")
    (->> (om-elem-parse-this-object)
         (om-elem-is-any-type-p '(bold)))
    => t
    (->> (om-elem-parse-this-object)
         (om-elem-is-any-type-p '(bold italic)))
    => t
    (->> (om-elem-parse-this-object)
         (om-elem-is-any-type-p '(italic)))
    => nil)

  (defexamples-content om-elem-is-element-p
    nil
    (:buffer "*ziltoid*")
    (:comment "Parsing this text as an element gives a paragraph")
    (->> (om-elem-parse-this-element)
         (om-elem-is-element-p))
    => t
    (:comment "Parsing the same text as an object gives a bold object")
    (->> (om-elem-parse-this-object)
         (om-elem-is-element-p))
    => nil)

  (defexamples-content om-elem-is-branch-node-p
    nil
    (:buffer "*ziltoid*")
    (:comment "Parsing this as an element gives a paragraph type (an object container).")
    (->> (om-elem-parse-this-element)
         (om-elem-is-branch-node-p))
    => t
    (:comment "Parsing this as an object gives a bold type (also an object container).")
    (->> (om-elem-parse-this-object)
         (om-elem-is-branch-node-p))
    => t
    (:buffer "~ziltoid~")
    (:comment "Parsing this as an object gives a code type (not a container).")
    (->> (om-elem-parse-this-object)
         (om-elem-is-branch-node-p))
    => nil
    (:buffer "# ziltoid")
    (:comment "Parsing this as an element gives a comment type (not a container).")
    (->> (om-elem-parse-this-element)
         (om-elem-is-branch-node-p))
    => nil
    (:buffer "* I'm so great")
    (:comment "Parsing this as an element gives a table (a greater element).")
    (->> (om-elem-parse-this-element)
         (om-elem-is-branch-node-p))
    => t)

  (defexamples-content om-elem-is-branch-node-with-child-objects-p
    nil
    (:buffer "*ziltoid*")
    (:comment "Parsing this as an element gives a paragraph type (an object container).")
    (->> (om-elem-parse-this-element)
         (om-elem-is-branch-node-with-child-objects-p))
    => t
    (:comment "Parsing this as an object gives a bold type (also an object container).")
    (->> (om-elem-parse-this-object)
         (om-elem-is-branch-node-with-child-objects-p))
    => t
    (:buffer "~ziltoid~")
    (:comment "Parsing this as an object gives a code type (not a container).")
    (->> (om-elem-parse-this-object)
         (om-elem-is-branch-node-with-child-objects-p))
    => nil
    (:buffer "# ziltoid")
    (:comment "Parsing this as an element gives a comment type (not a container).")
    (->> (om-elem-parse-this-element)
         (om-elem-is-branch-node-with-child-objects-p))
    => nil
    (:buffer "* I'm so great")
    (:comment "Parsing this as an element gives a table (a greater element).")
    (->> (om-elem-parse-this-element)
         (om-elem-is-branch-node-with-child-objects-p))
    => nil)

  (defexamples-content om-elem-is-branch-element-with-child-elements-p
    nil
    (:buffer "* I'm so great")
    (:comment "Parsing this as an element gives a table (a greater element).")
    (->> (om-elem-parse-this-element)
         (om-elem-is-branch-element-with-child-elements-p))
    => t
    (:buffer "*ziltoid*")
    (:comment "Parsing this as an element gives a paragraph type (not a greater element).")
    (->> (om-elem-parse-this-element)
         (om-elem-is-branch-element-with-child-elements-p))
    => nil
    (:buffer "# ziltoid")
    (:comment "Parsing this as an element gives a comment type (not a container).")
    (->> (om-elem-parse-this-element)
         (om-elem-is-branch-element-with-child-elements-p))
    => nil))

(def-example-group "Property Manipulation"
  "Set, get, and map properties of nodes."

  (def-example-subgroup "Generic"
    nil

    (defexamples-content om-elem-set-property
      "Set property PROP to VALUE in ELEM."

      (:buffer "#+CALL: ktulu()")
      (->> (om-elem-parse-this-element)
           (om-elem-set-property :call "cthulhu")
           (om-elem-set-property :inside-header '(:cache no))
           (om-elem-set-property :arguments '("x=4"))
           (om-elem-set-property :end-header '(:exports results))
           (om-elem-to-trimmed-string))
      => "#+CALL: cthulhu[:cache no](x=4) :exports results"

      :begin-hidden
      (:buffer "CLOCK: [2019-01-01 Tue]")
      (->> (om-elem-parse-this-element)
           (om-elem-set-property
            :value (om-elem-build-timestamp!
                    'inactive '(2019 1 1) :end '(2019 1 2)))
           (om-elem-to-trimmed-string))
      => "CLOCK: [2019-01-01 Tue]--[2019-01-02 Wed] => 24:00"

      (:buffer "~learn to~")
      (->> (om-elem-parse-this-object)
           (om-elem-set-property :value "why?")
           (om-elem-to-trimmed-string))
      => "~why?~"

      (:buffer "# not here")
      (->> (om-elem-parse-this-element)
           (om-elem-set-property :value "still not here")
           (om-elem-to-trimmed-string))
      => "# still not here"

      (:buffer "#+BEGIN_COMMENT"
               "not here"
               "#+END_COMMENT")
      (->> (om-elem-parse-this-element)
           (om-elem-set-property :value "still not here")
           (om-elem-to-trimmed-string))
      => (:result "#+BEGIN_COMMENT"
                  "still not here"
                  "#+END_COMMENT")

      ;; TODO add diary-sexp

      (:buffer ":LOGBOOK:"
               ":END:")
      (->> (om-elem-parse-this-element)
           (om-elem-set-property :drawer-name "BOOKOFSOULS")
           (om-elem-to-trimmed-string))
      => (:result ":BOOKOFSOULS:"
                  ":END:")

      (:buffer "#+BEGIN: blockhead"
               "#+END:")
      (->> (om-elem-parse-this-element)
           (om-elem-set-property :block-name "blockfoot")
           (om-elem-set-property :arguments '(:cache no))
           (om-elem-to-trimmed-string))
      => (:result "#+BEGIN: blockfoot :cache no"
                  "#+END:")

      (:buffer "\\pi")
      (->> (om-elem-parse-this-object)
           (om-elem-set-property :name "gamma")
           (om-elem-set-property :use-brackets-p t)
           (om-elem-to-trimmed-string))
      => "\\gamma{}"

      ;; TODO test preserve indentation...
      (:buffer "#+BEGIN_EXAMPLE"
               "#+END_EXAMPLE")
      (->> (om-elem-parse-this-element)
           (om-elem-set-property :switches '("-n"))
           (om-elem-set-property :value "example.com")
           (om-elem-to-trimmed-string))
      => (:buffer "#+BEGIN_EXAMPLE -n"
                  "example.com"
                  "#+END_EXAMPLE")

      (:buffer "#+BEGIN_EXPORT latex"
               "#+END_EXPORT")
      (->> (om-elem-parse-this-element)
           (om-elem-set-property :type "domestic")
           (om-elem-set-property :value "bullets, bombs, and bigotry")
           (om-elem-to-trimmed-string))
      => (:buffer "#+BEGIN_EXPORT domestic"
                  "bullets, bombs, and bigotry"
                  "#+END_EXPORT")

      (:buffer "@@back-end:value@@")
      (->> (om-elem-parse-this-object)
           (om-elem-set-property :back-end "latex")
           (om-elem-set-property :value "new-value")
           (om-elem-to-trimmed-string))
      => "@@latex:new-value@@"

      (:buffer ": fixed")
      (->> (om-elem-parse-this-element)
           (om-elem-set-property :value "unfixed")
           (om-elem-to-trimmed-string))
      => ": unfixed"

      (:buffer "[fn:whitelabel] society")
      (->> (om-elem-parse-this-element)
           (om-elem-set-property :label "blacklabel")
           (om-elem-to-trimmed-string))
      => "[fn:blacklabel] society"

      ;; TODO test footnote section
      (:buffer "* dummy"
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

      (:buffer "call_kthulu()")
      (->> (om-elem-parse-this-object)
           (om-elem-set-property :call "cthulhu")
           (om-elem-set-property :inside-header '(:cache no))
           (om-elem-set-property :arguments '("x=4"))
           (om-elem-set-property :end-header '(:exports results))
           (om-elem-to-trimmed-string))
      => "call_cthulhu[:cache no](x=4)[:exports results]"

      (:buffer "src_emacs{(print 'yeah-boi)}")
      (->> (om-elem-parse-this-object)
           (om-elem-set-property :language "python")
           (om-elem-set-property :parameters '(:cache no))
           (om-elem-set-property :value "print \"yeah boi\"")
           (om-elem-to-trimmed-string))
      => "src_python[:cache no]{print \"yeah boi\"}"
      :end-hidden

      (:buffer "- thing")
      (->> (om-elem-parse-this-item)
           (om-elem-set-property :bullet 1)
           (om-elem-set-property :checkbox 'on)
           (om-elem-set-property :counter 2)
           (om-elem-set-property :tag '("tmsu"))
           (om-elem-to-trimmed-string))
      => "1. [@2] [X] tmsu :: thing"

      :begin-hidden
      (:buffer "#+KEY: VAL")
      (->> (om-elem-parse-this-element)
           (om-elem-set-property :key "kee")
           (om-elem-set-property :value "vahl")
           (om-elem-to-trimmed-string))
      => "#+kee: vahl"

      ;; this is stupid, who would ever do this?
      (:buffer "\begin{env}"
               "body"
               "\end{env}")
      (->> (om-elem-parse-this-element)
           (om-elem-set-property :value "\begin{vne}\nbody\end{vne}")
           (om-elem-to-trimmed-string))
      => (:buffer "\begin{vne}"
                  "body"
                  "\end{vne}")

      ;; TODO this is also stupid...
      (:buffer "$2+2=4$")
      (->> (om-elem-parse-this-object)
           (om-elem-set-property :value "$2+2=5$")
           (om-elem-to-trimmed-string))
      => "$2+2=5$"

      (:buffer "https://example.com")
      (->> (om-elem-parse-this-object)
           (om-elem-set-property :path "/dev/null")
           (om-elem-set-property :type "file")
           (om-elem-set-property :format 'bracket)
           (om-elem-to-trimmed-string))
      => "[[file:/dev/null]]"

      (:buffer "{{{economics}}}")
      (->> (om-elem-parse-this-object)
           (om-elem-set-property :key "freakonomics")
           (om-elem-set-property :args '("x=4" "y=2"))
           (om-elem-to-trimmed-string))
      => "{{{freakonomics(x=4,y=2)}}}"

      (:buffer "* dummy"
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

      (:buffer "* dummy"
               "CLOSED: [2019-01-01 Tue]")
      ;; TODO need public function
      (->> (om-elem-parse-this-headline)
           (om-elem--headline-get-planning)
           (om-elem-set-property
            :closed (om-elem-build-timestamp! 'inactive '(2019 1 2)))
           (om-elem-to-trimmed-string))
      => "CLOSED: [2019-01-02 Wed]"

      (:buffer "#+BEGIN_special"
               "#+END_special")
      (->> (om-elem-parse-this-element)
           (om-elem-set-property :type "talent")
           (om-elem-to-trimmed-string))
      => (:result "#+BEGIN_talent"
                  "#+END_talent")

      (:buffer "#+BEGIN_SRC"
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

      (:buffer "* dummy [50%]")
      (->> (om-elem-parse-this-headline)
           (om-elem--headline-get-statistics-cookie)
           (om-elem-set-property :value '(0 5))
           (om-elem-to-trimmed-string))
      => "[0/5]"

      (:buffer "sub_woofer")
      (->> (om-elem-parse-object-at 5)
           (om-elem-set-property :use-brackets-p t)
           (om-elem-to-trimmed-string))
      => "_{woofer}"

      (:buffer "super^woofer")
      (->> (om-elem-parse-object-at 7)
           (om-elem-set-property :use-brackets-p t)
           (om-elem-to-trimmed-string))
      => "^{woofer}"

      (:buffer "| a |")
      (->> (om-elem-parse-this-element)
           (om-elem-set-property :tblfm '("x=$2"))
           (om-elem-to-trimmed-string))
      => (:result "| a |"
                  "#+TBLFM: x=$2")

      (:buffer "<<found>>")
      (->> (om-elem-parse-this-object)
           (om-elem-set-property :value "lost")
           (om-elem-to-trimmed-string))
      => "<<lost>>"

      (:buffer "[2019-01-01 Tue]")
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

      (:buffer "=I am not a crook=")
      (->> (om-elem-parse-this-object)
           (om-elem-set-property :value "You totally are")
           (om-elem-to-trimmed-string))
      => "=You totally are="
      :end-hidden

      ;; TODO add post-blank

      (:buffer "* not valuable")
      (:comment "Throw error when setting a property that doesn't exist")
      (->> (om-elem-parse-this-headline)
           (om-elem-set-property :value "wtf")
           (om-elem-to-trimmed-string))
      !!> error)

    (defexamples-content om-elem-set-properties
      nil
      
      (:buffer "- thing")
      (->> (om-elem-parse-this-item)
           (om-elem-set-properties (list :bullet 1
                                         :checkbox 'on
                                         :counter 2
                                         :tag '("tmsu")))
           (om-elem-to-trimmed-string))
      => "1. [@2] [X] tmsu :: thing")

    (defexamples-content om-elem-get-property
      nil

      (:buffer "#+CALL: ktulu[:cache no](x=4) :exports results")
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
      (:buffer "CLOCK: [2019-01-01 Tue]")
      (->> (om-elem-parse-this-element)
           (om-elem-get-property :value)
           (om-elem-to-string))
      => "[2019-01-01 Tue]"

      (:buffer "~learn to~")
      (->> (om-elem-parse-this-object)
           (om-elem-get-property :value))
      => "learn to"

      (:buffer "# not here")
      (->> (om-elem-parse-this-element)
           (om-elem-get-property :value))
      => "not here"

      (:buffer "#+BEGIN_COMMENT"
               "not here"
               "#+END_COMMENT")
      (->> (om-elem-parse-this-element)
           (om-elem-get-property :value))
      => "not here"

      ;; TODO add diary-sexp

      (:buffer ":LOGBOOK:"
               ":END:")
      (->> (om-elem-parse-this-element)
           (om-elem-get-property :drawer-name))
      => "LOGBOOK"

      (:buffer "#+BEGIN: blockhead :cache no"
               "#+END:")
      (->> (om-elem-parse-this-element)
           (om-elem-get-property :block-name))
      => "blockhead"
      (->> (om-elem-parse-this-element)
           (om-elem-get-property :arguments))
      => '(:cache no)

      (:buffer "\\pi{}")
      (->> (om-elem-parse-this-object)
           (om-elem-get-property :name))
      => "pi"
      (->> (om-elem-parse-this-object)
           (om-elem-get-property :use-brackets-p))
      => t

      ;; TODO test preserve indentation...
      => (:buffer "#+BEGIN_EXAMPLE -n"
                  "example.com"
                  "#+END_EXAMPLE")
      (->> (om-elem-parse-this-element)
           (om-elem-get-property :switches))
      => '("-n")
      (->> (om-elem-parse-this-element)
           (om-elem-get-property :value))
      => "example.com"

      (:buffer "#+BEGIN_EXPORT domestic"
               "bullets, bombs, and bigotry"
               "#+END_EXPORT")
      (->> (om-elem-parse-this-element)
           (om-elem-get-property :type))
      ;; TODO why capitalized?
      => "DOMESTIC"
      (->> (om-elem-parse-this-element)
           (om-elem-get-property :value))
      => "bullets, bombs, and bigotry\n"

      (:buffer "@@back-end:value@@")
      (->> (om-elem-parse-this-object)
           (om-elem-get-property :back-end))
      => "back-end"
      (->> (om-elem-parse-this-object)
           (om-elem-get-property :value))
      => "value"

      (:buffer ": fixed")
      (->> (om-elem-parse-this-element)
           (om-elem-get-property :value))
      => "fixed"

      (:buffer "[fn:blacklabel] society")
      (->> (om-elem-parse-this-element)
           (om-elem-get-property :label))
      => "blacklabel"

      ;; TODO test footnote section
      ;; TODO the priority should be parsable after "COMMENT"
      (:buffer "** TODO [#A] COMMENT dummy                                   :tmsu:ARCHIVE:"
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

      (:buffer "call_ktulu[:cache no](x=4)[:exports results]")
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

      (:buffer "src_python[:cache no]{print \"yeah boi\"}")
      (->> (om-elem-parse-this-object)
           (om-elem-get-property :language))
      => "python"
      (->> (om-elem-parse-this-object)
           (om-elem-get-property :parameters))
      => '(:cache no)
      (->> (om-elem-parse-this-object)
           (om-elem-get-property :value))
      => "print \"yeah boi\""

      (:buffer "- [@2] [X] tmsu :: thing")
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

      (:buffer "#+KEY: VAL")
      (->> (om-elem-parse-this-element)
           (om-elem-get-property :key))
      => "KEY"
      (->> (om-elem-parse-this-element)
           (om-elem-get-property :value))
      => "VAL"

      ;; this is stupid, who would ever do this?
      (:buffer "\begin{env}"
               "body"
               "\end{env}")
      (->> (om-elem-parse-this-element)
           (om-elem-get-property :value))
      => (:buffer "\begin{env}"
                  "body"
                  "\end{env}")

      ;; TODO this is also stupid...
      (:buffer "$2+2=4$")
      (->> (om-elem-parse-this-object)
           (om-elem-get-property :value))
      => "$2+2=4$"
      :end-hidden

      (:buffer "[[file:/dev/null]]")
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
      (:buffer "{{{economics(x=4,y=2)}}}")
      (->> (om-elem-parse-this-object)
           (om-elem-get-property :key))
      => "economics"
      (->> (om-elem-parse-this-object)
           (om-elem-get-property :args))
      => '("x=4" "y=2")

      (:buffer "* dummy"
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

      (:buffer "* dummy"
               "CLOSED: [2019-01-01 Tue]")
      ;; TODO need public function
      (->> (om-elem-parse-this-headline)
           (om-elem--headline-get-planning)
           (om-elem-get-property :closed)
           (om-elem-to-string))
      => "[2019-01-01 Tue]"

      (:buffer "#+BEGIN_special"
               "#+END_special")
      (->> (om-elem-parse-this-element)
           (om-elem-get-property :type))
      => "special"

      (:buffer "#+BEGIN_SRC emacs -n :cache no"
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

      (:buffer "* dummy [50%]")
      (->> (om-elem-parse-this-headline)
           (om-elem--headline-get-statistics-cookie)
           (om-elem-get-property :value))
      => '(50)

      (:buffer "sub_{woofer}")
      (->> (om-elem-parse-object-at 6)
           (om-elem-get-property :use-brackets-p))
      => 6

      (:buffer "super_{woofer}")
      (->> (om-elem-parse-object-at 8)
           (om-elem-get-property :use-brackets-p))
      => 8

      (:buffer "| a |"
               "#+TBLFM: x=$2")
      (->> (om-elem-parse-this-element)
           (om-elem-get-property :tblfm))
      => '("x=$2")

      (:buffer "<<found>>")
      (->> (om-elem-parse-this-object)
           (om-elem-get-property :value))
      => "found"

      (:buffer "<2020-02-02 Sun 12:00 +1d -1d>--<2020-02-03 Mon 12:00 +1d -1d>")
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

      (:buffer "=I am not a crook=")
      (->> (om-elem-parse-this-object)
           (om-elem-get-property :value))
      => "I am not a crook"
      :end-hidden

      ;; TODO add post-blank

      (:buffer "* not arguable")
      (:comment "Throw error when requesting a property that doesn't exist")
      (->> (om-elem-parse-this-headline)
           (om-elem-get-property :value))
      !!> error)

    (defexamples-content om-elem-map-property
      nil

      (:buffer "#+CALL: ktulu()")
      (->> (om-elem-parse-this-element)
           (om-elem-map-property :call #'s-upcase)
           (om-elem-to-trimmed-string))
      => "#+CALL: KTULU()"

      :begin-hidden

      ;; TODO add clock

      (:buffer "~learn to~")
      (->> (om-elem-parse-this-object)
           (om-elem-map-property :value #'s-upcase)
           (om-elem-to-trimmed-string))
      => "~LEARN TO~"

      (:buffer "# not here")
      (->> (om-elem-parse-this-element)
           (om-elem-map-property :value #'s-upcase)
           (om-elem-to-trimmed-string))
      => "# NOT HERE"

      (:buffer "#+BEGIN_COMMENT"
               "not here"
               "#+END_COMMENT")
      (->> (om-elem-parse-this-element)
           (om-elem-map-property :value #'s-upcase)
           (om-elem-to-trimmed-string))
      => (:result "#+BEGIN_COMMENT"
                  "NOT HERE"
                  "#+END_COMMENT")

      ;; TODO add diary-sexp

      (:buffer ":LOGBOOK:"
               ":END:")
      (->> (om-elem-parse-this-element)
           (om-elem-map-property :drawer-name #'s-capitalize)
           (om-elem-to-trimmed-string))
      => (:result ":Logbook:"
                  ":END:")

      (:buffer "#+BEGIN: blockhead"
               "#+END:")
      (->> (om-elem-parse-this-element)
           (om-elem-map-property :block-name #'s-upcase)
           (om-elem-to-trimmed-string))
      => (:result "#+BEGIN: BLOCKHEAD"
                  "#+END:")

      :end-hidden

      ;; TODO add entity

      (:buffer "#+BEGIN_EXAMPLE"
               "example.com"
               "#+END_EXAMPLE")
      (->> (om-elem-parse-this-element)
           (om-elem-map-property* :value (concat "https://" it))
           (om-elem-to-trimmed-string))
      => (:result "#+BEGIN_EXAMPLE"
                  "https://example.com"
                  "#+END_EXAMPLE")

      :begin-hidden

      (:buffer "#+BEGIN_EXPORT domestic"
               "bullets, bombs, and bigotry"
               "#+END_EXPORT")
      (->> (om-elem-parse-this-element)
           (om-elem-map-property :type #'s-upcase)
           (om-elem-map-property :value #'s-upcase)
           (om-elem-to-trimmed-string))
      => (:result "#+BEGIN_EXPORT DOMESTIC"
                  "BULLETS, BOMBS, AND BIGOTRY"
                  "#+END_EXPORT")

      (:buffer "@@back-end:value@@")
      (->> (om-elem-parse-this-object)
           (om-elem-map-property :back-end #'s-upcase)
           (om-elem-map-property :value #'s-upcase)
           (om-elem-to-trimmed-string))
      => "@@BACK-END:VALUE@@"

      (:buffer ": fixed")
      (->> (om-elem-parse-this-element)
           (om-elem-map-property :value #'s-upcase)
           (om-elem-to-trimmed-string))
      => ": FIXED"

      (:buffer "[fn:blacklabel] society")
      (->> (om-elem-parse-this-element)
           (om-elem-map-property :label #'s-upcase)
           (om-elem-to-trimmed-string))
      => "[fn:BLACKLABEL] society"

      ;; TODO add example for headline

      (:buffer "call_ktulu()")
      (->> (om-elem-parse-this-object)
           (om-elem-map-property :call #'s-upcase)
           (om-elem-to-trimmed-string))
      => "call_KTULU()"

      ;; TODO add example for inline src block

      (:buffer "- tag :: thing")
      (->> (om-elem-parse-this-item)
           (om-elem-map-property :tag (lambda (it) (-map #'s-upcase it)))
           (om-elem-to-trimmed-string))
      => "- TAG :: thing"

      (:buffer "#+KEY: VAL")
      (->> (om-elem-parse-this-element)
           (om-elem-map-property :key (-partial #'s-prepend "OM_"))
           (om-elem-map-property :value (-partial #'s-prepend "OM_"))
           (om-elem-to-trimmed-string))
      => "#+OM_KEY: OM_VAL"

      ;; TODO add examples for latex frag/env

      ;; TODO add example for link

      (:buffer "{{{economics}}}")
      (->> (om-elem-parse-this-object)
           (om-elem-map-property :key #'s-upcase)
           (om-elem-to-trimmed-string))
      => "{{{ECONOMICS}}}"

      (:buffer "* dummy"
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

      (:buffer "#+BEGIN_special"
               "#+END_special")
      (->> (om-elem-parse-this-element)
           (om-elem-map-property :type #'s-upcase)
           (om-elem-to-trimmed-string))
      => (:result "#+BEGIN_SPECIAL"
                  "#+END_SPECIAL")

      ;; TODO add example for src block

      ;; TODO add example for statistics cookie

      (:buffer "<<found>>")
      (->> (om-elem-parse-this-object)
           (om-elem-map-property :value #'s-upcase)
           (om-elem-to-trimmed-string))
      => "<<FOUND>>"

      (:buffer "=I am not a crook=")
      (->> (om-elem-parse-this-object)
           (om-elem-map-property :value #'s-upcase)
           (om-elem-to-trimmed-string))
      => "=I AM NOT A CROOK="
      :end-hidden

      (:buffer "~code~")
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

    (defexamples-content om-elem-map-properties
      nil

      (:buffer "#+KEY: VAL")
      (->> (om-elem-parse-this-element)
           (om-elem-map-properties
            (list :key (-partial #'s-prepend "OM_")
                  :value (-partial #'s-prepend "OM_")))
           (om-elem-to-trimmed-string))
      => "#+OM_KEY: OM_VAL"

      ;; TODO spice this up...
      ;; TODO this makes the document parser puke for some reason
      ;; (->> (om-elem-parse-this-element)
      ;;      (om-elem-map-properties*
      ;;       (:key (s-prepend "OM_" it) :value (s-prepend "OM_" it)))
      ;;      (om-elem-to-trimmed-string))
      ;; => "#+OM_KEY: OM_VAL"
      )

    (defexamples-content om-elem-toggle-property
      nil

      (:buffer "\\pi")
      (->> (om-elem-parse-this-object)
           (om-elem-toggle-property :use-brackets-p)
           (om-elem-to-trimmed-string))
      => "\\pi{}"

      ;; TODO test src/example block preserve indent
      
      (:buffer "* headline")
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

      (:buffer "sub_woofer")
      (->> (om-elem-parse-object-at 5)
           (om-elem-toggle-property :use-brackets-p)
           (om-elem-to-trimmed-string))
      => "_{woofer}"

      (:buffer "super^woofer")
      (->> (om-elem-parse-object-at 7)
           (om-elem-toggle-property :use-brackets-p)
           (om-elem-to-trimmed-string))
      => "^{woofer}"

      :end-hidden

      (:buffer "- [ ] nope")
      (:comment "Throw an error when trying to toggle a non-boolean property")
      (->> (om-elem-parse-this-item)
           (om-elem-toggle-property :checkbox)
           (om-elem-to-trimmed-string))
      !!> error)

    (defexamples-content om-elem-shift-property
      ;; TODO need to ensure that the min/max priorities are always the same
      nil

      (:buffer "* no priorities")
      (:comment "Do nothing if there is nothing to shift.")
      (->> (om-elem-parse-this-headline)
           (om-elem-shift-property :priority 1)
           (om-elem-to-trimmed-string))
      => "* no priorities"

      (:buffer "* [#A] priorities")
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

      (:buffer "* TODO or not todo")
      (:comment "Throw error when shifting an unshiftable property")
      (->> (om-elem-parse-this-headline)
           (om-elem-shift-property :todo-keyword 1)
           (om-elem-to-string))
      !!> error

      :begin-hidden

      (:buffer "*bold*")
      (->> (om-elem-parse-this-object)
           (om-elem-shift-property :post-blank 1)
           (om-elem-to-string))
      => "*bold* "
      (->> (om-elem-parse-this-object)
           (om-elem-shift-property :post-blank -1)
           (om-elem-to-string))
      => "*bold*"

      (:buffer "1. thing")
      (->> (om-elem-parse-this-item)
           (om-elem-shift-property :counter 1)
           (om-elem-to-trimmed-string))
      => "1. thing"

      (:buffer "1. [@1] thing")
      (->> (om-elem-parse-this-item)
           (om-elem-shift-property :counter 1)
           (om-elem-to-trimmed-string))
      => "1. [@2] thing"
      (->> (om-elem-parse-this-item)
           (om-elem-shift-property :counter -1)
           (om-elem-to-trimmed-string))
      => "1. [@1] thing"

      (:buffer "* noob level")
      (->> (om-elem-parse-this-headline)
           (om-elem-shift-property :level 1)
           (om-elem-to-trimmed-string))
      => "** noob level"

      (:comment "Do nothing when final value is less than one.")
      (->> (om-elem-parse-this-headline)
           (om-elem-shift-property :level -1)
           (om-elem-to-trimmed-string))
      => "* noob level"

      (:buffer "* headline"
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

      (:buffer "#+CALL: ktulu(y=1)")
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

      (:buffer "* headline       :tag1:")
      (->> (om-elem-parse-this-headline)
           (om-elem-insert-into-property :tags 0 "tag0")
           (om-elem-to-trimmed-string))
      => "* headline                                                        :tag0:tag1:"

      :begin-hidden

      (:buffer "#+BEGIN_EXAMPLE -n"
               "#+END_EXAMPLE")
      (->> (om-elem-parse-this-element)
           (om-elem-insert-into-property :switches -1 "-r")
           (om-elem-to-trimmed-string))
      => (:result "#+BEGIN_EXAMPLE -n -r"
                  "#+END_EXAMPLE")


      (:buffer "call_ktulu(y=1)")
      (->> (om-elem-parse-this-object)
           (om-elem-insert-into-property :arguments 0 "x=4")
           (om-elem-to-trimmed-string))
      => "call_ktulu(x=4,y=1)"

      (:buffer "{{{economics(x=4)}}}")
      (->> (om-elem-parse-this-object)
           (om-elem-insert-into-property :args 0 "z=2")
           (om-elem-to-trimmed-string))
      => "{{{economics(z=2,x=4)}}}"
      
      (:buffer "#+BEGIN_SRC emacs-lisp -n"
               "#+END_SRC")
      (->> (om-elem-parse-this-element)
           (om-elem-insert-into-property :switches -1 "-r")
           (om-elem-to-trimmed-string))
      => (:result "#+BEGIN_SRC emacs-lisp -n -r"
                  "#+END_SRC")

      (:buffer "| a |"
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

      (:buffer "#+CALL: ktulu(y=1)")
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

      (:buffer "* headline       :tag1:")
      (->> (om-elem-parse-this-headline)
           (om-elem-remove-from-property :tags "tag1")
           (om-elem-to-trimmed-string))
      => "* headline"

      :begin-hidden

      (:buffer "#+BEGIN_EXAMPLE -n"
               "#+END_EXAMPLE")
      (->> (om-elem-parse-this-element)
           (om-elem-remove-from-property :switches "-n")
           (om-elem-to-trimmed-string))
      => (:result "#+BEGIN_EXAMPLE"
                  "#+END_EXAMPLE")

      (:buffer "call_ktulu(y=1)")
      (->> (om-elem-parse-this-object)
           (om-elem-remove-from-property :arguments "y=1")
           (om-elem-to-trimmed-string))
      => "call_ktulu()"

      (:buffer "{{{economics(x=4)}}}")
      (->> (om-elem-parse-this-object)
           (om-elem-remove-from-property :args "x=4")
           (om-elem-to-trimmed-string))
      => "{{{economics}}}"
      
      (:buffer "#+BEGIN_SRC emacs-lisp -n"
               "#+END_SRC")
      (->> (om-elem-parse-this-element)
           (om-elem-remove-from-property :switches "-n")
           (om-elem-to-trimmed-string))
      => (:result "#+BEGIN_SRC emacs-lisp"
                  "#+END_SRC")

      (:buffer "| a |"
               "#+TBLFM: x=$2")
      (->> (om-elem-parse-this-element)
           (om-elem-remove-from-property :tblfm "x=$2")
           (om-elem-to-trimmed-string))
      => "| a |"
      :end-header)

    (defexamples-content om-elem-plist-put-property
      nil

      (:buffer "#+CALL: ktulu[:cache no]()")
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

      (:buffer "#+BEGIN: blockhead :format \"[%s]\""
               "#+END:")
      (->> (om-elem-parse-this-element)
           (om-elem-plist-put-property :arguments :format "<%s>")
           (om-elem-to-trimmed-string))
      => (:result "#+BEGIN: blockhead :format \"<%s>\""
                  "#+END:")

      (:buffer "call_ktulu[:cache no]()")
      (->> (om-elem-parse-this-object)
           (om-elem-plist-put-property :inside-header :cache 'yes)
           (om-elem-plist-put-property :end-header :results 'html)
           (om-elem-to-trimmed-string))
      => "call_ktulu[:cache yes]()[:results html]"

      (:buffer "src_emacs-lisp[:exports results]{}")
      (->> (om-elem-parse-this-object)
           (om-elem-plist-put-property :parameters :exports 'both)
           (om-elem-to-trimmed-string))
      => "src_emacs-lisp[:exports both]{}"

      (:buffer "#+BEGIN_SRC emacs-lisp -n :exports results"
               "#+END_SRC")
      (->> (om-elem-parse-this-element)
           (om-elem-plist-put-property :parameters :exports 'both)
           (om-elem-to-trimmed-string))
      => (:result "#+BEGIN_SRC emacs-lisp -n :exports both"
                  "#+END_SRC")
      :end-hidden)

    (defexamples-content om-elem-plist-remove-property
      nil

      (:buffer "#+CALL: ktulu() :results html")
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

      (:buffer "#+BEGIN: blockhead :format \"[%s]\""
               "#+END:")
      (->> (om-elem-parse-this-element)
           (om-elem-plist-remove-property :arguments :format)
           (om-elem-to-trimmed-string))
      => (:result "#+BEGIN: blockhead"
                  "#+END:")

      (:buffer "call_ktulu[:cache no]()[:results html]")
      (->> (om-elem-parse-this-object)
           (om-elem-plist-remove-property :inside-header :cache)
           (om-elem-plist-remove-property :end-header :results)
           (om-elem-to-trimmed-string))
      => "call_ktulu()"

      (:buffer "src_emacs-lisp[:exports results]{}")
      (->> (om-elem-parse-this-object)
           (om-elem-plist-remove-property :parameters :exports)
           (om-elem-to-trimmed-string))
      => "src_emacs-lisp{}"

      (:buffer "#+BEGIN_SRC emacs-lisp -n :exports results"
               "#+END_SRC")
      (->> (om-elem-parse-this-element)
           (om-elem-plist-remove-property :parameters :exports)
           (om-elem-to-trimmed-string))
      => (:result "#+BEGIN_SRC emacs-lisp -n"
                  "#+END_SRC")
      :end-hidden)

    ;; (defexamples-content om-elem-property-is-nil-p
    ;;   nil
    ;;   (:buffer "* TODO dummy")
    ;;   (->> (om-elem-parse-this-headline)
    ;;        (om-elem-property-is-nil-p :todo-keyword))
    ;;   => nil
    ;;   (->> (om-elem-parse-this-headline)
    ;;        (om-elem-property-is-nil-p :commentedp))
    ;;   => t)

    ;; (defexamples-content om-elem-property-is-non-nil-p
    ;;   nil
    ;;   (:buffer "* TODO dummy")
    ;;   (->> (om-elem-parse-this-headline)
    ;;        (om-elem-property-is-non-nil-p :todo-keyword))
    ;;   => t
    ;;   (->> (om-elem-parse-this-headline)
    ;;        (om-elem-property-is-non-nil-p :commentedp))
    ;;   => nil)

    ;; (defexamples-content om-elem-property-is-eq-p
    ;;   nil
    ;;   (:buffer "* [#A] dummy")
    ;;   (->> (om-elem-parse-this-headline)
    ;;        (om-elem-property-is-eq-p :priority ?A))
    ;;   => t
    ;;   (->> (om-elem-parse-this-headline)
    ;;        (om-elem-property-is-eq-p :priority ?B))
    ;;   => nil)

    ;; (defexamples-content om-elem-property-is-equal-p
    ;;   nil
    ;;   (:buffer "* TODO dummy")
    ;;   (->> (om-elem-parse-this-headline)
    ;;        (om-elem-property-is-equal-p :todo-keyword "TODO"))
    ;;   => t
    ;;   (->> (om-elem-parse-this-headline)
    ;;        (om-elem-property-is-equal-p :todo-keyword "DONE"))
    ;;   => nil)

    ;; (defexamples-content om-elem-property-is-predicate-p
    ;;   nil
    ;;   (:buffer "* this is a dummy")
    ;;   (->> (om-elem-parse-this-headline)
    ;;        (om-elem-property-is-predicate-p*
    ;;         :title (s-contains? "dummy" (car it))))
    ;;   => t)
    )

  (def-example-subgroup "Clock"
    nil

    ;; TODO add get/set/shift duration
    ;; TODO add get/set/shift start/end/single/double time

    (defexamples-content om-elem-clock-is-running-p
      nil
      (:buffer "CLOCK: [2019-01-01 Tue 00:00]")
      (->> (om-elem-parse-this-element)
           (om-elem-clock-is-running-p))
      => t
      (:buffer "CLOCK: [2019-01-01 Tue 00:00]--[2019-01-02 Wed 00:00] => 24:00")
      (->> (om-elem-parse-this-element)
           (om-elem-clock-is-running-p))
      => nil))

  (defexamples-content om-elem-clock-map-timestamp
    nil
    ;; TODO add more unit tests for this
    (:buffer "CLOCK: [2019-01-01 Tue 00:00]")
    (->> (om-elem-parse-this-element)
         (om-elem-clock-map-timestamp*
          (om-elem-timestamp-shift 1 'day it))
         (om-elem-to-trimmed-string))
    => "CLOCK: [2019-01-02 Wed 00:00]"
    )
  

  (def-example-subgroup "Headline"
    nil

    (defexamples-content om-elem-headline-is-done-p
      nil
      (:buffer "* TODO darn")
      (->> (om-elem-parse-this-headline)
           (om-elem-headline-is-done-p))
      => nil
      (:buffer "* DONE yay")
      (->> (om-elem-parse-this-headline)
           (om-elem-headline-is-done-p))
      => t)

    (defexamples-content om-elem-headline-is-archived-p
      nil
      (:buffer "* dummy")
      (->> (om-elem-parse-this-headline)
           (om-elem-headline-is-archived-p))
      => nil
      (:buffer "* dummy                                                             :ARCHIVE:")
      (->> (om-elem-parse-this-headline)
           (om-elem-headline-is-archived-p))
      => t)

    (defexamples-content om-elem-headline-is-commented-p
      nil
      (:buffer "* dummy")
      (->> (om-elem-parse-this-headline)
           (om-elem-headline-is-commented-p))
      => nil
      (:buffer "* COMMENT dummy")
      (->> (om-elem-parse-this-headline)
           (om-elem-headline-is-commented-p))
      => t)

    (defexamples-content om-elem-headline-has-tag-p
      nil
      (:buffer "* dummy")
      (->> (om-elem-parse-this-headline)
           (om-elem-headline-has-tag-p "tmsu"))
      => nil
      (:buffer "* dummy                                                             :tmsu:")
      (->> (om-elem-parse-this-headline)
           (om-elem-headline-has-tag-p "tmsu"))
      => t)

    (defexamples-content om-elem-headline-get-statistics-cookie
      nil
      (:buffer "* statistically significant [10/10]")
      (->> (om-elem-parse-this-headline)
           (om-elem-headline-get-statistics-cookie)
           (om-elem-to-string))
      => "[10/10]"
      (:buffer "* not statistically significant")
      (->> (om-elem-parse-this-headline)
           (om-elem-headline-get-statistics-cookie))
      => nil)

    ;; TODO add the shortcut version title setter

    )


  ;; TODO add inlinetask

  (def-example-subgroup "Item"
    nil

    ;; TODO add shortcut tag setter

    (defexamples-content om-elem-item-is-unchecked-p
      nil
      (:buffer "- one"
               "- [ ] two"
               "- [X] three"
               "- [-] four")
      (->> (om-elem-parse-this-element)
           (om-elem--get-children)
           (-map #'om-elem-item-is-unchecked-p))
      => '(nil t nil nil))

    (defexamples-content om-elem-item-is-checked-p
      nil
      (:buffer "- one"
               "- [ ] two"
               "- [X] three"
               "- [-] four")
      (->> (om-elem-parse-this-element)
           (om-elem--get-children)
           (-map #'om-elem-item-is-checked-p))
      => '(nil nil t nil))

    (defexamples-content om-elem-item-is-trans-p
      nil
      (:buffer "- one"
               "- [ ] two"
               "- [X] three"
               "- [-] four")
      (->> (om-elem-parse-this-element)
           (om-elem--get-children)
           (-map #'om-elem-item-is-trans-p))
      => '(nil nil nil t))
    
    (defexamples-content om-elem-item-toggle-checkbox
      nil
      (:buffer "- [ ] one")
      (->> (om-elem-parse-this-item)
           (om-elem-item-toggle-checkbox)
           (om-elem-to-trimmed-string))
      => "- [X] one"
      (->> (om-elem-parse-this-item)
           (om-elem-item-toggle-checkbox)
           (om-elem-item-toggle-checkbox)
           (om-elem-to-trimmed-string))
      => "- [ ] one"
      (:buffer "- [-] one")
      (->> (om-elem-parse-this-item)
           (om-elem-item-toggle-checkbox)
           (om-elem-to-trimmed-string))
      => "- [-] one"
      (:buffer "- one")
      (->> (om-elem-parse-this-item)
           (om-elem-item-toggle-checkbox)
           (om-elem-to-trimmed-string))
      => "- one")
    )

  (def-example-subgroup "Planning"
    nil

    (defexamples-content om-elem-planning-set-timestamp
      nil
      (:buffer "* dummy"
               "CLOSED: [2019-01-01 Tue]")
      (:comment "Change an existing timestamp in planning")
      (->> (om-elem-parse-this-headline)
           (om-elem--headline-get-planning)
           (om-elem-planning-set-timestamp
            :closed '(2019 1 2 &warning all 1 day &repeater cumulate 2 month))
           (om-elem-to-trimmed-string))
      => "CLOSED: [2019-01-02 Wed +2m -1d]"
      (:comment "Add a new timestamp and remove another")
      (->> (om-elem-parse-this-headline)
           (om-elem--headline-get-planning)
           (om-elem-planning-set-timestamp
            :deadline '(2112 1 1))
           (om-elem-planning-set-timestamp
            :closed nil)
           (om-elem-to-trimmed-string))
      => "DEADLINE: [2112-01-01 Fri]")

    (defexamples-content om-elem-planning-map-timestamp
      nil
      (:buffer "* dummy"
               "CLOSED: [2019-01-01 Tue]")
      (:comment "Apply mapping function if timestamp exists")
      (->> (om-elem-parse-this-headline)
           (om-elem--headline-get-planning)
           (om-elem-planning-map-timestamp*
            :closed (om-elem-timestamp-shift 1 'day it))
           (om-elem-to-trimmed-string))
      => "CLOSED: [2019-01-02 Wed]"
      (:comment "Do nothing if timestamp does not exist")
      (->> (om-elem-parse-this-headline)
           (om-elem--headline-get-planning)
           (om-elem-planning-map-timestamp*
            :deadline (om-elem-timestamp-shift 1 'day it))
           (om-elem-to-trimmed-string))
      => "CLOSED: [2019-01-01 Tue]"
      (:comment "Throw error if new timestamp is not allowed")
      (->> (om-elem-parse-this-headline)
           (om-elem--headline-get-planning)
           (om-elem-planning-map-timestamp
            :closed #'om-elem-timestamp-toggle-active)
           (om-elem-to-trimmed-string))
      !!> error))

  (def-example-subgroup "Statistics Cookie"
    nil
    (defexamples-content om-elem-statistics-cookie-is-complete-p
      nil
      (:buffer "* statistically significant [10/10]")
      (->> (om-elem-parse-this-headline)
           ;; TODO make public
           (om-elem-headline-get-statistics-cookie)
           (om-elem-statistics-cookie-is-complete-p))
      => t
      (:buffer "* statistically significant [1/10]")
      (->> (om-elem-parse-this-headline)
           ;; TODO make public
           (om-elem-headline-get-statistics-cookie)
           (om-elem-statistics-cookie-is-complete-p))
      => nil
      (:buffer "* statistically significant [100%]")
      (->> (om-elem-parse-this-headline)
           ;; TODO make public
           (om-elem-headline-get-statistics-cookie)
           (om-elem-statistics-cookie-is-complete-p))
      => t
      (:buffer "* statistically significant [33%]")
      (->> (om-elem-parse-this-headline)
           ;; TODO make public
           (om-elem-headline-get-statistics-cookie)
           (om-elem-statistics-cookie-is-complete-p))
      => nil))

  (def-example-subgroup "Timestamp"
    nil

    (defexamples-content om-elem-timestamp-get-start-time
      nil
      (:buffer "[2019-01-01 Tue]")
      (->> (om-elem-parse-this-object)
           (om-elem-timestamp-get-start-time))
      => '(2019 1 1 nil nil)
      (:buffer "[2019-01-01 Tue]--[2019-01-02 Wed]")
      (->> (om-elem-parse-this-object)
           (om-elem-timestamp-get-start-time))
      => '(2019 1 1 nil nil)
      (:buffer "[2019-01-01 Tue 00:00-12:00]")
      (->> (om-elem-parse-this-object)
           (om-elem-timestamp-get-start-time))
      => '(2019 1 1 0 0))

    (defexamples-content om-elem-timestamp-get-end-time
      nil
      (:buffer "[2019-01-01 Tue]")
      (->> (om-elem-parse-this-object)
           (om-elem-timestamp-get-end-time))
      => nil
      (:buffer "[2019-01-01 Tue]--[2019-01-02 Wed]")
      (->> (om-elem-parse-this-object)
           (om-elem-timestamp-get-end-time))
      => '(2019 1 2 nil nil)
      (:buffer "[2019-01-01 Tue 00:00-12:00]")
      (->> (om-elem-parse-this-object)
           (om-elem-timestamp-get-end-time))
      => '(2019 1 1 12 0))

    (defexamples-content om-elem-timestamp-is-active-p
      nil
      (:buffer "<2019-01-01 Tue>")
      (->> (om-elem-parse-this-object)
           (om-elem-timestamp-is-active-p))
      => t
      (:buffer "[2019-01-01 Tue]")
      (->> (om-elem-parse-this-object)
           (om-elem-timestamp-is-active-p))
      => nil)

    (defexamples-content om-elem-timestamp-is-ranged-p
      nil
      (:buffer "[2019-01-01 Tue]--[2019-01-02 Wed]")
      (->> (om-elem-parse-this-object)
           (om-elem-timestamp-is-ranged-p))
      => t
      (:buffer "[2019-01-01 Tue 00:00-12:00]")
      (->> (om-elem-parse-this-object)
           (om-elem-timestamp-is-ranged-p))
      => t
      (:buffer "[2019-01-01 Tue]")
      (->> (om-elem-parse-this-object)
           (om-elem-timestamp-is-ranged-p))
      => nil)

    (defexamples-content om-elem-timestamp-set-start-time
      nil
      (:buffer "[2019-01-02 Wed]")
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
      (:buffer "[2019-01-01 Tue]")
      (:comment "Add the end time")
      (->> (om-elem-parse-this-object)
           (om-elem-timestamp-set-end-time '(2019 1 2))
           (om-elem-to-trimmed-string))
      => "[2019-01-01 Tue]--[2019-01-02 Wed]"
      (:buffer "[2019-01-01 Tue]--[2019-01-02 Wed]")
      (:comment "Remove the end time")
      (->> (om-elem-parse-this-object)
           (om-elem-timestamp-set-end-time nil)
           (om-elem-to-trimmed-string))
      => "[2019-01-01 Tue]")

    (defexamples-content om-elem-timestamp-set-single-time
      nil
      (:buffer "[2019-01-01 Tue]")
      (:comment "Don't make a range")
      (->> (om-elem-parse-this-object)
           (om-elem-timestamp-set-single-time '(2019 1 2))
           (om-elem-to-trimmed-string))
      => "[2019-01-02 Wed]"
      (:buffer "[2019-01-01 Tue]--[2019-01-02 Wed]")
      (:comment "Output is not a range despite input being ranged")
      (->> (om-elem-parse-this-object)
           (om-elem-timestamp-set-single-time '(2019 1 3))
           (om-elem-to-trimmed-string))
      => "[2019-01-03 Thu]")

    (defexamples-content om-elem-timestamp-set-double-time
      nil
      (:buffer "[2019-01-01 Tue]")
      (:comment "Make a range")
      (->> (om-elem-parse-this-object)
           (om-elem-timestamp-set-double-time '(2019 1 2) '(2019 1 3))
           (om-elem-to-trimmed-string))
      => "[2019-01-02 Wed]--[2019-01-03 Thu]"
      (:buffer "[2019-01-01 Tue]--[2019-01-03 Wed]")
      (:comment "Output is not a range despite input being ranged")
      (->> (om-elem-parse-this-object)
           (om-elem-timestamp-set-double-time '(2019 1 4) '(2019 1 5))
           (om-elem-to-trimmed-string))
      => "[2019-01-04 Fri]--[2019-01-05 Sat]")

    (defexamples-content om-elem-timestamp-set-range
      nil
      (:buffer "[2019-01-01 Tue]")
      (:comment "Use days as the unit for short format")
      (->> (om-elem-parse-this-object)
           (om-elem-timestamp-set-range 1)
           (om-elem-to-trimmed-string))
      => "[2019-01-01 Tue]--[2019-01-02 Wed]"
      (:buffer "[2019-01-01 Tue 00:00]")
      (:comment "Use minutes as the unit for long format")
      (->> (om-elem-parse-this-object)
           (om-elem-timestamp-set-range 3)
           (om-elem-to-trimmed-string))
      => "[2019-01-01 Tue 00:00]--[2019-01-01 Tue 00:03]"
      (:buffer "[2019-01-01 Tue]--[2019-01-03 Wed]")
      (:comment "Set range to 0 to remove end time")
      (->> (om-elem-parse-this-object)
           (om-elem-timestamp-set-range 0)
           (om-elem-to-trimmed-string))
      => "[2019-01-01 Tue]")

    (defexamples-content om-elem-timestamp-set-type
      nil
      (:buffer "[2019-01-01 Tue]")
      (->> (om-elem-parse-this-object)
           (om-elem-timestamp-set-type 'active)
           (om-elem-to-trimmed-string))
      => "<2019-01-01 Tue>")

    (defexamples-content om-elem-timestamp-shift
      nil
      (:buffer "[2019-01-01 Tue 12:00]")
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
      (:buffer "[2019-01-01 Tue]")
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
      (:buffer "[2019-01-01 Tue 12:00]")
      (:comment "If not a range, change start time and leave implicit end time.")
      (->> (om-elem-parse-this-object)
           (om-elem-timestamp-shift-start -1 'year)
           (om-elem-to-trimmed-string))
      => "[2018-01-01 Mon 12:00]--[2019-01-01 Tue 12:00]"
      (:buffer "[2019-01-01 Tue]--[2019-01-03 Thu]")
      (:comment "Change only start time if a range")
      (->> (om-elem-parse-this-object)
           (om-elem-timestamp-shift-start 1 'day)
           (om-elem-to-trimmed-string))
      => "[2019-01-02 Wed]--[2019-01-03 Thu]")

    (defexamples-content om-elem-timestamp-shift-end
      nil
      (:buffer "[2019-01-01 Tue]")
      (:comment "Shift implicit end time if not a range.")
      (->> (om-elem-parse-this-object)
           (om-elem-timestamp-shift-end 1 'day)
           (om-elem-to-trimmed-string))
      => "[2019-01-01 Tue]--[2019-01-02 Wed]"
      (:buffer "[2019-01-01 Tue]--[2019-01-02 Wed]")
      (:comment "Move only the second time if a range.")
      (->> (om-elem-parse-this-object)
           (om-elem-timestamp-shift-end 1 'day)
           (om-elem-to-trimmed-string))
      => "[2019-01-01 Tue]--[2019-01-03 Thu]")

    (defexamples-content om-elem-timestamp-toggle-active
      nil
      (:buffer "[2019-01-01 Tue]")
      (->> (om-elem-parse-this-object)
           (om-elem-timestamp-toggle-active)
           (om-elem-to-trimmed-string))
      => "<2019-01-01 Tue>"
      (->> (om-elem-parse-this-object)
           (om-elem-timestamp-toggle-active)
           (om-elem-timestamp-toggle-active)
           (om-elem-to-trimmed-string))
      => "[2019-01-01 Tue]"
      (:buffer "[2019-01-01 Tue]--[2019-01-02 Wed]")
      (->> (om-elem-parse-this-object)
           (om-elem-timestamp-toggle-active)
           (om-elem-to-trimmed-string))
      => "<2019-01-01 Tue>--<2019-01-02 Wed>"
      (->> (om-elem-parse-this-object)
           (om-elem-timestamp-toggle-active)
           (om-elem-timestamp-toggle-active)
           (om-elem-to-trimmed-string))
      => "[2019-01-01 Tue]--[2019-01-02 Wed]")))

(def-example-group "Branch/Child Manipulation"
  "Set, get, and map the children of branch nodes."

  (def-example-subgroup "Generic"
    nil

    (defexamples-content om-elem-get-children
      nil

      (:buffer "/this/ is a *paragraph*")
      (:comment "Return objects for object containers")
      (->> (om-elem-parse-this-element)
           (om-elem-get-children)
           (-map #'om-elem-get-type))
      => '(italic plain-text bold)

      (:buffer "* headline"
               "stuff"
               "** subheadline")
      (:comment "Return elements for greater elements")
      (->> (om-elem-parse-this-subtree)
           (om-elem-get-children)
           (-map #'om-elem-get-type))
      => '(section headline)

      (:buffer "#+CALL: ktulu()")
      (:comment "Throw error when attempting to get contents of a non-container")
      (->> (om-elem-parse-this-element)
           (om-elem-get-children)
           (-map #'om-elem-get-type))
      !!> error

      :begin-hidden

      (:buffer "| a | b |")
      (->> (om-elem-parse-this-table-row)
           (om-elem-get-children)
           (-map #'om-elem-get-type))
      => '(table-cell table-cell)

      (:buffer "#+BEGIN_VERSE"
               "verse /666/"
               "#+END_VERSE")
      (->> (om-elem-parse-this-element)
           (om-elem-get-children)
           (-map #'om-elem-get-type))
      ;; plain-text for the newline at the end...I think
      => '(plain-text italic plain-text)

      (:buffer "#+BEGIN_CENTER"
               "paragraph thing"
               "#+END_CENTER")
      (->> (om-elem-parse-this-element)
           (om-elem-get-children)
           (-map #'om-elem-get-type))
      => '(paragraph)

      (:buffer ":LOGBOOK:"
               "- log entry"
               "CLOCK: [2019-01-01 Tue]"
               ":END:")
      (->> (om-elem-parse-this-element)
           (om-elem-get-children)
           (-map #'om-elem-get-type))
      => '(plain-list clock)

      (:buffer "[fn:1] bigfoot")
      (->> (om-elem-parse-this-element)
           (om-elem-get-children)
           (-map #'om-elem-get-type))
      => '(paragraph)

      (:buffer "- item"
               "  - subitem")
      (->> (om-elem-parse-this-element)
           (om-elem-get-children)
           (-map #'om-elem-get-type))
      => '(item)
      (->> (om-elem-parse-this-item)
           (om-elem-get-children)
           (-map #'om-elem-get-type))
      => '(paragraph plain-list)

      (:buffer "* dummy"
               ":PROPERTIES:"
               ":ONE: one"
               ":TWO: two"
               ":END:")
      (->> (om-elem-parse-this-headline)
           (om-elem--headline-get-properties-drawer)
           (om-elem-get-children)
           (-map #'om-elem-get-type))
      => '(node-property node-property) 

      (:buffer "#+BEGIN_QUOTE"
               "no pity for the majority"
               "#+END_QUOTE")
      (->> (om-elem-parse-this-element)
           (om-elem-get-children)
           (-map #'om-elem-get-type))
      => '(paragraph)

      ;; (:buffer "* dummy"
      ;;           "stuff")
      ;; (->> (om-elem-parse-this-headline)
      ;;      (om-elem-headline-get-section)
      ;;      (om-elem-get-children)
      ;;      (-map #'om-elem-get-type))
      ;; => '(paragraph)

      (:buffer "| a |"
               "| b |")
      (->> (om-elem-parse-this-element)
           (om-elem-get-children)
           (-map #'om-elem-get-type))
      => '(table-row table-row)

      :end-hidden)

    (defexamples-content om-elem-set-children
      nil

      (:buffer "/this/ is a *paragraph*")
      (:comment "Set contents for object containers")
      (->> (om-elem-parse-this-element)
           (om-elem-set-children (list "this is lame"))
           (om-elem-to-trimmed-string))
      => "this is lame"

      (:buffer "* headline"
               "stuff"
               "** subheadline")
      (:comment "Set contents for greater elements")
      (->> (om-elem-parse-this-subtree)
           (om-elem-set-children (list (om-elem-build-headline! :title-text "only me" :level 2)))
           (om-elem-to-trimmed-string))
      => (:result "* headline"
                  "** only me")

      (:buffer "#+CALL: ktulu()")
      (:comment "Throw error when attempting to get contents of a non-container")
      (->> (om-elem-parse-this-element)
           (om-elem-set-children "nil by mouth")
           (om-elem-to-trimmed-string))
      !!> error

      :begin-hidden

      ;; TODO add hidden tests

      :end-hidden)

    (defexamples-content om-elem-map-children
      nil

      (:buffer "/this/ is a *paragraph*")
      (->> (om-elem-parse-this-element)
           (om-elem-map-children
            (lambda (objs) (append objs (list " ...yeah"))))
           (om-elem-to-trimmed-string))
      => "/this/ is a *paragraph* ...yeah"

      (:buffer "* headline"
               "** subheadline")
      (->> (om-elem-parse-this-subtree)
           (om-elem-map-children* (--map (om-elem-shift-property :level 1 it) it))
           (om-elem-to-trimmed-string))
      => (:result "* headline"
                  "*** subheadline")

      (:buffer "#+CALL: ktulu()")
      (:comment "Throw error when attempting to map contents of a non-container")
      (->> (om-elem-parse-this-element)
           (om-elem-map-children #'ignore)
           (om-elem-to-trimmed-string))
      !!> error

      :begin-hidden

      ;; TODO add hidden tests

      :end-hidden)

    
    (defexamples-content om-elem-is-childless-p
      nil
      (:buffer "* dummy"
               "filled with useless knowledge")
      (->> (om-elem-parse-this-headline)
           (om-elem-is-childless-p))
      => nil
      (:buffer "* dummy")
      (->> (om-elem-parse-this-headline)
           (om-elem-is-childless-p))
      => t
      (:buffer "#+CALL: ktulu()")
      (:comment "Throw error when attempting to determine if non-container is empty")
      (->> (om-elem-parse-this-element)
           (om-elem-is-childless-p))
      !!> error))

  (def-example-subgroup "Headline"
    nil

    (defexamples-content om-elem-headline-update-item-statistics
      nil
      (:buffer "* statistically significant [/]"
               "- irrelevant data"
               "- [ ] good data"
               "- [X] bad data")
      (->> (om-elem-parse-this-headline)
           (om-elem-headline-update-item-statistics)
           (om-elem-to-trimmed-string))
      => (:result "* statistically significant [1/2]"
                  "- irrelevant data"
                  "- [ ] good data"
                  "- [X] bad data")

      (:buffer "* statistically significant [%]"
               "- irrelevant data"
               "- [ ] good data"
               "- [X] bad data")
      (->> (om-elem-parse-this-headline)
           (om-elem-headline-update-item-statistics)
           (om-elem-to-trimmed-string))
      => (:result "* statistically significant [50%]"
                  "- irrelevant data"
                  "- [ ] good data"
                  "- [X] bad data")

      (:buffer "* statistically significant"
               "- irrelevant data"
               "- [ ] good data"
               "- [X] bad data")
      (->> (om-elem-parse-this-headline)
           (om-elem-headline-update-item-statistics)
           (om-elem-to-trimmed-string))
      => (:result "* statistically significant"
                  "- irrelevant data"
                  "- [ ] good data"
                  "- [X] bad data"))

    (defexamples-content om-elem-headline-update-todo-statistics
      nil
      (:buffer "* statistically significant [/]"
               "** irrelevant data"
               "** TODO good data"
               "** DONE bad data")
      (->> (om-elem-parse-this-subtree)
           (om-elem-headline-update-todo-statistics)
           (om-elem-to-trimmed-string))
      => (:result "* statistically significant [1/2]"
                  "** irrelevant data"
                  "** TODO good data"
                  "** DONE bad data")

      (:buffer "* statistically significant [%]"
               "** irrelevant data"
               "** TODO good data"
               "** DONE bad data")
      (->> (om-elem-parse-this-subtree)
           (om-elem-headline-update-todo-statistics)
           (om-elem-to-trimmed-string))
      => (:result "* statistically significant [50%]"
                  "** irrelevant data"
                  "** TODO good data"
                  "** DONE bad data")

      (:buffer "* statistically significant"
               "** irrelevant data"
               "** TODO good data"
               "** DONE bad data")
      (->> (om-elem-parse-this-subtree)
           (om-elem-headline-update-todo-statistics)
           (om-elem-to-trimmed-string))
      => (:result "* statistically significant"
                  "** irrelevant data"
                  "** TODO good data"
                  "** DONE bad data"))

    ;; (defexamples-content om-elem-headline-is-scheduled-p
    ;;   nil
    ;;   (:buffer "* lazy")
    ;;   (->> (om-elem-parse-this-headline)
    ;;        (om-elem-headline-is-scheduled-p))
    ;;   => nil
    ;;   (:buffer "* proactive"
    ;;             "SCHEDULED: [2019-01-01 Tue]")
    ;;   (->> (om-elem-parse-this-headline)
    ;;        (om-elem-headline-is-scheduled-p))
    ;;   => t)

    ;; (defexamples-content om-elem-headline-is-deadlined-p
    ;;   nil
    ;;   (:buffer "* lazy")
    ;;   (->> (om-elem-parse-this-headline)
    ;;        (om-elem-headline-is-deadlined-p))
    ;;   => nil
    ;;   (:buffer "* proactive"
    ;;             "DEADLINE: [2019-01-01 Tue]")
    ;;   (->> (om-elem-parse-this-headline)
    ;;        (om-elem-headline-is-deadlined-p))
    ;;   => t)
    
    ;; (defexamples-content om-elem-headline-is-closed-p
    ;;   nil
    ;;   (:buffer "* lazy")
    ;;   (->> (om-elem-parse-this-headline)
    ;;        (om-elem-headline-is-closed-p))
    ;;   => nil
    ;;   (:buffer "* proactive"
    ;;             "CLOSED: [2019-01-01 Tue]")
    ;;   (->> (om-elem-parse-this-headline)
    ;;        (om-elem-headline-is-closed-p))
    ;;   => t)

    ;; (defexamples-content om-elem-headline-get-subheadlines
    ;;   nil
    ;;   (:buffer "* headline 1"
    ;;             "sectional stuff"
    ;;             "** headline 2"
    ;;             "** headline 3")
    ;;   (->> (om-elem-parse-this-subtree)
    ;;        (om-elem-headline-get-subheadlines)
    ;;        (-map #'om-elem-to-trimmed-string))
    ;;   => '("** headline 2" "** headline 3")
    ;;   (:buffer "* headline 1"
    ;;             "sectional stuff")
    ;;   (->> (om-elem-parse-this-subtree)
    ;;        (om-elem-headline-get-subheadlines)
    ;;        (-map #'om-elem-to-trimmed-string))
    ;;   => nil)

    ;; (defexamples-content om-elem-headline-get-section
    ;;   nil
    ;;   (:buffer "* headline 1"
    ;;             "sectional stuff"
    ;;             "** headline 2"
    ;;             "** headline 3")
    ;;   (->> (om-elem-parse-this-subtree)
    ;;        (om-elem-headline-get-section)
    ;;        (om-elem-to-trimmed-string))
    ;;   => "sectional stuff"
    ;;   (:buffer "* headline 1"
    ;;             "** headline 2"
    ;;             "** headline 3")
    ;;   (->> (om-elem-parse-this-subtree)
    ;;        (om-elem-headline-get-section)
    ;;        (om-elem-to-trimmed-string))
    ;;   => "")

    ;; (defexamples-content om-elem-headline-get-drawer
    ;;   nil
    ;;   (:buffer "* headline 1"
    ;;             ":LOGBOOK:"
    ;;             "- random note"
    ;;             ":END:"
    ;;             "rest of the section"
    ;;             "** headline 2")
    ;;   (->> (om-elem-parse-this-subtree)
    ;;        (om-elem-headline-get-drawer "LOGBOOK")
    ;;        (om-elem-to-trimmed-string))
    ;;   => (:result ":LOGBOOK:"
    ;;               "- random note"
    ;;               ":END:")
    ;;   (->> (om-elem-parse-this-subtree)
    ;;        (om-elem-headline-get-drawer "OTHER")
    ;;        (om-elem-to-trimmed-string))
    ;;   => "")

    ;; (defexamples-content om-elem-headline-get-path
    ;;   nil
    ;;   (:buffer "* one"
    ;;             "** two"
    ;;             "*** three")
    ;;   (--> (om-elem-parse-this-subtree)
    ;;        (om-elem-match-first it 'headline)
    ;;        (om-elem-headline-get-path it)
    ;;        (om-elem-to-trimmed-string it))
    ;;   => '("one"))

    (defexamples-content om-elem-headline-indent-subheadline
      nil
      (:buffer "* one"
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
      (:buffer "* one"
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
      (:buffer "* one"
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
      (:buffer "* one"
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

  ;; (def-example-subgroup "Item"
  ;;   nil

  ;; (defexamples-content om-elem-item-get-level
  ;;   nil
  ;;   (:buffer "- one"
  ;;             "  - two"
  ;;             "    - three")
  ;;   (->> (om-elem-parse-this-item)
  ;;        (om-elem-)))

  ;; (defexamples-content om-elem-item-get-sublist
  ;;   nil
  ;;   (:buffer "- one"
  ;;             "  - two"
  ;;             "  - three"
  ;;             "- four")
  ;;   (->> (om-elem-parse-this-item)
  ;;        (om-elem-item-get-sublist)
  ;;        (om-elem-to-trimmed-string))
  ;;   => (:result "- two"
  ;;               "- three")
  ;;   (:buffer "- one"
  ;;             "- two")
  ;;   (->> (om-elem-parse-this-item)
  ;;        (om-elem-item-get-sublist)
  ;;        (om-elem-to-trimmed-string))
  ;;   => "")

  ;; (defexamples-content om-elem-item-get-paragraph
  ;;   nil
  ;;   (:buffer "- one")
  ;;   (->> (om-elem-parse-this-item)
  ;;        (om-elem-item-get-paragraph)
  ;;        (om-elem-to-trimmed-string))
  ;;   => "one"
  ;;   (:buffer "- [ ] one")
  ;;   (->> (om-elem-parse-this-item)
  ;;        (om-elem-item-get-paragraph)
  ;;        (om-elem-to-trimmed-string))
  ;;   => "one"
  ;;   (:buffer "- tmsu :: one")
  ;;   (->> (om-elem-parse-this-item)
  ;;        (om-elem-item-get-paragraph)
  ;;        (om-elem-to-trimmed-string))
  ;;   => "one"
  ;;   (:buffer "- tmsu ::")
  ;;   (->> (om-elem-parse-this-item)
  ;;        (om-elem-item-get-paragraph)
  ;;        (om-elem-to-trimmed-string))
  ;;   => ""))

  (def-example-subgroup "Plain List"
    nil
    
    (defexamples-content om-elem-plain-list-set-type
      nil
      (:buffer "- [ ] one"
               "- [X] two")
      (->> (om-elem-parse-this-element)
           (om-elem-plain-list-set-type 'ordered)
           (om-elem-to-trimmed-string))
      => (:result "1. [ ] one"
                  "2. [X] two")
      (:buffer "1. [ ] one"
               "2. [X] two")
      (->> (om-elem-parse-this-element)
           (om-elem-plain-list-set-type '-)
           (om-elem-to-trimmed-string))
      => (:result "- [ ] one"
                  "- [X] two"))

    (defexamples-content om-elem-plain-list-indent-item
      nil
      (:buffer "- one"
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
      (:buffer "- one"
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
      (:buffer "- one"
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
      (:buffer "- one"
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
      (:buffer "| 1 | 2 | 3 |"
               "|---+---+---|"
               "| a | b | c |")
      (->> (om-elem-parse-this-element)
           (om-elem-table-get-cell 0 0)
           (om-elem--get-children)
           (car))
      => "1"
      (->> (om-elem-parse-this-element)
           (om-elem-table-get-cell 1 1)
           (om-elem--get-children)
           (car))
      => "b"
      (->> (om-elem-parse-this-element)
           (om-elem-table-get-cell -1 -1)
           (om-elem--get-children)
           (car))
      => "c"
      :begin-hidden
      (->> (om-elem-parse-this-element)
           (om-elem-table-get-cell 0 3)
           (om-elem--get-children)
           (car))
      !!> error
      :end-hidden)

    (defexamples-content om-elem-table-replace-cell
      nil
      (:buffer "| 1 | 2 |"
               "|---+---|"
               "| a | b |")
      (->> (om-elem-parse-this-element)
           (om-elem-table-replace-cell
            0 0 (om-elem-build-table-cell "2"))
           (om-elem-to-trimmed-string))
      => (:result "| 2 | 2 |"
                  "|---+---|"
                  "| a | b |")
      (->> (om-elem-parse-this-element)
           (om-elem-table-replace-cell
            -1 -1 (om-elem-build-table-cell "B"))
           (om-elem-to-trimmed-string))
      => (:result "| 1 | 2 |"
                  "|---+---|"
                  "| a | B |"))

    (defexamples-content om-elem-table-replace-cell!
      nil
      (:buffer "| 1 | 2 |"
               "|---+---|"
               "| a | b |")
      (->> (om-elem-parse-this-element)
           (om-elem-table-replace-cell! 0 0 "2")
           (om-elem-to-trimmed-string))
      => (:result "| 2 | 2 |"
                  "|---+---|"
                  "| a | b |")
      (->> (om-elem-parse-this-element)
           (om-elem-table-replace-cell! -1 -1 "B")
           (om-elem-to-trimmed-string))
      => (:result "| 1 | 2 |"
                  "|---+---|"
                  "| a | B |"))

    (defexamples-content om-elem-table-clear-cell
      nil
      (:buffer "| 1 | 2 |"
               "|---+---|"
               "| a | b |")
      (->> (om-elem-parse-this-element)
           (om-elem-table-clear-cell 0 0)
           (om-elem-to-trimmed-string))
      => (:result "|   | 2 |"
                  "|---+---|"
                  "| a | b |")
      (->> (om-elem-parse-this-element)
           (om-elem-table-clear-cell -1 -1)
           (om-elem-to-trimmed-string))
      => (:result "| 1 | 2 |"
                  "|---+---|"
                  "| a |   |"))

    (defexamples-content om-elem-table-delete-column
      nil
      (:buffer "| a | b |"
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

    (defexamples-content om-elem-table-replace-column
      nil
      (:buffer "| a | b |"
               "|---+---|"
               "| c | d |")
      (->> (om-elem-parse-element-at 1)
           (om-elem-table-replace-column
            0 (list
               (om-elem-build-table-cell "A")
               (om-elem-build-table-cell "B")))
           (om-elem-to-trimmed-string))
      => (:result "| A | b |"
                  "|---+---|"
                  "| B | d |")
      (->> (om-elem-parse-element-at 1)
           (om-elem-table-replace-column
            -1 (list
                (om-elem-build-table-cell "A")
                (om-elem-build-table-cell "B")))
           (om-elem-to-trimmed-string))
      => (:result "| a | A |"
                  "|---+---|"
                  "| c | B |"))

    (defexamples-content om-elem-table-replace-column!
      nil
      (:buffer "| a | b |"
               "|---+---|"
               "| c | d |")
      (->> (om-elem-parse-element-at 1)
           (om-elem-table-replace-column! 0 '("A" "B"))
           (om-elem-to-trimmed-string))
      => (:result "| A | b |"
                  "|---+---|"
                  "| B | d |")
      (->> (om-elem-parse-element-at 1)
           (om-elem-table-replace-column! -1 '("A" "B"))
           (om-elem-to-trimmed-string))
      => (:result "| a | A |"
                  "|---+---|"
                  "| c | B |"))

    (defexamples-content om-elem-table-clear-column
      nil
      (:buffer "| a | b |"
               "|---+---|"
               "| c | d |")
      (->> (om-elem-parse-element-at 1)
           (om-elem-table-clear-column 0)
           (om-elem-to-trimmed-string))
      => (:result "|   | b |"
                  "|---+---|"
                  "|   | d |")
      (->> (om-elem-parse-element-at 1)
           (om-elem-table-clear-column -1)
           (om-elem-to-trimmed-string))
      => (:result "| a |   |"
                  "|---+---|"
                  "| c |   |"))

    (defexamples-content om-elem-table-replace-row
      nil
      (:buffer "| a | b |"
               "|---+---|"
               "| c | d |")
      (->> (om-elem-parse-element-at 1)
           (om-elem-table-replace-row
            0 (om-elem-build-table-row
               (om-elem-build-table-cell "A")
               (om-elem-build-table-cell "B")))
           (om-elem-to-trimmed-string))
      => (:result "| A | B |"
                  "|---+---|"
                  "| c | d |")
      (->> (om-elem-parse-element-at 1)
           (om-elem-table-replace-row
            -1 (om-elem-build-table-row
                (om-elem-build-table-cell "A")
                (om-elem-build-table-cell "B")))
           (om-elem-to-trimmed-string))
      => (:result "| a | b |"
                  "|---+---|"
                  "| A | B |"))

    (defexamples-content om-elem-table-replace-row!
      nil
      (:buffer "| a | b |"
               "|---+---|"
               "| c | d |")
      (->> (om-elem-parse-element-at 1)
           (om-elem-table-replace-row! 0 '("A" "B"))
           (om-elem-to-trimmed-string))
      => (:result "| A | B |"
                  "|---+---|"
                  "| c | d |")
      (->> (om-elem-parse-element-at 1)
           (om-elem-table-replace-row! -1 '("A" "B"))
           (om-elem-to-trimmed-string))
      => (:result "| a | b |"
                  "|---+---|"
                  "| A | B |"))

    (defexamples-content om-elem-table-clear-row
      nil
      (:buffer "| a | b |"
               "|---+---|"
               "| c | d |")
      (->> (om-elem-parse-element-at 1)
           (om-elem-table-clear-row 0)
           (om-elem-to-trimmed-string))
      => (:result "|   |   |"
                  "|---+---|"
                  "| c | d |")
      (->> (om-elem-parse-element-at 1)
           (om-elem-table-clear-row -1)
           (om-elem-to-trimmed-string))
      => (:result "| a | b |"
                  "|---+---|"
                  "|   |   |"))

    (defexamples-content om-elem-table-delete-row
      nil
      (:buffer "| a | b |"
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
      (:buffer "| a | b |"
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
      (:buffer "| a | b |"
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
      (:buffer "| a | b |"
               "|---+---|"
               "| c | d |")
      (->> (om-elem-parse-element-at 1)
           (om-elem-table-insert-row
            1
            (om-elem-build-table-row
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
            (om-elem-build-table-row
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
            (om-elem-build-table-row
             (om-elem-build-table-cell "x")
             (om-elem-build-table-cell "y")))
           (om-elem-to-trimmed-string))
      => (:result "| a | b |"
                  "|---+---|"
                  "| c | d |"
                  "| x | y |"))

    (defexamples-content om-elem-table-insert-row!
      nil
      (:buffer "| a | b |"
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

(def-example-group "Node Matching"
  "Use pattern-matching to selectively perform operations on nodes in trees."

  (defexamples-content om-elem-match
    nil

    (:buffer "* headline one"
             "** TODO headline two"
             "** COMMENT headline three"
             "** headline four")

    (:comment "Use a symbol to match a type, in this case all "
              "headlines.")
    (->> (om-elem-parse-this-subtree)
         (om-elem-match '(headline))
         (--map (om-elem-to-trimmed-string it)))
    => '("** TODO headline two"
         "** COMMENT headline three"
         "** headline four")

    (:comment "Use integers specify the index to return. Negative "
              "integers count from the end. Out of range integers "
              "return nil")
    (->> (om-elem-parse-this-subtree)
         (om-elem-match '(1))
         (--map (om-elem-to-trimmed-string it)))
    => '("** COMMENT headline three")
    (->> (om-elem-parse-this-subtree)
         (om-elem-match '(-1))
         (--map (om-elem-to-trimmed-string it)))
    => '("** headline four")
    (->> (om-elem-parse-this-subtree)
         (om-elem-match '(3))
         (--map (om-elem-to-trimmed-string it)))
    => nil

    (:comment "Use a two-membered list with an operator and an "
              "integer to match a range of indices. Allowed "
              "operators are <, >, <=, and and >=.")
    (->> (om-elem-parse-this-subtree)
         (om-elem-match '((> 0)))
         (--map (om-elem-to-trimmed-string it)))
    => '("** COMMENT headline three" "** headline four")
    (->> (om-elem-parse-this-subtree)
         (om-elem-match '((>= 1)))
         (--map (om-elem-to-trimmed-string it)))
    => '("** COMMENT headline three" "** headline four")

    (:comment "Use a plist to match based on an elements properties.")
    (->> (om-elem-parse-this-subtree)
         (om-elem-match '((:todo-keyword "TODO")))
         (--map (om-elem-to-trimmed-string it)))
    => '("** TODO headline two")
    (->> (om-elem-parse-this-subtree)
         (om-elem-match '((:todo-keyword nil)))
         (--map (om-elem-to-trimmed-string it)))
    => '("** COMMENT headline three" "** headline four")
    (->> (om-elem-parse-this-subtree)
         (om-elem-match '((:todo-keyword "DONE")))
         (--map (om-elem-to-trimmed-string it)))
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
    (->> (om-elem-parse-this-subtree)
         (om-elem-match '(section paragraph bold))
         (--map (om-elem-to-trimmed-string it)))
    => '("*text1*" "*text2*")

    (:comment "Use the keyword :any as a wildcard to match any "
              "element at a particular level.")
    (->> (om-elem-parse-this-subtree)
         (om-elem-match '(:any :any bold))
         (--map (om-elem-to-trimmed-string it)))
    => '("*text1*" "*text2*")
    ;; TODO not sure why an empty string comes out here
    (->> (om-elem-parse-this-subtree)
         (om-elem-match '(section paragraph :any))
         (--map (om-elem-to-trimmed-string it)))
    => '("this is" "*text1*" "of" "*text2*" "")
    (->> (om-elem-parse-this-subtree)
         (om-elem-match '(:any bold))
         (--map (om-elem-to-trimmed-string it)))
    => nil

    (:comment "Use the keyword :many to match one or more levels "
              "of any element.")
    (->> (om-elem-parse-this-subtree)
         (om-elem-match '(:many bold))
         (--map (om-elem-to-trimmed-string it)))
    => '("*text1*" "*text2*" "*text3*" "*text4*" "*text5*")

    (:comment "Use the keyword :many! to match one or more levels, "
              "except unlike :many do not match within any elements "
              "that have already matched.")
    ;; TODO add predicate???
    (->> (om-elem-parse-this-subtree)
         (om-elem-match '(headline :many! headline))
         (--map (om-elem-to-trimmed-string it)))
    => '("*** headline three
and here is even more *text4* and *text5*
**** headline 4")

    (:buffer "* headline one"
             "** TODO headline two"
             "** COMMENT headline three"
             "** headline four")
    (:comment "Find the first subheadline")
    (->> (om-elem-parse-this-subtree)
         (om-elem-match '(:first headline))
         (car)
         (om-elem-to-trimmed-string))
    => "** TODO headline two"
    
    (:buffer "* headline one"
             "** TODO headline two"
             "** COMMENT headline three"
             "** headline four")
    (:comment "Find the last subheadline")
    (->> (om-elem-parse-this-subtree)
         (om-elem-match '(:last headline))
         (car)
         (om-elem-to-trimmed-string))
    => "** headline four")

  (defexamples-content om-elem-match-delete
    nil
    (:buffer "* headline one"
             "** headline two"
             "** headline three"
             "** headline four")
    (:comment "Selectively delete headlines")
    (->> (om-elem-parse-this-subtree)
         (om-elem-match-delete '(headline))
         (om-elem-to-trimmed-string))
    => "* headline one"
    (->> (om-elem-parse-this-subtree)
         (om-elem-match-delete '(:first headline))
         (om-elem-to-trimmed-string))
    => (:result "* headline one"
                "** headline three"
                "** headline four")
    (->> (om-elem-parse-this-subtree)
         (om-elem-match-delete '(:last headline))
         (om-elem-to-trimmed-string))
    => (:result "* headline one"
                "** headline two"
                "** headline three"))

  (defexamples-content om-elem-match-extract
    nil
    (:buffer "pull me /under/")
    (--> (om-elem-parse-this-element)
         (om-elem-match-extract '(:many italic) it)
         (cons (-map #'om-elem-to-trimmed-string (car it))
               (om-elem-to-trimmed-string (cdr it))))
    => '(("/under/") . "pull me"))

  (defexamples-content om-elem-match-map
    nil

    (:buffer "* headline one"
             "** TODO headline two"
             "** headline three"
             "** headline four")

    (:comment "Selectively mark headlines as DONE")
    (->> (om-elem-parse-this-subtree)
         (om-elem-match-map '(headline)
           (lambda (it) (om-elem-set-property :todo-keyword "DONE" it)))
         (om-elem-to-trimmed-string))
    => (:result "* headline one"
                "** DONE headline two"
                "** DONE headline three"
                "** DONE headline four")
    (->> (om-elem-parse-this-subtree)
         (om-elem-match-map* '(:first headline)
           (om-elem-set-property :todo-keyword "DONE" it))
         (om-elem-to-trimmed-string))
    => (:result "* headline one"
                "** DONE headline two"
                "** headline three"
                "** headline four")
    (->> (om-elem-parse-this-subtree)
         (om-elem-match-map '(:last headline)
           (-partial #'om-elem-set-property :todo-keyword "DONE"))
         (om-elem-to-trimmed-string))
    => (:result "* headline one"
                "** TODO headline two"
                "** headline three"
                "** DONE headline four"))
  
  (defexamples-content om-elem-match-mapcat
    nil

    (:buffer "* one"
             "** two")
    (->> (om-elem-parse-this-subtree)
         (om-elem-match-mapcat* '(:first headline)
           (list (om-elem-build-headline! :title-text "1.5" :level 2) it))
         (om-elem-to-trimmed-string))
    => (:result "* one"
                "** 1.5"
                "** two"))

  (defexamples-content om-elem-match-replace
    nil
    (:buffer "*1* 2 *3* 4 *5* 6 *7* 8 *9* 10")
    (->> (om-elem-parse-this-element)
         (om-elem-match-replace '(:many bold)
           (om-elem-build-bold :post-blank 1 "0"))
         (om-elem-to-trimmed-string))
    => "*0* 2 *0* 4 *0* 6 *0* 8 *0* 10")

  (defexamples-content om-elem-match-insert-before
    nil
    (:buffer "* one"
             "** two"
             "** three")
    (->> (om-elem-parse-this-subtree)
         (om-elem-match-insert-before '(headline)
           (om-elem-build-headline! :title-text "new" :level 2))
         (om-elem-to-trimmed-string))
    => (:result "* one"
                "** new"
                "** two"
                "** new"
                "** three"))

  (defexamples-content om-elem-match-insert-after
    nil
    (:buffer "* one"
             "** two"
             "** three")
    (->> (om-elem-parse-this-subtree)
         (om-elem-match-insert-after '(headline)
           (om-elem-build-headline! :title-text "new" :level 2))
         (om-elem-to-trimmed-string))
    => (:result "* one"
                "** two"
                "** new"
                "** three"
                "** new"))

  (defexamples-content om-elem-match-insert-within
    nil
    (:buffer "* one"
             "** two"
             "** three")
    (->> (om-elem-parse-this-subtree)
         (om-elem-match-insert-within '(headline) 0
           (om-elem-build-headline! :title-text "new" :level 3))
         (om-elem-to-trimmed-string))
    => (:result "* one"
                "** two"
                "*** new"
                "** three"
                "*** new")
    (:comment "The nil pattern denotes top-level element")
    (->> (om-elem-parse-this-subtree)
         (om-elem-match-insert-within nil 1
           (om-elem-build-headline! :title-text "new" :level 2))
         (om-elem-to-trimmed-string))
    => (:result "* one"
                "** two"
                "** new"
                "** three"))

  ;; add splice

  (defexamples-content om-elem-match-splice-before
    nil
    (:buffer "* one"
             "** two"
             "** three")
    (->> (om-elem-parse-this-subtree)
         (om-elem-match-splice-before '(0)
           (list
            (om-elem-build-headline! :title-text "new0" :level 2)
            (om-elem-build-headline! :title-text "new1" :level 2)))
         (om-elem-to-trimmed-string))
    => (:result "* one"
                "** new0"
                "** new1"
                "** two"
                "** three"))

  (defexamples-content om-elem-match-splice-after
    nil
    (:buffer "* one"
             "** two"
             "** three")
    (->> (om-elem-parse-this-subtree)
         (om-elem-match-splice-after '(0)
           (list
            (om-elem-build-headline! :title-text "new0" :level 2)
            (om-elem-build-headline! :title-text "new1" :level 2)))
         (om-elem-to-trimmed-string))
    => (:result "* one"
                "** two"
                "** new0"
                "** new1"
                "** three"))

  (defexamples-content om-elem-match-splice-within
    nil
    (:buffer "* one"
             "** two"
             "** three")
    (->> (om-elem-parse-this-subtree)
         (om-elem-match-splice-within nil 1
           (list
            (om-elem-build-headline! :title-text "new0" :level 2)
            (om-elem-build-headline! :title-text "new1" :level 2)))
         (om-elem-to-trimmed-string))
    => (:result "* one"
                "** two"
                "** new0"
                "** new1"
                "** three")))

(def-example-group "Buffer Side Effects"
  "Map node manipulations into buffers."

  (def-example-subgroup "Insert"
    nil

    (defexamples-content om-elem-insert
      nil
      (:buffer "* one"
               "")
      (->> (om-elem-build-headline! :title-text "two")
           (om-elem-insert (point-max)))
      $> (:result "* one"
                  "* two")

      (:buffer "a *game* or a /boy/")
      (->> (om-elem-build-paragraph! "we don't care if you're")
           (om-elem-insert (point-min)))
      $> (:result "we don't care if you're"
                  "a *game* or a /boy/"))

    (defexamples-content om-elem-insert-tail
      nil
      :begin-hidden
      (:buffer "* one"
               "")
      (->> (om-elem-build-headline! :title-text "two")
           (om-elem-insert-tail (point-max)))
      $> (:result "* one"
                  "* two")

      (:buffer "a *game* or a /boy/")
      (->> (om-elem-build-paragraph! "we don't care if you're")
           (om-elem-insert-tail (point-min)))
      $> (:result "we don't care if you're"
                  "a *game* or a /boy/")
      :end-hidden))

  (def-example-subgroup "Update"
    nil

    (defexamples-content om-elem-update
      nil
      
      (:buffer "* TODO win grammy")
      (->> (om-elem-parse-this-headline)
           (om-elem-update
            (lambda (hl) (om-elem-set-property :todo-keyword "DONE" hl))))
      $> "* DONE win grammy"

      (:buffer "* win grammy [0/0]"
               "- [ ] write punk song"
               "- [ ] get new vocalist"
               "- [ ] sell 2 singles")
      (->> (om-elem-parse-this-headline)
           (om-elem-update*
             (->> (om-elem-match-map '(:many item) #'om-elem-item-toggle-checkbox it)
                  (om-elem-headline-update-item-statistics))))
      $> (:result "* win grammy [3/3]"
                  "- [X] write punk song"
                  "- [X] get new vocalist"
                  "- [X] sell 2 singles")
      )

    (defexamples-content om-elem-update-object-at
      nil
      (:buffer "[[http://example.com][desc]]")
      (om-elem-update-object-at* (point)
        (om-elem-set-property :path "//buymoreram.com" it))
      $> "[[http://buymoreram.com][desc]]")

    (defexamples-content om-elem-update-element-at
      nil
      (:buffer "#+CALL: ktulu()")
      (om-elem-update-element-at* (point)
        (om-elem-set-properties 
         (list :call "cthulhu"
               :inside-header '(:cache no)
               :arguments '("x=4")
               :end-header '(:results html))
         it))
      $> "#+CALL: cthulhu[:cache no](x=4) :results html")

    (defexamples-content om-elem-update-table-row-at
      nil
      (:buffer "| a | b |")
      (om-elem-update-table-row-at* (point)
        (om-elem-map-children* (cons (om-elem-build-table-cell! "0") it) it))
      $> "| 0 | a | b |")

    (defexamples-content om-elem-update-item-at
      nil
      (:buffer "- [ ] thing")
      (om-elem-update-item-at* (point)
        (om-elem-item-toggle-checkbox it))
      $> "- [X] thing")

    (defexamples-content om-elem-update-headline-at
      nil
      (:buffer "* TODO might get done")
      (om-elem-update-headline-at* (point)
        (om-elem-set-property :todo-keyword "DONE" it))
      $> "* DONE might get done")

    (defexamples-content om-elem-update-subtree-at
      nil
      (:buffer "* one"
               "** two"
               "** three")
      (om-elem-update-subtree-at* (point)
        (om-elem-headline-indent-subheadline 1 it))
      $> (:result "* one"
                  "** two"
                  "*** three")))

  ;; TODO should probably still test these

  ;; (defexamples-content om-elem-update-this-object
  ;;   nil)

  ;; (defexamples-content om-elem-update-this-element
  ;;   nil)
  
  ;; (defexamples-content om-elem-update-this-table-row
  ;;   nil)

  ;; (defexamples-content om-elem-update-this-item
  ;;   nil)

  ;; (defexamples-content om-elem-update-this-headline
  ;;   nil)

  ;; (defexamples-content om-elem-update-this-subtree
  ;;   nil))

  (def-example-subgroup "Misc"
    nil

    (defexamples-content om-elem-fold
      nil)

    (defexamples-content om-elem-unfold
      nil)))
