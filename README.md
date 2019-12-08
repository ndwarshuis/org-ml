# om.el

A functional API for org-mode inspired by
[@magnars](https://github.com/magnars)'s
[dash.el](https://github.com/magnars/dash.el) and
[s.el](https://github.com/magnars/s.el) libraries.

# Installation

This package is not yet in MELPA. To install, clone this repository
somewhere into your load path:

```
git clone https://github.com/ndwarshuis/om.el
```

Then require in your emacs config:

```
(require 'om.el)
```

# Motivation

Org-mode comes with a powerful, built-in parse-tree generator
specified in `org-element.el`. The generated parse-tree is simply a
heavily-nested list which can be easily manipulated using (mostly
pure) functional code. This contrasts the majority of functions
normally used to interface with org-mode files, which are imperative
in nature (`org-insert-headine`, `outline-next-heading`, etc) as they
depend on the mutable state of Emacs buffers. In general, functional
code is
([arguably](https://en.wikipedia.org/wiki/Functional_programming#Comparison_to_imperative_programming))
more robust, readable, and testable, especially in use-cases such as
this where a stateless abstract data structure is being transformed
and queried.

The `org-element.el` provides a minimal API for handling this
parse-tree in a functional manner, but lacks many of the higher-level
functions necessary for intuitive, large-scale use. The `om.el`
package is designed to provide this API. Furthermore, it is highly
compatible with the `dash.el` package, which is a generalized
functional library for emacs-lisp.

# Org-Element Overview

Parsing a buffer with the function `org-element-parse-buffer` will
yield a parse-tree composed of leaves and branches.

The leaves can be one of two broad categories:
- Objects: roughly correspond to raw, possibly-formatted text
  (plain-text, timestamp, code, etc)
- Elements: more complex structures, some of which may have objects in
  their properties (clock, planning, src-blocks, etc)

Each leaf is represented by a list where the first member is the type
and the second member is a plist describing the element or object's
properties:

``` emacs-lisp
(type (:prop1 value1 :prop2 value2 ...))
```

The branches with the leaves come in three
broad types:
- Recursive objects: objects that contain other objects
- Object Containers: elements that contain other objects
- Greater elements: elements that contain other elements

Branches of the tree have almost the same list structure as leaves; in
addition to the type and plist, each child is appended at the end:
  
``` emacs-lisp
(type (:prop1 value1 :prop2 value2) child1 child2 ...)
```

In summary, this implies the following hierarchy of types that
can validly fit into each other in the buffer parse-tree:
- Greater Elements
  - Greater Elements
  - Elements
  - Object Containers
    - Objects
    - Recursive Objects
      - Recursive Objects
      - Objects
      
More information and a full list of elements, objects and their
compatible properties can be found in [the org-element API
documentation](https://orgmode.org/worg/dev/org-element-api.html).

# Conventions

### Threading

Each function that operates on an element/object will take the
element/object as its right-most argument. This allows convenient
function chaining using `dash.el`'s right-threading operators (`->>`
and `-some->>`). The examples below almost exclusively demonstrate
this pattern. Additionally, the right-argument convention also allows
convenient partial application using `-partial` from `dash.el`.

### Higher-order functions

Higher-order functions (functions that take other functions as
arguments) have two forms. The first takes a (usually unary) function
and applies it:

``` emacs-lisp
(om-elem-map-property :value (lambda (s) (concat "foo" s)) elem)
(om-elem-map-property :value (-partial concat "foo") elem)
```

This can equivalently be written using an anaphoric form where the
original function name is appended with `*`. The symbol `it`
carries the value of the unary argument (unless otherwise specified):

``` emacs-lisp
(om-elem-map-property* :value (concat "foo" it) elem)
```

### Side effect functions

All functions that read and write from buffers are named like
`om-elem-OPERATION-THING-at` where `OPERATION` is some operation to be
performed on `THING` in the current buffer. All these functions take
`point` as one of their arguments to denote where in the buffer to
perform `OPERATION`.

All of these functions have current-point convenience analogues that
are named as `om-elem-OPERATION-this-THING` where `OPERATION` and
`THING` carry the same meaning, but `OPERATION` is done at the current
point and `point` is not an argument to the function.

For the sake of brevity, only the former form of these functions are
given in the examples below.

# Function Summary


## Buffer Parsing


Parse buffers to trees.

* [om-elem-parse-object-at](#om-elem-parse-object-at-point) `(point)`
* [om-elem-parse-element-at](#om-elem-parse-element-at-point) `(point)`
* [om-elem-parse-headline-at](#om-elem-parse-headline-at-point) `(point)`
* [om-elem-parse-subtree-at](#om-elem-parse-subtree-at-point) `(point)`
* [om-elem-parse-item-at](#om-elem-parse-item-at-point) `(point)`
* [om-elem-parse-table-row-at](#om-elem-parse-table-row-at-point) `(point)`
* [om-elem-parse-section-at](#om-elem-parse-section-at-point) `(point)`

## String Conversion


Convert nodes to strings.

* [om-elem-to-string](#om-elem-to-string-node) `(node)`
* [om-elem-to-trimmed-string](#om-elem-to-trimmed-string-node) `(node)`

## Building


Build new nodes.


### Leaf Objects

* [om-elem-build-code](#om-elem-build-code-value-key-post-blank) `(value &key post-blank)`
* [om-elem-build-entity](#om-elem-build-entity-name-key-use-brackets-p-post-blank) `(name &key use-brackets-p post-blank)`
* [om-elem-build-inline-babel-call](#om-elem-build-inline-babel-call-call-key-inside-header-arguments-end-header-post-blank) `(call &key inside-header arguments end-header post-blank)`
* [om-elem-build-inline-src-block](#om-elem-build-inline-src-block-language-value-key-parameters-post-blank) `(language value &key parameters post-blank)`
* [om-elem-build-line-break](#om-elem-build-line-break-key-post-blank) `(&key post-blank)`
* [om-elem-build-statistics-cookie](#om-elem-build-statistics-cookie-value-key-post-blank) `(value &key post-blank)`
* [om-elem-build-target](#om-elem-build-target-value-key-post-blank) `(value &key post-blank)`
* [om-elem-build-timestamp](#om-elem-build-timestamp-type-year-start-month-start-day-start-year-end-month-end-day-end-key-hour-start-minute-start-hour-end-minute-end-repeater-type-repeater-unit-repeater-value-warning-type-warning-unit-warning-value-post-blank) `(type year-start month-start day-start year-end month-end day-end &key hour-start minute-start hour-end minute-end repeater-type repeater-unit repeater-value warning-type warning-unit warning-value post-blank)`
* [om-elem-build-verbatim](#om-elem-build-verbatim-value-key-post-blank) `(value &key post-blank)`

### Branch Objects

* [om-elem-build-bold](#om-elem-build-bold-key-post-blank-rest-objs) `(&key post-blank &rest objs)`
* [om-elem-build-footnote-reference](#om-elem-build-footnote-reference-key-label-post-blank-rest-objs) `(&key label post-blank &rest objs)`
* [om-elem-build-italic](#om-elem-build-italic-key-post-blank-rest-objs) `(&key post-blank &rest objs)`
* [om-elem-build-link](#om-elem-build-link-path-key-format-type-fuzzy-post-blank-rest-objs) `(path &key format (type fuzzy) post-blank &rest objs)`
* [om-elem-build-radio-target](#om-elem-build-radio-target-key-post-blank-rest-objs) `(&key post-blank &rest objs)`
* [om-elem-build-strike-through](#om-elem-build-strike-through-key-post-blank-rest-objs) `(&key post-blank &rest objs)`
* [om-elem-build-superscript](#om-elem-build-superscript-key-use-brackets-p-post-blank-rest-objs) `(&key use-brackets-p post-blank &rest objs)`
* [om-elem-build-subscript](#om-elem-build-subscript-key-use-brackets-p-post-blank-rest-objs) `(&key use-brackets-p post-blank &rest objs)`
* [om-elem-build-table-cell](#om-elem-build-table-cell-key-post-blank-rest-objs) `(&key post-blank &rest objs)`
* [om-elem-build-underline](#om-elem-build-underline-key-post-blank-rest-objs) `(&key post-blank &rest objs)`

### Leaf Elements

* [om-elem-build-babel-call](#om-elem-build-babel-call-call-key-inside-header-arguments-end-header-post-blank) `(call &key inside-header arguments end-header post-blank)`
* [om-elem-build-clock](#om-elem-build-clock-value-key-post-blank) `(value &key post-blank)`
* [om-elem-build-comment](#om-elem-build-comment-value-key-post-blank) `(value &key post-blank)`
* [om-elem-build-comment-block](#om-elem-build-comment-block-value-key-post-blank) `(value &key post-blank)`
* [om-elem-build-diary-sexp](#om-elem-build-diary-sexp-value-key-post-blank) `(value &key post-blank)`
* [om-elem-build-example-block](#om-elem-build-example-block-value-key-preserve-indent-switches-post-blank) `(value &key preserve-indent switches post-blank)`
* [om-elem-build-export-block](#om-elem-build-export-block-type-value-key-post-blank) `(type value &key post-blank)`
* [om-elem-build-fixed-width](#om-elem-build-fixed-width-value-key-post-blank) `(value &key post-blank)`
* [om-elem-build-horizontal-rule](#om-elem-build-horizontal-rule-key-post-blank) `(&key post-blank)`
* [om-elem-build-keyword](#om-elem-build-keyword-key-value-key-post-blank) `(key value &key post-blank)`
* [om-elem-build-latex-environment](#om-elem-build-latex-environment-value-key-post-blank) `(value &key post-blank)`
* [om-elem-build-node-property](#om-elem-build-node-property-key-value-key-post-blank) `(key value &key post-blank)`
* [om-elem-build-planning](#om-elem-build-planning-key-closed-deadline-scheduled-post-blank) `(&key closed deadline scheduled post-blank)`
* [om-elem-build-src-block](#om-elem-build-src-block-value-key-language-parameters-preserve-indent-switches-post-blank) `(value &key language parameters preserve-indent switches post-blank)`
* [om-elem-build-table-row-hline](#om-elem-build-table-row-hline-key-post-blank) `(&key post-blank)`

### Branch Elements with Child Objects

* [om-elem-build-paragraph](#om-elem-build-paragraph-key-post-blank-rest-objs) `(&key post-blank &rest objs)`
* [om-elem-build-table-row](#om-elem-build-table-row-key-post-blank-rest-objs) `(&key post-blank &rest objs)`
* [om-elem-build-verse-block](#om-elem-build-verse-block-key-post-blank-rest-objs) `(&key post-blank &rest objs)`

### Branch Elements with Child Elements

* [om-elem-build-center-block](#om-elem-build-center-block-key-post-blank-rest-nodes) `(&key post-blank &rest nodes)`
* [om-elem-build-drawer](#om-elem-build-drawer-drawer-name-key-post-blank-rest-nodes) `(drawer-name &key post-blank &rest nodes)`
* [om-elem-build-footnote-definition](#om-elem-build-footnote-definition-label-key-post-blank-rest-nodes) `(label &key post-blank &rest nodes)`
* [om-elem-build-headline](#om-elem-build-headline-key-archivedp-commentedp-footnote-section-p-level-1-pre-blank-0-priority-tags-title-todo-keyword-post-blank-rest-nodes) `(&key archivedp commentedp footnote-section-p (level 1) (pre-blank 0) priority tags title todo-keyword post-blank &rest nodes)`
* [om-elem-build-item](#om-elem-build-item-key-bullet-quote---checkbox-counter-tag-post-blank-rest-nodes) `(&key (bullet '-) checkbox counter tag post-blank &rest nodes)`
* [om-elem-build-plain-list](#om-elem-build-plain-list-key-post-blank-rest-nodes) `(&key post-blank &rest nodes)`
* [om-elem-build-property-drawer](#om-elem-build-property-drawer-key-post-blank-rest-nodes) `(&key post-blank &rest nodes)`
* [om-elem-build-quote-block](#om-elem-build-quote-block-key-post-blank-rest-nodes) `(&key post-blank &rest nodes)`
* [om-elem-build-section](#om-elem-build-section-key-post-blank-rest-nodes) `(&key post-blank &rest nodes)`
* [om-elem-build-table](#om-elem-build-table-key-tblfm-post-blank-rest-nodes) `(&key tblfm post-blank &rest nodes)`

### Miscellaneous Builders

* [om-elem-build-timestamp-diary-sexp](#om-elem-build-timestamp-diary-sexp-form-key-post-blank) `(form &key post-blank)`
* [om-elem-build-table-row-hline](#om-elem-build-table-row-hline-key-post-blank) `(&key post-blank)`

### Shorthand Builders


Build nodes with more convenient/shorter syntax.

* [om-elem-build-timestamp!](#om-elem-build-timestamp-type-start-key-end-repeater-warning-post-blank) `(type start &key end repeater warning post-blank)`
* [om-elem-build-clock!](#om-elem-build-clock-start-key-end-post-blank) `(start &key end post-blank)`
* [om-elem-build-planning!](#om-elem-build-planning-key-closed-deadline-scheduled-post-blank) `(&key closed deadline scheduled post-blank)`
* [om-elem-build-property-drawer!](#om-elem-build-property-drawer-key-post-blank-rest-keyvals) `(&key post-blank &rest keyvals)`
* [om-elem-build-headline!](#om-elem-build-headline-key-level-1-title-text-todo-keyword-tags-pre-blank-priority-commentedp-archivedp-post-blank-planning-properties-statistics-cookie-section-contents-rest-subheadlines) `(&key (level 1) title-text todo-keyword tags pre-blank priority commentedp archivedp post-blank planning properties statistics-cookie section-contents &rest subheadlines)`
* [om-elem-build-item!](#om-elem-build-item-key-post-blank-bullet-checkbox-tag-paragraph-counter-rest-subitems) `(&key post-blank bullet checkbox tag paragraph counter &rest subitems)`
* [om-elem-build-paragraph!](#om-elem-build-paragraph-string-key-post-blank) `(string &key post-blank)`
* [om-elem-build-table!](#om-elem-build-table-key-tblfm-post-blank-rest-row-lists) `(&key tblfm post-blank &rest row-lists)`

## Type Predicates


Test node types.

* [om-elem-is-type-p](#om-elem-is-type-p-type-node) `(type node)`
* [om-elem-is-any-type-p](#om-elem-is-any-type-p-types-node) `(types node)`
* [om-elem-is-element-p](#om-elem-is-element-p-node) `(node)`
* [om-elem-is-container-p](#om-elem-is-container-p-node) `(node)`
* [om-elem-is-object-container-p](#om-elem-is-object-container-p-node) `(node)`
* [om-elem-is-greater-element-p](#om-elem-is-greater-element-p-node) `(node)`

## Property Manipulation


Set, get, and map properties of nodes.


### Generic

* [om-elem-set-property](#om-elem-set-property-prop-value-node) `(prop value node)`
* [om-elem-set-properties](#om-elem-set-properties-plist-node) `(plist node)`
* [om-elem-get-property](#om-elem-get-property-prop-node) `(prop node)`
* [om-elem-map-property](#om-elem-map-property-prop-fun-node) `(prop fun node)`
* [om-elem-map-properties](#om-elem-map-properties-plist-node) `(plist node)`
* [om-elem-toggle-property](#om-elem-toggle-property-prop-node) `(prop node)`
* [om-elem-shift-property](#om-elem-shift-property-prop-n-node) `(prop n node)`
* [om-elem-insert-into-property](#om-elem-insert-into-property-prop-index-string-node) `(prop index string node)`
* [om-elem-remove-from-property](#om-elem-remove-from-property-prop-string-node) `(prop string node)`
* [om-elem-plist-put-property](#om-elem-plist-put-property-prop-key-value-node) `(prop key value node)`
* [om-elem-plist-remove-property](#om-elem-plist-remove-property-prop-key-node) `(prop key node)`

### Clock

* [om-elem-clock-is-running-p](#om-elem-clock-is-running-p-clock) `(clock)`
* [om-elem-clock-map-timestamp](#om-elem-clock-map-timestamp-fun-clock) `(fun clock)`

### Headline

* [om-elem-headline-is-done-p](#om-elem-headline-is-done-p-headline) `(headline)`
* [om-elem-headline-is-archived-p](#om-elem-headline-is-archived-p-headline) `(headline)`
* [om-elem-headline-is-commented-p](#om-elem-headline-is-commented-p-headline) `(headline)`
* [om-elem-headline-has-tag-p](#om-elem-headline-has-tag-p-tag-headline) `(tag headline)`
* [om-elem-headline-get-statistics-cookie](#om-elem-headline-get-statistics-cookie-headline) `(headline)`

### Item

* [om-elem-item-is-unchecked-p](#om-elem-item-is-unchecked-p-item) `(item)`
* [om-elem-item-is-checked-p](#om-elem-item-is-checked-p-item) `(item)`
* [om-elem-item-is-trans-p](#om-elem-item-is-trans-p-item) `(item)`
* [om-elem-item-toggle-checkbox](#om-elem-item-toggle-checkbox-item) `(item)`

### Planning

* [om-elem-planning-set-timestamp](#om-elem-planning-set-timestamp-prop-planning-list-planning) `(prop planning-list planning)`
* [om-elem-planning-map-timestamp](#om-elem-planning-map-timestamp-prop-fun-planning) `(prop fun planning)`

### Statistics Cookie

* [om-elem-statistics-cookie-is-complete-p](#om-elem-statistics-cookie-is-complete-p-statistics-cookie) `(statistics-cookie)`

### Timestamp

* [om-elem-timestamp-get-start-time](#om-elem-timestamp-get-start-time-timestamp) `(timestamp)`
* [om-elem-timestamp-get-end-time](#om-elem-timestamp-get-end-time-timestamp) `(timestamp)`
* [om-elem-timestamp-is-active-p](#om-elem-timestamp-is-active-p-timestamp) `(timestamp)`
* [om-elem-timestamp-is-ranged-p](#om-elem-timestamp-is-ranged-p-timestamp) `(timestamp)`
* [om-elem-timestamp-set-start-time](#om-elem-timestamp-set-start-time-time-timestamp) `(time timestamp)`
* [om-elem-timestamp-set-end-time](#om-elem-timestamp-set-end-time-time-timestamp) `(time timestamp)`
* [om-elem-timestamp-set-single-time](#om-elem-timestamp-set-single-time-time-timestamp) `(time timestamp)`
* [om-elem-timestamp-set-double-time](#om-elem-timestamp-set-double-time-time1-time2-timestamp) `(time1 time2 timestamp)`
* [om-elem-timestamp-set-range](#om-elem-timestamp-set-range-range-timestamp) `(range timestamp)`
* [om-elem-timestamp-set-type](#om-elem-timestamp-set-type-type-timestamp) `(type timestamp)`
* [om-elem-timestamp-shift](#om-elem-timestamp-shift-n-unit-timestamp) `(n unit timestamp)`
* [om-elem-timestamp-shift-start](#om-elem-timestamp-shift-start-n-unit-timestamp) `(n unit timestamp)`
* [om-elem-timestamp-shift-end](#om-elem-timestamp-shift-end-n-unit-timestamp) `(n unit timestamp)`
* [om-elem-timestamp-toggle-active](#om-elem-timestamp-toggle-active-timestamp) `(timestamp)`

## Branch/Child Manipulation


Set, get and map the children of branch nodes.


### Generic

* [om-elem-get-contents](#om-elem-get-contents-node) `(node)`
* [om-elem-set-contents](#om-elem-set-contents-contents-node) `(contents node)`
* [om-elem-map-contents](#om-elem-map-contents-fun-node) `(fun node)`
* [om-elem-is-empty-p](#om-elem-is-empty-p-node) `(node)`

### Headline

* [om-elem-headline-update-item-statistics](#om-elem-headline-update-item-statistics-headline) `(headline)`
* [om-elem-headline-update-todo-statistics](#om-elem-headline-update-todo-statistics-headline) `(headline)`
* [om-elem-headline-indent-subheadline](#om-elem-headline-indent-subheadline-index-headline) `(index headline)`
* [om-elem-headline-indent-subtree](#om-elem-headline-indent-subtree-index-headline) `(index headline)`
* [om-elem-headline-unindent-subheadline](#om-elem-headline-unindent-subheadline-index-child-index-headline) `(index child-index headline)`
* [om-elem-headline-unindent-subtree](#om-elem-headline-unindent-subtree-index-headline) `(index headline)`

### Plain List

* [om-elem-plain-list-set-type](#om-elem-plain-list-set-type-type-plain-list) `(type plain-list)`
* [om-elem-plain-list-indent-item](#om-elem-plain-list-indent-item-index-plain-list) `(index plain-list)`
* [om-elem-plain-list-indent-item-tree](#om-elem-plain-list-indent-item-tree-index-plain-list) `(index plain-list)`
* [om-elem-plain-list-unindent-item](#om-elem-plain-list-unindent-item-index-child-index-plain-list) `(index child-index plain-list)`
* [om-elem-plain-list-unindent-items](#om-elem-plain-list-unindent-items-index-plain-list) `(index plain-list)`

### Table

* [om-elem-table-get-cell](#om-elem-table-get-cell-row-index-column-index-table) `(row-index column-index table)`
* [om-elem-table-replace-cell](#om-elem-table-replace-cell-row-index-column-index-cell-table) `(row-index column-index cell table)`
* [om-elem-table-replace-cell!](#om-elem-table-replace-cell-row-index-column-index-cell-text-table) `(row-index column-index cell-text table)`
* [om-elem-table-clear-cell](#om-elem-table-clear-cell-row-index-column-index-table) `(row-index column-index table)`
* [om-elem-table-delete-column](#om-elem-table-delete-column-column-index-table) `(column-index table)`
* [om-elem-table-replace-column](#om-elem-table-replace-column-column-index-column-cells-table) `(column-index column-cells table)`
* [om-elem-table-replace-column!](#om-elem-table-replace-column-column-index-column-text-table) `(column-index column-text table)`
* [om-elem-table-clear-column](#om-elem-table-clear-column-column-index-table) `(column-index table)`
* [om-elem-table-replace-row](#om-elem-table-replace-row-row-index-row-cells-table) `(row-index row-cells table)`
* [om-elem-table-replace-row!](#om-elem-table-replace-row-row-index-row-text-table) `(row-index row-text table)`
* [om-elem-table-clear-row](#om-elem-table-clear-row-row-index-table) `(row-index table)`
* [om-elem-table-delete-row](#om-elem-table-delete-row-row-index-table) `(row-index table)`
* [om-elem-table-insert-column](#om-elem-table-insert-column-column-index-column-cells-table) `(column-index column-cells table)`
* [om-elem-table-insert-column!](#om-elem-table-insert-column-column-index-column-text-table) `(column-index column-text table)`
* [om-elem-table-insert-row](#om-elem-table-insert-row-row-index-row-table) `(row-index row table)`
* [om-elem-table-insert-row!](#om-elem-table-insert-row-row-index-row-text-table) `(row-index row-text table)`

## Node Matching


Use pattern-matching to selectively perform operations on nodes in trees.

* [om-elem-match](#om-elem-match-pattern-node) `(pattern node)`
* [om-elem-match-delete](#om-elem-match-delete-pattern-node) `(pattern node)`
* [om-elem-match-extract](#om-elem-match-extract-pattern-node) `(pattern node)`
* [om-elem-match-map](#om-elem-match-map-pattern-fun-node) `(pattern fun node)`
* [om-elem-match-mapcat](#om-elem-match-mapcat-pattern-fun-node) `(pattern fun node)`
* [om-elem-match-replace](#om-elem-match-replace-pattern-node-node) `(pattern node* node)`
* [om-elem-match-insert-before](#om-elem-match-insert-before-pattern-node-node) `(pattern node* node)`
* [om-elem-match-insert-after](#om-elem-match-insert-after-pattern-node-node) `(pattern node* node)`
* [om-elem-match-insert-within](#om-elem-match-insert-within-pattern-index-node-node) `(pattern index node* node)`
* [om-elem-match-splice-before](#om-elem-match-splice-before-pattern-nodes-node) `(pattern nodes* node)`
* [om-elem-match-splice-after](#om-elem-match-splice-after-pattern-nodes-node) `(pattern nodes* node)`
* [om-elem-match-splice-within](#om-elem-match-splice-within-pattern-index-nodes-node) `(pattern index nodes* node)`

## Buffer Side Effects


Insert and update elements and objects into buffers.


### Insert

* [om-elem-insert](#om-elem-insert-point-node) `(point node)`
* [om-elem-insert-tail](#om-elem-insert-tail-point-node) `(point node)`

### Update

* [om-elem-update](#om-elem-update-fun-node) `(fun node)`
* [om-elem-update-object-at](#om-elem-update-object-at-point-fun) `(point fun)`
* [om-elem-update-element-at](#om-elem-update-element-at-point-fun) `(point fun)`
* [om-elem-update-table-row-at](#om-elem-update-table-row-at-point-fun) `(point fun)`
* [om-elem-update-item-at](#om-elem-update-item-at-point-fun) `(point fun)`
* [om-elem-update-headline-at](#om-elem-update-headline-at-point-fun) `(point fun)`
* [om-elem-update-subtree-at](#om-elem-update-subtree-at-point-fun) `(point fun)`

### Misc

* [om-elem-fold-contents](#om-elem-fold-contents-node) `(node)`
* [om-elem-unfold-contents](#om-elem-unfold-contents-node) `(node)`

# Function Examples


## Buffer Parsing


Parse buffers to trees.

#### om-elem-parse-object-at `(point)`

Return the object tree under `point` or nil if not on an object.

If `type` is supplied, only return nil if the object under point is
not of that type. `type` is a symbol from `om-elem-objects`.

```el
;; Given the following contents:
; *text*

(->> (om-elem-parse-object-at 1)
     (om-elem-get-type))
 ;; => 'bold

;; Given the following contents:
; [2019-01-01 Tue]

(->> (om-elem-parse-object-at 1)
     (om-elem-get-type))
 ;; => 'timestamp

;; Given the following contents:
; - notme

;; Return nil when parsing an element
(om-elem-parse-object-at
 1)
 ;; => nil

```

#### om-elem-parse-element-at `(point)`

Return element under `point` or nil if not on an element.

This function will return every element available in `om-elem-elements`
with the exception of 'section', 'item', and 'table-row'. To
specifically parse these, use the functions `om-elem-parse-section-at',
`om-elem-parse-item-at`, and [`om-elem-parse-table-row-at`](#om-elem-parse-table-row-at-point).

```el
;; Given the following contents:
; #+CALL: ktulu()

(->> (om-elem-parse-element-at 1)
     (om-elem-get-type))
 ;; => 'babel-call

;; Given the following contents:
; - plain-list

;; Give the plain-list, not the item for this function
(->> (om-elem-parse-element-at 1)
     (om-elem-get-type))
 ;; => 'plain-list

;; Given the following contents:
; | R | A |
; | G | E |

;; Return a table, not the table-row for this function
(->> (om-elem-parse-element-at 1)
     (om-elem-get-type))
 ;; => 'table

```

#### om-elem-parse-headline-at `(point)`

Return headline tree under `point` or nil if not on a headline.
`point` does not need to be on the headline itself. Only the headline
and its section will be returned. To include subheadlines, use
`om-elem-parse-headline-subtree-at`.

```el
;; Given the following contents:
; * headline

;; Return the headline itself
(->> (om-elem-parse-headline-at 1)
     (om-elem-to-trimmed-string))
 ;; => "* headline"

;; Given the following contents:
; * headline
; section crap

;; Return headline and section
(->> (om-elem-parse-headline-at 1)
     (om-elem-to-trimmed-string))
 ;; => "* headline
 ;      section crap"

;; Return headline when point is in the section
(->> (om-elem-parse-headline-at 12)
     (om-elem-to-trimmed-string))
 ;; => "* headline
 ;      section crap"

;; Given the following contents:
; * headline
; section crap
; ** not parsed

;; Don't parse any subheadlines
(->> (om-elem-parse-headline-at 1)
     (om-elem-to-trimmed-string))
 ;; => "* headline
 ;      section crap"

;; Given the following contents:
; nothing nowhere

;; Return nil if not under a headline
(->> (om-elem-parse-headline-at 1)
     (om-elem-to-trimmed-string))
 ;; => ""

```

#### om-elem-parse-subtree-at `(point)`

Return headline tree under `point` or nil if not on a headline.
`point` does not need to be on the headline itself. Unlike
[`om-elem-parse-headline-at`](#om-elem-parse-headline-at-point), the returned tree will include
subheadlines.

```el
;; Given the following contents:
; * headline

;; Return the headline itself
(->> (om-elem-parse-subtree-at 1)
     (om-elem-to-trimmed-string))
 ;; => "* headline"

;; Given the following contents:
; * headline
; section crap

;; Return headline and section
(->> (om-elem-parse-subtree-at 1)
     (om-elem-to-trimmed-string))
 ;; => "* headline
 ;      section crap"

;; Return headline when point is in the section
(->> (om-elem-parse-subtree-at 12)
     (om-elem-to-trimmed-string))
 ;; => "* headline
 ;      section crap"

;; Given the following contents:
; * headline
; section crap
; ** parsed

;; Return all the subheadlines
(->> (om-elem-parse-subtree-at 1)
     (om-elem-to-trimmed-string))
 ;; => "* headline
 ;      section crap
 ;      ** parsed"

;; Given the following contents:
; nothing nowhere

;; Return nil if not under a headline
(->> (om-elem-parse-subtree-at 1)
     (om-elem-to-trimmed-string))
 ;; => ""

```

#### om-elem-parse-item-at `(point)`

Return item element under `point` or nil if not on an item.
This will return the item even if `point` is not at the beginning of
the line.

```el
;; Given the following contents:
; - item

;; Return the item itself
(->> (om-elem-parse-item-at 1)
     (om-elem-to-trimmed-string))
 ;; => "- item"

;; Also return the item when not at beginning of line
(->> (om-elem-parse-item-at 5)
     (om-elem-to-trimmed-string))
 ;; => "- item"

;; Given the following contents:
; - item
;   - item 2

;; Return item and its subitems
(->> (om-elem-parse-item-at 1)
     (om-elem-to-trimmed-string))
 ;; => "- item
 ;        - item 2"

;; Given the following contents:
; * not item

;; Return nil if not an item
(->> (om-elem-parse-item-at 1)
     (om-elem-to-trimmed-string))
 ;; => ""

```

#### om-elem-parse-table-row-at `(point)`

Return table-row element under `point` or nil if not on a table-row.

```el
;; Given the following contents:
; | bow | stroke |

;; Return the row itself
(->> (om-elem-parse-table-row-at 1)
     (om-elem-to-trimmed-string))
 ;; => "| bow | stroke |"

;; Also return the row when not at beginning of line
(->> (om-elem-parse-table-row-at 5)
     (om-elem-to-trimmed-string))
 ;; => "| bow | stroke |"

;; Given the following contents:
; - bow and arrow choke

;; Return nil if not a table-row
(->> (om-elem-parse-table-row-at 1)
     (om-elem-to-trimmed-string))
 ;; => ""

```

#### om-elem-parse-section-at `(point)`

Return tree of the section under `point` or nil if not on a section.
If `point` is on or within a headline, return the section under that
headline. If `point` is before the first headline (if any), return
the section at the top of the org buffer.

```el
;; Given the following contents:
; over headline
; * headline
; under headline

;; Return the section above the headline
(->> (om-elem-parse-section-at 1)
     (om-elem-to-trimmed-string))
 ;; => "over headline"

;; Return the section under headline
(->> (om-elem-parse-section-at 25)
     (om-elem-to-trimmed-string))
 ;; => "under headline"

;; Given the following contents:
; * headline
; ** subheadline

;; Return nil if no section under headline
(->> (om-elem-parse-section-at 1)
     (om-elem-to-trimmed-string))
 ;; => ""

;; Given the following contents:
; 

;; Return nil if no section at all
(->> (om-elem-parse-section-at 1)
     (om-elem-to-trimmed-string))
 ;; => ""

```


## String Conversion


Convert nodes to strings.

#### om-elem-to-string `(node)`

Return `node` as an interpreted string without text properties.

```el
(om-elem-to-string '(bold (:begin 1 :end 5 :parent nil :post-blank 0 :post-affiliated nil)
				"text"))
 ;; => "*text*"

(om-elem-to-string '(bold (:begin 1 :end 5 :parent nil :post-blank 3 :post-affiliated nil)
				"text"))
 ;; => "*text*   "

(om-elem-to-string nil)
 ;; => ""

```

#### om-elem-to-trimmed-string `(node)`

Like [`om-elem-to-string`](#om-elem-to-string-node) but strip whitespace when returning `node`.

```el
(om-elem-to-trimmed-string '(bold (:begin 1 :end 5 :parent nil :post-blank 0 :post-affiliated nil)
					"text"))
 ;; => "*text*"

(om-elem-to-trimmed-string '(bold (:begin 1 :end 5 :parent nil :post-blank 3 :post-affiliated nil)
					"text"))
 ;; => "*text*"

(om-elem-to-trimmed-string nil)
 ;; => ""

```


## Building


Build new nodes.


### Leaf Objects

#### om-elem-build-code `(value &key post-blank)`

Build a code object.

The following properties are settable:
- `value`: a oneline string
- `post-blank`: a non-negative integer

```el
(->> (om-elem-build-code "text")
     (om-elem-to-trimmed-string))
 ;; => "~text~"

```

#### om-elem-build-entity `(name &key use-brackets-p post-blank)`

Build a entity object.

The following properties are settable:
- `name`: a string that makes `org-entity-get` return non-nil
- `use-brackets-p`: nil or t
- `post-blank`: a non-negative integer

```el
(->> (om-elem-build-entity "gamma")
     (om-elem-to-trimmed-string))
 ;; => "\\gamma"

```

#### om-elem-build-inline-babel-call `(call &key inside-header arguments end-header post-blank)`

Build a inline-babel-call object.

The following properties are settable:
- `call`: a oneline string
- `inside-header`: a plist
- `arguments`: a list of oneline strings
- `end-header`: a plist
- `post-blank`: a non-negative integer

```el
(->> (om-elem-build-inline-babel-call "name")
     (om-elem-to-trimmed-string))
 ;; => "call_name()"

(->> (om-elem-build-inline-babel-call "name" :arguments '("n=4"))
     (om-elem-to-trimmed-string))
 ;; => "call_name(n=4)"

(->> (om-elem-build-inline-babel-call "name" :inside-header '(:key val))
     (om-elem-to-trimmed-string))
 ;; => "call_name[:key val]()"

(->> (om-elem-build-inline-babel-call "name" :end-header '(:key val))
     (om-elem-to-trimmed-string))
 ;; => "call_name()[:key val]"

```

#### om-elem-build-inline-src-block `(language value &key parameters post-blank)`

Build a inline-src-block object.

The following properties are settable:
- `language`: a oneline string
- `value`: a oneline string
- `parameters`: a plist
- `post-blank`: a non-negative integer

```el
(->> (om-elem-build-inline-src-block "lang" "value")
     (om-elem-to-trimmed-string))
 ;; => "src_lang{value}"

(->> (om-elem-build-inline-src-block "lang" "value" :parameters '(:key val))
     (om-elem-to-trimmed-string))
 ;; => "src_lang[:key val]{value}"

```

#### om-elem-build-line-break `(&key post-blank)`

Build a line-break object.

The following properties are settable:

- `post-blank`: a non-negative integer

```el
(->> (om-elem-build-line-break)
     (om-elem-to-trimmed-string))
 ;; => "\\\\"

```

#### om-elem-build-statistics-cookie `(value &key post-blank)`

Build a statistics-cookie object.

The following properties are settable:
- `value`: a list of non-neg integers like (`perc`) or (`num` `den`) which make [`num`/`den`] and [`perc` %] respectively
- `post-blank`: a non-negative integer

```el
(->> (om-elem-build-statistics-cookie '(nil))
     (om-elem-to-trimmed-string))
 ;; => "[%]"

(->> (om-elem-build-statistics-cookie '(50))
     (om-elem-to-trimmed-string))
 ;; => "[50%]"

(->> (om-elem-build-statistics-cookie '(1 3))
     (om-elem-to-trimmed-string))
 ;; => "[1/3]"

(->> (om-elem-build-statistics-cookie '(nil nil))
     (om-elem-to-trimmed-string))
 ;; => "[/]"

```

#### om-elem-build-target `(value &key post-blank)`

Build a target object.

The following properties are settable:
- `value`: a oneline string
- `post-blank`: a non-negative integer

```el
(->> (om-elem-build-target "text")
     (om-elem-to-trimmed-string))
 ;; => "<<text>>"

```

#### om-elem-build-timestamp `(type year-start month-start day-start year-end month-end day-end &key hour-start minute-start hour-end minute-end repeater-type repeater-unit repeater-value warning-type warning-unit warning-value post-blank)`

Build a timestamp object.

The following properties are settable:
- `type`: a symbol from 'inactive', 'active', 'inactive-ranged', or 'active-ranged'
- `year-start`: a positive integer
- `month-start`: a positive integer
- `day-start`: a positive integer
- `year-end`: a positive integer
- `month-end`: a positive integer
- `day-end`: a positive integer
- `hour-start`: a non-negative integer or nil
- `minute-start`: a non-negative integer or nil
- `hour-end`: a non-negative integer or nil
- `minute-end`: a non-negative integer or nil
- `repeater-type`: nil or a symbol from 'catch-up', 'restart', or 'cumulate'
- `repeater-unit`: nil or a symbol from 'year' 'month' 'week' 'day', or 'hour'
- `repeater-value`: a positive integer or nil
- `warning-type`: nil or a symbol from 'all' or 'first'
- `warning-unit`: nil or a symbol from 'year' 'month' 'week' 'day', or 'hour'
- `warning-value`: a positive integer or nil
- `post-blank`: a non-negative integer

```el
(->> (om-elem-build-timestamp 'inactive
			      2019 1 15 2019 1 15)
     (om-elem-to-trimmed-string))
 ;; => "[2019-01-15 Tue]"

(->> (om-elem-build-timestamp 'active-range
			      2019 1 15 2019 1 16)
     (om-elem-to-trimmed-string))
 ;; => "<2019-01-15 Tue>--<2019-01-16 Wed>"

(->> (om-elem-build-timestamp 'inactive
			      2019 1 15 2019 1 15 :warning-type 'all
			      :warning-unit 'day
			      :warning-value 1)
     (om-elem-to-trimmed-string))
 ;; => "[2019-01-15 Tue -1d]"

```

#### om-elem-build-verbatim `(value &key post-blank)`

Build a verbatim object.

The following properties are settable:
- `value`: a oneline string
- `post-blank`: a non-negative integer

```el
(->> (om-elem-build-verbatim "text")
     (om-elem-to-trimmed-string))
 ;; => "=text="

```


### Branch Objects

#### om-elem-build-bold `(&key post-blank &rest objs)`

Build a bold object with `objs` as contents.

The following properties are settable:

- `post-blank`: a non-negative integer

```el
(->> (om-elem-build-bold "text")
     (om-elem-to-trimmed-string))
 ;; => "*text*"

```

#### om-elem-build-footnote-reference `(&key label post-blank &rest objs)`

Build a footnote-reference object with `objs` as contents.

The following properties are settable:
- `label`: a oneline string or nil
- `post-blank`: a non-negative integer

```el
(->> (om-elem-build-footnote-reference)
     (om-elem-to-trimmed-string))
 ;; => "[fn:]"

(->> (om-elem-build-footnote-reference :label "label")
     (om-elem-to-trimmed-string))
 ;; => "[fn:label]"

(->> (om-elem-build-footnote-reference :label "label" "content")
     (om-elem-to-trimmed-string))
 ;; => "[fn:label:content]"

```

#### om-elem-build-italic `(&key post-blank &rest objs)`

Build a italic object with `objs` as contents.

The following properties are settable:

- `post-blank`: a non-negative integer

```el
(->> (om-elem-build-italic "text")
     (om-elem-to-trimmed-string))
 ;; => "/text/"

```

#### om-elem-build-link `(path &key format (type fuzzy) post-blank &rest objs)`

Build a link object with `objs` as contents.

The following properties are settable:
- `path`: a oneline string
- `format`: the symbol 'plain', 'bracket' or 'angle'
- `type`: a oneline string from `org-link-types` or "coderef", "custom-id", "file", "id", "radio", or "fuzzy"
- `post-blank`: a non-negative integer

```el
(->> (om-elem-build-link "target")
     (om-elem-to-trimmed-string))
 ;; => "[[target]]"

(->> (om-elem-build-link "target" :type "file")
     (om-elem-to-trimmed-string))
 ;; => "[[file:target]]"

(->> (om-elem-build-link "target" "desc")
     (om-elem-to-trimmed-string))
 ;; => "[[target][desc]]"

```

#### om-elem-build-radio-target `(&key post-blank &rest objs)`

Build a radio-target object with `objs` as contents.

The following properties are settable:

- `post-blank`: a non-negative integer

```el
(->> (om-elem-build-radio-target "text")
     (om-elem-to-trimmed-string))
 ;; => "<<<text>>>"

```

#### om-elem-build-strike-through `(&key post-blank &rest objs)`

Build a strike-through object with `objs` as contents.

The following properties are settable:

- `post-blank`: a non-negative integer

```el
(->> (om-elem-build-strike-through "text")
     (om-elem-to-trimmed-string))
 ;; => "+text+"

```

#### om-elem-build-superscript `(&key use-brackets-p post-blank &rest objs)`

Build a superscript object with `objs` as contents.

The following properties are settable:
- `use-brackets-p`: nil or t
- `post-blank`: a non-negative integer

```el
(->> (om-elem-build-superscript "text")
     (om-elem-to-trimmed-string))
 ;; => "^text"

```

#### om-elem-build-subscript `(&key use-brackets-p post-blank &rest objs)`

Build a subscript object with `objs` as contents.

The following properties are settable:
- `use-brackets-p`: nil or t
- `post-blank`: a non-negative integer

```el
(->> (om-elem-build-subscript "text")
     (om-elem-to-trimmed-string))
 ;; => "_text"

```

#### om-elem-build-table-cell `(&key post-blank &rest objs)`

Build a table-cell object with `objs` as contents.

The following properties are settable:

- `post-blank`: a non-negative integer

```el
(->> (om-elem-build-table-cell "text")
     (om-elem-build-table-row)
     (om-elem-to-trimmed-string))
 ;; => "| text |"

```

#### om-elem-build-underline `(&key post-blank &rest objs)`

Build a underline object with `objs` as contents.

The following properties are settable:

- `post-blank`: a non-negative integer

```el
(->> (om-elem-build-underline "text")
     (om-elem-to-trimmed-string))
 ;; => "_text_"

```


### Leaf Elements

#### om-elem-build-babel-call `(call &key inside-header arguments end-header post-blank)`

Build a babel-call element.

The following properties are settable:
- `call`: a oneline string
- `inside-header`: a plist
- `arguments`: a list of oneline strings
- `end-header`: a plist
- `post-blank`: a non-negative integer

```el
(->> (om-elem-build-babel-call "name")
     (om-elem-to-trimmed-string))
 ;; => "#+CALL: name()"

(->> (om-elem-build-babel-call "name" :arguments '("arg=x"))
     (om-elem-to-trimmed-string))
 ;; => "#+CALL: name(arg=x)"

(->> (om-elem-build-babel-call "name" :inside-header '(:key val))
     (om-elem-to-trimmed-string))
 ;; => "#+CALL: name[:key val]()"

(->> (om-elem-build-babel-call "name" :end-header '(:key val))
     (om-elem-to-trimmed-string))
 ;; => "#+CALL: name() :key val"

```

#### om-elem-build-clock `(value &key post-blank)`

Build a clock element.

The following properties are settable:
- `value`: an unranged, inactive timestamp with no warning or repeater
- `post-blank`: a non-negative integer

```el
(->> (om-elem-build-clock (om-elem-build-timestamp! 'inactive
						    '(2019 1 1 0 0)))
     (om-elem-to-trimmed-string))
 ;; => "CLOCK: [2019-01-01 Tue 00:00]"

(->> (om-elem-build-clock (om-elem-build-timestamp! 'inactive
						    '(2019 1 1 0 0)
						    :end '(2019 1 1 1 0)))
     (om-elem-to-trimmed-string))
 ;; => "CLOCK: [2019-01-01 Tue 00:00]--[2019-01-01 Tue 01:00] =>  1:00"

```

#### om-elem-build-comment `(value &key post-blank)`

Build a comment element.

The following properties are settable:
- `value`: a oneline string
- `post-blank`: a non-negative integer

```el
(->> (om-elem-build-comment "text")
     (om-elem-to-trimmed-string))
 ;; => "# text"

```

#### om-elem-build-comment-block `(value &key post-blank)`

Build a comment-block element.

The following properties are settable:
- `value`: a oneline string
- `post-blank`: a non-negative integer

```el
(->> (om-elem-build-comment-block "text")
     (om-elem-to-trimmed-string))
 ;; => "#+BEGIN_COMMENT
 ;      text
 ;      #+END_COMMENT"

```

#### om-elem-build-diary-sexp `(value &key post-blank)`

Build a diary-sexp element.

The following properties are settable:
- `value`: a list form
- `post-blank`: a non-negative integer

```el
(->> (om-elem-build-diary-sexp '(text))
     (om-elem-to-trimmed-string))
 ;; => "%%(text)"

```

#### om-elem-build-example-block `(value &key preserve-indent switches post-blank)`

Build a example-block element.

The following properties are settable:
- `value`: a string
- `preserve-indent`: nil or t
- `switches`: a list of oneline strings
- `post-blank`: a non-negative integer

```el
(->> (om-elem-build-example-block "text")
     (om-elem-to-trimmed-string))
 ;; => "#+BEGIN_EXAMPLE
 ;      text
 ;      #+END_EXAMPLE"

(->> (om-elem-build-example-block "text" :switches '("switches"))
     (om-elem-to-trimmed-string))
 ;; => "#+BEGIN_EXAMPLE switches
 ;      text
 ;      #+END_EXAMPLE"

```

#### om-elem-build-export-block `(type value &key post-blank)`

Build a export-block element.

The following properties are settable:
- `type`: a oneline string
- `value`: a string
- `post-blank`: a non-negative integer

```el
(->> (om-elem-build-export-block "type" "value
")
     (om-elem-to-trimmed-string))
 ;; => "#+BEGIN_EXPORT type
 ;      value
 ;      #+END_EXPORT"

```

#### om-elem-build-fixed-width `(value &key post-blank)`

Build a fixed-width element.

The following properties are settable:
- `value`: a oneline string
- `post-blank`: a non-negative integer

```el
(->> (om-elem-build-fixed-width "text")
     (om-elem-to-trimmed-string))
 ;; => ": text"

```

#### om-elem-build-horizontal-rule `(&key post-blank)`

Build a horizontal-rule element.

The following properties are settable:

- `post-blank`: a non-negative integer

```el
(->> (om-elem-build-horizontal-rule)
     (om-elem-to-trimmed-string))
 ;; => "-----"

```

#### om-elem-build-keyword `(key value &key post-blank)`

Build a keyword element.

The following properties are settable:
- `key`: a oneline string
- `value`: a oneline string
- `post-blank`: a non-negative integer

```el
(->> (om-elem-build-keyword "FILETAGS" "tmsu")
     (om-elem-to-trimmed-string))
 ;; => "#+FILETAGS: tmsu"

```

#### om-elem-build-latex-environment `(value &key post-blank)`

Build a latex-environment element.

The following properties are settable:
- `value`: list of strings like (`env` `body`) or (`env`)
- `post-blank`: a non-negative integer

```el
(->> (om-elem-build-latex-environment '("env" "text"))
     (om-elem-to-trimmed-string))
 ;; => "\\begin{env}
 ;      text
 ;      \\end{env}"

```

#### om-elem-build-node-property `(key value &key post-blank)`

Build a node-property element.

The following properties are settable:
- `key`: a oneline string
- `value`: a oneline string
- `post-blank`: a non-negative integer

```el
(->> (om-elem-build-node-property "key" "val")
     (om-elem-to-trimmed-string))
 ;; => ":key:      val"

```

#### om-elem-build-planning `(&key closed deadline scheduled post-blank)`

Build a planning element.

The following properties are settable:
- `closed`: a zero-range, inactive timestamp object
- `deadline`: a zero-range, inactive timestamp object
- `scheduled`: a zero-range, inactive timestamp object
- `post-blank`: a non-negative integer

```el
(->> (om-elem-build-planning :closed (om-elem-build-timestamp! 'inactive
							       '(2019 1 1)))
     (om-elem-to-trimmed-string))
 ;; => "CLOSED: [2019-01-01 Tue]"

(->> (om-elem-build-planning :scheduled (om-elem-build-timestamp! 'inactive
								  '(2019 1 1)))
     (om-elem-to-trimmed-string))
 ;; => "SCHEDULED: [2019-01-01 Tue]"

(->> (om-elem-build-planning :deadline (om-elem-build-timestamp! 'inactive
								 '(2019 1 1)))
     (om-elem-to-trimmed-string))
 ;; => "DEADLINE: [2019-01-01 Tue]"

```

#### om-elem-build-src-block `(value &key language parameters preserve-indent switches post-blank)`

Build a src-block element.

The following properties are settable:
- `value`: a string
- `language`: a string or nil
- `parameters`: a plist
- `preserve-indent`: nil or t
- `switches`: a list of oneline strings
- `post-blank`: a non-negative integer

```el
(->> (om-elem-build-src-block "body")
     (om-elem-to-trimmed-string))
 ;; => "#+BEGIN_SRC
 ;        body
 ;      #+END_SRC"

(->> (om-elem-build-src-block "body" :language "emacs-lisp")
     (om-elem-to-trimmed-string))
 ;; => "#+BEGIN_SRC emacs-lisp
 ;        body
 ;      #+END_SRC"

(->> (om-elem-build-src-block "body" :switches '("-n 20" "-r"))
     (om-elem-to-trimmed-string))
 ;; => "#+BEGIN_SRC -n 20 -r
 ;        body
 ;      #+END_SRC"

(->> (om-elem-build-src-block "body" :parameters '(:key val))
     (om-elem-to-trimmed-string))
 ;; => "#+BEGIN_SRC :key val
 ;        body
 ;      #+END_SRC"

```

#### om-elem-build-table-row-hline `(&key post-blank)`

Build a table-row element with the 'rule' type.

```el
(->> (om-elem-build-table (om-elem-build-table-row (om-elem-build-table-cell "text"))
			  (om-elem-build-table-row-hline))
     (om-elem-to-trimmed-string))
 ;; => "| text |
 ;      |------|"

```


### Branch Elements with Child Objects

#### om-elem-build-paragraph `(&key post-blank &rest objs)`

Build a paragraph element with `objs` as contents.

The following properties are settable:

- `post-blank`: a non-negative integer

```el
(->> (om-elem-build-paragraph "text")
     (om-elem-to-trimmed-string))
 ;; => "text"

```

#### om-elem-build-table-row `(&key post-blank &rest objs)`

Build a table-row element with `objs` as contents.

The following properties are settable:

- `post-blank`: a non-negative integer

```el
(->> (om-elem-build-table-cell "a")
     (om-elem-build-table-row)
     (om-elem-to-trimmed-string))
 ;; => "| a |"

```

#### om-elem-build-verse-block `(&key post-blank &rest objs)`

Build a verse-block element with `objs` as contents.

The following properties are settable:

- `post-blank`: a non-negative integer

```el
(->> (om-elem-build-verse-block "text
")
     (om-elem-to-trimmed-string))
 ;; => "#+BEGIN_VERSE
 ;      text
 ;      #+END_VERSE"

```


### Branch Elements with Child Elements

#### om-elem-build-center-block `(&key post-blank &rest nodes)`

Build a center-block element with `nodes` as contents.

The following properties are settable:

- `post-blank`: a non-negative integer

```el
(->> (om-elem-build-paragraph "text")
     (om-elem-build-center-block)
     (om-elem-to-trimmed-string))
 ;; => "#+BEGIN_CENTER
 ;      text
 ;      #+END_CENTER"

```

#### om-elem-build-drawer `(drawer-name &key post-blank &rest nodes)`

Build a drawer element with `nodes` as contents.

The following properties are settable:
- `drawer-name`: a oneline string
- `post-blank`: a non-negative integer

```el
(->> (om-elem-build-paragraph "text")
     (om-elem-build-drawer "NAME")
     (om-elem-to-trimmed-string))
 ;; => ":NAME:
 ;      text
 ;      :END:"

```

#### om-elem-build-footnote-definition `(label &key post-blank &rest nodes)`

Build a footnote-definition element with `nodes` as contents.

The following properties are settable:
- `label`: a oneline string or nil
- `post-blank`: a non-negative integer

```el
(->> (om-elem-build-paragraph "footnote contents")
     (om-elem-build-footnote-definition "label")
     (om-elem-to-trimmed-string))
 ;; => "[fn:label] footnote contents"

```

#### om-elem-build-headline `(&key archivedp commentedp footnote-section-p (level 1) (pre-blank 0) priority tags title todo-keyword post-blank &rest nodes)`

Build a headline element with `nodes` as contents.

The following properties are settable:
- `archivedp`: nil or t
- `commentedp`: nil or t
- `footnote-section-p`: nil or t
- `level`: a positive integer
- `pre-blank`: a non-negative integer
- `priority`: an integer between (inclusive) `org-highest-priority` and `org-lowest-priority`
- `tags`: a string list
- `title`: a secondary string
- `todo-keyword`: a oneline string or nil
- `post-blank`: a non-negative integer

```el
(->> (om-elem-build-headline)
     (om-elem-to-trimmed-string))
 ;; => "*"

(->> (om-elem-build-headline :title '("dummy"))
     (om-elem-to-trimmed-string))
 ;; => "* dummy"

(->> (om-elem-build-headline :title '("dummy")
			      :level 3)
     (om-elem-to-trimmed-string))
 ;; => "*** dummy"

(->> (om-elem-build-headline :title '("dummy")
			      :todo-keyword "DONE")
     (om-elem-to-trimmed-string))
 ;; => "* DONE dummy"

(->> (om-elem-build-headline :title '("dummy")
			      :priority 65)
     (om-elem-to-trimmed-string))
 ;; => "* [#A] dummy"

(->> (om-elem-build-headline :title '("dummy")
			      :footnote-section-p t)
     (om-elem-to-trimmed-string))
 ;; => "* Footnotes"

(->> (om-elem-build-headline :title '("dummy")
			      :commentedp t)
     (om-elem-to-trimmed-string))
 ;; => "* COMMENT dummy"

```

#### om-elem-build-item `(&key (bullet '-) checkbox counter tag post-blank &rest nodes)`

Build a item element with `nodes` as contents.

The following properties are settable:
- `bullet`: a positive integer (for '1.'), a positive integer in a list (for '1)'), a '-', or a '+'
- `checkbox`: nil or the symbols 'on', 'off', or 'trans'
- `counter`: a positive integer or nil
- `tag`: a secondary string
- `post-blank`: a non-negative integer

```el
(->> (om-elem-build-paragraph "item contents")
     (om-elem-build-item)
     (om-elem-to-trimmed-string))
 ;; => "- item contents"

(->> (om-elem-build-paragraph "item contents")
     (om-elem-build-item :bullet 1)
     (om-elem-to-trimmed-string))
 ;; => "1. item contents"

(->> (om-elem-build-paragraph "item contents")
     (om-elem-build-item :checkbox 'on)
     (om-elem-to-trimmed-string))
 ;; => "- [X] item contents"

(->> (om-elem-build-paragraph "item contents")
     (om-elem-build-item :tag '("tmsu"))
     (om-elem-to-trimmed-string))
 ;; => "- tmsu :: item contents"

(->> (om-elem-build-paragraph "item contents")
     (om-elem-build-item :counter 10)
     (om-elem-to-trimmed-string))
 ;; => "- [@10] item contents"

```

#### om-elem-build-plain-list `(&key post-blank &rest nodes)`

Build a plain-list element with `nodes` as contents.

The following properties are settable:

- `post-blank`: a non-negative integer

```el
(->> (om-elem-build-paragraph "item contents")
     (om-elem-build-item)
     (om-elem-build-plain-list)
     (om-elem-to-trimmed-string))
 ;; => "- item contents"

```

#### om-elem-build-property-drawer `(&key post-blank &rest nodes)`

Build a property-drawer element with `nodes` as contents.

The following properties are settable:

- `post-blank`: a non-negative integer

```el
(->> (om-elem-build-node-property "key" "val")
     (om-elem-build-property-drawer)
     (om-elem-to-trimmed-string))
 ;; => ":PROPERTIES:
 ;      :key:      val
 ;      :END:"

```

#### om-elem-build-quote-block `(&key post-blank &rest nodes)`

Build a quote-block element with `nodes` as contents.

The following properties are settable:

- `post-blank`: a non-negative integer

```el
(->> (om-elem-build-paragraph "quoted stuff")
     (om-elem-build-quote-block)
     (om-elem-to-trimmed-string))
 ;; => "#+BEGIN_QUOTE
 ;      quoted stuff
 ;      #+END_QUOTE"

```

#### om-elem-build-section `(&key post-blank &rest nodes)`

Build a section element with `nodes` as contents.

The following properties are settable:

- `post-blank`: a non-negative integer

```el
(->> (om-elem-build-paragraph "text")
     (om-elem-build-section)
     (om-elem-to-trimmed-string))
 ;; => "text"

```

#### om-elem-build-table `(&key tblfm post-blank &rest nodes)`

Build a table element with `nodes` as contents.

The following properties are settable:
- `tblfm`: a list of oneline strings
- `post-blank`: a non-negative integer

```el
(->> (om-elem-build-table-cell "cell")
     (om-elem-build-table-row)
     (om-elem-build-table)
     (om-elem-to-trimmed-string))
 ;; => "| cell |"

```


### Miscellaneous Builders

#### om-elem-build-timestamp-diary-sexp `(form &key post-blank)`

Build a diary-sexp timestamp element from `string`.
`string` is a lisp form as a string.

```el
(->> (om-elem-build-timestamp-diary-sexp '(diary-float t 4 2))
     (om-elem-to-string))
 ;; => "<%%(diary-float t 4 2)>"

```

#### om-elem-build-table-row-hline `(&key post-blank)`

Build a table-row element with the 'rule' type.

```el
(->> (om-elem-build-table-row-hline)
     (om-elem-to-trimmed-string))
 ;; => "|-"

```


### Shorthand Builders


Build nodes with more convenient/shorter syntax.

#### om-elem-build-timestamp! `(type start &key end repeater warning post-blank)`

Build a timestamp object.

`type` is one if 'active' or 'inactive' (the range suffix will be added
if an end time is supplied).

`start` specifies the start time and is a list of integers in one of 
the following forms:
- (year month day): short form
- (year month day nil nil) short form
- (year month day hour minute) long form

`end` (if supplied) will add the ending time, and follows the same 
formatting rules as `start`.

`repeater` and `warning` are lists formatted as (`type` `value` `unit`) where
the three members correspond to the :repeater/warning-type, -value,
and -unit properties in [`om-elem-build-timestamp`](#om-elem-build-timestamp-type-year-start-month-start-day-start-year-end-month-end-day-end-key-hour-start-minute-start-hour-end-minute-end-repeater-type-repeater-unit-repeater-value-warning-type-warning-unit-warning-value-post-blank).

Building a diary sexp timestamp is not possible with this function.

```el
(->> (om-elem-build-timestamp! 'inactive
			       '(2019 1 1))
     (om-elem-to-string))
 ;; => "[2019-01-01 Tue]"

(->> (om-elem-build-timestamp! 'inactive
			       '(2019 1 1 12 0)
			       :warning '(all 1 day)
			       :repeater '(cumulate 1 month))
     (om-elem-to-string))
 ;; => "[2019-01-01 Tue 12:00 +1m -1d]"

(->> (om-elem-build-timestamp! 'inactive
			       '(2019 1 1)
			       :end '(2019 1 2))
     (om-elem-to-string))
 ;; => "[2019-01-01 Tue]--[2019-01-02 Wed]"

```

#### om-elem-build-clock! `(start &key end post-blank)`

Build a clock object.

`start` and `end` follow the same rules as their respective arguments in
[`om-elem-build-timestamp!`](#om-elem-build-timestamp-type-start-key-end-repeater-warning-post-blank).

```el
(->> (om-elem-build-clock! '(2019 1 1))
     (om-elem-to-trimmed-string))
 ;; => "CLOCK: [2019-01-01 Tue]"

(->> (om-elem-build-clock! '(2019 1 1 12 0))
     (om-elem-to-trimmed-string))
 ;; => "CLOCK: [2019-01-01 Tue 12:00]"

(->> (om-elem-build-clock! '(2019 1 1 12 0)
			   :end '(2019 1 1 13 0))
     (om-elem-to-trimmed-string))
 ;; => "CLOCK: [2019-01-01 Tue 12:00]--[2019-01-01 Tue 13:00] =>  1:00"

```

#### om-elem-build-planning! `(&key closed deadline scheduled post-blank)`

Build a planning element using shorthand arguments.
`closed`, `deadline`, and `scheduled` are lists with the following structure
(brackets denote optional members):

'(year minute day [hour] [min]
    [&warning type value unit])
    [&repeater type value unit])'

In terms of arguments supplied to [`om-elem-build-timestamp!`](#om-elem-build-timestamp-type-start-key-end-repeater-warning-post-blank), the
first five members correspond to the list supplied as `time`, and the
type/value/unit correspond to the lists supplied to `warning` and
`repeater`. The order of warning and repeater does not matter.

```el
(->> (om-elem-build-planning! :closed '(2019 1 1))
     (om-elem-to-trimmed-string))
 ;; => "CLOSED: [2019-01-01 Tue]"

(->> (om-elem-build-planning! :closed '(2019 1 1)
			       :scheduled '(2018 1 1))
     (om-elem-to-trimmed-string))
 ;; => "SCHEDULED: [2018-01-01 Mon] CLOSED: [2019-01-01 Tue]"

(->> (om-elem-build-planning! :closed '(2019 1 1 &warning all 1 day &repeater cumulate 1 month))
     (om-elem-to-trimmed-string))
 ;; => "CLOSED: [2019-01-01 Tue +1m -1d]"

```

#### om-elem-build-property-drawer! `(&key post-blank &rest keyvals)`

Create a property drawer element.

Each member in `keyvals` is a list of symbols like (key val), where each
list will generate a node property in the property drawer like ':Key:
Val'.

```el
(->> (om-elem-build-property-drawer! '(key val))
     (om-elem-to-trimmed-string))
 ;; => ":PROPERTIES:
 ;      :key:      val
 ;      :END:"

```

#### om-elem-build-headline! `(&key (level 1) title-text todo-keyword tags pre-blank priority commentedp archivedp post-blank planning properties statistics-cookie section-contents &rest subheadlines)`

Build a headline element.

`title-text` is a oneline string for the title of the headline.

`planning` is a list like ('planning-type' 'args' ...) where
'planning-type' is one of :closed, :deadline, or :scheduled, and
'args' are the args supplied to any of the planning types in
[`om-elem-build-planning!`](#om-elem-build-planning-key-closed-deadline-scheduled-post-blank). Up to all three planning types can be used
in the same list like (:closed args :deadline args :scheduled).

`statistics-cookie` is a list following the same format as 
[`om-elem-build-statistics-cookie`](#om-elem-build-statistics-cookie-value-key-post-blank).

`section-contents` is a list of elements that will go in the headline
section.

`subheadlines` contains zero or more headlines that will go under the
created headline.

All arguments not mentioned here follow the same rules as
[`om-elem-build-headline`](#om-elem-build-headline-key-archivedp-commentedp-footnote-section-p-level-1-pre-blank-0-priority-tags-title-todo-keyword-post-blank-rest-nodes)

```el
(->> (om-elem-build-headline! :title-text "really impressive title")
     (om-elem-to-trimmed-string))
 ;; => "* really impressive title"

(->> (om-elem-build-headline! :title-text "really impressive title" :statistics-cookie '(0 9000))
     (om-elem-to-trimmed-string))
 ;; => "* really impressive title [0/9000]"

(->> (om-elem-build-headline! :title-text "really impressive title" :properties '((key val))
			       :section-contents (list (om-elem-build-paragraph! "section text"))
			       (om-elem-build-headline! :level 2 :title-text "subhead"))
     (om-elem-to-trimmed-string))
 ;; => "* really impressive title
 ;      :PROPERTIES:
 ;      :key:      val
 ;      :END:
 ;      section text
 ;      ** subhead"

```

#### om-elem-build-item! `(&key post-blank bullet checkbox tag paragraph counter &rest subitems)`

Build an item element.

`tag` is a string representing the tag.

`paragraph` is a string that will be the initial text in the item.

`subitems` contains the items that will go under this item.

All other arguments follow the same rules as [`om-elem-build-item`](#om-elem-build-item-key-bullet-quote---checkbox-counter-tag-post-blank-rest-nodes).

```el
(->> (om-elem-build-item! :bullet '(1)
			   :tag "complicated *tag*" :paragraph "petulant /frenzy/" (om-elem-build-item! :bullet '-
													 :paragraph "below"))
     (om-elem-to-trimmed-string))
 ;; => "1. complicated *tag* :: petulant /frenzy/
 ;         - below"

```

#### om-elem-build-paragraph! `(string &key post-blank)`

Build a paragraph element.

`string` is the text to be parsed into a paragraph. It must contain valid
formatting (eg, text that will be formatted into objects).

```el
(->> (om-elem-build-paragraph! "stuff /with/ *formatting*" :post-blank 2)
     (om-elem-to-string))
 ;; => "stuff /with/ *formatting*
 ;      
 ;      
 ;      "

(->> (om-elem-build-paragraph! "* stuff /with/ *formatting*")
     (om-elem-to-string))
Error

```

#### om-elem-build-table! `(&key tblfm post-blank &rest row-lists)`

Build a table element.

`row-lists` is a list of lists where each member is a string to be put
in a table cell or the symbol 'hline' which represents a horizontal
line.

All other arguments follow the same rules as [`om-elem-build-table`](#om-elem-build-table-key-tblfm-post-blank-rest-nodes).

```el
(->> (om-elem-build-table! '("R" "A")
			   '("G" "E"))
     (om-elem-to-trimmed-string))
 ;; => "| R | A |
 ;      | G | E |"

(->> (om-elem-build-table! '("L" "O")
			   'hline
			   '("V" "E"))
     (om-elem-to-trimmed-string))
 ;; => "| L | O |
 ;      |---+---|
 ;      | V | E |"

```


## Type Predicates


Test node types.

#### om-elem-is-type-p `(type node)`

Return t if the type of `node` is `type` (a symbol).

```el
;; Given the following contents:
; *ziltoid*

(->> (om-elem-parse-this-object)
     (om-elem-is-type-p 'bold))
 ;; => t

(->> (om-elem-parse-this-object)
     (om-elem-is-type-p 'italic))
 ;; => nil

```

#### om-elem-is-any-type-p `(types node)`

Return t if the type of `node` is in `types` (a list of symbols).

```el
;; Given the following contents:
; *ziltoid*

(->> (om-elem-parse-this-object)
     (om-elem-is-any-type-p '(bold)))
 ;; => t

(->> (om-elem-parse-this-object)
     (om-elem-is-any-type-p '(bold italic)))
 ;; => t

(->> (om-elem-parse-this-object)
     (om-elem-is-any-type-p '(italic)))
 ;; => nil

```

#### om-elem-is-element-p `(node)`

Return t if `node` is an element type.

```el
;; Given the following contents:
; *ziltoid*

;; Parsing this text as an element gives a paragraph
(->> (om-elem-parse-this-element)
     (om-elem-is-element-p))
 ;; => t

;; Parsing the same text as an object gives a bold object
(->> (om-elem-parse-this-object)
     (om-elem-is-element-p))
 ;; => nil

```

#### om-elem-is-container-p `(node)`

Return t if `node` is a container.
Containers are elements or objects that may contain other elements
or objects.

```el
;; Given the following contents:
; *ziltoid*

;; Parsing this as an element gives a paragraph type (an object container).
(->> (om-elem-parse-this-element)
     (om-elem-is-container-p))
 ;; => t

;; Parsing this as an object gives a bold type (also an object container).
(->> (om-elem-parse-this-object)
     (om-elem-is-container-p))
 ;; => t

;; Given the following contents:
; ~ziltoid~

;; Parsing this as an object gives a code type (not a container).
(->> (om-elem-parse-this-object)
     (om-elem-is-container-p))
 ;; => nil

;; Given the following contents:
; # ziltoid

;; Parsing this as an element gives a comment type (not a container).
(->> (om-elem-parse-this-element)
     (om-elem-is-container-p))
 ;; => nil

;; Given the following contents:
; * I'm so great

;; Parsing this as an element gives a table (a greater element).
(->> (om-elem-parse-this-element)
     (om-elem-is-container-p))
 ;; => t

```

#### om-elem-is-object-container-p `(node)`

Return t if `node` is an object container.
Object containers are elements or objects that may contain objects.

```el
;; Given the following contents:
; *ziltoid*

;; Parsing this as an element gives a paragraph type (an object container).
(->> (om-elem-parse-this-element)
     (om-elem-is-object-container-p))
 ;; => t

;; Parsing this as an object gives a bold type (also an object container).
(->> (om-elem-parse-this-object)
     (om-elem-is-object-container-p))
 ;; => t

;; Given the following contents:
; ~ziltoid~

;; Parsing this as an object gives a code type (not a container).
(->> (om-elem-parse-this-object)
     (om-elem-is-object-container-p))
 ;; => nil

;; Given the following contents:
; # ziltoid

;; Parsing this as an element gives a comment type (not a container).
(->> (om-elem-parse-this-element)
     (om-elem-is-object-container-p))
 ;; => nil

;; Given the following contents:
; * I'm so great

;; Parsing this as an element gives a table (a greater element).
(->> (om-elem-parse-this-element)
     (om-elem-is-object-container-p))
 ;; => nil

```

#### om-elem-is-greater-element-p `(node)`

Return t if `node` is a greater element.
Greater elements are elements that may contain other elements.

```el
;; Given the following contents:
; * I'm so great

;; Parsing this as an element gives a table (a greater element).
(->> (om-elem-parse-this-element)
     (om-elem-is-greater-element-p))
 ;; => t

;; Given the following contents:
; *ziltoid*

;; Parsing this as an element gives a paragraph type (not a greater element).
(->> (om-elem-parse-this-element)
     (om-elem-is-greater-element-p))
 ;; => nil

;; Given the following contents:
; # ziltoid

;; Parsing this as an element gives a comment type (not a container).
(->> (om-elem-parse-this-element)
     (om-elem-is-greater-element-p))
 ;; => nil

```


## Property Manipulation


Set, get, and map properties of nodes.


### Generic

#### om-elem-set-property `(prop value node)`

Set property `prop` to `value` in `elem`.

```el
;; Given the following contents:
; #+CALL: ktulu()

(->> (om-elem-parse-this-element)
     (om-elem-set-property :call "cthulhu")
     (om-elem-set-property :inside-header '(:cache no))
     (om-elem-set-property :arguments '("x=4"))
     (om-elem-set-property :end-header '(:exports results))
     (om-elem-to-trimmed-string))
 ;; => "#+CALL: cthulhu[:cache no](x=4) :exports results"

;; Given the following contents:
; - thing

(->> (om-elem-parse-this-item)
     (om-elem-set-property :bullet 1)
     (om-elem-set-property :checkbox 'on)
     (om-elem-set-property :counter 2)
     (om-elem-set-property :tag '("tmsu"))
     (om-elem-to-trimmed-string))
 ;; => "1. [@2] [X] tmsu :: thing"

;; Given the following contents:
; * not valuable

;; Throw error when setting a property that doesn't exist
(->> (om-elem-parse-this-headline)
     (om-elem-set-property :value "wtf")
     (om-elem-to-trimmed-string))
Error

```

#### om-elem-set-properties `(plist node)`

Set all properties in `node` to the values corresponding to `plist`.
`plist` is a list of property-value pairs that corresponds to the
property list in `node`.

See builder functions for a list of properties and their rules for
each type.

```el
;; Given the following contents:
; - thing

(->> (om-elem-parse-this-item)
     (om-elem-set-properties (list :bullet 1 :checkbox 'on
				    :counter 2 :tag '("tmsu")))
     (om-elem-to-trimmed-string))
 ;; => "1. [@2] [X] tmsu :: thing"

```

#### om-elem-get-property `(prop node)`

Return the value or property `prop` in `node`.

See builder functions for a list of properties and their rules for
each type.

```el
;; Given the following contents:
; #+CALL: ktulu[:cache no](x=4) :exports results

(->> (om-elem-parse-this-element)
     (om-elem-get-property :call))
 ;; => "ktulu"

(->> (om-elem-parse-this-element)
     (om-elem-get-property :inside-header))
 ;; => '(:cache no)

(->> (om-elem-parse-this-element)
     (om-elem-get-property :arguments))
 ;; => '("x=4")

(->> (om-elem-parse-this-element)
     (om-elem-get-property :end-header))
 ;; => '(:exports results)

;; Given the following contents:
; [[file:/dev/null]]

(->> (om-elem-parse-this-object)
     (om-elem-get-property :path))
 ;; => "/dev/null"

(->> (om-elem-parse-this-object)
     (om-elem-get-property :type))
 ;; => "file"

(->> (om-elem-parse-this-object)
     (om-elem-get-property :format))
 ;; => 'bracket

;; Given the following contents:
; * not arguable

;; Throw error when requesting a property that doesn't exist
(->> (om-elem-parse-this-headline)
     (om-elem-get-property :value))
Error

```

#### om-elem-map-property `(prop fun node)`

Apply function `fun` to the value of property `prop` in `node`.
`fun` takes one argument (the current value of `prop`) and returns
a new value to which `prop` will be set.

See builder functions for a list of properties and their rules for
each type.

```el
;; Given the following contents:
; #+CALL: ktulu()

(->> (om-elem-parse-this-element)
     (om-elem-map-property :call (function s-upcase))
     (om-elem-to-trimmed-string))
 ;; => "#+CALL: KTULU()"

;; Given the following contents:
; #+BEGIN_EXAMPLE
; example.com
; #+END_EXAMPLE

(->> (om-elem-parse-this-element)
     (om-elem-map-property* :value (concat "https://" it))
     (om-elem-to-trimmed-string))
 ;; => "#+BEGIN_EXAMPLE
 ;      https://example.com
 ;      #+END_EXAMPLE"

;; Given the following contents:
; ~code~

;; Throw error if property doesn't exist
(->> (om-elem-parse-this-object)
     (om-elem-map-property :title (function s-upcase))
     (om-elem-to-trimmed-string))
Error

;; Throw error if function doesn't return proper type
(->> (om-elem-parse-this-object)
     (om-elem-map-property* :value (if it 1 0))
     (om-elem-to-trimmed-string))
Error

```

#### om-elem-map-properties `(plist node)`

Alter the values of properties in place within `node`.
`plist` is a property list where the keys are properties in `node` and
its values are functions to be mapped to these properties.

See builder functions for a list of properties and their rules for
each type.

```el
;; Given the following contents:
; #+KEY: VAL

(->> (om-elem-parse-this-element)
     (om-elem-map-properties (list :key (-partial (function s-prepend)
						  "OM_")
				    :value (-partial (function s-prepend)
						     "OM_")))
     (om-elem-to-trimmed-string))
 ;; => "#+OM_KEY: OM_VAL"

```

#### om-elem-toggle-property `(prop node)`

Flip the value of property `prop` in `node`.
This function only applies to properties that are booleans.

The following elements and properties are supported:

entity
- :use-brackets-p

example-block
- :preserve-indent

headline
- :archivedp
- :commentedp
- :footnote-section-p

src-block
- :preserve-indent

subscript
- :use-brackets-p

superscript
- :use-brackets-p

```el
;; Given the following contents:
; \pi

(->> (om-elem-parse-this-object)
     (om-elem-toggle-property :use-brackets-p)
     (om-elem-to-trimmed-string))
 ;; => "\\pi{}"

;; Given the following contents:
; * headline

(->> (om-elem-parse-this-headline)
     (om-elem-toggle-property :archivedp)
     (om-elem-to-trimmed-string))
 ;; => "* headline                                                          :ARCHIVE:"

(->> (om-elem-parse-this-headline)
     (om-elem-toggle-property :commentedp)
     (om-elem-to-trimmed-string))
 ;; => "* COMMENT headline"

(->> (om-elem-parse-this-headline)
     (om-elem-toggle-property :footnote-section-p)
     (om-elem-to-trimmed-string))
 ;; => "* Footnotes"

;; Given the following contents:
; - [ ] nope

;; Throw an error when trying to toggle a non-boolean property
(->> (om-elem-parse-this-item)
     (om-elem-toggle-property :checkbox)
     (om-elem-to-trimmed-string))
Error

```

#### om-elem-shift-property `(prop n node)`

Shift property `prop` by `n` (an integer) units within `node`.
This only applies the properties that are represented as integers.

The following elements and properties are supported:

all elements
- :post-blank

headline
- :level
- :pre-blank
- :priority

item
- :counter

```el
;; Given the following contents:
; * no priorities

;; Do nothing if there is nothing to shift.
(->> (om-elem-parse-this-headline)
     (om-elem-shift-property :priority 1)
     (om-elem-to-trimmed-string))
 ;; => "* no priorities"

;; Given the following contents:
; * [#A] priorities

(->> (om-elem-parse-this-headline)
     (om-elem-shift-property :priority -1)
     (om-elem-to-trimmed-string))
 ;; => "* [#B] priorities"

(->> (om-elem-parse-this-headline)
     (om-elem-shift-property :priority -2)
     (om-elem-to-trimmed-string))
 ;; => "* [#C] priorities"

;; Wrap priority around when crossing the min or max
(->> (om-elem-parse-this-headline)
     (om-elem-shift-property :priority 1)
     (om-elem-to-trimmed-string))
 ;; => "* [#C] priorities"

;; Given the following contents:
; * TODO or not todo

;; Throw error when shifting an unshiftable property
(->> (om-elem-parse-this-headline)
     (om-elem-shift-property :todo-keyword 1)
     (om-elem-to-string))
Error

```

#### om-elem-insert-into-property `(prop index string node)`

Insert `string` into `prop` at `index` within `node` if it is not already there.
This only applies to properties that are represented as lists of strings.

The following elements and properties are supported:

babel-call
- :arguments

example-block
- :switches

headline
- :tags

inline-babel-call
- :arguments

macro
- :args

src-block
- :switches

table
- :tblfm

```el
;; Given the following contents:
; #+CALL: ktulu(y=1)

(->> (om-elem-parse-this-element)
     (om-elem-insert-into-property :arguments 0 "x=4")
     (om-elem-to-trimmed-string))
 ;; => "#+CALL: ktulu(x=4,y=1)"

;; Do nothing if the string is already in the list
(->> (om-elem-parse-this-element)
     (om-elem-insert-into-property :arguments 0 "y=1")
     (om-elem-to-trimmed-string))
 ;; => "#+CALL: ktulu(y=1)"

;; Throw error when inserting into a property that is not a list of strings
(->> (om-elem-parse-this-element)
     (om-elem-insert-into-property :end-header 0 "html")
     (om-elem-to-trimmed-string))
Error

;; Given the following contents:
; * headline       :tag1:

(->> (om-elem-parse-this-headline)
     (om-elem-insert-into-property :tags 0 "tag0")
     (om-elem-to-trimmed-string))
 ;; => "* headline                                                        :tag0:tag1:"

```

#### om-elem-remove-from-property `(prop string node)`

Remove string `string` from list `prop` within `node`.
This only applies to properties that are represented as lists of 
strings.

See [`om-elem-insert-into-property`](#om-elem-insert-into-property-prop-index-string-node) for a list of supported elements
and properties that may be used with this function.

```el
;; Given the following contents:
; #+CALL: ktulu(y=1)

(->> (om-elem-parse-this-element)
     (om-elem-remove-from-property :arguments "y=1")
     (om-elem-to-trimmed-string))
 ;; => "#+CALL: ktulu()"

;; Do nothing if the string does not exist
(->> (om-elem-parse-this-element)
     (om-elem-remove-from-property :arguments "d=666")
     (om-elem-to-trimmed-string))
 ;; => "#+CALL: ktulu(y=1)"

;; Throw error when removing from property that is not a string list
(->> (om-elem-parse-this-element)
     (om-elem-remove-from-property :end-header ":results")
     (om-elem-to-trimmed-string))
Error

;; Given the following contents:
; * headline       :tag1:

(->> (om-elem-parse-this-headline)
     (om-elem-remove-from-property :tags "tag1")
     (om-elem-to-trimmed-string))
 ;; => "* headline"

```

#### om-elem-plist-put-property `(prop key value node)`

Insert `key` and `value` pair into `prop` within `node`.
`key` is a keyword and `value` is a symbol. This only applies to 
properties that are represented as plists.

The following elements and properties are supported:.

babel-call
- :inside-header
- :end-header

dynamic-block
- :arguments

inline-babel-call
- :inside-header
- :end-header

inline-src-block
- :parameters

src-block
- :parameters

```el
;; Given the following contents:
; #+CALL: ktulu[:cache no]()

(->> (om-elem-parse-this-element)
     (om-elem-plist-put-property :end-header :results 'html)
     (om-elem-to-trimmed-string))
 ;; => "#+CALL: ktulu[:cache no]() :results html"

;; Change the value of key if it already is present
(->> (om-elem-parse-this-element)
     (om-elem-plist-put-property :inside-header :cache 'yes)
     (om-elem-to-trimmed-string))
 ;; => "#+CALL: ktulu[:cache yes]()"

;; Do nothing if the key and value already exist
(->> (om-elem-parse-this-element)
     (om-elem-plist-put-property :inside-header :cache 'no)
     (om-elem-to-trimmed-string))
 ;; => "#+CALL: ktulu[:cache no]()"

;; Throw error if setting property that isn't a plist
(->> (om-elem-parse-this-element)
     (om-elem-plist-put-property :arguments :cache 'no)
     (om-elem-to-trimmed-string))
Error

```

#### om-elem-plist-remove-property `(prop key node)`

Remove `key` and its value from `prop` within `node`.
`key` is a keyword. This only applies to properties that are
represented as plists.

See [`om-elem-plist-put-property`](#om-elem-plist-put-property-prop-key-value-node) for a list of supported elements
and properties that may be used with this function.

```el
;; Given the following contents:
; #+CALL: ktulu() :results html

(->> (om-elem-parse-this-element)
     (om-elem-plist-remove-property :end-header :results)
     (om-elem-to-trimmed-string))
 ;; => "#+CALL: ktulu()"

;; Do nothing if the key is not present
(->> (om-elem-parse-this-element)
     (om-elem-plist-remove-property :inside-header :cache)
     (om-elem-to-trimmed-string))
 ;; => "#+CALL: ktulu() :results html"

;; Throw error if trying to remove key from non-plist property
(->> (om-elem-parse-this-element)
     (om-elem-plist-remove-property :arguments :cache)
     (om-elem-to-trimmed-string))
Error

```


### Clock

#### om-elem-clock-is-running-p `(clock)`

Return t if `clock` element is running (eg is open).

```el
;; Given the following contents:
; CLOCK: [2019-01-01 Tue 00:00]

(->> (om-elem-parse-this-element)
     (om-elem-clock-is-running-p))
 ;; => t

;; Given the following contents:
; CLOCK: [2019-01-01 Tue 00:00]--[2019-01-02 Wed 00:00] => 24:00

(->> (om-elem-parse-this-element)
     (om-elem-clock-is-running-p))
 ;; => nil

```

#### om-elem-clock-map-timestamp `(fun clock)`

Apply `fun` to timestamp in `clock`.
`fun` is a function that takes the current timestamp and returns
a modified timestamp. The returned timestamp must be inactive and
cannot contain any warnings or repeaters.

```el
;; Given the following contents:
; CLOCK: [2019-01-01 Tue 00:00]

(->> (om-elem-parse-this-element)
     (om-elem-clock-map-timestamp* (om-elem-timestamp-shift 1 'day
							    it))
     (om-elem-to-trimmed-string))
 ;; => "CLOCK: [2019-01-02 Wed 00:00]"

```


### Headline

#### om-elem-headline-is-done-p `(headline)`

Return t if `headline` element has a `done` todo keyword.

```el
;; Given the following contents:
; * TODO darn

(->> (om-elem-parse-this-headline)
     (om-elem-headline-is-done-p))
 ;; => nil

;; Given the following contents:
; * DONE yay

(->> (om-elem-parse-this-headline)
     (om-elem-headline-is-done-p))
 ;; => t

```

#### om-elem-headline-is-archived-p `(headline)`

Return t if `headline` element is archived.

```el
;; Given the following contents:
; * dummy

(->> (om-elem-parse-this-headline)
     (om-elem-headline-is-archived-p))
 ;; => nil

;; Given the following contents:
; * dummy                                                             :ARCHIVE:

(->> (om-elem-parse-this-headline)
     (om-elem-headline-is-archived-p))
 ;; => t

```

#### om-elem-headline-is-commented-p `(headline)`

Return t if `headline` element is commented.

```el
;; Given the following contents:
; * dummy

(->> (om-elem-parse-this-headline)
     (om-elem-headline-is-commented-p))
 ;; => nil

;; Given the following contents:
; * COMMENT dummy

(->> (om-elem-parse-this-headline)
     (om-elem-headline-is-commented-p))
 ;; => t

```

#### om-elem-headline-has-tag-p `(tag headline)`

Return t if `headline` element is tagged with `tag`.

```el
;; Given the following contents:
; * dummy

(->> (om-elem-parse-this-headline)
     (om-elem-headline-has-tag-p "tmsu"))
 ;; => nil

;; Given the following contents:
; * dummy                                                             :tmsu:

(->> (om-elem-parse-this-headline)
     (om-elem-headline-has-tag-p "tmsu"))
 ;; => t

```

#### om-elem-headline-get-statistics-cookie `(headline)`

Return the statistics cookie object from `headline` if it exists.

```el
;; Given the following contents:
; * statistically significant [10/10]

(->> (om-elem-parse-this-headline)
     (om-elem-headline-get-statistics-cookie)
     (om-elem-to-string))
 ;; => "[10/10]"

;; Given the following contents:
; * not statistically significant

(->> (om-elem-parse-this-headline)
     (om-elem-headline-get-statistics-cookie))
 ;; => nil

```


### Item

#### om-elem-item-is-unchecked-p `(item)`

Return t if `item` element is unchecked.

```el
;; Given the following contents:
; - one
; - [ ] two
; - [X] three
; - [-] four

(->> (om-elem-parse-this-element)
     (om-elem--get-contents)
     (-map (function om-elem-item-is-unchecked-p)))
 ;; => '(nil t nil nil)

```

#### om-elem-item-is-checked-p `(item)`

Return t if `item` element is checked.

```el
;; Given the following contents:
; - one
; - [ ] two
; - [X] three
; - [-] four

(->> (om-elem-parse-this-element)
     (om-elem--get-contents)
     (-map (function om-elem-item-is-checked-p)))
 ;; => '(nil nil t nil)

```

#### om-elem-item-is-trans-p `(item)`

Return t if `item` element is transitional.

```el
;; Given the following contents:
; - one
; - [ ] two
; - [X] three
; - [-] four

(->> (om-elem-parse-this-element)
     (om-elem--get-contents)
     (-map (function om-elem-item-is-trans-p)))
 ;; => '(nil nil nil t)

```

#### om-elem-item-toggle-checkbox `(item)`

Toggle the checked/unchecked state of `item` element.

```el
;; Given the following contents:
; - [ ] one

(->> (om-elem-parse-this-item)
     (om-elem-item-toggle-checkbox)
     (om-elem-to-trimmed-string))
 ;; => "- [X] one"

(->> (om-elem-parse-this-item)
     (om-elem-item-toggle-checkbox)
     (om-elem-item-toggle-checkbox)
     (om-elem-to-trimmed-string))
 ;; => "- [ ] one"

;; Given the following contents:
; - [-] one

(->> (om-elem-parse-this-item)
     (om-elem-item-toggle-checkbox)
     (om-elem-to-trimmed-string))
 ;; => "- [-] one"

;; Given the following contents:
; - one

(->> (om-elem-parse-this-item)
     (om-elem-item-toggle-checkbox)
     (om-elem-to-trimmed-string))
 ;; => "- one"

```


### Planning

#### om-elem-planning-set-timestamp `(prop planning-list planning)`

Set the timestamp of `planning` matching `prop`.

`prop` is one of :closed, :deadline, or :scheduled. `planning-list` is the
same as that described in [`om-elem-build-planning!`](#om-elem-build-planning-key-closed-deadline-scheduled-post-blank).

```el
;; Given the following contents:
; * dummy
; CLOSED: [2019-01-01 Tue]

;; Change an existing timestamp in planning
(->> (om-elem-parse-this-headline)
     (om-elem--headline-get-planning)
     (om-elem-planning-set-timestamp :closed '(2019 1 2 &warning all 1 day &repeater cumulate 2 month))
     (om-elem-to-trimmed-string))
 ;; => "CLOSED: [2019-01-02 Wed +2m -1d]"

;; Add a new timestamp and remove another
(->> (om-elem-parse-this-headline)
     (om-elem--headline-get-planning)
     (om-elem-planning-set-timestamp :deadline '(2112 1 1))
     (om-elem-planning-set-timestamp :closed nil)
     (om-elem-to-trimmed-string))
 ;; => "DEADLINE: [2112-01-01 Fri]"

```

#### om-elem-planning-map-timestamp `(prop fun planning)`

Modify timestamp matching `prop` in place in `planning` using `fun`.

`prop` is one of :closed, :deadline, or :scheduled. `fun` must return a
timestamp conforming to that described in [`om-elem-build-planning`](#om-elem-build-planning-key-closed-deadline-scheduled-post-blank).

The only difference between using this function and using 
[`om-elem-map-property`](#om-elem-map-property-prop-fun-node) is that the former will silently no-op if `prop`
is nil. The latter will throw an error unless `fun` is able to handle
nil values.

```el
;; Given the following contents:
; * dummy
; CLOSED: [2019-01-01 Tue]

;; Apply mapping function if timestamp exists
(->> (om-elem-parse-this-headline)
     (om-elem--headline-get-planning)
     (om-elem-planning-map-timestamp* :closed (om-elem-timestamp-shift 1 'day
								       it))
     (om-elem-to-trimmed-string))
 ;; => "CLOSED: [2019-01-02 Wed]"

;; Do nothing if timestamp does not exist
(->> (om-elem-parse-this-headline)
     (om-elem--headline-get-planning)
     (om-elem-planning-map-timestamp* :deadline (om-elem-timestamp-shift 1 'day
									 it))
     (om-elem-to-trimmed-string))
 ;; => "CLOSED: [2019-01-01 Tue]"

;; Throw error if new timestamp is not allowed
(->> (om-elem-parse-this-headline)
     (om-elem--headline-get-planning)
     (om-elem-planning-map-timestamp :closed (function om-elem-timestamp-toggle-active))
     (om-elem-to-trimmed-string))
Error

```


### Statistics Cookie

#### om-elem-statistics-cookie-is-complete-p `(statistics-cookie)`

Return t is `statistics-cookie` element is complete.

```el
;; Given the following contents:
; * statistically significant [10/10]

(->> (om-elem-parse-this-headline)
     (om-elem-headline-get-statistics-cookie)
     (om-elem-statistics-cookie-is-complete-p))
 ;; => t

;; Given the following contents:
; * statistically significant [1/10]

(->> (om-elem-parse-this-headline)
     (om-elem-headline-get-statistics-cookie)
     (om-elem-statistics-cookie-is-complete-p))
 ;; => nil

;; Given the following contents:
; * statistically significant [100%]

(->> (om-elem-parse-this-headline)
     (om-elem-headline-get-statistics-cookie)
     (om-elem-statistics-cookie-is-complete-p))
 ;; => t

;; Given the following contents:
; * statistically significant [33%]

(->> (om-elem-parse-this-headline)
     (om-elem-headline-get-statistics-cookie)
     (om-elem-statistics-cookie-is-complete-p))
 ;; => nil

```


### Timestamp

#### om-elem-timestamp-get-start-time `(timestamp)`

Return the time list of `timestamp` or start time if a range.
The return value will be a list as specified by the `time` argument in
[`om-elem-build-timestamp!`](#om-elem-build-timestamp-type-start-key-end-repeater-warning-post-blank).

```el
;; Given the following contents:
; [2019-01-01 Tue]

(->> (om-elem-parse-this-object)
     (om-elem-timestamp-get-start-time))
 ;; => '(2019 1 1 nil nil)

;; Given the following contents:
; [2019-01-01 Tue]--[2019-01-02 Wed]

(->> (om-elem-parse-this-object)
     (om-elem-timestamp-get-start-time))
 ;; => '(2019 1 1 nil nil)

;; Given the following contents:
; [2019-01-01 Tue 00:00-12:00]

(->> (om-elem-parse-this-object)
     (om-elem-timestamp-get-start-time))
 ;; => '(2019 1 1 0 0)

```

#### om-elem-timestamp-get-end-time `(timestamp)`

Return the end time list of `timestamp` end or nil if not a range.
The return value will be a list as specified by the `time` argument in
[`om-elem-build-timestamp!`](#om-elem-build-timestamp-type-start-key-end-repeater-warning-post-blank).

```el
;; Given the following contents:
; [2019-01-01 Tue]

(->> (om-elem-parse-this-object)
     (om-elem-timestamp-get-end-time))
 ;; => nil

;; Given the following contents:
; [2019-01-01 Tue]--[2019-01-02 Wed]

(->> (om-elem-parse-this-object)
     (om-elem-timestamp-get-end-time))
 ;; => '(2019 1 2 nil nil)

;; Given the following contents:
; [2019-01-01 Tue 00:00-12:00]

(->> (om-elem-parse-this-object)
     (om-elem-timestamp-get-end-time))
 ;; => '(2019 1 1 12 0)

```

#### om-elem-timestamp-is-active-p `(timestamp)`

Return t if `timestamp` is active.

```el
;; Given the following contents:
; <2019-01-01 Tue>

(->> (om-elem-parse-this-object)
     (om-elem-timestamp-is-active-p))
 ;; => t

;; Given the following contents:
; [2019-01-01 Tue]

(->> (om-elem-parse-this-object)
     (om-elem-timestamp-is-active-p))
 ;; => nil

```

#### om-elem-timestamp-is-ranged-p `(timestamp)`

Return t if `timestamp` is ranged.

```el
;; Given the following contents:
; [2019-01-01 Tue]--[2019-01-02 Wed]

(->> (om-elem-parse-this-object)
     (om-elem-timestamp-is-ranged-p))
 ;; => t

;; Given the following contents:
; [2019-01-01 Tue 00:00-12:00]

(->> (om-elem-parse-this-object)
     (om-elem-timestamp-is-ranged-p))
 ;; => t

;; Given the following contents:
; [2019-01-01 Tue]

(->> (om-elem-parse-this-object)
     (om-elem-timestamp-is-ranged-p))
 ;; => nil

```

#### om-elem-timestamp-set-start-time `(time timestamp)`

Set start time of `timestamp` element to `time`.
`time` is a list analogous to the same argument specified in
[`om-elem-build-timestamp!`](#om-elem-build-timestamp-type-start-key-end-repeater-warning-post-blank).

```el
;; Given the following contents:
; [2019-01-02 Wed]

;; If not a range this will turn into a range by moving only the start time.
(->> (om-elem-parse-this-object)
     (om-elem-timestamp-set-start-time '(2019 1 1))
     (om-elem-to-trimmed-string))
 ;; => "[2019-01-01 Tue]--[2019-01-02 Wed]"

;; Set a different time with different precision.
(->> (om-elem-parse-this-object)
     (om-elem-timestamp-set-start-time '(2019 1 1 10 0))
     (om-elem-to-trimmed-string))
 ;; => "[2019-01-01 Tue 10:00]--[2019-01-02 Wed]"

```

#### om-elem-timestamp-set-end-time `(time timestamp)`

Set end time of `timestamp` element to `time`.
`time` is a list analogous to the same argument specified in
[`om-elem-build-timestamp!`](#om-elem-build-timestamp-type-start-key-end-repeater-warning-post-blank).

```el
;; Given the following contents:
; [2019-01-01 Tue]

;; Add the end time
(->> (om-elem-parse-this-object)
     (om-elem-timestamp-set-end-time '(2019 1 2))
     (om-elem-to-trimmed-string))
 ;; => "[2019-01-01 Tue]--[2019-01-02 Wed]"

;; Given the following contents:
; [2019-01-01 Tue]--[2019-01-02 Wed]

;; Remove the end time
(->> (om-elem-parse-this-object)
     (om-elem-timestamp-set-end-time nil)
     (om-elem-to-trimmed-string))
 ;; => "[2019-01-01 Tue]"

```

#### om-elem-timestamp-set-single-time `(time timestamp)`

Set start time of `timestamp` to `time`, and remove the end time.
`time` is a list analogous to the same argument specified in
[`om-elem-build-timestamp!`](#om-elem-build-timestamp-type-start-key-end-repeater-warning-post-blank).

```el
;; Given the following contents:
; [2019-01-01 Tue]

;; Don't make a range
(->> (om-elem-parse-this-object)
     (om-elem-timestamp-set-single-time '(2019 1 2))
     (om-elem-to-trimmed-string))
 ;; => "[2019-01-02 Wed]"

;; Given the following contents:
; [2019-01-01 Tue]--[2019-01-02 Wed]

;; Output is not a range despite input being ranged
(->> (om-elem-parse-this-object)
     (om-elem-timestamp-set-single-time '(2019 1 3))
     (om-elem-to-trimmed-string))
 ;; => "[2019-01-03 Thu]"

```

#### om-elem-timestamp-set-double-time `(time1 time2 timestamp)`

Set start and end time of `timestamp` to `time1` and `time2` respectively.
`time1` and `time2` are lists analogous to the `time` argument specified in
[`om-elem-build-timestamp!`](#om-elem-build-timestamp-type-start-key-end-repeater-warning-post-blank).

```el
;; Given the following contents:
; [2019-01-01 Tue]

;; Make a range
(->> (om-elem-parse-this-object)
     (om-elem-timestamp-set-double-time '(2019 1 2)
					'(2019 1 3))
     (om-elem-to-trimmed-string))
 ;; => "[2019-01-02 Wed]--[2019-01-03 Thu]"

;; Given the following contents:
; [2019-01-01 Tue]--[2019-01-03 Wed]

;; Output is not a range despite input being ranged
(->> (om-elem-parse-this-object)
     (om-elem-timestamp-set-double-time '(2019 1 4)
					'(2019 1 5))
     (om-elem-to-trimmed-string))
 ;; => "[2019-01-04 Fri]--[2019-01-05 Sat]"

```

#### om-elem-timestamp-set-range `(range timestamp)`

Set the `range` of `timestamp`.
If `timestamp` is ranged, keep start time the same and adjust the end
time. If not, make a new end time. The units for `range` are in minutes
if `timestamp` is in long format and days if `timestamp` is in short
format.

```el
;; Given the following contents:
; [2019-01-01 Tue]

;; Use days as the unit for short format
(->> (om-elem-parse-this-object)
     (om-elem-timestamp-set-range 1)
     (om-elem-to-trimmed-string))
 ;; => "[2019-01-01 Tue]--[2019-01-02 Wed]"

;; Given the following contents:
; [2019-01-01 Tue 00:00]

;; Use minutes as the unit for long format
(->> (om-elem-parse-this-object)
     (om-elem-timestamp-set-range 3)
     (om-elem-to-trimmed-string))
 ;; => "[2019-01-01 Tue 00:00]--[2019-01-01 Tue 00:03]"

;; Given the following contents:
; [2019-01-01 Tue]--[2019-01-03 Wed]

;; Set range to 0 to remove end time
(->> (om-elem-parse-this-object)
     (om-elem-timestamp-set-range 0)
     (om-elem-to-trimmed-string))
 ;; => "[2019-01-01 Tue]"

```

#### om-elem-timestamp-set-type `(type timestamp)`

Set type of `timestamp` element to `type`.
`type` can be either 'active' or 'inactive'.

```el
;; Given the following contents:
; [2019-01-01 Tue]

(->> (om-elem-parse-this-object)
     (om-elem-timestamp-set-type 'active)
     (om-elem-to-trimmed-string))
 ;; => "<2019-01-01 Tue>"

```

#### om-elem-timestamp-shift `(n unit timestamp)`

Shift `timestamp` time by `n` `units`.

This function will move the start and end times together; therefore
ranged inputs will always output ranged timestamps and same for
non-ranged. To move the start and end time independently, use
[`om-elem-timestamp-shift-start`](#om-elem-timestamp-shift-start-n-unit-timestamp) or [`om-elem-timestamp-shift-end`](#om-elem-timestamp-shift-end-n-unit-timestamp).

`n` is a positive or negative integer and `unit` is one of 'minute',
'hour', 'day', 'month', or 'year'. Overflows will wrap around
transparently; for instance, supplying 'minute' for `unit` and 90 for `n`
will increase the hour property by 1 and the minute property by 30.

```el
;; Given the following contents:
; [2019-01-01 Tue 12:00]

;; Change each unit, and wrap around to the next unit as needed.
(->> (om-elem-parse-this-object)
     (om-elem-timestamp-shift 30 'minute)
     (om-elem-to-trimmed-string))
 ;; => "[2019-01-01 Tue 12:30]"

(->> (om-elem-parse-this-object)
     (om-elem-timestamp-shift 60 'minute)
     (om-elem-to-trimmed-string))
 ;; => "[2019-01-01 Tue 13:00]"

(->> (om-elem-parse-this-object)
     (om-elem-timestamp-shift 1 'hour)
     (om-elem-to-trimmed-string))
 ;; => "[2019-01-01 Tue 13:00]"

(->> (om-elem-parse-this-object)
     (om-elem-timestamp-shift 1 'day)
     (om-elem-to-trimmed-string))
 ;; => "[2019-01-02 Wed 12:00]"

(->> (om-elem-parse-this-object)
     (om-elem-timestamp-shift 31 'day)
     (om-elem-to-trimmed-string))
 ;; => "[2019-02-01 Fri 12:00]"

(->> (om-elem-parse-this-object)
     (om-elem-timestamp-shift 1 'month)
     (om-elem-to-trimmed-string))
 ;; => "[2019-02-01 Fri 12:00]"

(->> (om-elem-parse-this-object)
     (om-elem-timestamp-shift 13 'month)
     (om-elem-to-trimmed-string))
 ;; => "[2020-02-01 Sat 12:00]"

(->> (om-elem-parse-this-object)
     (om-elem-timestamp-shift 1 'year)
     (om-elem-to-trimmed-string))
 ;; => "[2020-01-01 Wed 12:00]"

(->> (om-elem-parse-this-object)
     (om-elem-timestamp-shift 0 'year)
     (om-elem-to-trimmed-string))
 ;; => "[2019-01-01 Tue 12:00]"

;; Given the following contents:
; [2019-01-01 Tue]

;; Error when shifting hour/minute in short format
(->> (om-elem-parse-this-object)
     (om-elem-timestamp-shift 30 'minute)
     (om-elem-to-trimmed-string))
Error

(->> (om-elem-parse-this-object)
     (om-elem-timestamp-shift 30 'hour)
     (om-elem-to-trimmed-string))
Error

```

#### om-elem-timestamp-shift-start `(n unit timestamp)`

Shift `timestamp` start time by `n` `units`.

`n` and `unit` behave the same as those in [`om-elem-timestamp-shift`](#om-elem-timestamp-shift-n-unit-timestamp).

If `timestamp` is not range, the output will be a ranged timestamp with
the shifted start time and the end time as that of `timestamp`. If this
behavior is not desired, use [`om-elem-timestamp-shift`](#om-elem-timestamp-shift-n-unit-timestamp).

```el
;; Given the following contents:
; [2019-01-01 Tue 12:00]

;; If not a range, change start time and leave implicit end time.
(->> (om-elem-parse-this-object)
     (om-elem-timestamp-shift-start -1 'year)
     (om-elem-to-trimmed-string))
 ;; => "[2018-01-01 Mon 12:00]--[2019-01-01 Tue 12:00]"

;; Given the following contents:
; [2019-01-01 Tue]--[2019-01-03 Thu]

;; Change only start time if a range
(->> (om-elem-parse-this-object)
     (om-elem-timestamp-shift-start 1 'day)
     (om-elem-to-trimmed-string))
 ;; => "[2019-01-02 Wed]--[2019-01-03 Thu]"

```

#### om-elem-timestamp-shift-end `(n unit timestamp)`

Shift `timestamp` end time by `n` `units`.

`n` and `unit` behave the same as those in [`om-elem-timestamp-shift`](#om-elem-timestamp-shift-n-unit-timestamp).

If `timestamp` is not range, the output will be a ranged timestamp with
the shifted end time and the start time as that of `timestamp`. If this
behavior is not desired, use [`om-elem-timestamp-shift`](#om-elem-timestamp-shift-n-unit-timestamp).

```el
;; Given the following contents:
; [2019-01-01 Tue]

;; Shift implicit end time if not a range.
(->> (om-elem-parse-this-object)
     (om-elem-timestamp-shift-end 1 'day)
     (om-elem-to-trimmed-string))
 ;; => "[2019-01-01 Tue]--[2019-01-02 Wed]"

;; Given the following contents:
; [2019-01-01 Tue]--[2019-01-02 Wed]

;; Move only the second time if a range.
(->> (om-elem-parse-this-object)
     (om-elem-timestamp-shift-end 1 'day)
     (om-elem-to-trimmed-string))
 ;; => "[2019-01-01 Tue]--[2019-01-03 Thu]"

```

#### om-elem-timestamp-toggle-active `(timestamp)`

Toggle the active/inactive type of `timestamp` element.

```el
;; Given the following contents:
; [2019-01-01 Tue]

(->> (om-elem-parse-this-object)
     (om-elem-timestamp-toggle-active)
     (om-elem-to-trimmed-string))
 ;; => "<2019-01-01 Tue>"

(->> (om-elem-parse-this-object)
     (om-elem-timestamp-toggle-active)
     (om-elem-timestamp-toggle-active)
     (om-elem-to-trimmed-string))
 ;; => "[2019-01-01 Tue]"

;; Given the following contents:
; [2019-01-01 Tue]--[2019-01-02 Wed]

(->> (om-elem-parse-this-object)
     (om-elem-timestamp-toggle-active)
     (om-elem-to-trimmed-string))
 ;; => "<2019-01-01 Tue>--<2019-01-02 Wed>"

(->> (om-elem-parse-this-object)
     (om-elem-timestamp-toggle-active)
     (om-elem-timestamp-toggle-active)
     (om-elem-to-trimmed-string))
 ;; => "[2019-01-01 Tue]--[2019-01-02 Wed]"

```


## Branch/Child Manipulation


Set, get and map the children of branch nodes.


### Generic

#### om-elem-get-contents `(node)`

Return the contents of `node` as a list.

```el
;; Given the following contents:
; /this/ is a *paragraph*

;; Return objects for object containers
(->> (om-elem-parse-this-element)
     (om-elem-get-contents)
     (-map (function om-elem-get-type)))
 ;; => '(italic plain-text bold)

;; Given the following contents:
; * headline
; stuff
; ** subheadline

;; Return elements for greater elements
(->> (om-elem-parse-this-subtree)
     (om-elem-get-contents)
     (-map (function om-elem-get-type)))
 ;; => '(section headline)

;; Given the following contents:
; #+CALL: ktulu()

;; Throw error when attempting to get contents of a non-container
(->> (om-elem-parse-this-element)
     (om-elem-get-contents)
     (-map (function om-elem-get-type)))
Error

```

#### om-elem-set-contents `(contents node)`

Set the contents of `node` to `contents`.
`contents` is a list of elements or objects; the types permitted in this
list depend on the type of `node`.

```el
;; Given the following contents:
; /this/ is a *paragraph*

;; Set contents for object containers
(->> (om-elem-parse-this-element)
     (om-elem-set-contents (list "this is lame"))
     (om-elem-to-trimmed-string))
 ;; => "this is lame"

;; Given the following contents:
; * headline
; stuff
; ** subheadline

;; Set contents for greater elements
(->> (om-elem-parse-this-subtree)
     (om-elem-set-contents (list (om-elem-build-headline! :title-text "only me" :level 2)))
     (om-elem-to-trimmed-string))
 ;; => "* headline
 ;      ** only me"

;; Given the following contents:
; #+CALL: ktulu()

;; Throw error when attempting to get contents of a non-container
(->> (om-elem-parse-this-element)
     (om-elem-set-contents "nil by mouth")
     (om-elem-to-trimmed-string))
Error

```

#### om-elem-map-contents `(fun node)`

Apply `fun` to the contents of `node`. 
`fun` is a function that takes the current contents as a list and
returns a modified contents as a list.

```el
;; Given the following contents:
; /this/ is a *paragraph*

(->> (om-elem-parse-this-element)
     (om-elem-map-contents (lambda (objs)
			     (append objs (list " ...yeah"))))
     (om-elem-to-trimmed-string))
 ;; => "/this/ is a *paragraph* ...yeah"

;; Given the following contents:
; * headline
; ** subheadline

(->> (om-elem-parse-this-subtree)
     (om-elem-map-contents* (--map (om-elem-shift-property :level 1 it)
				   it))
     (om-elem-to-trimmed-string))
 ;; => "* headline
 ;      *** subheadline"

;; Given the following contents:
; #+CALL: ktulu()

;; Throw error when attempting to map contents of a non-container
(->> (om-elem-parse-this-element)
     (om-elem-map-contents (function ignore))
     (om-elem-to-trimmed-string))
Error

```

#### om-elem-is-empty-p `(node)`

Return t if `node` is empty.
This will throw an error if `node` is not a container type.

```el
;; Given the following contents:
; * dummy
; filled with useless knowledge

(->> (om-elem-parse-this-headline)
     (om-elem-is-empty-p))
 ;; => nil

;; Given the following contents:
; * dummy

(->> (om-elem-parse-this-headline)
     (om-elem-is-empty-p))
 ;; => t

;; Given the following contents:
; #+CALL: ktulu()

;; Throw error when attempting to determine if non-container is empty
(->> (om-elem-parse-this-element)
     (om-elem-is-empty-p))
Error

```


### Headline

#### om-elem-headline-update-item-statistics `(headline)`

Update the statistics cookie for `headline`.
The percent/fraction will be computed as the number of checked items
over the number of items with checkboxes (non-checkbox items will
not be considered).

```el
;; Given the following contents:
; * statistically significant [/]
; - irrelevant data
; - [ ] good data
; - [X] bad data

(->> (om-elem-parse-this-headline)
     (om-elem-headline-update-item-statistics)
     (om-elem-to-trimmed-string))
 ;; => "* statistically significant [1/2]
 ;      - irrelevant data
 ;      - [ ] good data
 ;      - [X] bad data"

;; Given the following contents:
; * statistically significant [%]
; - irrelevant data
; - [ ] good data
; - [X] bad data

(->> (om-elem-parse-this-headline)
     (om-elem-headline-update-item-statistics)
     (om-elem-to-trimmed-string))
 ;; => "* statistically significant [50%]
 ;      - irrelevant data
 ;      - [ ] good data
 ;      - [X] bad data"

;; Given the following contents:
; * statistically significant
; - irrelevant data
; - [ ] good data
; - [X] bad data

(->> (om-elem-parse-this-headline)
     (om-elem-headline-update-item-statistics)
     (om-elem-to-trimmed-string))
 ;; => "* statistically significant
 ;      - irrelevant data
 ;      - [ ] good data
 ;      - [X] bad data"

```

#### om-elem-headline-update-todo-statistics `(headline)`

Update the statistics cookie for `headline`.
The percent/fraction will be computed as the number of done
subheadlines over the number of todo subheadlines (eg non-todo
subheadlines will not be counted).

```el
;; Given the following contents:
; * statistically significant [/]
; ** irrelevant data
; ** TODO good data
; ** DONE bad data

(->> (om-elem-parse-this-subtree)
     (om-elem-headline-update-todo-statistics)
     (om-elem-to-trimmed-string))
 ;; => "* statistically significant [1/2]
 ;      ** irrelevant data
 ;      ** TODO good data
 ;      ** DONE bad data"

;; Given the following contents:
; * statistically significant [%]
; ** irrelevant data
; ** TODO good data
; ** DONE bad data

(->> (om-elem-parse-this-subtree)
     (om-elem-headline-update-todo-statistics)
     (om-elem-to-trimmed-string))
 ;; => "* statistically significant [50%]
 ;      ** irrelevant data
 ;      ** TODO good data
 ;      ** DONE bad data"

;; Given the following contents:
; * statistically significant
; ** irrelevant data
; ** TODO good data
; ** DONE bad data

(->> (om-elem-parse-this-subtree)
     (om-elem-headline-update-todo-statistics)
     (om-elem-to-trimmed-string))
 ;; => "* statistically significant
 ;      ** irrelevant data
 ;      ** TODO good data
 ;      ** DONE bad data"

```

#### om-elem-headline-indent-subheadline `(index headline)`

Indent the subheadline without moving its contents at `index` within `headline`.

```el
;; Given the following contents:
; * one
; ** two
; ** three
; *** four

(->> (om-elem-parse-element-at 1)
     (om-elem-headline-indent-subheadline 0)
     (om-elem-to-trimmed-string))
Error

(->> (om-elem-parse-element-at 1)
     (om-elem-headline-indent-subheadline 1)
     (om-elem-to-trimmed-string))
 ;; => "* one
 ;      ** two
 ;      *** three
 ;      *** four"

```

#### om-elem-headline-indent-subtree `(index headline)`

Indent the subheadline and its contents at `index` within `headline`.

```el
;; Given the following contents:
; * one
; ** two
; ** three
; *** four

(->> (om-elem-parse-element-at 1)
     (om-elem-headline-indent-subtree 1)
     (om-elem-to-trimmed-string))
 ;; => "* one
 ;      ** two
 ;      *** three
 ;      **** four"

```

#### om-elem-headline-unindent-subheadline `(index child-index headline)`

Unindent subheadline at `child-index` in the subheadline at `index` in `headline`.
This will not move the contents under the headline at `child-index`.

```el
;; Given the following contents:
; * one
; ** two
; ** three
; *** four
; *** four
; *** four

(->> (om-elem-parse-element-at 1)
     (om-elem-headline-unindent-subheadline 1 1)
     (om-elem-to-trimmed-string))
 ;; => "* one
 ;      ** two
 ;      ** three
 ;      *** four
 ;      ** four
 ;      *** four"

```

#### om-elem-headline-unindent-subtree `(index headline)`

Unindent all subheadlines under the subheadline at `index` in `headline`.

```el
;; Given the following contents:
; * one
; ** two
; ** three
; *** four
; *** four
; *** four

(->> (om-elem-parse-element-at 1)
     (om-elem-headline-unindent-subtree 1)
     (om-elem-to-trimmed-string))
 ;; => "* one
 ;      ** two
 ;      ** three
 ;      ** four
 ;      ** four
 ;      ** four"

```


### Plain List

#### om-elem-plain-list-set-type `(type plain-list)`

Set the type of `plain-list` greater element to `type`.
`type` is '-', '+', or 'ordered'.

```el
;; Given the following contents:
; - [ ] one
; - [X] two

(->> (om-elem-parse-this-element)
     (om-elem-plain-list-set-type 'ordered)
     (om-elem-to-trimmed-string))
 ;; => "1. [ ] one
 ;      2. [X] two"

;; Given the following contents:
; 1. [ ] one
; 2. [X] two

(->> (om-elem-parse-this-element)
     (om-elem-plain-list-set-type '-)
     (om-elem-to-trimmed-string))
 ;; => "- [ ] one
 ;      - [X] two"

```

#### om-elem-plain-list-indent-item `(index plain-list)`

Indent the subitem at `index` in `plain-list` without moving items below it.

```el
;; Given the following contents:
; - one
; - two
;   - three
; - four

;; It makes no sense to indent the first item
(->> (om-elem-parse-element-at 1)
     (om-elem-plain-list-indent-item 0)
     (om-elem-to-trimmed-string))
Error

(->> (om-elem-parse-element-at 1)
     (om-elem-plain-list-indent-item 1)
     (om-elem-to-trimmed-string))
 ;; => "- one
 ;        - two
 ;        - three
 ;      - four"

(->> (om-elem-parse-element-at 1)
     (om-elem-plain-list-indent-item 2)
     (om-elem-to-trimmed-string))
 ;; => "- one
 ;      - two
 ;        - three
 ;        - four"

```

#### om-elem-plain-list-indent-item-tree `(index plain-list)`

Indent the subitem at `index` in `plain-list` and move items below it.

```el
;; Given the following contents:
; - one
; - two
;   - three
; - four

(->> (om-elem-parse-element-at 1)
     (om-elem-plain-list-indent-item-tree 1)
     (om-elem-to-trimmed-string))
 ;; => "- one
 ;        - two
 ;          - three
 ;      - four"

```

#### om-elem-plain-list-unindent-item `(index child-index plain-list)`

Unindent subitem at `child-index` in the subitem at `index` in `plain-list`.
This will not move the contents under the item at `child-index`.

```el
;; Given the following contents:
; - one
; - two
;   - three
;   - three
;   - three
; - four

(->> (om-elem-parse-element-at 1)
     (om-elem-plain-list-unindent-item 1 0)
     (om-elem-to-trimmed-string))
 ;; => "- one
 ;      - two
 ;      - three
 ;        - three
 ;        - three
 ;      - four"

(->> (om-elem-parse-element-at 1)
     (om-elem-plain-list-unindent-item 1 1)
     (om-elem-to-trimmed-string))
 ;; => "- one
 ;      - two
 ;        - three
 ;      - three
 ;        - three
 ;      - four"

(->> (om-elem-parse-element-at 1)
     (om-elem-plain-list-unindent-item 2 1)
     (om-elem-to-trimmed-string))
 ;; => "- one
 ;      - two
 ;        - three
 ;        - three
 ;        - three
 ;      - four"

```

#### om-elem-plain-list-unindent-items `(index plain-list)`

Unindent all items under the item at `index` in `plain-list`.

```el
;; Given the following contents:
; - one
; - two
;   - three
;   - three
;   - three
; - four

(->> (om-elem-parse-element-at 1)
     (om-elem-plain-list-unindent-items 1)
     (om-elem-to-trimmed-string))
 ;; => "- one
 ;      - two
 ;      - three
 ;      - three
 ;      - three
 ;      - four"

(->> (om-elem-parse-element-at 1)
     (om-elem-plain-list-unindent-items 2)
     (om-elem-to-trimmed-string))
 ;; => "- one
 ;      - two
 ;        - three
 ;        - three
 ;        - three
 ;      - four"

```


### Table

#### om-elem-table-get-cell `(row-index column-index table)`

Return table-cell at `row-index` and `column-index` in `table` element.
`h-`lines do not count toward row indices, and all indices are
zero-indexed.

```el
;; Given the following contents:
; | 1 | 2 | 3 |
; |---+---+---|
; | a | b | c |

(->> (om-elem-parse-this-element)
     (om-elem-table-get-cell 0 0)
     (om-elem--get-contents)
     (car))
 ;; => "1"

(->> (om-elem-parse-this-element)
     (om-elem-table-get-cell 1 1)
     (om-elem--get-contents)
     (car))
 ;; => "b"

(->> (om-elem-parse-this-element)
     (om-elem-table-get-cell -1 -1)
     (om-elem--get-contents)
     (car))
 ;; => "c"

```

#### om-elem-table-replace-cell `(row-index column-index cell table)`

Replace a cell in `table` with `cell` (a table-cell element).
`row-index` and `column-index` are zero-indexed integers pointing to the
position of the cell to be replaced.

```el
;; Given the following contents:
; | 1 | 2 |
; |---+---|
; | a | b |

(->> (om-elem-parse-this-element)
     (om-elem-table-replace-cell 0 0 (om-elem-build-table-cell "2"))
     (om-elem-to-trimmed-string))
 ;; => "| 2 | 2 |
 ;      |---+---|
 ;      | a | b |"

(->> (om-elem-parse-this-element)
     (om-elem-table-replace-cell -1 -1 (om-elem-build-table-cell "B"))
     (om-elem-to-trimmed-string))
 ;; => "| 1 | 2 |
 ;      |---+---|
 ;      | a | B |"

```

#### om-elem-table-replace-cell! `(row-index column-index cell-text table)`

Replace a cell in `table` with `cell-text`.
`cell-text` is a string which will replace the contents of the cell at
`row-index` and `column-index` (zero-indexed integers).

```el
;; Given the following contents:
; | 1 | 2 |
; |---+---|
; | a | b |

(->> (om-elem-parse-this-element)
     (om-elem-table-replace-cell! 0 0 "2")
     (om-elem-to-trimmed-string))
 ;; => "| 2 | 2 |
 ;      |---+---|
 ;      | a | b |"

(->> (om-elem-parse-this-element)
     (om-elem-table-replace-cell! -1 -1 "B")
     (om-elem-to-trimmed-string))
 ;; => "| 1 | 2 |
 ;      |---+---|
 ;      | a | B |"

```

#### om-elem-table-clear-cell `(row-index column-index table)`

Clear a cell in `table`.
`row-index` and `column-index` are zero-indexed integers pointing to the
position of the cell to be replaced.

```el
;; Given the following contents:
; | 1 | 2 |
; |---+---|
; | a | b |

(->> (om-elem-parse-this-element)
     (om-elem-table-clear-cell 0 0)
     (om-elem-to-trimmed-string))
 ;; => "|   | 2 |
 ;      |---+---|
 ;      | a | b |"

(->> (om-elem-parse-this-element)
     (om-elem-table-clear-cell -1 -1)
     (om-elem-to-trimmed-string))
 ;; => "| 1 | 2 |
 ;      |---+---|
 ;      | a |   |"

```

#### om-elem-table-delete-column `(column-index table)`

Delete the column at `column-index` in `table`.

```el
;; Given the following contents:
; | a | b |
; |---+---|
; | c | d |

(->> (om-elem-parse-element-at 1)
     (om-elem-table-delete-column 0)
     (om-elem-to-trimmed-string))
 ;; => "| b |
 ;      |---|
 ;      | d |"

(->> (om-elem-parse-element-at 1)
     (om-elem-table-delete-column 1)
     (om-elem-to-trimmed-string))
 ;; => "| a |
 ;      |---|
 ;      | c |"

(->> (om-elem-parse-element-at 1)
     (om-elem-table-delete-column -1)
     (om-elem-to-trimmed-string))
 ;; => "| a |
 ;      |---|
 ;      | c |"

```

#### om-elem-table-replace-column `(column-index column-cells table)`

Replace column at `column-index` in `table` with `column-cells`.
`column-index` is the index of the column (starting at zero) and
`column-cells` is a list of table-cell objects.

```el
;; Given the following contents:
; | a | b |
; |---+---|
; | c | d |

(->> (om-elem-parse-element-at 1)
     (om-elem-table-replace-column 0 (list (om-elem-build-table-cell "A")
					   (om-elem-build-table-cell "B")))
     (om-elem-to-trimmed-string))
 ;; => "| A | b |
 ;      |---+---|
 ;      | B | d |"

(->> (om-elem-parse-element-at 1)
     (om-elem-table-replace-column -1 (list (om-elem-build-table-cell "A")
					    (om-elem-build-table-cell "B")))
     (om-elem-to-trimmed-string))
 ;; => "| a | A |
 ;      |---+---|
 ;      | c | B |"

```

#### om-elem-table-replace-column! `(column-index column-text table)`

Replace column at `column-index` in `table` with `column-text`.
`column-index` is the index of the column (starting at zero) and
`column-text` is a list of text to be made into table-cell objects.

```el
;; Given the following contents:
; | a | b |
; |---+---|
; | c | d |

(->> (om-elem-parse-element-at 1)
     (om-elem-table-replace-column! 0 '("A" "B"))
     (om-elem-to-trimmed-string))
 ;; => "| A | b |
 ;      |---+---|
 ;      | B | d |"

(->> (om-elem-parse-element-at 1)
     (om-elem-table-replace-column! -1 '("A" "B"))
     (om-elem-to-trimmed-string))
 ;; => "| a | A |
 ;      |---+---|
 ;      | c | B |"

```

#### om-elem-table-clear-column `(column-index table)`

Clear the column at `column-index` in `table`.

```el
;; Given the following contents:
; | a | b |
; |---+---|
; | c | d |

(->> (om-elem-parse-element-at 1)
     (om-elem-table-clear-column 0)
     (om-elem-to-trimmed-string))
 ;; => "|   | b |
 ;      |---+---|
 ;      |   | d |"

(->> (om-elem-parse-element-at 1)
     (om-elem-table-clear-column -1)
     (om-elem-to-trimmed-string))
 ;; => "| a |   |
 ;      |---+---|
 ;      | c |   |"

```

#### om-elem-table-replace-row `(row-index row-cells table)`

Replace row at `row-index` in `table` with `row-cells`.
`row-index` is the index of the row (starting at zero) and
`row-cells` is a list of table-cell objects.

```el
;; Given the following contents:
; | a | b |
; |---+---|
; | c | d |

(->> (om-elem-parse-element-at 1)
     (om-elem-table-replace-row 0 (om-elem-build-table-row (om-elem-build-table-cell "A")
							   (om-elem-build-table-cell "B")))
     (om-elem-to-trimmed-string))
 ;; => "| A | B |
 ;      |---+---|
 ;      | c | d |"

(->> (om-elem-parse-element-at 1)
     (om-elem-table-replace-row -1 (om-elem-build-table-row (om-elem-build-table-cell "A")
							    (om-elem-build-table-cell "B")))
     (om-elem-to-trimmed-string))
 ;; => "| a | b |
 ;      |---+---|
 ;      | A | B |"

```

#### om-elem-table-replace-row! `(row-index row-text table)`

Replace row at `row-index` in `table` with `row-text`.
`row-index` is the index of the row (starting at zero) and
`row-text` is a list of text to be made into table-cell objects.

```el
;; Given the following contents:
; | a | b |
; |---+---|
; | c | d |

(->> (om-elem-parse-element-at 1)
     (om-elem-table-replace-row! 0 '("A" "B"))
     (om-elem-to-trimmed-string))
 ;; => "| A | B |
 ;      |---+---|
 ;      | c | d |"

(->> (om-elem-parse-element-at 1)
     (om-elem-table-replace-row! -1 '("A" "B"))
     (om-elem-to-trimmed-string))
 ;; => "| a | b |
 ;      |---+---|
 ;      | A | B |"

```

#### om-elem-table-clear-row `(row-index table)`

Clear the row at `row-index` in `table`.

```el
;; Given the following contents:
; | a | b |
; |---+---|
; | c | d |

(->> (om-elem-parse-element-at 1)
     (om-elem-table-clear-row 0)
     (om-elem-to-trimmed-string))
 ;; => "|   |   |
 ;      |---+---|
 ;      | c | d |"

(->> (om-elem-parse-element-at 1)
     (om-elem-table-clear-row -1)
     (om-elem-to-trimmed-string))
 ;; => "| a | b |
 ;      |---+---|
 ;      |   |   |"

```

#### om-elem-table-delete-row `(row-index table)`

Delete the row at `row-index` in `table`.

```el
;; Given the following contents:
; | a | b |
; |---+---|
; | c | d |

(->> (om-elem-parse-element-at 1)
     (om-elem-table-delete-row 0)
     (om-elem-to-trimmed-string))
 ;; => "|---+---|
 ;      | c | d |"

(->> (om-elem-parse-element-at 1)
     (om-elem-table-delete-row 1)
     (om-elem-to-trimmed-string))
 ;; => "| a | b |
 ;      | c | d |"

(->> (om-elem-parse-element-at 1)
     (om-elem-table-delete-row -1)
     (om-elem-to-trimmed-string))
 ;; => "| a | b |
 ;      |---+---|"

```

#### om-elem-table-insert-column `(column-index column-cells table)`

Insert `column-cells` at `column-index` in `table`.

```el
;; Given the following contents:
; | a | b |
; |---+---|
; | c | d |

(->> (om-elem-parse-element-at 1)
     (om-elem-table-insert-column 1 (list (om-elem-build-table-cell "x")
					  (om-elem-build-table-cell "y")))
     (om-elem-to-trimmed-string))
 ;; => "| a | x | b |
 ;      |---+---+---|
 ;      | c | y | d |"

(->> (om-elem-parse-element-at 1)
     (om-elem-table-insert-column -1 (list (om-elem-build-table-cell "x")
					   (om-elem-build-table-cell "y")))
     (om-elem-to-trimmed-string))
 ;; => "| a | b | x |
 ;      |---+---+---|
 ;      | c | d | y |"

```

#### om-elem-table-insert-column! `(column-index column-text table)`

Insert `column-text` at `column-index` in `table`.

```el
;; Given the following contents:
; | a | b |
; |---+---|
; | c | d |

(->> (om-elem-parse-element-at 1)
     (om-elem-table-insert-column! 1 '("x" "y"))
     (om-elem-to-trimmed-string))
 ;; => "| a | x | b |
 ;      |---+---+---|
 ;      | c | y | d |"

(->> (om-elem-parse-element-at 1)
     (om-elem-table-insert-column! -1 '("x" "y"))
     (om-elem-to-trimmed-string))
 ;; => "| a | b | x |
 ;      |---+---+---|
 ;      | c | d | y |"

```

#### om-elem-table-insert-row `(row-index row table)`

Insert `row` at `row-index` in `table`.

```el
;; Given the following contents:
; | a | b |
; |---+---|
; | c | d |

(->> (om-elem-parse-element-at 1)
     (om-elem-table-insert-row 1 (om-elem-build-table-row (om-elem-build-table-cell "x")
							  (om-elem-build-table-cell "y")))
     (om-elem-to-trimmed-string))
 ;; => "| a | b |
 ;      | x | y |
 ;      |---+---|
 ;      | c | d |"

(->> (om-elem-parse-element-at 1)
     (om-elem-table-insert-row 2 (om-elem-build-table-row (om-elem-build-table-cell "x")
							  (om-elem-build-table-cell "y")))
     (om-elem-to-trimmed-string))
 ;; => "| a | b |
 ;      |---+---|
 ;      | x | y |
 ;      | c | d |"

(->> (om-elem-parse-element-at 1)
     (om-elem-table-insert-row -1 (om-elem-build-table-row (om-elem-build-table-cell "x")
							   (om-elem-build-table-cell "y")))
     (om-elem-to-trimmed-string))
 ;; => "| a | b |
 ;      |---+---|
 ;      | c | d |
 ;      | x | y |"

```

#### om-elem-table-insert-row! `(row-index row-text table)`

Insert `row-text` at `row-index` in `table`.

```el
;; Given the following contents:
; | a | b |
; |---+---|
; | c | d |

(->> (om-elem-parse-element-at 1)
     (om-elem-table-insert-row! 1 '("x" "y"))
     (om-elem-to-trimmed-string))
 ;; => "| a | b |
 ;      | x | y |
 ;      |---+---|
 ;      | c | d |"

(->> (om-elem-parse-element-at 1)
     (om-elem-table-insert-row! 2 '("x" "y"))
     (om-elem-to-trimmed-string))
 ;; => "| a | b |
 ;      |---+---|
 ;      | x | y |
 ;      | c | d |"

(->> (om-elem-parse-element-at 1)
     (om-elem-table-insert-row! -1 '("x" "y"))
     (om-elem-to-trimmed-string))
 ;; => "| a | b |
 ;      |---+---|
 ;      | c | d |
 ;      | x | y |"

```


## Node Matching


Use pattern-matching to selectively perform operations on nodes in trees.

#### om-elem-match `(pattern node)`

Return a list of all nodes matching `pattern` in `node`.

`pattern` is a list of form ([slicer [arg1] [arg2]] cond1 [cond2 ...]).

'slicer' is an optional prefix to the pattern describing how many
and which matches to return. If not given, all matches are
returned. Possible values are:

- :first - return the first match
- :last - return the last match
- :nth `n` - return the nth match where `n` is an integer denoting the
    index to return (starting at 0). It may be a negative number to
    start counting at the end of the match list, in which case -1 is the
    last index
- :sub `a` `b` - return a sublist between indices `a` and `b`. `a` and `b` follow
    the same rules as :nth

'cond' denotes conditions that that match nodes in the parse
tree. This first condition will select matches within the
contents of `node`, the next condition will select matches within
the matches from the first condition, and so on. The types of
conditions are:

- `pred` - match when `pred` evaluates to t; `pred` is a unary function that
    takes the current node as its argument
- `type` - match when the node's type is `eq` to `type` (a symbol)
- `index` - match when the node's index is `=` to `index` (an integer).
    The first index is zero. If `index` is negative, start counting
    backward from the end of contents where -1 is the last node
- (`op` `index`) - match when (`op` `node-index` `index`) returns t. `op` is
    one of '<', '>', '<=', or '>='
- `plist` - match nodes with the same properties and values as `plist`
- :many - match zero or more levels, must have at least one
    sub-pattern after it
- :many! - like :many but do not match within other matches
- :any - always match exactly one node

Additionally, conditions may be further refined using boolean forms:

- (:and c1 c2 [c3 ...]) - match when all conditions are true
- (:or c1 c2 [c3 ...]) - match when at least one condition is true
- (:not c) - match when condition is not true

The 'c' members in the forms above are one of any of the condition
types except :many, :many!, and :any. Boolean forms may be
nested within each other.

```el
;; Given the following contents:
; * headline one
; ** TODO headline two
; ** COMMENT headline three
; ** headline four

;; Use a symbol to match a type, in this case all headlines.
(->> (om-elem-parse-this-subtree)
     (om-elem-match '(headline))
     (--map (om-elem-to-trimmed-string it)))
 ;; => '("** TODO headline two" "** COMMENT headline three" "** headline four")

;; Use integers specify the index to return. Negative integers count from the
;; end. Out of range integers return nil
(->> (om-elem-parse-this-subtree)
     (om-elem-match '(1))
     (--map (om-elem-to-trimmed-string it)))
 ;; => '("** COMMENT headline three")

(->> (om-elem-parse-this-subtree)
     (om-elem-match '(-1))
     (--map (om-elem-to-trimmed-string it)))
 ;; => '("** headline four")

(->> (om-elem-parse-this-subtree)
     (om-elem-match '(3))
     (--map (om-elem-to-trimmed-string it)))
 ;; => nil

;; Use a two-membered list with an operator and an integer to match a range of
;; indices. Allowed operators are <, >, <=, and and >=.
(->> (om-elem-parse-this-subtree)
     (om-elem-match '((> 0)))
     (--map (om-elem-to-trimmed-string it)))
 ;; => '("** COMMENT headline three" "** headline four")

(->> (om-elem-parse-this-subtree)
     (om-elem-match '((>= 1)))
     (--map (om-elem-to-trimmed-string it)))
 ;; => '("** COMMENT headline three" "** headline four")

;; Use a plist to match based on an elements properties.
(->> (om-elem-parse-this-subtree)
     (om-elem-match '((:todo-keyword "TODO")))
     (--map (om-elem-to-trimmed-string it)))
 ;; => '("** TODO headline two")

(->> (om-elem-parse-this-subtree)
     (om-elem-match '((:todo-keyword nil)))
     (--map (om-elem-to-trimmed-string it)))
 ;; => '("** COMMENT headline three" "** headline four")

(->> (om-elem-parse-this-subtree)
     (om-elem-match '((:todo-keyword "DONE")))
     (--map (om-elem-to-trimmed-string it)))
 ;; => nil

;; Given the following contents:
; * headline one
; this is *text1* of *text2*
; ** headline two
; here is more *text3*
; *** headline three
; and here is even more *text4* and *text5*
; **** headline 4

;; Specify multiple levels of matching using multiple queries.
(->> (om-elem-parse-this-subtree)
     (om-elem-match '(section paragraph bold))
     (--map (om-elem-to-trimmed-string it)))
 ;; => '("*text1*" "*text2*")

;; Use the keyword :any as a wildcard to match any element at a particular
;; level.
(->> (om-elem-parse-this-subtree)
     (om-elem-match '(:any :any bold))
     (--map (om-elem-to-trimmed-string it)))
 ;; => '("*text1*" "*text2*")

(->> (om-elem-parse-this-subtree)
     (om-elem-match '(section paragraph :any))
     (--map (om-elem-to-trimmed-string it)))
 ;; => '("this is" "*text1*" "of" "*text2*" "")

(->> (om-elem-parse-this-subtree)
     (om-elem-match '(:any bold))
     (--map (om-elem-to-trimmed-string it)))
 ;; => nil

;; Use the keyword :many to match one or more levels of any element.
(->> (om-elem-parse-this-subtree)
     (om-elem-match '(:many bold))
     (--map (om-elem-to-trimmed-string it)))
 ;; => '("*text1*" "*text2*" "*text3*" "*text4*" "*text5*")

;; Use the keyword :many! to match one or more levels, except unlike :many do
;; not match within any elements that have already matched.
(->> (om-elem-parse-this-subtree)
     (om-elem-match '(headline :many! headline))
     (--map (om-elem-to-trimmed-string it)))
 ;; => '("*** headline three
 ;     and here is even more *text4* and *text5*
 ;     **** headline 4")

;; Given the following contents:
; * headline one
; ** TODO headline two
; ** COMMENT headline three
; ** headline four

;; Find the first subheadline
(->> (om-elem-parse-this-subtree)
     (om-elem-match '(:first headline))
     (car)
     (om-elem-to-trimmed-string))
 ;; => "** TODO headline two"

;; Given the following contents:
; * headline one
; ** TODO headline two
; ** COMMENT headline three
; ** headline four

;; Find the last subheadline
(->> (om-elem-parse-this-subtree)
     (om-elem-match '(:last headline))
     (car)
     (om-elem-to-trimmed-string))
 ;; => "** headline four"

```

#### om-elem-match-delete `(pattern node)`

Remove nodes matching `pattern` from `node` and return modified `node`.

`pattern` follows the same rules as [`om-elem-match`](#om-elem-match-pattern-node).

```el
;; Given the following contents:
; * headline one
; ** headline two
; ** headline three
; ** headline four

;; Selectively delete headlines
(->> (om-elem-parse-this-subtree)
     (om-elem-match-delete '(headline))
     (om-elem-to-trimmed-string))
 ;; => "* headline one"

(->> (om-elem-parse-this-subtree)
     (om-elem-match-delete '(:first headline))
     (om-elem-to-trimmed-string))
 ;; => "* headline one
 ;      ** headline three
 ;      ** headline four"

(->> (om-elem-parse-this-subtree)
     (om-elem-match-delete '(:last headline))
     (om-elem-to-trimmed-string))
 ;; => "* headline one
 ;      ** headline two
 ;      ** headline three"

```

#### om-elem-match-extract `(pattern node)`

Remove nodes matching `pattern` from `node`.
Return cons cell where the car is a list of all removed nodes and
the cdr is the modified `node`.

`pattern` follows the same rules as [`om-elem-match`](#om-elem-match-pattern-node).

```el
;; Given the following contents:
; pull me /under/

(--> (om-elem-parse-this-element)
     (om-elem-match-extract '(:many italic)
			    it)
     (cons (-map (function om-elem-to-trimmed-string)
		 (car it))
	   (om-elem-to-trimmed-string (cdr it))))
 ;; => '(("/under/") . "pull me")

```

#### om-elem-match-map `(pattern fun node)`

Apply `fun` to nodes matching `pattern` in `node`.
`fun` is a unary function that takes a node and returns a new node
which will replace the original.

`pattern` follows the same rules as [`om-elem-match`](#om-elem-match-pattern-node).

```el
;; Given the following contents:
; * headline one
; ** TODO headline two
; ** headline three
; ** headline four

;; Selectively mark headlines as DONE
(->> (om-elem-parse-this-subtree)
     (om-elem-match-map '(headline)
       (lambda (it)
	 (om-elem-set-property :todo-keyword "DONE" it)))
     (om-elem-to-trimmed-string))
 ;; => "* headline one
 ;      ** DONE headline two
 ;      ** DONE headline three
 ;      ** DONE headline four"

(->> (om-elem-parse-this-subtree)
     (om-elem-match-map* '(:first headline)
       (om-elem-set-property :todo-keyword "DONE" it))
     (om-elem-to-trimmed-string))
 ;; => "* headline one
 ;      ** DONE headline two
 ;      ** headline three
 ;      ** headline four"

(->> (om-elem-parse-this-subtree)
     (om-elem-match-map '(:last headline)
       (-partial (function om-elem-set-property)
		 :todo-keyword "DONE"))
     (om-elem-to-trimmed-string))
 ;; => "* headline one
 ;      ** TODO headline two
 ;      ** headline three
 ;      ** DONE headline four"

```

#### om-elem-match-mapcat `(pattern fun node)`

Apply `fun` to nodes matching `pattern` in `node`.
`fun` is a unary function that takes a node and returns a list of new
nodes which will be spliced in place of the original node.

`pattern` follows the same rules as [`om-elem-match`](#om-elem-match-pattern-node).

```el
;; Given the following contents:
; * one
; ** two

(->> (om-elem-parse-this-subtree)
     (om-elem-match-mapcat* '(:first headline)
       (list (om-elem-build-headline! :title-text "1.5" :level 2)
	     it))
     (om-elem-to-trimmed-string))
 ;; => "* one
 ;      ** 1.5
 ;      ** two"

```

#### om-elem-match-replace `(pattern node* node)`

Replace nodes matching `pattern` with `node`* within `node`.
Return modified `node`.

`pattern` follows the same rules as [`om-elem-match`](#om-elem-match-pattern-node).

```el
;; Given the following contents:
; *1* 2 *3* 4 *5* 6 *7* 8 *9* 10

(->> (om-elem-parse-this-element)
     (om-elem-match-replace '(:many bold)
       (om-elem-build-bold :post-blank 1 "0"))
     (om-elem-to-trimmed-string))
 ;; => "*0* 2 *0* 4 *0* 6 *0* 8 *0* 10"

```

#### om-elem-match-insert-before `(pattern node* node)`

Insert `node`* before every node matching `pattern` in `node`.
Return modified `node`.

`pattern` follows the same rules as [`om-elem-match`](#om-elem-match-pattern-node).

```el
;; Given the following contents:
; * one
; ** two
; ** three

(->> (om-elem-parse-this-subtree)
     (om-elem-match-insert-before '(headline)
       (om-elem-build-headline! :title-text "new" :level 2))
     (om-elem-to-trimmed-string))
 ;; => "* one
 ;      ** new
 ;      ** two
 ;      ** new
 ;      ** three"

```

#### om-elem-match-insert-after `(pattern node* node)`

Insert `node`* after every node matching `pattern` in `node`.
Return modified `node`.

`pattern` follows the same rules as [`om-elem-match`](#om-elem-match-pattern-node).

```el
;; Given the following contents:
; * one
; ** two
; ** three

(->> (om-elem-parse-this-subtree)
     (om-elem-match-insert-after '(headline)
       (om-elem-build-headline! :title-text "new" :level 2))
     (om-elem-to-trimmed-string))
 ;; => "* one
 ;      ** two
 ;      ** new
 ;      ** three
 ;      ** new"

```

#### om-elem-match-insert-within `(pattern index node* node)`

Insert new `node`* at `index` into nodes matching `pattern` in `node`.
Return modified `node`.

`pattern` follows the same rules as [`om-elem-match`](#om-elem-match-pattern-node) with the exception
that `pattern` may be nil. In this case `node`* will be inserted at `index`
in the immediate, top level contents of `node`.

```el
;; Given the following contents:
; * one
; ** two
; ** three

(->> (om-elem-parse-this-subtree)
     (om-elem-match-insert-within '(headline)
	 0 (om-elem-build-headline! :title-text "new" :level 3))
     (om-elem-to-trimmed-string))
 ;; => "* one
 ;      ** two
 ;      *** new
 ;      ** three
 ;      *** new"

;; The nil pattern denotes top-level element
(->> (om-elem-parse-this-subtree)
     (om-elem-match-insert-within nil 1 (om-elem-build-headline! :title-text "new" :level 2))
     (om-elem-to-trimmed-string))
 ;; => "* one
 ;      ** two
 ;      ** new
 ;      ** three"

```

#### om-elem-match-splice-before `(pattern nodes* node)`

Splice `nodes`* before every nodes matching `pattern` in `node`.
Return modified `node`. `nodes`* is a list of nodes.

`pattern` follows the same rules as [`om-elem-match`](#om-elem-match-pattern-node).

```el
;; Given the following contents:
; * one
; ** two
; ** three

(->> (om-elem-parse-this-subtree)
     (om-elem-match-splice-before '(0)
       (list (om-elem-build-headline! :title-text "new0" :level 2)
	     (om-elem-build-headline! :title-text "new1" :level 2)))
     (om-elem-to-trimmed-string))
 ;; => "* one
 ;      ** new0
 ;      ** new1
 ;      ** two
 ;      ** three"

```

#### om-elem-match-splice-after `(pattern nodes* node)`

Splice `nodes`* after every nodes matching `pattern` in `node`.
Return modified `node`. `nodes`* is a list of nodes.

`pattern` follows the same rules as [`om-elem-match`](#om-elem-match-pattern-node).

```el
;; Given the following contents:
; * one
; ** two
; ** three

(->> (om-elem-parse-this-subtree)
     (om-elem-match-splice-after '(0)
       (list (om-elem-build-headline! :title-text "new0" :level 2)
	     (om-elem-build-headline! :title-text "new1" :level 2)))
     (om-elem-to-trimmed-string))
 ;; => "* one
 ;      ** two
 ;      ** new0
 ;      ** new1
 ;      ** three"

```

#### om-elem-match-splice-within `(pattern index nodes* node)`

Splice new `nodes`* at `index` into nodes matching `pattern` in `node`.
Return modified `node`. `nodes`* is a list of nodes.

`pattern` follows the same rules as [`om-elem-match`](#om-elem-match-pattern-node) with the exception
that `pattern` may be nil. In this case `nodes`* will be inserted at `index`
in the immediate, top level contents of `node`.

```el
;; Given the following contents:
; * one
; ** two
; ** three

(->> (om-elem-parse-this-subtree)
     (om-elem-match-splice-within nil 1 (list (om-elem-build-headline! :title-text "new0" :level 2)
					      (om-elem-build-headline! :title-text "new1" :level 2)))
     (om-elem-to-trimmed-string))
 ;; => "* one
 ;      ** two
 ;      ** new0
 ;      ** new1
 ;      ** three"

```


## Buffer Side Effects


Insert and update elements and objects into buffers.


### Insert

#### om-elem-insert `(point node)`

Convert `node` to a string and insert at `point` in the current buffer.
Return `node`.

```el
;; Given the following contents:
; * one
; 

(->> (om-elem-build-headline! :title-text "two")
     (om-elem-insert (point-max)))
 ;; Output these buffer contents
 ;; $> "* one
 ;      * two"

;; Given the following contents:
; a *game* or a /boy/

(->> (om-elem-build-paragraph! "we don't care if you're")
     (om-elem-insert (point-min)))
 ;; Output these buffer contents
 ;; $> "we don't care if you're
 ;      a *game* or a /boy/"

```

#### om-elem-insert-tail `(point node)`

Like [`om-elem-insert`](#om-elem-insert-point-node) but insert `node` at `point` and move to end of insertion.

```el
no examples :(
```


### Update

#### om-elem-update `(fun node)`

Replace `node` in the current buffer with a new one. 
`fun` is a function that takes `node` as its only argument and returns a
modified `node`. This modified element is then written in place of the
old element in the current buffer.

```el
;; Given the following contents:
; * TODO win grammy

(->> (om-elem-parse-this-headline)
     (om-elem-update (lambda (hl)
		       (om-elem-set-property :todo-keyword "DONE" hl))))
 ;; Output these buffer contents
 ;; $> "* DONE win grammy"

;; Given the following contents:
; * win grammy [0/0]
; - [ ] write punk song
; - [ ] get new vocalist
; - [ ] sell 2 singles

(->> (om-elem-parse-this-headline)
     (om-elem-update* (->> (om-elem-match-map '(:many item)
			     (function om-elem-item-toggle-checkbox)
			     it)
			   (om-elem-headline-update-item-statistics))))
 ;; Output these buffer contents
 ;; $> "* win grammy [3/3]
 ;      - [X] write punk song
 ;      - [X] get new vocalist
 ;      - [X] sell 2 singles"

```

#### om-elem-update-object-at `(point fun)`

Update object under `point` using `fun`.
`fun` takes an object and returns a modified object

```el
;; Given the following contents:
; [[http://example.com][desc]]

(om-elem-update-object-at* (point)
  (om-elem-set-property :path "//buymoreram.com" it))
 ;; Output these buffer contents
 ;; $> "[[http://buymoreram.com][desc]]"

```

#### om-elem-update-element-at `(point fun)`

Update element under `point` using `fun`.
`fun` takes an element and returns a modified element

```el
;; Given the following contents:
; #+CALL: ktulu()

(om-elem-update-element-at* (point)
  (om-elem-set-properties (list :call "cthulhu" :inside-header '(:cache no)
				 :arguments '("x=4")
				 :end-header '(:results html))
			  it))
 ;; Output these buffer contents
 ;; $> "#+CALL: cthulhu[:cache no](x=4) :results html"

```

#### om-elem-update-table-row-at `(point fun)`

Update table-row under `point` using `fun`.
`fun` takes an table-row and returns a modified table-row

```el
;; Given the following contents:
; | a | b |

(om-elem-update-table-row-at* (point)
  (om-elem-map-contents* (cons (om-elem-build-table-cell! "0")
			       it)
			 it))
 ;; Output these buffer contents
 ;; $> "| 0 | a | b |"

```

#### om-elem-update-item-at `(point fun)`

Update item under `point` using `fun`.
`fun` takes an item and returns a modified item

```el
;; Given the following contents:
; - [ ] thing

(om-elem-update-item-at* (point)
  (om-elem-item-toggle-checkbox it))
 ;; Output these buffer contents
 ;; $> "- [X] thing"

```

#### om-elem-update-headline-at `(point fun)`

Update headline under `point` using `fun`.
`fun` takes an headline and returns a modified headline

```el
;; Given the following contents:
; * TODO might get done

(om-elem-update-headline-at* (point)
  (om-elem-set-property :todo-keyword "DONE" it))
 ;; Output these buffer contents
 ;; $> "* DONE might get done"

```

#### om-elem-update-subtree-at `(point fun)`

Update subtree under `point` using `fun`.
`fun` takes an subtree and returns a modified subtree

```el
;; Given the following contents:
; * one
; ** two
; ** three

(om-elem-update-subtree-at* (point)
  (om-elem-headline-indent-subheadline 1 it))
 ;; Output these buffer contents
 ;; $> "* one
 ;      ** two
 ;      *** three"

```


### Misc

#### om-elem-fold-contents `(node)`

Fold the contents of `node` if they exist.

```el
no examples :(
```

#### om-elem-unfold-contents `(node)`

Unfold the contents of `node` if they exist.

```el
no examples :(
```


<!-- 0.0.1 -->

# Acknowledgements

- [@magnars](https://github.com/magnars):
[dash.el](https://github.com/magnars/dash.el) and
[s.el](https://github.com/magnars/s.el)
- Devin Townsend: Puppetry
