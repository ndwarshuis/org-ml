;;; org-ml.el --- Functional Org Mode API -*- lexical-binding: t; -*-

;; Copyright (C) 2020  Nathan Dwarshuis

;; Author: Nathan Dwarshuis <ndwar@yavin4.ch>
;; Keywords: org-mode, outlines
;; Homepage: https://github.com/ndwarshuis/org-ml
;; Package-Requires: ((emacs "27.1") (org "9.7") (dash "2.17") (s "1.12"))
;; Version: 6.0.0

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; This is a functional API for org-mode primarily using the `org-element'
;; library. `org-element.el' provides the means for converting an org buffer to
;; a parse-tree data structure. This library contains functions to modify this
;; parse-tree in a more-or-less 'purely' functional manner (with the exception
;; of parsing from the buffer and writing back to the buffer). For the purpose
;; of this package, the resulting parse tree is composed of 'nodes'.

;; This library exposes the following types of functions:
;; - builder: build new nodes to be inserted into a parse tree
;; - property functions: return either property values (get) or nodes with
;;   modified properties (set and map)
;; - children functions: return either children of nodes (get) or return a node
;;   with modified children (set and map)
;; - node predicates: return t if node meets a condition
;; - pattern matching: return nodes based on a pattern that matches the parse
;;   tree (and perform operations on those nodes depending on the function)
;; - parsers: parse a buffer (optionally at current point) and return a parse
;;   tree
;; - writers: insert/update the contents of a buffer given a parse tree

;; For examples please see full documentation at:
;; https://github.com/ndwarshuis/org-ml

;;; Code:

(require 'org-element)
(require 'dash)
(require 's)
(require 'inline)

(eval-when-compile
  (require 'org-ml-macs))

;;; NODE TYPE SETS

;; When only considering types, nodes can be arranged in the following
;; sets (where nested sets are mutually exclusive)

;; +---------------------------------------------------+
;; | nodes                                             |
;; |                                                   |
;; | +-----------------------------------------------+ |
;; | | element nodes                                 | |
;; | | (`org-element-all-elements' + 'org-data')     | |
;; | |                                               | |
;; | | +-------------------------------------------+ | |
;; | | | leaf nodes                                | | |
;; | | +-------------------------------------------+ | |
;; | |                                               | |
;; | | +-------------------------------------------+ | |
;; | | | branch nodes                              | | |
;; | | |                                           | | |
;; | | | +---------------------------------------+ | | |
;; | | | | permitting child element nodes        | | | |
;; | | | | (aka "greater elements")              | | | |
;; | | | | (`org-element-greater-elements'       | | | |
;; | | | | + 'org-data)                          | | | |
;; | | | +---------------------------------------+ | | |
;; | | |                                           | | |
;; | | | +---------------------------------------+ | | |
;; | | | | permitting child object nodes         | | | |
;; | | | | (`org-element-object-containers' -    | | | |
;; | | | | `org-element-recursive-objects')      | | | |
;; | | | +---------------------------------------+ | | |
;; | | +-------------------------------------------+ | |
;; | +-----------------------------------------------+ |
;; |                                                   |
;; | +-----------------------------------------------+ |
;; | | object nodes                                  | |
;; | | (`org-element-all-objects' + 'plain-text')    | |
;; | |                                               | |
;; | | +-------------------------------------------+ | |
;; | | | leaf nodes                                | | |
;; | | +-------------------------------------------+ | |
;; | |                                               | |
;; | | +-------------------------------------------+ | |
;; | | | branch nodes permitting child object      | | |
;; | | | nodes (aka "recursive objects")           | | |
;; | | | (`org-element-recursive-objects')         | | |
;; | | +-------------------------------------------+ | |
;; | +-----------------------------------------------+ |
;; +---------------------------------------------------+

;; In `org-element.el' the types 'plain-text' and 'org-data' are
;; not mentioned but are required here to make the sets complete.
;; 'plain-text' is consider a leaf node of class object and 'org-data'
;; is considered a branch node of class element that is permitted to
;; hold other element nodes as children

(org-ml--defconst org-ml-elements
  (cons 'org-data org-element-all-elements)
  "List of all element types including `org-data'.")

(org-ml--defconst org-ml-objects
  (cons 'plain-text org-element-all-objects)
  "List of all object types including `plain-text'.")

(eval-and-compile
  (defconst org-ml-nodes
    (append org-ml-elements org-ml-objects)
    "List of all node types."))

(org-ml--defvaralias 'org-ml-branch-nodes-permitting-child-objects
  'org-element-object-containers
  "List of node types that can have objects as children.
These are also known as \"object containers\" in `org-element.el'")

(org-ml--defconst org-ml-branch-elements-permitting-child-objects
  (-intersection org-ml-branch-nodes-permitting-child-objects org-ml-elements)
  "List of element types that can have objects as children.")

(org-ml--defconst org-ml-branch-elements-permitting-child-elements
  (cons 'org-data org-element-greater-elements)
  "List of element types that can have elements as children.
These are also known as \"greater elements\" in `org-element.el'")

(org-ml--defconst org-ml-branch-elements
  (append org-ml-branch-elements-permitting-child-objects
          org-ml-branch-elements-permitting-child-elements)
  "List of element types that can have children.")

(org-ml--defvaralias 'org-ml-branch-objects
  'org-element-recursive-objects
  "List of object types that can have objects as children.
These are also known as \"recursive objects\" in `org-element.el'")

(org-ml--defconst org-ml-branch-nodes
  (append org-ml-branch-elements org-ml-branch-objects)
  "List of node types that can have children.")

;;; BRANCH NODE CHILD TYPE RESTRICTIONS

;; `org-element.el' specifies which object nodes may be children of other object
;; nodes but does not have the same thing for element nodes; implement here

(org-ml--defconst org-ml--object-restrictions
  (->>
   org-element-object-restrictions
   ;; remove non-object nodes
   (--remove (memq (car it) '(inlinetask item headline keyword)))
   ;; add plain-text type to everything except table-row
   (--map-when (not (eq (car it) 'table-row)) (-snoc it 'plain-text)))
  "Alist of object node type restrictions for object branch nodes.
The types in the cdr of each entry may be children of the type held at
the car.")

(org-ml--defconst org-ml--element-restrictions
  ;; TODO add inlinetask
  ;; this includes all elements except those that are restricted
  ;; (see comments below)
  (let ((standard '(babel-call center-block clock comment
                               comment-block diary-sexp drawer
                               dynamic-block example-block
                               export-block fixed-width
                               footnote-definition horizontal-rule
                               keyword latex-environment paragraph
                               plain-list planning property-drawer
                               quote-block special-block src-block
                               table verse-block)))
    `((center-block ,@(remove 'center-block standard))
      (drawer ,@(remove 'drawer standard))
      (dynamic-block ,@(remove 'dynamic-block standard))
      (footnote-definition ,@(remove 'footnote-definition standard))
      ;; headlines can only have headlines and sections
      (headline headline section)
      (item ,@standard)
      ;; plain-lists can only have items
      (plain-list item)
      ;; property-drawers can only have node-properties
      (property-drawer node-property)
      (quote-block ,@(remove 'quote-block standard))
      (section ,@standard)
      (special-block ,@standard)
      ;; tables can only have table-rows
      (table table-row)
      (org-data headline section)))
  "Alist of element node type restrictions for element branch nodes.
The types in the cdr of each entry may be children of the type held at
the car.")

(defconst org-ml--node-restrictions
  (eval-when-compile
    (append org-ml--element-restrictions org-ml--object-restrictions))
  "Alist of all restrictions for containers.")

(defconst org-ml--item-tag-restrictions
  (eval-when-compile
    (->> org-element-object-restrictions
         (alist-get 'item)
         (cons 'plain-text)))
  "List of node types which may be used in item node tag properties.")

(defconst org-ml--headline-title-restrictions
  (eval-when-compile
    (->> org-element-object-restrictions
         (alist-get 'headline)
         (cons 'plain-text)))
  "List of node types which may be used in item headline title properties.")

;;; CUSTOM

(defcustom org-ml-memoize-match-patterns nil
  "Memoize patterns in `org-ml-match' and friends.

These functions all take a PATTERN parameter that is used to
generate a lambda function, which is then used to computationally
search for the desired matches. Generating these lambda forms has
some overhead (and will increase with increasing pattern
complexity). Therefore, this value can be used to memoize (cache)
each unique lambda form for each pattern. When enabled, calls to
any of the match function using a unique pattern will generate
the corresponding lambda form only once, and then subsequent
calls will retrieve the form from the cache. This can increase
performance if relatively few patterns are used relative to the
calls made to pattern-consuming functions.

The following values are understood:
- nil: do not memoize anything
- `compiled': memoize byte-compiled lambda forms
- any other non-nil: memoize non-compiled lambda forms"
  :type 'boolean
  :group 'org-ml)

(defcustom org-ml-memoize-builders nil
  "Memoize `org-ml-build-*' functions.

These functions are pure and thus can be easily memoized.

One may wish to do this if one needs to create many nodes that
are the same, as node creation is relatively expensive. These
functions are also used internally by other parts of `org-ml',
thus memoizing these functions can achieve significant speed
increases in many scenerios. The downside is each unique node
will be stored, which takes space.

This variable globally controls memoization for these functions.
To control memoization on a per-type basis, see
`org-ml-memoize-builder-types'."
  :type 'boolean
  :group 'org-ml)

(org-ml--defconst org-ml-builder-types
   (append org-element-all-objects org-element-all-elements))

(defcustom org-ml-memoize-builder-types
  (-difference org-ml-builder-types org-ml-branch-nodes)
  "Specify the types for `org-ml-memoize-builders'."
  :type `(set ,@(--map (list 'const it) org-ml-builder-types))
  :group 'org-ml)

(defcustom org-ml-memoize-shorthand-builders nil
  "Memoize `org-ml-build-*!' functions.

Like the `org-ml-build-*' functions (no exclamation point), these
functions are pure and thus can be easily memoized.

One may wish to do this if one needs to create many nodes that
are the same, as node creation is relatively expensive. These
functions are also used internally by other parts of `org-ml',
thus memoizing these functions can achieve significant speed
increases in many scenerios. The downside is each unique node
will be stored, which takes space.

This variable globally controls memoization for these functions.
To control memoization on a per-type basis, see
`org-ml-memoize-shorthand-builder-types'."
  :type 'boolean
  :group 'org-ml)

(org-ml--defconst org-ml-shorthand-builder-types
  '(timestamp clock planning headline paragraph secondary-string item
              property-drawer table-cell table-row table))

(defcustom org-ml-memoize-shorthand-builder-types
  (-difference org-ml-shorthand-builder-types '(headline item))
  "Specify the types for `org-ml-memoize-shorthand-builders'.

Note that these don't perfectly correspond to `org-ml-nodes' since
some of these functions are composite node builders.

All in `org-ml-shorthand-builder-types' are enabled by default except for
`headline' and `item' since these take child nodes are arguments
which therefore lead to large key sizes."
  :type `(set ,@(--map (list 'const it) org-ml-shorthand-builder-types))
  :group 'org-ml)

(defcustom org-ml-use-impure nil
  "Run functions in impure mode.

For now this means that no function will make a copy of a node,
so all changes will be via side effect."
  :type 'boolean
  :group 'org-ml)

(defcustom org-ml-disable-checks nil
  "Run functions without checking nodes for proper types."
  :type 'boolean
  :group 'org-ml)

;;; AFFILIATED KEYWORD NODES

(org-ml--defconst org-ml--element-nodes-with-affiliated
  (eval-when-compile
    (-difference org-ml-elements
                 '(org-data comment clock headline inlinetask item
                            node-property planning property-drawer
                            section table-row))))

;;; LIST OPERATIONS (EXTENDING DASH.el)

(defun org-ml--pad-or-truncate (length pad list)
  "Return padded or truncated list starting from LIST.

If length of LIST is greater than LENGTH, truncate LIST to LENGTH
and return. If LIST is longer than LENGTH, add PAD to the end
of LIST until it's length equals LENGTH and return. Do nothing if
length of LIST is equal to LENGTH initially."
  (let ((blanks (- length (length list))))
    (if (< blanks 0) (-take length list)
      (append list (-repeat blanks pad)))))

;;; plist operations

(defun org-ml--plist-get-keys (plist)
  "Get the keys for PLIST."
  (-slice plist 0 nil 2))

(defun org-ml--plist-get-vals (plist)
  "Get the values for PLIST."
  (-slice plist 1 nil 2))

(defun org-ml--plist-map-values (fun plist)
  "Map FUN over the values in PLIST.
FUN is a unary function that returns a modified value."
  (nreverse
   (org-ml--reduce2-from* (cons (funcall fun it) (cons it-key acc))
     nil plist)))

(defun org-ml--is-plist (x)
  "Return t if X is a plist."
  (declare (pure t) (side-effect-free t))
  (when (listp x)
    (let ((is-plist t))
      (while (and is-plist (cdr x))
        (setq is-plist (keywordp (car x))
              x (cdr (cdr x))))
      (and (not x) is-plist))))

(defun org-ml--plist-remove (key plist)
  "Return PLIST with KEY and its value removed."
  (nreverse
   (org-ml--reduce2-from* (if (eq key it-key) acc (cons it (cons it-key acc)))
     nil plist)))

;;; inter-index operations

;; The "inter-index" alludes to the fact that these list operations
;; use an index value that refers to spaces between list members.
;; These functions are enhanced versions of what is provided in
;; `dash.el' and native emacs that handle negative indices and have
;; switches to handle out of bounds errors

(defun org-ml--convert-inter-index (n list &optional use-oor)
  "Return absolute index given N and LIST.

N is relative index where positions in LIST are given by the following:
- 0: before first member
- 1: before second member (and so on)
- -1: after last member
- -2: after penultimate member (and so on)

The absolute index to be returned will be N mapped to a positive
integer that refers to the same space in LIST.

If USE-OOR (use out-of-range) is t, return the closest valid index
if N refers to a location that is outside LIST. Otherwise throw an
error."
  (let* ((N (length list))
         (upper N)
         (lower (- (- N) 1)))
    (cond
     ((<= 0 n upper) n)
     ((>= -1 n lower) (+ 1 N n))
     ((and use-oor (< upper n)) upper)
     ((and use-oor (< n lower)) lower)
     (t (org-ml--arg-error
         "Index (%s) out of range; must be between %s and %s"
         n lower upper)))))

(defun org-ml--insert-at (n x list &optional use-oor)
  "Like `-insert-at' but can insert X at negative indices N in LIST.
See `org-ml--convert-inter-index' for the meaning of N and USE-OOR."
  (-insert-at (org-ml--convert-inter-index n list use-oor) x list))

(defun org-ml--split-at (n list &optional use-oor)
  "Like `-split-at' except allow negative indices in LIST.
See `org-ml--convert-inter-index' for the meaning of N and USE-OOR."
  (let ((n* (org-ml--convert-inter-index n list use-oor)))
    (when list
      (-split-at n* list))))

(defun org-ml--splice-at (n list* list &optional use-oor)
  "Return LIST with LIST* spliced at index N.
See `org-ml--convert-inter-index' for the meaning of N and USE-OOR."
  (--> (-map #'list list)
       (org-ml--insert-at n list* it use-oor)
       (apply #'append it)))

;;; intra-index operations

;; The "intra-index" alludes to the fact that these list operations
;; use an index value that refers to explicit list members.
;; These functions are enhanced versions of what is provided in
;; `dash.el' and native emacs that handle negative indices and have
;; switches to handle out of bounds errors

(defun org-ml--convert-intra-index (n list &optional use-oor)
  "Return absolute index given N and LIST.

N is relative index where positions in LIST are given by the following:
- 0: first member
- 1: second member (and so on)
- -1: last member
- -2: penultimate member (and so on)

The absolute index to be returned is N mapped to a positive integer
that refers to the same member in LIST.

If USE-OOR (use out-of-range) is non-nil, return the closest valid
index if N refers to a position outside of LIST.

In cases where LIST is nil, N is meaningless since it will never
refer to anything. In this case, return nil if USE-OOR is
`permit-empty', and throw an error otherwise (even if USE-OOR it
non-nil)."
  (let* ((N (length list))
         (upper (1- N))
         (lower (- N)))
    (cond
     ((= N 0) (unless (eq use-oor 'permit-empty)
                (error "List is empty, index is meaningless")))
     ((<= 0 n upper) n)
     ((>= -1 n lower) (+ N n))
     ((and use-oor (< upper n)) upper)
     ((and use-oor (< n lower)) 0)
     (t (org-ml--arg-error
         "Index (%s) out of range; must be between %s and %s"
         n lower upper)))))

(defun org-ml--remove-at (n list &optional use-oor)
  "Like `-remove-at' but honors negative indices N in LIST.
See `org-ml--convert-intra-index' for the meaning of N and USE-OOR."
  (-some-> (org-ml--convert-intra-index n list use-oor)
           (-remove-at list)))

(defun org-ml--replace-at (n x list &optional use-oor)
  "Like `-replace-at' but can substitute X at negative indices N in LIST.
See `org-ml--convert-intra-index' for the meaning of N and USE-OOR."
  (-some-> (org-ml--convert-intra-index n list use-oor)
           (-replace-at x list)))

(defun org-ml--nth (n list &optional use-oor)
  "Like `nth' but honors negative indices N in LIST.
See `org-ml--convert-intra-index' for the meaning of N and USE-OOR."
  (-some-> (org-ml--convert-intra-index n list use-oor)
           (nth list)))

;;; INTERNAL TYPE FUNCTIONS

(define-error 'arg-type-error "Argument type error")

(defun org-ml--arg-error (string &rest args)
  "Signal an `arg-type-error'.
STRING and ARGS are analogous to `error'."
  (signal 'arg-type-error `(,(apply #'format-message string args))))

(defun org-ml--is-any-type (types node)
  "Return t if the type of NODE is in TYPES (a list of symbols)."
  (declare (pure t) (side-effect-free t))
  (if (memq (org-ml-get-type node) types) t))

(defun org-ml--is-node (list)
  "Return t if LIST is a node."
  (declare (pure t) (side-effect-free t))
  (org-ml--is-any-type org-ml-nodes list))

(defun org-ml--is-type (type node)
  "Return t if the type of NODE is TYPE (a symbol)."
  (declare (pure t) (side-effect-free t))
  (eq (org-ml-get-type node) type))

(defun org-ml--is-table-row (node)
  "Return t if NODE is a standard table-row node."
  (declare (pure t) (side-effect-free t))
  (and (org-ml--is-type 'table-row node)
       (org-ml--property-is-eq :type 'standard node)))

(defun org-ml--filter-type (type node)
  "Return NODE if it is TYPE or nil otherwise."
  (declare (pure t) (side-effect-free t))
  (and (org-ml--is-type type node) node))

(defun org-ml--filter-types (types node)
  "Return NODE if it is one of TYPES or nil otherwise."
  (declare (pure t) (side-effect-free t))
  (and (org-ml-is-any-type types node) node))

(defun org-ml--is-secondary-string (list)
  "Return t if LIST is a secondary string."
  (declare (pure t) (side-effect-free t))
  (--none? (org-ml--is-any-type org-ml-elements it) list))

(defun org-ml--check-type (type node)
  "Check that NODE is TYPE; throw error if not."
  (unless org-ml-disable-checks
    (let ((y (org-ml-get-type node)))
      (unless (equal y type)
        (org-ml--arg-error "Node must be a %s, got a %s" type y)))))

(defun org-ml--check-types (types node)
  "Check that NODE is one of TYPES; throw error if not."
  (unless org-ml-disable-checks
    (let ((y (org-ml-get-type node)))
      (unless (memq y types)
        (org-ml--arg-error "Node must be one of %s, got a %s" types y)))))

;;; MISC HELPER FUNCTIONS

(defun org-ml--get-head (node)
  "Return the type and properties cells of NODE."
  (declare (pure t) (side-effect-free t))
  (if (stringp node) node (list (car node) (cadr node))))

(defun org-ml--from-string (string)
  "Convert STRING to org-element representation."
  (with-temp-buffer
    (insert string)
    (-> (org-ml-parse-this-buffer) (org-element-contents) (car))))

(define-inline org-ml-copy (node &optional keep)
  "Copy NODE if running in pure mode.

KEEP is passed to `org-element-copy'."
  (inline-letevals (node)
    (inline-quote
     (if org-ml-use-impure ,node (org-element-copy ,node ,keep)))))

(defmacro org-ml-wrap-impure (&rest body)
  "Run BODY in impure mode."
  `(let ((org-ml-use-impure t))
     ,@body))

(defmacro org-ml-wrap-check (&rest body)
  "Run BODY without node type checking."
  `(let ((org-ml-disable-checks t))
     ,@body))

(defmacro org-ml-> (&rest forms)
  "Thread FORMS using `->' and run in impure mode."
  (declare (indent 1))
  `(org-ml-wrap-impure (-> ,@forms)))

(defmacro org-ml->> (&rest forms)
  "Thread FORMS using `->>' and run in impure mode."
  (declare (indent 1))
  `(org-ml-wrap-impure (->> ,@forms)))

(defmacro org-ml--> (&rest forms)
  "Thread FORMS using `-->' and run in impure mode."
  (declare (indent 1))
  `(org-ml-wrap-impure (--> ,@forms)))

(defmacro org-ml-some-> (&rest forms)
  "Thread FORMS using `-some->' and run in impure mode."
  (declare (indent 1))
  `(org-ml-wrap-impure (-some-> ,@forms)))

(defmacro org-ml-some->> (&rest forms)
  "Thread FORMS using `-some->>' and run in impure mode."
  (declare (indent 1))
  `(org-ml-wrap-impure (-some->> ,@forms)))

(defmacro org-ml-some--> (&rest forms)
  "Thread FORMS using `-some-->' and run in impure mode."
  (declare (indent 1))
  `(org-ml-wrap-impure (-some--> ,@forms)))

(defmacro org-ml-as-> (&rest forms)
  "Thread FORMS using `-some-->' and run in impure mode."
  (declare (indent 1))
  `(org-ml-wrap-impure (-some--> ,@forms)))

;;; INTERNAL PREDICATES

(defun org-ml--is-oneline-string (x)
  "Return t if X is a string with no newlines."
  (declare (pure t))
  (and (stringp x) (not (s-contains? "\n" x))))

(defun org-ml--is-oneline-string-or-nil (x)
  "Return t if X is a string with no newlines or nil."
  (declare (pure t))
  (or (null x) (org-ml--is-oneline-string x)))

(defun org-ml--is-non-neg-integer (x)
  "Return t if X is a non-negative integer."
  (declare (pure t))
  (and (integerp x) (<= 0 x)))

(defun org-ml--is-non-neg-integer-or-nil (x)
  "Return t if X is a non-negative integer or nil."
  (declare (pure t))
  (or (null x) (org-ml--is-non-neg-integer x)))

(defun org-ml--is-pos-integer (x)
  "Return t if X is a positive integer."
  (declare (pure t))
  (and (integerp x) (< 0 x)))

(defun org-ml--is-pos-integer-or-nil (x)
  "Return t if X is a positive integer or nil."
  (declare (pure t))
  (or (null x) (org-ml--is-pos-integer x)))

(defun org-ml--is-string-list (x)
  "Return t if X is a list of strings without newlines or nil."
  (declare (pure t))
  (or (null x) (and (listp x) (-all? #'org-ml--is-oneline-string x))))

;;; INTERNAL NODE PROPERTY FUNCTIONS

(defun org-ml--get-nonstandard-properties (node)
  "Return the non-standard properties list of NODE."
  (if (stringp node) (text-properties-at 0 node) (cddr (nth 1 node))))

(defun org-ml-get-all-properties (node)
  "Return the properties list of NODE."
  (if (stringp node) (text-properties-at 0 node)
    (let ((arr-props (->> org-element--standard-properties
                          (--map (list it (org-element-property it node)))
                          (-flatten-n 1)))
          (plist-props (->> (nth 1 node)
                            (cddr)
                            (org-ml--plist-get-keys)
                            (--map (list it (org-element-property it node)))
                            (-flatten-n 1))))
      (append arr-props plist-props))))

(define-inline org-ml--get-post-blank-text (plain-text)
  "Return number of trailing spaces in PLAIN-TEXT."
  (inline-quote
   (length (car (s-match "[ ]*$" ,plain-text)))))

(define-inline org-ml--get-post-blank-textsafe (node)
  "Return number of trailing spaces in PLAIN-TEXT."
  (inline-letevals (node)
    (inline-quote
     (if (stringp ,node) (org-ml--get-post-blank-text ,node)
       (org-element-post-blank ,node)))))

(define-inline org-ml--set-post-blank (post-blank node)
  (inline-quote (org-element-put-property-2 :post-blank ,post-blank ,node)))

(define-inline org-ml--set-last-post-blank (pb nodes)
  "Set post-blank of last of NODES by PB."
  (inline-quote (org-ml--map-last* (org-ml--set-post-blank ,pb it) ,nodes)))

(defmacro org-ml--set-properties-raw (node &rest plist)
  "Set all properties in NODE to the values corresponding to PLIST.
PLIST is a list of property-value pairs that correspond to the
property list in NODE.

This is not meant for plain-text."
  (declare (indent 1))
  (let* ((n (make-symbol "it-node"))
         (forms (->> (-partition 2 plist)
                     (--map `(org-element-put-property ,n ,(car it) ,(cadr it))))))
    `(let ((,n ,node))
       ,@forms
       ,n)))

(eval-when-compile
  (defmacro org-ml--map-property-raw* (prop form node)
    "Return NODE with FUN applied to the value in PROP.
FUN is a form that returns a modified value and contains `it'
bound to the property value."
    (declare (indent 1))
    (let ((node* (make-symbol "node")))
      `(let ((,node* ,node))
         (let ((it (org-element-property-raw ,prop ,node*)))
           (org-element-put-property-2 ,prop ,form ,node*))))))

(define-inline org-ml--shift-post-blank (n node)
  "ADD N spaces to the end of NODE.

This will not work if NODE is a string."
  (inline-quote
   (org-ml--map-property-raw* :post-blank (+ it ,n) ,node)))

(define-inline org-ml--shift-last-post-blank (pb nodes)
  "Shift post-blank of last of NODES by PB."
  (inline-quote (org-ml--map-last* (org-ml--shift-post-blank ,pb it) ,nodes)))

(defun org-ml--shift-post-blank-textsafe (n node)
  "ADD N spaces to the end of NODE.

This will work if NODE is a string."
  (if (stringp node)
      (let ((pb (org-ml--get-post-blank-text node)))
        (concat (substring node (- pb)) (make-string (+ pb n) ?\ )))
    (org-ml--shift-post-blank n node)))

(define-inline org-ml--property-is-eq (prop val node)
  "Return t if PROP in NODE is `eq' to VAL."
  (inline-quote (eq ,val (org-element-property-raw ,prop ,node))))

;;; NODE PROPERTY TRANSLATION AND CHECKING FRAMEWORK

;; This code provides the internal framework for the following
;; operations where NODE is any node, PROP is a property of NODE,
;; and VALUE is the value of PROP:

;; Get: f(PROP NODE) -> VALUE
;; Set: f(PROP VALUE NODE) -> NODE'
;; Map: f(PROP FUN NODE) -> NODE' where FUN is a function that
;;      modifies the value of PROP in NODE and is like:
;;      f(VALUE) -> VALUE'

;; Get -> 'read', Set -> 'write', Map -> 'read/write'

;; `org-element.el' doesn't always store values as their native types (like some
;; strings look like plists converted to strings). Here, we make a distinction
;; between VALUE and its internal representation IVALUE (which is actually the
;; value stored in the node list and understood by `org-element.el'), where
;; VALUE may not always be `equal' to IVALUE. When performing any of the
;; operations above, this framework will transparently translate between VALUE
;; and IVALUE (using so called encoders and decodes). Furthermore, the VALUE for
;; any PROP must conform to a 'type' which is enforced by this framework.

;; The center of this framework is the constant
;; `org-ml--property-alist' which holds the relationship of all node
;; types and their properties, type checkers, and encoders/decoders.
;; This alist has the following structure:

;; - car of each member is the type of NODE
;; - cdr of each member is the property alist for the node type
;;   - the car of the property alist is the keyword for PROP
;;   - the cdr of the property alist is an attribute plist, and the
;;     keys of this plist include:
;;     - :pred - a predicate function that returns t if VALUE is the correct
;;       type for PROP
;;     - :type-desc - a string describing the data type for PROP
;;     - :encode - a unary function that converts VALUE to IVALUE; if not given
;;       this is the identity function
;;     - :decode - a function that inverts the function at :encode, if not given
;;       this is the identity function
;;     - :cis - a unary function that takes NODE and returns a modified NODE;
;;       the point of this it to "update" other properties when PROP is changed
;;     - :const - a value that PROP should always have
;;     - :shift - a binary function that shifts PROP; the first argument takes
;;       an integer describing the magnitude and direction of the shift and the
;;       second argument is VALUE for PROP; return a new VALUE; this only makes
;;       sense the type of PROP is an integer
;;     - :require - a boolean telling if PROP is required to be specified when
;;       creating a NODE of this type
;;     - :string-list - a boolean telling if the type of PROP is a list of
;;       strings
;;     - :plist - a boolean telling if the type of PROP is a plist
;;     - :toggle - a boolean telling if the type of PROP is a boolean

;; In terms of property attributes, the three property operations can be
;; described by the following pseudo code:

;; get: GET(PROP VALUE) -> NODE
;; 1) DECODE(IGET(PROP, NODE)) -> VALUE where IGET retrieves the IVALUE of PROP
;;    from NODE

;; set: SET(PROP VALUE NODE) -> NODE
;; 1) if PRED(VALUE) -> t, proceed to 2), else throw error
;; 2) ISET(PROP, ENCODE(VALUE), NODE)) -> NODE' where ISET sets the PROP of NODE
;;    to IVALUE
;; 3) If CIS is non-nil, run CIS(NODE') -> NODE'', else return NODE'

;; map: MAP(PROP FUN NODE) -> NODE'
;; 1) GET(PROP NODE) -> VALUE
;; 2) FUN(VALUE) -> VALUE'
;; 3) if PRED(VALUE') -> t proceed to 4), else throw error
;; 4) SET(PROP VALUE' NODE) -> NODE'

;; Thus GET only requires that the property exist in the type (which may be
;; nil, in which case GET returns nil). The decoder doesn't need to be present
;; as the identity function will be used if it isn't present.
;;
;; SET requires that the :pred attribute exists, since it needs to check that
;; the incoming value to assign is valid. If :encoder or :cis are unspecified
;; then these will be identity.
;;
;; MAP obviously requires both.

;;; property value predicates (type specific)

(defun org-ml--is-valid-link-format (x)
  "Return t if X is an allowed value for a link node format property."
  (memq x '(nil plain angle bracket)))

(defun org-ml--is-valid-link-type (x)
  "Return t if X is an allowed value for a link node type property."
  (->> '("coderef" "custorg-ml-id" "file" "id" "radio" "fuzzy")
       (append (org-link-types))
       (member x)))

(defun org-ml--is-valid-item-checkbox (x)
  "Return t if X is an allowed value for an item node checkbox property."
  (memq x '(nil on off trans)))

(defun org-ml--is-valid-item-tag (x)
  "Return t if X is an allowed value for an item node tag property."
  (and (listp x)
       (--all? (org-ml--is-any-type org-ml--item-tag-restrictions it) x)))

(defun org-ml--is-valid-item-bullet (x)
  "Return t if X is an allowed value for a item node bullet property."
  ;; NOTE org mode 9.1.9 has the following limitations:
  ;; - "+" will be converted to "-" when interpreted
  ;; - "1)" will be converted to "1." when interpreted
  ;; - alphanumeric symbols make the interpreter crash
  (pcase x ((or '- (pred integerp)) t)))

(defun org-ml--is-valid-clock-timestamp (x)
  "Return t if X is an allowed value for a clock node value property."
  (and (org-ml--is-type 'timestamp x)
       (memq (org-element-property-raw :type x) '(inactive inactive-range))
       (not (org-element-property-raw :repeater-type x))))

(defun org-ml--is-valid-planning-unclosed-timestamp (x)
  "Return t if X is an allowed value for a planning node timestamp property."
  (or (null x) (and (org-ml--is-type 'timestamp x)
                    (org-ml--property-is-eq :type 'active x))))

(defun org-ml--is-valid-planning-closed-timestamp (x)
  "Return t if X is an allowed value for a planning node timestamp property."
  (or (null x) (and (org-ml--is-type 'timestamp x)
                    (org-ml--property-is-eq :type 'inactive x))))

(defun org-ml--is-valid-entity-name (x)
  "Return t if X is an allowed value for an entity node name property."
  (org-entity-get x))

(defun org-ml--is-valid-headline-tags (x)
  "Return t if X is an allowed value for a headline node tags property."
  (and (listp x)
       (-all? #'org-ml--is-oneline-string x)
       (not (member org-archive-tag x))))

(defun org-ml--is-valid-headline-priority (x)
  "Return t if X is an allowed value for a headline node priority property."
  (or (null x) (and (integerp x)
                    (>= org-lowest-priority x org-highest-priority))))

(defun org-ml--is-valid-headline-title (x)
  "Return t if X is an allowed value for a headline node title property."
  (and
   (listp x)
   (--all? (org-ml--is-any-type org-ml--headline-title-restrictions it) x)))

(defun org-ml--is-valid-timestamp-type (x)
  "Return t if X is an allowed value for a timestamp node type property."
  (memq x '(inactive inactive-range active active-range)))

(defun org-ml--is-valid-timestamp-range-type (x)
  "Return t if X is an allowed value for a timestamp node range-type property."
  (memq x '(nil daterange timerange)))

(defun org-ml--is-valid-timestamp-repeater-type (x)
  "Return t if X is an allowed value for a timestamp node repeater-type property."
  (memq x '(nil catch-up restart cumulate)))

(defun org-ml--is-valid-timestamp-warning-type (x)
  "Return t if X is an allowed value for a timestamp node warning-type property."
  (memq x '(nil all first)))

(defun org-ml--is-valid-timestamp-unit (x)
  "Return t if X is an allowed value for a timestamp node unit property."
  (memq x '(nil year month week day hour)))

(defun org-ml--is-valid-latex-environment-value (x)
  "Return t if X is an allowed value for a latex-environment node value property."
  (pcase x
    ((or `(,(pred org-ml--is-oneline-string))
         `(,(pred org-ml--is-oneline-string) ,(pred stringp)))
     t)))

(defun org-ml--is-valid-statistics-cookie-value (x)
  "Return t if X is an allowed value for a statistics-cookie node value property."
  (pcase x
    ((or `(nil) `(nil nil)) t)
    (`(,(and (pred integerp) percent))
     (<= 0 percent 100))
    (`(,(and (pred integerp) numerator)
       ,(and (pred integerp) denominator))
     (and (org-ml--is-non-neg-integer numerator)
          (org-ml--is-non-neg-integer denominator)
          (<= numerator denominator)))))

(defun org-ml--is-valid-diary-sexp-value (x)
  "Return t if X is an allowed value for a diary-sexp node value property."
  (or (null x) (listp x)))

(defun org-ml--is-valid-header (x)
  "Return t if X is an allowed value for a header affiliated keyword property."
  (and (listp x) (--all? (org-ml--is-plist it) x)))

(defun org-ml--is-valid-results (x)
  "Return t if X is an allowed value for a results affiliated keyword property."
  (pcase x
    (`nil t)
    (`(,(pred stringp) ,(pred stringp)) t)
    (`(,(pred stringp)) t)))

(defun org-ml--is-valid-caption (x)
  "Return t if X is an allowed value for a caption affiliated keyword property."
  (and (listp x)
       (--all? (pcase it
                 ((pred stringp) t)
                 (`(,(pred stringp) ,(pred stringp)) t))
               x)))

;;; encode/decode (general)

(defun org-ml--decode-boolean (bool)
  "Return BOOL as either t or nil."
  (and bool t))

(defun org-ml--encode-string-or-nil (string)
  "Return STRING as either itself or \"\" if nil."
  (if (null string) "" string))

(defun org-ml--decode-string-or-nil (string)
  "Return STRING without text properties if not nil."
  (when string (substring-no-properties string)))

(defun org-ml--encode-string-list-delim (string-list delim)
  "Return STRING-LIST as string joined by DELIM."
  (-some->> string-list (s-join delim)))

(defun org-ml--decode-string-list-delim (string delim)
  "Return STRING as list of strings split by DELIM."
  (-some->> string (s-split delim)))

(defun org-ml--encode-string-list-space-delim (string-list)
  "Return STRING-LIST as string joined by spaces."
  (org-ml--encode-string-list-delim string-list " "))

(defun org-ml--decode-string-list-space-delim (string)
  "Return STRING as list of strings split by spaces."
  (org-ml--decode-string-list-delim string " "))

(defun org-ml--encode-string-list-comma-delim (string-list)
  "Return STRING-LIST as string joined by commas."
  (org-ml--encode-string-list-delim string-list ","))

(defun org-ml--decode-string-list-comma-delim (string)
  "Return STRING as list of strings split by commas."
  (org-ml--decode-string-list-delim string ","))

(defun org-ml--encode-plist (plist)
  "Return PLIST as string joined by spaces."
  (-some->> (--map (format "%S" it) plist) (s-join " ")))

(defun org-ml--decode-plist (string)
  "Return STRING as plist split by spaces."
  (-map #'intern (org-ml--decode-string-list-space-delim string)))

;;; encode/decode (type specific)

(defun org-ml--encode-latex-environment-value (value)
  "Return VALUE as a string representing a latex-environment.
VALUE is a list conforming to `org-ml--is-valid-latex-environment-value'."
  (-let (((env body) value))
    (if body (format "\\begin{%1$s}\n%2$s\n\\end{%1$s}" env body)
      (format "\\begin{%1$s}\n\\end{%1$s}" env))))

(defun org-ml--decode-latex-environment-value (value)
  "Return VALUE as a list representing a latex-environment.
The return value is a list conforming to
`org-ml--is-valid-latex-environment-value'."
  (let ((m (car (s-match-strings-all "\\\\begin{\\(.+\\)}\n\\(.*\\)\n?\\\\end{\\(.+\\)}" value))))
    (list (nth 1 m) (nth 2 m))))

(defun org-ml--encode-item-bullet (bullet)
  "Return BULLET as a formatted string.
BULLET must conform to `org-ml--is-valid-item-bullet'."
  ;; assume bullet conforms to pcase statement below
  (pcase bullet
    ('- "- ")
    ((pred integerp) (format "%s. " bullet))
    (_ (error "This should not happen"))))

(defun org-ml--decode-item-bullet (bullet)
  "Return BULLET as a symbol from a formatted string.
Return value will conform to `org-ml--is-valid-item-bullet'."
  ;; NOTE this must conform to the full range of item bullets since anything
  ;; could be parsed from an org file. Anything "invalid" should be converted to
  ;; it's closest "legal" bullet
  (if (s-matches? "^\\(-\\|+\\)" bullet) '-
    (let* ((case-fold-search nil)) ; need case-sensitivity
      (or (-some->> (s-match "^[0-9]+" bullet)
                    (car)
                    (string-to-number))
          ;; convert letters to numbers if they are used
          (-some->> (s-match "^[a-z]+" bullet)
                    (car)
                    (string-to-char)
                    (+ -96))
          (-some->> (s-match "^[A-Z]+" bullet)
                    (car)
                    (string-to-char)
                    (+ -64))
          (org-ml--arg-error "Invalid bullet found: %s" bullet)))))

(defun org-ml--decode-headline-tags (tags)
  "Return TAGS with `org-archive-tag' removed."
  (-map #'substring-no-properties (remove org-archive-tag tags)))

(defun org-ml--encode-statistics-cookie-value (value)
  "Return VALUE as formatted string representing the cookie.
VALUE must conform to `org-ml--is-valid-statistics-cookie-value'."
  (cl-flet
      ((mk-stat
        (v)
        (pcase v
          (`(nil) "%")
          (`(nil nil) "/")
          (`(,percent . nil)
           (format "%s%%" percent))
          (`(,numerator . (,denominator . nil))
           (format "%s/%s" numerator denominator))
          (_ (error "This should never happen")))))
    (format "[%s]" (mk-stat value))))

(defun org-ml--decode-statistics-cookie-value (value)
  "Return VALUE as a list representing the cookie.
Return value will conform to `org-ml--is-valid-statistics-cookie-value'."
  (cond
   ((equal "[%]" value) '(nil))
   ((equal "[/]" value) '(nil nil))
   (t
    (->>
     (or (s-match-strings-all "\\[\\([0-9]+\\)/\\([0-9]+\\)\\]" value)
         (s-match-strings-all "\\[\\([0-9]+\\)%\\]" value)
         (org-ml--arg-error "Invalid stats-cookie: %s" value))
     (cdar)
     (-map #'string-to-number)))))

(defun org-ml--encode-diary-sexp-value (value)
  "Return VALUE as a string.
VALUE must conform to `org-ml--is-valid-diary-sexp-value'."
  (if value (format "%%%%%S" value) "%%()"))

(defun org-ml--decode-diary-sexp-value (value)
  "Return VALUE as a form.
Return value will conform to `org-ml--is-valid-diary-sexp-value'."
  (->> (s-chop-prefix "%%" value) (read)))

(defun org-ml--encode-header (plists)
  "Return PLISTS as a list of strings."
  (--map
   (-some->> it
     (-partition 2)
     (--map (format "%S %s" (car it) (cadr it)))
     (s-join " "))
   plists))

(defun org-ml--decode-header (headers)
  "Return HEADERS (a list of strings) as a list of plists."
  (--map
   (->> (org-ml--decode-string-list-space-delim it)
        (--map-indexed (if (cl-evenp it-index) (intern it) it)))
   headers))

(defun org-ml--encode-results (results)
  "Return a encoded results affiliated keyword value.
RESULTS should conform to `org-ml--is-valid-caption'."
  (-let (((hash source) results))
    (when source `(,source . ,hash))))

(defun org-ml--decode-results (internal-results)
  "Return a decoded results affiliated keyword value.
The returned list will conform to `org-ml--is-valid-caption' given
INTERNAL-RESULTS stored in a node."
  (-let (((source . hash) internal-results))
    (if hash (list hash source) source)))

(defun org-ml--encode-caption (caption)
  "Return a encoded caption affiliated keyword value.
CAPTION should conform to `org-ml--is-valid-caption'."
  (->> (reverse caption)
       (--reduce-from
        (pcase it
          ((and (pred stringp) long)
           (cons `((,long)) acc))
          (`(,short ,long)
           (when long
             (cons (if short `((,long) ,short) `((,long))) acc))))
        nil)
       (-non-nil)))

(defun org-ml--decode-caption (internal-caption)
  "Return a decoded caption affiliated keyword value.
The returned list will conform to `org-ml--is-valid-caption' given
INTERNAL-CAPTION stored in a node."
  (--reduce-from
   (-let ((((long) short) it))
     (-> (if short (list (substring-no-properties short)
                         (substring-no-properties long))
           (substring-no-properties long))
         (cons acc)))
   nil (reverse internal-caption)))

;;; cis functions

(defun org-ml--update-macro-value (macro)
  "Return MACRO node with its value property updated.
This will be based on MACRO's key and value properties."
  (let* ((k (org-element-property-raw :key macro))
         (as (org-element-property-raw :args macro))
         (v (if as (format "%s(%s)" k (s-join "," as)) k)))
    (org-element-put-property-2 :value (format "{{{%s}}}" v) macro)))

(defun org-ml--update-clock-duration-and-status (clock)
  "Return CLOCK node with its duration and status properties updated.
This will be based on CLOCK's value property."
  (let* ((ts (org-element-property-raw :value clock))
         (seconds (org-ml--timestamp-get-length ts)))
    (if (= seconds 0)
        (org-ml--set-properties-raw clock
          :duration nil
          :status 'running)
      (let* ((h (-> seconds (/ 3600) (floor)))
             (m (-> seconds (- (* h 3600)) (/ 60) (floor))))
        (org-ml--set-properties-raw clock
          :duration (format "%2d:%02d" h m)
          :status 'closed
          ;; if the clock is going from non-ranged to ranged, it may not be in
          ;; collapsed form; ensure it is not in collapsed form
          :value (org-ml--timestamp-set-collapsed nil ts))))))

(defun org-ml--update-headline-tags (headline)
  "Return HEADLINE node with its tags updated.
This will be based on HEADLINE's archivedp property."
  (org-ml--map-property-raw* :tags
    (let ((tags* (remove org-archive-tag it)))
      (if (org-element-property :archivedp headline)
          (-snoc tags* org-archive-tag)
        tags*))
    (org-element-properties-resolve headline)))

(defun org-ml--link-update-type-explicit (link)
  "Return LINK with `:type-explicit-p' updated."
  (let ((x (-> (org-element-property-raw :type link)
               (member (org-link-types))
               (null)
               (not))))
    (org-element-put-property-2 :type-explicit-p x link)))

;;; shifters

(defun org-ml--shift-pos-integer (n x)
  "Return X shifted by N (both are integers).
If the value to return is less than 1, return 1."
  (when x
    (let ((x* (+ x n)))
      (if (< 0 x*) x* 1))))

(defun org-ml--shift-non-neg-integer (n x)
  "Return X shifted by N (both are integers).
If the value to return is less than 0, return 0."
  (when x
    (let ((x* (+ x n)))
      (if (<= 0 x*) x* 0))))

(defun org-ml--shift-headline-priority (n priority)
  "Return PRIORITY shifted by N (an integer).
If the final value is outside the bounds of `org-highest-priority'
and `org-lowest-priority', return as if cycling and wrapping
between the priority bounds until the return value is inside the
bounds."
  (when priority
    (let ((diff (1+ (- org-lowest-priority org-highest-priority)))
          (offset (- priority org-highest-priority)))
      (-> (- offset n)
          (mod diff)
          (- offset)
          (+ priority)))))

;;; property alist

(org-ml--defconst org-ml--property-alist
  (let* ((bool (list :pred #'booleanp
                     :decode 'org-ml--decode-boolean
                     :type-desc "nil or t"
                     :toggle t))
         (pos-int (list :pred #'org-ml--is-pos-integer
                        :type-desc "a positive integer"))
         (pos-int-nil (list :pred #'org-ml--is-pos-integer-or-nil
                            :type-desc "a positive integer or nil"))
         (nn-int (list :pred #'org-ml--is-non-neg-integer
                       :type-desc "a non-negative integer"))
         (nn-int-nil (list :pred #'org-ml--is-non-neg-integer-or-nil
                           :type-desc "a non-negative integer or nil"))
         (str (list :pred #'stringp
                    :type-desc "a string"))
         (str-nil (list :pred #'string-or-null-p
                        :type-desc "a string or nil"))
         (ol-str (list :pred #'org-ml--is-oneline-string
                       :type-desc "a oneline string"))
         (ol-str-nil (list :pred #'org-ml--is-oneline-string-or-nil
                           :type-desc "a oneline string or nil"))
         (plist (list :encode 'org-ml--encode-plist
                      :pred #'org-ml--is-plist
                      :decode 'org-ml--decode-plist
                      :plist t
                      :type-desc "a plist"))
         (slist (list :pred #'org-ml--is-string-list
                      :string-list t
                      :type-desc "a list of oneline strings"))
         (slist-com (list :encode 'org-ml--encode-string-list-comma-delim
                          :decode 'org-ml--decode-string-list-comma-delim
                          :pred #'org-ml--is-string-list
                          :string-list t
                          :type-desc "a list of oneline strings"))
         (slist-spc (list :encode 'org-ml--encode-string-list-space-delim
                          :decode 'org-ml--decode-string-list-space-delim
                          :pred #'org-ml--is-string-list
                          :string-list t
                          :type-desc "a list of oneline strings"))
         (planning-unclosed (list :pred #'org-ml--is-valid-planning-unclosed-timestamp
                                  :type-desc "a zero-range, active timestamp node"))
         (planning-closed (list :pred #'org-ml--is-valid-planning-closed-timestamp
                                :type-desc "a zero-range, inactive timestamp node"))
         (ts-unit (list :pred #'org-ml--is-valid-timestamp-unit
                        :type-desc '("nil or a symbol from `year' `month'"
                                     "`week' `day', or `hour'"))))
    (->>
     `((babel-call (:call ,@ol-str :require t)
                   (:inside-header ,@plist)
                   (:arguments ,@slist-com)
                   (:end-header ,@plist)
                   (:value))
       (bold)
       (center-block)
       (clock (:value :pred org-ml--is-valid-clock-timestamp
                      :cis org-ml--update-clock-duration-and-status
                      :type-desc ("a ranged or unranged inactive timestamp"
                                  "node with no warning or repeater")
                      :require t)
              (:status)
              (:duration))
       (code (:value ,@str :require t))
       (comment (:value ,@str :require t))
       (comment-block (:value ,@str :decode s-trim-right :require ""))
       (drawer (:drawer-name ,@ol-str :require t))
       (diary-sexp (:value :encode org-ml--encode-diary-sexp-value
                           :pred org-ml--is-valid-diary-sexp-value
                           :decode org-ml--decode-diary-sexp-value
                           :type-desc "a list form or nil"))
       (dynamic-block (:arguments ,@plist)
                      (:block-name ,@ol-str :require t))
       (entity (:name :pred org-ml--is-valid-entity-name
                      :type-desc "a string that makes `org-entity-get' return non-nil"
                      :require t)
               (:use-brackets-p ,@bool)
               (:latex)
               (:latex-math-p)
               (:html)
               (:ascii)
               (:latin1)
               (:utf-8))
       (example-block (:preserve-indent ,@bool)
                      (:switches ,@slist-spc)
                      (:value ,@str :require "" :decode s-trim-right)
                      ;; TODO some of these are tied to switches, it
                      ;; may be good to set them directly
                      (:number-lines)
                      (:retain-labels)
                      (:use-labels)
                      (:label-fmt))
       (export-block (:type ,@ol-str :require t)
                     (:value ,@str :require t))
       (export-snippet (:back-end ,@ol-str :require t)
                       (:value ,@str :require t))
       (fixed-width (:value ,@ol-str :decode s-trim-right :require t))
       (footnote-definition (:label ,@ol-str :require t)
                            (:pre-blank ,@nn-int
                                        :shift org-ml--shift-non-neg-integer
                                        :require 0))
       (footnote-reference (:label ,@ol-str-nil)
                           (:type))
       (headline (:archivedp ,@bool :cis org-ml--update-headline-tags)
                 (:commentedp ,@bool)
                 (:footnote-section-p ,@bool)
                 (:level ,@pos-int
                         :shift org-ml--shift-pos-integer
                         :require 1)
                 (:pre-blank ,@nn-int
                             :shift org-ml--shift-non-neg-integer
                             :require 0)
                 ;; ,@robust
                 (:priority :pred org-ml--is-valid-headline-priority
                            :shift org-ml--shift-headline-priority
                            :type-desc ("an integer between (inclusive)"
                                        "`org-highest-priority' and"
                                        "`org-lowest-priority'"))
                 (:tags :pred org-ml--is-valid-headline-tags
                        :decode org-ml--decode-headline-tags
                        :cis org-ml--update-headline-tags
                        :type-desc "a string list"
                        :string-list t)
                 (:title :pred org-ml--is-valid-headline-title
                         :type-desc "a secondary string")
                 (:todo-keyword ,@ol-str-nil
                                :decode org-ml--decode-string-or-nil)
                 (:raw-value)
                 (:todo-type))
       (horizontal-rule)
       (inline-babel-call (:call ,@ol-str :require t)
                          (:inside-header ,@plist)
                          (:arguments ,@slist-com)
                          (:end-header ,@plist)
                          (:value))
       (inline-src-block (:language ,@ol-str :require t)
                         (:parameters ,@plist)
                         (:value ,@str :require ""))
       ;; (inlinetask)
       (italic)
       (item (:bullet :encode org-ml--encode-item-bullet
                      :pred org-ml--is-valid-item-bullet
                      :decode org-ml--decode-item-bullet
                      :type-desc ("a positive integer (ordered)"
                                  "or the symbol `-' (unordered)")
                      :require '-)
             (:pre-blank ,@nn-int
                         :shift org-ml--shift-non-neg-integer
                         :require 0)
             (:checkbox :pred org-ml--is-valid-item-checkbox
                        :type-desc "nil or the symbols `on', `off', or `trans'")
             (:counter ,@pos-int-nil :shift org-ml--shift-pos-integer)
             (:tag :pred org-ml--is-valid-item-tag
                   :type-desc "a secondary string"))
             ;; (:structure))
       (keyword (:key ,@ol-str :require t)
                (:value ,@ol-str :require t))
       (latex-environment (:value :encode org-ml--encode-latex-environment-value
                                  :pred org-ml--is-valid-latex-environment-value
                                  :decode org-ml--decode-latex-environment-value
                                  :type-desc "a list of strings like (ENV BODY) or (ENV)"
                                  :require t))
       (latex-fragment (:value ,@str :require t))
       (line-break)
       (link (:path ,@ol-str :require t)
             (:format :pred org-ml--is-valid-link-format
                      :type-desc "the symbol `plain', `bracket' or `angle'")
             (:type :pred org-ml--is-valid-link-type
                    :cis org-ml--link-update-type-explicit
                    :type-desc ("a oneline string from `org-link-types'"
                                "or \"coderef\", \"custorg-ml-id\","
                                "\"file\", \"id\", \"radio\", or"
                                "\"fuzzy\"")
                    ;; TODO is fuzzy a good default?
                    :require "fuzzy")
             (:raw-link)
             (:application)
             (:search-option))
       (macro (:args ,@slist :cis org-ml--update-macro-value)
              (:key ,@ol-str :cis org-ml--update-macro-value :require t)
              (:value))
       (node-property (:key ,@ol-str :require t)
                      (:value ,@ol-str :require t))
       (paragraph)
       (plain-list ;;(:structure)
                   (:type))
       (plain-text)
       (planning (:closed ,@planning-closed)
                 (:deadline ,@planning-unclosed)
                 (:scheduled ,@planning-unclosed))
       (property-drawer)
       (quote-block)
       ;; TODO this should not have multiline strings in it
       (radio-target (:value))
       (section)
       (special-block (:type ,@ol-str :require t) (:parameters ,@ol-str-nil))
       (src-block (:value ,@str :decode s-trim-right :require "")
                  (:language ,@str-nil)
                  (:parameters ,@plist)
                  (:preserve-indent ,@bool)
                  (:switches ,@slist-spc)
                  (:number-lines)
                  (:retain-labels)
                  (:use-labels)
                  (:label-fmt))
       (statistics-cookie (:value
                           :encode org-ml--encode-statistics-cookie-value
                           :pred org-ml--is-valid-statistics-cookie-value
                           :decode org-ml--decode-statistics-cookie-value
                           :type-desc ("a list of non-neg integers"
                                       "like (PERC) or (NUM DEN)"
                                       "which make [NUM/DEN] and"
                                       "[PERC%] respectively")
                           :require t))
       (strike-through)
       ;; TODO these should only allow multiline strings if bracketed
       (subscript (:use-brackets-p ,@bool))
       (superscript (:use-brackets-p ,@bool))
       (table (:tblfm ,@slist)
              (:type :const 'org)
              (:value))
       ;; TODO this should not have multiline strings in it
       (table-cell)
       (table-row (:type :const 'standard))
       (target (:value ,@ol-str :require t))
       (timestamp (:type :pred org-ml--is-valid-timestamp-type
                         :type-desc ("a symbol from `inactive',"
                                     "`active', `inactive-range', or"
                                     "`active-range'")
                         :require t)
                  (:range-type :pred org-ml--is-valid-timestamp-range-type
                               :type-desc ("either symbol `daterange' or"
                                           "`timerange' or nil"))
                  (:year-start ,@pos-int :require t)
                  (:month-start ,@pos-int :require t)
                  (:day-start ,@pos-int :require t)
                  (:year-end ,@pos-int :require t)
                  (:month-end ,@pos-int :require t)
                  (:day-end ,@pos-int :require t)
                  (:hour-start ,@nn-int-nil)
                  (:minute-start ,@nn-int-nil)
                  (:hour-end ,@nn-int-nil)
                  (:minute-end ,@nn-int-nil)
                  (:repeater-type :pred org-ml--is-valid-timestamp-repeater-type
                                  :type-desc ("nil or a symbol from"
                                              "`catch-up', `restart',"
                                              "or `cumulate'"))
                  (:repeater-unit ,@ts-unit)
                  (:repeater-value ,@pos-int-nil)
                  (:repeater-deadline-unit ,@ts-unit)
                  (:repeater-deadline-value ,@pos-int-nil)
                  (:warning-type :pred org-ml--is-valid-timestamp-warning-type
                                 :type-desc ("nil or a symbol from"
                                             "`all' or `first'"))
                  (:warning-unit ,@ts-unit)
                  (:warning-value ,@pos-int-nil)
                  (:raw-value))
       (underline)
       (verbatim (:value ,@str :require t))
       (verse-block))
     (--map-when (memq (car it) org-ml--element-nodes-with-affiliated)
                 (append it
                         `((:name ,@str-nil)
                           (:plot ,@str-nil)
                           (:header :encode org-ml--encode-header
                                    :pred org-ml--is-valid-header
                                    :decode org-ml--decode-header
                                    :type-desc ("a list of plists where all"
                                                "plist values are strings"))
                           (:results :encode org-ml--encode-results
                                     :pred org-ml--is-valid-results
                                     :decode org-ml--decode-results
                                     :type-desc ("a list like (SOURCE) or"
                                                 "(HASH SOURCE) where HASH"
                                                 "and SOURCE are strings."))
                           (:caption :encode org-ml--encode-caption
                                     :pred org-ml--is-valid-caption
                                     :decode org-ml--decode-caption
                                     :type-desc ("a list including (LONG) or"
                                                 "(SHORT LONG) where SHORT and"
                                                 "LONG are both strings representing"
                                                 "the short and long captions"))))))))

;;; node property operations

;; alist functions

(eval-when-compile
  (defun org-ml--flatten-attribute (attr)
    (->> org-ml--property-alist
         (--map (cons (car it)
                      (->> (cdr it)
                           (--map (cons (car it) (plist-get (cdr it) attr)))
                           (-filter #'cdr))))
         (-filter #'cdr)))

  (defun org-ml--flatten-attribute-boolean (attr)
    (->> org-ml--property-alist
         (--map (cons (car it)
                      (->> (cdr it)
                           (--filter (plist-get (cdr it) attr))
                           (-map #'car))))
         (-filter #'cdr))))

(org-ml--defconst org-ml--property-decoder-functions
  (--map (cons (car it)
               (--map (cons (car it) (plist-get (cdr it) :decode))
                      (cdr it)))
         org-ml--property-alist))

(org-ml--defconst org-ml--property-encoder-functions
  (org-ml--flatten-attribute :encode))

(org-ml--defconst org-ml--property-predicate-functions
  (org-ml--flatten-attribute :pred))

(org-ml--defconst org-ml--property-shifter-functions
  (org-ml--flatten-attribute :shift))

(org-ml--defconst org-ml--property-updater-functions
  (org-ml--flatten-attribute :cis))

(org-ml--defconst org-ml--property-type-descriptions
  (cl-flet
      ((map-cdr
         (cell f)
         (cons (car cell) (funcall f (cdr cell)))))
    (->> (org-ml--flatten-attribute :type-desc)
         (--map (map-cdr it (lambda (props)
                              (--map (map-cdr it
                                              (lambda (desc)
                                                (if (listp desc)
                                                    (s-join " " desc)
                                                  desc)))
                                     props)))))))

(org-ml--defconst org-ml--properties-with-toggle
  (org-ml--flatten-attribute-boolean :toggle))

(org-ml--defconst org-ml--properties-with-string-list
  (org-ml--flatten-attribute-boolean :string-list))

(org-ml--defconst org-ml--properties-with-plist
  (org-ml--flatten-attribute-boolean :plist))

(defun org-ml--get-property-encoder (type prop)
  "Return the encoder for PROP of node TYPE."
  (->> (alist-get type org-ml--property-encoder-functions)
       (alist-get prop)))

(defun org-ml--get-property-decoder (type prop)
  "Return the decoder function for PROP of node TYPE.
If TYPE does not exist, return error. If PROP does not exist for
TYPE, also return error. If type does exist, return the decoder
function or nil if there is none."
    (-if-let (ps (alist-get type org-ml--property-decoder-functions))
        (-if-let (f (assq prop ps))
            (cdr f)
          (org-ml--arg-error "Type '%s' does not have property '%s'" type prop))
      (org-ml--arg-error "Tried to query '%s' for non-existent '%s'" prop type)))

(defun org-ml--get-property-updater (type prop)
  "Return the updater for PROP of node TYPE."
  (->> (alist-get type org-ml--property-updater-functions)
       (alist-get prop)))

(defun org-ml--get-property-type-desc (type prop)
  "Return the description for PROP of node TYPE."
  (->> (alist-get type org-ml--property-type-descriptions)
       (alist-get prop)))

(defun org-ml--get-property-shifter (type prop)
  "Lookup shifter function for TYPE and PROP."
  (if (eq prop :post-blank) #'org-ml--shift-non-neg-integer
    (->> (alist-get type org-ml--property-shifter-functions)
         (alist-get prop))))

(defun org-ml--get-property-predicate (type prop)
  "Lookup shifter function for TYPE and PROP."
  (if (eq prop :post-blank) #'org-ml--is-non-neg-integer
    (->> (alist-get type org-ml--property-predicate-functions)
         (alist-get prop))))

(defun org-ml--property-memq (alist type prop)
  "Return t if PROP is in the cdr of TYPE in ALIST."
  (memq prop (alist-get type alist)))

(defun org-ml--property-error-unsettable (prop type)
  "Throw error signifying that PROP is unsettable of node TYPE."
  (org-ml--arg-error "Property '%s' is unsettable for type '%s'" prop type))

(defun org-ml--property-error-wrong-type (prop type value)
  "Throw error signifying that VALUE is wrong for PROP of node TYPE."
  (let ((msg "Property '%s' in node of type '%s' must be %s. Got '%S'")
        (correct-type (org-ml--get-property-type-desc type prop)))
    (org-ml--arg-error msg prop type correct-type value)))

(defun org-ml--property-encode (prop value type)
  "Given TYPE and PROP, return encoded VALUE."
  (-if-let (pred (org-ml--get-property-predicate type prop))
      (if (funcall pred value)
          (-if-let (encode-fun (org-ml--get-property-encoder type prop))
              (funcall encode-fun value)
            value)
        (org-ml--property-error-wrong-type prop type value))
    (org-ml--property-error-unsettable prop type)))

;;; INTERNAL BRANCH/CHILD MANIPULATION

(defun org-ml--get-descendent (indices node)
  "Return the nested children of NODE as given by INDICES.
INDICES is a list of integers specifying the index and level of the
nested element to return."
  (if (not indices) node
    (->> (org-element-contents node)
         (nth (car indices))
         (org-ml--get-descendent (cdr indices)))))

(defun org-ml--set-children-nocheck (children node)
  "Return NODE with children set to CHILDREN."
   (let ((head (org-ml--get-head node)))
     (if children (append head children) head)))

(eval-when-compile
  (defmacro org-ml--map-children-nocheck* (form node)
    "Return NODE with FORM applied to its children.

FORM is a form with `it' bound to the list of children and
returns a modified list of children."
    (declare (debug (form form)))
    (let ((n (make-symbol "--node")))
    `(let* ((,n ,node)
            (it (org-element-contents ,n)))
       (org-ml--set-children-nocheck ,form ,n)))))

(defun org-ml--set-children-throw-error (type child-types illegal)
  "Throw an `arg-type-error' for TYPE.
In the message specify that allowed child types are CHILD-TYPES
and ILLEGAL types were attempted to be set."
  (cl-flet
      ((format-types
        (type-list)
        (->> type-list (-map #'symbol-name) (s-join ", "))))
    (let ((fmt (->> '("Setting illegal child types for node type '%s'"
                      "illegal types found: %s"
                      "allowed types are: %s")
                    (s-join "; ")))
          (illegal (format-types illegal))
          (child-types (format-types child-types)))
      (org-ml--arg-error fmt type illegal child-types))))

;;; BASE BUILDER FUNCTIONS

;;; build helpers

(eval-and-compile
  (defun org-ml--build-bare-node (type post-blank props children)
    "Return new node of TYPE with POST-BLANK, PROPS and CHILDREN.
TYPE is a symbol and POST-BLANK is a positive integer."
    ;; don't set children in the function itself a) so I can check for valid
    ;; types and b) because `org-element-create' will add :parent
    (let ((node (org-element-create type `(:post-blank ,(or post-blank 0) ,@props))))
      ;; Use this function here so we get child type checks
      (if children (org-ml-set-children children node) node))))

(defmacro org-ml--build-blank-node (type post-blank)
  "Return new node of TYPE with POST-BLANK and all properties set to nil."
  (let ((ips (->> (alist-get type org-ml--property-alist)
                  (-map #'car)
                  (--mapcat (list it nil))
                  (cons 'list))))
    `(org-ml--build-bare-node ',type ,post-blank ,ips nil)))

;;; base builders

;; define all base builders using this automated monstrosity

(defmacro org-ml--with-cache (table switch valid type key body)
  "Run BODY with a memoization cache.

TABLE is a symbol with an alist indexed by TYPE. SWITCH is a
symbol which will be dynamically read at runtime to determine if
the cache should be used. VALID is a symbol bound to a list of
valid types (at runtime) which should use the cache. KEY is the
lookup key to be used in the cache (which is actually a hash
table) and is assumed to correspond to the inputs to BODY.

If KEY is not in the cache, run BODY and put the result in the
cache under KEY. If KEY is in the cache, return whatever that is."
  (let* ((k (make-symbol "--key"))
         (n (make-symbol "--node"))
         (c (make-symbol "--cached"))
         (h (alist-get type (eval table))))
    (unless h
      (error "Failed to get cache table for %s" type))
    `(let* ((,k ,key)
            (,c (and ,switch (memq ',type ,valid) (gethash ,k ,h))))
       (if ,c (org-ml-copy ,c t)
         ;; turn off memoizer internally since some shorthand builders
         ;; call other shorthand builders and caching each layer is probably
         ;; overkill since none of these functions have that many arguments
         ;; to vary
         (let ((,switch nil))
           (let ((,n ,body))
             (puthash ,k (org-ml-copy ,n t) ,h)
             ,n))))))

(eval-and-compile
  (defvar org-ml--builder-cache
    (--map (cons it (make-hash-table :test #'equal)) org-ml-builder-types)
    "Alist of hash tables to store builder results."))

(defun org-ml-clear-builder-cache ()
  "Clear the memoization cache for node builders."
  (interactive)
  (--each org-ml--builder-cache
    (clrhash (cdr it))))

(defmacro org-ml--with-builder-cache (type key body)
  "Run BODY with builder cache.
See org-ml--with-cache' for meaning of TYPE and KEY."
  (declare (indent 1))
  `(org-ml--with-cache
     org-ml--builder-cache
     org-ml-memoize-builders
     org-ml-memoize-builder-types
     ,type ,key ,body))

(eval-when-compile
  (defun org-ml--autodef-kwd-to-sym (keyword)
    "Return KEYWORD as a string with no leading colon."
    (->> (symbol-name keyword) (s-chop-prefix ":") (intern)))

  (defun org-ml--autodef-prepend-article (string)
    "Return STRING starting with \"a\" or \"an\" depending on first word."
    (let ((a (--> (symbol-name string)
                  (s-left 1 it)
                  (if (member it '("a" "e" "i" "o" "u")) "an" "a"))))
      (format "%s %s" a string)))

  (defun org-ml--autodef-categorize-prop (prop)
    "Return category for PROP."
    (-let (((&plist :require :pred :const) (cdr prop)))
      (cond
       (const 'const)
       ((not pred) 'null)
       ((eq require t) 'req)
       (t 'key))))

  (defun org-ml--autodef-prop-form (len fun-0 fun-n props)
    "Return form to set properties to PROPS.
If list PROPS is length LEN, use FUN-0, otherwise FUN-N."
    (declare (indent 1))
    (if (= len (length props)) `(,fun-0 ,@props) `(,fun-n (list ,@props))))

  (defun org-ml--indent-doc (s)
    (with-temp-buffer
      (insert s)
      (fill-paragraph)
      (buffer-string)))

  (defun org-ml--autodef-make-docstring (type rest-arg props)
    "Return docstring for PROPS.
TYPE is the type of the node in question and REST-ARG is the
symbol for the rest argument."
    (let ((class (if (memq type org-element-all-elements) "element" "object"))
          (end (if (not rest-arg) "."
                 (->> (symbol-name rest-arg)
                      (s-upcase)
                      (format " with %s as children."))))
          ;; (post-blank (if element? "newlines" "spaces"))
          (prop
           (-some->>
            (append (alist-get 'req props) (alist-get 'key props))
            (--map (let ((p (->> (car it)
                                 (symbol-name)
                                 (s-chop-prefix ":")
                                 (s-upcase)))
                         (r (-->
                             (plist-get (cdr it) :require)
                             (pcase it
                               ((pred stringp)
                                (format "(default %S)" it))
                               (`(quote ,s)
                                (format "(default `%s')" s))
                               ((guard (eq it t))
                                "(required)")
                               (_ ""))))
                         (d (plist-get (cdr it) :type-desc)))
                     (unless d
                       (error "No type-desc: %s %s" type p))
                     (->> (if (listp d) (s-join " " d) d)
                          (format "- %s: %s %s" p r)
                          (org-ml--indent-doc))))
            (s-join "\n"))))
      (concat
       (format "Build %s %s node" (org-ml--autodef-prepend-article type) class)
       end
       "\n\nThe following properties are settable:\n"
       prop "\n- POST-BLANK: a non-negative integer")))

  (defun org-ml--autodef-build-node-form (entry)
    "Return defun form for ENTRY."
    (let* ((type (car entry))
           (name (intern (format "org-ml-build-%s" type)))
           (props (->> (cdr entry)
                       (--remove (eq :post-blank (car it)))
                       (-non-nil)
                       (-group-by #'org-ml--autodef-categorize-prop)))
           (pos-args (->> (alist-get 'req props)
                          (--map (org-ml--autodef-kwd-to-sym (car it)))))
           (kw-args (->> (alist-get 'key props)
                         (--map (let ((prop (org-ml--autodef-kwd-to-sym (car it)))
                                      (default (plist-get (cdr it) :require)))
                                  (if default `(,prop ,default) prop)))))
           (rest-arg (cond
                      ((memq type org-element-greater-elements) 'element-nodes)
                      ((memq type org-element-object-containers) 'object-nodes)))
           (args (let ((a `(,@pos-args &key ,@kw-args post-blank)))
                   (if rest-arg `(,@a &rest ,rest-arg) a)))
           (const-props (->> (alist-get 'const props)
                             (--mapcat (list (car it)
                                             (plist-get (cdr it) :const)))))
           (nil-props (->> (alist-get 'null props)
                           (-map #'car)
                           (--mapcat (list it nil))))
           (strict-props
            (->> (append (alist-get 'key props)
                         (alist-get 'req props))
                 (-map #'car)
                 (--mapcat (list it
                                 `(org-ml--property-encode
                                   ,it ,(org-ml--autodef-kwd-to-sym it) ',type)))))
           (all-props (-some->> (append strict-props nil-props const-props)
                       (cons 'list)))
           (updaters (->> (alist-get type org-ml--property-updater-functions)
                          (-map #'cdr)
                          (-uniq)))
           (doc (org-ml--autodef-make-docstring type rest-arg props))
           (inner-body `(org-ml--build-bare-node
                         ',type post-blank
                         ,all-props
                         ,rest-arg))
           (prop-syms (->> (alist-get 'key props)
                           (append (alist-get 'req props))
                           (-map #'car)
                           (-map #'org-ml--autodef-kwd-to-sym)
                           (cons 'post-blank)))
           (memoizer-key
            (cond
             ((and rest-arg (not prop-syms)) rest-arg)
             ((and rest-arg (= (length prop-syms) 1)) `(cons ,@prop-syms ,rest-arg))
             (rest-arg `(append (list ,@prop-syms) ,rest-arg))
             ((not prop-syms) nil)
             ((= (length prop-syms) 1) (car prop-syms))
             (t `(list ,@prop-syms))))
           (body (if updaters
                     (let ((us (--map `(funcall #',it node) updaters)))
                       `(let ((node ,inner-body))
                          ,@us))
                   inner-body))
           (memoized-body
            (if memoizer-key
                `(org-ml--with-builder-cache ,type ,memoizer-key ,body)
              body)))
      (macroexpand `(org-ml--defun-kw ,name ,args ,doc ,memoized-body))))

  (defmacro org-ml--autodef-build-node-functions ()
    "Define all build node functions."
    (let ((forms (--> (--remove (eq 'plain-text (car it)) org-ml--property-alist)
                      (--map (org-ml--autodef-build-node-form it) it))))
      `(progn ,@forms))))

(org-ml--autodef-build-node-functions)

;; INTERNAL TYPE-SPECIFIC PROPERTY FUNCTIONS

;;; object nodes
;;
;; statistics-cookie

(defun org-ml--statistics-cookie-get-format (statistics-cookie)
  "Return format of STATISTICS-COOKIE as a symbol.
If fractional cookie, return `fraction'; if percentage cookie return
`percent', else throw error (which should never happen)."
  (let ((value (org-element-property-raw :value statistics-cookie)))
    (cond ((s-contains? "/" value) 'fraction)
          ((s-contains? "%" value) 'percent)
          (t (org-ml--arg-error "Unparsable statistics cookie: %s" value)))))

;; timestamp (auxiliary functions)

;; terminology (in haskell types)
;; type Date = (Y, M, D)
;; type Time = (H, M)
;; type DateTime = (Y, M, D, H, M)
;; type TimeList = (Y, M, D, (Maybe H), (Maybe M))

(defun org-ml-is-time-p (time)
  "Return t if TIME is a list like (hour min)."
  (pcase time (`(,(pred integerp) ,(pred integerp)) t)))

(defun org-ml-timelist-has-time (timelist)
  "Return t if TIMELIST has a time."
  (pcase timelist
    (`(,(pred integerp) ,(pred integerp) ,(pred integerp)
       ,(pred integerp) ,(pred integerp))
     t)))

;; make these public, not sure where else to put them
(defun org-ml-timelist-to-unixtime (timelist)
  "Return the unix time (integer seconds) of TIMELIST.
The returned value is dependent on the time zone of the operating
system."
  (->> (-let (((y m d H M) timelist))
         (list 0 (or M 0) (or H 0) d m y nil -1 (current-time-zone)))
       (encode-time)
       (float-time)
       (round)))

(defun org-ml-unixtime-to-timelist (has-time unixtime)
  "Return the long time list of UNIXTIME.

The list will be formatted like (YEAR MONTH DAY HOUR MIN) unless
HAS-TIME is nil, in which case HOUR and MIN will be set to nil."
  (-let (((M H d m y) (-slice (decode-time unixtime (current-time-zone)) 1 6)))
    (if has-time (list y m d H M) (list y m d nil nil))))

(defun org-ml-unixtime-to-datetime (unixtime)
  "Return the long time list of UNIXTIME.
The list will be formatted like (YEAR MONTH DAY HOUR MIN)."
  (reverse (-slice (decode-time unixtime (current-time-zone)) 1 6)))

(defun org-ml-unixtime-to-date (unixtime)
  "Return the short time list of UNIXTIME.
The list will be formatted like (YEAR MONTH DAY nil nil)."
  (append (-take 3 (org-ml-unixtime-to-datetime unixtime)) '(nil nil)))

(defun org-ml--timelist-truncate (timelist)
  "Return the date of TIMELIST with hour amd minute fields nil-ed."
  `(,@(-take 3 timelist) nil nil))

(defun org-ml-timelist-split (timelist)
  "Return TIMELIST split into ((Y M D) (H M)).

The second member will be nil if either hours or minutes is nil."
  (-let (((ymd (h m)) (-split-at 3 timelist)))
    (list ymd (if (and h m) (list h m) nil))))

(defun org-ml-times-equal-date-p (time1 time2)
  "Return t if the dates of TIME1 and TIME2 are the same."
  (equal (org-ml--timelist-truncate time1) (org-ml--timelist-truncate time2)))

(defun org-ml--timelists-get-range-type (timelist1 timelist2 original)
  "Return range type of TIMELIST1 and TIMELIST2.

Valid return values are nil (unranged), `daterange' (ranged with
different dates), or `timerange' (ranged with same date).

ORIGINAL can be any of the return values above. If TIME1 and TIME2 are
a timerange as defined above, return ORIGINAL if it is non-nil."
  (-let (((d1 t1) (org-ml-timelist-split timelist1))
         ((d2 t2) (org-ml-timelist-split timelist2)))
    (if (equal d1 d2)
        (if (equal t1 t2) nil (or original 'timerange))
      'daterange)))

;; ASSUME any "impossible datetimes" will be corrected when the timelist is
;; parsed back into a timestamp (or however it will be used). Ie if I add 10000
;; days to any timestamp assume this will be reflected sensibly in the month
;; and year of the final result downstream.
(defun org-ml-timelist-shift (n unit timelist)
  "Return modified TIMELIST shifted N UNIT's.

UNIT is one of `day', `week', `month', `year', `minute', or `hour'.
N is an integer."
  (-let (((i s) (cond
                 ((eq unit 'year) `(0 ,n))
                 ((eq unit 'month) `(1 ,n))
                 ((eq unit 'week) `(2 ,(* 7 n)))
                 ((eq unit 'day) `(2 ,n))
                 ((and (eq unit 'hour) (org-ml-timelist-has-time timelist)) `(3 ,n))
                 ((and (eq unit 'minute) (org-ml-timelist-has-time timelist)) `(4 ,n))
                 (t (org-ml--arg-error "Invalid time unit: %S" unit)))))
    (org-ml--map-at* i (+ s it) timelist)))

(defun org-ml--time-shift (n unit time)
  "Return modified TIME shifted N UNITs (modulo).

UNIT is `minute', or `hour'. N is an integer."
  (-let* ((f (pcase unit
               (`hour 60)
               (`minute 1)
               (_ (org-ml--arg-error "Invalid time unit: %S" unit))))
          ((H M) time)
          (s (+ (* n f) (* H 60) M)))
    (list (mod (/ s 60) 24) (mod s 60))))

;; timestamp (regular)

;; ASSUME the source of truth for if a timestamp is ranged or not is in the
;; :ranged-type property. This is much faster than querying each piece of the
;; timestamp and inferring if it is ranged or not. It also is less ambiguous for
;; in cases where the timestamp may be collapsed.

(defun org-ml--check-time (H M)
  "Check time, decomposed into H and M."
  (unless (and (integerp H) (integerp M))
    (org-ml--arg-error "Invalid time %s" (list H M))))

(defun org-ml--check-time-from-list (time)
  "Check TIME."
  (if (consp time)
      (-let (((H M) time))
        (org-ml--check-time H M))
    (org-ml--arg-error "Time must not be nil")))

(defun org-ml--check-timelist (y m d H M)
  "Check timelist, decomposed into Y M D H and M."
  (unless (and (integerp y)
               (integerp m)
               (integerp d)
               (or (not H) (integerp H))
               (or (not M) (integerp M)))
    (org-ml--arg-error "Invalid timelist %s" (list y m d H M))))

(defun org-ml--check-timelist-from-list (timelist)
  "Check TIMELIST."
  (if (consp timelist)
      (-let (((y m d H M) timelist))
        (org-ml--check-timelist y m d H M))
    (org-ml--arg-error "Timelist must not be nil")))

(defun org-ml--check-warning (type value unit)
  "Check that warning (TYPE VALUE UNIT) is valid."
  (unless (and (org-ml--is-valid-timestamp-warning-type type)
               (integerp value)
               (org-ml--is-valid-timestamp-unit unit))
    (org-ml--arg-error "Invalid warning %s" (list type value unit))))

(defun org-ml--check-repeater (type value unit)
  "Check that repeater (TYPE VALUE UNIT) is valid."
  (unless (and (org-ml--is-valid-timestamp-repeater-type type)
               (integerp value)
               (org-ml--is-valid-timestamp-unit unit))
    (org-ml--arg-error "Invalid repeater %s" (list type value unit))))

(defun org-ml--check-deadline (value unit)
  "Check that deadline (VALUE UNIT) is valid."
  (unless (and (integerp value) (org-ml--is-valid-timestamp-unit unit))
    (org-ml--arg-error "Invalid deadline %s" (list value unit))))

(defun org-ml--check-warning-from-list (warning)
  "Check that WARNING is valid."
  (if (consp warning)
      (-let (((y v u) warning))
        (org-ml--check-warning y v u))
    (org-ml--arg-error "Warning must not be nil")))

(defun org-ml--check-repeater-from-list (repeater)
  "Check that REPEATER is valid."
  (if (consp repeater)
      (-let (((y v u) repeater))
        (org-ml--check-repeater y v u))
    (org-ml--arg-error "Repeater must not be nil")))

(defun org-ml--check-deadline-from-list (deadline)
  "Check that DEADLINE is valid."
  (if (consp deadline)
      (-let (((v u) deadline))
        (org-ml--check-deadline v u))
    (org-ml--arg-error "Repeater must not be nil")))

(defun org-ml--timestamp-get-start-timelist (timestamp)
  "Return the timelist of the start time in TIMESTAMP."
  (-let (((&plist :minute-start n :hour-start h :day-start d
                  :month-start m :year-start y)
          (org-ml--get-nonstandard-properties timestamp)))
    `(,y ,m ,d ,h ,n)))

(defun org-ml--timestamp-get-start-date (timestamp)
  "Return the start date of TIMESTAMP."
  (-let (((&plist :year-start y :month-start m :day-start d)
          (org-ml--get-nonstandard-properties timestamp)))
    `(,y ,m ,d)))

(defun org-ml--timestamp-get-start-time (timestamp)
  "Return the start time of TIMESTAMP or nil if not set."
  (-let (((&plist :minute-start m :hour-start h)
          (org-ml--get-nonstandard-properties timestamp)))
    (if (and h m) `(,h ,m) nil)))

(defun org-ml--timestamp-get-end-timelist (timestamp)
  "Return the timelist of the end time in TIMESTAMP."
  (-let (((&plist :minute-end n :hour-end h :day-end d
                  :month-end m :year-end y)
          (org-ml--get-nonstandard-properties timestamp)))
    `(,y ,m ,d ,h ,n)))

(defun org-ml--timestamp-get-end-date (timestamp)
  "Return the end date of TIMESTAMP."
  (-let (((&plist :year-end y :month-end m :day-end d)
          (org-ml--get-nonstandard-properties timestamp)))
    `(,y ,m ,d)))

(defun org-ml--timestamp-get-end-time (timestamp)
  "Return the end time of TIMESTAMP or nil if not set."
  (-let (((&plist :minute-end m :hour-end h)
          (org-ml--get-nonstandard-properties timestamp)))
    (if (and h m) `(,h ,m) nil)))

(defun org-ml--timestamp-get-start-unixtime (timestamp)
  "Return the unixtime of the start time in TIMESTAMP."
  (->> (org-ml--timestamp-get-start-timelist timestamp)
       (org-ml-timelist-to-unixtime)))

(defun org-ml--timestamp-get-end-unixtime (timestamp)
  "Return the unixtime of the end time in TIMESTAMP."
  (->> (org-ml--timestamp-get-end-timelist timestamp)
       (org-ml-timelist-to-unixtime)))

(defun org-ml--timestamp-has-equal-dates-p (timestamp)
  "Return t if start and end dates of TIMESTAMP are the same."
  (equal (org-ml--timestamp-get-start-date timestamp)
         (org-ml--timestamp-get-end-date timestamp)))

(defun org-ml--timestamp-get-length (timestamp)
  "Return the range of TIMESTAMP in seconds."
  (- (org-ml--timestamp-get-end-unixtime timestamp)
     (org-ml--timestamp-get-start-unixtime timestamp)))

(defun org-ml--timestamp-is-active (timestamp)
  "Return t if TIMESTAMP is an active type."
  (memq (org-element-property-raw :type timestamp) '(active active-range)))

(defun org-ml--timestamp-is-range-type (timestamp)
  "Return t if TIMESTAMP has a range type."
  (memq (org-element-property-raw :type timestamp)
        '(active-range inactive-range)))

(defun org-ml--timestamp-is-ranged (timestamp)
  "Return t if TIMESTAMP has a range greater than 0 seconds."
  (/= 0 (org-ml--timestamp-get-length timestamp)))

(defun org-ml--timestamp-set-start-timelist-nocheck (timelist timestamp)
  "Set the start of TIMESTAMP using TIMELIST. Does not set type."
  (-let (((y m d H M) timelist))
    (org-ml--set-properties-raw timestamp
      :year-start y
      :month-start m
      :day-start d
      :hour-start H
      :minute-start M)))

(defun org-ml--timestamp-set-start-timelist (timelist timestamp)
  "Return TIMESTAMP with start time set according to TIMELIST."
  (->> (org-ml--timestamp-set-start-timelist-nocheck timelist timestamp)
       (org-ml--timestamp-update-type-ranged)))

(defun org-ml--timestamp-set-end-timelist-nocheck (timelist timestamp)
  "Set the end of TIMESTAMP using TIMELIST. Does not set type.

Set end to start if TIMELIST is nil."
  (-let (((y m d H M)
          (or timelist (org-ml--timestamp-get-start-timelist timestamp))))
    (org-ml--set-properties-raw timestamp
      :year-end y
      :month-end m
      :day-end d
      :hour-end H
      :minute-end M)))

(defun org-ml--timestamp-set-end-timelist (timelist timestamp)
  "Return TIMESTAMP with end set according to TIMELIST."
  (->> (org-ml--timestamp-set-end-timelist-nocheck timelist timestamp)
       (org-ml--timestamp-update-type-ranged)))

(defun org-ml--timestamp-set-single-timelist (timelist timestamp)
  "Return TIMESTAMP with start/end set to TIMELIST."
  (->> (org-ml--timestamp-set-start-timelist-nocheck timelist timestamp)
       (org-ml--timestamp-set-end-timelist-nocheck timelist)
       (org-ml--timestamp-set-range-type nil)))

(defun org-ml--timestamp-set-double-timelist (timelist1 timelist2 timestamp)
  "Return TIMESTAMP with start/end set to TIMELIST1/TIMELIST2."
  (->> (org-ml--timestamp-set-start-timelist-nocheck timelist1 timestamp)
       (org-ml--timestamp-set-end-timelist-nocheck timelist2)
       (org-ml--timestamp-update-type-ranged)))

(defun org-ml--timestamp-set-type-ranged (is-ranged timestamp)
  "Return TIMESTAMP with `:type' set according to IS-RANGED."
  (org-ml--map-property-raw* :type
    (pcase it
      ((or `active `active-range)
       (if is-ranged 'active-range 'active))
      ((or `inactive `inactive-range)
       (if is-ranged 'inactive-range 'inactive))
      (e (org-ml--arg-error "Invalid timestamp type: %s" e)))
    timestamp))

(defun org-ml--timestamp-set-range-type (range-type timestamp)
  "Return TIMESTAMP updated to reflect RANGE-TYPE.
Specifically update `:range-type' and `:type'."
  (->> (org-ml--timestamp-set-type-ranged range-type timestamp)
       (org-element-put-property-2 :range-type range-type)))

(defun org-ml--timestamp-set-length (n unit timestamp)
  "Return TIMESTAMP with end time shifted to N UNITs from start time."
  (let* ((t1 (org-ml--timestamp-get-start-timelist timestamp))
         (has-time (org-ml-timelist-has-time t1))
         ;; convert to unixtime and back to fix any overflow values
         (t2 (->> (org-ml-timelist-shift n unit t1)
                  (org-ml-timelist-to-unixtime)
                  (org-ml-unixtime-to-timelist has-time)))
         (rt (->> (org-element-property-raw :range-type timestamp)
                  (org-ml--timelists-get-range-type t1 t2))))
    (->> (org-ml--timestamp-set-end-timelist-nocheck t2 timestamp)
         (org-ml--timestamp-update-type-ranged)
         (org-ml--timestamp-set-range-type rt))))

(defun org-ml--timestamp-update-type-ranged (timestamp)
  "Return TIMESTAMP with updated `:type' and `:range-type'.

Specifically, this assumes that the start and/or end of TIMESTAMP
have just been updated, and that the `:type' and `:range-type'
are now out of sync with the range between start/end. If deciding
between `timerange' or `daterange', prefer the original value of
TIMESTAMP if possible."
  (let* ((t1 (org-ml--timestamp-get-start-timelist timestamp))
         (t2 (org-ml--timestamp-get-end-timelist timestamp))
         (rt (->> (org-element-property-raw :range-type timestamp)
                  (org-ml--timelists-get-range-type t1 t2))))
    (org-ml--timestamp-set-range-type rt timestamp)))

(defun org-ml--timestamp-set-active (flag timestamp)
  "Return TIMESTAMP with active type if FLAG is t."
  (let ((type (if (org-element-property-raw :range-type timestamp)
                  (if flag 'active-range 'inactive-range)
                (if flag 'active 'inactive))))
    (org-element-put-property-2 :type type timestamp)))

(defun org-ml--timestamp-get-warning (timestamp)
  "Return the warning component of TIMESTAMP.
Return a list like (TYPE VALUE UNIT) or nil."
  (-let (((&plist :warning-type y :warning-value v :warning-unit u)
          (org-ml--get-nonstandard-properties timestamp)))
    (when (and y v u) `(,y ,v, u))))

(defun org-ml--timestamp-set-warning (warning timestamp)
  "Return TIMESTAMP with warning properties set to WARNING list."
  (-let (((type value unit) warning))
    (org-ml--set-properties-raw timestamp
      :warning-type type
      :warning-value value
      :warning-unit unit)))

(defun org-ml--timestamp-get-repeater (timestamp)
  "Return the repeater component of TIMESTAMP.
Return a list like (TYPE VALUE UNIT) or nil."
  (-let (((&plist :repeater-type y :repeater-value v :repeater-unit u)
          (org-ml--get-nonstandard-properties timestamp)))
    (when (and y v u) `(,y ,v, u))))

(defun org-ml--timestamp-set-repeater (repeater timestamp)
  "Return TIMESTAMP with repeater properties set to REPEATER."
  (unless repeater
    (org-ml--set-properties-raw timestamp
      :repeater-deadline-value nil
      :repeater-deadline-unit nil))
  (-let (((type value unit) repeater))
    (org-ml--set-properties-raw timestamp
      :repeater-type type
      :repeater-value value
      :repeater-unit unit)))

(defun org-ml--timestamp-set-deadline (deadline timestamp)
  "Return TIMESTAMP with repeater properties set to DEADLINE."
  (if (not (org-ml--timestamp-get-repeater timestamp)) timestamp
    (-let (((value unit) deadline))
      (org-ml--set-properties-raw timestamp
        :repeater-deadline-value value
        :repeater-deadline-unit unit))))

(defun org-ml--timestamp-set-collapsed (flag timestamp)
  "Return TIMESTAMP with collapsed set to FLAG."
  (pcase (org-element-property-raw :range-type timestamp)
    ;; collapsed
    (`timerange
     (if flag timestamp
       (->> (org-ml-copy timestamp)
            (org-element-put-property-2 :range-type 'daterange))))
    ;; uncollapsed
    (`daterange
     (if (and (org-ml--timestamp-get-start-time timestamp)
              (org-ml--timestamp-get-end-time timestamp))
         (cond
          ((and (eq flag t) (org-ml--timestamp-has-equal-dates-p timestamp))
           (org-element-put-property-2 :range-type 'timerange timestamp))
          ((and (eq flag 'force))
           (let ((s (org-ml--timestamp-get-start-timelist timestamp)))
             (->> (org-ml-copy timestamp)
                  (org-ml--timestamp-set-end-timelist-nocheck s)
                  (org-element-put-property-2 :range-type 'timerange))))
          (t
           timestamp))
       timestamp))
    ;; neither
    (`nil
     timestamp)
    (e
     (error "Invalid range-type %s" e))))

(defun org-ml--timestamp-set-start-time (time timestamp-diary)
  "Set the start of TIMESTAMP-DIARY to TIME. Does not set type."
  (-let (((H M) time))
    (org-ml--set-properties-raw timestamp-diary
      :hour-start H
      :minute-start M)))

(defun org-ml--timestamp-set-end-time (time timestamp-diary)
  "Set the end of TIMESTAMP-DIARY to TIME. Does not set type."
  (-let (((H M) time))
    (org-ml--set-properties-raw timestamp-diary
      :hour-end H
      :minute-end M)))

(defun org-ml--timestamp-update-type-ranged-timeonly (timestamp-diary)
  "Return TIMESTAMP-DIARY with updated `:range-type'."
  (let* ((t1 (org-ml--timestamp-get-start-time timestamp-diary))
         (t2 (org-ml--timestamp-get-end-time timestamp-diary))
         (rt (if (equal t1 t2) nil 'timerange)))
    (org-element-put-property-2 :range-type rt timestamp-diary)))

;; timestamp (diary sexp)

;;; element nodes
;;
;; item

(defun org-ml--item-get-subcomponents (item)
  "Return the children of ITEM broken down into subcomponents.
The returned list will be of the form (HEAD SUBITEMS POST-BLANK
REST) where HEAD consists of all nodes before the first nested
plain-list, SUBITEMS will be all items in the nested plain-list,
POST-BLANK will be the post-blank of the nested plain-list, and
REST will be everything after the plain-list (which should be nil
for all sensible items).

Example item showing how this breaks down:

- HEAD
  - SUBITEM1
  - SUBITEM2 (with POST-BLANK 1 below)

  REST

REST may itself contain more plain lists, but (for now at least)
let's consider these cases extremely rare. This function will
still do the right thing, but any plain list in REST will be
off-limits for the indent/outdent functions that use this
function."
  (-let* (((h (s . r)) (->> (org-element-contents item)
                            (--split-with (not (org-ml--is-type 'plain-list it)))))
          (pb (if s (org-element-post-blank s) 0))
          (i (org-element-contents s)))
    (list h i pb r)))

(defun org-ml--item-set-subcomponents (subcomponents item)
  "Return the child subcomponents of ITEM.
SUBCOMPONENTS is a list like that returned by
`org-ml--item-get-subcomponents'."
  (-let* (((head subitems sub-pb rest) subcomponents))
    (-when-let (pb (cond
                    (rest (org-element-post-blank (-last-item rest)))
                    (subitems sub-pb)
                    (head (org-element-post-blank (-last-item head)))))
      ;; TODO why did I do this?
      (let ((rest* (org-ml--set-last-post-blank 0 rest))
            (sublist (apply #'org-ml-build-plain-list
                            :post-blank (if rest sub-pb 0)
                            subitems)))
        (->> (org-ml--set-children-nocheck `(,@head ,sublist ,@rest*) item)
             (org-ml--shift-post-blank-textsafe pb))))))

(defmacro org-ml--item-map-subcomponents* (form item)
  "Return ITEM with subcomponents modified.
FORM is a form where the subcomponents of item are bound to the
symbol `it' and returns modified subcomponents. The subcomponents
will conform to those given in `org-ml--item-get-subcomponents'."
  (declare (debug (form form)))
  (let ((i (make-symbol "item")))
    `(let ((,i ,item))
       (let ((it (org-ml--item-get-subcomponents ,i)))
         (org-ml--item-set-subcomponents ,form ,i)))))

(defmacro org-ml--item-map-subcomponents-cond*
    (head-form subitem-form rest-form item)
  "Return ITEM with subcomponents modified.

First, split ITEM using `org-ml--item-get-subcomponents' and
assign each of the four outputs to `it-head', `it-subitems',
`it-rest-blank', and `it-rest' respectively.

REST-FORM will run with all `it' variables are non-nil.
This should return a modified `it-rest' analogue.

SUBITEM-FORM will run if `it-rest' is nil and the rest are
non-nil. This should return a list (SUBITEMS REST-BLANK REST).

HEAD-FORM will run if `it-rest' and `it-subitems' are nil and the
others are non-nil. This should return a list to be fed into
`org-ml--item-set-subcomponents'."
  (declare (indent 3))
  (let ((h (make-symbol "--head"))
        (s (make-symbol "--subitems"))
        (b (make-symbol "--blank"))
        (r (make-symbol "--rest")))
  `(org-ml--item-map-subcomponents*
    (-let (((,h ,s ,b ,r) it))
      (cond
       (,r
        (let ((it-rest ,r))
          (list ,h ,s ,b ,rest-form)))
       (,s
        (let ((it-subitems ,s))
          (cons ,h ,subitem-form)))
       (t
        (let ((it-head ,h))
          ,head-form))))
    ,item)))

(defun org-ml--item-get-subitems (item)
  "Return the subitems of ITEM."
  (-let (((_ s _ _) (org-ml--item-get-subcomponents item)))
    s))

(defun org-ml--item-set-subitems (subitems item)
  "Return ITEM with subitems set to SUBITEMS."
  (org-ml--item-map-subcomponents*
   (-let (((h _ p r) it))
     (list h subitems p r))
   item))

(defmacro org-ml--item-map-subitems* (form item)
  "Return a ITEM with FORM applied to its sublist if present.
FORM is a Lisp form in which the symbol `it' is bound to the
items in the sub plain-list, and returns a modified list of
items."
  (declare (debug (form form)))
  (let ((h (make-symbol "h"))
        (p (make-symbol "p"))
        (r (make-symbol "r")))
    `(org-ml--item-map-subcomponents*
      (-let (((,h it ,p ,r) it))
        (list ,h ,form ,p ,r))
      ,item)))

;; headline

(defun org-ml--headline-shift-level (n headline)
  "Return HEADLINE node with the level property shifted by N.
If the level is less then one after shifting, set level to one."
  (->> (org-element-properties-resolve headline)
       (org-ml--map-property-raw* :level (org-ml--shift-pos-integer n it))))

(defun org-ml--headline-set-statistics-cookie (value headline)
  "Return HEADLINE node with statistics cookie set by VALUE.
VALUE is a list conforming to `org-ml--is-valid-statistics-cookie-value'
or nil to erase the statistics cookie if present."
  (org-ml--map-property-raw*
   :title
   (let ((last? (org-ml--is-type 'statistics-cookie (-last-item it))))
     (cond
      ((and last? value)
       ;; NOTE use full property setter here since this will call the encoder
       (org-ml--map-last* (org-ml-set-property :value value it) (org-ml-copy it)))
      ((and last? (not value))
       (-drop-last 1 it))
      (value
       (-snoc it (org-ml-build-statistics-cookie value)))
      (t it)))
   (org-element-properties-resolve headline)))

(defun org-ml--headline-set-statistics-cookie-fraction (done total headline)
  "Return HEADLINE node with statistics cookie set by DONE and TOTAL.

DONE and TOTAL are integers representing the numerator and denominator
respectively of the statistics-cookie's fractional value. Both must
be greater than zero, and DONE must be less than or equal to TOTAL."
  (-if-let (cookie (org-ml-headline-get-statistics-cookie headline))
      (let* ((format (org-ml--statistics-cookie-get-format cookie))
             (value (if (eq 'fraction format) `(,done ,total)
                      (-> (float done)
                          (/ total)
                          (* 100)
                          (round)
                          (list)))))
        (org-ml--headline-set-statistics-cookie value headline))
    headline))

;; planning

(defun org-ml--build-planning-timestamp (active timelist)
  "Build a planning timestamp.

ACTIVE is a boolean. TIMELIST is a list like (year month date
[hour] [minute]).

Note this is a more optimized version of `org-ml-build-timestamp!'"
  (-let (((y m d H M) timelist))
    (org-ml--check-timelist y m d H M)
    (org-ml--set-properties-raw (org-ml--build-blank-node timestamp 0)
      :year-start y
      :month-start m
      :day-start d
      :hour-start H
      :minute-start M
      :year-end y
      :month-end m
      :day-end d
      :hour-end H
      :minute-end M
      :type (if active 'active 'inactive))))

(defun org-ml--planning-list-to-timestamp (planning-list)
  "Return timestamp node from PLANNING-LIST.
See `org-ml-build-planning!' for syntax of PLANNING-LIST."
  (-let* ((p (-partition-before-pred
              (lambda (it) (memq it '(&warning &repeater)))
              planning-list))
          (ts (org-ml--build-planning-timestamp t (car p))))
    (-when-let (w (alist-get '&warning p))
      (org-ml--check-warning-from-list w)
      (org-ml--timestamp-set-warning w ts))
    (-when-let (r (alist-get '&repeater p))
      (org-ml--check-repeater-from-list r)
      (org-ml--timestamp-set-repeater r ts))
    ts))

(defun org-ml--timestamp-to-planning-list (timestamp)
  "Return TIMESTAMP as planning list.
See `org-ml-build-planning!' for syntax of PLANNING-LIST. This is
only meant for deadline or scheduled timestamps, since the list
for closed is trival."
  (let ((timelist (org-ml--timestamp-get-start-timelist timestamp))
        (warning (org-ml--timestamp-get-warning timestamp))
        (repeater (org-ml--timestamp-get-repeater timestamp)))
    (append timelist
            (and warning (cons '&warning warning))
            (and repeater (cons '&repeater repeater)))))

;; clock

(defun org-ml--build-clock-timestamp (start end)
  "Build clock timestamp from START and END.

Both arguments are lists like (year month date hour minute).

This is a more optimized version of `org-ml-build-timestamp!'."
  (-let (((y0 m0 d0 H0 M0) start)
         ((y1 m1 d1 H1 M1) (or end start)))
    (org-ml--check-timelist y0 m0 d0 H0 M0)
    (when end
      (org-ml--check-timelist y1 m1 d1 H1 M1))
    (org-ml--set-properties-raw (org-ml--build-blank-node timestamp 0)
      :year-start y0
      :month-start m0
      :day-start d0
      :hour-start H0
      :minute-start M0
      :year-end y1
      :month-end m1
      :day-end d1
      :hour-end H1
      :minute-end M1
      :range-type (and end 'daterange)
      :type (if end 'inactive-range 'inactive))))

;;; INTERNAL TYPE-SPECIFIC BRANCH/CHILD FUNCTIONS

;;; headline

(defun org-ml-headline-get-section (headline)
  "Return children of section node in HEADLINE node or nil if none."
  (--> (car (org-element-contents headline))
       (when (org-ml--is-type 'section it) (org-element-contents it))))

(defun org-ml-headline-set-section (children headline)
  "Return HEADLINE with section node containing CHILDREN.
If CHILDREN is nil, return HEADLINE with no section node."
  (org-ml--map-children-nocheck*
    (if (org-ml--is-type 'section (car it))
        (cons (org-ml-set-children children (car it)) (cdr it))
      (cons (apply #'org-ml-build-section children) it))
    headline))

(org-ml--defun-anaphoric* org-ml-headline-map-section (fun headline)
  "Return HEADLINE node with child section node modified by FUN.

FUN is a unary function that takes a section node's children as a list
returns a modified child list."
  (--> (org-ml-headline-get-section headline)
       (org-ml-headline-set-section (funcall fun it) headline)))

(defun org-ml-headline-get-subheadlines (headline)
  "Return list of child headline nodes in HEADLINE node or nil if none."
  (let ((children (org-element-contents headline)))
    (if (org-ml--is-type 'section (car children)) (cdr children) children)))

(defun org-ml-headline-set-subheadlines (subheadlines headline)
  "Return HEADLINE node with SUBHEADLINES set to child subheadlines."
  (org-ml--map-children-nocheck*
    (-if-let (section (assq 'section it))
        (cons section subheadlines)
      subheadlines)
    headline))

(org-ml--defun-anaphoric* org-ml-headline-map-subheadlines (fun headline)
  "Return HEADLINE node with child headline nodes modified by FUN.

FUN is a unary function that takes a list of headlines and returns
a modified list of headlines."
  (--> (org-ml-headline-get-subheadlines headline)
       (org-ml-headline-set-subheadlines (funcall fun it) headline)))

(defun org-ml--headline-subtree-shift-level (n headline)
  "Return HEADLINE node with its level shifted by N.
Also shift all HEADLINE node's child headline nodes by N.
If the final shifted level is less one, set level to one (for parent
and child nodes)."
  (->> (org-ml-copy headline)
       (org-ml--headline-shift-level n)
       (org-ml-headline-map-subheadlines*
         (--map (org-ml--headline-subtree-shift-level n it) it))))

(defun org-ml--headline-set-level (level headline)
  "Return HEADLINE node with its level set to LEVEL.
Additionally set all child headline nodes to be (+ 1 level) for
first layer, (+ 2 level) for second, and so on."
  ;; NOTE full setter needed since this is called from the headline builder
  (->> (org-element-put-property-2 :level level headline)
       (org-ml-headline-map-subheadlines*
         (--map (org-ml--headline-set-level (1+ level) it) it))))

;;; table

(defun org-ml--table-get-width (table)
  "Return the width of TABLE as an integer.
This effectively is the maximum of all table-row lengths."
  (->> (org-element-contents table)
       (--map (length (org-element-contents it)))
       (-max)))

(defun org-ml--table-pad-or-truncate (length list)
  "Pad or truncate LIST of table-cell nodes by LENGTH.
Behavior is the same as `org-ml--pad-or-truncate' where the padded value
is a blank table-cell node."
  (let ((pad (org-ml-build-table-cell "")))
    (org-ml--pad-or-truncate length pad list)))

(defun org-ml--column-map-down-rows (fun column-index table)
  "Return TABLE node with FUN applied down the rows at COLUMN-INDEX.

FUN is a unary function that takes a table-cell node and returns
a modified table-cell node."
  (cl-flet
      ((zip-into-rows
        (row new-cell)
        (if (org-ml--property-is-eq :type 'rule row) row
          (org-ml--map-children-nocheck* (funcall fun new-cell it) row))))
    (org-ml--map-children-nocheck*
     (->> (--find-indices (org-ml--property-is-eq :type 'rule it) it)
          (--reduce-from (-insert-at it nil acc) column-index)
          (org-ml--table-pad-or-truncate (length it))
          (-zip-with #'zip-into-rows it))
     table)))

(defun org-ml--table-get-row (row-index table)
  "Return the table-row node at ROW-INDEX within TABLE.
Rule-type table-row nodes do not factor when counting the index."
  (->> (org-element-contents table)
       (--filter (org-ml--property-is-eq :type 'standard it))
       (org-ml--nth row-index)))

(defun org-ml--table-replace-column (column-index column-cells table)
  "Return TABLE with COLUMN-CELLS in place of original cells at COLUMN-INDEX."
  (org-ml--column-map-down-rows
   (lambda (new-cell cells) (org-ml--replace-at column-index new-cell cells))
   column-cells
   table))

(defun org-ml--table-row-pad-maybe (table table-row)
  "Return TABLE-ROW with row truncated or padded.
See `org-ml--table-pad-or-truncate' for how padding and truncation is
performed. TABLE is used to get the table width."
  (if (org-ml--property-is-eq :type 'rule table-row) table-row
    (let ((width (org-ml--table-get-width table)))
      (org-ml--map-children-nocheck*
       (org-ml--table-pad-or-truncate width it)
       table-row))))

(defun org-ml--table-replace-row (row-index table-row table)
  "Return TABLE node with row at ROW-INDEX replaced by TABLE-ROW."
  (let ((table-row (org-ml--table-row-pad-maybe table table-row)))
    (org-ml--map-children-nocheck*
     (org-ml--replace-at row-index table-row it)
     table)))

(defun org-ml--table-clear-row (row-index table)
  "Return TABLE with table-cells in row at ROW-INDEX filled with blanks."
  (org-ml--table-replace-row row-index (org-ml-build-table-row! '(" ")) table))

(defun org-ml--table-clear-column (column-index table)
  "Return TABLE with table-cells in column at COLUMN-INDEX filled with blanks."
  (org-ml--table-replace-column column-index `(,(org-ml-build-table-cell " ")) table))

;;; COMPOSITE BUILDERS

;;; misc builders

(org-ml--defun-kw org-ml-build-timestamp-diary (form &key start end post-blank)
  "Return a new diary-sexp timestamp node from FORM.

TIME1 and TIME1 are lists like (hour min) which specify the
time(s) of the diary timestamp. If TIME2 is provided, TIME1 must
also be provided and the timestamp will be ranged. Optionally set
POST-BLANK (a positive integer)."
  ;; TODO this isn't very efficient
  (->> (org-ml--build-blank-node timestamp post-blank)
       (org-element-put-property-2 :type 'diary)
       (org-ml-timestamp-diary-set-value form)
       (org-ml-timestamp-diary-set-double-time start end)))

(org-ml--defun-kw org-ml-build-table-row-hline (&key post-blank)
  "Return a new rule-typed table-row node.
Optionally set POST-BLANK (a positive integer)."
  (->> (org-ml--build-blank-node table-row post-blank)
       (org-element-put-property-2 :type 'rule)))

;;; shorthand builders

;; These function offer a shorter and more convenient way of building
;; nodes. They all end in '!' (and all associated functions later

(eval-and-compile
  (defvar org-ml--shorthand-builder-cache
    (--map (cons it (make-hash-table :test #'equal))
           org-ml-shorthand-builder-types)
    "Alist of hash tables to store shorthand builder results."))

(defun org-ml-clear-shorthand-builder-cache ()
  "Clear the memoization cache for shorthand node builders."
  (interactive)
  (--each org-ml--shorthand-builder-cache
    (clrhash (cdr it))))

(defmacro org-ml--with-shorthand-builder-cache (type key body)
  "Run BODY with shorthand builder cache.
See org-ml--with-cache' for meaning of TYPE and KEY."
  (declare (indent 1))
  `(org-ml--with-cache
     org-ml--shorthand-builder-cache
     org-ml-memoize-shorthand-builders
     org-ml-memoize-shorthand-builder-types
     ,type ,key ,body))


(defun org-ml-build-secondary-string! (string)
  "Return a secondary string (list of object nodes) from STRING.
STRING is any string that contains a textual representation of
object nodes. If the string does not represent a list of object nodes,
throw an error."
  (org-ml--with-shorthand-builder-cache secondary-string
    string
    ;; add space to prevent leading stars from parsing as headlines
    (-if-let (d (->> (org-ml--from-string (concat " " string))
                     (org-ml--get-descendent '(0))))
        ;; special case, anything starting with what looks like a bullet will
        ;; be parsed as a list with one item
        (if (org-ml--is-type 'plain-list d)
            (-let* ((i (org-ml--get-descendent '(0) d))
                    ((first . rest) (->> (org-ml--get-descendent '(0) i)
                                         (org-element-contents)))
                    (bullet (org-element-property-raw :bullet i)))
              (if (org-ml--is-type 'plain-text first)
                  `(,(concat bullet first) ,@rest)
                `(,bullet ,first ,@rest)))
          (if-let (ss (org-element-contents d))
              (cond
               ((not (org-ml--is-secondary-string ss))
                (org-ml--arg-error "Secondary string must only contain objects"))
               ((equal (car ss) " ")
                (-drop 1 ss))
               (t (org-ml--map-first* (substring it 1) ss)))
            (org-ml--arg-error "Could not make secondary string from %S" string)))
      (org-ml--arg-error "Could not make secondary string from %S" string))))

(org-ml--defun-kw org-ml-build-timestamp! (start &key end active repeater
                                                 deadline warning collapsed
                                                 post-blank)
  "Return a new timestamp node.

START specifies the start time and is a list of integers in one of
the following forms:
- (YEAR MONTH DAY): short form
- (YEAR MONTH DAY nil nil): short form
- (YEAR MONTH DAY HOUR MINUTE): long form

END (if supplied) will add the ending time, and follows the same
formatting rules as START.

ACTIVE is a boolean where t signifies the type is `active', else
`inactive' (the range suffix will be added if an end time is
supplied).

REPEATER, DEADLINE, and WARNING are lists corresponding to those
required for `org-ml-timestamp-set-repeater',
`org-ml-timestamp-set-deadline', and
`org-ml-timestamp-set-warning' respectively.

Building a diary sexp timestamp is not possible with this function."
  (org-ml--check-timelist-from-list start)
  (when end
    (org-ml--check-timelist-from-list end))
  (when repeater
    (org-ml--check-repeater-from-list repeater))
  (when warning
    (org-ml--check-warning-from-list warning))
  (when deadline
    (org-ml--check-deadline-from-list deadline))
  (org-ml--with-shorthand-builder-cache timestamp
    (list start end active repeater deadline warning collapsed post-blank)
    (org-ml->> (org-ml--build-blank-node timestamp post-blank)
      (org-ml--timestamp-set-start-timelist-nocheck start)
      (org-ml--timestamp-set-end-timelist-nocheck end)
      (org-ml--timestamp-set-active active)
      (org-ml--timestamp-update-type-ranged)
      (org-ml--timestamp-set-warning warning)
      (org-ml--timestamp-set-repeater repeater)
      (org-ml--timestamp-set-deadline deadline)
      (org-ml--timestamp-set-collapsed (or collapsed t)))))

(org-ml--defun-kw org-ml-build-clock! (start &key end post-blank)
  "Return a new clock node.

START and END follow the same rules as their respective arguments in
`org-ml-build-timestamp!'."
  (org-ml--with-shorthand-builder-cache clock
    (list start end post-blank)
    (let ((ts (org-ml--build-clock-timestamp start end)))
      (org-ml-build-clock ts :post-blank post-blank))))

(org-ml--defun-kw org-ml-build-planning! (&key closed deadline scheduled
                                               post-blank)
  "Return a new planning node.

DEADLINE and SCHEDULED are lists with the following structure
\(brackets denote optional members):

\(YEAR MINUTE DAY [HOUR] [MIN]
 [&warning TYPE VALUE UNIT]
 [&repeater TYPE VALUE UNIT])

In terms of arguments supplied to `org-ml-build-timestamp!', the first
five members correspond to the list supplied as TIME, and the TYPE,
VALUE, and UNIT fields correspond to the lists supplied to WARNING and
REPEATER arguments. The order of warning and repeater does not
matter.

CLOSED is a similar list to above but does not have &warning or
&repeater."
  (org-ml--with-shorthand-builder-cache planning
    (list closed deadline scheduled post-blank)
    (let ((node (org-ml--build-blank-node planning (or post-blank 0))))
      (when closed
        (->> (org-ml--build-planning-timestamp nil closed)
             (org-element-put-property node :closed)))
      (when deadline
        (->> (org-ml--planning-list-to-timestamp deadline)
             (org-element-put-property node :deadline)))
      (when scheduled
        (->> (org-ml--planning-list-to-timestamp scheduled)
             (org-element-put-property node :scheduled)))
      node)))

(org-ml--defun-kw org-ml-build-property-drawer! (&key post-blank &rest keyvals)
  "Return a new property-drawer node.

Each member in KEYVALS is a list like (KEY VAL) where KEY and VAL
are both strings, where each list will generate a node-property
node in the property-drawer node like \":key: val\"."
  (org-ml--with-shorthand-builder-cache property-drawer
    (cons post-blank keyvals)
    (->> keyvals
         (--map (let ((key (car it))
                      (val (cadr it)))
                  (org-ml-build-node-property key val)))
         (apply #'org-ml-build-property-drawer :post-blank post-blank))))

(org-ml--defun-kw org-ml-build-headline! (&key (level 1) title-text
                                               todo-keyword tags pre-blank
                                               priority commentedp archivedp
                                               post-blank planning
                                               statistics-cookie
                                               section-children
                                               &rest subheadlines)
  "Return a new headline node.

TITLE-TEXT is a oneline string for the title of the headline.

PLANNING is a list like (PLANNING-TYPE ARGS ...) where
PLANNING-TYPE is one of `:closed', `:deadline', or `:scheduled', and
ARGS are the args supplied to any of the planning types in
`org-ml-build-planning!'. Up to all three planning types can be used
in the same list like (:closed ARGS :deadline ARGS :scheduled ARGS).

STATISTICS-COOKIE is a list following the same format as
`org-ml-build-statistics-cookie'.

SECTION-CHILDREN is a list of elements that will go in the headline
section.

SUBHEADLINES contains zero or more headlines that will go under the
created headline. The level of all members in SUBHEADLINES will
automatically be adjusted to LEVEL + 1.

All arguments not mentioned here follow the same rules as
`org-ml-build-headline'"
  (org-ml--with-shorthand-builder-cache headline
    (append (list level title-text todo-keyword tags pre-blank priority
                  commentedp archivedp post-blank planning statistics-cookie
                  section-children)
            subheadlines)
    (let* ((planning (-some->> planning (apply #'org-ml-build-planning!)))
           (section (-some->>  (if planning (cons planning section-children)
                                 section-children)
                      (apply #'org-ml-build-section)))
           (shls (--map (org-ml--headline-set-level (1+ level) it) subheadlines))
           (nodes (--> shls (if section (cons section it) it))))
      (->> (apply #'org-ml-build-headline
                  :todo-keyword todo-keyword
                  :level level
                  :tags tags
                  :post-blank post-blank
                  :pre-blank pre-blank
                  :priority priority
                  :commentedp commentedp
                  :archivedp archivedp
                  nodes)
           (org-ml-headline-set-title! title-text statistics-cookie)))))

(org-ml--defun-kw org-ml-build-paragraph! (string &key post-blank)
  "Return a new paragraph node from STRING.

STRING is the text to be parsed into a paragraph and must contain
valid textual representations of object nodes."
  ;; ASSUME all children coming from `org-ml-build-secondary-string!' will be
  ;; valid so bypass type checking overhead.
  (org-ml--with-shorthand-builder-cache paragraph
    (list string post-blank)
    (->> (org-ml--build-blank-node paragraph post-blank)
         (org-ml-set-children (org-ml-build-secondary-string! string)))))

(org-ml--defun-kw org-ml-build-item! (&key post-blank bullet checkbox tag
                                           paragraph counter &rest children)
  "Return a new item node.

TAG is a string representing the tag (make with
`org-ml-build-secondary-string!') .

PARAGRAPH is a string that will be the initial text in the item
\(made with `org-ml-build-paragraph!').

CHILDREN contains the nodes that will go under this item after
PARAGRAPH.

All other arguments follow the same rules as `org-ml-build-item'."
  (org-ml--with-shorthand-builder-cache item
    (append (list post-blank bullet checkbox tag paragraph counter) children)
    (let ((children* (or (-some-> paragraph
                           (org-ml-build-paragraph!)
                           (cons children))
                         children))
          (tag (-some->> tag (org-ml-build-secondary-string!))))
      (apply #'org-ml-build-item
             :post-blank post-blank
             :bullet bullet
             :checkbox checkbox
             :counter counter
             :tag tag
             children*))))

(defun org-ml-build-table-cell! (string)
  "Return a new table-cell node.

STRING is the text to be contained in the table-cell node. It must
contain valid textual representations of objects that are allowed in
table-cell nodes."
  (org-ml--with-shorthand-builder-cache table-cell
    string
    (apply #'org-ml-build-table-cell (org-ml-build-secondary-string! string))))

(defun org-ml-build-table-row! (row-list)
  "Return a new table-row node.

ROW-LIST is a list of strings to be built into table-cell nodes via
`org-ml-build-table-cell!' (see that function for restrictions).
Alternatively, ROW-LIST may the symbol `hline' instead of a string to
create a rule-typed table-row."
  (org-ml--with-shorthand-builder-cache table-row
    row-list
    (if (eq row-list 'hline) (org-ml-build-table-row-hline)
      (->> (-map #'org-ml-build-table-cell! row-list)
           (apply #'org-ml-build-table-row)))))

(org-ml--defun-kw org-ml-build-table! (&key tblfm post-blank &rest row-lists)
  "Return a new table node.

Each member of ROW-LISTS will be converted to a table-row node
via `org-ml-build-table-row!' (see that function for
restrictions).

All other arguments follow the same rules as `org-ml-build-table'."
  (org-ml--with-shorthand-builder-cache table
    (append (list tblfm post-blank) row-lists)
    (->> (-map #'org-ml-build-table-row! row-lists)
         (apply #'org-ml-build-table :tblfm tblfm :post-blank post-blank))))

(defun org-ml-build-org-data (&rest nodes)
  "Return a new org-data node using NODES.
NODES should be either headline or section nodes."
  (->> (org-ml--build-blank-node org-data nil)
       (org-ml-set-children nodes)))

;;; logbook items

;; internal

(defun org-ml--log-replace (placeholder string heading)
  "Return HEADING with PLACEHOLDER replaced by STRING."
  (->> (cons placeholder string)
       (list)
       (org-replace-escapes heading)))

(defun org-ml--log-replace-new (string heading)
  "Return HEADING with placeholder \"%s\" replaced by STRING."
  (--> (format "\"%s\"" string)
       (org-ml--log-replace "%s" it heading)))

(defun org-ml--log-replace-old (string heading)
  "Return HEADING with placeholder \"%S\" replaced by STRING."
  (--> (format "\"%s\"" string)
       (org-ml--log-replace "%S" it heading)))

(defun org-ml--log-replace-new-state (state heading)
  "Return HEADING with placeholder \"%s\" replaced by string STATE."
  (org-ml--log-replace-new state heading))

(defun org-ml--log-replace-old-state (state heading)
  "Return HEADING with placeholder \"%S\" replaced by string STATE."
  (org-ml--log-replace-old state heading))

(defun org-ml--log-replace-new-timestamp (timestamp heading)
  "Return HEADING with placeholder \"%s\" replaced by TIMESTAMP.
TIMESTAMP is a timestamp node and will be converted to an inactive
timestamp if active."
  (-> (org-ml-timestamp-set-active nil timestamp)
      (org-ml-to-string)
      (org-ml--log-replace-new heading)))

(defun org-ml--log-replace-old-timestamp (timestamp heading)
  "Return HEADING with placeholder \"%S\" replaced by TIMESTAMP.
TIMESTAMP is a timestamp node and will be converted to an inactive
timestamp if active."
  (-> (org-ml-timestamp-set-active nil timestamp)
      (org-ml-to-string)
      (org-ml--log-replace-old heading)))

(defun org-ml--log-replace-timestamp (unixtime active-p long-p heading)
  "Return HEADING with timestamp placeholders replaced by a timestamp.

UNIXTIME is an integer to be converted to a timestamp.

The type of timestamp and the placeholders that are replaced depend
on the boolean values of ACTIVE-P and LONG-P:
- ACTIVE-P and LONG-P are t: long active timestamp replacing \"T\"
- ACTIVE-P is t: short active timestamp replacing \"D\"
- LONG-P is t: long inactive timestamp replacing \"t\"
- both nil: short inactive timestamp replacing \"d\""
  (let ((key (cond ((and active-p long-p) "%T")
                   (active-p "%D")
                   (long-p "%t")
                   (t "%d")))
        (time (if long-p (org-ml-unixtime-to-datetime unixtime)
                (org-ml-unixtime-to-date unixtime))))
    ;; TODO this can likely be optimized
    (--> (org-ml-build-timestamp! time :active active-p)
         (org-ml-to-string it)
         (org-ml--log-replace key it heading))))

(defun org-ml--log-replace-username (username heading)
  "Return HEADING with \"%u\" replaced by symbol USERNAME."
  (org-ml--log-replace "%u" username heading))

(defun org-ml--log-replace-full-username (full-username heading)
  "Return HEADING with \"%U\" replaced by symbol FULL-USERNAME."
  (org-ml--log-replace "%U" full-username heading))

(defun org-ml--log-get (type)
  "Return the log heading associated with symbol TYPE.
This function will only use the default value of
`org-log-note-headings' and is thus a pure function."
  (alist-get type (default-value 'org-log-note-headings)))

(defun org-ml--build-log-item (note heading)
  "Return an item with string HEADING as its first line.
If string NOTE is supplied, append this after a newline object node
in the first paragraph of the returned item."
  (->> (if note (format "%s \\\\\n  %s" heading note) heading)
       (org-ml-build-paragraph!)
       (org-ml-build-item)))

(defun org-ml--build-log-item-trans (type unixtime old-timestamp note)
  "Return an item for any of the transition log entry types.
These are re/del-schedule/deadline (specified with TYPE) transitioning
from OLD-TIMESTAMP at UNIXTIME with optionally supplied NOTE."
  (->> (org-ml--log-get type)
       (org-ml--log-replace-old-timestamp old-timestamp)
       (org-ml--log-replace-timestamp unixtime nil t)
       (org-ml--build-log-item note)))

;; public

(defun org-ml-build-log-done (unixtime &optional note)
  "Return an item node for a done log entry.

This will format the log entry from the default value for the
`done' cell in `org-log-note-headings'.

UNIXTIME is an integer representing the time to be used for all
timestamp nodes.

If string NOTE is supplied, append a note to the log entry."
  (->> (org-ml--log-get 'done)
       (org-ml--log-replace-timestamp unixtime nil t)
       (org-ml--build-log-item note)))

(defun org-ml-build-log-state (unixtime new-state old-state &optional note)
  "Return an item node for a state change log entry.

This will format the log entry from the default value for the
`state' cell in `org-log-note-headings'.

UNIXTIME is an integer representing the time to be used for all
timestamp nodes.

NEW-STATE and OLD-STATE are strings for the new and old todo keywords
respectively.

If string NOTE is supplied, append a note to the log entry."
  (->> (org-ml--log-get 'state)
       (org-ml--log-replace-new-state new-state)
       (org-ml--log-replace-old-state old-state)
       (org-ml--log-replace-timestamp unixtime nil t)
       (org-ml--build-log-item note)))

(defun org-ml-build-log-note (unixtime note)
  "Return an item node for a new note log entry.

This will format the log entry from the default value for the
`note' cell in `org-log-note-headings'.

UNIXTIME is an integer representing the time to be used for all
timestamp nodes.

NOTE is a string for the note text."
  (->> (org-ml--log-get 'note)
       (org-ml--log-replace-timestamp unixtime nil t)
       (org-ml--build-log-item note)))

(defun org-ml-build-log-reschedule (unixtime old-timestamp &optional note)
  "Return an item node for a new schedule log entry.

This will format the log entry from the default value for the
`reschedule' cell in `org-log-note-headings'.

UNIXTIME is an integer representing the time to be used for all
timestamp nodes.

OLD-TIMESTAMP is a timestamp node of the schedule that is being
deleted. It will always be converted to an inactive timestamp.

If string NOTE is supplied, append a note to the log entry."
  (org-ml--build-log-item-trans 'reschedule unixtime old-timestamp note))

(defun org-ml-build-log-delschedule (unixtime old-timestamp &optional note)
  "Return an item node for a delete schedule log entry.

This will format the log entry from the default value for the
`delschedule' cell in `org-log-note-headings'.

UNIXTIME is an integer representing the time to be used for all
timestamp nodes.

OLD-TIMESTAMP is a timestamp node of the schedule that is being
deleted. It will always be converted to an inactive timestamp.

If string NOTE is supplied, append a note to the log entry."
  (org-ml--build-log-item-trans 'delschedule unixtime old-timestamp note))

(defun org-ml-build-log-redeadline (unixtime old-timestamp &optional note)
  "Return an item node for a new deadline log entry.

This will format the log entry from the default value for the
`redeadline' cell in `org-log-note-headings'.

UNIXTIME is an integer representing the time to be used for all
timestamp nodes.

OLD-TIMESTAMP is a timestamp node of the deadline that is being
deleted. It will always be converted to an inactive timestamp.

If string NOTE is supplied, append a note to the log entry."
  (org-ml--build-log-item-trans 'redeadline unixtime old-timestamp note))

(defun org-ml-build-log-deldeadline (unixtime old-timestamp &optional note)
  "Return an item node for a delete deadline log entry.

This will format the log entry from the default value for the
`deldeadline' cell in `org-log-note-headings'.

UNIXTIME is an integer representing the time to be used for all
timestamp nodes.

OLD-TIMESTAMP is a timestamp node of the deadline that is being
deleted. It will always be converted to an inactive timestamp.

If string NOTE is supplied, append a note to the log entry."
  (org-ml--build-log-item-trans 'deldeadline unixtime old-timestamp note))

(defun org-ml-build-log-refile (unixtime &optional note)
  "Return an item node for a refile log entry.
This will format the log entry from the default value for the
`deldeadline' cell in `org-log-note-headings'.

UNIXTIME is an integer representing the time to be used for all
timestamp nodes.

If string NOTE is supplied, append a note to the log entry."
  (->> (org-ml--log-get 'refile)
       (org-ml--log-replace-timestamp unixtime nil t)
       (org-ml--build-log-item note)))

(org-ml--defun-kw org-ml-build-log-type (type &key old new unixtime username
                                              full-username note)
  "Return an item for an arbitrary log entry.

TYPE is a symbol corresponding to the car of one of the cells in
`org-log-note-headings'. Unlike the other log entry build functions
in this package, this function will not use the default value of
`org-log-note-headings' which means it can be used for customly
formatted log entries.

The arguments correspond to the following formatting placeholders
(see `org-log-note-headings' for more information on these
placeholders):
- NEW: either a string or timestamp node that will replace the
  new state/timestamp placeholder (%s)
- OLD: like NEW but for the old state/timestamp placeholder (%S)
- UNIXTIME: an integer corresponding to the time to be used for the
  timestamp placeholders (%t/%T/%d/%D)
- USERNAME: a string for the username (%u)
- FULL-USERNAME: a string for the full username (%U)

If any of these arguments are not supplied but their placeholders
are present in the heading determined by TYPE, the placeholders will
not be substituted.

If string NOTE is supplied, append a note to the log entry."
  ;; TODO this can likely be made faster (if desired) by not relying on the
  ;; individual replacement functions; doing it this way will call
  ;; `org-replace-escapes' multiple times, which is likely not as fast
  (cl-flet
      ((replace-note
        (old-p rep note)
        (if (not rep) note
          (let ((fun
                 (cond
                  ((org-ml--is-type 'timestamp rep)
                   (if old-p #'org-ml--log-replace-old-timestamp
                     #'org-ml--log-replace-new-timestamp))
                  ((stringp rep)
                   (if old-p #'org-ml--log-replace-old-state
                     #'org-ml--log-replace-new-state))
                  (t
                   (org-ml--arg-error "Must be string or timestamp: Got %S" rep)))))
            (funcall fun rep note))))
       (replace-timestamps
        (heading)
        (if (not unixtime) heading
          (->> heading
               (org-ml--log-replace-timestamp unixtime nil nil)
               (org-ml--log-replace-timestamp unixtime t nil)
               (org-ml--log-replace-timestamp unixtime nil t)
               (org-ml--log-replace-timestamp unixtime t t)))))
    (--> (alist-get type org-log-note-headings)
         (replace-timestamps it)
         (replace-note t old it)
         (replace-note nil new it)
         (if username (org-ml--log-replace-username username it) it)
         (if full-username
             (org-ml--log-replace-full-username full-username it)
           it)
         (org-ml--build-log-item note it))))

;;; PUBLIC TYPE FUNCTIONS

(defalias 'org-ml-get-type #'org-element-type
  "Return the type of NODE.")

(defun org-ml-is-type (type node)
  "Return t if the type of NODE is TYPE (a symbol)."
  (declare (pure t))
  (unless (memq type org-ml-nodes)
    (org-ml--arg-error "Argument 'type' must be in `org-ml-nodes': Got %s" type))
  (org-ml--is-type type node))

(defun org-ml-is-any-type (types node)
  "Return t if the type of NODE is in TYPES (a list of symbols)."
  (declare (pure t))
  (-some->>
   (-difference types org-ml-nodes)
   (org-ml--arg-error
    "All in 'types' must be in `org-ml-nodes'; these were not: %s"))
  (org-ml--is-any-type types node))

(defun org-ml-is-element (node)
  "Return t if NODE is an element class."
  (org-ml--is-any-type org-ml-elements node))

(defun org-ml-is-branch-node (node)
  "Return t if NODE is a branch node."
  (org-ml--is-any-type org-ml-branch-nodes node))

(defun org-ml-node-may-have-child-objects (node)
  "Return t if NODE is a branch node that may have child objects."
  (org-ml--is-any-type org-ml-branch-nodes-permitting-child-objects node))

(defun org-ml-node-may-have-child-elements (node)
  "Return t if NODE is a branch node that may have child elements.

Note this implies that NODE is also of class element since only
elements may have other elements as children."
  (org-ml--is-any-type org-ml-branch-elements-permitting-child-elements node))

;;; PUBLIC PROPERTY FUNCTIONS

;;; polymorphic

(defun org-ml-contains-point-p (point node)
  "Return t if POINT is within the boundaries of NODE."
  (-let ((b (org-element-begin node))
         (e (org-element-end node)))
    (if (and (integerp b) (integerp e))
        (<= b point e)
      (error "Node boundaries are not defined"))))

(defun org-ml--property-is-attribute (prop)
  "Return t if PROP is of the form :attr_X where X is anything."
  (and (keywordp prop) (s-prefix-p ":attr_" (symbol-name prop) t)))

(defun org-ml-set-property (prop value node)
  "Return NODE with PROP set to VALUE.

See builder functions for a list of properties and their rules for
each type."
  (let ((type (org-ml-get-type node)))
    ;; Specialized code to handle :attr_X properties which can't be put in
    ;; `org-ml--property-alist'. Values for these can only be lists of strings
    ;; and have no encoder or decoder.
    (cond
     ((and (memq type org-ml--element-nodes-with-affiliated)
           (org-ml--property-is-attribute prop))
      (if (org-ml--is-string-list value)
          (org-element-put-property-2 prop value node)
        (org-ml--arg-error "All attributes like '%s' must be a list of strings. Got '%S'"
                           prop value)))
     ((eq type 'plain-text)
      (if (eq prop :post-blank)
          (concat (s-trim-right node) (make-string value ?\ ))
        (org-add-props node nil prop value)))
     (t
      (let* ((value* (org-ml--property-encode prop value type))
             (node* (->> (if (eq type 'headline)
                             (org-element-properties-resolve node)
                           node)
                         (org-ml-copy)
                         (org-element-put-property-2 prop value*))))
        (-if-let (update-fun (org-ml--get-property-updater type prop))
            (funcall update-fun node*)
          node*))))))

(defun org-ml-set-properties (plist node)
  "Return NODE with all properties set to the values according to PLIST.

PLIST is a list of property-value pairs that corresponds to the
property list in NODE.

See builder functions for a list of properties and their rules for
each type."
  (cl-flet
      ((split-keyvals-maybe
        (type keyvals)
        (if (not (memq type org-ml--element-nodes-with-affiliated))
            (list keyvals nil)
          (--> keyvals
               (--group-by (org-ml--property-is-attribute (car it)) it)
               (list (alist-get nil it) (alist-get t it))))))
    (if (not (org-ml--is-plist plist))
        (org-ml--arg-error "Not a plist: %S" plist)
      (-let* ((type (org-ml-get-type node))
              ;; this will divide the keywords to those that are of the form
              ;; :attr_X which must be set differently
              ((kv kv-attrs) (split-keyvals-maybe type (-partition 2 plist)))
              (update-funs
               (->> (--map (org-ml--get-property-updater type (car it)) kv)
                    (-uniq)
                    (-non-nil)))
              (node* (org-ml-copy node)))
        (--each kv (->> (org-ml--property-encode (car it) (cadr it) type)
                        (org-element-put-property node* (car it))))
        (--each kv-attrs (org-element-put-property node* (car it) (cadr it)))
        (--each update-funs (funcall it node*))
        node*))))

(defun org-ml-get-property (prop node)
  "Return the value of PROP of NODE."
  (let ((type (org-ml-get-type node)))
    (if (and (eq type 'plain-text) (eq prop :post-blank))
        (org-ml--get-post-blank-text node)
      (let ((decoder-fun (unless (or (and (memq type org-ml--element-nodes-with-affiliated)
                                          (org-ml--property-is-attribute prop))
                                     (memq prop org-element--standard-properties))
                           (org-ml--get-property-decoder type prop)))
            (value (org-element-property prop node)))
        (if decoder-fun (funcall decoder-fun value) value)))))

(defun org-ml-get-properties (props node)
  "Return all the values of PROPS from NODE.
PROPS is a list of all the properties desired, and the returned
list will be the values of these properties in the order
requested. To get all properties of NODE, use
`org-ml--get-all-properties'."
  (--map (org-ml-get-property it node) props))

(org-ml--defun-anaphoric* org-ml-map-property (prop fun node)
  "Return NODE with FUN applied to the value of PROP.

FUN is a unary function which takes the current value of PROP and
returns a new value to which PROP will be set.

See builder functions for a list of properties and their rules for
each type."
  (--> (org-ml-get-property prop node)
       (org-ml-set-property prop (funcall fun it) node)))

(defun org-ml-map-properties (plist node)
  "Return NODE with functions applied to the values of properties.

PLIST is a property list where the keys are properties of NODE and
its values are unary functions to be mapped to these properties.

See builder functions for a list of properties and their rules for
each type."
  ;; TODO this is slow since it will copy the node for each property iteration
  (cond
   ((not plist) node)
   ((org-ml--is-plist plist)
    (->> (org-ml-map-property (nth 0 plist) (nth 1 plist) node)
         (org-ml-map-properties (-drop 2 plist))))
   (t (org-ml--arg-error "Not a plist: %s" plist))))

(defmacro org-ml-map-properties* (plist node)
  "Anaphoric form of `org-ml-map-properties'.

PLIST is a property list where the keys are properties of NODE and
its values are forms to be mapped to these properties."
  (declare (debug (form form)))
  (let ((p (make-symbol "plist*")))
    `(let ((,p (org-ml--plist-map-values (lambda (form) `(lambda (it) ,form)) ',plist)))
       (org-ml-map-properties ,p ,node))))

(defun org-ml-toggle-property (prop node)
  "Return NODE with the value of PROP flipped.

This function only applies to properties that are booleans."
  (let ((type (org-ml-get-type node)))
    (if (org-ml--property-memq org-ml--properties-with-toggle type prop)
        (org-ml-map-property prop #'not node)
      (org-ml--arg-error "Not a toggle-able property"))))

(defun org-ml-shift-property (prop n node)
  "Return NODE with PROP shifted by N (an integer).

This only applies the properties that are represented as integers."
  (let* ((type (org-ml-get-type node))
         (fun (org-ml--get-property-shifter type prop)))
    (if fun (org-ml-map-property* prop (funcall fun n it) node)
      (org-ml--arg-error "'%s' not a shiftable for '%s'" prop type))))

(defun org-ml-insert-into-property (prop index string node)
  "Return NODE with STRING inserted at INDEX into PROP.

This only applies to properties that are represented as lists of
strings."
  (let ((type (org-ml-get-type node)))
    (if (org-ml--property-memq org-ml--properties-with-string-list type prop)
        (org-ml-map-property* prop
          (if (member string it) it (org-ml--insert-at index string it))
          node)
      (org-ml--arg-error "Property '%s' in node of type '%s' is not a string-list"
                         prop type))))

(defun org-ml-remove-from-property (prop string node)
  "Return NODE with STRING removed from PROP if present.

This only applies to properties that are represented as lists of
strings.

See `org-ml-insert-into-property' for a list of supported elements
and properties that may be used with this function."
  (let ((type (org-ml-get-type node)))
    (if (org-ml--property-memq org-ml--properties-with-string-list type prop)
        (org-ml-map-property* prop (-remove-item string it) node)
      (org-ml--arg-error "Property '%s' in node of type '%s' is not a string-list"
                     prop type))))

(defun org-ml-plist-put-property (prop key value node)
  "Return NODE with VALUE corresponding to KEY inserted into PROP.

KEY is a keyword and VALUE is a symbol. This only applies to
properties that are represented as plists."
  (if (org-ml--property-memq org-ml--properties-with-plist (org-ml-get-type node) prop)
      (org-ml-map-property* prop (plist-put it key value) node)
    (org-ml--arg-error "Not a plist property")))

(defun org-ml-plist-remove-property (prop key node)
  "Return NODE with KEY and its corresponding value removed from PROP.

KEY is a keyword. This only applies to properties that are
represented as plists.

See `org-ml-plist-put-property' for a list of supported elements
and properties that may be used with this function."
  (if (org-ml--property-memq org-ml--properties-with-plist (org-ml-get-type node) prop)
      (org-ml-map-property* prop (org-ml--plist-remove key it) node)
    (org-ml--arg-error "Not a plist property")))

;; update polymorphic property function documentation:
;;
;; For the functions immediately above, modify the docstrings to inform the user
;; which node types and property combinations may be used. This information is
;; stored in `org-ml--property-alist'.

(defun org-ml--get-types-with-property-attribute (attr)
  "Return alist of all nodes types that contain ATTR.
Return a list like ((TYPE (PROP1 ...)) ...) where TYPE is the
node type and PROPX are the properties that contain ATTR."
  (->> org-ml--property-alist
       (--map (cons (car it) (--filter (plist-get (cdr it) attr) (cdr it))))
       (-filter #'cdr)))

(defun org-ml--format-alist-operations (type-alist)
  "Return a formatted string of TYPE-ALIST.
TYPE-ALIST is a list like that given by
`org-ml--format-alist-operations'."
  (->> type-alist
       (--map (cons (car it) (-map #'car (cdr it))))
       (--map (format "\n%s\n%s"
                      (car it)
                      (s-join "\n" (--map (format "- %S" it) (cdr it)))))
       (s-join "\n")))

(defun org-ml--append-documentation (fun string)
  "Append STRING to the docstring of FUN."
  (let ((msg "\n\nThe following types and properties are supported:\n")
        (doc (documentation fun)))
    ;; ensure we only update once, otherwise reloads will keep adding to the
    ;; docstrings
    (unless (s-contains? msg doc)
      (->> (concat doc msg string)
           (function-put fun 'function-documentation)))))

(->> (org-ml--get-types-with-property-attribute :toggle)
     (org-ml--format-alist-operations)
     (org-ml--append-documentation 'org-ml-toggle-property))

(->> (org-ml--get-types-with-property-attribute :shift)
     (--map (cons (car it) (--remove (eq :post-blank (car it)) (cdr it))))
     (-filter #'cdr)
     (org-ml--format-alist-operations)
     (concat "\nall elements\n- :post-blank\n")
     (org-ml--append-documentation 'org-ml-shift-property))

(->> (org-ml--get-types-with-property-attribute :string-list)
     (org-ml--format-alist-operations)
     (org-ml--append-documentation 'org-ml-insert-into-property))

(->> (org-ml--get-types-with-property-attribute :plist)
     (org-ml--format-alist-operations)
     (org-ml--append-documentation 'org-ml-plist-put-property))

(defun org-ml-get-parents (node)
  "Return parents of NODE as a list.
The toplevel parent will be the left-most member, and NODE itself
will be the rightmost member."
  (cl-labels
      ((get-parents
        (acc node)
        (if (or (null node) (eq 'org-data (car node))) acc
          (get-parents (cons node acc) (org-element-parent node)))))
    (get-parents nil node)))

(defun org-ml-remove-parent (node)
  "Return NODE with the :parent property set to nil.

Short synopsis:

Use this function to declutter a node if you are trying to print
its literal list representation or you are running into infinite
loops caused by self-referential lists (there are probably other
valid reasons but these are the main ones).

Gory details:

The :parent property refers to the node one level higher in the
tree that contains NODE as a child. It will be present in a node
that is generated from a parse operation with
`org-ml-parse-this-buffer' or related. This property offers a
nice shortcut to traverse up the node tree from a child. Besides
this, it is not necessary as the tree structure itself already
encodes all parent-child relationships. Further, it is not used
by org-element internally to convert nodes into strings (such as
with `org-ml-to-string') and thus can be thought of as a
\"read-only\" property. This is why :parent will be set to nil when
building a new node with the \"org-ml-build-\" family of functions
and why `org-ml-set-property' forbids setting this property.

In many cases, one can safely ignore :parent unless, of course,
one actually needs to read it with `org-ml-get-parents' or
`org-ml-get-property'. However, it heavily clutters the list
representation of nodes, and therefore it is nice to remove this
property whenever literal node lists are printed/visualized (eg
for debugging). Note that for deep trees, each parent will itself
have a :parent property pointing to its own parent, with this
pattern repeating until the top of the tree.

Furthermore, each parent will itself contain its own child node,
which implies a circular/self-referential list. For the most
part, this won't matter. However, some functions don't like
dealing with circular lists and will complain about infinite
recursion. If this is happening, the :parent property is likely
to blame, and setting it to nil has a high probability of fixing
the issue."
  (if (stringp node)
      (progn (remove-text-properties 0 (length node) '(:parent) node) node)
    (org-element-put-property-2 :parent nil node)))

(defun org-ml--caption-remove-parents (node)
  "Remove parents from CAPTION property in NODE if present."
  (cl-flet*
      ((remove-ss
        (ss)
        (-map #'org-ml-remove-parents ss))
       (remove-from-caption
        (caption)
        (pcase caption
          (`(,(pred org-ml--is-secondary-string))
           (list (remove-ss (car caption))))
          (`(,(pred org-ml--is-secondary-string)
             . ,(pred org-ml--is-secondary-string))
           (-let (((long . short) caption))
             (cons (remove-ss long) (remove-ss short))))
          ;; TODO error here?
          (_ caption))))
    (if (and (org-ml--is-any-type org-ml--element-nodes-with-affiliated node)
             (org-element-property-raw :caption node))
        (org-ml--map-property-raw* :caption
          (-map #'remove-from-caption it)
          node)
      node)))

(defun org-ml-remove-parents (node)
  "Like `org-ml-remove-parent' but for children of NODE as well.

See `org-ml-remove-parent' for why you might want this."
  (cl-flet*
      ((remove-recursive
        (nodes)
        (--map (org-ml-remove-parents it) nodes))
       (remove-within-prop
        (prop node)
        (org-ml--map-property-raw* prop
          (remove-recursive it)
          node)))
    (->>
     ;; remove parents from secondary strings (if necessary)
     (pcase (org-ml-get-type node)
       (`headline (->> (org-element-properties-resolve node)
                       (remove-within-prop :title)))
       (`item (remove-within-prop :tag node))
       (_ node))
     (org-ml--caption-remove-parents)
     (org-ml-remove-parent)
     (org-ml--map-children-nocheck* (remove-recursive it)))))

;;; object nodes
;;
;; entity

(defun org-ml-entity-get-replacement (key entity)
  "Return replacement string or symbol for ENTITY node.

KEY is one of:
- `:latex' (the entity's latex representation)
- `:latex-math-p' (t if the latex representation requires math mode,
  nil otherwise)
- `:html' (the entity's html representation)
- `:ascii' (the entity's ascii representation)
- `:latin1' (the entity's Latin1 representation)
- `:utf-8' (the entity's utf8 representation)

Any other keys will trigger an error."
  (org-ml--check-type 'entity entity)
  (-if-let (index (-elem-index key (list :latex :latex-math-p :html
                                         :ascii :latin1 :utf-8)))
      (->> (org-element-property-raw :name entity)
           (org-entity-get)
           (cdr)
           (nth index))
    (org-ml--arg-error "Invalid encoding requested: %s" index)))

;; statistics-cookie

(defun org-ml-statistics-cookie-is-complete (statistics-cookie)
  "Return t is STATISTICS-COOKIE node is complete."
  (org-ml--check-type 'statistics-cookie statistics-cookie)
  (let ((val (org-element-property-raw :value statistics-cookie)))
    (or (-some->>
         (s-match "\\([[:digit:]]+\\)%" val)
         (nth 1)
         (string-to-number)
         (= 100))
        (-some->>
         (s-match "\\([[:digit:]]+\\)/\\([[:digit:]]+\\)" val)
         (cdr)
         (-map #'string-to-number)
         (apply #'=)))))

;; timestamp (standard)

(defun org-ml-timestamp-get-start-time (timestamp)
  "Return the time list for start time of TIMESTAMP node.
The return value will be a list as specified by the TIME argument in
`org-ml-build-timestamp!'."
  (org-ml--check-type 'timestamp timestamp)
  (org-ml--timestamp-get-start-timelist timestamp))

(defun org-ml-timestamp-get-end-time (timestamp)
  "Return the end time list for end time of TIMESTAMP or nil if not a range.
The return value will be a list as specified by the TIME argument in
`org-ml-build-timestamp!'."
  (org-ml--check-type 'timestamp timestamp)
  (and (org-ml--timestamp-is-range-type timestamp)
       (org-ml--timestamp-get-end-timelist timestamp)))

(defun org-ml-timestamp-get-length (timestamp)
  "Return the length of TIMESTAMP node in seconds as an integer.
If non-ranged, this function will return 0. If ranged but
the start time is in the future relative to end the time, return
a negative integer."
  (org-ml--check-type 'timestamp timestamp)
  (org-ml--timestamp-get-length timestamp))

(defun org-ml-timestamp-is-active (timestamp)
  "Return t if TIMESTAMP node is active."
  (org-ml--check-type 'timestamp timestamp)
  (let ((y (org-element-property-raw :type timestamp)))
    (if (memq y '(active active-range)) t)))

(defun org-ml-timestamp-is-ranged (timestamp)
  "Return t if TIMESTAMP node is ranged."
  (org-ml--check-type 'timestamp timestamp)
  (let ((y (org-element-property-raw :type timestamp)))
    (if (memq y '(active-ranged inactive-range)) t)))

(defun org-ml-timestamp-range-contains-p (unixtime timestamp)
  "Return t if UNIXTIME is between start and end time of TIMESTAMP node.
The boundaries are inclusive. If TIMESTAMP has a range of zero, then
only return t if UNIXTIME is the same as TIMESTAMP. TIMESTAMP will be
interpreted according to the localtime of the operating system."
  (org-ml--check-type 'timestamp timestamp)
  (let ((ut1 (org-ml--timestamp-get-start-unixtime timestamp))
        (ut2 (org-ml--timestamp-get-end-unixtime timestamp)))
    (<= ut1 unixtime ut2)))

(defun org-ml-timestamp-set-start-time (time timestamp)
  "Return TIMESTAMP node with start time set to TIME.
TIME is a list analogous to the same argument specified in
`org-ml-build-timestamp!'."
  (org-ml--check-type 'timestamp timestamp)
  (org-ml--check-timelist-from-list time)
  (org-ml--timestamp-set-start-timelist time (org-ml-copy timestamp)))

(defun org-ml-timestamp-set-end-time (time timestamp)
  "Return TIMESTAMP node with end time set to TIME.
TIME is a list analogous to the same argument specified in
`org-ml-build-timestamp!'."
  (org-ml--check-type 'timestamp timestamp)
  (when time
    (org-ml--check-timelist-from-list time))
  (org-ml--timestamp-set-end-timelist time (org-ml-copy timestamp)))

(defun org-ml-timestamp-set-single-time (time timestamp)
  "Return TIMESTAMP node with start and end times set to TIME.
TIME is a list analogous to the same argument specified in
`org-ml-build-timestamp!'."
  (org-ml--check-type 'timestamp timestamp)
  (org-ml--check-timelist-from-list time)
  (org-ml--timestamp-set-single-timelist time (org-ml-copy timestamp)))

(defun org-ml-timestamp-set-double-time (time1 time2 timestamp)
  "Return TIMESTAMP node with start/end times set to TIME1/TIME2 respectively.
TIME1 and TIME2 are lists analogous to the TIME argument specified in
`org-ml-build-timestamp!'."
  (org-ml--check-type 'timestamp timestamp)
  (org-ml--check-timelist-from-list time1)
  (org-ml--check-timelist-from-list time2)
  (org-ml--timestamp-set-double-timelist time1 time2 (org-ml-copy timestamp)))

(defun org-ml-timestamp-set-length (n unit timestamp)
  "Return TIMESTAMP node with length set to N UNITs.
If TIMESTAMP is ranged, keep start time the same and adjust the end
time. If not, make a new end time. The units for RANGE are in minutes
if TIMESTAMP is in long format and days if TIMESTAMP is in short
format."
  (org-ml--check-type 'timestamp timestamp)
  ;; ASSUME unit will be checked internally
  (org-ml--timestamp-set-length n unit (org-ml-copy timestamp)))

(defun org-ml-timestamp-set-active (flag timestamp)
  "Return TIMESTAMP node with active type if FLAG is t."
  (org-ml--check-type 'timestamp timestamp)
  (org-ml--timestamp-set-active flag (org-ml-copy timestamp)))

(defun org-ml-timestamp-shift (n unit timestamp)
  "Return TIMESTAMP node with time shifted by N UNIT's.

This function will move the start and end times together; therefore
ranged inputs will always output ranged timestamps and same for
non-ranged. To move the start and end time independently, use
`org-ml-timestamp-shift-start' or `org-ml-timestamp-shift-end'.

N is a positive or negative integer and UNIT is one of `minute',
`hour', `day', `month', or `year'. Overflows will wrap around
transparently; for instance, supplying `minute' for UNIT and 90 for N
will increase the hour property by 1 and the minute property by 30."
  ;; ASSUME unit will be checked internally
  ;;
  ;; if not ranged, simply need to shift start and end (which are the same);
  ;; otherwise need to shift both, set both, and update the timerange depending
  ;; on if we straddle a day boundary after the shift
  (org-ml--check-type 'timestamp timestamp)
  (let ((rt (org-element-property-raw :range-type timestamp))
        (timestamp* (org-ml-copy timestamp)))
    (if (not rt)
        (let ((t1 (->> (org-ml--timestamp-get-start-timelist timestamp)
                       (org-ml-timelist-shift n unit))))
          (->> (org-ml--timestamp-set-start-timelist t1 timestamp*)
               (org-ml--timestamp-set-end-timelist t1)))
      (-let* ((s1 (->> (org-ml--timestamp-get-start-timelist timestamp)
                       (org-ml-timelist-shift n unit)))
              (s2 (->> (org-ml--timestamp-get-end-timelist timestamp)
                       (org-ml-timelist-shift n unit)))
              ;; total micro-optimization...
              ((d1 t1) (org-ml-timelist-split s1))
              ((d2 t2) (org-ml-timelist-split s2))
              (rt* (if (and (not (equal t1 t2)) (equal d1 d2))
                       (or rt 'timerange)
                     'daterange)))
        (->> (org-ml--timestamp-set-start-timelist s1 timestamp*)
             (org-ml--timestamp-set-end-timelist s2)
             (org-ml--timestamp-set-range-type rt*))))))

(defun org-ml-timestamp-shift-start (n unit timestamp)
  "Return TIMESTAMP node with start time shifted by N UNIT's.

N and UNIT behave the same as those in `org-ml-timestamp-shift'.

If TIMESTAMP is not range, the output will be a ranged timestamp with
the shifted start time and the end time as that of TIMESTAMP. If this
behavior is not desired, use `org-ml-timestamp-shift'."
  ;; ASSUME unit will be checked internally
  (org-ml--check-type 'timestamp timestamp)
  (let* ((t1 (->> (org-ml--timestamp-get-start-timelist timestamp)
                  (org-ml-timelist-shift n unit)))
         (t2 (org-ml--timestamp-get-end-timelist timestamp))
         (rt (->> (org-element-property-raw :range-type timestamp)
                  (org-ml--timelists-get-range-type t1 t2))))
    (->> (org-ml-copy timestamp)
         (org-ml--timestamp-set-start-timelist t1)
         (org-ml--timestamp-set-range-type rt))))

(defun org-ml-timestamp-shift-end (n unit timestamp)
  "Return TIMESTAMP node with end time shifted by N UNIT's.

N and UNIT behave the same as those in `org-ml-timestamp-shift'.

If TIMESTAMP is not range, the output will be a ranged timestamp with
the shifted end time and the start time as that of TIMESTAMP. If this
behavior is not desired, use `org-ml-timestamp-shift'."
  ;; ASSUME unit will be checked internally
  (org-ml--check-type 'timestamp timestamp)
  (let* ((t1 (org-ml--timestamp-get-start-timelist timestamp))
         (t2 (->> (org-ml--timestamp-get-end-timelist timestamp)
                  (org-ml-timelist-shift n unit)))
         (rt (->> (org-element-property-raw :range-type timestamp)
                  (org-ml--timelists-get-range-type t1 t2))))
    (->> (org-ml-copy timestamp)
         (org-ml--timestamp-set-end-timelist t2)
         (org-ml--timestamp-set-range-type rt))))

(defun org-ml-timestamp-toggle-active (timestamp)
  "Return TIMESTAMP node with its active/inactive type flipped."
  (org-ml--check-type 'timestamp timestamp)
  (-> (org-ml--timestamp-is-active timestamp)
      (not)
      (org-ml--timestamp-set-active (org-ml-copy timestamp))))

(defun org-ml-timestamp-truncate (timestamp)
  "Return TIMESTAMP node with start/end times forced to short format."
  (org-ml--check-type 'timestamp timestamp)
  (let* ((t1 (->> (org-ml--timestamp-get-start-timelist timestamp)
                  (org-ml--timelist-truncate)))
         (t2 (->> (org-ml--timestamp-get-end-timelist timestamp)
                  (org-ml--timelist-truncate)))
         ;; NOTE it is impossible for range-type to be 'timerange since hours
         ;; and minutes will be missing
         (rt (if (equal t1 t2) nil 'daterange)))
    (->> (org-ml-copy timestamp)
         (org-ml--timestamp-set-start-timelist t1)
         (org-ml--timestamp-set-end-timelist t2)
         (org-ml--timestamp-set-range-type rt))))

(defun org-ml-timestamp-truncate-start (timestamp)
  "Return TIMESTAMP node with start time forced to short format.

Collapsed timestamps will become uncollapsed."
  (org-ml--check-type 'timestamp timestamp)
  (let* ((t1 (->> (org-ml--timestamp-get-start-timelist timestamp)
                  (org-ml--timelist-truncate)))
         (t2 (->> (org-ml--timestamp-get-end-timelist timestamp)
                  (org-ml--timelist-truncate)))
         (rt (if (equal t1 t2) nil 'daterange)))
    (->> (org-ml-copy timestamp)
         (org-ml--timestamp-set-start-timelist t1)
         (org-ml--timestamp-set-range-type rt))))

(defun org-ml-timestamp-truncate-end (timestamp)
  "Return TIMESTAMP node with end time forced to short format.

Collapsed timestamps will become uncollapsed."
  (org-ml--check-type 'timestamp timestamp)
  (let* ((t1 (->> (org-ml--timestamp-get-start-timelist timestamp)
                  (org-ml--timelist-truncate)))
         (t2 (->> (org-ml--timestamp-get-end-timelist timestamp)
                  (org-ml--timelist-truncate)))
         (rt (if (equal t1 t2) nil 'daterange)))
    (->> (org-ml-copy timestamp)
         (org-ml--timestamp-set-end-timelist t2)
         (org-ml--timestamp-set-range-type rt))))

(defun org-ml-timestamp-set-collapsed (flag timestamp)
  "Return TIMESTAMP with collapsed set to FLAG.

Collapsed timestamps are like [yyyy-mm-dd xxx hh:mm-hh:mm].

Uncollapsed timestamp are like
[yyyy-mm-dd xxx hh:mm]--[yyyy-mm-dd xxx hh:mm].

FLAG may be one of nil, t, or `force'.

If nil, uncollapse the timestamp if it is collapsed. The dates in
the uncollapsed timestamp will be the same. Has no effect if the
timestamp is not collapsed.

If t, collapse the timestamp from uncollapsed format if the following
conditions are met:
1. the dates are the same
2. start and end hours/minutes are non-nil

Has no effect if timestamp id not uncollapsed and these
conditions are not met.

If `force', ignore condition 1 above. The date in the collapsed
timestamp will be taken from the start date and the end date will
be ignored."
  (org-ml--check-type 'timestamp timestamp)
  (org-ml--timestamp-set-collapsed flag timestamp))

(defun org-ml-timestamp-get-warning (timestamp)
  "Return the warning component of TIMESTAMP.
Return a list like (TYPE VALUE UNIT)."
  (org-ml--check-type 'timestamp timestamp)
  (-let (((&plist :warning-type y
                  :warning-value v
                  :warning-unit u)
          (org-ml--get-nonstandard-properties timestamp)))
    (when (and y v u) `(,y ,v ,u))))

(defun org-ml-timestamp-set-warning (warning timestamp)
  "Set the warning of TIMESTAMP to WARNING.

WARNING is a list like (TYPE VALUE UNIT). TYPE is `all' or
`first' VALUE and is an integer. UNIT is one of `year', `month',
`week', or `day'."
  (org-ml--check-type 'timestamp timestamp)
  (when warning
    (org-ml--check-warning-from-list warning))
  (->> (org-ml-copy timestamp)
       (org-ml--timestamp-set-warning warning)))

(org-ml--defun-anaphoric* org-ml-timestamp-map-warning (fun timestamp)
  "Apply FUN to the warning of TIMESTAMP.
FUN is a function that takes a warning list like and returns a
new warning list. The same rules that apply to
`org-ml-timestamp-set-warning' and `org-ml-timestamp-get-warning'
apply here."
  ;; TODO this will check node type twice
  (let ((w (org-ml-timestamp-get-warning timestamp)))
    (org-ml-timestamp-set-warning (funcall fun w) timestamp)))

(defun org-ml-timestamp-get-repeater (timestamp)
  "Return the repeater component of TIMESTAMP.
Return a list like (TYPE VALUE UNIT) or nil."
  (org-ml--check-type 'timestamp timestamp)
  (org-ml--timestamp-get-repeater timestamp))

(defun org-ml-timestamp-set-repeater (repeater timestamp)
  "Set the repeater of TIMESTAMP to REPEATER.

REPEATER is a list like (TYPE VALUE UNIT); TYPE is one of
`cumulate', `restart', or `catch-up'. VALUE is an integer. UNIT
is one of `year', `month', `week', or `day'.

Setting REPEATER to nil will remove the repeater and its deadline
if present."
  (org-ml--check-type 'timestamp timestamp)
  (when repeater
    (org-ml--check-repeater-from-list repeater))
  (->> (org-ml-copy timestamp)
       (org-ml--timestamp-set-repeater repeater)))

(org-ml--defun-anaphoric* org-ml-timestamp-map-repeater (fun timestamp)
  "Apply FUN to the warning of TIMESTAMP.
FUN is a function that takes a repeater list like and returns a
new repeater list. The same rules that apply to
`org-ml-timestamp-set-repeater' and
`org-ml-timestamp-get-repeater' apply here."
  (let ((r (org-ml-timestamp-get-repeater timestamp)))
    (org-ml-timestamp-set-repeater (funcall fun r) timestamp)))

(defun org-ml-timestamp-get-deadline (timestamp)
  "Return the repeater component of TIMESTAMP.
Return a list like (VALUE UNIT) or nil."
  (org-ml--check-type 'timestamp timestamp)
  (-let (((&plist :repeater-deadline-value dv
                  :repeater-deadline-unit du)
          (org-ml--get-nonstandard-properties timestamp)))
    (when (and dv du) (list dv du))))

(defun org-ml-timestamp-set-deadline (deadline timestamp)
  "Set the repeater of TIMESTAMP to DEADLINE.

DEADLINE is a list like (VALUE UNIT); VALUE is an integer. UNIT
is one of `year', `month', `week', or `day'.

Setting DEADLINE to nil will remove the deadline. Will have no effect
if repeater is not present."
  (org-ml--check-type 'timestamp timestamp)
  (when deadline
    (org-ml--check-deadline-from-list deadline))
  (->> (org-ml-copy timestamp)
       (org-ml--timestamp-set-deadline deadline)))

(org-ml--defun-anaphoric* org-ml-timestamp-map-deadline (fun timestamp)
  "Apply FUN to the deadline of TIMESTAMP.
FUN is a function that takes a repeater list like and returns a
new repeater list. The same rules that apply to
`org-ml-timestamp-set-deadline' and
`org-ml-timestamp-get-deadline' apply here."
  (let ((d (org-ml-timestamp-get-deadline timestamp)))
    (org-ml-timestamp-set-deadline (funcall fun d) timestamp)))

;; timestamp (diary)

(defun org-ml-timestamp-diary-set-value (form timestamp-diary)
  "Return TIMESTAMP-DIARY node with value set to FORM.
The node must have a type `eq' to `diary'. FORM is a quoted list."
  (org-ml--check-type 'timestamp timestamp-diary)
  (if (listp form)
      (->> (org-ml-copy timestamp-diary)
           (org-element-put-property-2 :raw-value (format "<%%%%%S>" form))
           (org-element-put-property-2 :diary-sexp (format "%S" form)))
    (org-ml--arg-error "Timestamp-diary node value must be a form: Got %S" form)))

(defun org-ml-timestamp-diary-get-start-time (timestamp-diary)
  "Return start time for  TIMESTAMP-DIARY or nil."
  (org-ml--check-type 'timestamp timestamp-diary)
  (org-ml--timestamp-get-start-time timestamp-diary))

(defun org-ml-timestamp-diary-get-end-time (timestamp-diary)
  "Return end time for  TIMESTAMP-DIARY or nil."
  (org-ml--check-type 'timestamp timestamp-diary)
  (org-ml--timestamp-get-end-time timestamp-diary))

(defun org-ml-timestamp-diary-set-single-time (time timestamp-diary)
  "Return TIMESTAMP-DIARY node with start/end time set to TIME.
The node must have a type `eq' to `diary'. TIME is a list
like (hour min). If TIME is nil remove the time."
  (org-ml--check-type 'timestamp timestamp-diary)
  (when time
    (org-ml--check-time-from-list time))
  (->> (org-ml-copy timestamp-diary)
       (org-ml--timestamp-set-start-time time)
       (org-ml--timestamp-set-end-time time)
       (org-ml--timestamp-update-type-ranged-timeonly)))

(defun org-ml-timestamp-diary-set-start-time (time timestamp-diary)
  "Return TIMESTAMP-DIARY node with start time set to TIME.
The node must have a type `eq' to `diary'. TIME is a list
like (hour min). TIME may not be nil"
  (org-ml--check-type 'timestamp timestamp-diary)
  (org-ml--check-time-from-list time)
  (let* ((start (org-ml--timestamp-get-start-time timestamp-diary))
         (end (or (org-ml--timestamp-get-end-time timestamp-diary) start time)))
    (->> (org-ml-copy timestamp-diary)
         (org-ml--timestamp-set-start-time time)
         (org-ml--timestamp-set-end-time end)
         (org-ml--timestamp-update-type-ranged-timeonly))))

(defun org-ml-timestamp-diary-set-end-time (time timestamp-diary)
  "Return TIMESTAMP-DIARY node with end time set to TIME.
The node must have a type `eq' to `diary'. TIME is a list
like (hour min). If TIME is nil then remove the end time.
If start time is not set, return node unchanged."
  (org-ml--check-type 'timestamp timestamp-diary)
  (when time
    (org-ml--check-time-from-list time))
  (let ((start (org-ml--timestamp-get-start-time timestamp-diary)))
    (if (not start) timestamp-diary
      (->> (org-ml-copy timestamp-diary)
           (org-ml--timestamp-set-end-time (or time start))
           (org-ml--timestamp-update-type-ranged-timeonly)))))

(defun org-ml-timestamp-diary-set-double-time (time1 time2 timestamp-diary)
  "Return TIMESTAMP-DIARY node with time set to TIME1 and TIME2.
The node must have a type `eq' to `diary'. TIME1 and TIME2 are
lists like (hour min). Either time may be nil, but if TIME1 is nil
then TIME2 must also be nil."
  (org-ml--check-type 'timestamp timestamp-diary)
  (when (and (not time1) time2)
    (org-ml--arg-error "Time1 cannot be nil if Time2 is non-nil"))
  (when time1
    (org-ml--check-time-from-list time1))
  (when time2
    (org-ml--check-time-from-list time2))
  (->> (org-ml-copy timestamp-diary)
       (org-ml--timestamp-set-start-time time1)
       (org-ml--timestamp-set-end-time (or time2 time1))
       (org-ml--timestamp-update-type-ranged-timeonly)))

(defun org-ml-timestamp-diary-set-length (n unit timestamp-diary)
  "Return TIMESTAMP-DIARY node with range set to N UNITs.
If TIMESTAMP-DIARY is ranged, keep start time the same and adjust
the end time. If not, make a new end time."
  (org-ml--check-type 'timestamp timestamp-diary)
  (-if-let (start (org-ml--timestamp-get-start-time timestamp-diary))
      (let ((s (org-ml--time-shift n unit start)))
        (->> (org-ml-copy timestamp-diary)
             (org-ml--timestamp-set-end-time s)
             (org-ml--timestamp-update-type-ranged-timeonly)))
    timestamp-diary))

(defun org-ml-timestamp-diary-shift (n unit timestamp-diary)
  "Return TIMESTAMP-DIARY node with time shifted by N UNITs.

This function will move the start and end times together;
therefore ranged inputs will always output ranged timestamps and
same for non-ranged. To move the start and end time
independently, use `org-ml-timestamp-diary-shift-start' or
`org-ml-timestamp-shift-end'.

N is a positive or negative integer and UNIT is one of `minute',
`hour', `day', `month', or `year'. Overflows will wrap around
transparently; for instance, supplying `minute' for UNIT and 90
for N will increase the hour property by 1 and the minute
property by 30."
  (org-ml--check-type 'timestamp timestamp-diary)
  (-if-let (start (org-ml--timestamp-get-start-time timestamp-diary))
      ;; 'or' to guard against nil end time when start is set, which is not
      ;; supposed to happen (but might)
      (let* ((end (or (org-ml--timestamp-get-end-time timestamp-diary) start))
             (start* (org-ml--time-shift n unit start))
             (end* (org-ml--time-shift n unit end)))
        (->> (org-ml-copy timestamp-diary)
             (org-ml--timestamp-set-start-time start*)
             (org-ml--timestamp-set-end-time end*)
             ;; update this in case range is in undefined state
             (org-ml--timestamp-update-type-ranged-timeonly)))
    timestamp-diary))

(defun org-ml-timestamp-diary-shift-start (n unit timestamp-diary)
  "Return TIMESTAMP-DIARY node with start time shifted by N UNITs.

N and UNIT behave the same as those in `org-ml-timestamp-diary-shift'.

If TIMESTAMP-DIARY is not range, the output will be a ranged timestamp with
the shifted start time and the end time as that of TIMESTAMP-DIARY. If this
behavior is not desired, use `org-ml-timestamp-diary-shift'."
  (org-ml--check-type 'timestamp timestamp-diary)
  (-if-let (start (org-ml--timestamp-get-start-time timestamp-diary))
      (let ((end (or (org-ml--timestamp-get-end-time timestamp-diary) start))
            (start* (org-ml--time-shift n unit start)))
        (->> (org-ml-copy timestamp-diary)
             (org-ml--timestamp-set-start-time start*)
             (org-ml--timestamp-set-end-time end)
             (org-ml--timestamp-update-type-ranged-timeonly)))
    timestamp-diary))

(defun org-ml-timestamp-diary-shift-end (n unit timestamp-diary)
  "Return TIMESTAMP-DIARY node with end time shifted by N UNITs.

N and UNIT behave the same as those in `org-ml-timestamp-diary-shift'.

If TIMESTAMP-DIARY is not range, the output will be a ranged timestamp with
the shifted end time and the start time as that of TIMESTAMP-DIARY. If this
behavior is not desired, use `org-ml-timestamp-diary-shift'."
  (org-ml--check-type 'timestamp timestamp-diary)
  (-if-let (start (org-ml--timestamp-get-start-time timestamp-diary))
      (let* ((end (or (org-ml--timestamp-get-end-time timestamp-diary) start)))
        (-> (org-ml--time-shift n unit end)
            (org-ml--timestamp-set-end-time (org-ml-copy timestamp-diary))
            (org-ml--timestamp-update-type-ranged-timeonly)))
    timestamp-diary))

;;; element nodes
;;
;; clock

(defun org-ml-clock-is-running (clock)
  "Return t if CLOCK element is running (eg is open)."
  (org-ml--check-type 'clock clock)
  (org-ml--property-is-eq :status 'running clock))

;; headline

(defun org-ml-headline-get-statistics-cookie (headline)
  "Return the statistics cookie node from HEADLINE if it exists."
  (org-ml--check-type 'headline headline)
  (->> (org-element-property :title headline)
       (-last-item)
       (org-ml--filter-type 'statistics-cookie)))

(defun org-ml-headline-is-done (headline)
  "Return t if HEADLINE node has a done todo-keyword."
  (org-ml--check-type 'headline headline)
  (-> (org-element-property :todo-keyword headline)
      (member org-done-keywords)
      (and t)))

(defun org-ml-headline-has-tag (tag headline)
  "Return t if HEADLINE node is tagged with TAG."
  (org-ml--check-type 'headline headline)
  (if (member tag (org-element-property :tags headline)) t))

(defun org-ml-headline-set-title! (title-text stats-cookie-value headline)
  "Return HEADLINE node with new title.

TITLE-TEXT is a string to be parsed into object nodes for the title
via `org-ml-build-secondary-string!' (see that function for restrictions)
and STATS-COOKIE-VALUE is a list described in
`org-ml-build-statistics-cookie'."
  (org-ml--check-type 'headline headline)
  (let ((ss (org-ml-build-secondary-string! title-text)))
    (if (not stats-cookie-value)
        (org-ml-set-property :title ss headline)
      (let ((ss* (org-ml--set-last-post-blank 1 ss))
            (sc (org-ml-build-statistics-cookie stats-cookie-value)))
        (org-ml-set-property :title (-snoc ss* sc) headline)))))

;; item

(defun org-ml-item-toggle-checkbox (item)
  "Return ITEM node with its checkbox state flipped.
This only affects item nodes with checkboxes in the `on' or `off'
states; return ITEM node unchanged if the checkbox property is `trans'
or nil."
  (org-ml--check-type 'item item)
  (pcase (org-element-property-raw :checkbox item)
    ('on (org-element-put-property-2 :checkbox 'off (org-ml-copy item)))
    ('off (org-element-put-property-2 :checkbox 'on (org-ml-copy item)))
    ((or `trans `nil) item)
    (_ (error "This should not happen"))))

;;; PUBLIC BRANCH/CHILD FUNCTIONS

;;; polymorphic

(defun org-ml-children-contain-point (point branch-node)
  "Return t if POINT is within the boundaries of BRANCH-NODE's children."
  (org-ml--check-types org-ml-branch-nodes branch-node)
  (-let ((b (org-element-contents-begin branch-node))
         (e (org-element-contents-end branch-node)))
    (<= b point e)))

(defun org-ml-get-children (branch-node)
  "Return the children of BRANCH-NODE as a list."
  (org-ml--check-types org-ml-branch-nodes branch-node)
  (org-element-contents branch-node))

(defun org-ml-set-children (children branch-node)
  "Return BRANCH-NODE with its children set to CHILDREN.
CHILDREN is a list of nodes; the types permitted in this list depend
on the type of NODE."
  (let ((type (org-ml-get-type branch-node)))
    (-if-let (child-types (alist-get type org-ml--node-restrictions))
        (-if-let (illegal (-difference (-map #'org-ml-get-type children)
                                       child-types))
            (org-ml--set-children-throw-error type child-types illegal)
          (org-ml--set-children-nocheck children branch-node))
      ;; this should not happen
      (error "Child type restrictions not found for %s" type))))

(org-ml--defun-anaphoric* org-ml-map-children (fun branch-node)
  "Return BRANCH-NODE with FUN applied to its children.
FUN is a unary function that takes the current list of children and
returns a modified list of children."
  (--> (org-ml-get-children branch-node)
       (org-ml-set-children (funcall fun it) branch-node)))

(defun org-ml-is-childless (branch-node)
  "Return t if BRANCH-NODE has no children."
  (not (org-ml-get-children branch-node)))

;;; objects

(defun org-ml--normalize-secondary-string (secondary-string)
  "Return SECONDARY-STRING with all adjacent strings concatenated."
  (cl-flet
      ((concat-maybe
        (acc node)
        (let ((last (car acc)))
          (if (and (org-ml--is-type 'plain-text last)
                   (org-ml--is-type 'plain-text node))
              (cons (concat last node) (cdr acc))
            (cons node acc)))))
    (reverse (-reduce-from #'concat-maybe nil secondary-string))))

(eval-when-compile
  (defmacro org-ml--mapcat-normalize (form secondary-string)
    "Return mapped, concatenated, and normalized SECONDARY-STRING.
FORM is a form supplied to `--mapcat'."
    (declare (debug (def-form form)))
    `(->> (--map ,form ,secondary-string)
          (apply #'append)
          (org-ml--normalize-secondary-string))))

(defun org-ml-unwrap (object-node)
  "Return the children of OBJECT-NODE as a secondary string.
If OBJECT-NODE is a plain-text node, wrap it in a list and return.
Else add the post-blank property of OBJECT-NODE to the last member
of its children and return children as a secondary string."
  (org-ml--check-types org-ml-objects object-node)
  (if (org-ml--is-type 'plain-text object-node)
      (list object-node)
    (let ((post-blank (org-ml--get-post-blank-textsafe object-node)))
      (->> (org-ml-copy object-node t)
           (org-element-contents)
           (org-ml--map-last*
            (org-ml--shift-post-blank-textsafe post-blank it))))))

(defun org-ml-unwrap-types-deep (types object-node)
  "Return the children of OBJECT-NODE as a secondary string.
If OBJECT-NODE is a plain-text node, wrap it in a list and return.
Else recursively descend into the children of OBJECT-NODE and splice
the children of nodes with type in TYPES in place of said node and
return the result as a secondary string."
  ;; TODO this will check for object nodes in nested levels which is redundant
  (org-ml--check-types org-ml-objects object-node)
  (cond
   ((org-ml--is-type 'plain-text object-node)
    (list object-node))
   ((org-ml-is-any-type types object-node)
    (let ((post-blank (org-ml--get-post-blank-textsafe object-node)))
      (->> (org-element-contents object-node)
           (org-ml--mapcat-normalize
            (->> (org-ml-copy it)
                 (org-ml-unwrap-types-deep types)))
           (org-ml--map-last* (org-ml--shift-post-blank-textsafe post-blank it)))))
   (t
    (->> object-node
         (org-ml-map-children*
           (org-ml--mapcat-normalize (org-ml-unwrap-types-deep types it) it))
         (list)))))

(defun org-ml-unwrap-deep (object-node)
  "Return the children of OBJECT-NODE as plain-text wrapped in a list."
  (org-ml-unwrap-types-deep org-ml-nodes object-node))

;;; secondary strings

(defun org-ml-flatten (secondary-string)
  "Return SECONDARY-STRING with its first level unwrapped.
The unwrap operation will be done with `org-ml-unwrap'."
  (org-ml--mapcat-normalize (org-ml-unwrap it) secondary-string))

(defun org-ml-flatten-types-deep (types secondary-string)
  "Return SECONDARY-STRING with object nodes in TYPES unwrapped.
The unwrap operation will be done with `org-ml-unwrap-types-deep'."
  (org-ml--mapcat-normalize (org-ml-unwrap-types-deep types it) secondary-string))

(defun org-ml-flatten-deep (secondary-string)
  "Return SECONDARY-STRING with all object nodes unwrapped to plain-text.
The unwrap operation will be done with `org-ml-unwrap-deep'."
  (org-ml--mapcat-normalize (org-ml-unwrap-deep it) secondary-string))

;;; item

(defun org-ml--append-join-plain-lists (nodes1 nodes2)
  "Append NODES1 and NODES2 into one list.
If the last node in NODES1 and the first node in NODES2 are
plain-lists, join the two lists together."
  (let ((last (-last-item nodes1))
        (first (car nodes2)))
    (if (and (org-ml--is-type 'plain-list last)
             (org-ml--is-type 'plain-list first))
        (let ((pb (org-element-post-blank last)))
          (--> (org-element-contents last)
               (org-ml--set-last-post-blank pb it)
               (append it (org-element-contents first))
               (org-ml--set-children-nocheck it last)
               (cons it (cdr nodes2))
               (append (-drop-last 1 nodes1) it)))
      (append nodes1 nodes2))))

(defun org-ml-item-get-paragraph (item)
  "Return the first paragraph's children of ITEM or nil if none."
  (org-ml--check-type 'item item)
  (-when-let (first-child (car (org-element-contents item)))
    (when (org-ml--is-type 'paragraph first-child)
      (org-element-contents first-child))))

(defun org-ml-item-set-paragraph (secondary-string item)
  "Set the first paragraph's children of ITEM to SECONDARY-STRING."
  (org-ml--check-type 'item item)
  (org-ml-map-children*
    (if (org-ml--is-type 'paragraph (car it))
        (if (not secondary-string) (cdr it)
          (cons (org-ml-set-children secondary-string (car it)) (cdr it)))
      (cons (apply #'org-ml-build-paragraph secondary-string) it))
    item))

(org-ml--defun-anaphoric* org-ml-item-map-paragraph (fun item)
  "Apply FUN to the first paragraph's children in ITEM.
FUN is a UNARY function that takes the secondary-string of the
first paragraph and returns modified secondary-string."
  (--> (org-ml-item-get-paragraph item)
       (org-ml-item-set-paragraph (funcall fun it) item)))

;;; headline (supercontents)

;; Everything under a headline in the "section" should follow a predictable
;; structure. The planning and property-drawer nodes is always first and second
;; respectively (if present) followed by a "logbook" and the "contents" The
;; "logbook" contains two types of nodes, here called "log items" (or sometimes
;; simply "items" if the context is obvious) and "clocks." The former include
;; any plain-list/item node as given by `org-log-note-headings' (except for
;; 'clock-out' which applies only to clocks), and clocks includes clock nodes
;; and optionally plain-list/item nodes that represent the clock-out notes.
;; Anything that comes after the logbook is deemed "contents." To make this even
;; more complicated, the spacing after these nodes potentially interacts the
;; encapsulating headline itself through the :pre-blank property. For instance,
;; if a planning node has a non-zero :post-blank property, this value should be
;; set to the :pre-blank property of the headline if the planning node is
;; deleted (indicating that the space "moves up").

;; To simplify this entire process, here we introduce an abstraction layer
;; called the "supercontents" which will encompass the entire headline section
;; and the :pre-blank property of the headline itself. This will represent all
;; these components in a standardized way. A similar data structure for
;; "logbook" also exists within the supercontents for similar reasons, as the
;; logbook has many different complex representations.

;; In haskell types, the supercontents and logbook are like this:

;; type Blank = Natural
;;
;; type Property = (Text, Text)
;;
;; data LogBook = LogBook
;;   { clocks :: [ClockNode]
;;   , items :: [ItemNode]
;;   , unknown :: [Node]
;;   }
;;
;; data SuperContents = SuperContents
;;   { planning :: (Maybe Planning)
;;   , node-properties :: [Property]
;;   , logbook :: LogBook
;;   , blank :: Blank
;;   , contents :: [Nodes]
;;   }

;; There is one *very important* assumption built into this data structure. If
;; the planning, property-drawer, or logbook are present, there must not be any
;; spaces between them the nor can there be a space between the headline and the
;; first node. By extension this means :pre-blank (in the encapsulating
;; headline) must be 0 if planning, property-drawer, or logbook are present. In
;; this case, "blank" represents the first blank after any of these three
;; components and the first node after (the start of the "contents"). If
;; planning, property-drawer, or logbook are not present, "blank" is the same as
;; :pre-blank. The advantage of this setup is that this annoying whitespace
;; doesn't need to be transferred to different nodes as the headline section is
;; edited.

;; The "logbook" requires its own special attention, since it could be several
;; different things depending on configuration. This is controlled by
;; `org-log-into-drawer', `org-clock-into-drawer', and `org-log-note-clock-out'.
;; These roughly control when the log items and/or clocks are in a named drawer
;; or "loose" by themselves.
;; 
;; Aside from these variables, the logbook will be defined as follows:
;; - a continuous string of multi-line text after the headline metadata (in
;;   other words, the logbook cannot contain consecutive newlines)
;; - any items that are parsed as log items must conform to
;;   `org-log-note-headines', which (for now) means they must end with a
;;   timestamp on the first line (in the future, this will be extended since it
;;   is possible to modify `org-log-note-headings', even if it is not the best
;;   idea). Note this also implies that any log item must have a timestamp
;;   regardless of `org-log-note-headings', which makes sense for a log note to
;;   have...
;; - any clock notes are defined as any item that is not a log item but after a
;;   clock
;;
;; The first node that breaks any of the above conditions will be the dividing
;; line between the logbook and contents. Note that the one loophole this
;; creates is that it is theoretically possible (but unlikely) that an item
;; immediately after a clock could be interpreted as a clock note even if it was
;; not intended as one.

;; Any operation involving the logbook or contents will either require
;; separating the supercontents of the headline into the supercontents object,
;; merging (the reverse), or both. All cases will require the user-specified
;; config to determine how to perform the separation/merge.
;;
;; Steps for separating a headlines section nodes to a supercontents list are:
;; 1. determine the logbook configuration
;; 2. initialize a list of functions called the "state" which will be used to
;;    identify and collect logbook nodes
;; 3. use the state to "walk" down the nodes of the headline's section, sorting
;;    them as items, clocks, or unknown; as the state iterates, the functions
;;    in the state list will be modified to reflect valid logbook nodes that
;;    can be subsequently parsed (hence the name)
;; 4. Stop "walking" when the state node either runs out of functions or a node
;;    is encountered that satisfies none of the state functions; all the
;;    remaining nodes are deemed "contents"
;; 5. return a supercontents list using the sorted items/clocks/unknown nodes
;;    from the walk and the contents
;;
;; Steps for merging a supercontents list to nodes are:
;; 1. determine the logbook configuration
;; 2. sort the items and clocks by their timestamp (most recent at the top)
;; 3. merge the items and clocks if required by the config
;; 4. encapsulate items and clocks in drawers if required
;; 5. append items and clocks (or drawers if applicable)
;; 6. append the logbook nodes from above with the contents from the contents

;; logbook struct

(define-inline org-ml--logbook-init (items clocks unknown)
  "Create a new logbook alist.
ITEMS, CLOCKS, and UNKNOWN correspond to a list of item nodes,
clock notes (which may contain item nodes for notes) and other
nodes."
  (declare (pure t) (side-effect-free t))
  (inline-quote
   (list :items ,items :clocks ,clocks :unknown ,unknown)))

(define-inline org-ml-logbook-get-items (logbook)
  "Return the :items slot from LOGBOOK."
  (declare (pure t) (side-effect-free t))
  (inline-quote
   (plist-get ,logbook :items)))

(define-inline org-ml-logbook-get-clocks (logbook)
  "Return the :clocks slot from LOGBOOK."
  (declare (pure t) (side-effect-free t))
  (inline-quote
   (plist-get ,logbook :clocks)))

(define-inline org-ml-logbook-get-post-blank (logbook)
  "Return the :clocks slot from LOGBOOK."
  (declare (pure t) (side-effect-free t))
  (inline-quote
   (plist-get ,logbook :post-blank)))

(define-inline org-ml-logbook-set-items (items logbook)
  "Set the :items slot in LOGBOOK to ITEMS."
  (declare (pure t) (side-effect-free t))
  (inline-quote
   (-let (((&plist :clocks :unknown) ,logbook))
     (org-ml--logbook-init ,items clocks unknown))))

(define-inline org-ml-logbook-set-clocks (clocks logbook)
  "Set the :clocks slot in LOGBOOK to CLOCKS."
  (declare (pure t) (side-effect-free t))
  (inline-quote
   (-let (((&plist :items :unknown) ,logbook))
     (org-ml--logbook-init items ,clocks unknown))))

(org-ml--defun-anaphoric* org-ml-logbook-map-items (fun logbook)
  "Apply function to :item slot in LOGBOOK.
FUN is a unary function that takes a list of items and returns a
new list of items."
  (--> (org-ml-logbook-get-items logbook)
       (org-ml-logbook-set-items (funcall fun it) logbook)))

(org-ml--defun-anaphoric* org-ml-logbook-map-clocks (fun logbook)
  "Apply function to :clocks slot in LOGBOOK.
FUN is a unary function that takes a list of clocks and returns a
new list of clocks."
  (--> (org-ml-logbook-get-clocks logbook)
       (org-ml-logbook-set-clocks (funcall fun it) logbook)))

;; supercontents struct

;; This is a structure that the user may interact with, so some of these
;; functions are public

(define-inline org-ml--supercontents-init-from-lb
  (planning node-props logbook blank contents)
  "Create a supercontents plist.

PLANNING is a planning node or nil.

NODE-PROPS is a list of like (KEY VAL) for each node property.

LOGBOOK is a logbook as given by `org-ml--logbook-init'.

BLANK is the blank space above the contents.

CONTENTS is a list of nodes corresponding to the headline
contents (the stuff after the logbook)."
  (declare (pure t) (side-effect-free t))
  (inline-quote
   (list :planning ,planning
         :node-props ,node-props
         :logbook ,logbook
         :blank ,blank
         :contents ,contents)))

(define-inline org-ml--supercontents-init
  (planning node-props items clocks unknown blank contents)
  "Create a supercontents alist.
ITEMS, CLOCKS, UNKNOWN, and POST-BLANK are lists corresponding to
the arguments in `org-ml--logbook-init' and CONTENTS has the same
meaning as `org-ml--supercontents-init-from-lb'."
  (declare (pure t) (side-effect-free t))
  (inline-quote
   (let ((lb (org-ml--logbook-init ,items ,clocks ,unknown)))
     (org-ml--supercontents-init-from-lb ,planning ,node-props lb ,blank ,contents))))

(define-inline org-ml-supercontents-get-planning (supercontents)
  "Return the :planning slot of SUPERCONTENTS."
  (declare (pure t) (side-effect-free t))
  (inline-quote (plist-get ,supercontents :planning)))

(define-inline org-ml-supercontents-set-planning (planning supercontents)
  "Set the :planning slot of SUPERCONTENTS to CONTENTS."
  (declare (pure t) (side-effect-free t))
  (inline-quote
   (-let (((&plist :node-props n :logbook l :blank b :contents c) ,supercontents))
     (org-ml--supercontents-init-from-lb ,planning n l b c))))

(define-inline org-ml-supercontents-get-node-properties (supercontents)
  "Return the :node-props slot of SUPERCONTENTS."
  (declare (pure t) (side-effect-free t))
  (inline-quote (plist-get ,supercontents :node-props)))

(define-inline org-ml-supercontents-set-node-properties (node-props supercontents)
  "Set the :node-props slot of SUPERCONTENTS to CONTENTS."
  (declare (pure t) (side-effect-free t))
  (inline-quote
   (-let (((&plist :planning p :logbook l :blank b :contents c) ,supercontents))
     (org-ml--supercontents-init-from-lb p ,node-props l b c))))

(define-inline org-ml-supercontents-get-contents (supercontents)
  "Return the :contents slot of SUPERCONTENTS."
  (declare (pure t) (side-effect-free t))
  (inline-quote (plist-get ,supercontents :contents)))

(define-inline org-ml-supercontents-set-contents (contents supercontents)
  "Set the :contents slot of SUPERCONTENTS to CONTENTS."
  (declare (pure t) (side-effect-free t))
  (inline-quote
   (-let (((&plist :planning p :node-props n :logbook l :blank b) ,supercontents))
     (org-ml--supercontents-init-from-lb p n l b ,contents))))

(org-ml--defun-anaphoric* org-ml-supercontents-map-contents (fun supercontents)
  "Apply function to :contents slot in SUPERCONTENTS.
FUN is a unary function that takes a list of nodes and returns a
new list of nodes."
  (--> (org-ml-supercontents-get-contents supercontents)
       (org-ml-supercontents-set-contents (funcall fun it) supercontents)))

(define-inline org-ml-supercontents-get-logbook (supercontents)
  "Return the :logbook slot of SUPERCONTENTS."
  (declare (pure t) (side-effect-free t))
  (inline-quote
   (plist-get ,supercontents :logbook)))

(define-inline org-ml-supercontents-set-logbook (logbook supercontents)
  "Set the :logbook slot of SUPERCONTENTS to LOGBOOK."
  (declare (pure t) (side-effect-free t))
  (inline-quote
   (-let (((&plist :planning p :node-props n :blank b :contents c) ,supercontents))
     (org-ml--supercontents-init-from-lb p n ,logbook b c))))

(org-ml--defun-anaphoric* org-ml-supercontents-map-logbook (fun supercontents)
  "Apply function to :logbook slot in SUPERCONTENTS.
FUN is a unary function that takes a logbook and returns a new
logbook."
  (--> (org-ml-supercontents-get-logbook supercontents)
       (org-ml-supercontents-set-logbook (funcall fun it) supercontents)))

;; supercontents config (scc) data structure

;; Internal alist representing the user-specified config. The user form of the
;; config is a plist with the keys :log-into-drawer, :clock-into-drawer, and
;; :clock-notes which correspond to `org-log-into-drawer',
;; `org-clock-into-drawer', and `org-log-note-clock-out' respectively.

;; The options :log/clock-into-drawer control the "drawer configuration". Eight
;; configurations are possible:
;; 
;; | log    | clock   | result                                                                  |
;; |----------+----------+----------------------------------------------------------------------|
;; | nil    | nil     | items and clocks loose                                                  |
;; | t      | nil     | items in a drawer called LOGBOOK, clocks loose                          |
;; | nil    | t       | clocks in a drawer called LOGBOOK, items loose                          |
;; | STR1   | STR2    | items and clocks in different drawers called STR1 and STR2              |
;; | STR/t  | STR/t   | items and clocks in the same drawer called STRING (or LOGBOOK if t)     |
;; | nil    | INTEGER | items loose, clocks in a drawer called LOGBOOK if > INTEGER             |
;; | t      | INTEGER | items in drawer called LOGBOOK, and same for clocks if > INTEGER        |
;; | STR    | INTEGER | items in drawer called STR clocks in drawer called LOGBOOK if > INTEGER |
;;
;; :clock-out-notes applies to all the above cases and is thus an independent
;; consideration

(defun org-ml-logbook-item-get-timestamp (item)
  "Return the log timestamp of ITEM if it exists."
  (cl-flet
      ((is-long-inactive-timestamp
        (node)
        (when (and (org-ml--is-type 'timestamp node)
                   (org-ml--property-is-eq :type 'inactive node)
                   (-some->> (org-ml--timestamp-get-start-timelist node)
                     (org-ml-timelist-has-time)))
          (org-ml--timestamp-get-start-unixtime node)))
       (is-line-break
        (node)
        (or (org-ml--is-type 'line-break node)
            (and (org-ml--is-type 'plain-text node)
                 (equal "\n" node))))
       (get-paragraph-children
        (item)
        (-when-let (first-child (car (org-element-contents item)))
          (when (org-ml--is-type 'paragraph first-child)
            (org-element-contents first-child)))))
    (when (org-ml--is-type 'item item)
      (let ((pchildren (get-paragraph-children item)))
        (-if-let (i (-find-index #'is-line-break pchildren))
            (is-long-inactive-timestamp (nth (1- i) pchildren))
          (is-long-inactive-timestamp (-last-item pchildren)))))))

(defun org-ml--scc-encode (config)
  "Return a supercontents-config object from CONFIG."
  (cl-flet*
      ((select-name
        (option)
        (pcase option
          (`t "LOGBOOK")
          (`nil nil)
          ((and (pred stringp) s) s)
          (e (error "Invalid option: %s" e)))))
    (-let* (((&plist :log-into-drawer lid
                     :clock-into-drawer cid
                     :clock-out-notes notes)
             config)
            (clock-limit (and (integerp cid) cid))
            (id-name (select-name lid))
            (cd-name (if clock-limit "LOGBOOK" (select-name cid)))
            (single-drawer? (equal id-name cd-name)))
      `((:drawers :items ,(and (not single-drawer?) id-name)
                  :clocks ,(and (not single-drawer?) cd-name)
                  :mixed ,(and single-drawer? id-name)
                  :clock-limit ,clock-limit)
        (:clock-notes . ,notes)
        (:is-log-item-fun . ,#'org-ml-logbook-item-get-timestamp)))))

(defun org-ml--scc-get-drawer-key (key scc)
  "Return the drawer from SCC in slot denoted by KEY."
  (-let (((&alist :drawers) scc))
    (plist-get drawers key)))

(define-inline org-ml--scc-get-clock-notes (scc)
  "Return the :clock-notes slot from SCC."
  (inline-quote
   (alist-get :clock-notes ,scc)))

(define-inline org-ml--scc-get-log-item-fun (scc)
  "Return the :is-log-item-fun slot from SCC."
  (inline-quote
   (alist-get :is-log-item-fun ,scc)))

;; logbook separation (nodes -> logbook + contents)

(defun org-ml--node-has-trailing-space (node)
  "Return t if NODE has at least one newline after it."
  (and (< 0 (org-ml--get-post-blank-textsafe node)) t))

(defun org-ml--node-is-drawer-with-name (drawer-name node)
  "Return t if NODE is a drawer with DRAWER-NAME."
  (and (org-ml--is-type 'drawer node)
       (equal drawer-name (org-element-property-raw :drawer-name node))))

(defun org-ml--flatten-plain-lists (nodes)
  "Return NODES with unwrapped plain-list nodes.
\"Unwrapping\" means replacing the plain-list with its top-level
items."
  (cl-flet
      ((flatten
        (plain-list)
        (let ((pb (org-element-post-blank plain-list)))
          (->> (org-element-contents plain-list)
               (org-ml--set-last-post-blank pb)))))
    (--splice (org-ml--is-type 'plain-list it) (flatten it) nodes)))

(defun org-ml--wrap-plain-lists (nodes)
  "Return NODES with all subsequent items wrapped as plain-lists.
This is the dual of `org-ml--flatten-plain-lists'."
  (cl-flet
      ((wrap
        (acc node)
        (cond
         ((and (org-ml--is-type 'item node)
               (org-ml--is-type 'plain-list (car acc)))
          (cons (org-ml-map-children* (cons node it) (car acc))
                (cdr acc)))
         ((org-ml--is-type 'item node)
          (let* ((pb (org-element-post-blank node))
                 (pl (->> (org-ml--set-post-blank 0 node)
                          (org-ml-build-plain-list :post-blank pb))))
            (cons pl acc)))
         (t
          (cons node acc)))))
    (-reduce-from #'wrap nil (reverse nodes))))

(defun org-ml--separate-logbook (scc mode nodes)
  "Separate NODES into logbook components.
SCC is the supercontents-config as given by `org-ml--scc-encode'.
MODE is the mode by which to separate the nodes and is one of
`mixed' (items and clocks are mixed together), `clocks', or
`items'. The returned list will be like (ITEMS CLOCKS UNKNOWN)."
  (let ((n (org-ml--scc-get-clock-notes scc))
        (f (org-ml--scc-get-log-item-fun scc)))
    (cl-flet
        ((split
          (acc node)
          (if (not node) acc
            (cond
             ((and (org-ml--is-type 'clock node)
                   (memq mode '(:mixed :clocks)))
              (cons (cons 'clocks node) acc))
             ((and (org-ml--is-type 'item node)
                   (memq mode '(:items :mixed))
                   (funcall f node))
              (cons (cons 'items node) acc))
             ((and (org-ml--is-type 'item node)
                   (memq mode '(:clocks :mixed))
                   n
                   (not (funcall f node))
                   (org-ml--is-type 'clock (cdr (car acc))))
              (cons (cons 'clocks node) acc))
             (t
              (cons (cons 'unknown node) acc))))))
      (->> (org-ml--flatten-plain-lists nodes)
           (-reduce-from #'split nil)))))

(defmacro org-ml--state-slot (key limit eliminators next-fun)
  "Return a new slot for logbook separator state.
KEY is the slot's key and LIMIT, ELIMINATORS, and NEXT-FUN are
the respectibe values for the plist part of the slot."
  (declare (indent 3))
  `(list ,key
         :limit ,limit
         :eliminators ,eliminators
         :next ,next-fun))

(defun org-ml--state-add-slot (slot state)
  "Add SLOT to STATE if SLOT's key is not already present."
  (if (--any? (eq (car slot) (car it)) (cdr state)) state
    (cons 'state (cons slot (cdr state)))))

(defun org-ml--state-remove-slot (key state)
  "Remove slot from STATE given by KEY."
  (cons 'state (--remove-first (eq key (car it)) (cdr state))))

(defun org-ml--state-tick (key state)
  "Update STATE based on KEY.

The following will happen:
1) the slot named KEY will have its limit decremented by 1
2) all slots with limits of 0 will be removed
3) all slots with eliminators containing KEY will be removed"
  (cl-flet
      ((decrement
        (slot)
        (-let* (((key . (&plist :limit :eliminators :next)) slot)
                (limit* (when limit (1- limit))))
          (org-ml--state-slot key limit* eliminators next)))
       (is-at-limit
        (slot)
        (let ((limit (plist-get (cdr slot) :limit)))
          (when limit (= 0 limit))))
       (can-eliminate
        (key slot)
        (let ((el (plist-get (cdr slot) :eliminators)))
          (or (eq t el) (memq key el)))))
    (->> (cdr state)
         (--map-first (eq key (car it)) (decrement it))
         (-remove #'is-at-limit)
         (--remove (can-eliminate key it))
         (cons 'state))))

(defun org-ml--item-get-next-state (scc state node)
  "Return updated state for NODE if it is a valid log item.
SCC is given by `org-ml--scc-encode' and STATE is given by
`org-ml--state-init'. STATE will be updated by called
`org-ml--state-tick'."
  (let ((f (org-ml--scc-get-log-item-fun scc)))
    (when (and (org-ml--is-type 'item node) (funcall f node))
      (list (org-ml--state-tick :item state) (list (cons 'items node))))))

(defun org-ml--clock-note-get-next-state (scc state node)
  "Return updated state for NODE if it is a valid clock note.
SCC is given by `org-ml--scc-encode' and STATE is given by
`org-ml--state-init'. STATE will be updated by called
`org-ml--state-tick'."
  (-let ((f (org-ml--scc-get-log-item-fun scc)))
    (when (and (org-ml--is-type 'item node) (not (funcall f node)))
      (list (org-ml--state-tick :clock-note state)
            (list (cons 'clocks node))))))

(defun org-ml--clock-get-next-state (scc state node)
  "Return updated state for NODE if it is a valid clock.

This will distinguish between clocks and clock notes as
appropriate, and if a clock as found and clock notes are allowed,
STATE will be updated with a new slot to detect clock notes that
will exist for one pass.

SCC is given by `org-ml--scc-encode' and STATE is given by
`org-ml--state-init'. STATE will be updated by called
`org-ml--state-tick'."
  (when (org-ml--is-type 'clock node)
    (let* ((slot (org-ml--state-slot :clock-notes 1 t
                   #'org-ml--clock-note-get-next-state))
           (next-state (--> (org-ml--state-tick :clock state)
                            (if (org-ml--scc-get-clock-notes scc)
                                (org-ml--state-add-slot slot it)
                              it))))
      (list next-state (list (cons 'clocks node))))))

(defun org-ml--drawer-get-next-state (mode name scc state node)
  "Return updated state for NODE if it is a valid drawer.

The drawer will be separated using `org-ml--separate-logbook'
according to MODE but only if NAME matches.

SCC is given by `org-ml--scc-encode' and STATE is given by
`org-ml--state-init'. STATE will be updated by called
`org-ml--state-tick'."
  (when (org-ml--node-is-drawer-with-name name node)
    (let ((drawer-nodes (->> (org-element-contents node)
                             (org-ml--separate-logbook scc mode)))
                             ;; (reverse)))
          (key (cl-case mode
                 (items :item-drawer)
                 (clocks :clock-drawer)
                 (mixed :mixed-drawer))))
      (list (org-ml--state-tick key state) drawer-nodes))))

(defun org-ml--clock-get-next-state* (scc state node)
  "Return updated state for NODE if it is a valid clock.

Unlike `org-ml--clock-get-next-state' this will add a new slot to
STATE that detects item drawers (using the :item-drawer name from
SCC) and removes the slot that detects mixed drawers. This is only
intended to be used for the configuration option where clocks may
or may not be in a drawer with items.

SCC is given by `org-ml--scc-encode' and STATE is given by
`org-ml--state-init'. STATE will be updated by called
`org-ml--state-tick'."
  (-let (((next-state log-nodes) (org-ml--clock-get-next-state scc state node)))
    (when next-state
      (let* ((name (org-ml--scc-get-drawer-key :mixed scc))
             (slot (org-ml--state-slot :item-drawer 1 nil
                     (-partial #'org-ml--drawer-get-next-state :items name)))
             (next-state (->> (org-ml--state-add-slot slot state)
                              (org-ml--state-remove-slot :mixed-drawer)
                              (org-ml--state-tick :clock))))
        (list next-state log-nodes)))))

(defun org-ml--mixed-drawer-get-next-state** (mode name scc state node)
  "Return updated state for NODE if it is a valid mixed drawer.

Unlike `org-ml--drawer-get-next-state' this will remove the slot
that detects clocks This is only intended to be used for the
configuration option where clocks may or may not be in a drawer
with items. MODE and NAME carry the same meaning.

SCC is given by `org-ml--scc-encode' and STATE is given by
`org-ml--state-init'. STATE will be updated by called
`org-ml--state-tick'."
  (-let (((next-state log-nodes)
          (org-ml--drawer-get-next-state mode name scc state node)))
    (if (--any? (eq 'clocks (car it)) log-nodes)
        (let ((next-state (->> (org-ml--state-remove-slot :clock next-state)
                               (org-ml--state-tick :mixed-drawer))))
          (list next-state log-nodes)))))

(defun org-ml--init-state (scc)
  "Create a new state from SCC.
The state will be a list like (state SLOT1 SLOT2 ...) where SLOTX
is given by `org-ml--state-slot'. The purpose of this data
structure is to represent valid logbook nodes for a particular
configuration as well as their order and number. The reason it is
called a state is because it will be used in an iterator as the
logbook is separated from the contents, and a `stateful' iterator
is needed because most logbook configurations have sequential
dependencies for valid nodes."
  (-let ((funs
          (pcase (alist-get :drawers scc)

            ;; items not in drawer, clocks not in drawer
            (`(:items nil :clocks nil :mixed nil :clock-limit nil)
             (list (org-ml--state-slot :item nil nil
                     #'org-ml--item-get-next-state)
                   (org-ml--state-slot :clock nil nil
                     #'org-ml--clock-get-next-state)))

            ;; items and clocks in the same drawer
            (`(:items nil :clocks nil :mixed ,m :clock-limit nil)
             (list (org-ml--state-slot :mixed-drawer 1 nil
                     (-partial #'org-ml--drawer-get-next-state :mixed m))))

            ;; items not in drawer, clocks in drawer
            (`(:items nil :clocks ,c :mixed nil :clock-limit nil)
             (list (org-ml--state-slot :item nil nil
                     #'org-ml--item-get-next-state)
                   (org-ml--state-slot :clock-drawer 1 nil
                     (-partial #'org-ml--drawer-get-next-state :clocks c))))

            ;; items not in drawer, clocks might be in a drawer
            (`(:items nil :clocks ,c :mixed nil :clock-limit ,L)
             (list (org-ml--state-slot :item nil nil
                     #'org-ml--item-get-next-state)
                   (org-ml--state-slot :clock L '(:clock-drawer)
                     #'org-ml--clock-get-next-state)
                   (org-ml--state-slot :clock-drawer 1 '(:clock)
                     (-partial #'org-ml--drawer-get-next-state :clocks c))))

            ;; items in drawer, clocks not in drawer
            (`(:items ,i :clocks nil :mixed nil :clock-limit nil)
             (list (org-ml--state-slot :clock nil nil
                     #'org-ml--clock-get-next-state)
                   (org-ml--state-slot :item-drawer 1 nil
                     (-partial #'org-ml--drawer-get-next-state :items i))))

            ;; items in drawer, clocks in a different drawer
            (`(:items ,i :clocks ,c :mixed nil :clock-limit nil)
             (list (org-ml--state-slot :item-drawer 1 nil
                     (-partial #'org-ml--drawer-get-next-state :items i))
                   (org-ml--state-slot :clock-drawer 1 nil
                     (-partial #'org-ml--drawer-get-next-state :clocks c))))

            ;; items in drawer, clocks either loose or in a different drawer
            (`(:items ,i :clocks ,c :mixed nil :clock-limit ,L)
             (list (org-ml--state-slot :item-drawer 1 nil
                     (-partial #'org-ml--drawer-get-next-state :items i))
                   (org-ml--state-slot :clock L '(:clock-drawer)
                     #'org-ml--clock-get-next-state)
                   (org-ml--state-slot :clock-drawer 1 '(:clock)
                     (-partial #'org-ml--drawer-get-next-state :clocks c))))

            ;; items in drawer, clocks might be in the same drawer
            (`(:items nil :clocks nil :mixed ,m :clock-limit ,L)
             (list (org-ml--state-slot :clock L nil
                     #'org-ml--clock-get-next-state*)
                   (org-ml--state-slot :mixed-drawer 1 nil
                     (-partial #'org-ml--mixed-drawer-get-next-state** :mixed m))))

            (e (error "This shouldn't happen: %s" e)))))
    (cons 'state funs)))

(defmacro org-ml--reduce-state (initial-state form list)
  "Sort of like `--reduce' but with state.
It is only \"sort of\" like a standard reduce function for two
reasons a) it keeps track of state and b) returns the remainder
of LIST when the state is nil (which signifies termination of the
loop). FORM is a form where `it' is bound to the current node,
`acc' is bound to the accumulated nodes, and `it-state' is bound
to the current state. FORM must return a list like
\(NEW-STATE NEW-ACC), where NEW-STATE and NEW-ACC become the state
and accumulator on the next iteration. INITIAL-STATE is bound to
`it-state' on the first iteration."
  (declare (indent 1))
  (let ((r (make-symbol "--rest")))
    `(let ((it-state ,initial-state)
           (,r ,list)
           acc it)
       (while (and it-state ,r)
         (setq it (car ,r))
         (-setq (it-state acc) ,form)
         (when it-state
           (setq ,r (cdr ,r))))
       (list acc ,r))))

(defun org-ml--split-logbook (config nodes)
  "Return NODES split to logbook components.
CONFIG is a plist parsable by `org-ml--scc-encode'."
  (if (not nodes) (list (org-ml--logbook-init nil nil nil) 0 nil)
    (cl-flet
        ((map-cdr
           (list)
           (-map #'cdr list))
         (try-test-funs
           (scc state node)
           (-let ((test-funs (--map (plist-get (cdr it) :next) (cdr state))))
             (--reduce-from (if acc acc (funcall it scc state node)) nil test-funs))))
      (-let* ((scc (org-ml--scc-encode config))
              (init-state (org-ml--init-state scc))
              (flat (org-ml--flatten-plain-lists nodes))
              (i (-some->> flat
                   (-find-index #'org-ml--node-has-trailing-space)
                   (1+)))
              ((nodes-before-space nodes-after-space)
               (if i (-split-at i flat) (list flat nil)))
              (first-space-post-blank (->> (-last-item nodes-before-space)
                                           (org-element-post-blank)))
              ((logbook-nodes contents-nodes-before-space)
               (org-ml--reduce-state init-state
                 (-let (((next-state logbook-nodes) (try-test-funs scc it-state it)))
                   (if logbook-nodes
                       (list next-state (append logbook-nodes acc))
                     (list nil acc)))
                 nodes-before-space))
              ((&alist 'items 'clocks 'unknown) (->> (reverse logbook-nodes)
                                                     (-group-by #'car)))
              (post-blank
               (if logbook-nodes
                   (if contents-nodes-before-space 0 first-space-post-blank)
                 0))
              (contents (->> nodes-after-space
                             (append contents-nodes-before-space)
                             (org-ml--wrap-plain-lists))))
        (list (org-ml--logbook-init
               (map-cdr items)
               (map-cdr clocks)
               (map-cdr unknown))
              post-blank
              contents)))))

;; logbook merging (supercontents -> nodes)

(defun org-ml--sort-logbook (mode scc nodes)
  "Sort NODES and return.
NODES will be sorted according to their timestamps and are
assumed to be valid log items or clocks/clock-notes (anything
else will trigger an error). SCC is a supercontents-config as
returned by `org-ml--scc-init'. MODE is one of :mixed, :items,
or :clocks depending on what is intended to be sorted."
  (-let (((&alist :clock-notes n :is-log-item-fun f) scc))
    (cl-labels
        ((get-ts
          (node)
          (cl-case (org-ml-get-type node)
            (clock
             (-some->> (org-element-property-raw :value node)
               (org-ml--timestamp-get-start-unixtime)))
            (item
             (funcall f node))))
         (prepare-node
          (acc node)
          ;; Return a list like (NODE . NOTE) where NODE is a clock or item and
          ;; NOTE is a clock note, which is only added to NODE if the type of
          ;; NODE is a clock and NOTE appears immediately after.
          (cond
           ((and n
                 (memq mode '(:clocks :mixed))
                 (org-ml--is-type 'item node)
                 (org-ml--is-type 'clock (car (car acc)))
                 (not (funcall f node)))
            (cons (list (car (car acc)) node) (cdr acc)))
           ((or (and (memq mode '(:items :mixed))
                     (org-ml--is-type 'item node)
                     (funcall f node))
                (and (memq mode '(:clocks :mixed))
                     (org-ml--is-type 'clock node)))
            (cons (list node) acc))
           (t
            (let ((msg (cond
                        ((and n (eq mode :clocks))
                         "Not a valid clock or note: %s")
                        ((and n (eq mode :mixed))
                         "Not a valid item, clock, or note: %s")
                        ((eq mode :clocks)
                         "Not a valid clock: %s")
                        ((eq mode :items)
                         "Not a valid item: %s")
                        (t
                         "Not a valid clock or item: %s"))))
              (error msg node)))))
         ;; this should be imperative because the recursive version has O(n)
         ;; calls to itself...byebye stack :(
         (merge-nodes
          (nodes-a nodes-b)
          (let (merged)
            (while (or nodes-a nodes-b)
              (pcase (cons nodes-a nodes-b)
                (`(,as . nil)
                 (setq merged (append (nreverse as) merged)
                       nodes-a nil))
                (`(nil . ,bs)
                 (setq merged (append (nreverse bs) merged)
                       nodes-b nil))
                (`((,a . ,as) . (,b . ,bs))
                 (let ((ts-a (get-ts (car a)))
                       (ts-b (get-ts (car b))))
                   (cond
                    ((not ts-a)
                     (error "Could not get timestamp for logbook node: %s" a))
                    ((not ts-b)
                     (error "Could not get timestamp for logbook node: %s" b))
                    ((<= ts-b ts-a)
                     (setq merged (cons a merged)
                           nodes-a as))
                    ((< ts-a ts-b)
                     (setq merged (cons b merged)
                           nodes-b bs))
                    (t
                     (error "Unknown merge error")))))))
            (nreverse merged)))
         (merge-and-sort
          (nodes)
          (let ((L (length nodes)))
            (if (<= L 1) nodes
              (-let (((left right) (-split-at (/ L 2) nodes)))
                (merge-nodes (merge-and-sort left) (merge-and-sort right)))))))
      (->> (-reduce-from #'prepare-node nil nodes)
           (merge-and-sort)
           (-flatten-n 1)
           (org-ml--wrap-plain-lists)))))

(defun org-ml--merge-logbook (scc items clocks)
  "Merge ITEMS and CLOCKS.
Return these two inputs as a single sorted list (highest
timestamp first) according to `org-ml--sort-logbook'. SCC is a
supercontents-config as returned by `org-ml--scc-encode'."
  (org-ml--sort-logbook :mixed scc (append items clocks)))

(defun org-ml--logbook-items-to-nodes (scc logbook)
  "Return items in LOGBOOK as a sorted list of NODES.
SCC is a supercontents-config as returned by
`org-ml--scc-encode'."
  (->> (org-ml-logbook-get-items logbook)
       (org-ml--sort-logbook :items scc)))

(defun org-ml--logbook-clocks-to-nodes (scc logbook)
  "Return clocks in LOGBOOK as a sorted list of NODES.
SCC is a supercontents-config as returned by
`org-ml--scc-encode'."
  (->> (org-ml-logbook-get-clocks logbook)
       (org-ml--sort-logbook :clocks scc)))

(defun org-ml--logbook-to-nodes (config logbook)
  "Return LOGBOOK as a list of NODES.
Anything in the UNKNOWN slot will be ignored. The exact
nodes (drawers, loose items, etc) will be determined by the SCC.
CONFIG is a config plist to be given to `org-ml--scc-encode'."
  ;; TODO clean this up
  (-let (((&plist :items :clocks) logbook))
    (when (or items clocks)
      (cl-flet*
          ((build-drawer
             (name children)
             (apply #'org-ml-build-drawer name children))
           (build-drawer-maybe
             (name children)
             (-some->> children (build-drawer name)))
           (cons-drawer-maybe
             (name drawer-nodes loose-nodes)
             (let ((drawer (-some->> drawer-nodes (build-drawer name))))
               (if drawer (cons drawer loose-nodes) loose-nodes)))
           (below-limit
             (limit lb)
             (->> (org-ml-logbook-get-clocks lb)
                  (--count (org-ml--is-type 'clock it))
                  (>= limit)))
           (merge-nodes
             (enconf lb)
             (-let (((&plist :items :clocks) lb))
               (org-ml--merge-logbook enconf items clocks)))
           (build-mixed-drawer-maybe
             (enconf m lb)
             (-some->> (merge-nodes enconf lb)
               (build-drawer m)
               (list)))
           (to-item-clock-nodes
             (enconf lb)
             (list (org-ml--logbook-items-to-nodes enconf lb)
                   (org-ml--logbook-clocks-to-nodes enconf lb))))
        (-let (((enconf &as &alist :drawers d) (org-ml--scc-encode config)))
          (pcase d

            ;; items not in drawer, clocks not in drawer
            (`(:items nil :clocks nil :mixed nil :clock-limit nil)
             (merge-nodes enconf logbook))

            ;; items and clocks in the same drawer
            (`(:items nil :clocks nil :mixed ,m :clock-limit nil)
             (build-mixed-drawer-maybe enconf m logbook))

            ;; items in drawer, clocks not in drawer
            (`(:items ,i :clocks nil :mixed nil :clock-limit nil)
             (-let* (((items clocks) (to-item-clock-nodes enconf logbook)))
               (cons-drawer-maybe i items clocks)))

            ;; items not in drawer, clocks in drawer
            (`(:items nil :clocks ,c :mixed nil :clock-limit nil)
             (-let* (((items clocks) (to-item-clock-nodes enconf logbook)))
               (cons-drawer-maybe c clocks items)))

            ;; items in drawer, clocks might be in the same drawer
            (`(:items nil :clocks nil :mixed ,m :clock-limit ,l)
             (if (below-limit l logbook)
                 (-let* (((items clocks) (to-item-clock-nodes enconf logbook)))
                   (cons-drawer-maybe m items clocks))
               (build-mixed-drawer-maybe enconf m logbook)))
            
            ;; items not in drawer, clocks might be in a drawer
            (`(:items nil :clocks ,c :mixed nil :clock-limit ,l)
             (if (below-limit l logbook) (merge-nodes enconf logbook)
               (-let* (((items clocks) (to-item-clock-nodes enconf logbook)))
                 (cons-drawer-maybe c clocks items))))

            ;; items in drawer, clocks in a different drawer
            (`(:items ,i :clocks ,c :mixed nil :clock-limit nil)
             (-let* (((items clocks) (to-item-clock-nodes enconf logbook))
                     (items-drawer (build-drawer-maybe i items))
                     (clocks-drawer (build-drawer-maybe c clocks)))
               (-non-nil (list items-drawer clocks-drawer))))

            ;; items in drawer, clocks either loose or in a different drawer
            (`(:items ,i :clocks ,c :mixed nil :clock-limit ,l)
             (-let* (((items clocks) (to-item-clock-nodes enconf logbook))
                     (items-drawer (build-drawer-maybe i items)))
               (if (below-limit l logbook)
                   (if items-drawer (cons items-drawer clocks) clocks)
                 (->> (build-drawer-maybe c clocks)
                      (list items-drawer)
                      (-non-nil)))))

            (e (error "This shouldn't happen: %s" e))))))))

;; section -> supercontents

;; Introduce another data abstraction here called the "supersection". This is
;; much simpler than the supercontents, and consists only of the :pre-blank and
;; section children of the encapsulating headline. The main purpose of this is
;; to provide a means to update "the stuff under the headline" easily without
;; triggering the headline itself to undefer most of its properties. The
;; :pre-blank is part of the headline node even though it visually affects the
;; stuff underneath it. This especially matters for functions like
;; `org-ml-update-supersections' and `org-ml-update-supercontents' which can be
;; massively sped up if they don't need to update the headline in the buffer.

(define-inline org-ml--supersection-init (pre-blank section)
  "Create a supersection plist from PRE-BLANK and SECTION.
SECTION is a list of nodes under the section node in a headline."
  (inline-quote (list :pre-blank ,pre-blank :section ,section)))

(defun org-ml--planning-split (planning)
  "Decompose PLANNING into lists."
  (list :closed (-some->> (org-element-property-raw :closed planning)
                  (org-ml--timestamp-get-start-timelist))
        :scheduled (-some->> (org-element-property-raw :scheduled planning)
                     (org-ml--timestamp-to-planning-list))
        :deadline (-some->> (org-element-property-raw :deadline planning)
                    (org-ml--timestamp-to-planning-list))))

(defun org-ml--property-drawer-split (property-drawer)
  "Decompose PROPERTY-DRAWER into a list of node-property nodes."
  (->> (org-element-contents property-drawer)
       (--map (list (org-element-property-raw :key it)
                    (org-element-property-raw :value it)))))

(defun org-ml--from-first-second-rest
    (config planning prop-drawer blank-node children)
  "Create a new supercontents node in various ways.

CONFIG is a list corresponding to `org-ml--scc-encode'. PLANNING
is a planning node. PROP-DRAWER is a property-drawer node.
BLANK-NODE is a node that has a post-blank behind it. CHILDREN is
everything after the planning and/or property-drawer."
  (-let* ((pb (org-element-post-blank blank-node))
          ((logbook blank contents) (if (< 0 pb)
                                        `(nil ,pb ,children)
                                      (org-ml--split-logbook config children))))
    (org-ml--supercontents-init-from-lb
     (and planning (org-ml--planning-split planning))
     (and prop-drawer (org-ml--property-drawer-split prop-drawer))
     logbook
     blank
     contents)))

(defun org-ml--supersection-to-supercontents (config supersection)
  "Convert SUPERSECTION to supercontents.
CONFIG is a list corresponding to `org-ml--scc-encode'."
  (-let (((&plist :pre-blank pb :section children) supersection))
    ;; If pre-blank is >0, by definition there is no planning,
    ;; property-drawer, or logbook
    (if (< 0 pb) (org-ml--supercontents-init nil nil nil nil nil pb children)
      (-let* (((first . rest1) children)
              ((second . rest2) rest1)
              (t1 (org-ml-get-type first))
              (t2 (org-ml-get-type second)))
        (cond
         ((and (eq t1 'planning) (eq t2 'property-drawer))
          (org-ml--from-first-second-rest config first second second rest2))
         ((eq t1 'planning)
          (org-ml--from-first-second-rest config first nil first rest1))
         ((eq t1 'property-drawer)
          (org-ml--from-first-second-rest config nil first first rest1))
         (t
          (->> (org-ml--split-logbook config children)
               (apply #'org-ml--supercontents-init-from-lb nil nil))))))))

(defun org-ml--supercontents-to-supersection (config supercontents)
  "Convert SUPERCONTENTS to supersection.
CONFIG is a list corresponding to `org-ml--scc-encode'."
  (-let* (((&plist :planning p :node-props n :logbook lb :blank b :contents c)
           supercontents)
          (anyp (or (plist-get p :closed)
                    (plist-get p :scheduled)
                    (plist-get p :deadline)))
          (lb-nodes (org-ml--logbook-to-nodes config lb)))
    (cond
     (lb-nodes
      (org-ml--supersection-init
       0 `(,@(when anyp (list (apply #'org-ml-build-planning! p)))
           ,@(when n (list (apply #'org-ml-build-property-drawer! n)))
           ,@(org-ml--set-last-post-blank b lb-nodes)
           ,@c)))
     (n
      (org-ml--supersection-init
       0 `(,@(when anyp (list (apply #'org-ml-build-planning! p)))
           ,(apply #'org-ml-build-property-drawer! :post-blank b n)
           ,@c)))
     (anyp
      (org-ml--supersection-init
       0 (cons (apply #'org-ml-build-planning! :post-blank b p) c)))
     (t
      (org-ml--supersection-init b c)))))

;; public supercontents functions

(defun org-ml-headline-get-supercontents (config headline)
  "Return the supercontents of HEADLINE node.

Supercontents will be a plist like:

\(
  :planning PLANNING
  :node-props PROPS
  :logbook LB
  :blank BLANK
  :contents CONTENTS
)

PLANNING is a plist like the analogous argument of
`org-ml-build-planning!' or nil if non-existent.

PROPS is a list of node-property nodes.

LB is the logbook, which is another plist (see below).

BLANK is the value of any whitespace after the planning,
property-drawer, or logbook (assuming any exist) or the
:pre-blank value of the encapsulating headline (if they don't
exist).

CONTENTS is a list of nodes after all the other stuff above.

The logbook will be have keys :items, :clocks, and :unknown,
where the first two will include the item and clock nodes of the
logbook respectively, and the third will contain anything that
could not be identified as a valid logbook entry. Note that items
are actually stored under a plain-list node but will be returned
here as a flat list of items for convenience. Also note that the
:clocks slot can also include item nodes if clock notes are
returned.

CONFIG is a plist representing the logbook configuration to
target and will contain the following keys;
- :log-into-drawer - corresponds to the value of
  symbol `org-log-into-drawer' and carriers the same meaning
- :clock-into-drawer - corresponds to the value of
  symbol `org-clock-into-drawer' and carriers the same meaning
- :clock-out-notes - corresponds to the value of
  `org-log-note-clock-out'

Any values not given will default to nil. Note that there is no
way to infer what the logbook configuration should be, and thus
this controls how the logbook will be parsed; this means it also
determines which nodes will be returned in the :items/:clocks
slots and which will be deemed :unknown (see above) so be sure
this plist is set according to your desired target configuration."
  (->> (org-ml-headline-get-supersection headline)
       (org-ml--supersection-to-supercontents config)))
     
(defun org-ml-headline-set-supercontents (config supercontents headline)
  "Set logbook and contents of HEADLINE according to SUPERCONTENTS.
See `org-ml-headline-get-supercontents' for the meaning of CONFIG
and the structure of the SUPERCONTENTS list."
  (-> (org-ml--supercontents-to-supersection config supercontents)
      (org-ml-headline-set-supersection headline)))

(org-ml--defun-anaphoric* org-ml-headline-map-supercontents (config fun headline)
  "Map a function over the supercontents of HEADLINE.
FUN is a unary function that takes a supercontents list and
returns a modified supercontents list. See
`org-ml-headline-get-supercontents' for the meaning of CONFIG and
the structure of the supercontents list."
  (--> (org-ml-headline-get-supercontents config headline)
       (org-ml-headline-set-supercontents config (funcall fun it) headline)))

(defun org-ml-headline-get-supersection (headline)
  "Return supersection list for the section in HEADLINE."
  (org-ml--check-type 'headline headline)
  (org-ml--supersection-init
   (org-element-property-raw :pre-blank headline)
   (org-ml-headline-get-section headline)))

(defun org-ml-headline-set-supersection (supersection headline)
  "Return SUPERSECTION for the section in HEADLINE."
  (org-ml--check-type 'headline headline)
  (-let* (((&plist :pre-blank p :section m) supersection)
          (pb (org-element-property-raw :pre-blank headline))
          (headline* (if (/= p pb)
                         (->> (org-ml-copy headline)
                              (org-element-put-property-2 :pre-blank p))
                       headline)))
    (org-ml-headline-set-section m headline*)))

(org-ml--defun-anaphoric* org-ml-headline-map-supersection (fun headline)
  "Apply FUN to HEADLINE supersection."
  (let ((it (org-ml-headline-get-supersection headline)))
    (org-ml-headline-set-supersection (funcall fun it) headline)))

;; planning

(defun org-ml-headline-get-planning (headline)
  "Return the planning node in HEADLINE or nil if none."
  (org-ml--check-type 'headline headline)
  ;; TODO it seems silly that we need to "convert" the logbook when I'm not
  ;; modifying it. Lazy eval?
  (->> (org-ml-headline-get-supercontents nil headline)
       (org-ml-supercontents-get-planning)))

(defun org-ml-headline-set-planning (planning headline)
  "Return HEADLINE node with planning components set to PLANNING node."
  (org-ml--check-type 'headline headline)
  (org-ml-headline-map-supercontents* nil
    (org-ml-supercontents-set-planning planning it)
    headline))

(org-ml--defun-anaphoric* org-ml-headline-map-planning (fun headline)
  "Return HEADLINE node with planning node modified by FUN.

FUN is a unary function that takes a planning node and returns a
modified planning node."
   (--> (org-ml-headline-get-planning headline)
        (org-ml-headline-set-planning (funcall fun it) headline)))

;; node-properties (eg the entire property drawer)

(defun org-ml-headline-get-node-properties (headline)
  "Return a list of node-properties nodes in HEADLINE or nil if none."
  (org-ml--check-type 'headline headline)
  ;; TODO it seems silly that we need to "convert" the logbook when I'm not
  ;; modifying it. Lazy eval?
  (->> (org-ml-headline-get-supercontents nil headline)
       (org-ml-supercontents-get-node-properties)))

(defun org-ml-headline-set-node-properties (node-properties headline)
  "Return HEADLINE node with property drawer containing NODE-PROPERTIES.
NODE-PROPERTIES is a list of (key . value) pairs (both strings)."
  (org-ml--check-type 'headline headline)
  (org-ml-headline-map-supercontents* nil
    (org-ml-supercontents-set-node-properties node-properties it)
    headline))

(org-ml--defun-anaphoric* org-ml-headline-map-node-properties (fun headline)
  "Return HEADLINE node with property-drawer node modified by FUN.

FUN is a unary function that takes a property-drawer node and returns
a modified property-drawer node."
   (--> (org-ml-headline-get-node-properties headline)
        (org-ml-headline-set-node-properties (funcall fun it) headline)))

;; node-property

(defun org-ml-headline-get-node-property (key headline)
  "Return value of property with KEY in HEADLINE or nil if not found.
If multiple properties with KEY are present, only return the first."
  (->> (org-ml-headline-get-node-properties headline)
       (--first (equal key (car it)))
       (cadr)))

(defun org-ml-headline-set-node-property (key value headline)
  "Return HEADLINE with node property matching KEY set to VALUE.
If a property matching KEY is present, set it to VALUE. If multiple
properties matching KEY are present, only set the first."
  (org-ml-headline-map-node-properties*
    (-if-let (np (-some->> value (list key)))
        (-if-let (i (--find-index (equal key (car it)) it))
            (-replace-at i np it)
          (cons np it))
      (--remove-first (equal key (car it)) it))
    headline))

(org-ml--defun-anaphoric* org-ml-headline-map-node-property (key fun headline)
  "Return HEADLINE node with property value matching KEY modified by FUN.

FUN is a unary function that takes a node-property value and returns
a modified node-property value."
   (--> (org-ml-headline-get-node-property key headline)
        (org-ml-headline-set-node-property key (funcall fun it) headline)))

;; public logbook/contents getters/setters/mappers

(defun org-ml-headline-get-logbook-items (config headline)
  "Return the logbook items of HEADLINE.
See `org-ml-headline-get-supercontents' for the meaning of
CONFIG. The returned items will be a flat list of item nodes,
not a plain-list node."
  (->> (org-ml-headline-get-supercontents config headline)
       (org-ml-supercontents-get-logbook)
       (org-ml-logbook-get-items)))

(defun org-ml-headline-set-logbook-items (config items headline)
  "Set the logbook items of HEADLINE to ITEMS.
See `org-ml-headline-get-supercontents' for the meaning of
CONFIG. ITEMS must be supplied as a flat list of valid logbook
item nodes, not as a plain-list node."
  (org-ml-headline-map-supercontents* config
    (org-ml-supercontents-map-logbook* (org-ml-logbook-set-items items it) it)
    headline))

(org-ml--defun-anaphoric* org-ml-headline-map-logbook-items (config fun headline)
  "Map a function over the logbook items of HEADLINE.
FUN is a unary function that takes a list of item nodes and
returns a modified list of item nodes. See
`org-ml-headline-get-supercontents' for the meaning of CONFIG."
  (--> (org-ml-headline-get-logbook-items config headline)
       (org-ml-headline-set-logbook-items config (funcall fun it) headline)))

(defun org-ml-headline-get-logbook-clocks (config headline)
  "Return the logbook clocks of HEADLINE.
See `org-ml-headline-get-supercontents' for the meaning of
CONFIG. The returned list will include clock nodes and maybe item
nodes if :clock-out-notes is t in CONFIG."
  (->> (org-ml-headline-get-supercontents config headline)
       (org-ml-supercontents-get-logbook)
       (org-ml-logbook-get-clocks)))

(defun org-ml-headline-set-logbook-clocks (config clocks headline)
  "Set the logbook clocks of HEADLINE to CLOCKS.
See `org-ml-headline-get-supercontents' for the meaning of
CONFIG. CLOCKS must be supplied as a flat list of valid clock
nodes and optionally item nodes if :clock-out-notes is t in
CONFIG."
  (org-ml-headline-map-supercontents* config
    (org-ml-supercontents-map-logbook* (org-ml-logbook-set-clocks clocks it) it)
    headline))

(org-ml--defun-anaphoric* org-ml-headline-map-logbook-clocks (config fun headline)
  "Map a function over the logbook clocks of HEADLINE.
FUN is a unary function that takes a list of clock nodes and
optionally item nodes to represent the clock notes and returns a
modified list of said nodes. `org-ml-headline-get-supercontents'
for the meaning of CONFIG."
  (--> (org-ml-headline-get-logbook-clocks config headline)
       (org-ml-headline-set-logbook-clocks config (funcall fun it) headline)))

(defun org-ml-headline-get-contents (config headline)
  "Return the contents of HEADLINE.
Contents is everything in the headline after the logbook and will
be returned as a flat list of nodes. See
`org-ml-headline-get-supercontents' for the meaning of CONFIG."
  (->> (org-ml-headline-get-supercontents config headline)
       (org-ml-supercontents-get-contents)))

(defun org-ml-headline-set-contents (config contents headline)
  "Set the contents of HEADLINE to CONTENTS.
Contents is everything in the headline after the logbook, and
CONTENTS must be a flat list of nodes. See
`org-ml-headline-get-supercontents' for the meaning of CONFIG."
  (org-ml-headline-map-supercontents* config
    (org-ml-supercontents-set-contents contents it)
    headline))

(org-ml--defun-anaphoric* org-ml-headline-map-contents (config fun headline)
  "Map a function over the contents of HEADLINE.
Contents is everything in the headline after the logbook. FUN is
a unary function that takes a list of nodes representing the
contents and returns a modified list of nodes. See
`org-ml-headline-get-supercontents' for the meaning of CONFIG."
  (--> (org-ml-headline-get-contents config headline)
       (org-ml-headline-set-contents config (funcall fun it) headline)))

;; public high-level logbook operations

(defun org-ml-headline-logbook-append-item (config item headline)
  "Append ITEM to the logbook of HEADLINE.
See `org-ml-headline-get-supercontents' for the meaning of
CONFIG. ITEM must be a valid logbook item. The logbook will be
started if it does not already exist, else ITEM will be added in
chronological order."
  (org-ml-headline-map-logbook-items* config (cons item it) headline))

(defun org-ml-headline-logbook-append-open-clock (config unixtime headline)
  "Append an open clock to the logbook of HEADLINE.
See `org-ml-headline-get-supercontents' for the meaning of
CONFIG. UNIXTIME will set the start time of the clock. The
logbook will be started if it does not already exist, else the
new clock will be added in chronological order."
  (let ((clock (-> (org-ml-unixtime-to-datetime unixtime)
                   (org-ml-build-clock!))))
    (org-ml-headline-map-logbook-clocks* config (cons clock it) headline)))

(defun org-ml-headline-logbook-close-open-clock (config unixtime note headline)
  "Close an open clock to the logbook of HEADLINE.
See `org-ml-headline-get-supercontents' for the meaning of
CONFIG. UNIXTIME will set the end time of the clock. This will
only close an open clock if it is the most recent clock; else it
will do nothing. NOTE is a string representing the clock-out
note (or nil if not desired). Note that supplying a non-nil
clock-note when it is not allowed by CONFIG will trigger an
error."
  (org-ml-headline-map-logbook-clocks* config
    (-let (((first . rest) it))
      (if (not (and first (org-ml-clock-is-running first))) it
        (let* ((time (org-ml-unixtime-to-datetime unixtime))
               (closed (->> first
                            ;; NOTE making copies here is necessary
                            (org-ml-map-property* :value
                              (org-ml-timestamp-set-end-time time it))))
               (note* (-some->> note
                        (org-ml-build-paragraph)
                        (org-ml-build-item))))
          (if note* `(,closed ,note* ,@rest) (cons closed rest)))))
    headline))

(defun org-ml-headline-logbook-convert-config (config1 config2 headline)
  "Convert the logbook of HEADLINE to a new configuration.
CONFIG1 is the current config and CONFIG2 is the target config.
Note that any logbook nodes that are invalid under CONFIG1 will
be silently dropped, and nodes which do not conform to CONFIG2
will trigger an error. See `org-ml-headline-get-supercontents'
for the structure of both config lists."
  (--> (org-ml-headline-get-supercontents config1 headline)
       (org-ml-headline-set-supercontents config2 it headline)))

;; misc

(defun org-ml-headline-get-path (headline)
  "Return tree path of HEADLINE node.

The return value is a list of headline titles (including that from
HEADLINE) leading to the root node."
  (org-ml--check-type 'headline headline)
  (->> (org-ml-get-parents headline)
       (--map (org-element-property :raw-value it))))

(defun org-ml-headline-update-item-statistics (headline)
  "Return HEADLINE node with updated statistics cookie via items.

The percent/fraction will be computed as the number of checked items
over the number of items with checkboxes (non-checkbox items will
not be considered)."
  (let* ((items
          (->> (org-ml-headline-get-section headline)
               (org-element-contents)
               (--filter (org-ml--is-type 'plain-list it))
               (-mapcat #'org-element-contents)
               (--filter (org-element-property-raw :checkbox it))))
         (done (length (--filter (org-ml--property-is-eq :checkbox 'on it)
                                 items)))
         (total (length items)))
    (->> (org-ml-copy headline)
         (org-ml--headline-set-statistics-cookie-fraction done total))))

(defun org-ml-headline-update-todo-statistics (headline)
  "Return HEADLINE node with updated statistics cookie via subheadlines.

The percent/fraction will be computed as the number of done
subheadlines over the number of todo subheadlines (eg non-todo
subheadlines will not be counted)."
  (let* ((subtodo (->> (org-ml-headline-get-subheadlines headline)
                       (--filter (org-element-property :todo-keyword it))))
         (done (length (-filter #'org-ml-headline-is-done subtodo)))
         (total (length subtodo)))
    (->> (org-ml-copy headline)
         (org-ml--headline-set-statistics-cookie-fraction done total))))

;;; plain-list

;; TODO there seems to be a bug in the interpreter that prevents "+" bullets from
;; being recognized (as of org-9.1.9 they are simply read as "-")
(defun org-ml-plain-list-set-type (type plain-list)
  "Return PLAIN-LIST node with type property set to TYPE.
TYPE is one of the symbols `unordered' or `ordered'."
  (org-ml--check-type 'plain-list plain-list)
  (cond
   ((eq type 'unordered)
    (org-ml--map-children-nocheck*
      (--map (org-ml-set-property :bullet '- it) it)
      plain-list))
   ((eq type 'ordered)
    ;; NOTE the org-interpreter seems to use the correct, ordered numbers if any
    ;; number is set here. This behavior may not be reliable.
    (org-ml--map-children-nocheck*
      (--map (org-ml-set-property :bullet 1 it) it)
      plain-list))
   (t (org-ml--arg-error "Invalid type: %s" type))))

;;; table

(defun org-ml-table-get-cell (row-index column-index table)
  "Return table-cell node at ROW-INDEX and COLUMN-INDEX in TABLE node.
Rule-type rows do not count toward row indices."
  (org-ml--check-type 'table table)
  (->> (org-ml--table-get-row row-index table)
       (org-element-contents)
       (org-ml--nth column-index)))

(defun org-ml-table-delete-row (row-index table)
  "Return TABLE node with row at ROW-INDEX deleted."
  (org-ml--check-type 'table table)
  (org-ml--map-children-nocheck* (org-ml--remove-at row-index it) table))

(defun org-ml-table-delete-column (column-index table)
  "Return TABLE node with column at COLUMN-INDEX deleted."
  (org-ml--check-type 'table table)
  (org-ml--map-children-nocheck*
   (--map
    (if (org-ml--property-is-eq :type 'rule it) it
      (org-ml--map-children-nocheck* (org-ml--remove-at column-index it) it))
    it)
   table))

(defun org-ml-table-insert-column! (column-index column-text table)
  "Return TABLE node with COLUMN-TEXT inserted at COLUMN-INDEX.

COLUMN-INDEX is the index of the column and COLUMN-TEXT is a list of
strings to be made into table-cells to be inserted following the same
syntax as `org-ml-build-table-cell!'."
  (org-ml--check-type 'table table)
  (let ((column (-map #'org-ml-build-table-cell! column-text)))
    (org-ml--column-map-down-rows
     (lambda (new-cell cells) (org-ml--insert-at column-index new-cell cells))
     column
     table)))

(defun org-ml-table-insert-row! (row-index row-text table)
  "Return TABLE node with ROW-TEXT inserted at ROW-INDEX.

ROW-INDEX is the index of the column and ROW-TEXT is a list of strings
to be made into table-cells to be inserted following the same syntax
as `org-ml-build-table-row!'."
  (org-ml--check-type 'table table)
  (if (not row-text) (org-ml--table-clear-row row-index table)
    (let ((row (->> (org-ml-build-table-row! row-text)
                    (org-ml--table-row-pad-maybe table))))
      (org-ml--map-children-nocheck*
        (org-ml--insert-at row-index row it)
        table))))

(defun org-ml-table-replace-cell! (row-index column-index cell-text table)
  "Return TABLE node with a table-cell node replaced by CELL-TEXT.

If CELL-TEXT is a string, it will replace the children of the
table-cell at ROW-INDEX and COLUMN-INDEX in TABLE. CELL-TEXT will be
processed the same as the argument given to `org-ml-build-table-cell!'.

If CELL-TEXT is nil, it will set the cell to an empty string."
  (org-ml--check-type 'table table)
  (let* ((cell (if cell-text (org-ml-build-table-cell! cell-text)
                 (org-ml-build-table-cell "")))
         (row (->> (org-ml--table-get-row row-index table)
                   (org-ml--map-children-nocheck*
                     (org-ml--replace-at column-index cell it)))))
    (org-ml--table-replace-row row-index row table)))

(defun org-ml-table-replace-column! (column-index column-text table)
  "Return TABLE node with the column at COLUMN-INDEX replaced by COLUMN-TEXT.

If COLUMN-TEXT is a list of strings, it will replace the table-cells
at COLUMN-INDEX. Each member of COLUMN-TEXT will be processed the
same as the argument given to `org-ml-build-table-cell!'.

If COLUMN-TEXT is nil, it will clear all cells at COLUMN-INDEX."
  (org-ml--check-type 'table table)
  (if (not column-text) (org-ml--table-clear-column column-index table)
    (let ((column-cells (-map #'org-ml-build-table-cell! column-text)))
      (org-ml--table-replace-column column-index column-cells table))))

(defun org-ml-table-replace-row! (row-index row-text table)
  "Return TABLE node with the row at ROW-INDEX replaced by ROW-TEXT.

If ROW-TEXT is a list of strings, it will replace the cells at
ROW-INDEX. Each member of ROW-TEXT will be processed the same as
the argument given to `org-ml-build-table-row!'.

If ROW-TEXT is nil, it will clear all cells at ROW-INDEX."
  (org-ml--check-type 'table table)
  (let ((row-cells (org-ml-build-table-row! row-text)))
    (org-ml--table-replace-row row-index row-cells table)))

;;; INDENTATION FUNCTIONS

;; NOTE: for headlines, promote = outdent, and demote = indent

;;; indentation (single and tree)

;; high level steps to indent
;;
;; Assume an abstract tree thing like this:
;; - 0.
;; - 1.
;;   - 1.0
;; - 2.
;;
;; We wish to indent 1. There are two cases:
;; 1. indent only 1.
;; 2. indent 1. and 1.0 along with it
;;
;; In both cases, make 1.0 a child of 0. Remove 1.0 from the
;; top-level list and leave 1.0 and 2. untouched
;;
;; In case 2, this is all that is needed since 1.0 is already a child of 1. and
;; will "autoindent" as 1. itself is moved.
;;
;; To make it "stay in place," as in case 1, remove 1.0 as a child of
;; 1., append it to the end of the list containing 2., and set this list (with
;; both 1. and 1.0.) as the child of 0.
;;
;; parameters for indenting:
;; - index of target to indent (1 in above example)

;; TODO these mostly work except for whitespace edgecases, and those are really
;; ugly to work around

(defmacro org-ml--tree-set-child* (index form tree)
  "Return TREE with node at INDEX set as child of the node before it.
FORM is a Lisp form that takes the last member of TREE
immediately before INDEX (called \"parent\", bound to `it') and
the item at INDEX to be set as its child (bound to `it-target')
and returns a new \"parent\" node."
  (declare (indent 1) (debug (form form form)))
  (let ((i (make-symbol "index"))
        (h (make-symbol "head"))
        (T (make-symbol "tail")))
    `(let ((,i ,index))
       (unless (and (integerp ,i) (< 0 ,index))
         (error "Cannot indent topmost item at this level"))
       (-let (((,h ,T) (-split-at ,i ,tree)))
         (if (not ,T) (error "Index over range: %s" ,i)
           (let ((it-target (car ,T)))
             (append (org-ml--map-last* ,form ,h) (cdr ,T))))))))

(defun org-ml--headline-move-post-blank (headline)
  "Move :post-blank to :pre-blank if HEADLINE is totally empty."
  (if (org-element-contents headline) headline
    (let ((pre (org-element-property :pre-blank headline))
          (post (org-element-post-blank headline)))
      (org-ml--set-properties-raw (org-ml-copy headline)
        :pre-blank (+ pre post)
        :post-blank 0))))

;; headline

(defun org-ml-headline-demote-subtree (index headline)
  "Return HEADLINE node with child headline at INDEX demoted.
Unlike `org-ml-headline-demote-subheadline' this will also demote the
demoted headline node's children."
  (org-ml-headline-map-subheadlines*
    (org-ml--tree-set-child* index
      (org-ml--map-children-nocheck*
       (-snoc it (org-ml--headline-subtree-shift-level 1 it-target))
       (org-ml--headline-move-post-blank it))
      it)
    headline))

(defun org-ml-headline-demote-subheadline (index headline)
  "Return HEADLINE node with child headline at INDEX demoted.
Unlike `org-ml-headline-demote-subtree' this will not demote the
demoted headline node's children."
  (org-ml-headline-map-subheadlines*
    (org-ml--tree-set-child* index
      (let* ((headlines-in-target (org-ml-headline-get-subheadlines it-target))
             (tgt-children (org-element-contents it-target))
             (tgt-pb (if (org-ml--is-type 'section (car tgt-children))
                         (org-ml--get-post-blank-textsafe (car tgt-children))
                       (org-element-property-raw :pre-blank it-target)))
             (tgt-headline* (->> (org-ml-copy it-target)
                                 (org-ml-headline-set-subheadlines nil)
                                 (org-ml--headline-shift-level 1)
                                 (org-ml--set-post-blank tgt-pb))))
        (org-ml--map-children-nocheck*
         (append it (list tgt-headline*) headlines-in-target)
         (org-ml--headline-move-post-blank it)))
      it)
    headline))

;; plain-list

(defun org-ml-plain-list-indent-item-tree (index plain-list)
  "Return PLAIN-LIST node with child item at INDEX indented.
Unlike `org-ml-item-indent-item' this will also indent the indented item
node's children."
  (org-ml--check-type 'plain-list plain-list)
  (org-ml--map-children-nocheck*
   (org-ml--tree-set-child* index
     (let ((parent-pb (org-element-post-blank it))
           (indented-target (org-ml--set-post-blank 0 it-target)))
       (org-ml--item-map-subcomponents-cond*
           ;; If neither subitems nor rest present, add indented tree as new
           ;; subitem under parent. Put the parent post-blank at the end of
           ;; the header material.
           (list (org-ml--shift-last-post-blank parent-pb it-head)
                 `(,indented-target) 0 nil)
           ;; If rest not present but subitems present, append indented tree
           ;; to the end of these subitems. Put the parent post-blank at the
           ;; end of the list of subitems.
           (list (-snoc
                  (org-ml--shift-last-post-blank parent-pb it-subitems)
                  indented-target)
                 0 nil)
           ;; If rest present, append indented item tree to the end of rest.
           ;; Add the post-blank from parent to the end the last node in rest
             (cons
              (org-ml--shift-last-post-blank parent-pb it-rest)
              (org-ml-build-plain-list indented-target))
        it))
     it)
   plain-list))

(defun org-ml-plain-list-indent-item (index plain-list)
  "Return PLAIN-LIST node with child item at INDEX indented.
Unlike `org-ml-item-indent-item-tree' this will not indent the indented
item node's children."
  (org-ml--check-type 'plain-list plain-list)
  (org-ml--map-children-nocheck*
   (org-ml--tree-set-child* index
     ;; Get the target item (the one to be indented) and set its children
     ;; to nil. Assume that its subitems and anything after it (including the
     ;; post-blank) will not change.
     (-let* (((tgt-head tgt-subitems tgt-rest-pb tgt-rest)
              (org-ml--item-get-subcomponents it-target))
             (indented-target (org-ml--item-set-subcomponents
                               `(,tgt-head nil nil nil) it-target))
             ;; NOTE: this is the post-blank of the *topmost* item in front of
             ;; the one to be indented. Any space after the the last subitem (if
             ;; any) are reflected in this (more to come below).
             (parent-pb (org-element-post-blank it)))
       (org-ml--item-map-subcomponents-cond*
           (list
            (org-ml--shift-last-post-blank parent-pb it-head)
            (cons indented-target tgt-subitems)
            0 nil)
           ;; Otherwise, add the indented target and its children to the
           ;; subitems under the parent, and set the rest to be that of the
           ;; target (if anything). The only tricky part here is to most the
           ;; post-blank of the toplevel parent into the last subitem of the
           ;; parent (otherwise the post blank would move to the end of the
           ;; entire new list after indentation)
           (let ((psub (org-ml--shift-last-post-blank parent-pb it-subitems)))
             `((,@psub ,indented-target ,@tgt-subitems) 0 nil))
           ;; If the parent has "extra stuff" underneath its subitems (ie
           ;; "rest") then we need to append the indented item after this "extra
           ;; stuff." Make a new list with the indented item and its children
           ;; (which will be at the same level after the target is indented)
           (let ((rest*
                  (->> (cons indented-target tgt-subitems)
                       (apply #'org-ml-build-plain-list :post-blank tgt-rest-pb)
                       (cons it-rest))))
             `(,@rest* ,@tgt-rest))
         it))
     it)
   plain-list))

;;; unindentation (tree)

;; high level steps to unindent a tree
;;
;; Assume an abstract tree thing like this:
;; - 0.
;; - 1.
;;   - 1.0.
;;   - 1.1.
;;     - 1.1.0.
;;   - 1.2.
;; - 2.
;;
;; We want to unindent everything under 1. So just take all children of 1. and
;; splice them into the top-level list between 1. and 2. In this case 1.1.0 will
;; remain a child of 1.1 but it will be unindented as well because its parent is
;; being unindented
;;
;; parameters for unindenting a tree:
;; - the index whose children are to be unindented

(defmacro org-ml--split-children-at-index* (index form tree)
  "Return TREE with node at INDEX split according to FORM.
The node at INDEX will be bound to the symbol `it' which is to be
referenced in FORM, and FORM is to return a list like (PARENT
CHILDREN) where PARENT is the modified node at INDEX and CHILDREN
is a list of nodes that were children under PARENT but are to be
spliced after parent. The new TREE will effectively splice the
CHILDREN nodes after PARENT at the same level as PARENT."
  (declare (indent 1) (debug (form form form)))
  (let ((head (make-symbol "head"))
        (tail (make-symbol "tail")))
    `(-let* (((,head ,tail) (-split-at ,index ,tree))
             (it (car ,tail))
             ((parent children) ,form))
       `(,@,head ,parent ,@children ,@(cdr ,tail)))))

;; headline

(defun org-ml-headline-promote-all-subheadlines (index headline)
  "Return HEADLINE node with all child headlines under INDEX promoted."
  (org-ml-headline-map-subheadlines*
    (org-ml--split-children-at-index* index
      (let* ((children (->> (org-element-contents it)
                            (--map (org-ml--headline-subtree-shift-level -1 it))))
             (parent (org-ml--set-children-nocheck nil it))
             (parent* (org-ml--headline-move-post-blank parent)))
        (list parent* children))
      it)
    headline))

;; plain-list

(defun org-ml-plain-list-outdent-all-items (index plain-list)
  "Return PLAIN-LIST node with all child items under INDEX outdented."
  (org-ml--check-type 'plain-list plain-list)
  (org-ml--map-children-nocheck*
   (org-ml--split-children-at-index* index
     (-let* (((tgt-head tgt-subitems tgt-rest-pb tgt-rest)
              (org-ml--item-get-subcomponents it))
             (parent-pb (or (org-element-post-blank (-last-item tgt-head)) 0))
             (parent (->> (org-ml--item-set-subcomponents `(,tgt-head nil nil nil) it)
                          (org-ml--set-post-blank parent-pb)))
             (outdent-pb (org-element-post-blank it))
             (outdented
              (cond
               (tgt-rest
                (append
                 (org-ml--shift-last-post-blank tgt-rest-pb tgt-subitems)
                 (org-ml--shift-last-post-blank outdent-pb tgt-rest)))
               (tgt-subitems
                (org-ml--shift-last-post-blank outdent-pb tgt-subitems))
               (t
                nil))))
       `(,parent ,outdented))
     it)
   plain-list))

;;; unindentation (single target)

;; high level steps to unindent a single item
;;
;; Assume an abstract tree thing like this:
;; - 0.
;; - 1.
;;   - 1.0.
;;   - 1.1.
;;     - 1.1.0.
;;   - 1.2.
;; - 2.
;; 
;; We want to unindent 1.1. First, indent everything after 1.1 (in this case
;; only 1.2, which will be appended to the list starting with 1.1.0). Then move
;; 1.1 (with 1.1.0 and 1.2 as children) between 1 and 2 in the toplevel list.
;;
;; parameters for unindenting:
;; - parent index (in this case 1 for 1.)
;; - child index (in this case 1 for 1.1)

;; headline

;; TODO trigger error when child-index is out of range

(defun org-ml-headline-promote-subheadline (index child-index headline)
  "Return HEADLINE node with a child headline under INDEX promoted.
The specific child headline to promote is selected by CHILD-INDEX."
  (org-ml-headline-map-subheadlines*
    (org-ml--split-children-at-index* index
      (-let* (((head tail) (-split-at child-index (org-element-contents it)))
              (target (->> (car tail)
                           (org-ml-copy)
                           (org-ml--headline-subtree-shift-level -1)
                           (org-ml-headline-map-subheadlines*
                             (append it (cdr tail)))))
              (parent (org-ml--set-children-nocheck head it)))
        (list parent (list target)))
      it)
    headline))

;; plain-list

(defun org-ml-plain-list-outdent-item (index child-index plain-list)
  "Return PLAIN-LIST node with a child item under INDEX outdented.
The specific child item to outdent is selected by CHILD-INDEX."
  (org-ml--check-type 'plain-list plain-list)
  (org-ml--map-children-nocheck*
   (org-ml--split-children-at-index* index
     (-let* (((parent-head parent-subitems parent-rest-pb parent-rest)
              (org-ml--item-get-subcomponents it))
             ((above-outdent (to-outdent . outdent-subitems))
              (-split-at child-index parent-subitems))
             (parent-pb (or (org-element-post-blank (-last-item above-outdent)) 0))
             ;; Make new parent with the subitems that are above the item to
             ;; be outdented (if any) and remove its rest component since this
             ;; will become part of the outdented item's children
             (parent (->> (org-ml--set-post-blank parent-pb it)
                          (org-ml--item-set-subcomponents
                           `(,parent-head ,above-outdent 0 nil))))
             (tgt-pb (org-element-post-blank to-outdent)))
       (if (not to-outdent) `(,parent nil)
         (let ((outdented
                (org-ml--item-map-subcomponents-cond*
                    (list (org-ml--set-last-post-blank tgt-pb it-head)
                          outdent-subitems
                          0 nil)
                    (-> (org-ml--shift-last-post-blank tgt-pb it-subitems)
                        (append outdent-subitems)
                        (list 0 nil))
                    (let ((psub (apply #'org-ml-build-plain-list
                                       :post-blank parent-rest-pb
                                       outdent-subitems)))
                      `(,@it-rest ,psub ,@parent-rest))
                 to-outdent)))
           `(,parent (,outdented)))))
     it)
   plain-list))

;;; PRINTING FUNCTIONS

;; For the most part, printing a node only involves
;; `org-element-interpret-data', except this function has several limitations to
;; work around
;; - printing the string 'nil' where there should be a blank string
;; - printing the node when it should not be printed at all
;; - throwing an error when blank

;;; printing workaround functions

(defun org-ml--set-blank-children (node)
  "Set the children of NODE to a blank string (\"\")."
  (org-ml--set-children-nocheck '("") node))

;; Some objects and greater elements should be removed if blank. Table and plain
;; list will error, and the others make no sense if they are empty.
(defconst org-ml--rm-if-empty
  '(table plain-list bold italic radio-target strike-through
          superscript subscript table-cell underline)
  "Nodes that will be blank if printed and empty.
This is a workaround for a bug")

;; Some greater elements will print "nil" in their children if they are empty.
;; The workaround for this is to set the children to a single blank string if
;; empty
(defconst org-ml--blank-if-empty
  '(center-block drawer dynamic-block property-drawer quote-block
                 special-block table-cell verse-block)
  "Branch element nodes that require \"\" to correctly print empty.
This is a workaround for a bug.")

;; TODO do I still need this in 9.7?
(defun org-ml--blank (node)
  "Return NODE with empty child nodes `org-ml--blank-if-empty' set to contain \"\"."
  (if (not (org-element-contents node))
      (cond
       ((org-ml--is-any-type org-ml--blank-if-empty node)
        (org-ml--set-blank-children node))
       (t
        (unless (or (org-ml--is-any-type org-ml--rm-if-empty node)
                    (org-ml--is-table-row node))
          node)))
    (org-ml--map-children-nocheck*
     (remove nil (-map #'org-ml--blank it))
     node)))

;;; print functions

(defun org-ml-to-string (node)
  "Return NODE as an interpreted string without text properties."
  (cond
   ((null node)
    "")
   ((org-ml--is-node node)
    (let ((s (->> (org-ml--blank node)
                  (org-element-interpret-data)
                  (substring-no-properties))))
      (if (not (org-ml--is-type 'section node)) s
        ;; TODO this is a bug in 9.7; sections now don't carry a post-blank
        ;; property, and instead assume that the post-blank is encoded in the
        ;; underlying contents. Unfortunately, `org-element-interpret-data' will
        ;; normalize the underlying contents (as a string) to only have one
        ;; newline regardless of post-blank. This workaround will manually add
        ;; the newlines back in the case of section nodes
        (let ((pb (->> (org-element-contents node)
                       (-last-item)
                       (org-element-post-blank))))
          (concat s (make-string pb ?\n))))))
   (t
    (org-ml--arg-error "Can only stringify node or nil, got %s" node))))

(defun org-ml-to-trimmed-string (node)
  "Like `org-ml-to-string' but strip whitespace when returning NODE."
  (-some->> (org-ml-to-string node) (s-trim)))

;;; inverse printing functions

(defun org-ml-from-string (type string)
  "Convert STRING to a node.
TYPE is the node type intended by STRING; if STRING cannot be
parsed into TYPE this function will return nil."
  (cl-flet*
      ((string-to-post-blank
        (s)
        (-let (((b . e) (car (s-matched-positions-all "\n+$" s))))
          (if (not (and b e)) 0
            (let ((d (- e b)))
              (if (= 1 d) 0 d)))))
       (shift-property
        (prop n node)
        (org-ml--map-property-raw* prop (+ n it) node))
       (shift-property-maybe
        (prop n node)
        (org-ml--map-property-raw* prop (when it (+ n it)) node))
       (shift-object-node
        (n node)
        (->> (shift-property :begin n node)
             (shift-property :end n)))
       (shift-branch-object-node
        (n node)
        (->> (shift-object-node n node)
             (shift-property-maybe :contents-begin n)
             (shift-property-maybe :contents-end n)))
       (shift-element-node
        (n node)
        (->> (shift-object-node n node)
             (shift-property :post-affiliated n)))
       (shift-branch-element-node
        (n node)
        (->> (shift-element-node n node)
             (shift-property-maybe :contents-begin n)
             (shift-property-maybe :contents-end n)))
       (shift-property-node
        (prop n node)
        (org-ml--map-property-raw* prop
          (-some->> it (shift-object-node n))
          node))
       (decrement-object-node
        (node)
        (if (org-ml-is-branch-node node) (shift-branch-object-node -1 node)
          (shift-object-node -1 node)))
       (decrement-node
        (node)
        (if (org-ml-is-element node)
            (if (org-ml-is-branch-node node) (shift-branch-element-node -1 node)
              (shift-element-node -1 node))
          (decrement-object-node node)))
       (remove-leading-space-maybe
        (node)
        (org-ml--map-children-nocheck*
         (org-ml--map-first*
          (if (org-ml--is-type 'paragraph it)
              (org-ml--map-children-nocheck*
               (if (equal (car it) " ") (cdr it)
                 (org-ml--map-first*
                  (substring it 1)
                  it))
               it))
          it)
         node))
       (from-prefixed-string
        (prefix level string)
        (-if-let (x (->> (concat prefix string)
                         (org-ml--from-string)
                         (org-ml--get-descendent level)))
            (->> (shift-branch-object-node -1 x)
                 (org-ml-match-map '((:not plain-text) *) #'decrement-object-node)))))
    (-some->> (cond
               ((eq type 'paragraph)
                (let* ((pb (string-to-post-blank string))
                       (e (1+ (length string)))
                       (ce (- e pb)))
                  (-> (org-ml-build-paragraph! string :post-blank pb)
                    (org-ml--set-properties-raw
                        :begin 1
                        :contents-begin 1
                        :end e
                        :contents-end ce))))
               ((and (eq type 'section) (s-matches-p "^\\*" string))
                (->> (concat " " string)
                     (org-ml--from-string)
                     (remove-leading-space-maybe)
                     (shift-property :end -1)
                     (shift-property-maybe :contents-end -1)
                     (org-ml-match-map '((:and 0 (:not plain-text)) *)
                       (lambda (node)
                         (->> (if (not (org-ml-is-branch-node node)) node
                                (shift-property :contents-end -1 node))
                              (shift-property :end -1))))
                     (org-ml-match-map '((:and (> 0) (:not plain-text)) *)
                       #'decrement-node)))
               ((eq type 'node-property)
                (let* ((pb (string-to-post-blank string))
                       (e (1+ (- (length string) pb))))
                  (-if-let (x (->> (format "* dummy\n:PROPERTIES:\n%s\n:END:" string)
                                   (org-ml--from-string)
                                   (org-ml--get-descendent '(0 0 0))))
                      (org-ml--set-properties-raw x
                        :post-affiliated 1
                        :begin 1
                        :post-blank pb
                        :end e))))
               ((eq type 'property-drawer)
                (-if-let (x (->> (concat "* dummy\n" string)
                                 (org-ml--from-string)
                                 (org-ml--get-descendent '(0 0))))
                    (->> (shift-branch-element-node -8 x)
                         (org-ml--map-children-nocheck*
                          (--map (shift-element-node -8 it) it)))))
               ((eq type 'planning)
                (-if-let (x (->> (concat "* dummy\n" string)
                                 (org-ml--from-string)
                                 (org-ml--get-descendent '(0 0))))
                    (->> (shift-element-node -8 x)
                         (shift-property-node :scheduled -8)
                         (shift-property-node :deadline -8)
                         (shift-property-node :closed -8))))
               ((eq type 'bold)
                (from-prefixed-string " " '(0 1) string))
               ((memq type '(superscript subscript))
                (from-prefixed-string "s" '(0 1) string))
               ((eq type 'table-cell)
                (from-prefixed-string "|" '(0 0 0) string))
               (t (let ((level (cond
                                ((eq type 'headline) nil)
                                ((eq type 'section) nil)
                                ((eq type 'item) '(0 0))
                                ((eq type 'table-row) '(0 0))
                                ((memq type org-ml-objects) '(0 0))
                                (t '(0)))))
                    (->> (org-ml--from-string string)
                         (org-ml--get-descendent level)))))
      (org-element-put-property-2 :parent nil))))

;;; PATTERN MATCHING

;; This is a framework for applying "pattern matching" on node trees. All these
;; functions search through the node tree and return (and sometimes operate on)
;; a list of matches much like the UNIX find function for searching filesystems.

;; Patterns are composed of the following parts:
;; conditions - match a node based on its type, properties, and index
;; wildcards - keywords that match one of more nodes regardless of type,
;;   properties, and index
;; slicers - keywords with arguments that limit the returned match list to a
;;   subset all matches (such as first match or 2nd - 5th matches)
;;
;; Of the above, only conditions are required in the pattern

;; When a pattern is fed into any of the match functions, it will first be
;; 'compiled' into a lambda function that will walk through the node tree and
;; accumulate/return the results the pattern requests. All possible functions
;; operate on the same data structure which is a list of children in the tree at
;; each level with the indices cons'ed to them like ((L . R) . CHILD) where L is
;; the left index and R is the right index (starting from -1 and counting down
;; to the left given a list of children in a node). The right index is necessary
;; for negative index matching.

;; This data structure ensures that any child has the information necessary for
;; a condition form to determine if a match is successful (if this data
;; structure wasn't used, index matching would fail as it would require
;; knowledge of the entire list when the match is made)

;; For slicers, two tricks are used to ensure that work is minimized. The first
;; is that searches are limited to the maximum number of matches needed. If only
;; the first match is needed, the search will stop after one match. If the 2nd
;; to 5th matches are needed, the search will stop after 5 matches and return
;; this with the first match dropped. The second trick is that the search is
;; reversed if the slicer requests negatively indexed results. If the last match
;; is needed, reverse the tree and return the first result. These tricks are
;; possible/easier with the indexed-children data structure described above as
;; it ensures that indexing information is preserved even when the children are
;; reversed, and ensures that matching can be made on one child node at a time,
;; which guarantees the limit will never be overshot.

(defmacro org-ml--map-indexed (reverse? form list)
  "Like `--map-indexed' but can be told to reverse the result.
If REVERSE? is t, the final results are reversed (which actually
means not reversed since the results are made in reverse order).
FORM and LIST carry the same meaning."
  (declare (indent 1))
  (let* ((r (make-symbol "result"))
         (return (if reverse? r `(nreverse ,r))))
    `(let (,r)
       (--each ,list (!cons ,form ,r))
       ,return)))

(defmacro org-ml--get-children-indexed (reverse? node)
  "Return list of children from NODE (if any) with index annotations.
If REVERSE is t, reverse the final result."
  `(let* ((children (org-element-contents ,node))
          (len (- (length children))))
     (org-ml--map-indexed ,reverse?
       (cons `(,it-index . ,(+ len it-index)) it) children)))

(defmacro org-ml--reduce-from-while (pred form initial-value list)
  "Like `--reduce-from' but only reduce LIST while PRED is t.
FORM and INITIAL-VALUE work the same way, and the exposed symbols `it'
and `acc' carry the same meaning."
  (declare (debug (form form form form)))
  `(let ((acc ,initial-value))
     (--each-while ,list ,pred (setq acc ,form))
     acc))

(defun org-ml--match-make-condition-form (condition)
  "Return a Lisp form equivalent to CONDITION.
Assume that `it' is a symbol bound to a list of the form
\((INDEX RINDEX) . NODE) where NODE is the node being matched to
CONDITION, INDEX is the INDEX of NODE, and RINDEX is the right-index
of NODE (starting at -1 on the rightmost side of the children list)."
  ;; initialize some 'accessor' forms
  (let ((it-node '(cdr it))
        (it-lindex '(caar it))
        (it-rindex '(cdar it)))
    (pcase condition
      ;;
      ;; condition should not be nil
      (`nil
       (org-ml--arg-error "Condition cannot be nil"))
      ;;
      ;; quote is invalid (may be accidentally in condition)
      (`(quote . ,_)
       (org-ml--arg-error "'quote' not allowed in condition"))
      ;;
      ;; function is invalid (may be accidentally in condition)
      (`(function . ,_)
       (org-ml--arg-error "'function' not allowed in condition"))
      ;;
      ;; literal node
      ((and (pred org-ml--is-node) pattern-node)
       `(equal ,it-node ',pattern-node))
      ;;
      ;; type
      ((and (pred (lambda (y) (memq y org-ml-nodes))) type)
       `(org-ml-is-type ',type ,it-node))
      ;; 
      ;; index
      ((and (pred integerp) index)
       `(= ,index ,(if (< index 0) it-rindex it-lindex)))
      ;; 
      ;; relative index
      (`(,(and (or '< '<= '> '>=) op) ,(and (pred integerp) index))
       `(funcall #',op ,(if (< index 0) it-rindex it-lindex) ,index))
      ;; 
      ;; predicate
      (`(:pred . (,pred . nil))
       `(funcall #',pred ,it-node))
      ;; 
      ;; not
      (`(:not . (,p . nil))
       `(not ,(org-ml--match-make-condition-form p)))
      ;; 
      ;; and
      (`(:and . ,(and (pred and) p))
       `(and ,@(-map #'org-ml--match-make-condition-form p)))
      ;; 
      ;; or
      (`(:or . ,(and (pred and) p))
       `(or ,@(-map #'org-ml--match-make-condition-form p)))
      ;;
      ;; property
      ;; NOTE: this must go last if we don't want :pred/:and/:or/:not
      ;; to be interpreted as a property
      (`(,(and (pred keywordp) prop) . (,val . nil))
       `(equal (org-ml-get-property ,prop ,it-node) ,val))
      ;;
      ;; :any
      (:any t)
      ;;
      (p (org-ml--arg-error "Invalid condition: %s" p)))))

(defun org-ml--match-pattern-make-inner-form (end? limit pattern)
  "Return matching form for PATTERN.
END? is a boolean describing if the search should be made in reverse.
If t, reverse all children when obtaining from any given node. LIMIT
is an integer or nil describing the number of matches at which the
search should terminate. If nil, don't perform any checks and
terminate only when the entire tree is searched within PATTERN."
  (let* ((accum '(cons (cdr it) acc))
         (get-children `(org-ml--get-children-indexed ,end? (cdr it)))
         (reduce (if (not limit) '(--reduce-from)
                   `(org-ml--reduce-from-while (< (length acc) ,limit)))))
    (pcase pattern
      ;; slicers should not be here
      (`(,(or :first :last :nth :slice) . ,_)
       (org-ml--arg-error "Slicers can only appear at the front of pattern"))
      ;; empty pattern - add current node to accumulator as-is
      ('nil
       accum)
      ;; * - if condition0 and condition1 match, add node to accumulator and
      ;;   descend into child to repeat, if only condition0 matches just descend
      ;;   into child and continue
      (`(,condition . (* . nil))
       (let* ((pred (org-ml--match-make-condition-form condition))
              ;; need to explicitly check limit here because not in reduce form
              ;; where limit is build in, this doesn't conform to the pattern of
              ;; the rest of this function
              (add-maybe
               (if limit `(if (< (length acc) ,limit) ,accum acc) accum))
              (add-descend
               (if (not end?) `(get-many ,add-maybe ,get-children)
                 `(let ((acc (get-many acc ,get-children)))
                    ,add-maybe))))
         `(cl-labels
              ((get-many
                (acc children)
                (,@reduce (if ,pred ,add-descend acc) acc children)))
            (let ((acc ,accum))
              (get-many acc ,get-children)))))
      (`(,condition0 . (* . ,ps))
       (let* ((condition1 (car ps))
              (ps (cdr ps))
              (pred0 (org-ml--match-make-condition-form condition0))
              (pred1 (org-ml--match-make-condition-form condition1))
              (inner
               (if (not ps) accum
                 (org-ml--match-pattern-make-inner-form end? limit ps)))
              ;; need to explicitly check limit here because not in reduce form
              ;; where limit is build in, this doesn't conform to the pattern of
              ;; the rest of this function
              (add-maybe
               (if (not limit) `(if ,pred1 ,inner acc)
                 `(if (and (< (length acc) ,limit) ,pred1) ,inner acc)))
              (add-descend
               (if end? `(let ((acc (get-many acc ,get-children)))
                           ,add-maybe)
                 `(get-many ,add-maybe ,get-children))))
         `(cl-labels
              ((get-many
                (acc children)
                (,@reduce (cond (,pred0 ,add-descend)
                                (,pred1 ,inner)
                                (t acc))
                          acc children)))
            (get-many acc ,get-children))))
      ;; condition - descend into the children of matching nodes and either
      ;;   continue searching or add to accumulator if no more conditions to
      ;;   match
      (`(,condition . ,ps)
       (let ((pred (org-ml--match-make-condition-form condition))
             (inner
              (if (not ps) accum
                (org-ml--match-pattern-make-inner-form end? limit ps))))
         `(,@reduce (if ,pred ,inner acc) acc ,get-children)))
      ;;
      (ps (org-ml--arg-error "Invalid pattern: %s" ps)))))

(defun org-ml--match-is-alternate-form (form)
  "Return t if FORM is an alternative pattern form (eg has `|`s)."
  (and (listp form) (memq '| form)))

(defun org-ml--match-pattern-expand-alternations (pattern)
  "Convert PATTERN with alternations to a list of patterns.
Eg given (a (b | c)), return ((a b) (a c)). This will act
recursively on nested alternations. The returned list will
be deduplicated."
  (cl-flet
      ((add-subpattern
        (acc p)
        (if (org-ml--match-is-alternate-form p)
            (let ((p* (->>
                       (-split-on '| p)
                       (-replace '(nil) nil)
                       (-mapcat #'org-ml--match-pattern-expand-alternations))))
              (-mapcat (lambda (a) (--map (append a it) p*)) acc))
          (--map (append it (list p)) acc))))
    (-uniq (-reduce-from #'add-subpattern '(()) pattern))))

(defun org-ml--match-pattern-process-alternations (end? limit alt-patterns)
  "Convert ALT-PATTERNS to a matching form.
ALT-PATTERNS is a list of patterns created from expanded
alternations in the original pattern.

See `org-ml--match-pattern-make-inner-form' for the meaning of
END? and LIMIT."
  (->> (if end? alt-patterns (reverse alt-patterns))
       (--map (org-ml--match-pattern-make-inner-form end? limit it))
       ;; use nested let statements to keep track of accumulator
       ;; note the comma usage to make this extra confusing :)
       (--reduce `(let ((acc ,it)) ,acc))))

(defun org-ml--match-pattern-simplify-wildcards (pattern)
  "Return PATTERN with wildcards replaced by simpler syntax.
Specifically, this means brackets, `\\?`, `+` wildcards will be put in
terms of explicit conditions, alternative branches, and `*` wildcards."
  (cl-flet*
      ((append-n
        (acc n)
        (append (-repeat (1- n) (car acc)) acc))
       (append-m-n
        (acc m n)
        (--> (-repeat n (car acc))
             (-reductions-from (lambda (a b) (cons b a)) nil it)
             (-drop m it)
             (-interpose '(|) it)
             (if (= m 0) (cons '(nil) it) it)
             (-flatten-n 1 it)
             (cons it (cdr acc))))
       (expand
        (acc sym)
        (pcase sym
          ;; match X at least once
          ;; (X +) -> (X X *)
          ('+
           (append (list '* (car acc)) acc))
          ;; match X 0 or 1 times
          ;; (X \?) -> ((nil | X))
          ('\? (cons (list nil '| (car acc)) (cdr acc)))
          ;; match X N times
          ;; (X [N]) -> (X1 X2 ... XN)
          (`[,(and (pred integerp) n)]
           (if (< 0 n) (append-n acc n)
             (org-ml--arg-error "In [N], N must be > 0: got %s" n)))
          ;; match X at least M times
          ;; (X [M nil]) -> (X1 X2 ... XN X *)
          ;; (X [M !]) -> (X1 X2 ... XN X *!)
          (`[,(and (pred integerp) m)
             ,(and (pred (lambda (x) (or (null x) (eq '! x)))) n)]
           (let ((wc (if (eq n '!) '*! '*)))
             (if (< 0 m) (append (cons wc (-repeat m (car acc))) acc)
               (org-ml--arg-error "In [M nil] M must be positive; got %s" m))))
          ;; match X M to N times (inclusive)
          ;; (X [M N]) -> (XM XM+1 ... XN-1 XN)
          (`[,(and (pred integerp) m) ,(and (pred integerp) n)]
           (cond
            ;; if they are equal and greater than 0, same as [N]
            ((= 0 m n)
             (org-ml--arg-error "Both in [M N] cannot be zero"))
            ((and (< 0 m) (< 0 n) (= m n))
             (append-n acc n))
            ((or (< m 0) (< n 0))
             (org-ml--arg-error "Both in [M N] must be positive: got %s and %s" m n))
            ((< n m)
             (org-ml--arg-error "In [M N], M must be <= N: got %s and %s" m n))
            (t
             (append-m-n acc m n))))
          ;; all else
          (s (cons s acc)))))
    (reverse (-reduce-from #'expand nil pattern))))

(defun org-ml--match-make-pattern-form (end? limit pattern)
  "Return non-slicer matching form for PATTERN.
See `org-ml--match-pattern-make-inner-form' for meaning of END?
and LIMIT which are passed directly through this function. NODE
is the target node to be matched"
  (let ((body (->> (org-ml--match-pattern-simplify-wildcards pattern)
                   (org-ml--match-pattern-expand-alternations)
                   (org-ml--match-pattern-process-alternations end? limit))))
    ;; NOTE: the accumulator is assembled in reverse due to the nature of linked
    ;; lists. Consing to the front is a linear operation, while appending to the
    ;; back is a quadratic operation since the list needs to be fully traversed
    ;; with each append and the list is growing. This means that the list is
    ;; reversed here if `END?' is nil (which means we want the list in
    ;; forward-order) and left in reverse order if `END?' is t (meaning backward
    ;; order)
    (if end? body `(nreverse ,body))))

(defun org-ml--match-make-slicer-form (pattern)
  "Return matching form with slicer operations for PATTERN.
NODE is the node to be matched."
  (pcase pattern
    ;; :first - search until one match found and return that
    (`(:first . ,ps)
     (org-ml--match-make-pattern-form nil 1 ps))
    ;;
    ;; :last - search backwards until one match found and return that
    (`(:last . ,ps)
     (org-ml--match-make-pattern-form t 1 ps))
    ;;
    ;; :nth - search until N matches found and return Nth; note that nil will be
    ;;   returned if N refers to anything outside the results list
    (`(:nth . (,n . ,ps))
     (unless (integerp n)
       (org-ml--arg-error ":nth argument must be an integer"))
     (if (<= 0 n)
         `(-drop ,n ,(org-ml--match-make-pattern-form nil (1+ n) ps))
       `(-drop-last ,(1- (- n)) ,(org-ml--match-make-pattern-form t (- n) ps))))
    ;;
    ;; :sub - search until B matches found, drop A+1, and return; note that if B
    ;;   is longer than the results then all results will be dropped and nil
    ;;   will be ultimately returned
    (`(:sub . (,a . (,b . ,ps)))
     (cond
      ((not (and (integerp a) (integerp b)))
       (org-ml--arg-error ":sub arguments must be an integers"))
      ((> a b)
       (org-ml--arg-error ":sub left index must be less than right index"))
      ((and (<= 0 a) (<= 0 b))
       `(-drop ,a ,(org-ml--match-make-pattern-form nil (1+ b) ps)))
      ((and (< a 0) (< b 0))
       `(-drop-last ,(1- (- b)) ,(org-ml--match-make-pattern-form t (- a) ps)))
      (t
       (org-ml--arg-error "Both indices must be on the same side of zero"))))
    ;;
    ;; no slicer - search without limit and return all
    (ps (org-ml--match-make-pattern-form nil nil ps))))

(defvar org-ml--match-form-cache (make-hash-table :test #'equal)
  "Cache of previously generated lambda forms.")

(defun org-ml-clear-match-cache ()
  "Clear the pattern cache for `org-ml-match' and friends.
See `org-ml-memoize-match-patterns' for details."
  (interactive)
  (clrhash org-ml--match-form-cache))

(defun org-ml--match-make-lambda-form-nocache (pattern)
  "Return callable lambda form for PATTERN.
NODE is the node to be matched."
  (let ((body (org-ml--match-make-slicer-form pattern)))
    `(lambda (it) (let ((it (cons nil it)) (acc)) ,body))))

(defun org-ml--match-make-lambda-form (pattern)
  "Run memoized version of `org-ml--match-make-lambda-form-nocache'.
PATTERN has the same meaning."
  (if org-ml-memoize-match-patterns
      (or (gethash pattern org-ml--match-form-cache)
          (let ((form (--> (org-ml--match-make-lambda-form-nocache pattern)
                           (if (eq 'compiled org-ml-memoize-match-patterns)
                               (byte-compile it)
                             it))))
            (puthash pattern form org-ml--match-form-cache)
            form))
    (org-ml--match-make-lambda-form-nocache pattern)))

;;; match

(defun org-ml-match (pattern node)
  "Return a list of child nodes matching PATTERN in NODE.

PATTERN is a list like ([SLICER [X] [Y]] [SUB1 ...]).

SLICER is an optional prefix to the pattern describing how many
and which matches to return. If not given, all matches are
returned. Possible values are:

- `:first' - return the first match
- `:last' - return the last match
- `:nth' X - return the nth match where X is an integer denoting
  the index to return (starting at 0). X may be a negative number
  to start counting at the end of the match list, in which case
  -1 is the last index. Using 0 and -1 for X is equivalent to
  using `:first' and `:last' respectively
- `:sub' X Y - return a sublist between indices X and Y. X may
  not be greater than Y, and both must either be non-negative
  integers or negative integers. In the case of negative
  integers, the indices refer to the same counterparts as
  described in `:nth'. If X and Y are equal, this slicer has the
  same behavior as `:nth'.

SUBX denotes subpatterns that that match nodes in the parse tree.
Subpatterns may either be wildcards or conditions.

Conditions match exactly one level of the node tree being
searched based on the node's type (the symbol returned by
`org-ml-get-type'), properties (the value returned by
`org-ml-get-property' for a valid property keyword), and
index (the position of the node in the list returned by
`org-ml-get-children'). For index, both left indices (where zero
refers to the left end of the list) and right indices (where -1
refers to the right end of the list) are understood. Conditions
may either be atomic or compound, where compound conditions are
themselves composed of atomic or compound conditions.

The types of atomic conditions are:

- TYPE - match when the node's type is `eq' to TYPE (a symbol)
- INDEX - match when the node's index is `=' to INDEX (an
  integer)
- (OP INDEX) - match when (OP NODE-INDEX INDEX) returns t. OP is
  one of `<', `>', `<=', or `>=' and NODE-INDEX is the index of
  the node being evaluated
- (PROP VAL) - match nodes whose property PROP (a keyword) is
  `equal' to VAL; VAL is obtained by evaluating
  `org-ml-get-property' with PROP and the current node; if PROP
  is invalid, an error will be thrown
- (:pred PRED) - match when PRED evaluates to t; PRED is a symbol
  for a unary function that takes the current node as its
  argument

Compound conditions start with an operator followed by their
component conditions. The types of compound conditions are:

- (:and C1 C2 [C3 ...]) - match when all `C' are true
- (:or C1 C2 [C3 ...]) - match when at least one `C' is true
- (:not C) - match when `C' is not true

In addition, SUBX may be a wildcard keyword or symbol. These are
analogous to the special characters found in POSIX extended
regular expression syntax. Specifically, `[' and `]' correspond
to `{' and `}' respectively and `:any' corresponds to the `.'
operator. All other characters have the same meaning between this
function and POSIX extended regular expressions.:

- `:any' - always match exactly one node
- SUB `?' - match SUB zero or once
- SUB `*' - match SUB zero or more times
- SUB `+' - match SUB one or more times
- SUB [N] - match SUB N times
- SUB [M N] - match SUB M to N times (inclusive); if M or N is
  nil, this will match \"at most N times\" or \"at least M times\"
  respectively
- (ALT-A1 [ALT-A2 ...] | ALT-B1 [ALT-B2 ...] [| ...]) - match
  any of the ALT expressions separated by `|' where ALT is a list
  of subpatterns as described above or nil to match nothing;
  these expressions may be nested

If PATTERN is nil, return NODE. Likewise, if any wildcard
patterns match the nil pattern, also return NODE along with
anything else the wildcard matches. Examples of this would
be (SUB *), (SUB ?), and ((nil | SUB)).

For increased performance, this function (and all others that
consume a PATTERN parameter) can be memoized using
`org-ml-memoize-match-patterns'. If nil, PATTERN is processed
into a lambda form for every function call. If t, the resulting
lambda forms are cached for each unique PATTERN, running
generation step only once if multiple instances of the same
PATTERN are used. Note that `org-ml-memoize-match-patterns' is
shared between all functions that consume a PATTERN parameter."
  (let ((match-fun (org-ml--match-make-lambda-form pattern)))
    (funcall match-fun node)))

;;; generalized tree modification

;; this macro provides the means of using a list of matches returned from
;; `org-ml--match' for other operations that use the match list as targets for
;; modifying the original tree

(eval-when-compile
  (defmacro org-ml--modify-children (node form)
    "Recursively modify the children of NODE using FORM.
FORM returns a list of element or object nodes as the new children,
and the variable `it' is bound to the original children."
    (declare (debug (form def-form)) (indent 1))
    ;; TODO this makes a closure
    `(cl-labels
         ((rec
           (node)
           (if (not (org-ml-is-branch-node node)) node
             (org-ml-map-children*
               (let ((it (--map (rec it) it)))
                 ,form)
               node))))
       (rec ,node))))

;;; delete

(defun org-ml--delete-targets (node targets)
  "Return NODE without children in TARGETS (a list of nodes)."
  (org-ml--modify-children node
    (--remove (member it targets) it)))

(defun org-ml-match-delete (pattern node)
  "Return NODE without children matching PATTERN.

PATTERN follows the same rules as `org-ml-match'."
  (-if-let (targets (org-ml-match pattern node))
      (org-ml--delete-targets node targets)
    node))

;;; extract

(defun org-ml-match-extract (pattern node)
  "Remove nodes matching PATTERN from NODE.
Return cons cell where the car is a list of all removed nodes and
the cdr is the modified NODE.

PATTERN follows the same rules as `org-ml-match'."
  (-if-let (targets (org-ml-match pattern node))
      (cons targets (org-ml--delete-targets node targets))
    node))

;;; map

(org-ml--defun-anaphoric* org-ml-match-map (pattern fun node)
  "Return NODE with FUN applied to children matching PATTERN.
FUN is a unary function that takes a node and returns a new node
which will replace the original.

PATTERN follows the same rules as `org-ml-match'."
  (-if-let (targets (org-ml-match pattern node))
      (org-ml--modify-children node
        (--map-when (member it targets) (funcall fun it) it))
    node))

;;; mapcat

(org-ml--defun-anaphoric* org-ml-match-mapcat (pattern fun node)
  "Return NODE with FUN applied to children matching PATTERN.
FUN is a unary function that takes a node and returns a list of new
nodes which will be spliced in place of the original node.

PATTERN follows the same rules as `org-ml-match'."
  (-if-let (targets (org-ml-match pattern node))
      (org-ml--modify-children node
        (--mapcat (if (member it targets) (funcall fun it) (list it)) it))
    node))

;;; replace

(defun org-ml-match-replace (pattern node* node)
  "Return NODE with NODE* in place of children matching PATTERN.

PATTERN follows the same rules as `org-ml-match'."
  (declare (indent 1))
  (-if-let (targets (org-ml-match pattern node))
      (org-ml--modify-children node
        (--map-when (member it targets) node* it))
    node))

;;; insert-before

(defun org-ml-match-insert-before (pattern node* node)
  "Return NODE with NODE* inserted before children matching PATTERN.

PATTERN follows the same rules as `org-ml-match'."
  (declare (indent 1))
  (-if-let (targets (org-ml-match pattern node))
      (org-ml--modify-children node
        (--mapcat (if (member it targets) (list node* it) (list it)) it))
    node))

;;; insert-after

(defun org-ml-match-insert-after (pattern node* node)
  "Return NODE with NODE* inserted after children matching PATTERN.

PATTERN follows the same rules as `org-ml-match'."
  (declare (indent 1))
  (-if-let (targets (org-ml-match pattern node))
      (org-ml--modify-children node
        (--mapcat (if (member it targets) (list it node*) (list it)) it))
    node))

;;; insert-within

(defun org-ml-match-insert-within (pattern index node* node)
  "Return NODE with NODE* inserted at INDEX in children matching PATTERN.

PATTERN follows the same rules as `org-ml-match' with the exception
that PATTERN may be nil. In this case NODE* will be inserted at INDEX
in the immediate, top level children of NODE."
  (declare (indent 2))
  (if pattern
      (-if-let (targets (org-ml-match pattern node))
          (org-ml--modify-children node
            (--map-when
             (member it targets)
             (org-ml-map-children*
               (org-ml--insert-at index node* it t)
               it)
             it))
        node)
    (org-ml-map-children* (org-ml--insert-at index node* it t) node)))

;;; splice

(defun org-ml-match-splice (pattern nodes* node)
  "Return NODE with NODES* spliced in place of children matching PATTERN.
NODES* is a list of nodes.

PATTERN follows the same rules as `org-ml-match'."
  (declare (indent 1))
  (-if-let (targets (org-ml-match pattern node))
      (org-ml--modify-children node
        (--mapcat (if (member it targets) nodes* (list it)) it))
    node))

;;; splice-before

(defun org-ml-match-splice-before (pattern nodes* node)
  "Return NODE with NODES* spliced before children matching PATTERN.
NODES* is a list of nodes.

PATTERN follows the same rules as `org-ml-match'."
  (declare (indent 1))
  (-if-let (targets (org-ml-match pattern node))
      (org-ml--modify-children node
        (--mapcat (if (member it targets)
                      (append nodes* (list it))
                    (list it))
                  it))
    node))

;;; splice-after

(defun org-ml-match-splice-after (pattern nodes* node)
  "Return NODE with NODES* spliced after children matching PATTERN.
NODES* is a list of nodes.

PATTERN follows the same rules as `org-ml-match'."
  (declare (indent 1))
  (-if-let (targets (org-ml-match pattern node))
      (org-ml--modify-children node
        (--mapcat (if (member it targets) (cons it nodes*) (list it)) it))
    node))

;;; splice-within

(defun org-ml-match-splice-within (pattern index nodes* node)
  "Return NODE with NODES* spliced at INDEX in children matching PATTERN.
NODES* is a list of nodes.

PATTERN follows the same rules as `org-ml-match' with the exception
that PATTERN may be nil. In this case NODES* will be inserted at INDEX
in the immediate, top level children of NODE."
  (declare (indent 2))
  (if pattern
      (-if-let (targets (org-ml-match pattern node))
          (org-ml--modify-children node
            (--map-when
             (member it targets)
             (org-ml-map-children*
               (org-ml--splice-at index nodes* it t)
               it)
             it))
        node)
    (org-ml-map-children* (org-ml--splice-at index nodes* it t) node)))

;;; side-effects

(org-ml--defun-anaphoric* org-ml-match-do (pattern fun node)
  "Like `org-ml-match-map' but for side effects only.
FUN is a unary function that has side effects and is applied to the
matches from NODE using PATTERN. This function itself returns nil.

PATTERN follows the same rules as `org-ml-match'."
  (-when-let (targets (org-ml-match pattern node))
      (--each targets (funcall fun it))))

;;; BUFFER PARSING

;;; org-element--parse-elements wrapper

(defun org-ml--parse-elements (beg end mode)
  "Call `org-element--parse-elements' and unpack the result.
BEG, END and MODE are passed to `org-element--parse-elements'."
  ;; NOTE: A subject to review if something breaks eventually with another Org
  ;; update.
  ;;
  ;; HACK: In future versions of Org as of commit fc80d052d, the last
  ;; argument to `org-element--parse-elemnts' may not be nil.  We create a
  ;; dummy list, pass it to the function and unpack the result.
  (cddr
   (org-element--parse-elements beg end mode nil nil nil (list 'org-data nil))))

;;; parse at specific point

(defun org-ml--parse-objects (type begin end)
  "Return a parsed object defined in the buffer by BEGIN and END.
TYPE is the type of the node to be parsed."
  (if (eq type 'link)
      ;; NOTE these two variables will change the parsed link representation in
      ;; an irreversible and non-obvious way, thus set them to nil (which means
      ;; that parsing and then printing will compose to the identity)
      (let ((org-link-abbrev-alist nil)
            (org-link-translation-function nil))
        (org-ml--parse-elements begin end 'first-section))
    (org-ml--parse-elements begin end 'first-section)))

;; TODO add test for plain-text parsing
(defun org-ml-parse-object-at (point)
  "Return object node under POINT or nil if not on an object."
  (save-excursion
    (goto-char point)
    (-let* ((context (org-element-context))
            (type (org-ml-get-type context))
            ((offset nesting) (pcase type
                                ((or `superscript `subscript) '(-1 (0 1)))
                                (`table-cell '(-1 (0 0 0)))
                                (_ '(0 (0 0)))))
            (begin (org-element-begin context))
            (end (org-element-end context))
            (tree (org-ml--parse-objects type (+ begin offset) end)))
      (->> (car tree)
           (org-ml--get-descendent nesting)
           (org-ml--filter-types org-ml-objects)))))

;; TODO this seems really inefficient; essentially we are parsing twice and
;; there is probably a better way to do this with the new API
(defun org-ml--parse-element-at (point type)
  "Return element node immediately under POINT.
For a list of all possible return types refer to `org-ml-elements'; this
will return everything in this list except `section' which is
ambiguous when referring to a single point.
\(see `org-ml-parse-section-at').

If TYPE is non-nil, only return nil if the object under point is not
of that type. TYPE is a symbol from `org-ml-elements'. Furthermore,
setting TYPE to `table-row' will prefer table-row elements over table
elements and likewise when setting TYPE to `item' for plain-list
elements vs item elements."
  (save-excursion
    (goto-char point)
    (let* ((node (org-element-at-point))
           (node-type (org-ml-get-type node)))
      ;; NOTE this will not filter by type if it is a leaf node
      (if (not (memq node-type org-ml-branch-nodes)) node
        ;; need to parse again if branch-node since
        ;; `org-element-at-point' does not parse children
        (-let* ((begin (org-element-begin node))
                (end (org-element-end node))
                (contents-end (org-element-contents-end node))
                (tree (car (org-ml--parse-elements begin end 'first-section)))
                (nesting (pcase node-type
                           (`headline nil)
                           ;; `org-element-at-point' will return a table if on
                           ;; the first row of a table, and a table-row
                           ;; otherwise
                           (`table-row '(0 0))
                           (`table (if (eq type 'table-row) '(0 0) '(0)))
                           (`plain-list (if (eq type 'item) '(0 0) '(0)))
                           (`item '(0 0))
                           (_ '(0))))
                (node* (->> (org-ml--get-descendent nesting tree)
                            ;; set ending boundaries according to what we get
                            ;; from `org-element-at-point'
                            (org-element-put-property-2 :end end)
                            (org-element-put-property-2 :contents-end contents-end))))
          ;; some elements will always have post-blank set to 0, so no need to
          ;; update it
          (--> (if (memq node-type '(section table-row)) node*
                 (let ((pb (org-element-post-blank node)))
                   (org-ml--set-post-blank pb node*)))
               (if type (org-ml--filter-type type it) it)))))))

(defun org-ml-parse-element-at (point)
  "Return element node under POINT or nil if not on an element.

This function will return every element available in `org-ml-elements'
with the exception of `section', `item', and `table-row'. To
specifically parse these, use the functions `org-ml-parse-section-at',
`org-ml-parse-item-at', and `org-ml-parse-table-row-at'."
  (org-ml--parse-element-at point nil))

(defun org-ml-parse-table-row-at (point)
  "Return table-row node under POINT or nil if not on a table-row."
  (save-excursion
    (goto-char point)
    (beginning-of-line)
    (org-ml--parse-element-at (point) 'table-row)))

(defun org-ml-parse-item-at (point)
  "Return item node under POINT or nil if not on an item.
This will return the item node even if POINT is not at the beginning
of the line."
  ;; TODO this doesn't work if not on the first item
  (save-excursion
    (goto-char point)
    (beginning-of-line)
    (org-ml--parse-element-at (point) 'item)))

(defun org-ml--parse-headline-subtree-at (point subtree)
  "Return headline node under POINT in the current buffer.
POINT may be anywhere between the points given by
`org-back-to-heading' and `org-end-of-subtree'; it does not matter
if the node immediately under POINT is not a headline. If SUBTREE is
t, parse the entire subtree, else just parse the top headline."
  (save-excursion
    (goto-char point)
    (when (ignore-errors (org-back-to-heading t))
      (let* ((b (point))
             (e (if subtree
                   (progn
                     (org-end-of-subtree)
                     ;; skip ahead to the next headline because
                     ;; `org-end-of-subtree' does not by default, which misses
                     ;; any spacing after headlines
                     (or (outline-next-heading)
                         (point-max)))
                  (or (outline-next-heading) (point-max))))
             (tree (car (org-ml--parse-elements b e 'first-section))))
        ;; TODO this is a hack; since org 9.6 setting the boundaries at the next
        ;; headline will not stop the parser from parsing the entire subtree,
        ;; even if we don't want it. Workaround is to parse the entire subtree
        ;; and possibly throw away most of it
        (if subtree tree
          (let ((cs (org-element-contents tree)))
            (if (< 1 (length cs))
                (org-ml-set-children (list (car cs)) tree)
              tree)))))))

(defun org-ml-parse-headline-at (point)
  "Return headline node under POINT or nil if not on a headline.
POINT does not need to be on the headline itself. Only the headline
and its section will be returned. To include subheadlines, use
`org-ml-parse-subtree-at'."
  (org-ml--parse-headline-subtree-at point nil))

(defun org-ml-parse-subtree-at (point)
  "Return headline node under POINT or nil if not on a headline.
POINT does not need to be on the headline itself. Unlike
`org-ml-parse-headline-at', the returned node will include
child headlines."
  (org-ml--parse-headline-subtree-at point t))

(defun org-ml-parse-section-at (point)
  "Return section node under POINT or nil if not on a section.
If POINT is on or within a headline, return the section under that
headline. If POINT is before the first headline (if any), return
the section at the top of the org buffer."
  (save-excursion
    (goto-char point)
    (->> (condition-case nil
             (progn
               (org-back-to-heading)
               ;; TODO this suffers from the same problem as the headline parser
               ;; (parses entire subtree and probably wastes most of it)
               (org-ml--parse-headline-subtree-at point nil))
           (error
            (org-ml--parse-elements
             (point-min) (or (outline-next-heading) (point-max)) 'first-section)))
         (org-ml--get-descendent '(0))
         (org-ml--filter-type 'section))))

;;; parse at current point

(eval-when-compile
  (defun org-ml--autodef-parse-node-form (name)
    "Return defun form for NAME."
    (let* ((fun-name (intern (format "org-ml-parse-this-%s" name)))
           (call (intern (format "org-ml-parse-%s-at" name)))
           (doc (format "Call `%s' with the current point." call))
           (body `(,call (point))))
      `(defun ,fun-name () ,doc ,body)))

  (defmacro org-ml--autodef-parse-node-functions ()
    "Define all parse functions."
    (let ((forms (->> '(object element table-row item headline subtree section)
                      (-map #'org-ml--autodef-parse-node-form))))
      `(progn ,@forms))))

(org-ml--autodef-parse-node-functions)

(defun org-ml-parse-this-toplevel-section ()
  "Return section node corresponding to the top of the current buffer.
If there is no such section, return nil."
  (save-excursion
    (goto-char (point-min))
    (unless (= ?* (char-after))
      (org-ml-parse-this-section))))

(defun org-ml-parse-this-buffer ()
  "Return org-data document tree for the current buffer.
Contrary to the org-element specification, the org-data element
returned from this function will have :begin and :end properties."
  (org-element-parse-buffer))

(defun org-ml-this-buffer-has-headlines ()
  "Return t if the current buffer has headlines, else return nil."
  (save-excursion
    (goto-char (point-min))
    (and (re-search-forward "^\\*" nil t) t)))

;;; BUFFER SIDE EFFECTS

;;; insert

(defun org-ml--nodes-to-string-maybe (nodes)
  "Return NODES as a string.
NODES may either be a single node or a list of nodes."
  (cond
   ((org-ml--is-node nodes) (org-ml-to-string nodes))
   ((listp nodes) (mapconcat #'org-ml-to-string nodes ""))
   (t (error "Must a node or a list of nodes"))))

(defun org-ml--insert (point node)
  "Convert NODE to a string and insert at POINT in the current buffer.
NODE may be a node or a list of nodes. Return NODE.
Does not save point."
  (goto-char point)
  (insert (org-ml--nodes-to-string-maybe node)))

(defun org-ml-insert (point node)
  "Convert NODE to a string and insert at POINT in the current buffer.
NODE may be a node or a list of nodes. Return NODE."
  (save-excursion (org-ml--insert point node))
  node)

(defun org-ml-insert-tail (point node)
  "Like `org-ml-insert' but insert NODE at POINT and move to end of insertion."
  (let ((s (org-ml--nodes-to-string-maybe node)))
    (save-excursion
      (goto-char point)
      (insert s))
    (goto-char (+ point (length s))))
  node)

;;; update

(defun org-ml--apply-overlays (os)
  "Apply overlays OS to the current buffer."
  (cl-flet
      ((apply-overlays
        (o)
        (let* ((beg (plist-get o :start))
               (end (plist-get o :end))
               (props (plist-get o :props))
               (o* (make-overlay beg end)))
          (--each (-partition 2 props) (apply #'overlay-put o* it)))))
    (-each os #'apply-overlays)))

;; Myers diff algorithm
;; Myers, E.W. AnO(ND) difference algorithm and its variations. Algorithmica 1,
;; 251–266 (1986). https://doi.org/10.1007/BF01840446

;; TODO There is a 99.99999% chance I can do way better than this. If the goal
;; is to figure out what stringy bits to put into the buffer based on what has
;; been modified, it makes more sense to make a crazy tree-based diff algorithm
;; that is specialized for org-element nodes (which almost assuredly does not
;; exist so I can't just steal a paper like I did with Myers) and only convert
;; the things that have changes to strings and put those in the buffer. At least
;; that seems to make sense, I haven't done complexity analysis yet.

;; this is a souped-up version of the linear-space diff algorithm as presented
;; from Myers; adapted from the python implementation listed here:
;; https://blog.robertelder.org/diff-algorithm/

(defun org-ml--diff-find-middle (str-eq M N)
  "Return the coordinates for the middle snake.

STR-EQ is a ternary function that takes a direction (0 = forward, 1
= backward) and X/Y coordinates; it will return t if the two
strings at the given coordinates in the indicated direction
match.

M and N are the length of the current substrings."
  (cl-flet*
      ((init-V
        (len)
        (make-vector len 0))
       (init-k
        (D len)
        (- D (* 2 (max 0 (- D len)))))
       (get-x
        (len V k)
        (elt V (mod k len))))
    (let* ((D-max (+ M N))
           (D-mid (ceiling D-max 2))
           (delta (- M N))
           (V-len (+ 2 (* 2 (min M N))))
           (V+ (init-V V-len))
           (V- (init-V V-len))
           ;; if D-max is odd, only check for overlaps in the forward direction
           ;; (and vice versa); note that the directions are coded 0 for forward
           ;; and 1 for backward (see below)
           (check-dir-p (mod (1+ D-max) 2))
           (D 0)
           ret
           dir fwd-p kstart kend z-lim x-vert x-horz x0 y0 x y z Va Vb offset k)
      ;; iterate through D-paths for all D
      (while (and (not ret) (<= D D-mid))
        (setq kstart (- (init-k D N))
              kend (init-k D M)
              dir 0)
        ;; this loop runs 2x for each direction (0 = forward and 1 = backward)
        (while (and (not ret) (<= dir 1))
          (setq fwd-p (= dir 0)
                Va (if fwd-p V+ V-)
                Vb (if fwd-p V- V+)
                offset (if fwd-p 1 0)
                z-lim (- D offset)
                k kstart)
          ;; iterate across all diagonals (k) to find furthest reaching paths
          (while (and (not ret) (<= k kend))
            (setq x-vert (get-x V-len Va (1+ k))
                  x-horz (get-x V-len Va (1- k))
                  x0 (if (or (= k (- D)) (and (/= k D) (< x-horz x-vert)))
                         x-vert
                       (1+ x-horz))
                  y0 (- x0 k)
                  x x0
                  y y0
                  z (- delta k))
            (while (and (< x M) (< y N) (funcall str-eq fwd-p x y))
              (setq x (1+ x)
                    y (1+ y)))
            (aset Va (mod k V-len) x)
            (when (and (= dir check-dir-p)
                       (<= (- z-lim) z z-lim)
                       (<= M (+ (get-x V-len Va k) (get-x V-len Vb z))))
              (setq ret (->> (if fwd-p `(,x0 ,y0 ,x ,y)
                               `(,(- M x) ,(- N y) ,(- M x0) ,(- N y0)))
                             (cons (- (* 2 D) offset)))))
            (setq k (+ 2 k)))
          (setq dir (1+ dir)))
        (setq D (1+ D)))
      ret)))

(defun org-ml--diff (str-a str-b)
  "Return the edit commands to make STR-A `equal' STR-B.

This is the linear-space version of the Myers diff algorithm with
several other enhancements, including tighter diagonal bounds to
prevent running off the edit grid (useless CPU cycles) and only
allocating memory for each V-array according to the minimum
string length and using them as circular buffers. After these
edits, the time complexity should be O(min(A, B)*D) and the space
complexity should be O(min(A, B).

Return value will be a list of either `(ins I M N)' or `(del I
J)'. For `ins' commands M and N are the indices from STR-B to
insert at I in STR-A, and for `del' commands I and J are the
indices between which will be deleted in STR-A. Note that
consecutive edits will be consolidated so the length of the
return list will not necessarily be the length of the LCS
computed by the Myers diff algorithm."
  (cl-labels
      ((diff
        (a0 a1 b0 b1 i j)
        (let* ((M (- a1 a0))
               (N (- b1 b0))
               (str=
                (lambda (fwd-p x y)
                  (if fwd-p
                      (= (elt str-a (+ a0 x)) (elt str-b (+ b0 y)))
                    (= (elt str-a (+ a0 (- M 1 x)))
                       (elt str-b (+ b0 (- N 1 y))))))))
          (cond
           ((and (< 0 M) (< 0 N))
            (-let (((D-tot x y u v) (org-ml--diff-find-middle str= M N)))
              (cond
               ((and (or (< 1 D-tot) (and (/= x u) (/= y v))))
                (append
                 (diff a0 (+ a0 x) b0 (+ b0 y) i j)
                 (diff (+ a0 u) (+ a0 M) (+ b0 v) (+ b0 N) (+ i u) (+ j v))))
               ((< M N)
                (diff 0 0 (+ b0 M) (+ b0 N) (+ i M) (+ j M)))
               ((< N M)
                (diff (+ a0 N) (+ a0 M) 0 0 (+ i N) (+ j N)))
               (t
                nil))))
           ((< 0 M)
            `((del ,i ,(+ i M))))
           ((< 0 N)
            `((ins ,i ,j ,(+ j N)))))))
       (consolidate
        (acc next)
        (-let (((last . rest) acc))
          (pcase `(,last ,next)
            (`((ins ,i0 ,m0 ,n0) (ins ,i1 ,m1 ,n1))
             (if (and (= i0 i1) (= n0 m1))
                 (cons `(ins ,i0 ,m0 ,n1) rest)
               (cons next acc)))
            (`((del ,i0 ,j0) (del ,i1 ,j1))
             (if (= j0 i1)
                 (cons `(del ,i0 ,j1) rest)
               (cons next acc)))
            (_
             (cons next acc))))))
    (let ((a1 (length str-a))
          (b1 (length str-b)))
      (->> (diff 0 a1 0 b1 0 0)
           (-reduce-from #'consolidate nil)))))

(defun org-ml--diff-region (start end new-str)
  "Use Myers Diff algorithm to update the current buffer.
The region to be updated will be between START and END and will
be made to look like NEW-STR. Only differences as given by the Myers
diff algorithm (eg insertions and deletions) will actually be
applied to the buffer."
  (-let* ((old-str (buffer-substring-no-properties start end))
          (edits (org-ml--diff old-str new-str)))
    (save-excursion
      (while edits
        (pcase (car edits)
          (`(ins ,i ,m ,n)
           (goto-char (+ start i))
           (insert (substring new-str m n)))
          (`(del ,i ,j)
           (delete-region (+ start i) (+ start j))))
        (!cdr edits)))))

;; (defun org-ml--properties-equal (type prop value1 value2)
;;   "Return t if VALUE1 and VALUE2 are 'the same'.
;; If TYPE and PROP are 'headline/:title or 'item/:tag respectively,
;; compare using `org-ml--equal' on all their members (as these are
;; secondary strings). Otherwise use `equal'. This function is meant
;; to avoid infinite loops which may be caused by comparing the
;; parent nodes in secondary strings."
;;   (if (or (and (eq type 'headline) (eq prop :title))
;;           (and (eq type 'item) (eq prop :tag)))
;;       (let ((matches t))
;;         (while (and value1 matches)
;;           (setq matches (org-ml--equal (car value1) (car value2))
;;                 value1 (cdr value1)
;;                 value2 (cdr value2)))
;;         (and (not value1) matches))
;;     (equal value1 value2)))

;; (defun org-ml--equal (node1 node2)
;;   "Test of NODE1 is 'the same' as NODE2.
;; 'The same' means that both nodes have the same type, children,
;; and properties, where children are assessed recursively.

;; For properties, order and presence matters, and all properties
;; except for parent will be tested for equality using `equal' when
;; comparing their values (if :parent is present in one, it will
;; still be expected in the other but their values are ignored).
;; This may be contradictory to the more general definition of 'the
;; same' because a plist is unordered, but this function is only
;; intended to test for equality in cases where NODE2 is a modified
;; version of NODE1 and thus their plists should have the same
;; order."
;;   (let* ((is-str-1 (stringp node1))
;;          (is-str-2 (stringp node2)))
;;     (cond
;;      ((and is-str-1 is-str-2)
;;       (equal node1 node2))
;;      ((not (and is-str-1 is-str-2))
;;       (-let (((t1 . (p1 . c1)) node1)
;;              ((t2 . (p2 . c2)) node2))
;;         ;; first test if types match
;;         (and (eq t1 t2)
;;              ;; then test children (which will test their types first)
;;              (let ((children-match t))
;;                (while (and c1 children-match)
;;                  (setq children-match (org-ml--equal (car c1) (car c2))
;;                        c1 (cdr c1)
;;                        c2 (cdr c2)))
;;                (and (not c2) children-match))
;;              ;; then test the plist, which will likely be slower than testing
;;              ;; types so do it last so the average run time is shorter
;;              (let ((plist-matches t))
;;                (while (and p1 plist-matches)
;;                  ;; skip over parents since these could make circular lists
;;                  (setq plist-matches (and (eq (car p1) (car p2))
;;                                           (or (eq p1 :parent)
;;                                               (org-ml--properties-equal
;;                                                t1 (car p1) (cadr p1) (cadr p2))))
;;                        p1 (cdr (cdr p1))
;;                        p2 (cdr (cdr p2))))
;;                (and (not p2) plist-matches))))))))

(defun org-ml--replace-region (begin end text)
  "Replace text between BEGIN and END with TEXT."
  (delete-region begin end)
  (goto-char begin)
  (insert text))

(defun org-ml--replace-bounds (diff-mode begin end node)
  "Replace text between BEGIN and END with NODE1 in current buffer.
See `org-ml~update' for meaning of DIFF-MODE."
  (let ((ov-cmd (-if-let (x (->> (overlays-in begin end)
                                 (--filter (eq 'outline (overlay-get it 'invisible)))
                                 (--map (list :start (overlay-start it)
                                              :end (overlay-end it)
                                              :props (overlay-properties it)))))
                    (list 'apply 'org-ml--apply-overlays x))))
    ;; hacky way to add overlays to undo tree
    (when ov-cmd
      (setq-local buffer-undo-list (cons ov-cmd buffer-undo-list)))
    (if diff-mode
        (org-ml--diff-region begin end (org-ml-to-string node))
      ;; convert node to string before deleting so deferred properties can get
      ;; what they need from the buffer
      (org-ml--replace-region begin end (org-ml-to-string node)))
    nil))

(org-ml--defun-anaphoric* org-ml--update (diff-mode fun node)
  "Internal version of `org-ml~update'.
DIFF-MODE, FUN, and NODE have the same meaning. The only
difference is this function does not save the point's position"
  ;; do all computation before modifying buffer
  ;;
  ;; NOTE force resolution so that we can convert back to string after
  ;; deleting the node from the buffer
  (let* ((begin (org-element-begin node))
         (end (org-element-end node)))
    (->> (funcall fun node)
         (org-ml--replace-bounds diff-mode begin end))))

(org-ml--defun* org-ml~update (diff-mode fun node)
  "Replace NODE in the current buffer with a new one.
FUN is a unary function that takes NODE and returns a modified node
or list of nodes.

DIFF-MODE describes how the buffer will be updated and can be one of
the following:
- t: use the Myers diff algorithm to compare the old buffer
  string with the new string from the modified NODE, and only
  edit the the regions that are different
- nil: use no diff algorithm; just replace the old buffer string
  entirely with the new one."
  (save-excursion
    (org-ml--update diff-mode fun node)))

(org-ml--defun* org-ml-update (fun node)
  "Replace NODE in the current buffer with a new one.
FUN is a unary function that takes NODE and returns a modified node
or list of nodes.

The modified NODE will be converted to a string and then compared
to the old buffer string using the Myers diff algorithm. This has
an average time complexity of O(M+N+D^2) where M and N are the
lengths of the old and new strings respectively and D is the
number of inserts or deletes required to change one into the
other. At the cost of performance, only the parts of the buffer
that need to be modified will actually be changed, which is less
likely to disturb overlays and move the cursor (and is also more
like how org-mode's build-in imperative functions behave).

If one does not need this level of precision, use the function
`org-ml~update' and supply nil for the DIFF-MODE argument. This
will simply replace the old node's string representation with the
modified node's string in its entirety. This will likely be
faster but could destroy overlays (eg folding) and will
reposition the cursor to the beginning of NODE if it is in the
middle of NODE."
  (org-ml~update t fun node))

;; generate all update functions for corresponding parse functions
;; since all take function args, also generate anaphoric forms
(eval-when-compile
  (defun org-ml--autodef-update-node-forms (name)
    "Return defun and defmacro forms for NAME."
    (cl-flet
        ((format-doc
          (name doclist)
          (--> (s-join "\n" doclist)
               (format it name))))
      (let* ((update-at (intern (format "org-ml-update-%s-at" name)))
             (update-this (intern (format "org-ml-update-this-%s" name)))
             (update-at~ (intern (format "org-ml~update-%s-at" name)))
             (update-this~ (intern (format "org-ml~update-this-%s" name)))
             (myers-doc (list "This function uses the Myers diff algorithm."
                              "See `org-ml-update' for what this means."))
             (diff-doc (list "See `org-ml~update' for the meaning of DIFF-MODE"))
             (update-at-doc-header
              (list "Update %1$s under POINT using FUN."
                    "FUN takes an %1$s and returns a modified %1$s"))
             (update-this-doc-header
              (list "Update %1$s under current point using FUN."
                    "FUN takes an %1$s and returns a modified %1$s"))
             (update-at-doc~
              (->> (append update-at-doc-header '("") diff-doc)
                   (format-doc name)))
             (update-this-doc~
              (->> (append update-this-doc-header '("") diff-doc)
                   (format-doc name)))
             (update-at-doc
              (->> (append update-at-doc-header '("") myers-doc)
                   (format-doc name)))
             (update-this-doc
              (->> (append update-this-doc-header '("") myers-doc)
                   (format-doc name)))
             (call (intern (format "org-ml-parse-%s-at" name)))
             (update-at-body~ `(org-ml~update diff-mode fun (,call point)))
             (update-this-body~ `(,update-at~ diff-mode (point) fun))
             (update-at-body `(,update-at~ t point fun))
             (update-this-body `(,update-this~ t fun)))
        (list `(org-ml--defun* ,update-at~ (diff-mode point fun)
                 ,update-at-doc~
                 ,update-at-body~)
              `(org-ml--defun* ,update-this~ (diff-mode fun)
                 ,update-this-doc~
                 ,update-this-body~)
              `(org-ml--defun* ,update-at (point fun)
                 ,update-at-doc
                 ,update-at-body)
              `(org-ml--defun* ,update-this (fun)
                 ,update-this-doc
                 ,update-this-body)))))

  (defmacro org-ml--autodef-update-node-functions ()
    "Define all update-node functions and macros."
    (let ((forms (->> '(object element table-row item headline subtree section)
                      (-mapcat #'org-ml--autodef-update-node-forms))))
      `(progn ,@forms))))

(org-ml--autodef-update-node-functions)

(org-ml--defun* org-ml-update-this-buffer (fun)
  "Apply FUN to the contents of the current buffer.
FUN is a unary function that takes a node of type `org-data' and
returns a modified node."
  (org-ml-update fun (org-ml-parse-this-buffer)))

;;; fold

(defun org-ml--fold-get-contents-begin-maybe (node)
  "Return :contents-begin minus one or nil if not found for NODE."
  (-some-> (org-element-contents-begin node) (1-)))

(eval-when-compile
  (defmacro org-ml--fold-get-contents-begin-offset (node offset)
    "Return the fold beginning boundary of NODE.
Try `org-ml--fold-get-contents-begin-maybe' first, and if this returns nil,
use OFFSET to calculated the beginning fold boundary beginning.
OFFSET can either be an integer or a form that evaluates to an
integer."
    (declare (indent 1) (debug (form form)))
    (let ((n (make-symbol "--node")))
      `(let ((,n ,node))
         (or (org-ml--fold-get-contents-begin-maybe ,n)
             (+ ,offset (org-element-begin ,n)))))))

(defun org-ml--fold-get-begin-boundary (node)
  "Return integer for point at the beginning of fold region for NODE."
  (cl-case (org-ml-get-type node)
    ;; Blocks must be folded regardless of if they have children
    (center-block
     (org-ml--fold-get-contents-begin-offset node 14))
    (dynamic-block
     (org-ml--fold-get-contents-begin-offset node
       (+ 9 (length (org-element-property-raw :block-name node)))))
    (drawer
     (org-ml--fold-get-contents-begin-offset node
       (+ 2 (length (org-element-property-raw :drawer-name node)))))
    (property-drawer
     (org-ml--fold-get-contents-begin-offset node 12))
    ((quote-block verse-block)
     (org-ml--fold-get-contents-begin-offset node 13))
    (special-block
     (org-ml--fold-get-contents-begin-offset node
       (+ 9 (length (org-element-property-raw :type node)))))
    ;; Headlines should only be folded if they have children
    (headline
     (org-ml--fold-get-contents-begin-maybe node))
    ;; Items are tricky since everything after the "first line" is folded. If
    ;; the first child is a paragraph, need to figure out how long its first
    ;; line is and add that to :contents-begin. Do nothing if there are no
    ;; children
    (item
     (-when-let (first (-first-item (org-element-contents node)))
       (let ((offset (if (not (org-ml--is-type 'paragraph first)) -1
                       (->> (org-ml-to-string first)
                            (s-split "\n")
                            (-first-item)
                            (length)))))
         (+ offset (org-element-contents-begin node)))))
    ;; These elements are not branch types and thus don't have child boundaries,
    ;; so will need to manually calculated where the boundaries should be
    ((comment-block example-block)
     (+ 15 (org-element-begin node)))
    (export-block
     (+ (org-element-begin node)
        (-if-let (type (org-element-property-raw :type node))
            (1+ (length type)) 0)
        14))
    (src-block
     (+ (org-element-begin node)
        (-if-let (meta (->
                        (list
                         (org-element-property-raw :language node)
                         (org-element-property-raw :switches node)
                         (org-element-property-raw :parameters node))
                        (-non-nil)))
            (1+ (length (s-join " " meta))) 0)
        11))))

(defun org-ml--fold-flag-region-block (begin end flag)
  "Set invisibility for region denoted by BEGIN and END.
FLAG is a boolean (t for invisible). The overlays applied should
only be used for block elements."
  ;; Code ripped off from `org-flag-region' with overlay properties set to
  ;; match those created in `org-hide-block-toggle'
  (remove-overlays begin end 'invisible)
  (when flag
    (let ((o (make-overlay begin end nil 'front-advance)))
      (overlay-put o 'evaporate t)
      (overlay-put o 'invisible 'org-hide-block)
      (overlay-put o 'isearch-open-invisible #'delete-overlay))))

(defun org-ml--fold-flag-node (flag node)
  "Set folding of buffer contents in NODE to FLAG."
  (-when-let (begin (org-ml--fold-get-begin-boundary node))
    (let ((end (- (org-element-end node) (org-element-post-blank node) 1)))
      (cl-case (org-ml-get-type node)
        ((drawer headline item property-drawer)
         (outline-flag-region begin end flag))
        ((center-block comment-block dynamic-block example-block
                       export-block quote-block special-block
                       src-block verse-block)
         (org-ml--fold-flag-region-block begin end flag))))))

(defun org-ml-fold (node)
  "Fold the children of NODE if they exist."
  (org-ml--fold-flag-node t node))

(defun org-ml-unfold (node)
  "Unfold the children of NODE if they exist."
  (org-ml--fold-flag-node nil node))

(defun org-ml-subtree-set-fold (fold-state headline)
  "Set the fold state of HEADLINE node.
This function will do nothing unless HEADLINE has children.
FOLD-STATE may be one of:
- `none`: hide everything
- `children`: show section and 1st-level subheadlines only
- `subtree`: show section and subheadline contents (but not drawers)
- `all`: show everything"
  (cl-flet
      ((fold-drawers
        (headline)
        (-when-let (section (org-ml-headline-get-section headline))
          (-some->> (--first (org-ml--is-type 'property-drawer it) section)
            (org-ml-fold))
          (let ((drawers (->> (org-element-contents section)
                              (--filter (org-ml--is-type 'drawer it)))))
            (--each drawers (org-ml-fold it))))))
    (cl-case fold-state
      (none
       (org-ml-fold headline))
      (children
       (org-ml-unfold headline)
       (fold-drawers headline)
       (--each (org-ml-headline-get-subheadlines headline) (org-ml-fold it)))
      (subtree
       (org-ml-unfold headline)
       (fold-drawers headline)
       (--each (org-ml-headline-get-subheadlines headline)
         (org-ml-subtree-set-fold fold-state it)))
      (all
       (org-ml-unfold headline)))))

;;; headline batch processing

(defun org-ml--get-forward-bounds (m n re)
  "Return the boundaries of headlines to parse.
M is the minimum number of headlines and N is the maximum number
of headlines. RE is a regular expression that will be used to
search for a headline. The return value will be a list
like (BEGIN END) where BEGIN is the start of the Mth headline and
END is the end of the Nth headline."
  (save-excursion
    (save-match-data
      (goto-char (point-min))
      (let (begin end)
        (when (re-search-forward re nil t)
          (let ((i 0)
                (next t))
            (while (and next (<= i n))
              (when (= m i)
                (setq begin (match-beginning 0)))
              (setq i (1+ i)
                    next (re-search-forward re nil t)))
            (setq end (if next (match-beginning 0) (point-max)))))
        (list begin end)))))

(defun org-ml--get-backward-bounds (m n re)
  "Return the boundaries of headlines to parse.
This is like `org-ml--get-forward-bounds' except it searches
backwards. M is the minimum number of headlines and N is the
maximum number of headlines. RE is a regular expression that will
be used to search for a headline. The return value will be a list
like (BEGIN END) where BEGIN is the start of the Nth headline and
END is the end of the Mth headline."
  (save-excursion
    (save-match-data
      (goto-char (point-max))
      (let ((i 0)
            (prev-point (point-max))
            begin end)
        (while (and (<= i n) (re-search-backward re nil t))
          (when (= m i)
            (setq end prev-point))
          (setq i (1+ i)
                prev-point (point)))
        (when end
          (setq begin (point)))
        (list begin end)))))

(defun org-ml--get-region-bounds (begin end re)
  "Return the boundaries of headlines to parse.
RE is a regular expression that will be used to search for a
headline. The return value will be a list like (PBEGIN PEND)
where PBEGIN is the start of the headline immediately after BEGIN
and PEND is the end of the headline immediately before END."
  (save-match-data
    (save-excursion
      (let ((b (progn
                 (goto-char begin)
                 (if (looking-at re) begin
                   (when (re-search-forward re nil t)
                     (match-beginning 0)))))
            (e (or (progn
                     (goto-char end)
                     (if (looking-at re) end
                       (when (re-search-forward re nil t)
                         (match-beginning 0))))
                   (point-max))))
        (list b e)))))

(defun org-ml--parse-patterns-where (which re)
  "Return the parse boundaries of a headline based on WHICH.
See `org-ml-get-some-headlines' for the meaning of WHICH. RE is a
regular expression used to search for the next headline."
  (declare (indent 1))
  (cl-flet
      ((int-or-nil-p
        (x)
        (or (null x) (integerp x))))
    (-let (((b e)
            (pcase which
              ;; parse N
              (`all
               (org-ml--get-region-bounds (point-min) (point-max) re))
              ((and (pred integerp) n)
               (if (<= 0 n) (org-ml--get-forward-bounds 0 n re)
                 (org-ml--get-backward-bounds 0 n re)))
              ;; parse M-N
              (`(,(and (pred integerp) m) ,(and (pred integerp) n))
               (cond
                ((<= 0 m n) (org-ml--get-forward-bounds m n re))
                ((<= m n -1) (org-ml--get-backward-bounds (1- (- n)) (1- (- m)) re))
                ((< n m) (org-ml--arg-error "M must be less than or equal to N"))
                (t (org-ml--arg-error "M and N must be the same sign"))))
              ;; parse region between A and B
              (`[,(and (pred int-or-nil-p) a) ,(and (pred int-or-nil-p) b)]
               (let ((a (or a (point-min)))
                     (b (or b (point-max))))
                 (org-ml--get-region-bounds a b re)))
              (e (org-ml--arg-error "Invalid 'which' specification: Got %S" e)))))
      (when (and b e)
        (org-ml--parse-elements b e 'first-section)))))

(defun org-ml-parse-headlines (which)
  "Return list of headline nodes from current buffer.

WHICH describes the location of headlines to be parsed and is one
of the following:
- N: parse up to index N headlines (which 0 is the first); if negative
  start counting from the last headline (which -1 refers to the last)
- (M N): like N but parse after index M headlines; M and N may both
  be similarly negative
- [A B]: parse all headlines whose first point falls between points
  A and B in the buffer; if A and B are nil, use `point-min' and
  `point-max' respectively.
- `all': parse all headlines (equivalent to [nil nil])

Each headline is obtained with `org-ml-parse-headline-at'."
  (cl-labels
      ((get-subheadlines
        (headline)
        (->> (org-ml-headline-get-subheadlines headline)
             (-mapcat #'get-subheadlines)
             (cons headline))))
    (->> (org-ml--parse-patterns-where which "^\\*+ ")
         (-mapcat #'get-subheadlines))))

(defun org-ml-parse-subtrees (which)
  "Return list of subtree nodes from current buffer.

WHICH has analogous meaning to that in `org-ml-parse-headlines'
except applied to subtrees not individual headlines."
  (org-ml--parse-patterns-where which "^\\* "))

(org-ml--defun* org-ml-update-headlines (which fun)
  "Update some headlines in the current using FUN.

See `org-ml-parse-headlines' for the meaning of WHICH.

Headlines are updated using `org-ml~update' with DIFF-ARG set to
nil (see this for use and meaning of FUN)."
  ;; don't use the myers diff algorithm here, since these functions are meant
  ;; for batch processing.
  (save-excursion
    (cl-labels
        ((map-to-subheadlines
          (headline)
          (org-ml-headline-map-subheadlines*
            (-map #'map-to-subheadlines it)
            (funcall fun headline))))
      ;; NOTE there two main ways to do this. We can either flatten the output
      ;; of `org-ml--parse-patterns-where' into individual headlines (eg no
      ;; headline would have a subheadline and all subheadlines would be in the
      ;; top level of the list) and update each of them in the buffer
      ;; individually using `FUN'. Or we can do we we do here, which is to apply
      ;; `FUN' recursively to each subtree and then update the entire subtree in
      ;; place in the buffer. This has the advantage of not requiring the
      ;; subtrees to be broken apart which could introduce whitespace errors
      ;; between headlines, section, etc. It has the disadvantage of requiring
      ;; more text to be modified in the buffer at once, which could be
      ;; disruptive.
      (--> (org-ml--parse-patterns-where which "^\\*+ ")
           (nreverse it)
           (--each it (org-ml~update nil #'map-to-subheadlines it))))))

(org-ml--defun* org-ml-update-subtrees (which fun)
  "Update some toplevel subtrees in the current buffer using FUN.

See `org-ml-parse-subtrees' for the meaning of WHICH.

Subtrees are updated using `org-ml~update' with DIFF-ARG set to
nil (see this for use and meaning of FUN)."
  (save-excursion
    (--> (org-ml--parse-patterns-where which "^\\* ")
         (nreverse it)
         (--each it (org-ml~update nil fun it)))))

(org-ml--defun* org-ml-update-supersections (which fun)
  "Update some headline supersections in the current using FUN.

See `org-ml-parse-headlines' for the meaning of WHICH.

Headlines are updated using `org-ml~update' with DIFF-ARG set to
nil (see this for use and meaning of FUN)."
  ;; don't use the myers diff algorithm here, since these functions are meant
  ;; for batch processing.
  (save-excursion
    (cl-labels
        ((map-to-subheadlines
           (headline)
           (-each (nreverse (org-ml-headline-get-subheadlines headline))
             #'map-to-subheadlines)
           (-let* (((ss0 &as &plist :pre-blank pb0 :section nodes0)
                    (org-ml-headline-get-supersection headline))
                   ((ss1 &as &plist :pre-blank pb1 :section nodes1) (funcall fun ss0)))
             (if (or (not pb1) (= pb0 pb1))
                 (let ((s (->> (-map #'org-ml-to-string nodes1)
                               (apply #'concat (make-string pb0 ?\n)))))
                   (if nodes0
                       (let ((begin (org-element-begin (-first-item nodes0)))
                             (end (org-element-end (-last-item nodes0))))
                         (org-ml--replace-region begin end s))
                     (let* ((begin (or (org-element-contents-begin headline)
                                       ;; If there are no contents, go to
                                       ;; headline start, try to go to next
                                       ;; line, and insert new line if we can't
                                       ;; (which means we are at the end)
                                       (progn
                                         (goto-char (org-element-begin headline))
                                         (forward-line)
                                         (if (bolp) (point)
                                           ;; use this since this plays nice
                                           ;; with evil mode
                                           (when (re-search-forward "$" nil t)
                                             (replace-match "\n" nil nil))
                                           (forward-line)
                                           (point))))))
                       (goto-char begin)
                       (insert s))))
               (let ((headline* (->> (org-ml-copy headline)
                                     (org-ml-headline-set-subheadlines nil)
                                     (org-ml-headline-set-supersection ss1)))
                     (begin (org-element-begin headline))
                     (end (or (outline-next-heading) (point-max))))
                 (org-ml--replace-bounds nil begin end headline*))))))
      (-each (nreverse (org-ml--parse-patterns-where which "^\\* "))
        #'map-to-subheadlines))))

(org-ml--defun* org-ml-update-supercontents (config which fun)
  "Update some headline supercontents in the current using FUN.

See `org-ml-parse-headlines' for the meaning of WHICH.

Headlines are updated using `org-ml~update' with DIFF-ARG set to
nil (see this for use and meaning of FUN)."
  (org-ml-update-supersections* which
    (->> (org-ml--supersection-to-supercontents config it)
         (funcall fun)
         (org-ml--supercontents-to-supersection config))))

;;; deprecated functions

(define-obsolete-function-alias 'org-ml-timestamp-get-range 'org-ml-timestamp-get-length "6.0.0")

(defun org-ml-timestamp-set-range (n timestamp)
  "Return TIMESTAMP node with range set to N seconds.

If TIMESTAMP is ranged, keep start time the same and adjust the end
time. If not, make a new end time. The units for RANGE are in minutes
if TIMESTAMP is in long format and days if TIMESTAMP is in short
format.

This function is deprecated. Use `org-ml-timestamp-set-length'
instead."
  (if (->> (org-ml-timestamp-get-start-time timestamp)
           (org-ml-timelist-has-time))
      (org-ml-timestamp-set-length (* 60 n) 'minute timestamp)
    (org-ml-timestamp-set-length (* 86400 n) 'day timestamp)))

(define-obsolete-function-alias `org-ml-time-is-long 'org-ml-timelist-has-time "6.0.0")

(define-obsolete-function-alias `org-ml-time-to-unixtime 'org-ml-timelist-to-unixtime "6.0.0")

(defun org-ml-unixtime-to-time-short (unixtime)
  "Convert UNIXTIME to list like (YEAR MONTH DAY).

This function is deprecated."
  (-take 3 (org-ml-unixtime-to-timelist nil unixtime)))

(defun org-ml-unixtime-to-time-long (unixtime)
  "Convert UNIXTIME to list like (YEAR MONTH DAY HOUR MINUTE).

This function is deprecated."
  (org-ml-unixtime-to-timelist t unixtime))

(provide 'org-ml)
;;; org-ml.el ends here
