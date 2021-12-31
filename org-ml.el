;;; org-ml.el --- Functional Org Mode API -*- lexical-binding: t; -*-

;; Copyright (C) 2020  Nathan Dwarshuis

;; Author: Nathan Dwarshuis <ndwar@yavin4.ch>
;; Keywords: org-mode, outlines
;; Homepage: https://github.com/ndwarshuis/org-ml
;; Package-Requires: ((emacs "26.1") (org "9.3") (dash "2.17") (s "1.12"))
;; Version: 5.7.3

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

(eval-when-compile
  (require 'org-ml-macs))

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
- nil: don't memoize anything
- 'compiled': memoize byte-compiled lambda forms
- any other non-nil: memoize non-compiled lambda forms"
  :type 'boolean
  :group 'org-ml)

(defcustom org-ml-parse-habits nil
  "Parse habits if set to t."
  :type 'boolean
  :group 'org-ml)

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
  "List of all element types including 'org-data'.")

(org-ml--defconst org-ml-objects
  (cons 'plain-text org-element-all-objects)
  "List of all object types including 'plain-text'.")

(defconst org-ml-nodes
  (eval-when-compile
    (append org-ml-elements org-ml-objects))
  "List of all node types.")

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
     ((and use-oor (< n lower)) lower)
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

(defun org-ml--is-node (list)
  "Return t if LIST is a node."
  (org-ml-is-any-type org-ml-nodes list))

(defun org-ml--is-table-row (node)
  "Return t if NODE is a standard table-row node."
  (and (org-ml-is-type 'table-row node)
       (org-ml--property-is-eq :type 'standard node)))

(defun org-ml--filter-type (type node)
  "Return NODE if it is TYPE or nil otherwise."
  (and (org-ml-is-type type node) node))

(defun org-ml--filter-types (types node)
  "Return NODE if it is one of TYPES or nil otherwise."
  (and (org-ml-is-any-type types node) node))

(defun org-ml--is-secondary-string (list)
  "Return t if LIST is a secondary string."
  (--none? (org-ml-is-any-type org-ml-elements it) list))

;;; MISC HELPER FUNCTIONS

(defun org-ml--get-head (node)
  "Return the type and properties cells of NODE."
  (if (stringp node) node (list (car node) (cadr node))))

(defun org-ml--construct (type props children)
  "Make a new node list structure of TYPE, PROPS, and CHILDREN.
TYPE is a symbol, PROPS is a plist, and CHILDREN is a list or nil."
  `(,type ,props ,@children))

(defun org-ml--from-string (string)
  "Convert STRING to org-element representation."
  (with-temp-buffer
    (insert string)
    (-> (org-ml-parse-this-buffer) (org-ml-get-children) (car))))

;;; INTERNAL PREDICATES

(defun org-ml--is-oneline-string (x)
  "Return t if X is a string with no newlines."
  (and (stringp x) (not (s-contains? "\n" x))))

(defun org-ml--is-oneline-string-or-nil (x)
  "Return t if X is a string with no newlines or nil."
  (or (null x) (org-ml--is-oneline-string x)))

(defun org-ml--is-non-neg-integer (x)
  "Return t if X is a non-negative integer."
  (and (integerp x) (<= 0 x)))

(defun org-ml--is-non-neg-integer-or-nil (x)
  "Return t if X is a non-negative integer or nil."
  (or (null x) (org-ml--is-non-neg-integer x)))

(defun org-ml--is-pos-integer (x)
  "Return t if X is a positive integer."
  (and (integerp x) (< 0 x)))

(defun org-ml--is-pos-integer-or-nil (x)
  "Return t if X is a positive integer or nil."
  (or (null x) (org-ml--is-pos-integer x)))

(defun org-ml--is-string-list (x)
  "Return t if X is a list of strings without newlines or nil."
  (or (null x) (and (listp x) (-all? #'org-ml--is-oneline-string x))))

;;; INTERNAL NODE PROPERTY FUNCTIONS

(defun org-ml-get-all-properties (node)
  "Return the properties list of NODE."
  (if (stringp node) (text-properties-at 0 node) (nth 1 node)))

(defun org-ml--get-property-nocheck (prop node)
  "Return PROP from NODE."
  (if (and (stringp node) (eq prop :post-blank))
      (length (car (s-match "[ ]*$" node)))
    (org-element-property prop node)))

(defun org-ml--get-parent (node)
  "Return the parent of NODE."
  (org-ml--get-property-nocheck :parent node))

(defun org-ml--get-parent-headline (node)
  "Return the most immediate parent headline node of NODE."
  (-when-let (parent (org-ml--get-property-nocheck :parent node))
    (if (org-ml-is-type 'headline parent) parent
      (org-ml--get-parent-headline parent))))

(defun org-ml--set-property-nocheck (prop value node)
  "Set PROP in NODE to VALUE."
  (if (stringp node)
      (if (eq prop :post-blank)
          (->> (s-trim-right node) (s-append (s-repeat value " ")))
        (org-add-props node nil prop value))
    (-let (((type . (props . children)) node))
      (org-ml--construct type (plist-put props prop value) children))))

(defun org-ml--set-properties-nocheck (plist node)
  "Set all properties in NODE to the values corresponding to PLIST.
PLIST is a list of property-value pairs that correspond to the
property list in NODE."
  ;; TODO this currently doesn't work with plain-text
  (if (org-ml--is-plist plist)
      (-let (((type . (props . children)) node))
        (--> (org-ml--reduce2-from* (plist-put acc it-key it) props plist)
             (org-ml--construct type it children)))
    (org-ml--arg-error "Not a plist: %S" plist)))

(defun org-ml--set-property-nocheck-nil (prop node)
  "Set PROP to nil in NODE."
  (org-ml--set-property-nocheck prop nil node))

(defun org-ml--set-properties-nocheck-nil (props node)
  "Set all PROPS to new in NODE."
  ;; TODO this currently doesn't work with plain-text
  (-let (((type . (nprops . children)) node))
    (--> (--reduce-from (plist-put acc it nil) nprops props)
         (org-ml--construct type it children))))

(eval-when-compile
  (defmacro org-ml--map-property-nocheck* (prop form node)
    "Return NODE with FUN applied to the value in PROP.
FUN is a form that returns a modified value and contains `it'
bound to the property value."
    (declare (indent 1))
    (let ((node* (make-symbol "node")))
      `(let ((,node* ,node))
         (let ((it (org-ml--get-property-nocheck ,prop ,node*)))
           (org-ml--set-property-nocheck ,prop ,form ,node*))))))

(defun org-ml--property-is-nil (prop node)
  "Return t if PROP in NODE is nil."
  (not (org-ml--get-property-nocheck prop node)))

(defun org-ml--property-is-eq (prop val node)
  "Return t if PROP in NODE is `eq' to VAL."
  (eq val (org-ml--get-property-nocheck prop node)))

(eval-when-compile
  (defmacro org-ml--property-is-predicate* (prop form node)
    "Return t if FUN applied to the value of PROP in NODE results not nil.
FORM is a predicate form that takes one with `it' bound to the
property value."
    `(let ((it (org-ml--get-property-nocheck ,prop ,node))) (and ,form t))))

;;; NODE PROPERTY TRANSLATION AND CHECKING FRAMEWORK

;; This code provides the internal framework for the following
;; operations where NODE is any node, PROP is a property of NODE,
;; and VALUE is the value of PROP:

;; Get: f(PROP NODE) -> VALUE
;; Set: f(PROP VALUE NODE) -> NODE'
;; Map: f(PROP FUN NODE) -> NODE' where FUN is a function that
;;      modifies the value of PROP in NODE and is like:
;;      f(VALUE) -> VALUE'

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
;;     - :encode - a unary function that converts VALUE to IVALUE; if this not
;;       given then VALUE and IVALUE are `equal'
;;     - :decode - a function that inverts the function at :encode
;;     - :cis - a unary function that takes NODE and returns a modified NODE;
;;       the point of this it so 'update' other properties when PROP is changed
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
;; 1) if PROP(VALUE) -> t, proceed to 2), else throw error
;; 2) ISET(PROP, ENCODE(VALUE), NODE)) -> NODE' where ISET sets the PROP of NODE
;;    to IVALUE
;; 3) If CIS is non-nil, run CIS(NODE') -> NODE'', else return NODE'

;; map: MAP(PROP FUN NODE) -> NODE'
;; 1) GET(PROP NODE) -> VALUE
;; 2) FUN(VALUE) -> VALUE'
;; 3) if PRED(VALUE') -> t proceed to 4), else throw error
;; 4) SET(PROP VALUE' NODE) -> NODE'

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
       (--all? (org-ml-is-any-type org-ml--item-tag-restrictions it) x)))

(defun org-ml--is-valid-item-bullet (x)
  "Return t if X is an allowed value for a item node bullet property."
  ;; NOTE org mode 9.1.9 has the following limitations:
  ;; - "+" will be converted to "-" when interpreted
  ;; - "1)" will be converted to "1." when interpreted
  ;; - alphanumeric symbols make the interpreter crash
  (pcase x ((or '- (pred integerp)) t)))

(defun org-ml--is-valid-clock-timestamp (x)
  "Return t if X is an allowed value for a clock node value property."
  (and (org-ml-is-type 'timestamp x)
       (org-ml--property-is-predicate* :type
         (memq it '(inactive inactive-range)) x)
       (org-ml--property-is-nil :repeater-type x)))

(defun org-ml--is-valid-planning-unclosed-timestamp (x)
  "Return t if X is an allowed value for a planning node timestamp property."
  (or (null x) (and (org-ml-is-type 'timestamp x)
                    (org-ml--property-is-eq :type 'active x))))

(defun org-ml--is-valid-planning-closed-timestamp (x)
  "Return t if X is an allowed value for a planning node timestamp property."
  (or (null x) (and (org-ml-is-type 'timestamp x)
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
   (--all? (org-ml-is-any-type org-ml--headline-title-restrictions it) x)))

(defun org-ml--is-valid-timestamp-type (x)
  "Return t if X is an allowed value for a timestamp node type property."
  (memq x '(inactive inactive-range active active-range)))

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
  (-some->> (org-ml--map* (format "%S" it) plist) (s-join " ")))

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
  (->> (reverse plists)
       (org-ml--map*
        (-some->> it
          (-partition 2)
          (org-ml--map* (format "%S %s" (car it) (cadr it)))
          (s-join " ")))))

(defun org-ml--decode-header (headers)
  "Return HEADERS (a list of strings) as a list of plists."
  (->> (reverse headers)
       (org-ml--map*
        (->> (org-ml--decode-string-list-space-delim it)
             (--map-indexed (if (cl-evenp it-index) (intern it) it))))))

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
  (->> caption
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
   nil internal-caption))

;;; cis functions

(defun org-ml--update-macro-value (macro)
  "Return MACRO node with its value property updated.
This will be based on MACRO's key and value properties."
  (let* ((k (org-ml--get-property-nocheck :key macro))
         (as (org-ml--get-property-nocheck :args macro))
         (v (if as (format "%s(%s)" k (s-join "," as)) k)))
    (org-ml--set-property-nocheck :value (format "{{{%s}}}" v) macro)))

(defun org-ml--update-clock-duration-and-status (clock)
  "Return CLOCK node with its duration and status properties updated.
This will be based on CLOCK's value property."
  (let* ((ts (org-ml--get-property-nocheck :value clock))
         (seconds (org-ml--timestamp-get-range ts)))
    (if (= seconds 0)
        (->> (org-ml--set-property-nocheck :duration nil clock)
             (org-ml--set-property-nocheck :status 'running))
      (let* ((h (-> seconds (/ 3600) (floor)))
             (m (-> seconds (- (* h 3600)) (/ 60) (floor)))
             ;; if the clock is going from non-ranged to ranged, it may not be
             ;; in collapsed form; ensure it is not in collapsed form
             ;;
             ;; TODO it may be cleaner to simply restrict the clocks to being in
             ;; non-collapsed form (eg at using the :pred slot in the master
             ;; property-alist table) however...I'm pretty sure collapsed clocks
             ;; are still valid org-syntax, even if they don't appear by default
             (ts* (org-ml-timestamp-set-collapsed nil ts)))
        (->> clock
             (org-ml--set-property-nocheck :duration (format "%2d:%02d" h m))
             (org-ml--set-property-nocheck :status 'closed)
             (org-ml--set-property-nocheck :value ts*))))))

(defun org-ml--update-headline-tags (headline)
  "Return HEADLINE node with its tags updated.
This will be based on HEADLINE's archivedp property."
  (org-ml--map-property-nocheck* :tags
    (let ((tags* (remove org-archive-tag it)))
      (if (org-ml--get-property-nocheck :archivedp headline)
          (-snoc tags* org-archive-tag) tags*))
    headline))


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
                                     "`week' `day', or `hour'")))
         (post-blank `(:post-blank ,@nn-int
                                   :shift org-ml--shift-non-neg-integer)))
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
                   :type-desc "a secondary string")
             (:structure))
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
       (plain-list (:structure)
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
       (special-block (:type ,@ol-str :require t))
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
                  (:warning-type :pred org-ml--is-valid-timestamp-warning-type
                                 :type-desc ("nil or a symbol from"
                                             "`all' or `first'"))
                  (:warning-unit ,@ts-unit)
                  (:warning-value ,@pos-int-nil)
                  (:raw-value))
       (underline)
       (verbatim (:value ,@str :require t))
       (verse-block))
     ;; add post-blank/begin/end to everything
     (--map (append it `(,post-blank (:begin) (:end) (:parent))))
     (--map-when (memq (car it) org-ml-branch-nodes)
                 (-snoc it '(:contents-begin) '(:contents-end)))
     (--map-when (memq (car it) org-ml-elements)
                 (-snoc it '(:post-affiliated)))
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

(defun org-ml--get-property-attribute (attribute type prop)
  "Return ATTRIBUTE for PROP of node TYPE.
Signal an error if PROP in TYPE does not have ATTRIBUTE."
  (-if-let (type-list (alist-get type org-ml--property-alist))
      (if (assq prop type-list)
          (-when-let (plist (alist-get prop type-list))
            (plist-get plist attribute))
        (org-ml--arg-error "Type '%s' does not have property '%s'" type prop))
    (org-ml--arg-error "Tried to query property '%s' for non-existent type '%s'" prop type)))

(defun org-ml--get-property-encoder (type prop)
  "Return the encoder function for PROP of node TYPE."
  (org-ml--get-property-attribute :encode type prop))

(defun org-ml--get-property-decoder (type prop)
  "Return the decoder function for PROP of node TYPE."
  (org-ml--get-property-attribute :decode type prop))

(defun org-ml--get-property-cis-function (type prop)
  "Return the cis function for PROP of node TYPE."
  (org-ml--get-property-attribute :cis type prop))

(defun org-ml--get-property-type-desc (type prop)
  "Return the type-description string for PROP of node TYPE."
  (let ((desc (org-ml--get-property-attribute :type-desc type prop)))
    (if (listp desc) (s-join " " desc) desc)))

;;; INTERNAL BRANCH/CHILD MANIPULATION

(defun org-ml--get-descendent (indices node)
  "Return the nested children of NODE as given by INDICES.
INDICES is a list of integers specifying the index and level of the
nested element to return."
  (if (not indices) node
    (->> (org-ml-get-children node)
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
    `(let* ((node ,node)
            (it (org-ml-get-children node)))
       (org-ml--set-children-nocheck ,form node))))

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

(defun org-ml--init-properties (props)
  "Return a plist where the keys are PROPS and all values are nil."
  (apply #'append (org-ml--map* (list it nil) props)))

(defun org-ml--build-bare-node (type post-blank)
  "Return a new node assembled from TYPE with POST-BLANK.
TYPE is a symbol and POST-BLANK is a positive integer."
  `(,type (:post-blank ,(or post-blank 0))))

(defun org-ml--build-leaf-node (type post-blank)
  "Return a new object-typed node from TYPE and POST-BLANK."
  (org-ml--build-bare-node type post-blank))

(defun org-ml--build-branch-node (type post-blank children)
  "Return a new branch object-typed node from TYPE, POST-BLANK, and CHILDREN."
  (->> (org-ml--build-bare-node type post-blank)
       (org-ml-set-children children)))

(defun org-ml--build-blank-node (type)
  "Return a new node of type TYPE with all properties set to nil."
  (->> (alist-get type org-ml--property-alist)
       (-map #'car)
       (org-ml--init-properties)
       (list type)))

;;; base builders

;; define all base builders using this automated monstrosity

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
                          (format "- %s: %s %s" p r))))
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
           (const-props (-some->> (alist-get 'const props)
                                  (--mapcat (list (car it)
                                                  (plist-get (cdr it) :const)))
                                  (org-ml--autodef-prop-form 2
                                    #'org-ml--set-property-nocheck
                                    #'org-ml--set-properties-nocheck)))
           (nil-props (-some->> (alist-get 'null props)
                                (-map #'car)
                                (org-ml--autodef-prop-form 1
                                  #'org-ml--set-property-nocheck-nil
                                  #'org-ml--set-properties-nocheck-nil)))
           (strict-props (-some->> (append (alist-get 'key props)
                                           (alist-get 'req props))
                                   (-map #'car)
                                   (--mapcat (list it (org-ml--autodef-kwd-to-sym it)))
                                   (org-ml--autodef-prop-form 2
                                     #'org-ml-set-property
                                     #'org-ml-set-properties)))
           (doc (org-ml--autodef-make-docstring type rest-arg props))
           (builder (let ((a `(',type post-blank)))
                      (if rest-arg `(org-ml--build-branch-node ,@a ,rest-arg)
                        `(org-ml--build-leaf-node ,@a))))
           (body (if (or strict-props nil-props const-props)
                     `(->> ,@(-non-nil (list builder const-props
                                             nil-props strict-props)))
                   builder)))
      (macroexpand `(org-ml--defun-kw ,name ,args ,doc ,body))))

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
  (let ((value (org-ml--get-property-nocheck :value statistics-cookie)))
    (cond ((s-contains? "/" value) 'fraction)
          ((s-contains? "%" value) 'percent)
          (t (org-ml--arg-error "Unparsable statistics cookie: %s" value)))))

;; timestamp (auxiliary functions)

(defun org-ml-time-is-long (time)
  "Return t if TIME is a long format time list."
  (pcase time
    (`(,(pred integerp) ,(pred integerp) ,(pred integerp)
       ,(pred integerp) ,(pred integerp))
     t)))

;; make these public, not sure where else to put them
(defun org-ml-time-to-unixtime (time)
  "Return the unix time (integer seconds) of time list TIME.
The returned value is dependent on the time zone of the operating
system."
  (->> (if (org-ml-time-is-long time)
           (-let (((y m d H M) time))
             (list 0 M H d m y nil -1 (current-time-zone)))
         (-let (((y m d) time))
           (list 0 0 0 d m y nil -1 (current-time-zone))))
       (encode-time)
       (float-time)
       (round)))

(defun org-ml-unixtime-to-time-long (unixtime)
  "Return the long time list of UNIXTIME.
The list will be formatted like (YEAR MONTH DAY HOUR MIN)."
  (reverse (-slice (decode-time unixtime (current-time-zone)) 1 6)))

(defun org-ml-unixtime-to-time-short (unixtime)
  "Return the short time list of UNIXTIME.
The list will be formatted like (YEAR MONTH DAY nil nil)."
  (append (-take 3 (org-ml-unixtime-to-time-long unixtime)) '(nil nil)))

(defun org-ml--time-truncate (time)
  "Return the short time format of TIME regardless of input format."
  (-take 3 time))

(defun org-ml--time-shift (n unit time)
  "Return modified time list TIME shifted N UNIT's.

UNIT is one of `day', `week', `month', `year', `minute', or `hour'.
N is an integer."
  (-let (((i s) (cond
                 ((eq unit 'year) (list 0 n))
                 ((eq unit 'month) (list 1 n))
                 ((eq unit 'week) (list 2 (* 7 n)))
                 ((eq unit 'day) (list 2 n))
                 ((and (eq unit 'hour) (org-ml-time-is-long time)) (list 3 n))
                 ((and (eq unit 'minute) (org-ml-time-is-long time)) (list 4 n))
                 (t (org-ml--arg-error "Invalid time unit: %S" unit)))))
    (org-ml--map-at* i (+ s it) time)))

(defconst org-ml--time-start-keys
  '(:year-start :month-start :day-start :hour-start :minute-start)
  "Properties for the starting time values of a timestamp node.")

(defconst org-ml--time-end-keys
  '(:year-end :month-end :day-end :hour-end :minute-end)
  "Properties for the ending time values of a timestamp node.")

(defun org-ml--time-format-props (time start?)
  "Return plist representation of time list TIME.
If START? is t, the plist will represent a start time, else and
end time."
  (let* ((props (if start? org-ml--time-start-keys org-ml--time-end-keys))
         (time* (pcase time
                  (`(,(pred integerp)
                     ,(pred integerp)
                     ,(pred integerp))
                   (append time '(nil nil)))
                  ((or `(,(pred integerp)
                         ,(pred integerp)
                         ,(pred integerp)
                         ,(pred integerp)
                         ,(pred integerp))
                       `(,(pred integerp)
                         ,(pred integerp)
                         ,(pred integerp)
                         ,(pred null)
                         ,(pred null)))
                   time)
                  (`nil (-repeat 5 nil))
                  (_ (org-ml--arg-error "Invalid time given: %s" time)))))
    (-interleave props time*)))

(defconst org-ml--warning-keys
  '(:warning-type :warning-value :warning-unit)
  "Properties for timestamp warning.")

(defconst org-ml--repeater-keys
  '(:repeater-type :repeater-value :repeater-unit)
  "Properties for timestamp repeater.")

(defun org-ml--decorator-format (dec warning? valid-types)
  "Return plist representing a timestamp warning or repeater (decorators).

DEC is a list like (TYPE VALUE UNIT) of the decorator, WARNING?
is t if the decorator is a warning or nil if it is a repeater,
and VALID-TYPES are the allowed values for TYPE given in DEC."
  (let ((props (if warning? org-ml--warning-keys org-ml--repeater-keys)))
    (if (not dec) (org-ml--init-properties props)
      (-let (((type value unit) dec))
        (unless (or (not type) (memq type valid-types))
          (org-ml--arg-error "Invalid decorator type: %s" type))
        (unless (or (not value) (integerp value))
          (org-ml--arg-error "Invalid decorator value: %s" value))
        (unless (or (not unit) (memq unit '(year month week day hour)))
          (org-ml--arg-error "Invalid decorator unit: %s" unit))
        (-interleave props dec)))))

;; timestamp (regular)

(defun org-ml--timestamp-get-start-time (timestamp)
  "Return the time list of the start time in TIMESTAMP."
  (-let (((&plist :minute-start n :hour-start h :day-start d
                  :month-start m :year-start y)
          (org-ml-get-all-properties timestamp)))
    `(,y ,m ,d ,h ,n)))

(defun org-ml--timestamp-get-end-time (timestamp)
  "Return the time list of the end time in TIMESTAMP."
  (-let (((&plist :minute-end n :hour-end h :day-end d
                  :month-end m :year-end y)
          (org-ml-get-all-properties timestamp)))
    `(,y ,m ,d ,h ,n)))

(defun org-ml--timestamp-get-start-unixtime (timestamp)
  "Return the unixtime of the start time in TIMESTAMP."
  (->> (org-ml--timestamp-get-start-time timestamp)
       (org-ml-time-to-unixtime)))

(defun org-ml--timestamp-get-end-unixtime (timestamp)
  "Return the unixtime of the end time in TIMESTAMP."
  (->> (org-ml--timestamp-get-end-time timestamp)
       (org-ml-time-to-unixtime)))

(defun org-ml--timestamp-get-range (timestamp)
  "Return the range of TIMESTAMP in seconds."
  (- (org-ml--timestamp-get-end-unixtime timestamp)
     (org-ml--timestamp-get-start-unixtime timestamp)))

(defun org-ml--timestamp-is-active (timestamp)
  "Return t if TIMESTAMP is an active type."
  (memq (org-ml--get-property-nocheck :type timestamp) '(active active-range)))

(defun org-ml--timestamp-is-range-type (timestamp)
  "Return t if TIMESTAMP has a range type."
  (memq (org-ml--get-property-nocheck :type timestamp)
        '(active-range inactive-range)))

(defun org-ml--timestamp-is-ranged (timestamp)
  "Return t if TIMESTAMP has a range greater than 0 seconds."
  (/= 0 (org-ml--timestamp-get-range timestamp)))

(defun org-ml--timestamp-is-ranged-lowres (timestamp)
  "Return t if TIMESTAMP is range according to only year, month, and day."
  (-let* (((l s) (-split-at 3 (org-ml--timestamp-get-start-time timestamp)))
          ((L S) (-split-at 3 (org-ml--timestamp-get-end-time timestamp))))
    ;; lowres if Y/M/D is different and Min/Hour are the same but only if
    ;; Min/Hour are both not nil
    (and (not (equal l L)) (or (equal s S)
                               (memq nil s)
                               (memq nil S)))))

(defun org-ml--timestamp-set-start-time-nocheck (time timestamp)
  "Set the start TIME of TIMESTAMP. Does not set type."
  (let ((time* (org-ml--time-format-props time t)))
      (org-ml--set-properties-nocheck time* timestamp)))

(defun org-ml--timestamp-set-start-time (time timestamp)
  "Return TIMESTAMP with start time properties set according to time list TIME."
  (->> (org-ml--timestamp-set-start-time-nocheck time timestamp)
       (org-ml--timestamp-update-type-ranged)))

(defun org-ml--timestamp-set-end-time-nocheck (time timestamp)
  "Set the end TIME of TIMESTAMP. Does not set type."
  (if time
      (-> (org-ml--time-format-props time nil)
          (org-ml--set-properties-nocheck timestamp))
    (-> (org-ml--timestamp-get-start-time timestamp)
        (org-ml--time-format-props nil)
        (org-ml--set-properties-nocheck timestamp))))

(defun org-ml--timestamp-set-end-time (time timestamp)
  "Return TIMESTAMP with end time properties set according to time list TIME."
  (let ((ts* (org-ml--timestamp-set-end-time-nocheck time timestamp)))
    (if time (org-ml--timestamp-update-type-ranged ts*)
      (org-ml--timestamp-set-type-ranged nil ts*))))

(defun org-ml--timestamp-set-single-time (time timestamp)
  "Return TIMESTAMP with start/end properties set to time list TIME."
  (->> (org-ml--timestamp-set-start-time-nocheck time timestamp)
       (org-ml--timestamp-set-end-time-nocheck time)
       (org-ml--timestamp-set-type-ranged nil)))

(defun org-ml--timestamp-set-double-time (time1 time2 timestamp)
  "Return TIMESTAMP with start and end properties set to time lists TIME1 and TIME2."
  (->> (org-ml--timestamp-set-start-time-nocheck time1 timestamp)
       (org-ml--timestamp-set-end-time-nocheck time2)
       (org-ml--timestamp-update-type-ranged)))

(defun org-ml--timestamp-set-range (range timestamp)
  "Return TIMESTAMP with end time shifted to RANGE seconds from start time."
  (let* ((start (org-ml--timestamp-get-start-time timestamp))
         (long? (org-ml-time-is-long start))
         (range (* range (if long? 60 86400)))
         (t2 (--> (org-ml-time-to-unixtime start)
                  (+ it range)
                  (if long? (org-ml-unixtime-to-time-long it)
                    (org-ml-unixtime-to-time-short it)))))
    (->> (org-ml--timestamp-set-end-time-nocheck t2 timestamp)
         (org-ml--timestamp-update-type-ranged))))

(defun org-ml--timestamp-update-type-ranged (timestamp)
  "Return TIMESTAMP with updated type based on if it is ranged."
  (-> (org-ml--timestamp-is-ranged-lowres timestamp)
      (org-ml--timestamp-set-type-ranged timestamp)))

(defun org-ml--timestamp-set-type-ranged (ranged? timestamp)
  "Return TIMESTAMP with type set according to RANGED?."
  (org-ml--map-property-nocheck* :type
    (pcase it
      ((or `active `active-range)
       (if ranged? 'active-range 'active))
      ((or `inactive `inactive-range)
       (if ranged? 'inactive-range 'inactive))
      (e (org-ml--arg-error "Invalid timestamp type: %s" e)))
    timestamp))

(defun org-ml--timestamp-set-active (flag timestamp)
  "Return TIMESTAMP with active type if FLAG is t."
  (let* ((type (if (org-ml--timestamp-is-ranged-lowres timestamp)
                   (if flag 'active-range 'inactive-range)
                 (if flag 'active 'inactive))))
    (org-ml--set-property-nocheck :type type timestamp)))

(defun org-ml--timestamp-set-warning (warning timestamp)
  "Return TIMESTAMP with warning properties set to WARNING list."
  (-> (org-ml--decorator-format warning t '(all first))
      (org-ml--set-properties-nocheck timestamp)))

(defun org-ml--timestamp-set-repeater (repeater timestamp)
  "Return TIMESTAMP with warning properties set to REPEATER list."
  (-> (org-ml--decorator-format repeater nil '(catch-up restart cumulate))
      (org-ml--set-properties-nocheck timestamp)))

(defun org-ml--timestamp-shift-start (n unit timestamp)
  "Return TIMESTAMP with start time shifted N UNIT's."
  (let ((time* (->> (org-ml--timestamp-get-start-time timestamp)
                    (org-ml--time-shift n unit))))
    (->> (org-ml--timestamp-set-start-time time* timestamp)
         (org-ml--timestamp-update-type-ranged))))

(defun org-ml--timestamp-shift-end (n unit timestamp)
  "Return TIMESTAMP with end time shifted N UNIT's."
  (let ((time* (->> (org-ml--timestamp-get-end-time timestamp)
                    (org-ml--time-shift n unit))))
    (->> (org-ml--timestamp-set-end-time time* timestamp)
         (org-ml--timestamp-update-type-ranged))))

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
for all sensible items)."
  (-let* (((h (s . r)) (->> (org-ml-get-children item)
                            (--split-with (not (org-ml-is-type 'plain-list it)))))
          (pb (if s (org-ml--get-property-nocheck :post-blank s) 0))
          (i (org-ml-get-children s)))
    (list h i pb r)))

(defun org-ml--item-set-subcomponents (subcomponents item)
  "Return the child subcomponents of ITEM.
SUBCOMPONENTS is a list like that returned by
`org-ml--item-get-subcomponents'."
  (-let* (((head subitems sub-pb rest) subcomponents))
    (-when-let (pb (cond
                    (rest (-some->> (-last-item rest)
                            (org-ml--get-property-nocheck :post-blank)))
                    (subitems sub-pb)
                    (head (-some->> (-last-item head)
                            (org-ml--get-property-nocheck :post-blank)))))
      (let ((rest* (-some->> rest
                     (org-ml--map-last*
                      (org-ml--set-property-nocheck :post-blank 0 it))))
            (sublist (-some->> subitems
                       (apply #'org-ml-build-plain-list
                              :post-blank (if rest sub-pb 0))
                       (list)))
            (head* (-some->> head
                     (org-ml--map-last*
                      (org-ml--set-property-nocheck :post-blank 0 it)))))
        (->> (org-ml--set-children-nocheck (append head* sublist rest*) item)
             (org-ml--map-property-nocheck* :post-blank (+ pb it)))))))

(defmacro org-ml--item-map-subcomponents* (form item)
  "Return ITEM with subcomponents modified.
FORM is a form where the subcomponents of item are bound to the
symbol 'it' and returns modified subcomponents. The subcomponents
will conform to those given in `org-ml--item-get-subcomponents'."
  (declare (debug (form form)))
  (let ((i (make-symbol "item")))
    `(let ((,i ,item))
       (let ((it (org-ml--item-get-subcomponents ,i)))
         (org-ml--item-set-subcomponents ,form ,i)))))

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
FORM is a Lisp form in which the symbol 'it' is bound to the
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
  (org-ml--map-property-nocheck* :level (org-ml--shift-pos-integer n it) headline))

(defun org-ml--headline-set-statistics-cookie (value headline)
  "Return HEADLINE node with statistics cookie set by VALUE.
VALUE is a list conforming to `org-ml--is-valid-statistics-cookie-value'
or nil to erase the statistics cookie if present."
  (org-ml--map-property-nocheck*
   :title
   (let ((last? (org-ml-is-type 'statistics-cookie (-last-item it))))
     (cond
      ((and last? value)
       (org-ml--map-last* (org-ml-set-property :value value it) it))
      ((and last? (not value))
       (-drop-last 1 it))
      (value
       (-snoc it (org-ml-build-statistics-cookie value)))
      (t it)))
   headline))

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

(defun org-ml--planning-list-to-timestamp (active planning-list)
  "Return timestamp node from PLANNING-LIST.
See `org-ml-build-planning!' for syntax of PLANNING-LIST.
ACTIVE is a flag denoting if the timestamp is to be active."
  (when planning-list
    (let* ((p (-partition-before-pred
               (lambda (it) (memq it '(&warning &repeater)))
               planning-list)))
      (org-ml-build-timestamp! (car p)
                           :active active
                           :warning (alist-get '&warning p)
                           :repeater (alist-get '&repeater p)))))

;;; INTERNAL TYPE-SPECIFIC BRANCH/CHILD FUNCTIONS

;;; headline

(defun org-ml-headline-get-section (headline)
  "Return children of section node in HEADLINE node or nil if none."
  (-some--> (car (org-ml-get-children headline))
    (when (org-ml-is-type 'section it) (org-ml-get-children it))))

(defun org-ml-headline-set-section (children headline)
  "Return HEADLINE with section node containing CHILDREN.
If CHILDREN is nil, return HEADLINE with no section node."
  (org-ml--map-children-nocheck*
    (if (org-ml-is-type 'section (car it))
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
  (let ((children (org-ml-get-children headline)))
    (if (org-ml-is-type 'section (car children)) (cdr children) children)))

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
  (->> (org-ml--headline-shift-level n headline)
       (org-ml-headline-map-subheadlines*
         (org-ml--map* (org-ml--headline-subtree-shift-level n it) it))))

(defun org-ml--headline-set-level (level headline)
  "Return HEADLINE node with its level set to LEVEL.
Additionally set all child headline nodes to be (+ 1 level) for
first layer, (+ 2 level) for second, and so on."
  (->> (org-ml-set-property :level level headline)
       (org-ml-headline-map-subheadlines*
         (org-ml--map* (org-ml--headline-set-level (1+ level) it) it))))

;;; table

(defun org-ml--table-get-width (table)
  "Return the width of TABLE as an integer.
This effectively is the maximum of all table-row lengths."
  (->> (org-ml-get-children table)
       (org-ml--map* (length (org-ml-get-children it)))
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
  (-some->> (org-ml-get-children table)
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

;; these are nodes that cannot and should not be built with the 'normal' build
;; functions because they are too weird

;; ...and this is here because I can't think of where else to put it
(defun org-ml-clone-node (node)
  "Return copy of NODE, which may be a circular tree.

This is only necessary to copy nodes parsed using any of parsing
functions from this package (for example, `org-ml-parse-this-headline')
because these retain parent references which makes the node a circular
list. None of the builder functions add parent references, so
`copy-tree' will be a faster alternative to this function."
  (let ((print-circle t))
    (read (format "%S" node))))

(defun org-ml-clone-node-n (n node)
  "Like `org-ml-clone-node' but make N copies of NODE."
  (let ((ret))
    (--dotimes n (!cons (org-ml-clone-node node) ret))
    ret))

(org-ml--defun-kw org-ml-build-timestamp-diary (form &key post-blank)
  "Return a new diary-sexp timestamp node from FORM.
Optionally set POST-BLANK (a positive integer)."
  (->> (org-ml--build-blank-node 'timestamp)
       (org-ml-set-property :post-blank (or post-blank 0))
       (org-ml--set-property-nocheck :type 'diary)
       (org-ml-timestamp-diary-set-value form)))

(org-ml--defun-kw org-ml-build-table-row-hline (&key post-blank)
  "Return a new rule-typed table-row node.
Optionally set POST-BLANK (a positive integer)."
  (->> (org-ml--build-blank-node 'table-row)
       (org-ml-set-property :post-blank (or post-blank 0))
       (org-ml--set-property-nocheck :type 'rule)))

;;; shorthand builders

;; These function offer a shorter and more convenient way of building
;; nodes. They all end in '!' (and all associated functions later
;; that replicate their syntax here do the same)

(defun org-ml-build-secondary-string! (string)
  "Return a secondary string (list of object nodes) from STRING.
STRING is any string that contains a textual representation of
object nodes. If the string does not represent a list of object nodes,
throw an error."
  ;; fool parser to always parse objects, bold will parse to headlines
  ;; because of the stars
  (-if-let (ss (->> (org-ml--from-string (concat " " string))
                    (org-ml--get-descendent '(0))
                    (org-ml-get-children)))
      (cond
       ((not (org-ml--is-secondary-string ss))
        (org-ml--arg-error "Secondary string must only contain objects"))
       ((equal (car ss) " ")
        (-drop 1 ss))
       (t (org-ml--map-first* (substring it 1) ss)))
    (org-ml--arg-error "Could not make secondary string from %S" string)))

(org-ml--defun-kw org-ml-build-timestamp! (start &key end active repeater
                                                 warning post-blank)
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

REPEATER and WARNING are lists corresponding to those required
for `org-ml-timestamp-set-repeater' and
`org-ml-timestamp-set-warning' respectively.

Building a diary sexp timestamp is not possible with this function."
  (->> (org-ml--build-blank-node 'timestamp)
       (org-ml-set-property :post-blank (or post-blank 0))
       (org-ml--timestamp-set-start-time-nocheck start)
       (org-ml--timestamp-set-end-time-nocheck end)
       (org-ml--timestamp-set-active active)
       (org-ml-timestamp-set-warning warning)
       (org-ml-timestamp-set-repeater repeater)))

(org-ml--defun-kw org-ml-build-clock! (start &key end post-blank)
  "Return a new clock node.

START and END follow the same rules as their respective arguments in
`org-ml-build-timestamp!'."
  (let ((ts (org-ml-build-timestamp! start :end end)))
    (org-ml-build-clock ts :post-blank post-blank)))

(org-ml--defun-kw org-ml-build-planning! (&key closed deadline scheduled
                                               post-blank)
  "Return a new planning node.

CLOSED, DEADLINE, and SCHEDULED are lists with the following structure
\(brackets denote optional members):

\(YEAR MINUTE DAY [HOUR] [MIN]
 [&warning TYPE VALUE UNIT]
 [&repeater TYPE VALUE UNIT])

In terms of arguments supplied to `org-ml-build-timestamp!', the first
five members correspond to the list supplied as TIME, and the TYPE,
VALUE, and UNIT fields correspond to the lists supplied to WARNING and
REPEATER arguments. The order of warning and repeater does not
matter."
  (org-ml-build-planning
   :closed (org-ml--planning-list-to-timestamp nil closed)
   :deadline (org-ml--planning-list-to-timestamp t deadline)
   :scheduled (org-ml--planning-list-to-timestamp t scheduled)
   :post-blank post-blank))

(org-ml--defun-kw org-ml-build-property-drawer! (&key post-blank &rest keyvals)
  "Return a new property-drawer node.

Each member in KEYVALS is a list like (KEY VAL) where KEY and VAL
are both strings, where each list will generate a node-property
node in the property-drawer node like \":key: val\"."
  (->> keyvals
       (org-ml--map* (let ((key (symbol-name (car it)))
                           (val (symbol-name (cadr it))))
                       (org-ml-build-node-property key val)))
       (apply #'org-ml-build-property-drawer :post-blank post-blank)))

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
  (let* ((planning (-some->> planning (apply #'org-ml-build-planning!)))
         (section (-some->>  (if planning (cons planning section-children)
                               section-children)
                    (apply #'org-ml-build-section)))
         (shls (org-ml--map* (org-ml--headline-set-level (1+ level) it)
                             subheadlines))
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
         (org-ml-headline-set-title! title-text statistics-cookie))))

(org-ml--defun-kw org-ml-build-paragraph! (string &key post-blank)
  "Return a new paragraph node from STRING.

STRING is the text to be parsed into a paragraph and must contain
valid textual representations of object nodes."
  (->> (org-ml-build-secondary-string! string)
       (apply #'org-ml-build-paragraph :post-blank (or post-blank 0))))

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
           children*)))

(defun org-ml-build-table-cell! (string)
  "Return a new table-cell node.

STRING is the text to be contained in the table-cell node. It must
contain valid textual representations of objects that are allowed in
table-cell nodes."
  (apply #'org-ml-build-table-cell (org-ml-build-secondary-string! string)))

(defun org-ml-build-table-row! (row-list)
  "Return a new table-row node.

ROW-LIST is a list of strings to be built into table-cell nodes via
`org-ml-build-table-cell!' (see that function for restrictions).
Alternatively, ROW-LIST may the symbol `hline' instead of a string to
create a rule-typed table-row."
  (if (eq row-list 'hline) (org-ml-build-table-row-hline)
    (->> (-map #'org-ml-build-table-cell! row-list)
         (apply #'org-ml-build-table-row))))

(org-ml--defun-kw org-ml-build-table! (&key tblfm post-blank &rest row-lists)
  "Return a new table node.

ROW-LISTS is a list of lists where each member list will be converted
to a table-row node via `org-ml-build-table-row!' (see that function for
restrictions).

All other arguments follow the same rules as `org-ml-build-table'."
  (->> (-map #'org-ml-build-table-row! row-lists)
       (apply #'org-ml-build-table :tblfm tblfm :post-blank post-blank)))

(defun org-ml-build-org-data (&rest headline-or-sections-nodes)
  "Return a new org-data node."
  (->> (org-ml--build-blank-node 'org-data)
    (org-ml--set-property-nocheck-nil :beg)
    (org-ml--set-property-nocheck-nil :end)
    (org-ml-set-children headline-or-sections-nodes)))

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
        (time (if long-p (org-ml-unixtime-to-time-long unixtime)
                (org-ml-unixtime-to-time-short unixtime))))
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
'done` cell in `org-log-note-headings'.

UNIXTIME is an integer representing the time to be used for all
timestamp nodes.

If string NOTE is supplied, append a note to the log entry."
  (->> (org-ml--log-get 'done)
       (org-ml--log-replace-timestamp unixtime nil t)
       (org-ml--build-log-item note)))

(defun org-ml-build-log-state (unixtime new-state old-state &optional note)
  "Return an item node for a state change log entry.

This will format the log entry from the default value for the
'state` cell in `org-log-note-headings'.

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
'note` cell in `org-log-note-headings'.

UNIXTIME is an integer representing the time to be used for all
timestamp nodes.

NOTE is a string for the note text."
  (->> (org-ml--log-get 'note)
       (org-ml--log-replace-timestamp unixtime nil t)
       (org-ml--build-log-item note)))

(defun org-ml-build-log-reschedule (unixtime old-timestamp &optional note)
  "Return an item node for a new schedule log entry.

This will format the log entry from the default value for the
'reschedule` cell in `org-log-note-headings'.

UNIXTIME is an integer representing the time to be used for all
timestamp nodes.

OLD-TIMESTAMP is a timestamp node of the schedule that is being
deleted. It will always be converted to an inactive timestamp.

If string NOTE is supplied, append a note to the log entry."
  (org-ml--build-log-item-trans 'reschedule unixtime old-timestamp note))

(defun org-ml-build-log-delschedule (unixtime old-timestamp &optional note)
  "Return an item node for a delete schedule log entry.

This will format the log entry from the default value for the
'delschedule` cell in `org-log-note-headings'.

UNIXTIME is an integer representing the time to be used for all
timestamp nodes.

OLD-TIMESTAMP is a timestamp node of the schedule that is being
deleted. It will always be converted to an inactive timestamp.

If string NOTE is supplied, append a note to the log entry."
  (org-ml--build-log-item-trans 'delschedule unixtime old-timestamp note))

(defun org-ml-build-log-redeadline (unixtime old-timestamp &optional note)
  "Return an item node for a new deadline log entry.

This will format the log entry from the default value for the
'redeadline` cell in `org-log-note-headings'.

UNIXTIME is an integer representing the time to be used for all
timestamp nodes.

OLD-TIMESTAMP is a timestamp node of the deadline that is being
deleted. It will always be converted to an inactive timestamp.

If string NOTE is supplied, append a note to the log entry."
  (org-ml--build-log-item-trans 'redeadline unixtime old-timestamp note))

(defun org-ml-build-log-deldeadline (unixtime old-timestamp &optional note)
  "Return an item node for a delete deadline log entry.

This will format the log entry from the default value for the
'deldeadline` cell in `org-log-note-headings'.

UNIXTIME is an integer representing the time to be used for all
timestamp nodes.

OLD-TIMESTAMP is a timestamp node of the deadline that is being
deleted. It will always be converted to an inactive timestamp.

If string NOTE is supplied, append a note to the log entry."
  (org-ml--build-log-item-trans 'deldeadline unixtime old-timestamp note))

(defun org-ml-build-log-refile (unixtime &optional note)
  "Return an item node for a refile log entry.
This will format the log entry from the default value for the
'deldeadline` cell in `org-log-note-headings'.

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
                  ((org-ml-is-type 'timestamp rep)
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

(defun org-ml-get-type (node)
  "Return the type of NODE."
  (org-element-type node))

(defun org-ml-is-type (type node)
  "Return t if the type of NODE is TYPE (a symbol)."
  (unless (memq type org-ml-nodes)
    (org-ml--arg-error "Argument 'type' must be in `org-ml-nodes': Got %s" type))
  (eq (org-ml-get-type node) type))

(defun org-ml-is-any-type (types node)
  "Return t if the type of NODE is in TYPES (a list of symbols)."
  (-some->>
   (-difference types org-ml-nodes)
   (org-ml--arg-error
    "All in 'types' must be in `org-ml-nodes'; these were not: %s"))
  (if (memq (org-ml-get-type node) types) t))

(defun org-ml-is-element (node)
  "Return t if NODE is an element class."
  (org-ml-is-any-type org-ml-elements node))

(defun org-ml-is-branch-node (node)
  "Return t if NODE is a branch node."
  (org-ml-is-any-type org-ml-branch-nodes node))

(defun org-ml-node-may-have-child-objects (node)
  "Return t if NODE is a branch node that may have child objects."
  (org-ml-is-any-type org-ml-branch-nodes-permitting-child-objects node))

(defun org-ml-node-may-have-child-elements (node)
  "Return t if NODE is a branch node that may have child elements.

Note this implies that NODE is also of class element since only
elements may have other elements as children."
  (org-ml-is-any-type org-ml-branch-elements-permitting-child-elements node))

;;; PUBLIC PROPERTY FUNCTIONS

;;; polymorphic

(defun org-ml-contains-point-p (point node)
  "Return t if POINT is within the boundaries of NODE."
  (-let (((&plist :begin :end) (org-ml-get-all-properties node)))
    (if (and (integerp begin) (integerp end))
        (<= begin point end)
      (error "Node boundaries are not defined"))))

(defun org-ml--property-is-attribute (prop)
  "Return t if PROP is of the form :attr_X where X is anything."
  (and (keywordp prop) (s-prefix-p ":attr_" (symbol-name prop) t)))

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
  (-if-let (pred (org-ml--get-property-attribute :pred type prop))
      (if (funcall pred value)
          (-if-let (encode-fun (org-ml--get-property-encoder type prop))
              (funcall encode-fun value)
            value)
        (org-ml--property-error-wrong-type prop type value))
    (org-ml--property-error-unsettable prop type)))

(defun org-ml-set-property (prop value node)
  "Return NODE with PROP set to VALUE.

See builder functions for a list of properties and their rules for
each type."
  (let ((type (org-ml-get-type node)))
    ;; Specialized code to handle :attr_X properties which can't be put in
    ;; `org-ml--property-alist'. Values for these can only be lists of strings
    ;; and have no encoder or decoder.
    (if (and (memq type org-ml--element-nodes-with-affiliated)
             (org-ml--property-is-attribute prop))
        (if (org-ml--is-string-list value)
            (org-ml--set-property-nocheck prop value node)
          (org-ml--arg-error "All attributes like '%s' must be a list of strings. Got '%S'"
           prop value))
      (let ((value* (org-ml--property-encode prop value type))
            (update-fun (org-ml--get-property-cis-function type prop)))
        (--> (org-ml--set-property-nocheck prop value* node)
             (if update-fun (funcall update-fun it) it))))))

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
               (list (alist-get nil it) (alist-get t it)))))
       (put
        (acc keyval)
        (plist-put acc (car keyval) (cadr keyval)))
       (put-encode
        (acc keyval type)
        (-let* (((prop value) keyval))
          (plist-put acc prop (org-ml--property-encode prop value type)))))
    (if (not (org-ml--is-plist plist))
        (org-ml--arg-error "Not a plist: %S" plist)
      (-let* ((type (org-ml-get-type node))
              (keyvals (-partition 2 plist))
              ;; this will divide the keywords to those that are of the form
              ;; :attr_X which must be set differently
              ((kv kv-attrs) (split-keyvals-maybe type keyvals))
              (update-funs
               (->> (--map (org-ml--get-property-cis-function type (car it)) kv)
                    (-uniq)
                    (-non-nil))))
        (--> (org-ml-get-all-properties node)
             (if kv (--reduce-from (put-encode acc it type) it kv) it)
             (if kv-attrs (-reduce-from #'put it kv-attrs) it)
             (org-ml--construct type it (org-ml-get-children node))
             (if update-funs (--reduce-from (funcall it acc) it update-funs) it))))))

(defun org-ml-get-property (prop node)
  "Return the value of PROP of NODE."
  (let* ((type (org-ml-get-type node))
         (decoder-fun (unless (and (memq type org-ml--element-nodes-with-affiliated)
                                   (org-ml--property-is-attribute prop))
                        (org-ml--get-property-decoder type prop)))
         (value (org-ml--get-property-nocheck prop node)))
    (if decoder-fun (funcall decoder-fun value) value)))

(defun org-ml-get-properties (props node)
  "Return all the values of PROPS from NODE.
PROPS is a list of all the properties desired, and the returned
list will be the values of these properties in the order
requested. To get the raw plist of NODE, use
`org-ml--get-all-properties'."
  (org-ml--map* (org-ml-get-property it node) props))

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
    (if (org-ml--get-property-attribute :toggle type prop)
        (org-ml-map-property prop #'not node)
      (org-ml--arg-error "Not a toggle-able property"))))

(defun org-ml-shift-property (prop n node)
  "Return NODE with PROP shifted by N (an integer).

This only applies the properties that are represented as integers."
  (let* ((type (org-ml-get-type node))
         (fun (org-ml--get-property-attribute :shift type prop)))
    (if fun
        (org-ml-map-property* prop (funcall fun n it) node)
      (org-ml--arg-error "Not a shiftable property"))))

(defun org-ml-insert-into-property (prop index string node)
  "Return NODE with STRING inserted at INDEX into PROP.

This only applies to properties that are represented as lists of
strings."
  (let ((type (org-ml-get-type node)))
    (if (org-ml--get-property-attribute :string-list type prop)
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
    (if (org-ml--get-property-attribute :string-list type prop)
        (org-ml-map-property* prop (-remove-item string it) node)
      (org-ml--arg-error "Property '%s' in node of type '%s' is not a string-list"
                     prop type))))

(defun org-ml-plist-put-property (prop key value node)
  "Return NODE with VALUE corresponding to KEY inserted into PROP.

KEY is a keyword and VALUE is a symbol. This only applies to
properties that are represented as plists."
  (if (org-ml--get-property-attribute :plist (org-ml-get-type node) prop)
      (org-ml-map-property* prop (plist-put it key value) node)
    (org-ml--arg-error "Not a plist property")))

(defun org-ml-plist-remove-property (prop key node)
  "Return NODE with KEY and its corresponding value removed from PROP.

KEY is a keyword. This only applies to properties that are
represented as plists.

See `org-ml-plist-put-property' for a list of supported elements
and properties that may be used with this function."
  (if (org-ml--get-property-attribute :plist (org-ml-get-type node) prop)
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
          (get-parents (cons node acc) (org-ml-get-property :parent node)))))
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
'read-only' property. This is why :parent will be set to nil when
building a new node with the 'org-ml-build-' family of functions
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
  ;; TODO this will check if node is s string twice
  (if (stringp node)
      (progn (remove-text-properties 0 (length node) '(:parent) node) node)
    (org-ml--set-property-nocheck-nil :parent node)))

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
    (if (and (org-ml-is-any-type org-ml--element-nodes-with-affiliated node)
             (org-ml--get-property-nocheck :caption node))
        (org-ml--map-property-nocheck* :caption
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
        (org-ml--map-property-nocheck* prop
          (remove-recursive it)
          node)))
    (->>
     ;; remove parents from secondary strings (if necessary)
     (pcase (org-ml-get-type node)
       (`headline (remove-within-prop :title node))
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
  (-if-let (index (-elem-index key (list :latex :latex-math-p :html
                                         :ascii :latin1 :utf-8)))
      (->> (org-ml-get-property :name entity)
           (org-entity-get)
           (cdr)
           (nth index))
    (org-ml--arg-error "Invalid encoding requested: %s" index)))

;; statistics-cookie

(defun org-ml-statistics-cookie-is-complete (statistics-cookie)
  "Return t is STATISTICS-COOKIE node is complete."
  (let ((val (org-ml--get-property-nocheck :value statistics-cookie)))
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
  (org-ml--timestamp-get-start-time timestamp))

(defun org-ml-timestamp-get-end-time (timestamp)
  "Return the end time list for end time of TIMESTAMP or nil if not a range.
The return value will be a list as specified by the TIME argument in
`org-ml-build-timestamp!'."
  (and (org-ml--timestamp-is-range-type timestamp)
       (org-ml--timestamp-get-end-time timestamp)))

;; TODO this should be ""-get-length
(defun org-ml-timestamp-get-range (timestamp)
  "Return the range of TIMESTAMP node in seconds as an integer.
If non-ranged, this function will return 0. If ranged but
the start time is in the future relative to end the time, return
a negative integer."
  (org-ml--timestamp-get-range timestamp))

(defun org-ml-timestamp-is-active (timestamp)
  "Return t if TIMESTAMP node is active."
  (or (org-ml--property-is-eq :type 'active timestamp)
      (org-ml--property-is-eq :type 'active-range timestamp)))

(defun org-ml-timestamp-is-ranged (timestamp)
  "Return t if TIMESTAMP node is ranged."
  (or (org-ml--property-is-eq :type 'active-range timestamp)
      (org-ml--property-is-eq :type 'inactive-range timestamp)))

(defun org-ml-timestamp-range-contains-p (unixtime timestamp)
  "Return t if UNIXTIME is between start and end time of TIMESTAMP node.
The boundaries are inclusive. If TIMESTAMP has a range of zero, then
only return t if UNIXTIME is the same as TIMESTAMP. TIMESTAMP will be
interpreted according to the localtime of the operating system."
  (let ((ut1 (org-ml--timestamp-get-start-unixtime timestamp))
        (ut2 (org-ml--timestamp-get-end-unixtime timestamp)))
    (<= ut1 unixtime ut2)))

(defun org-ml-timestamp-set-start-time (time timestamp)
  "Return TIMESTAMP node with start time set to TIME.
TIME is a list analogous to the same argument specified in
`org-ml-build-timestamp!'."
  (org-ml--timestamp-set-start-time time timestamp))

(defun org-ml-timestamp-set-end-time (time timestamp)
  "Return TIMESTAMP node with end time set to TIME.
TIME is a list analogous to the same argument specified in
`org-ml-build-timestamp!'."
  (org-ml--timestamp-set-end-time time timestamp))

(defun org-ml-timestamp-set-single-time (time timestamp)
  "Return TIMESTAMP node with start and end times set to TIME.
TIME is a list analogous to the same argument specified in
`org-ml-build-timestamp!'."
  (org-ml--timestamp-set-single-time time timestamp))

(defun org-ml-timestamp-set-double-time (time1 time2 timestamp)
  "Return TIMESTAMP node with start/end times set to TIME1/TIME2 respectively.
TIME1 and TIME2 are lists analogous to the TIME argument specified in
`org-ml-build-timestamp!'."
  (org-ml--timestamp-set-double-time time1 time2 timestamp))

;; TODO this should be ""-set-length
(defun org-ml-timestamp-set-range (range timestamp)
  "Return TIMESTAMP node with range set to RANGE.
If TIMESTAMP is ranged, keep start time the same and adjust the end
time. If not, make a new end time. The units for RANGE are in minutes
if TIMESTAMP is in long format and days if TIMESTAMP is in short
format."
  (org-ml--timestamp-set-range range timestamp))

(defun org-ml-timestamp-set-active (flag timestamp)
  "Return TIMESTAMP node with active type if FLAG is t."
  (org-ml--timestamp-set-active flag timestamp))

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
  (->> (org-ml--timestamp-shift-start n unit timestamp)
       (org-ml--timestamp-shift-end n unit)))

(defun org-ml-timestamp-shift-start (n unit timestamp)
  "Return TIMESTAMP node with start time shifted by N UNIT's.

N and UNIT behave the same as those in `org-ml-timestamp-shift'.

If TIMESTAMP is not range, the output will be a ranged timestamp with
the shifted start time and the end time as that of TIMESTAMP. If this
behavior is not desired, use `org-ml-timestamp-shift'."
  (org-ml--timestamp-shift-start n unit timestamp))

(defun org-ml-timestamp-shift-end (n unit timestamp)
  "Return TIMESTAMP node with end time shifted by N UNIT's.

N and UNIT behave the same as those in `org-ml-timestamp-shift'.

If TIMESTAMP is not range, the output will be a ranged timestamp with
the shifted end time and the start time as that of TIMESTAMP. If this
behavior is not desired, use `org-ml-timestamp-shift'."
  (org-ml--timestamp-shift-end n unit timestamp))

(defun org-ml-timestamp-toggle-active (timestamp)
  "Return TIMESTAMP node with its active/inactive type flipped."
  (-> (org-ml--timestamp-is-active timestamp)
      (not)
      (org-ml--timestamp-set-active timestamp)))

(defun org-ml-timestamp-truncate (timestamp)
  "Return TIMESTAMP node with start/end times forced to short format."
  (let ((t1 (->> (org-ml--timestamp-get-start-time timestamp)
                 (org-ml--time-truncate)))
        (t2 (->> (org-ml--timestamp-get-end-time timestamp)
                 (org-ml--time-truncate))))
    (org-ml--timestamp-set-double-time t1 t2 timestamp)))

(defun org-ml-timestamp-truncate-start (timestamp)
  "Return TIMESTAMP node with start time forced to short format."
  (let ((time (->> (org-ml--timestamp-get-start-time timestamp)
                   (org-ml--time-truncate))))
    (org-ml--timestamp-set-start-time time timestamp)))

(defun org-ml-timestamp-truncate-end (timestamp)
  "Return TIMESTAMP node with end time forced to short format."
  (let ((time (->> (org-ml--timestamp-get-end-time timestamp)
                   (org-ml--time-truncate))))
    (org-ml--timestamp-set-end-time time timestamp)))

(defun org-ml-timestamp-set-collapsed (flag timestamp)
  "Return TIMESTAMP with collapsed set to FLAG.

If timestamp is ranged but not outside of one day, it may be collapsed
\(FLAG is t) to short format like [yyyy-mm-dd xxx hh:mm-hh:mm] or
expanded (FLAG is nil) to long format like [yyyy-mm-dd xxx
hh:mm]--[yyyy-mm-dd xxx hh:mm]. If these conditions are not met,
return TIMESTAMP untouched regardless of FLAG.

Note: the default for all timestamp functions in `om.el' is to favor
collapsed format."
  (if (and (not (org-ml--timestamp-is-ranged-lowres timestamp))
           (org-ml--timestamp-is-ranged timestamp))
      (org-ml--timestamp-set-type-ranged (not flag) timestamp)
    timestamp))

(defun org-ml-timestamp-get-warning (timestamp)
  "Return the warning component of TIMESTAMP.
Return a list like (TYPE VALUE UNIT)."
  (-let (((&plist :warning-type y
                  :warning-value v
                  :warning-unit u)
          (org-ml-get-all-properties timestamp)))
    `(,y ,v ,u)))

(defun org-ml-timestamp-set-warning (warning timestamp)
  "Set the warning of TIMESTAMP to WARNING.

WARNING is a list like (TYPE VALUE UNIT). TYPE is one of 'year',
'month', 'week', or 'day'. VALUE and is an integer. UNIT is one
of 'year', 'month', 'week', or 'day'."
  (org-ml--timestamp-set-warning warning timestamp))

(org-ml--defun-anaphoric* org-ml-timestamp-map-warning (fun timestamp)
  "Apply FUN to the warning of TIMESTAMP.
FUN is a function that takes a warning list like and returns a
new warning list. The same rules that apply to
`org-ml-timestamp-set-warning' and `org-ml-timestamp-get-warning'
apply here."
  (--> (org-ml-timestamp-get-warning timestamp)
    (org-ml-timestamp-set-warning (funcall fun it) timestamp)))

(defun org-ml-timestamp-get-repeater (timestamp)
  "Return the repeater component of TIMESTAMP.
Return a list like (TYPE VALUE UNIT). If `org-ml-parse-habits' is
t, return a list like (TYPE VALUE UNIT HABIT-VALUE HABIT-UNIT)."
  (-let* (((&plist :repeater-type y
                   :repeater-value v
                   :repeater-unit u
                   :raw-value r)
           (org-ml-get-all-properties timestamp))
          (rep `(,y ,v ,u)))
    (if (not org-ml-parse-habits) rep
      (-let* (((v u) (-some->> r
                       (s-match "+[0-9]+[ymwd]/\\([0-9]+\\)\\([ymwd]\\)")
                       (cdr)))
              (v* (-some-> v (string-to-number)))
              (u* (-some--> u
                    (pcase it
                      ("y" 'year)
                      ("m" 'month)
                      ("w" 'week)
                      ("d" 'day)
                      (_ (error "Unknown unit (this shouldn't happen)"))))))
            `(,@rep ,v* ,u*)))))

(defun org-ml--timestamp-unit-to-string (unit)
  (pcase unit
    (`year "y")
    (`month "m")
    (`week "w")
    (`day "d")
    (e (error "Invalid unit: %s" e))))

(defun org-ml-timestamp-set-repeater (repeater timestamp)
  "Set the repeater of TIMESTAMP to REPEATER.

REPEATER is a list like (TYPE VALUE UNIT) or (TYPE VALUE UNIT
HABIT-VALUE HABIT-UNIT); if `org-ml-parse-habits' is nil, only
accept the former and error on the latter (and vice versa). TYPE
is one of 'year', 'month', 'week', or 'day'. VALUE and
HABIT-VALUE are integers. UNIT and HABIT-UNIT are one of 'year',
'month', 'week', or 'day'.

In order to remove the repeater entirely, set REPEATER to nil
or (nil nil nil). To delete just the habit (if it exists and
`org-ml-parse-habits' is t) set REPEATER to (TYPE VALUE UNIT nil
nil)."
  (pcase repeater
    (`nil
     (->> (if org-ml-parse-habits
              (org-ml--map-property-nocheck* :raw-value
                (replace-regexp-in-string " [.+]?+[0-9]+[ymwd]\\(/[0-9]+[ymwd]\\)?" "" it)
                timestamp)
            timestamp)
          (org-ml--timestamp-set-repeater nil)))
    (`(,_ ,_ ,_)
     (if org-ml-parse-habits
         (error "Habit parsing is enabled; use a 5-membered list")
       (org-ml--timestamp-set-repeater repeater timestamp)))
    (`(,rt ,rv ,ru ,hv ,hu)
     (if (not org-ml-parse-habits)
         (error "Habit parsing is disabled; use a 3-membered list")
       (->> (org-ml--timestamp-set-repeater (list rt rv ru) timestamp)
            (org-ml--map-property-nocheck* :raw-value
              (let ((r (if (not (and hv hu)) ""
                         (->> (org-ml--timestamp-unit-to-string hu)
                              (format "/%s%s" hv)))))
                (save-match-data
                  (-if-let (m (string-match "\\(+[0-9]+[ymwd]\\)\\(/[0-9]+[ymwd]\\)?" it))
                      (-let (((b1 e1 b2 e2) (-if-let (b (match-beginning 2))
                                                `(0 ,b ,(match-end 2) nil)
                                              (let ((i (match-end 1)))
                                                `(0 ,i ,i -1)))))
                        (concat (substring it b1 e1) r (substring it b2 e2)))
                    (if (and rt rv ru)
                        (let* ((y* (pcase rt
                                     (`cumulate "+")
                                     (`catch-up "++")
                                     (`restart ".+")
                                     (e (error "Unknown repeater type: %s" e))))
                               (rep (->> (org-ml--timestamp-unit-to-string ru)
                                         (format " %s%s%s" y* rv))))
                          (concat (substring it 0 -1) rep r (substring it -1)))
                      it))))))))
    (e (error "Invalid repeater definition: %s" e))))

(org-ml--defun-anaphoric* org-ml-timestamp-map-repeater (fun timestamp)
  "Apply FUN to the warning of TIMESTAMP.
FUN is a function that takes a repeater list like and returns a
new repeater list. The same rules that apply to
`org-ml-timestamp-set-repeater' and
`org-ml-timestamp-get-repeater' apply here."
  (--> (org-ml-timestamp-get-repeater timestamp)
    (org-ml-timestamp-set-repeater (funcall fun it) timestamp)))

;; timestamp (diary)

(defun org-ml-timestamp-diary-set-value (form timestamp-diary)
  "Return TIMESTAMP-DIARY node with value set to FORM.
The node must have a type `eq' to `diary'. FORM is a quoted list."
  (if (listp form)
      (org-ml--set-property-nocheck :raw-value (format "<%%%%%S>" form)
                                    timestamp-diary)
    (org-ml--arg-error "Timestamp-diary node value must be a form: Got %S" form)))

;;; element nodes
;;
;; clock

(defun org-ml-clock-is-running (clock)
  "Return t if CLOCK element is running (eg is open)."
  (org-ml--property-is-eq :status 'running clock))

;; headline

(defun org-ml-headline-get-statistics-cookie (headline)
  "Return the statistics cookie node from HEADLINE if it exists."
  (->> (org-ml--get-property-nocheck :title headline)
       (-last-item)
       (org-ml--filter-type 'statistics-cookie)))

(defun org-ml-headline-is-done (headline)
  "Return t if HEADLINE node has a done todo-keyword."
  (-> (org-ml--get-property-nocheck :todo-keyword headline)
      (member org-done-keywords)
      (and t)))

(defun org-ml-headline-has-tag (tag headline)
  "Return t if HEADLINE node is tagged with TAG."
  (if (member tag (org-ml--get-property-nocheck :tags headline)) t))

(defun org-ml-headline-set-title! (title-text stats-cookie-value headline)
  "Return HEADLINE node with new title.

TITLE-TEXT is a string to be parsed into object nodes for the title
via `org-ml-build-secondary-string!' (see that function for restrictions)
and STATS-COOKIE-VALUE is a list described in
`org-ml-build-statistics-cookie'."
  (let ((ss (org-ml-build-secondary-string! title-text)))
    (if (not stats-cookie-value)
        (org-ml-set-property :title ss headline)
      (let ((ss* (org-ml--map-last*
                  (org-ml--set-property-nocheck :post-blank 1 it) ss))
            (sc (org-ml-build-statistics-cookie stats-cookie-value)))
        (org-ml-set-property :title (-snoc ss* sc) headline)))))

;; item

(defun org-ml-item-toggle-checkbox (item)
  "Return ITEM node with its checkbox state flipped.
This only affects item nodes with checkboxes in the `on' or `off'
states; return ITEM node unchanged if the checkbox property is `trans'
or nil."
  (pcase (org-ml-get-property :checkbox item)
    ('on (org-ml-set-property :checkbox 'off item))
    ('off (org-ml-set-property :checkbox 'on item))
    ((or `trans `nil) item)
    (_ (error "This should not happen"))))

;; planning

(defun org-ml-planning-set-timestamp! (prop planning-list planning)
  "Return PLANNING node with PROP set to PLANNING-LIST.

PROP is one of `:closed', `:deadline', or `:scheduled'. PLANNING-LIST
is the same as that described in `org-ml-build-planning!'."
  (unless (memq prop '(:closed :deadline :scheduled))
    (org-ml--arg-error "PROP must be ':closed', ':deadline', or ':scheduled'. Got %S" prop))
  (let* ((active (if (eq prop :closed) nil t))
         (ts (org-ml--planning-list-to-timestamp active planning-list)))
    (org-ml-set-property prop ts planning)))

;;; PUBLIC BRANCH/CHILD FUNCTIONS

;;; polymorphic

(defun org-ml-children-contain-point (point branch-node)
  "Return t if POINT is within the boundaries of BRANCH-NODE's children."
  (-let (((&plist :contents-begin :contents-end)
          (org-ml-get-all-properties branch-node)))
    (if (and (integerp contents-begin) (integerp contents-end))
        (<= contents-begin point contents-end)
      (error "Node boundaries are not defined"))))

(defun org-ml-get-children (branch-node)
  "Return the children of BRANCH-NODE as a list."
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
          (if (and (org-ml-is-type 'plain-text last)
                   (org-ml-is-type 'plain-text node))
              (cons (concat last node) (cdr acc))
            (cons node acc)))))
    (reverse (-reduce-from #'concat-maybe nil secondary-string))))

(eval-when-compile
  (defmacro org-ml--mapcat-normalize (form secondary-string)
    "Return mapped, concatenated, and normalized SECONDARY-STRING.
FORM is a form supplied to `--mapcat'."
    (declare (debug (def-form form)))
    `(->> (org-ml--map* ,form ,secondary-string)
          (apply #'append)
          (org-ml--normalize-secondary-string))))

(defun org-ml-unwrap (object-node)
  "Return the children of OBJECT-NODE as a secondary string.
If OBJECT-NODE is a plain-text node, wrap it in a list and return.
Else add the post-blank property of OBJECT-NODE to the last member
of its children and return children as a secondary string."
  (if (org-ml-is-type 'plain-text object-node)
      (list object-node)
    (let ((post-blank (org-ml--get-property-nocheck :post-blank object-node)))
      (org-ml--map-last* (org-ml-map-property* :post-blank (+ it post-blank) it)
        (org-ml-get-children object-node)))))

(defun org-ml-unwrap-types-deep (types object-node)
  "Return the children of OBJECT-NODE as a secondary string.
If OBJECT-NODE is a plain-text node, wrap it in a list and return.
Else recursively descend into the children of OBJECT-NODE and splice
the children of nodes with type in TYPES in place of said node and
return the result as a secondary string."
  (cond
   ((org-ml-is-type 'plain-text object-node)
    (list object-node))
   ((org-ml-is-any-type types object-node)
    (let ((post-blank (org-ml--get-property-nocheck :post-blank object-node)))
      (->> (org-ml-get-children object-node)
           (org-ml--mapcat-normalize (org-ml-unwrap-types-deep types it))
           (org-ml--map-last* (org-ml-map-property* :post-blank
                            (+ it post-blank) it)))))
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
    (if (and (org-ml-is-type 'plain-list last)
             (org-ml-is-type 'plain-list first))
        (let ((pb (org-ml-get-property :post-blank last)))
          (--> (org-ml-get-children last)
               (org-ml--map-last* (org-ml-set-property :post-blank pb it) it)
               (append it (org-ml-get-children first))
               (org-ml--set-children-nocheck it last)
               (cons it (cdr nodes2))
               (append (-drop-last 1 nodes1) it)))
      (append nodes1 nodes2))))

(defun org-ml-item-get-paragraph (item)
  "Return the first paragraph's children of ITEM or nil if none."
  (-when-let (first-child (car (org-ml-get-children item)))
    (when (org-ml-is-type 'paragraph first-child)
      (org-ml-get-children first-child))))

(defun org-ml-item-set-paragraph (secondary-string item)
  "Set the first paragraph's children of ITEM to SECONDARY-STRING."
  (org-ml-map-children*
    (if (org-ml-is-type 'paragraph (car it))
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

;;; headline (metadata)

;; planning

(defun org-ml-headline-get-planning (headline)
  "Return the planning node in HEADLINE or nil if none."
  (-some--> (car (org-ml-headline-get-section headline))
    (when (org-ml-is-type 'planning it) it)))

(defun org-ml-headline-set-planning (planning headline)
  "Return HEADLINE node with planning components set to PLANNING node."
  (-let* ((children (org-ml-headline-get-section headline))
          (first-type (org-ml-get-type (car children))))
    (cond
     ((and planning (eq first-type 'planning))
      (org-ml-headline-set-section (cons planning (cdr children)) headline))
     (planning
      (let ((pb (org-ml-get-property :pre-blank headline)))
        (--> (org-ml-map-property* :post-blank (+ pb it) planning)
             (org-ml-headline-set-section (cons it children) headline)
             (org-ml-set-property :pre-blank 0 it))))
     ((eq first-type 'planning)
      (--> (org-ml-get-property :post-blank (car children))
           (org-ml-set-property :pre-blank it headline)
           (org-ml-headline-set-section (cdr children) it)))
     (t
      headline))))

(org-ml--defun-anaphoric* org-ml-headline-map-planning (fun headline)
  "Return HEADLINE node with planning node modified by FUN.

FUN is a unary function that takes a planning node and returns a
modified planning node."
   (--> (org-ml-headline-get-planning headline)
        (org-ml-headline-set-planning (funcall fun it) headline)))

;; node-properties (eg the entire property drawer)

(defun org-ml-headline-get-node-properties (headline)
  "Return a list of node-properties nodes in HEADLINE or nil if none."
  ;; assume the property drawer is the first or second child of section
  (-some--> (org-ml-headline-get-section headline)
    (if (org-ml-is-type 'property-drawer (car it)) (car it)
      (when (org-ml-is-type 'property-drawer (cadr it)) (cadr it)))
    (org-ml-get-children it)))

(defun org-ml-headline-set-node-properties (node-properties headline)
  "Return HEADLINE node with property drawer containing NODE-PROPERTIES.
NODE-PROPERTIES is a list of node-property nodes."
  (-let* ((children (org-ml-headline-get-section headline))
          ((first . r1) children)
          ((second . r2) r1)
          (t1 (org-ml-get-type first))
          (t2 (org-ml-get-type second)))
    (cond
     ((and node-properties (eq t1 'planning) (eq t2 'property-drawer))
      (--> (org-ml-set-children node-properties second)
           (org-ml-headline-set-section `(,first ,it ,@r2) headline)))
     ((and node-properties (eq t1 'property-drawer))
      (--> (org-ml-set-children node-properties first)
           (org-ml-headline-set-section (cons it r1) headline)))
     ((and node-properties (eq t1 'planning))
      (--> (org-ml-get-property :post-blank first)
           (apply #'org-ml-build-property-drawer :post-blank it node-properties)
           `(,(org-ml-set-property :post-blank 0 first) ,it ,@r1)
           (org-ml-headline-set-section it headline)))
     (node-properties
      (--> (org-ml-get-property :pre-blank headline)
           (apply #'org-ml-build-property-drawer :post-blank it node-properties)
           (org-ml-headline-set-section (cons it children) headline)
           (org-ml-set-property :pre-blank 0 it)))
     ((eq t2 'property-drawer)
      (let ((pb (org-ml-get-property :post-blank second)))
        (--> (org-ml-map-property* :post-blank (+ pb it) first)
             (org-ml-headline-set-section (cons it r2) headline))))
     ((eq t1 'property-drawer)
      (let ((pb (org-ml-get-property :post-blank first)))
        (--> (org-ml-map-property* :pre-blank (+ pb it) headline)
             (org-ml-headline-set-section r1 it))))
     (t
      headline))))

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
  (-some->> (org-ml-headline-get-node-properties headline)
    (--first (equal key (org-ml-get-property :key it)))
    (org-ml-get-property :value)))

(defun org-ml-headline-set-node-property (key value headline)
  "Return HEADLINE with node property matching KEY set to VALUE.
If a property matching KEY is present, set it to VALUE. If multiple
properties matching KEY are present, only set the first."
  (org-ml-headline-map-node-properties*
    (-if-let (np (-some->> value (org-ml-build-node-property key)))
        (-if-let (i (--find-index (equal key (org-ml-get-property :key it)) it))
            (-replace-at i np it)
          (cons np it))
      (--remove-first (equal key (org-ml-get-property :key it)) it))
    headline))

(org-ml--defun-anaphoric* org-ml-headline-map-node-property (key fun headline)
  "Return HEADLINE node with property value matching KEY modified by FUN.

FUN is a unary function that takes a node-property value and returns
a modified node-property value."
   (--> (org-ml-headline-get-node-property key headline)
        (org-ml-headline-set-node-property key (funcall fun it) headline)))

;;; headline (logbook/contents)

;; Everything after the planning and property drawer of the headline can be
;; either part of the "logbook" or the "contents". The "logbook" contains two
;; types of nodes, here called "log items" (or sometimes simply "items" if the
;; context is obvious) and "clocks." The former include any plain-list/item node
;; as given by `org-log-note-headings' (except for 'clock-out' which applies
;; only to clocks), and clocks includes clock nodes and optionally
;; plain-list/item nodes that represent the clock-out notes. Anything that comes
;; after the logbook is deemed "contents." Together, the "logbook" and
;; "contents" are hereafter referred collectively as the "supercontents."

;; There are many ways to configure the logbook, and these are controlled by
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
;; - any clock not are defined as any item that is not a log item but after a
;;   clock
;;
;; The first node that breaks any of the above conditions will be the dividing
;; line between the logbook and contents. Note that the one loophole this
;; creates is that it is theoretically possible (but unlikely) that an item
;; immediately after a clock could be interpreted as a clock note even if it was
;; not intended as one.

;; Since there are many possible configurations for the logbook and consequently
;; many details involved in determining what is "logbook" and what is
;; "contents," there are several specialized data structures used for this
;; process. The logbook itself is represented by an alist consisting of the
;; items, clocks, and unknown nodes from the logbook. This alist is part of a
;; larger list called the "supercontents" which consists of two keys for the
;; logbook and contents. The config for the logbook will be representing on the
;; user side by a plist which corresponds to the possible configuration
;; variables noted above; internally it will be 'encoded' to a more
;; readily-parsable alist.

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

;; logbook data structure

;; alist to store the separated nodes from a logbook

(defun org-ml--logbook-init (items clocks unknown post-blank)
  "Create a new logbook alist.
ITEMS, CLOCKS, and UNKNOWN correspond to a list of item nodes,
clock notes (which may contain item nodes for notes) and other
nodes. POST-BLANK corresponds to the number of extra newlines
between the logbook and the contents."
  `((:items ,@items)
    (:clocks ,@clocks)
    (:unknown ,@unknown)
    (:post-blank . ,(or post-blank 0))))

(defun org-ml-logbook-get-items (logbook)
  "Return the :items slot from LOGBOOK."
  (alist-get :items logbook))

(defun org-ml-logbook-get-clocks (logbook)
  "Return the :clocks slot from LOGBOOK."
  (alist-get :clocks logbook))

(defun org-ml-logbook-get-post-blank (logbook)
  "Return the :clocks slot from LOGBOOK."
  (alist-get :post-blank logbook))

(defun org-ml-logbook-set-items (items logbook)
  "Set the :items slot in LOGBOOK to ITEMS."
  (-let (((&alist :clocks :unknown :post-blank) logbook))
    (org-ml--logbook-init items clocks unknown post-blank)))

(defun org-ml-logbook-set-clocks (clocks logbook)
  "Set the :clocks slot in LOGBOOK to CLOCKS."
  (-let (((&alist :items :unknown :post-blank) logbook))
    (org-ml--logbook-init items clocks unknown post-blank)))

(defun org-ml-logbook-set-post-blank (post-blank logbook)
  "Set the :post-blank slot in LOGBOOK to POST-BLANK."
  (-let (((&alist :items :clocks :unknown) logbook))
    (org-ml--logbook-init items clocks unknown post-blank)))

(org-ml--defun-anaphoric* org-ml-logbook-map-items (fun logbook)
  "Apply function to :item slot in LOGBOOK.
FUN is a unary function that takes a list of items and returns a
new list of items."
  (--> (alist-get :items logbook)
       (org-ml-logbook-set-items (funcall fun it) logbook)))

(org-ml--defun-anaphoric* org-ml-logbook-map-clocks (fun logbook)
  "Apply function to :clocks slot in LOGBOOK.
FUN is a unary function that takes a list of clocks and returns a
new list of clocks."
  (--> (alist-get :clocks logbook)
       (org-ml-logbook-set-clocks (funcall fun it) logbook)))

;; supercontents data structure

;; alist to store the separated logbook and contents from a headline section

;; NOTE: this is a structure that the user may interact with, so some of these
;; functions are public

(defun org-ml--supercontents-init-from-lb (logbook contents)
  "Create a supercontents alist.
LOGBOOK is a logbook as given by `org-ml--logbook-init' and
CONTENTS is a list of nodes corresponding to the headline
contents (the stuff after the logbook)."
  `((:logbook ,@logbook) (:contents ,@contents)))

(defun org-ml--supercontents-init (items clocks unknown post-blank contents)
  "Create a supercontents alist.
ITEMS, CLOCKS, UNKNOWN, and POST-BLANK are lists corresponding to
the arguments in `org-ml--logbook-init' and CONTENTS has the same
meaning as `org-ml--supercontents-init-from-lb'."
  (let ((lb (org-ml--logbook-init items clocks unknown post-blank)))
    (org-ml--supercontents-init-from-lb lb contents)))

(defun org-ml-supercontents-get-contents (supercontents)
  "Return the :contents slot of SUPERCONTENTS."
  (alist-get :contents supercontents))

(defun org-ml-supercontents-set-contents (contents supercontents)
  "Set the :contents slot of SUPERCONTENTS to CONTENTS."
  (-let (((&alist :logbook) supercontents))
    (org-ml--supercontents-init-from-lb logbook contents)))

(org-ml--defun-anaphoric* org-ml-supercontents-map-contents (fun supercontents)
  "Apply function to :contents slot in SUPERCONTENTS.
FUN is a unary function that takes a list of nodes and returns a
new list of nodes."
  (--> (org-ml-supercontents-get-contents supercontents)
       (org-ml-supercontents-set-contents (funcall fun it) supercontents)))

(defun org-ml-supercontents-get-logbook (supercontents)
  "Return the :logbook slot of SUPERCONTENTS."
  (alist-get :logbook supercontents))

(defun org-ml-supercontents-set-logbook (logbook supercontents)
  "Set the :logbook slot of SUPERCONTENTS to LOGBOOK."
  (-let (((&alist :contents) supercontents))
    (org-ml--supercontents-init-from-lb logbook contents)))

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
;; possible configurations are possible:
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
        (when (and (org-ml-is-type 'timestamp node)
                   (org-ml--property-is-eq :type 'inactive node)
                   (-some->> (org-ml--timestamp-get-start-time node)
                     (org-ml-time-is-long)))
          (org-ml--timestamp-get-start-unixtime node)))
       (is-line-break
        (node)
        (or (org-ml-is-type 'line-break node)
            (and (org-ml-is-type 'plain-text node)
                 (equal "\n" node))))
       (get-paragraph-children
        (item)
        (-when-let (first-child (car (org-ml-get-children item)))
          (when (org-ml-is-type 'paragraph first-child)
            (org-ml-get-children first-child)))))
    (when (org-ml-is-type 'item item)
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

(defun org-ml--scc-get-clock-notes (scc)
  "Return the :clock-notes slot from SCC."
  (alist-get :clock-notes scc))

(defun org-ml--scc-get-log-item-fun (scc)
  "Return the :is-log-item-fun slot from SCC."
  (alist-get :is-log-item-fun scc))

;; logbook separation (nodes -> supercontents)

(defun org-ml--node-has-trailing-space (node)
  "Return t if NODE has at least one newline after it."
  (and (< 0 (org-ml-get-property :post-blank node)) t))

(defun org-ml--node-is-drawer-with-name (drawer-name node)
  "Return t if NODE is a drawer with DRAWER-NAME."
  (and (org-ml-is-type 'drawer node)
       (equal drawer-name (org-ml-get-property :drawer-name node))))

(defun org-ml--flatten-plain-lists (nodes)
  "Return NODES with unwrapped plain-list nodes.
\"Unwrapping\" means replacing the plain-list with its top-level
items."
  (cl-flet
      ((flatten
        (plain-list)
        (let ((pb (org-ml-get-property :post-blank plain-list)))
          (->> (org-ml-get-children plain-list)
               (org-ml--map-last* (org-ml-set-property :post-blank pb it))))))
    (--splice (org-ml-is-type 'plain-list it) (flatten it) nodes)))

(defun org-ml--wrap-plain-lists (nodes)
  "Return NODES with all subsequent items wrapped as plain-lists.
This is the dual of `org-ml--flatten-plain-lists'."
  (cl-flet
      ((wrap
        (acc node)
        (cond
         ((and (org-ml-is-type 'item node)
               (org-ml-is-type 'plain-list (car acc)))
          (cons (org-ml-map-children* (cons node it) (car acc))
                (cdr acc)))
         ((org-ml-is-type 'item node)
          (let* ((pb (org-ml-get-property :post-blank node))
                 (pl (->> (org-ml-set-property :post-blank 0 node)
                          (org-ml-build-plain-list :post-blank pb))))
            (cons pl acc)))
         (t
          (cons node acc)))))
    (-reduce-from #'wrap nil (reverse nodes))))

(defun org-ml--separate-logbook (scc mode nodes)
  "Separate NODES into logbook components.
SCC is the supercontents-config as given by `org-ml--scc-encode'.
MODE is the mode by which to separate the nodes and is one of
'mixed' (items and clocks are mixed together), 'clocks', or
'items'. The returned list will be like (ITEMS CLOCKS UNKNOWN)."
  (let ((n (org-ml--scc-get-clock-notes scc))
        (f (org-ml--scc-get-log-item-fun scc)))
    (cl-flet
        ((split
          (acc node)
          (if (not node) acc
            (cond
             ((and (org-ml-is-type 'clock node)
                   (memq mode '(:mixed :clocks)))
              (cons (cons 'clocks node) acc))
             ((and (org-ml-is-type 'item node)
                   (memq mode '(:items :mixed))
                   (funcall f node))
              (cons (cons 'items node) acc))
             ((and (org-ml-is-type 'item node)
                   (memq mode '(:clocks :mixed))
                   n
                   (not (funcall f node))
                   (org-ml-is-type 'clock (cdr (car acc))))
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
    (when (and (org-ml-is-type 'item node) (funcall f node))
      (list (org-ml--state-tick :item state) (list (cons 'items node))))))

(defun org-ml--clock-note-get-next-state (scc state node)
  "Return updated state for NODE if it is a valid clock note.
SCC is given by `org-ml--scc-encode' and STATE is given by
`org-ml--state-init'. STATE will be updated by called
`org-ml--state-tick'."
  (-let ((f (org-ml--scc-get-log-item-fun scc)))
    (when (and (org-ml-is-type 'item node) (not (funcall f node)))
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
  (when (org-ml-is-type 'clock node)
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
    (let ((drawer-nodes (->> (org-ml-get-children node)
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
logbook is separated from the contents, and a 'stateful' iterator
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
  "Sort of like `--reduce` but with state.
It is only 'sort of' like a standard reduce function for two
reasons a) it keeps track of state and b) returns the remainder
of LIST when the state is nil (which signifies termination of the
loop). FORM is a form where `it' is bound to the current node,
`acc' is bound to the accumulated nodes, and `it-state' is bound
to the current state. FORM must return a list like
\(NEW-STATE NEW-ACC), where NEW-STATE and NEW-ACC become the state
and accumulator on the next iteration. INITIAL-STATE is bound to
`it-state' on the first iteration."
  (declare (indent 1))
  `(let ((it-state ,initial-state)
         (rest ,list)
         acc it)
     (while (and it-state rest)
       (setq it (car rest))
       (-setq (it-state acc) ,form)
       (when it-state
         (setq rest (cdr rest))))
     (list acc rest)))

(defun org-ml--supercontents-from-nodes (config nodes)
  "Return a supercontents object based on NODES.
CONFIG is a plist parsable by `org-ml--scc-encode'."
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
            (first-space-post-blank (-some->>
                                        (-last-item nodes-before-space)
                                      (org-ml-get-property :post-blank)))
            ((logbook-nodes contents-nodes-before-space)
             (org-ml--reduce-state init-state
               (-let (((next-state logbook-nodes) (try-test-funs scc it-state it)))
                 (if logbook-nodes
                     (list next-state (append logbook-nodes acc))
                   (list nil acc)))
               nodes-before-space))
            ((&alist 'items 'clocks 'unknown) (->> (reverse logbook-nodes)
                                                   (-group-by #'car)))
            (post-blank (when logbook-nodes
                          (if contents-nodes-before-space 0 first-space-post-blank)))
            (contents (->> nodes-after-space
                           (append contents-nodes-before-space)
                           (org-ml--wrap-plain-lists))))
      (org-ml--supercontents-init (map-cdr items)
                                  (map-cdr clocks)
                                  (map-cdr unknown)
                                  post-blank contents))))

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
             (-some->> (org-ml-get-property :value node)
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
                 (org-ml-is-type 'item node)
                 (org-ml-is-type 'clock (car (car acc)))
                 (not (funcall f node)))
            (cons (list (car (car acc)) node) (cdr acc)))
           ((or (and (memq mode '(:items :mixed))
                     (org-ml-is-type 'item node)
                     (funcall f node))
                (and (memq mode '(:clocks :mixed))
                     (org-ml-is-type 'clock node)))
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
         (merge
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
                (merge (merge-and-sort left) (merge-and-sort right)))))))
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
        (limit logbook)
        (->> (org-ml-logbook-get-clocks logbook)
             (--count (org-ml-is-type 'clock it))
             (>= limit)))
       (merge
        (enconf logbook)
        (-let (((&alist :items :clocks) logbook))
          (org-ml--merge-logbook enconf items clocks)))
       (build-mixed-drawer-maybe
        (enconf m logbook)
        (-some->> (merge enconf logbook)
          (build-drawer m)
          (list)))
       (to-item-clock-nodes
        (enconf logbook)
        (list (org-ml--logbook-items-to-nodes enconf logbook)
              (org-ml--logbook-clocks-to-nodes enconf logbook)))
       (to-nodes
        (config logbook)
        (-let (((enconf &as &alist :drawers d) (org-ml--scc-encode config)))
          (pcase d

           ;; items not in drawer, clocks not in drawer
           (`(:items nil :clocks nil :mixed nil :clock-limit nil)
            (merge enconf logbook))

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
            (if (below-limit l logbook) (merge enconf logbook)
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

           (e (error "This shouldn't happen: %s" e))))))
    (let ((pb (org-ml-logbook-get-post-blank logbook)))
      (->> (to-nodes config logbook)
           (org-ml--map-last* (org-ml-set-property :post-blank pb it))))))

(defun org-ml--supercontents-to-nodes (config supercontents)
  "Return SUPERCONTENTS as a list of nodes.
The exact configuration of the returned nodes will depend on
CONFIG. POST-BLANK is the blank space to put between the logbook
and the contents."
  (let* ((logbook (->> (org-ml-supercontents-get-logbook supercontents)
                       (org-ml--logbook-to-nodes config)))
         (contents (org-ml-supercontents-get-contents supercontents)))
    (org-ml--append-join-plain-lists logbook contents)))

;; public supercontents functions

(defun org-ml-headline-get-supercontents (config headline)
  "Return the supercontents of HEADLINE node.

Supercontents will be like ((:logbook LB) (:contents CONTENTS))
where LB is another alist representing the logbook, and CONTENTS
is everything under the headline after the logbook and before the
first subheadline (if present).

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
  (cl-flet
      ((drop-if-type
        (type children)
        (if (org-ml-is-type type (car children)) (cdr children) children)))
    (->> (org-ml-headline-get-section headline)
         (drop-if-type 'planning)
         (drop-if-type 'property-drawer)
         (org-ml--supercontents-from-nodes config))))

(defun org-ml-headline-set-supercontents (config supercontents headline)
  "Set logbook and contents of HEADLINE according to SUPERCONTENTS.
See `org-ml-headline-get-supercontents' for the meaning of CONFIG
and the structure of the SUPERCONTENTS list."
  (cl-flet
      ((set-blank
        (new-logbook? config headline prop node)
        (if new-logbook? (org-ml-set-property prop 0 node)
          (-if-let (pb (-some->> headline
                         (org-ml-headline-get-supercontents config)
                         (org-ml-supercontents-get-logbook)
                         (org-ml-logbook-get-post-blank)))
              (org-ml-set-property prop pb node)
            node))))
    (-let* (((first . (second . _)) (org-ml-headline-get-section headline))
            (t1 (org-ml-get-type first))
            (t2 (org-ml-get-type second))
            (nodes (org-ml--supercontents-to-nodes config supercontents))
            (new-logbook? (--> (org-ml-supercontents-get-logbook supercontents)
                               (or (org-ml-logbook-get-items it)
                                   (org-ml-logbook-get-clocks it)))))
      (cond
       ((and (eq t1 'planning) (eq t2 'property-drawer))
        (--> (set-blank new-logbook? config headline :post-blank second)
             `(,first ,it ,@nodes)
             (org-ml-headline-set-section it headline)))
       ((or (eq t1 'planning) (eq t1 'property-drawer))
        (--> (set-blank new-logbook? config headline :post-blank first)
             (cons it nodes)
             (org-ml-headline-set-section it headline)))
       (t
        (->> (set-blank new-logbook? config headline :pre-blank headline)
             (org-ml-headline-set-section nodes)))))))

(org-ml--defun-anaphoric* org-ml-headline-map-supercontents (config fun headline)
  "Map a function over the supercontents of HEADLINE.
FUN is a unary function that takes a supercontents list and
returns a modified supercontents list. See
`org-ml-headline-get-supercontents' for the meaning of CONFIG and
the structure of the supercontents list."
  (--> (org-ml-headline-get-supercontents config headline)
       (org-ml-headline-set-supercontents config (funcall fun it) headline)))

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
  (let ((clock (-> (org-ml-unixtime-to-time-long unixtime)
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
      (if (not (org-ml-clock-is-running first)) it
        (let* ((time (org-ml-unixtime-to-time-long unixtime))
               (closed (org-ml-map-property* :value
                         (org-ml-timestamp-set-end-time time it)
                         first))
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
  (->> (org-ml-get-parents headline)
       (org-ml--map* (org-ml-get-property :raw-value it))))

(defun org-ml-headline-update-item-statistics (headline)
  "Return HEADLINE node with updated statistics cookie via items.

The percent/fraction will be computed as the number of checked items
over the number of items with checkboxes (non-checkbox items will
not be considered)."
  (let* ((items
          (->> (org-ml-headline-get-section headline)
               (org-ml-get-children)
               (--filter (org-ml-is-type 'plain-list it))
               (-mapcat #'org-ml-get-children)
               (--remove (org-ml--property-is-nil :checkbox it))))
         (done (length (--filter (org-ml--property-is-eq :checkbox 'on it)
                                 items)))
         (total (length items)))
    (org-ml--headline-set-statistics-cookie-fraction done total headline)))

(defun org-ml-headline-update-todo-statistics (headline)
  "Return HEADLINE node with updated statistics cookie via subheadlines.

The percent/fraction will be computed as the number of done
subheadlines over the number of todo subheadlines (eg non-todo
subheadlines will not be counted)."
  (let* ((subtodo (->> (org-ml-headline-get-subheadlines headline)
                       (--filter (org-ml--get-property-nocheck :todo-keyword it))))
         (done (length (-filter #'org-ml-headline-is-done subtodo)))
         (total (length subtodo)))
    (org-ml--headline-set-statistics-cookie-fraction done total headline)))

;;; plain-list

;; TODO there seems to be a bug in the interpreter that prevents "+" bullets from
;; being recognized (as of org-9.1.9 they are simply read as "-")
(defun org-ml-plain-list-set-type (type plain-list)
  "Return PLAIN-LIST node with type property set to TYPE.
TYPE is one of the symbols `unordered' or `ordered'."
  (cond
   ((eq type 'unordered)
    (org-ml--map-children-nocheck*
      (org-ml--map* (org-ml-set-property :bullet '- it) it)
      plain-list))
   ((eq type 'ordered)
    ;; NOTE the org-interpreter seems to use the correct, ordered numbers if any
    ;; number is set here. This behavior may not be reliable.
    (org-ml--map-children-nocheck*
      (org-ml--map* (org-ml-set-property :bullet 1 it) it)
      plain-list))
   (t (org-ml--arg-error "Invalid type: %s" type))))

;;; table

(defun org-ml-table-get-cell (row-index column-index table)
  "Return table-cell node at ROW-INDEX and COLUMN-INDEX in TABLE node.
Rule-type rows do not count toward row indices."
  (-some->> (org-ml--table-get-row row-index table)
            (org-ml-get-children)
            (org-ml--nth column-index)))

(defun org-ml-table-delete-row (row-index table)
  "Return TABLE node with row at ROW-INDEX deleted."
  (org-ml--map-children-nocheck* (org-ml--remove-at row-index it) table))

(defun org-ml-table-delete-column (column-index table)
  "Return TABLE node with column at COLUMN-INDEX deleted."
  (org-ml--map-children-nocheck*
   (org-ml--map*
    (if (org-ml--property-is-eq :type 'rule it) it
      (org-ml--map-children-nocheck* (org-ml--remove-at column-index it) it))
    it)
   table))

(defun org-ml-table-insert-column! (column-index column-text table)
  "Return TABLE node with COLUMN-TEXT inserted at COLUMN-INDEX.

COLUMN-INDEX is the index of the column and COLUMN-TEXT is a list of
strings to be made into table-cells to be inserted following the same
syntax as `org-ml-build-table-cell!'."
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
  (if (not column-text) (org-ml--table-clear-column column-index table)
    (let ((column-cells (-map #'org-ml-build-table-cell! column-text)))
      (org-ml--table-replace-column column-index column-cells table))))

(defun org-ml-table-replace-row! (row-index row-text table)
  "Return TABLE node with the row at ROW-INDEX replaced by ROW-TEXT.

If ROW-TEXT is a list of strings, it will replace the cells at
ROW-INDEX. Each member of ROW-TEXT will be processed the same as
the argument given to `org-ml-build-table-row!'.

If ROW-TEXT is nil, it will clear all cells at ROW-INDEX."
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
immediately before INDEX (called \"parent\", bound to 'it') and
the item at INDEX to be set as its child (bound to 'it-target')
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

;; headline

(defun org-ml-headline-demote-subtree (index headline)
  "Return HEADLINE node with child headline at INDEX demoted.
Unlike `org-ml-headline-demote-subheadline' this will also demote the
demoted headline node's children."
  (org-ml-headline-map-subheadlines*
    (org-ml--tree-set-child* index
      (org-ml--map-children-nocheck*
       (-snoc it (org-ml--headline-subtree-shift-level 1 it-target))
       it)
      it)
    headline))

(defun org-ml-headline-demote-subheadline (index headline)
  "Return HEADLINE node with child headline at INDEX demoted.
Unlike `org-ml-headline-demote-subtree' this will not demote the
demoted headline node's children."
  (org-ml-headline-map-subheadlines*
    (org-ml--tree-set-child* index
      (let* ((headlines-in-target (org-ml-headline-get-subheadlines it-target))
             (tgt-children (org-ml-get-children it-target))
             (tgt-pb (if (org-ml-is-type 'section (car tgt-children))
                            (org-ml--get-property-nocheck :post-blank (car tgt-children))
                          (org-ml--get-property-nocheck :pre-blank it-target)))
             (tgt-headline* (->> it-target
                                 (org-ml-headline-set-subheadlines nil)
                                 (org-ml--headline-shift-level 1)
                                 (org-ml--set-property-nocheck :post-blank tgt-pb))))
        (org-ml--map-children-nocheck*
         (append it (list tgt-headline*) headlines-in-target)
         it))
      it)
    headline))

;; plain-list

(defun org-ml-plain-list-indent-item-tree (index plain-list)
  "Return PLAIN-LIST node with child item at INDEX indented.
Unlike `org-ml-item-indent-item' this will also indent the indented item
node's children."
  (org-ml--map-children-nocheck*
   (org-ml--tree-set-child* index
     (org-ml--item-map-subitems* (-snoc it it-target) it)
     it)
   plain-list))

(defun org-ml-plain-list-indent-item (index plain-list)
  "Return PLAIN-LIST node with child item at INDEX indented.
Unlike `org-ml-item-indent-item-tree' this will not indent the indented
item node's children."
  (org-ml--map-children-nocheck*
   (org-ml--tree-set-child* index
     (-let* (((h i pb r) (org-ml--item-get-subcomponents it-target))
             (tgt-item* (org-ml--item-set-subcomponents `(,h nil nil nil) it-target)))
       (org-ml--item-map-subcomponents*
        (-let (((h* i* pb* r*) it))
          (if r*
              (let ((pl (-some->> (cons tgt-item* i)
                          (apply #'org-ml-build-plain-list :post-blank pb)
                          (list))))
                (list h* i* pb* (append r* pl r)))
            (--> (org-ml--map-last*
                  (org-ml--map-property-nocheck* :post-blank (+ it pb*) it)
                  i*)
                 (list h* (append it (list tgt-item*) i) pb r))))
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
The node at INDEX will be bound to the symbol 'it' which is to be
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
      (let ((children (->> (org-ml-get-children it)
                           (org-ml--map* (org-ml--headline-subtree-shift-level -1 it))))
            (parent (org-ml--set-children-nocheck nil it)))
        (list parent children))
      it)
    headline))

;; plain-list

(defun org-ml-plain-list-outdent-all-items (index plain-list)
  "Return PLAIN-LIST node with all child items under INDEX outdented."
  (org-ml--map-children-nocheck*
   (org-ml--split-children-at-index* index
     (-let* (((h i pb r) (org-ml--item-get-subcomponents it))
             (parent (org-ml--item-set-subcomponents `(,h nil nil nil) it))
             (i* (org-ml--map-last*
                  (org-ml--map-property-nocheck* :post-blank (+ pb it) it)
                  i)))
       (list parent (append i* r)))
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
      (-let* (((head tail) (-split-at child-index (org-ml-get-children it)))
              (target (->> (car tail)
                           (org-ml--headline-shift-level -1)
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
  (org-ml--map-children-nocheck*
   (org-ml--split-children-at-index* index
     (-let* (((h i pb r) (org-ml--item-get-subcomponents it))
             ((parent-i (tgt . tgt-i)) (-split-at child-index i))
             (parent-pb (-some->> (-last-item parent-i)
                          (org-ml-get-property :post-blank)))
             (parent (->> (org-ml-set-property :post-blank 0 it)
                          (org-ml--item-set-subcomponents
                           `(,h ,parent-i ,parent-pb nil))))
             (tgt-pb (-some->> tgt (org-ml-get-property :post-blank)))
             (tgt* (-some->> tgt
                     (org-ml--item-map-subcomponents*
                      (-let* (((h* i* pb* r*) it)
                              (h** (org-ml--map-last*
                                    (org-ml-set-property :post-blank tgt-pb it)
                                    h*)))
                        (if (not r*) (list h** (append i* tgt-i) pb* r)
                          (--> (apply #'org-ml-build-plain-list tgt-i)
                               (list it)
                               (append r* it r)
                               (list h** i* pb* it)))))
                     (org-ml-set-property :post-blank pb)
                     (list))))
       (list parent tgt*))
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
                 special-block verse-block)
  "Branch element nodes that require \"\" to correctly print empty.
This is a workaround for a bug.")

(defun org-ml--blank (node)
  "Return NODE with empty child nodes `org-ml--blank-if-empty' set to contain \"\"."
  (if (org-ml-is-childless node)
      (cond
       ((and org-ml-parse-habits (org-ml-is-type 'timestamp node))
        (let ((s (org-element-interpret-data node)))
          (-if-let (h (-some->> (org-ml-get-property :raw-value node)
                        (s-match "+[[:digit:]]+[ymwd]/\\([[:digit:]]+[ymwd]\\)")
                        (cadr)))
              (concat (substring s 0 -1) "/" h (s-right 1 s))
            s)))
       ((org-ml-is-any-type org-ml--blank-if-empty node)
        (org-ml--set-blank-children node))
       (t
        (unless (or (org-ml-is-any-type org-ml--rm-if-empty node)
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
    (->> (org-ml--blank node)
         (org-element-interpret-data)
         (substring-no-properties)))
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
       ;; TODO these all smell like something that should just be in the
       ;; public API...except that I don't really mess around with the bounds
       ;; anywhere except here (mostly because they already exist when the
       ;; string is parsed
       (shift-property
        (prop n node)
        (org-ml--map-property-nocheck* prop (+ n it) node))
       (shift-property-maybe
        (prop n node)
        (org-ml--map-property-nocheck* prop (when it (+ n it)) node))
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
        (org-ml--map-property-nocheck* prop
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
          (if (org-ml-is-type 'paragraph it)
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
        (-some->> (concat prefix string)
          (org-ml--from-string)
          (org-ml--get-descendent level)
          (shift-branch-object-node -1)
          (org-ml-match-map '((:not plain-text) *) #'decrement-object-node))))
    (-some->> (cond
               ((eq type 'paragraph)
                (let* ((pb (string-to-post-blank string))
                       (e (1+ (length string)))
                       (ce (- e pb)))
                  (-some->> (org-ml-build-paragraph! string :post-blank pb)
                    (org-ml--set-properties-nocheck (list :begin 1
                                                          :contents-begin 1
                                                          :end e
                                                          :contents-end ce)))))
               ((and (eq type 'section) (s-matches-p "^\\*" string))
                (-some->> (concat " " string)
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
                  (-some->> (format "* dummy\n:PROPERTIES:\n%s\n:END:" string)
                    (org-ml--from-string)
                    (org-ml--get-descendent '(0 0 0))
                    (org-ml--set-properties-nocheck (list :post-affiliated 1
                                                          :begin 1
                                                          :post-blank pb
                                                          :end e)))))
               ((eq type 'property-drawer)
                (-some->> (concat "* dummy\n" string)
                  (org-ml--from-string)
                  (org-ml--get-descendent '(0 0))
                  (shift-branch-element-node -8)
                  (org-ml--map-children-nocheck*
                   (--map (shift-element-node -8 it) it))))
               ((eq type 'planning)
                (-some->> (concat "* dummy\n" string)
                  (org-ml--from-string)
                  (org-ml--get-descendent '(0 0))
                  (shift-element-node -8)
                  (shift-property-node :scheduled -8)
                  (shift-property-node :deadline -8)
                  (shift-property-node :closed -8)))
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
                    (-some->> (org-ml--from-string string)
                      (org-ml--get-descendent level)))))
      (org-ml--set-property-nocheck :parent nil))))

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
  `(let* ((children (org-ml-get-children ,node))
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
       `(equal (org-ml--get-property-nocheck ,prop ,it-node) ,val))
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
              (-mapcat (lambda (a) (org-ml--map* (append a it) p*)) acc))
          (org-ml--map* (append it (list p)) acc))))
    (-uniq (-reduce-from #'add-subpattern '(()) pattern))))

(defun org-ml--match-pattern-process-alternations (end? limit alt-patterns)
  "Convert ALT-PATTERNS to a matching form.
ALT-PATTERNS is a list of patterns created from expanded
alternations in the original pattern.

See `org-ml--match-pattern-make-inner-form' for the meaning of
END? and LIMIT."
  (->> (if end? alt-patterns (reverse alt-patterns))
       (org-ml--map* (org-ml--match-pattern-make-inner-form end? limit it))
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

(defconst org-ml--match-form-cache (make-hash-table :test #'equal)
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
  nil, this will match 'at most N times' or 'at least M times'
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
    (declare (debug (form def-form)))
    (declare (indent 1))
    ;; TODO this makes a closure
    `(cl-labels
         ((rec
           (node)
           (if (not (org-ml-is-branch-node node)) node
             (org-ml-map-children*
               (let ((it (org-ml--map* (rec it) it)))
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
            ((&plist :begin :end) (org-ml-get-all-properties context))
            (tree (org-ml--parse-objects type (+ begin offset) end)))
      (->> (car tree)
           (org-ml--get-descendent nesting)
           (org-ml--filter-types org-ml-objects)))))

(defun org-ml--parse-element-at (point type)
  "Return element node immediately under POINT.
For a list of all possible return types refer to `org-ml-elements'; this
will return everything in this list except 'section' which is
ambiguous when referring to a single point.
\(see `org-ml-parse-section-at').

If TYPE is non-nil, only return nil if the object under point is not
of that type. TYPE is a symbol from `org-ml-elements'. Furthermore,
setting TYPE to 'table-row' will prefer table-row elements over table
elements and likewise when setting TYPE to 'item' for plain-list
elements vs item elements."
  (save-excursion
    (goto-char point)
    ;; TODO this seems really inefficient; essentially we are parsing twice
    (let* ((node (org-element-at-point))
           (node-type (org-ml-get-type node)))
      ;; NOTE this will not filter by type if it is a leaf node
      (if (not (memq node-type org-ml-branch-nodes)) node
        ;; need to parse again if branch-node since
        ;; `org-element-at-point' does not parse children
        (-let* (((&plist :begin :end :contents-end :post-blank)
                 (org-ml-get-all-properties node))
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
                           (_ '(0)))))
          (--> (org-ml--get-descendent nesting tree)
               ;; set ending boundaries according to what we get from
               ;; `org-element-at-point'
               (org-ml--set-properties-nocheck (list :end end
                                                     :contents-end contents-end
                                                     :post-blank post-blank)
                                               it)
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
      (let ((b (point))
            (e (if subtree
                   (progn
                     (org-end-of-subtree)
                     ;; skip ahead to the next headline because
                     ;; `org-end-of-subtree' does not by default, which misses
                     ;; any spacing after headlines
                     (or (outline-next-heading)
                         (point-max)))
                 (or (outline-next-heading) (point-max)))))
        (car (org-ml--parse-elements b e 'first-section))))))

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
    (org-ml--get-descendent
     '(0)
     (condition-case nil
         (progn
           (org-back-to-heading)
           (org-ml--parse-headline-subtree-at point nil))
       (error
        (org-ml--parse-elements
         (point-min) (or (outline-next-heading) (point-max)) 'first-section))))))

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
  (let* ((c (org-ml-get-children (org-element-parse-buffer)))
         (b (if c (org-ml--get-property-nocheck :begin (-first-item c)) 1))
         (e (if c (org-ml--get-property-nocheck :end (-last-item c)) 1)))
    (org-ml--construct 'org-data `(:begin ,b :end ,e) c)))

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

;; this is verbatim from the Myers paper and should have O(M+N+D^2) on average
(defun org-ml--diff-find-ses (str-a str-b)
  "Given STR-A and STR-B, find the shortest edit sequence (SES).
Return a list like (D k Vd Dmax) where D is the length of the
shortest edit sequence, k is the final diagonal on which the diff
ends, Vd is a list of vectors describing the furthest, reaching
paths at every D (which the highest D first), and Dmax is the max
of D."
  (let* ((M (length str-a))
         (N (length str-b))
         (Dmax (+ M N))
         ;; this seems weird but it is much faster to use "strings" to hold
         ;; the endpoints rather than vectors (since all we need is an array
         ;; that holds positive integers, which is just a string)
         (V (string-to-multibyte (make-string (1+ (* 2 Dmax)) 0)))
         (D 0)
         k x y stop Vd)
    (if (= 0 Dmax) `(0 0 ,Vd ,Dmax)
      (aset V (1+ Dmax) 0)
      (while (and (not stop) (<= D Dmax))
        (setq k (- D))
        (while (and (not stop) (<= k D))
          (if (or (= k (- D))
                  (and (/= k D) (< (elt V (+ (1- k) Dmax))
                                   (elt V (+ (1+ k) Dmax)))))
              (setq x (elt V (+ (1+ k) Dmax)))
            (setq x (1+ (elt V (+ (1- k) Dmax)))))
          (setq y (- x k))
          (while (and (< x M) (< y N) (= (elt str-a x) (elt str-b y)))
            (setq x (1+ x)
                  y (1+ y)))
          (aset V (+ k Dmax) x)
          (when (and (>= x M) (>= y N))
            (setq stop t))
          (setq k (+ 2 k)))
        (setq Vd (cons (copy-sequence V) Vd))
        (unless stop
          (setq D (1+ D))))
      (list D (- M N) Vd Dmax))))

(defun org-ml--diff-ses-to-edits (D k Vd Dmax)
  "Return the edit path as given by the Myers diff algorithm.
See `org-ml--diff-find-ses' for the meaning of D, K, VD, and DMAX."
  ;; backtrack overview: this will walk up the edit path backwards to make a
  ;; condensed edit script (which is just like an edit script as referenced in
  ;; the Myers paper except that consecutive edits are collapsed into one
  ;; meta-edit).
  ;;
  ;; in order to get the collapsing part right, the basic idea is to define the
  ;; start of any edit as the most left-bound point in any diagonal (which is
  ;; stored as 'start-x' and 'start-y') and then traverse up/left until we hit a
  ;; new diagonal, in which case we use the previous x and y as the end of the
  ;; edit.
  (let (path prev-x prev-y start-x start-y start-vert? V x y vert? x*)
    (while (<= 0 D)
      ;; for the given set of endpoints at D, find the current x and y given k
      (setq V (car Vd)
            x (elt (car Vd) (+ Dmax k))
            y (- x k)
            ;; determine direction of the next endpoint and it's x value
            vert? (or (= k (- D))
                      (and (/= k D) (< (elt V (+ (1- k) Dmax))
                                       (elt V (+ (1+ k) Dmax)))))
            x* (if vert? (elt V (+ (1+ k) Dmax)) (1+ (elt V (+ (1- k) Dmax)))))
      ;; if current x = next x we must not be on a diagonal
      (if (and (= x* x) (< 0 x*) (eq start-vert? vert?))
          (progn
            (unless start-x
              (setq start-x x
                    start-y y
                    start-vert? vert?))
            (setq prev-x x
                  prev-y y))
        ;; if we are on a diagonal, close off the previously held point
        ;; and add it to the edit path as either an insert of a delete
        ;; depending on if we are traversing up or left
        (when start-x
          (setq path (cons (if start-vert?
                               `(ins ,(1- start-x) ,(1- prev-y) ,(1- start-y))
                             `(del ,(1- prev-x) ,(1- start-x)))
                           path)))
        ;; then walk up the diagonal to get to the next horizontal/vertical
        ;; sequence
        (while (< x* x)
          (setq x (1- x)
                y (1- y)))
        (setq start-x x
              start-y y
              start-vert? vert?
              prev-x x
              prev-y y))
      (setq k (if vert? (1+ k) (1- k))
            D (1- D)
            Vd (cdr Vd)))
    (nreverse path)))

(defun org-ml--diff-region (start end new-str)
  "Use Myers Diff algorithm to update the current buffer.
The region to be updated will be between START and END and will
be made to look like NEW-STR. Only differences as given by the Myers
diff algorithm (eg insertions and deletions) will actually be
applied to the buffer."
  (-let* ((old-str (buffer-substring-no-properties start end))
          ((D k Vd MAX) (org-ml--diff-find-ses old-str new-str))
          (cmds (org-ml--diff-ses-to-edits D k Vd MAX)))
    (save-excursion
      (while cmds
        (pcase (car cmds)
          (`(ins ,i ,m ,n)
           (goto-char (+ 1 start i))
           (insert (substring new-str m (1+ n))))
          (`(del ,i ,j)
           (delete-region (+ start i) (+ 1 start j))))
        (setq cmds (cdr cmds))))))

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

(org-ml--defun-anaphoric* org-ml--update (diff-mode fun node)
  "Internal version of `org-ml~update'.
DIFF-MODE, FUN, and NODE have the same meaning. The only
difference is this function does not save the point's position"
  ;; if node is of type 'org-data' it will have no props
  (let* ((begin (org-ml--get-property-nocheck :begin node))
         (end (org-ml--get-property-nocheck :end node))
         (ov-cmd (-some->> (overlays-in begin end)
                   (--filter (eq 'outline (overlay-get it 'invisible)))
                   (org-ml--map* (list :start (overlay-start it)
                                       :end (overlay-end it)
                                       :props (overlay-properties it)))
                   (list 'apply 'org-ml--apply-overlays)))
         ;; do all computation before modifying buffer
         ;;
         ;; TODO it might be useful to add this as a switch for cases where
         ;; `FUN' almost never changes anything, in which case it would be
         ;; much cheaper to check if the node is equal before inserting it.
         ;; In 99.9999% of cases, this is probably false, so just assume
         ;; the node has changed and update it
         ;;
         ;; (node0 (org-ml-clone-node node))
         (node* (funcall fun node)))
    ;; (unless (org-ml--equal node0 node*)
    ;; hacky way to add overlays to undo tree
    (when ov-cmd
      (setq-local buffer-undo-list (cons ov-cmd buffer-undo-list)))
    (if diff-mode
        (org-ml--diff-region begin end (org-ml-to-string node*))
      (progn
        (delete-region begin end)
        (org-ml--insert begin node*)))
    nil))

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
FUN is a unary function that takes a node of type 'org-data' and
returns a modified node."
  (org-ml-update fun (org-ml-parse-this-buffer)))

;;; fold

(defun org-ml--fold-get-contents-begin-maybe (node)
  "Return :contents-begin minus one or nil if not found for NODE."
  (-some-> (org-ml-get-property :contents-begin node) (1-)))

(eval-when-compile
  (defmacro org-ml--fold-get-contents-begin-offset (node offset)
    "Return the fold beginning boundary of NODE.
Try `org-ml--fold-get-contents-begin-maybe' first, and if this returns nil,
use OFFSET to calculated the beginning fold boundary beginning.
OFFSET can either be an integer or a form that evaluates to an
integer."
    (declare (indent 1) (debug (form form)))
    `(or (org-ml--fold-get-contents-begin-maybe ,node)
         (+ ,offset (org-ml-get-property :begin ,node)))))

(defun org-ml--fold-get-begin-boundary (node)
  "Return integer for point at the beginning of fold region for NODE."
  (cl-case (org-ml-get-type node)
    ;; Blocks must be folded regardless of if they have children
    (center-block
     (org-ml--fold-get-contents-begin-offset node 14))
    (dynamic-block
     (org-ml--fold-get-contents-begin-offset node
       (+ 9 (length (org-ml-get-property :block-name node)))))
    (drawer
     (org-ml--fold-get-contents-begin-offset node
       (+ 2 (length (org-ml-get-property :drawer-name node)))))
    (property-drawer
     (org-ml--fold-get-contents-begin-offset node 12))
    ((quote-block verse-block)
     (org-ml--fold-get-contents-begin-offset node 13))
    (special-block
     (org-ml--fold-get-contents-begin-offset node
       (+ 9 (length (org-ml-get-property :type node)))))
    ;; Headlines should only be folded if they have children
    (headline
     (org-ml--fold-get-contents-begin-maybe node))
    ;; Items are tricky since everything after the "first line" is folded. If
    ;; the first child is a paragraph, need to figure out how long its first
    ;; line is and add that to :contents-begin. Do nothing if there are no
    ;; children
    (item
     (-when-let (first (-first-item (org-ml-get-children node)))
       (let ((offset (if (not (org-ml-is-type 'paragraph first)) -1
                       (->> (org-ml-to-string first)
                            (s-split "\n")
                            (-first-item)
                            (length)))))
         (+ offset (org-ml-get-property :contents-begin node)))))
    ;; These elements are not branch types and thus don't have child boundaries,
    ;; so will need to manually calculated where the boundaries should be
    ((comment-block example-block)
     (+ 15 (org-ml-get-property :begin node)))
    (export-block
     (+ (org-ml-get-property :begin node)
        (-if-let (type (org-ml-get-property :type node))
            (1+ (length type)) 0)
        14))
    (src-block
     (+ (org-ml-get-property :begin node)
        (-if-let (meta (-some->
                        (list
                         (org-ml-get-property :language node)
                         (org-ml--get-property-nocheck :switches node)
                         (org-ml--get-property-nocheck :parameters node))
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
    (let ((end (- (org-ml-get-property :end node)
                  (org-ml-get-property :post-blank node)
                  1)))
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
          (-some->> section
                    (--first (org-ml-is-type 'property-drawer it))
                    (org-ml-fold))
          (let ((drawers (-some->> (org-ml-get-children section)
                                   (--filter (org-ml-is-type 'drawer it)))))
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
- 'all': parse all headlines (equivalent to [nil nil])

Each headline is obtained with `org-ml-parse-headline-at'."
  (cl-labels
      ((get-subheadlines
        (headline)
        (->> (org-ml-headline-get-subheadlines headline)
             (-mapcat #'get-subheadlines)
             (cons headline))))
    (->> (org-ml--parse-patterns-where which "^\\*")
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
      (--> (org-ml--parse-patterns-where which "^\\*")
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

;;; depreciated functions

;; affiliated keywords

(defun org-ml-get-affiliated-keyword (key node)
  "Get the value of affiliated keyword KEY in NODE.

See `org-ml-set-affiliated-keyword' for the meaning of KEY.

WARNING: This function is depreciated and will be removed in a
future major revision. Its functionality has been merged with
`org-ml-get-property'."
  (unless (or (memq key '(:caption :header :name :plot :results))
              (s-starts-with? ":attr_" (symbol-name key)))
    (org-ml--arg-error "Invalid affiliated keyword requested: %s" key))
  (org-ml--get-property-nocheck key node))

(defun org-ml-set-affiliated-keyword (key value node)
  "Set affiliated keyword KEY in NODE to VALUE.
This is just like `org-ml--set-property-nocheck' except it will
delete KEY from the plist if VALUE is nil.

NOTE that VALUE should reflect the required value of affiliated
keyword given by KEY. The format for each keyword is given below:
- NAME `STRING': `STRING'
- PLOT `STRING': `STRING'
- RESULTS[`STRING1'] `STRING2': (STRING2 . STRING1)
  where `STRING1' may be nil
- CAPTION[`STRING1'] `STRING2': ((STRING2 . STRING1) ...)
  where `STRING1' may be nil and multiple list members
  correspond to multiple caption entries
- HEADERS `STRING': (STRING ...) where multiple list members
  correspond to multiple headers entries
- CAPTION[`STRING'] `SECSTRING': ((STRING . SECSTRING) ...)
  where `STRING' may be nil and multiple list members
  correspond to multiple caption entries

In the case of ATTR_BACKEND, KEY is like `:attr_x' where `x'
corresponds to BACKEND and VALUE is a list of strings
corresponding to multiple entries of the attribute.

WARNING: This function is depreciated and will be removed in a
future major revision. Its functionality has been merged with
`org-ml-set-property'"
  (unless (org-ml-is-any-type org-ml--element-nodes-with-affiliated node)
    (org-ml--arg-error
     "Node type '%s' does not allow affiliated keywords"
     (org-ml-get-type node)))
  (let ((props
         (if value
             (plist-put (org-ml-get-all-properties node) key value)
           (org-ml--plist-remove key (org-ml-get-all-properties node)))))
    (org-ml--construct (org-ml-get-type node) props (org-ml-get-children node))))

(org-ml--defun-anaphoric* org-ml-map-affiliated-keyword (key fun node)
  "Apply FUN to value of affiliated keyword KEY in NODE.

See `org-ml-set-affiliated-keyword' for the meaning of KEY.

WARNING: This function is depreciated and will be removed in a
future major revision. Its functionality has been merged with
`org-ml-map-property'."
  (-some--> (org-ml-get-affiliated-keyword key node)
    (org-ml-set-affiliated-keyword key (funcall fun it) node)))

(defun org-ml-set-caption! (caption node)
  "Set the caption affiliated keyword of NODE.

CAPTION can be one of the following:
- STRING: produces #+CAPTION: `STRING'
- (STRING1 STRING2): produces #+CAPTION[`STRING2']: `STRING1'
- ((STRING1 STRING2) ...): like above but makes multiple
  caption entries
- nil: removes all captions

WARNING: This function is depreciated and will be removed in a
future major revision. Its functionality has been merged with
`org-ml-set-property'."
  (cl-flet
      ((is-metacell
        (cell)
        (pcase cell (`(,(pred stringp) ,(pred stringp)) t)))
       (convert-metacell
        (cell)
        (cons (org-ml-build-secondary-string! (cadr cell)) (car cell))))
    (let ((caption
           (pcase caption
             ((pred stringp)
              (list (list (org-ml-build-secondary-string! caption))))
             ((pred is-metacell)
              (list (convert-metacell caption)))
             ((pred (lambda (x) (-all? #'is-metacell x)))
              (-map #'convert-metacell caption))
             (`nil nil)
             (e (org-ml--arg-error "Invalid caption given: %s" e)))))
      (org-ml-set-affiliated-keyword :caption caption node))))

;; headline batch processing

(defalias 'org-ml-get-some-headlines 'org-ml-parse-headlines)

(defun org-ml-get-headlines ()
  "Return list of all headline nodes from current buffer.

This function is depreciated and will be removed in a later release.
Use `org-ml-parse-headlines' instead."
  (org-ml-parse-headlines 'all))

(defalias 'org-ml-get-some-subtrees 'org-ml-parse-subtrees)

(defun org-ml-get-subtrees ()
  "Return list of all subtree nodes from current buffer.

This function is depreciated and will be removed in a later release.
Use `org-ml-parse-subtrees' instead."
  (org-ml-parse-subtrees 'all))

(defalias 'org-ml-do-some-headlines 'org-ml-update-headlines)
(defalias 'org-ml-do-some-headlines* 'org-ml-update-headlines*)

(org-ml--defun* org-ml-do-headlines (fun)
  "Update all headlines in the current buffer using FUN.

Headlines are updated using `org-ml-update-this-headline' (see this for
use and meaning of FUN).

This function is depreciated and will be removed in a later release.
Use `org-ml-parse-headlines' instead."
  (org-ml-update-headlines 'all fun))

(defalias 'org-ml-do-some-subtrees 'org-ml-update-subtrees)
(defalias 'org-ml-do-some-subtrees* 'org-ml-update-subtrees*)

(org-ml--defun* org-ml-do-subtrees (fun)
  "Update all toplevel subtrees in the current buffer using FUN.

Subtrees are updated using `org-ml-update-this-subtree' (see this for use
and meaning of FUN).

This function is depreciated and will be removed in a later release.
Use `org-ml-update-subtrees' instead."
  (org-ml-update-subtrees 'all fun))

(provide 'org-ml)
;;; org-ml.el ends here
