;;; org-ml.el --- Functional Org Mode API -*- lexical-binding: t; -*-

;; Copyright (C) 2020  Nathan Dwarshuis

;; Author: Nathan Dwarshuis <ndwar@yavin4.ch>
;; Keywords: org-mode, outlines
;; Homepage: https://github.com/ndwarshuis/org-ml
;; Package-Requires: ((emacs "26.1") (org "9.3") (dash "2.17") (s "1.12"))
;; Version: 4.0.1

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

;; TODO `org-element-all-elements' does not exist in emacs <= 25
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
      (table table-row)))
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
  (let ((keys (org-ml--plist-get-keys plist)))
    (->> (org-ml--plist-get-vals plist)
         (--map (funcall fun it))
         (-interleave keys))))

(defun org-ml--is-plist (list)
  "Return t if LIST is a plist."
  (and
   (listp list)
   (cl-evenp (length list))
   (-all? #'keywordp (-slice list 0 nil 2))))

(defun org-ml--plist-remove (key plist)
  "Return PLIST with KEY and its value removed."
  (->> (-partition 2 plist) (--remove (eq (car it) key)) (-flatten-n 1)))

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

;;; MISC HELPER FUNCTIONS

(defun org-ml--get-head (node)
  "Return the type and properties cells of NODE."
  (if (stringp node) node
    (-take 2 node)))

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

(defun org-ml--get-property-nocheck (prop node)
  "Return PROP from NODE."
  (if (and (stringp node) (eq prop :post-blank))
      (length (car (s-match "[ ]*$" node)))
    (org-element-property prop node)))

(defun org-ml--get-all-properties (node)
  "Return the properties list of NODE."
  (if (stringp node) (text-properties-at 0 node) (nth 1 node)))

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
    (org-ml--construct
     (org-ml-get-type node)
     (plist-put (org-ml--get-all-properties node) prop value)
     (org-ml-get-children node))))

(defun org-ml--set-properties-nocheck (plist node)
  "Set all properties in NODE to the values corresponding to PLIST.
PLIST is a list of property-value pairs that correspond to the
property list in NODE."
  (if (org-ml--is-plist plist)
      (let ((props (org-ml--get-all-properties node)))
        (org-ml--construct
         (org-ml-get-type node)
         (->> (-partition 2 plist)
              (--reduce-from (apply #'plist-put acc it) props))
         (org-ml-get-children node)))
    (org-ml--arg-error "Not a plist: %S" plist)))

(defun org-ml--set-property-nocheck-nil (prop node)
  "Set PROP to nil in NODE."
  (org-ml--set-property-nocheck prop nil node))

(defun org-ml--set-properties-nocheck-nil (props node)
  "Set all PROPS to new in NODE."
  (let ((plist (--mapcat (list it nil) props)))
    (org-ml--set-properties-nocheck plist node)))

(eval-when-compile
  (defmacro org-ml--map-property-nocheck* (prop form node)
    "Return NODE with FUN applied to the value in PROP.
FUN is a form that returns a modified value and contains `it'
bound to the property value."
    (declare (indent 1))
    `(--> (org-ml--get-property-nocheck ,prop ,node)
          (funcall (lambda (it) ,form) it)
          (org-ml--set-property-nocheck ,prop it ,node))))

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
    `(and
      (funcall (lambda (it) ,form) (org-ml--get-property-nocheck ,prop ,node))
      t)))

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

(defun org-ml--is-valid-planning-timestamp (x)
  "Return t if X is an allowed value for a planning node timestamp property."
  (or (null x) (and (org-ml-is-type 'timestamp x)
                    (org-ml--property-is-eq :type 'active x))))

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

;; TODO this will make quotes turn to (quote )
(defun org-ml--encode-diary-sexp-value (value)
  "Return VALUE as a string.
VALUE must conform to `org-ml--is-valid-diary-sexp-value'."
  (if value (format "%%%%%S" value) "%%()"))

(defun org-ml--decode-diary-sexp-value (value)
  "Return VALUE as a form.
Return value will conform to `org-ml--is-valid-diary-sexp-value'."
  (->> (s-chop-prefix "%%" value) (read)))

;;; cis functions

(defun org-ml--update-macro-value (macro)
  "Return MACRO node with its value property updated.
This will be based on MACRO's key and value properties."
  (let* ((k (org-ml--get-property-nocheck :key macro))
         (as (org-ml--get-property-nocheck :args macro))
         (v (if as (format "%s(%s)" k (s-join "," as)) k)))
    (org-ml--set-property-nocheck :value (format "{{{%s}}}" v) macro)))

;; TODO this name is inaccurate, also, clocks need additional help to handle
;; the complexity and restrictions of ranges
(defun org-ml--update-clock-duration (clock)
  "Return CLOCK node with its duration and status properties updated.
This will be based on CLOCK's value property."
  (let* ((ts (org-ml--get-property-nocheck :value clock))
         (seconds (org-ml--timestamp-get-range ts))
         (ts* (if (/= seconds 0) (org-ml--timestamp-set-type-ranged t ts) ts)))
    ;; TODO this case highlights that I'm using imprecise terminology for
    ;; timestamps. Really the "range" in seconds should be "length" and the
    ;; "range" should refer to the type since this actually uses the word
    ;; "range"
    (if (not (org-ml--timestamp-is-range-type ts*))
        (org-ml--set-property-nocheck :value ts* clock)
      (let* ((h (-> seconds (/ 3600) (floor)))
             (m (-> seconds (- (* h 3600)) (/ 60) (floor))))
        (->> clock
             (org-ml--set-property-nocheck :duration (format "%2d:%02d" h m))
             (org-ml--set-property-nocheck :status 'running)
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
      (--> (- offset n)
           (mod it diff)
           (- it offset)
           (+ priority it)))))

;;; property alist

(org-ml--defconst org-ml--property-alist
  (let ((bool (list :pred #'booleanp
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
        (planning (list :pred #'org-ml--is-valid-planning-timestamp
                        :type-desc "a zero-range, active timestamp node"))
        (ts-unit (list :pred #'org-ml--is-valid-timestamp-unit
                       :type-desc '("nil or a symbol from `year' `month'"
                                    "`week' `day', or `hour'")))
        (post-blank (list :post-blank :pred #'org-ml--is-non-neg-integer
                          :shift #'org-ml--shift-non-neg-integer)))
    (->>
     `((babel-call (:call ,@ol-str :require t)
                   (:inside-header ,@plist)
                   (:arguments ,@slist-com)
                   (:end-header ,@plist)
                   (:value))
       (bold)
       (center-block)
       (clock (:value :pred org-ml--is-valid-clock-timestamp
                      :cis org-ml--update-clock-duration
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
                                :decode org-ml--decode-string-or-nil) ; TODO restrict this?
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
                    ;; TODO make this desc better
                    :type-desc ("a oneline string from `org-link-types'"
                                "or \"coderef\", \"custorg-ml-id\","
                                "\"file\", \"id\", \"radio\", or"
                                "\"fuzzy\"")
                    ;; TODO is fuzzy a good default?
                    :require "fuzzy")
             (:raw-link) ; TODO update children through this?
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
       (planning (:closed ,@planning)
                 (:deadline ,@planning)
                 (:scheduled ,@planning))
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
                 (-snoc it '(:post-affiliated))))))

;;; node property operations

;; alist functions

(defun org-ml--get-property-attribute (attribute type prop)
  "Return ATTRIBUTE for PROP of node TYPE.
Signal an error if PROP in TYPE does not have ATTRIBUTE."
  (-if-let (type-list (alist-get type org-ml--property-alist))
      (if (assoc prop type-list)
          (-when-let (plist (alist-get prop type-list))
            (plist-get plist attribute))
        (org-ml--arg-error "Type '%s' does not have property '%s'"
                       type prop))
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

(defun org-ml--map-children-nocheck (fun node)
  "Return NODE with FUN applied to its children.

FUN is a unary function that takes a list of children and returns
a modified list of children."
  (--> (org-ml-get-children node)
       (funcall fun it)
       (org-ml--set-children-nocheck it node)))

(defun org-ml--set-childen-throw-error (type child-types illegal)
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
  (--mapcat (list it nil) props))

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

(defun org-ml--time-is-long (time)
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
  (let* ((zone (list (current-time-zone)))
         (encode-args
          (if (org-ml--time-is-long time)
              (append '(0) (reverse time) zone)
            (append '(0 0 0) (reverse (-take 3 time)) zone))))
    (->> (apply #'encode-time encode-args)
         (float-time)
         (round))))

(defun org-ml-unixtime-to-time-long (unixtime)
  "Return the long time list of UNIXTIME.
The list will be formatted like (YEAR MONTH DAY HOUR MIN)."
  (reverse (-slice (decode-time unixtime (current-time-zone)) 1 6)))

(defun org-ml-unixtime-to-time-short (unixtime)
  "Return the short time list of UNIXTIME.
The list will be formatted like (YEAR MONTH DAY nil nil)."
  (append (-take 3 (org-ml-unixtime-to-time-long unixtime))
          '(nil nil)))

(defun org-ml--time-truncate (time)
  "Return the short time format of TIME regardless of input format."
  (-take 3 time))

(defun org-ml--time-shift (n unit time)
  "Return modified time list TIME shifted N UNIT's.

UNIT is one of `day', `week', `month', `year', `minute', or `hour'.
N is an integer."
  (cl-flet*
      ((get-shifts-short
        (n unit)
        (cl-case unit
          (day `(0 0 ,n 0 0))
          (week `(0 0 ,(* 7 n) 0 0))
          (month `(0 ,n 0 0 0))
          (year `(,n 0 0 0 0))
          ((minute hour)
           (org-ml--arg-error "Invalid unit for short timestamps: %S" unit))
          (t (org-ml--arg-error "Invalid time unit: %S" unit))))
       (get-shifts-long
        (n unit)
        (cl-case unit
          (minute `(0 0 0 0 ,n))
          (hour `(0 0 0 ,n 0))
          (t (get-shifts-short n unit))))
       (apply-shifts
        (shifts time)
        (->> (-zip-with #'+ time shifts)
             (reverse)
             (apply #'encode-time 0)
             (decode-time))))
    (if (org-ml--time-is-long time)
        (let ((shifts (get-shifts-long n unit)))
          (reverse (-slice (apply-shifts shifts time) 1 6)))
      (let ((shifts (get-shifts-short n unit))
            (time* (-replace nil 0 time)))
        (->> (-slice (apply-shifts shifts time*) 3 6)
             (append '(nil nil))
             (reverse))))))

(defun org-ml--time-format-props (time suffix)
  "Return plist representation of time list TIME.
SUFFIX is either `start' or `end'."
  (let* ((props (->> '(year month day hour minute)
                     (--map (intern (format ":%s-%s" it suffix)))))
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

(defun org-ml--decorator-format (dec dtype valid-types)
  "Return plist representing a timestamp warning or repeater (decorators).

DEC is a list like (TYPE VALUE UNIT) of the decorator, DTYPE is either
`warning' or `repeater', and VALID-TYPES are the allowed values for
TYPE given in DEC."
  (let ((props (->> '(type value unit)
                    (--map (intern (format ":%s-%s" dtype it))))))
    (if (not dec) (org-ml--init-properties props)
      (-let (((type value unit) dec))
        (unless (memq type valid-types)
          (org-ml--arg-error "Invalid %s type: %s" dtype type))
        (unless (integerp value)
          (org-ml--arg-error "Invalid %s value: %s" dtype value))
        (unless (memq unit '(year month week day hour))
          (org-ml--arg-error "Invalid %s unit: %s" dtype value))
        (-interleave props (list type value unit))))))

;; timestamp (regular)

(defun org-ml--timestamp-get-start-time (timestamp)
  "Return the time list of the start time in TIMESTAMP."
  (-let (((&plist :minute-start n :hour-start h :day-start d
                  :month-start m :year-start y)
          (org-ml--get-all-properties timestamp)))
    `(,y ,m ,d ,h ,n)))

(defun org-ml--timestamp-get-end-time (timestamp)
  "Return the time list of the end time in TIMESTAMP."
  (-let (((&plist :minute-end n :hour-end h :day-end d
                  :month-end m :year-end y)
          (org-ml--get-all-properties timestamp)))
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
  (let ((time* (org-ml--time-format-props time 'start)))
      (org-ml--set-properties-nocheck time* timestamp)))

(defun org-ml--timestamp-set-start-time (time timestamp)
  "Return TIMESTAMP with start time properties set according to time list TIME."
  (->> (org-ml--timestamp-set-start-time-nocheck time timestamp)
       (org-ml--timestamp-update-type-ranged)))

(defun org-ml--timestamp-set-end-time-nocheck (time timestamp)
  "Set the end TIME of TIMESTAMP. Does not set type."
  (if time
      (-> (org-ml--time-format-props time 'end)
          (org-ml--set-properties-nocheck timestamp))
    (-> (org-ml--timestamp-get-start-time timestamp)
        (org-ml--time-format-props 'end)
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
         (long? (org-ml--time-is-long start))
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
    (cl-case it
      ((active active-range)
       (if ranged? 'active-range 'active))
      ((inactive inactive-range)
       (if ranged? 'inactive-range 'inactive))
      (t (org-ml--arg-error "Invalid timestamp type: %s" it)))
    timestamp))

(defun org-ml--timestamp-set-active (flag timestamp)
  "Return TIMESTAMP with active type if FLAG is t."
  (let* ((type (if (org-ml--timestamp-is-ranged-lowres timestamp)
                   (if flag 'active-range 'inactive-range)
                 (if flag 'active 'inactive))))
    (org-ml--set-property-nocheck :type type timestamp)))

(defun org-ml--timestamp-set-warning (warning timestamp)
  "Return TIMESTAMP with warning properties set to WARNING list."
  (let ((types '(all first)))
    (-> (org-ml--decorator-format warning 'warning types)
        (org-ml--set-properties-nocheck timestamp))))

(defun org-ml--timestamp-set-repeater (repeater timestamp)
  "Return TIMESTAMP with warning properties set to REPEATER list."
  (let ((types '(catch-up restart cumulate)))
    (-> (org-ml--decorator-format repeater 'repeater types)
        (org-ml--set-properties-nocheck timestamp))))

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

(defun org-ml--planning-list-to-timestamp (planning-list)
  "Return timestamp node from PLANNING-LIST.
See `org-ml-build-planning!' for syntax of PLANNING-LIST."
  (when planning-list
    (let* ((p (-partition-before-pred
               (lambda (it) (memq it '(&warning &repeater)))
               planning-list)))
      (org-ml-build-timestamp! (car p)
                           :active t
                           :warning (alist-get '&warning p)
                           :repeater (alist-get '&repeater p)))))

;;; INTERNAL TYPE-SPECIFIC BRANCH/CHILD FUNCTIONS

;;; headline

(defun org-ml--headline-subtree-shift-level (n headline)
  "Return HEADLINE node with its level shifted by N.
Also shift all HEADLINE node's child headline nodes by N.
If the final shifted level is less one, set level to one (for parent
and child nodes)."
  (->> (org-ml--headline-shift-level n headline)
       (org-ml-headline-map-subheadlines
        (lambda (headlines)
          (--map (org-ml--headline-subtree-shift-level n it)
                 headlines)))))

(defun org-ml--headline-set-level (level headline)
  "Return HEADLINE node with its level set to LEVEL.
Additionally set all child headline nodes to be (+ 1 level) for
first layer, (+ 2 level for second, and so on."
  (->> (org-ml-set-property :level level headline)
       (org-ml--map-children-nocheck
         ;; TODO won't this also try to 'indent' the section?
         (lambda (subheadlines)
           (--map (org-ml--headline-set-level (1+ level) it) subheadlines)))))

;;; table

(defun org-ml--table-get-width (table)
  "Return the width of TABLE as an integer.
This effectively is the maximum of all table-row lengths."
  (->> (org-ml-get-children table)
       (--map (length (org-ml-get-children it)))
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
  (cl-flet*
      ((zip-into-rows
        (row new-cell)
        (if (org-ml--property-is-eq :type 'rule row) row
          (org-ml--map-children-nocheck
           (lambda (cells) (funcall fun new-cell cells))
           row)))
       (map-rows
        (rows)
        (->> rows
             (--find-indices (org-ml--property-is-eq :type 'rule it))
             (--reduce-from (-insert-at it nil acc) column-index)
             (org-ml--table-pad-or-truncate (length rows))
             (-zip-with #'zip-into-rows rows))))
    (org-ml--map-children-nocheck #'map-rows table)))

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
      (org-ml--map-children-nocheck
        (lambda (cells) (org-ml--table-pad-or-truncate width cells))
        table-row))))

(defun org-ml--table-replace-row (row-index table-row table)
  "Return TABLE node with row at ROW-INDEX replaced by TABLE-ROW."
  (let ((table-row (org-ml--table-row-pad-maybe table table-row)))
    (org-ml--map-children-nocheck
      (lambda (rows) (org-ml--replace-at row-index table-row rows))
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
       ((--any? (org-ml-is-any-type org-ml-elements it) ss)
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

REPEATER and WARNING are lists formatted as (TYPE VALUE UNIT) where
the three members correspond to the :repeater/warning-type, -value,
and -unit properties in `org-ml-build-timestamp'.

Building a diary sexp timestamp is not possible with this function."
  (->> (org-ml--build-blank-node 'timestamp)
       (org-ml-set-property :post-blank (or post-blank 0))
       (org-ml--timestamp-set-start-time-nocheck start)
       (org-ml--timestamp-set-end-time-nocheck end)
       (org-ml--timestamp-set-active active)
       (org-ml--timestamp-set-warning warning)
       (org-ml--timestamp-set-repeater repeater)))

(org-ml--defun-kw org-ml-build-clock! (start &key end post-blank)
  "Return a new clock node.

START and END follow the same rules as their respective arguments in
`org-ml-build-timestamp!'."
  (let ((ts (->> (org-ml-build-timestamp! start :end end)
                 (org-ml--timestamp-set-type-ranged (not (null end))))))
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
   :closed (org-ml--planning-list-to-timestamp closed)
   :deadline (org-ml--planning-list-to-timestamp deadline)
   :scheduled (org-ml--planning-list-to-timestamp scheduled)
   :post-blank post-blank))

;; TODO check keyvals somehow
(org-ml--defun-kw org-ml-build-property-drawer! (&key post-blank &rest keyvals)
  "Return a new property-drawer node.

Each member in KEYVALS is a list of symbols like (KEY VAL), where each
list will generate a node-property node in the property-drawer node
like \":key: val\"."
  (->> keyvals
       (--map (let ((key (symbol-name (car it)))
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
         (section (-some->>
                   (append `(,planning) section-children)
                   (-non-nil)
                   (apply #'org-ml-build-section)))
         (nodes (->> subheadlines
                     (--map (org-ml--headline-set-level (1+ level) it))
                     (append (list section))
                     (-non-nil))))
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
  (->> (--map (org-ml-build-table-row! it) row-lists)
       (apply #'org-ml-build-table :tblfm tblfm :post-blank post-blank)))

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
  (cl-flet
      ((replace-note
        (old-p rep note)
        (if (not rep) note
          (let* ((type (if old-p "old" "new"))
                 (fun
                  (cond
                   ((org-ml-is-type 'timestamp rep)
                    (intern (format "org-ml--log-replace-%s-timestamp" type)))
                   ((stringp rep)
                    (intern (format "org-ml--log-replace-%s-state" type)))
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
  (-let (((&plist :begin :end) (org-ml--get-all-properties node)))
    (if (and (integerp begin) (integerp end))
        (<= begin point end)
      (error "Node boundaries are not defined"))))

(defun org-ml-set-property (prop value node)
  "Return NODE with PROP set to VALUE.

See builder functions for a list of properties and their rules for
each type."
  (let ((type (org-ml-get-type node)))
    (-if-let (pred (org-ml--get-property-attribute :pred type prop))
        (if (funcall pred value)
            (let* ((encode-fun (org-ml--get-property-encoder type prop))
                   (update-fun (org-ml--get-property-cis-function type prop)))
              (-->
               (if encode-fun (funcall encode-fun value) value)
               (org-ml--set-property-nocheck prop it node)
               (if update-fun (funcall update-fun it) it)))
          (org-ml--arg-error
           "Property '%s' in node of type '%s' must be %s. Got '%S'"
           prop type (org-ml--get-property-type-desc type prop) value))
      (org-ml--arg-error "Property '%s' is unsettable for type '%s'"
                     prop type))))

(defun org-ml-set-properties (plist node)
  "Return NODE with all properties set to the values according to PLIST.

PLIST is a list of property-value pairs that corresponds to the
property list in NODE.

See builder functions for a list of properties and their rules for
each type."
  (cl-flet
      ((filter
        (acc keyval type)
        (-let* (((prop value) keyval))
          ;; TODO this function doesn't smell DRY
          (-if-let (pred (org-ml--get-property-attribute :pred type prop))
              (if (funcall pred value)
                  (let ((encode-fun (org-ml--get-property-encoder type prop)))
                    (->> (if encode-fun (funcall encode-fun value) value)
                         (funcall #'plist-put acc prop)))
                (org-ml--arg-error
                 "Property '%s' in node of type '%s' must be %s. Got '%S'"
                 prop type (org-ml--get-property-type-desc type prop) value))
            (org-ml--arg-error
             "Property '%s' is unsettable for type '%s'"
             prop type)))))
    (if (org-ml--is-plist plist)
        (let* ((cur-props (org-ml--get-all-properties node))
               (type (org-ml-get-type node))
               (keyvals (-partition 2 plist))
               (update-funs
                (->> (-map #'car keyvals)
                     (--map (org-ml--get-property-cis-function type it))
                     (-uniq)
                     (-non-nil)))
               (node*
                (org-ml--construct
                 (org-ml-get-type node)
                 (--reduce-from (filter acc it type) cur-props keyvals)
                 (org-ml-get-children node))))
          (if (not update-funs) node*
            (--reduce-from (funcall it acc) node* update-funs)))
      (org-ml--arg-error "Not a plist: %S" plist))))

;; TODO add plural version of this...
(defun org-ml-get-property (prop node)
  "Return the value of PROP of NODE."
  (let ((decoder-fun (-> (org-ml-get-type node)
                         (org-ml--get-property-decoder prop)))
        (value (org-ml--get-property-nocheck prop node)))
    (if decoder-fun (funcall decoder-fun value) value)))

(org-ml--defun* org-ml-map-property (prop fun node)
  "Return NODE with FUN applied to the value of PROP.

FUN is a unary function which takes the current value of PROP and
returns a new value to which PROP will be set.

See builder functions for a list of properties and their rules for
each type."
  (--> (org-ml-get-property prop node)
       (funcall fun it)
       (org-ml-set-property prop it node)))

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
  (cl-flet
      ((insert-at-maybe
        (string-list)
        (if (member string string-list) string-list
          (org-ml--insert-at index string string-list))))
    (let ((type (org-ml-get-type node)))
      (if (org-ml--get-property-attribute :string-list type prop)
          (org-ml-map-property prop #'insert-at-maybe node)
        (org-ml--arg-error "Property '%s' in node of type '%s' is not a string-list"
                       prop type)))))

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
  (let ((type (org-ml-get-type node)))
    (if (org-ml--get-property-attribute :plist type prop)
        (org-ml-map-property* prop (plist-put it key value) node)
      (org-ml--arg-error "Not a plist property"))))

(defun org-ml-plist-remove-property (prop key node)
  "Return NODE with KEY and its corresponding value removed from PROP.

KEY is a keyword. This only applies to properties that are
represented as plists.

See `org-ml-plist-put-property' for a list of supported elements
and properties that may be used with this function."
  (let ((type (org-ml-get-type node)))
    (if (org-ml--get-property-attribute :plist type prop)
        (org-ml-map-property* prop (org-ml--plist-remove key it) node)
      (org-ml--arg-error "Not a plist property"))))

;; update polymorphic property function documentation

;; TODO these docstrings suck :(
(defun org-ml--get-types-with-property-attribute (attr)
  "Return alist of all nodes types that contain ATTR."
  (->> org-ml--property-alist
       (--map (cons (car it) (--filter (plist-get (cdr it) attr) (cdr it))))
       (-filter #'cdr)))

(defun org-ml--format-alist-operations (type-alist)
  "Return a formatted string of TYPE-ALIST."
  (->> type-alist
       (--map (cons (car it) (-map #'car (cdr it))))
       (--map (format "\n%s\n%s"
                      (car it)
                      (s-join "\n" (--map (format "- %S" it) (cdr it)))))
       (s-join "\n")))

(defun org-ml--append-documentation (fun string)
  "Append STRING to the docstring of FUN."
  (let ((msg "\n\nThe following types and properties are supported:\n"))
    (--> (documentation fun)
         (concat it msg string)
         (function-put fun 'function-documentation it))))

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
         (-drop 1)
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
  (and (org-ml--timestamp-is-ranged timestamp)
       (org-ml--timestamp-get-end-time timestamp)))

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

;; timestamp (diary)

(defun org-ml-timestamp-diary-set-value (form timestamp-diary)
  "Return TIMESTAMP-DIARY node with value set to FORM.
The node must have a type `eq' to `diary'. FORM is a quoted list."
  (if (listp form)
      (org-ml--set-property-nocheck :raw-value (format "<%%%%%S>" form)
                                timestamp-diary)
    (org-ml--arg-error "Timestamp-diary node value must be a form: Got %S"
                   form)))

;;; element nodes
;;
;; clock

;; TODO add setters here since the timestamps follow a restricted pattern

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
  (cl-case (org-ml-get-property :checkbox item)
    ((or trans nil) item)
    ('on (org-ml-set-property :checkbox 'off item))
    ('off (org-ml-set-property :checkbox 'on item))
    (t (error "This should not happen"))))

;; planning

(defun org-ml-planning-set-timestamp! (prop planning-list planning)
  "Return PLANNING node with PROP set to PLANNING-LIST.

PROP is one of `:closed', `:deadline', or `:scheduled'. PLANNING-LIST
is the same as that described in `org-ml-build-planning!'."
  (unless (memq prop '(:closed :deadline :scheduled))
    (org-ml--arg-error "PROP must be ':closed', ':deadline', or ':scheduled'. Got %S" prop))
  (let ((ts (org-ml--planning-list-to-timestamp planning-list)))
    (org-ml-set-property prop ts planning)))

;; affiliated keywords

(defconst org-ml--element-nodes-with-affiliated
  (eval-when-compile
    (-difference org-ml-elements
                 '(org-data comment clock headline inlinetask item
                            node-property planning property-drawer
                            section table-row))))

(defun org-ml-get-affiliated-keyword (key node)
  "Get the value of affiliated keyword KEY in NODE.

See `org-ml-set-affiliated-keyword' for the meaning of KEY."
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
corresponding to multiple entries of the attribute."
  (unless (org-ml-is-any-type org-ml--element-nodes-with-affiliated node)
    (org-ml--arg-error
     "Node type '%s' does not allow affiliated keywords"
     (org-ml-get-type node)))
  (let ((props
         (if value
             (plist-put (org-ml--get-all-properties node) key value)
           (org-ml--plist-remove key (org-ml--get-all-properties node)))))
    (org-ml--construct (org-ml-get-type node) props (org-ml-get-children node))))

(org-ml--defun* org-ml-map-affiliated-keyword (key fun node)
  "Apply FUN to value of affiliated keyword KEY in NODE.

See `org-ml-set-affiliated-keyword' for the meaning of KEY."
  (-some--> (org-ml-get-affiliated-keyword key node)
            (funcall fun it)
            (org-ml-set-affiliated-keyword key it node)))

(defun org-ml-set-caption! (caption node)
  "Set the caption affiliated keyword of NODE.

CAPTION can be one of the following:
- STRING: produces #+CAPTION: `STRING'
- (STRING1 STRING2): produces #+CAPTION[`STRING2']: `STRING1'
- ((STRING1 STRING2) ...): like above but makes multiple
  caption entries
- nil: removes all captions"
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


;;; PUBLIC BRANCH/CHILD FUNCTIONS

;;; polymorphic

(defun org-ml-children-contain-point (point branch-node)
  "Return t if POINT is within the boundaries of BRANCH-NODE's children."
  (-let (((&plist :contents-begin :contents-end)
          (org-ml--get-all-properties branch-node)))
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
            (org-ml--set-childen-throw-error type child-types illegal)
          (org-ml--set-children-nocheck children branch-node))
      ;; this should not happen
      (error "Child type restrictions not found for %s" type))))

(org-ml--defun* org-ml-map-children (fun branch-node)
  "Return BRANCH-NODE with FUN applied to its children.
FUN is a unary function that takes the current list of children and
returns a modified list of children."
  (--> (org-ml-get-children branch-node)
       (funcall fun it)
       (org-ml-set-children it branch-node)))

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
    `(->> (--mapcat ,form ,secondary-string)
          (org-ml--normalize-secondary-string))))

(defun org-ml-unwrap (object-node)
  "Return the children of OBJECT-NODE as a secondary string.
If OBJECT-NODE is a plain-text node, wrap it in a list and return.
Else add the post-blank property of OBJECT-NODE to the last member
of its children and return children as a secondary string."
  (if (org-ml-is-type 'plain-text object-node)
      (list object-node)
    (let ((children (org-ml-get-children object-node))
          (post-blank (org-ml--get-property-nocheck :post-blank object-node)))
      (org-ml--map-last* (org-ml-map-property* :post-blank
                       (+ it post-blank) it)
        children))))

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
    (let* ((children (org-ml-get-children object-node))
           (post-blank (org-ml--get-property-nocheck :post-blank object-node)))
      (->> children
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

;;; headline

(defun org-ml-headline-get-section (headline)
  "Return children of section node in HEADLINE node or nil if none."
  (-some->> (org-ml-get-children headline)
            (assoc 'section)
            (org-ml-get-children)))

(defun org-ml-headline-set-section (children headline)
  "Return HEADLINE with section node containing CHILDREN.
If CHILDREN is nil, return HEADLINE with no section node."
  (org-ml--map-children-nocheck
    (lambda (cur-children)
      (let ((subheadlines (--filter (org-ml-is-type 'headline it) cur-children)))
        (if children
            (cons (apply #'org-ml-build-section children) subheadlines)
          subheadlines)))
    headline))

(org-ml--defun* org-ml-headline-map-section (fun headline)
  "Return HEADLINE node with child section node modified by FUN.

FUN is a unary function that takes a section node's children as a list
returns a modified child list."
  (--> (org-ml-headline-get-section headline)
       (funcall fun it)
       (org-ml-headline-set-section it headline)))

(defun org-ml-headline-get-subheadlines (headline)
  "Return list of child headline nodes in HEADLINE node or nil if none."
  (-some->> (org-ml-get-children headline)
            (--filter (org-ml-is-type 'headline it))))

(defun org-ml-headline-set-subheadlines (subheadlines headline)
  "Return HEADLINE node with SUBHEADLINES set to child subheadlines."
  (org-ml--map-children-nocheck
    (lambda (hl-children)
      (-if-let (section (assoc 'section hl-children))
          (cons section subheadlines)
        subheadlines))
    headline))

(org-ml--defun* org-ml-headline-map-subheadlines (fun headline)
  "Return HEADLINE node with child headline nodes modified by FUN.

FUN is a unary function that takes a list of headlines and returns
a modified list of headlines."
  (--> (org-ml-headline-get-subheadlines headline)
       (funcall fun it)
       (org-ml-headline-set-subheadlines it headline)))

;;; headline (metadata)

;; planning

(defun org-ml-headline-get-planning (headline)
  "Return the planning node in HEADLINE or nil if none."
  (-some--> (org-ml-headline-get-section headline)
            (car it)
            (when (org-ml-is-type 'planning it) it)))

(defun org-ml-headline-set-planning (planning headline)
  "Return HEADLINE node with planning components set to PLANNING node."
  (if planning
      (org-ml-headline-map-section*
        ;; if no section, build new section with planning in it
        (if (not it) (list planning)
          ;; if section, test if planning already in front and override
          ;; as needed
          (let ((r (if (org-ml-is-type 'planning (car it)) (cdr it) it)))
            (cons planning r)))
        headline)
    ;; if `PLANNING' is nil, remove planning from section if present
    (org-ml-headline-map-section*
      (--remove-first (org-ml-is-type 'planning it) it)
      headline)))

(org-ml--defun* org-ml-headline-map-planning (fun headline)
  "Return HEADLINE node with planning node modified by FUN.

FUN is a unary function that takes a planning node and returns a
modified planning node."
   (--> (org-ml-headline-get-planning headline)
        (funcall fun it)
        (org-ml-headline-set-planning it headline)))

;; node-properties (eg the entire property drawer)

(defun org-ml-headline-get-node-properties (headline)
  "Return a list of node-properties nodes in HEADLINE or nil if none."
  (-some--> (org-ml-headline-get-section headline)
            ;; assume the property drawer is the first or second
            ;; child of section
            (if (org-ml-is-type 'planning (car it)) (cdr it) it)
            (car it)
            (when (org-ml-is-type 'property-drawer it)
              (org-ml-get-children it))))

(defun org-ml-headline-set-node-properties (node-properties headline)
  "Return HEADLINE node with property drawer containing NODE-PROPERTIES.
NODE-PROPERTIES is a list of node-property nodes."
  (if node-properties
      (org-ml-headline-map-section*
        (let ((pd (apply #'org-ml-build-property-drawer node-properties)))
          ;; if no section, build new section with prop-drwr in it
          (if (not it) (list pd)
            ;; the prop-drwr could either be the first child or second
            ;; if planning is in front
            (let ((first (nth 0 it))
                  (second (nth 1 it)))
              (cond
               ((and (org-ml-is-type 'planning first)
                     (org-ml-is-type 'property-drawer second))
                (-replace-at 1 pd it))
               ((org-ml-is-type 'property-drawer first)
                (-replace-at 0 pd it))
               ((org-ml-is-type 'planning first)
                (-insert-at 1 pd it))
               (t
                (cons pd it))))))
        headline)
    ;; if `NODE-PROPERTIES' is nil, remove from section if present
    (org-ml-headline-map-section*
      (--remove-first (org-ml-is-type 'property-drawer it) it)
      headline)))

(org-ml--defun* org-ml-headline-map-node-properties (fun headline)
  "Return HEADLINE node with property-drawer node modified by FUN.

FUN is a unary function that takes a property-drawer node and returns
a modified property-drawer node."
   (--> (org-ml-headline-get-node-properties headline)
        (funcall fun it)
        (org-ml-headline-set-node-properties it headline)))

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
        (if (not it) (list np)
          ;; replace first np matching `KEY' or add to the front of
          ;; np's if not found
          (-if-let (i (--find-index (equal key (org-ml-get-property :key it)) it))
              (-replace-at i np it)
            (cons np it)))
      ;; remove first property matching `KEY' if `VALUE' is nil
      (--remove-first (equal key (org-ml-get-property :value it)) it))
    headline))

(org-ml--defun* org-ml-headline-map-node-property (key fun headline)
  "Return HEADLINE node with property value matching KEY modified by FUN.

FUN is a unary function that takes a node-property value and returns
a modified node-property value."
   (--> (org-ml-headline-get-node-property key headline)
        (funcall fun it)
        (org-ml-headline-set-node-property key it headline)))

;; logbook

(defun org-ml--node-is-drawer-with-name (drawer-name node)
  "Return t if NODE is a drawer with DRAWER-NAME."
  (and (org-ml-is-type 'drawer node)
       (equal drawer-name (org-ml-get-property :drawer-name node))))

(defun org-ml--node-is-drawer-with-names (drawer-names node)
  "Return t if NODE is a drawer with one of DRAWER-NAMES."
  (and (org-ml-is-type 'drawer node)
       (member (org-ml-get-property :drawer-name node) drawer-names)))

(defun org-ml--headline-logbook-node-has-blank (node)
  "Return t if a logbook item has a blank line in it."
  (cl-case (org-ml-get-type node)
    ((clock drawer)
     (and (< 0 (org-ml-get-property :post-blank node)) t))
    (plain-list
     (->> (org-ml-get-children node)
          (--any? (< 0 (org-ml-get-property :post-blank it)))))
    (t
     (error "Invalid type"))))

(defun org-ml--compile-logbook-loose-predicate (drawer-names)
  "Return a predicate function to match logbook nodes.
DRAWER-NAMES is a list of drawer names which may match the
drawers that might be part of the logbook."
  (let ((drawer-pred-form (-some--> drawer-names
                            (if (= 1 (length it))
                                `(org-ml--node-is-drawer-with-name ,(car it) it)
                              `(org-ml--node-is-drawer-with-names ',it it)))))
    (--> '(org-ml-is-any-type '(plain-list clock) it)
         (if drawer-pred-form `(or ,it ,drawer-pred-form) it)
         `(lambda (it) ,it))))

;; TODO this will return multiple drawers if they have the same name
(defun org-ml--headline-split-logbook-and-contents (drawer-names children)
  "Return CHILDREN split by logbook and the remainder.
DRAWER-NAMES is a list of drawer names which may match the
drawers that might be part of the logbook."
  (cl-flet
      ((cons-maybe
        (a b)
        (if a (cons a b) b))
       (snoc-maybe
        (a b)
        (if b (-snoc a b) a)))
    (-let* ((pred (org-ml--compile-logbook-loose-predicate drawer-names))
            ((logbook (unknown . content))
             ;; split after last contiguous logbook nodes without blank lines
             (--split-with (and (funcall pred it)
                                (not (org-ml--headline-logbook-node-has-blank it)))
                           children)))
      ;; if unknown is not a valid logbook node, add it to the contents
      (if (not (funcall pred unknown))
          (list logbook (cons-maybe unknown content))
        ;; if unknown is a logbook node but not a plain-list, add to logbook
        (if (not (org-ml-is-type 'plain-list unknown))
            (list (snoc-maybe logbook unknown) content)
          ;; if unknown is a logbook node and a plain-list, split the plain list
          ;; before the first blank
          (-let ((unknown-items (org-ml-get-children unknown)))
            (-if-let (i (--find-index (< 0 (org-ml-get-property :post-blank it))
                                      unknown-items))
                (-let* (((logbook-items content-items) (-split-at (1+ i) unknown-items))
                        (logbook-plain-list (-some->> logbook-items
                                              (apply #'org-ml-build-plain-list)))
                        (content-plain-list (-some->> content-items
                                              (apply #'org-ml-build-plain-list)))
                        (logbook* (snoc-maybe logbook logbook-plain-list))
                        (content* (cons-maybe content-plain-list content)))
                  (list logbook* content*))
              (list logbook (cons-maybe unknown content)))))))))

(defun org-ml--drop-node-type (type children)
  "Return cdr of CHILDREN if car is TYPE or return unchanged."
  (if (org-ml-is-type type (car children)) (cdr children) children))

(defun org-ml-headline-get-logbook-loose (log-into-drawer clock-into-drawer headline)
  "Return loose logbook nodes of HEADLINE as a list.

\"Loose entries\" will be defined here as logbook entries that
are not in a drawer according to the variables
`org-log-into-drawer' and `org-clock-into-drawer'. Org-mode does
not define an exact specification for what separates \"the
logbook\" from the rest of the headline, therefore this function
will make several (possibly error-prone) assumptions:
- the logbook always starts at the beginning of a headline after
  the planning and property drawers if they exist.
- the logbook ends when a node that is not a plain-list, clock,
  or drawer named according to `org-log-into-drawer' or
  `org-clock-into-drawer' is encountered, or when a blank line is
  encountered (whichever occurs early in the buffer)

LOG-INTO-DRAWER and CLOCK-INTO-DRAWER are variables corresponding
to `org-log-into-drawer' and `org-clock-into-drawer' and may be
strings or nil (this function is totally stateless and thus does
not reference `org-log-into-drawer' or `org-clock-into-drawer').
Note that this function only uses these parameters to determine
the boundaries of the logbook and does not actually return the
contents of the named drawers; use
`org-ml-headline-get-logbook-drawer' for these."
  (cl-flet
      ((filter-drawers-maybe
        (drawer-names children)
        (if (not drawer-names) children
          (--remove (org-ml--node-is-drawer-with-names drawer-names it) children))))
    (let ((drawer-names (-uniq (-non-nil (list clock-into-drawer log-into-drawer)))))
      (-some->> (org-ml-headline-get-section headline)
        (org-ml--drop-node-type 'planning)
        (org-ml--drop-node-type 'property-drawer)
        (org-ml--headline-split-logbook-and-contents drawer-names)
        (car)
        (filter-drawers-maybe drawer-names)))))

(defun org-ml-headline-get-contents (log-into-drawer clock-into-drawer headline)
  "Return all non-metadata nodes of HEADLINE as a list.

Non-metadata includes all nodes in HEADLINE's section that is not
a planning node, property-drawer node, or part of the logbook (eg
what would be returned by `org-ml-headline-get-logbook-loose'.

See `org-ml-headline-get-logbook-loose' for the meaning of
LOG-INTO-DRAWER and CLOCK-INTO-DRAWER. This function makes the
same assumptions as this reference."
  (let ((drawer-names (-uniq (-non-nil (list log-into-drawer clock-into-drawer)))))
    (-some->> (org-ml-headline-get-section headline)
      (org-ml--drop-node-type 'planning)
      (org-ml--drop-node-type 'property-drawer)
      (org-ml--headline-split-logbook-and-contents drawer-names)
      (cadr))))

(defun org-ml-headline-get-logbook-drawer (name other-name headline)
  "Return the children of the logbook drawer of HEADLINE.

NAME is the name of the drawer to be set (a string), and
OTHER-NAME is the name of the second logbook drawer (if
any) which may be nil or a string. If drawer with NAME appears
after the drawer with OTHER-NAME, OTHER-NAME must be given so
that both drawers are considered as part of the logbook."
  (let ((names (-uniq (-non-nil (list name other-name)))))
    (-some->> (org-ml-headline-get-section headline)
      (org-ml--drop-node-type 'planning)
      (org-ml--drop-node-type 'property-drawer)
      (org-ml--headline-split-logbook-and-contents names)
      (car)
      (--first (org-ml--node-is-drawer-with-name name it))
      (org-ml-get-children))))

(defun org-ml-headline-set-logbook-drawer (name other-name children headline)
  "Return HEADLINE with logbook drawer filled with CHILDREN.

NAME and OTHER-NAME have the same meaning as those in
`org-ml-headline-get-logbook-drawer'."
  (unless (--all? (org-ml-is-any-type '(plain-list clock) it) children)
    (org-ml--arg-error
     "Logbook must only contain clock or plain-list nodes. Got %s"
     children))
  (cl-labels
      ((set-logbook
        (lb children)
        (-if-let (i (--find-index (org-ml--node-is-drawer-with-name name it)
                                  children))
            (if lb (-replace-at i lb children) (-remove-at i children))
          (if lb (cons lb children) children)))
       (set-content
        (lb children)
        (-let (((l c) (-> (list name other-name)
                          (-non-nil)
                          (-uniq)
                          (org-ml--headline-split-logbook-and-contents children))))
          (append (set-logbook lb l) c))))
    (org-ml-headline-map-section*
      (let ((lb (-some->> children
                  (apply #'org-ml-build-drawer name))))
        (if (not it) (-some-> lb (list))
          (-let* (((all &as n0 . rest*) it)
                  ((n1-rest &as n1 . rest) rest*))
            (cond
             ((and (org-ml-is-type 'planning n0)
                   (org-ml-is-type 'property-drawer n1))
              (->> (set-content lb rest)
                   (cons n1)
                   (cons n0)))
             ((org-ml-is-any-type '(planning property-drawer) n0)
              (->> (set-content lb n1-rest)
                   (cons n0)))
             (t
              (set-content lb all))))))
      headline)))

(org-ml--defun* org-ml-headline-map-logbook-drawer (name other-name fun headline)
  "Return HEADLINE node with property value matching KEY modified by FUN.

FUN is a unary function that takes a list of child nodes from the
logbook value and returns a modified list of child nodes.

NAME and OTHER-NAME have the same meaning as those in
`org-ml-headline-get-logbook-drawer'."
  (declare (indent 2))
  (--> (org-ml-headline-get-logbook-drawer name other-name headline)
       (funcall fun it)
       (org-ml-headline-set-logbook-drawer name other-name it headline)))

(defun org-ml-headline-logbook-drawer-append-entry (name other-name item headline)
  "Return HEADLINE with ITEM node appended to the front of its logbook.

NAME and OTHER-NAME have the same meaning as those in
`org-ml-headline-get-logbook-drawer'."
  (org-ml-headline-map-logbook-drawer* name other-name
    ;; if logbook starts with a plain-list, add item to front of
    ;; said plain list
    (if (org-ml-is-type 'plain-list (car it))
        (org-ml--map-first* (org-ml-map-children* (cons item it) it) it)
      ;; else just append a new plain-list to the front
      (cons (org-ml-build-plain-list item) it))
    headline))

(defun org-ml-headline-logbook-drawer-append-open-clock (name other-name unixtime headline)
  "Return HEADLINE with an open clock append to front of its logbook.
UNIXTIME is an integer that will be used to build the clock node.

This does the functional equivalent of `org-clock-in' on the logbook.

NAME and OTHER-NAME have the same meaning as those in
`org-ml-headline-get-logbook-drawer'."
  (org-ml-headline-map-logbook-drawer* name other-name
    (-> (org-ml-unixtime-to-time-long unixtime)
        (org-ml-build-clock!)
        (cons it))
    headline))

(defun org-ml-headline-logbook-drawer-close-open-clock (name other-name unixtime note headline)
  "Return HEADLINE with the first clock closed.

The clock will be closed to UNIXTIME, and NOTE will be appended
as a clock out note if supplied (as string). If no open clocks
are found, return HEADLINE unmodified.

This does the functional equivalent of `org-clock-out' on the logbook.

NAME and OTHER-NAME have the same meaning as those in
`org-ml-headline-get-logbook-drawer'."
  (cl-flet
      ((close-clock
        (index logbook-children)
        (let ((time (org-ml-unixtime-to-time-long unixtime)))
          (org-ml--map-at* index
            (org-ml-map-property* :value
              (->> (org-ml-timestamp-set-end-time time it)
                   (org-ml--timestamp-set-type-ranged t))
              it)
            logbook-children)))
       (add-note-maybe
        (index logbook-children)
        (if (not note) logbook-children
          (let* ((next (1+ index))
                 (target (nth next logbook-children))
                 (item (->> (org-ml-build-paragraph note)
                            (org-ml-build-item))))
            ;; if plain-list is after the clock being closed, add the
            ;; note to the front of the plain-list, otherwise insert
            ;; a new plain-list
            (if (org-ml-is-type 'plain-list target)
                (org-ml--map-at* next
                  (org-ml-map-children* (cons item it) it)
                  logbook-children)
              (-insert-at next (org-ml-build-plain-list item)
                          logbook-children))))))
    (org-ml-headline-map-logbook-drawer* name other-name
      (-if-let (i (--find-index (and (org-ml-is-type 'clock it)
                                     (org-ml-clock-is-running it))
                                it))
          (->> it (close-clock i) (add-note-maybe i))
        it)
      headline)))

;; misc

(defun org-ml-headline-get-path (headline)
  "Return tree path of HEADLINE node.

The return value is a list of headline titles (including that from
HEADLINE) leading to the root node."
  (->> (org-ml-get-parents headline)
       (--map (org-ml-get-property :raw-value it))))

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
    (org-ml--map-children-nocheck
      (lambda (items)
        (--map (org-ml-set-property :bullet '- it) items))
      plain-list))
   ((eq type 'ordered)
    ;; NOTE the org-interpreter seems to use the correct, ordered numbers if any
    ;; number is set here. This behavior may not be reliable.
    (org-ml--map-children-nocheck
      (lambda (items)
        (--map (org-ml-set-property :bullet 1 it) items))
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
  (org-ml--map-children-nocheck
    (lambda (rows) (org-ml--remove-at row-index rows))
    table))

(defun org-ml-table-delete-column (column-index table)
  "Return TABLE node with column at COLUMN-INDEX deleted."
  (cl-flet*
      ((delete-cell
        (cells)
        (org-ml--remove-at column-index cells))
       (map-row
        (row)
        (if (org-ml--property-is-eq :type 'rule row) row
          (org-ml--map-children-nocheck #'delete-cell row))))
    (org-ml--map-children-nocheck (lambda (rows) (-map #'map-row rows)) table)))

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
      (org-ml--map-children-nocheck
        (lambda (rows) (org-ml--insert-at row-index row rows))
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
                   (org-ml--map-children-nocheck
                     (lambda (cells)
                       (org-ml--replace-at column-index cell cells))))))
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
;; 2. indent 1. and 1.1 along with it
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

;; TODO throw error when index out of range?

(defun org-ml--indent-members (fun index tree)
  "Return TREE with member at INDEX indented.
FUN is a binary function that takes the members of TREE immediately
before INDEX (called 'head') and the item at INDEX to be indented
\(called 'target'). It maps over the last item of 'head' and sets the
target as its child, or appends it to the end of its children if they
exist."
  (unless (and (integerp index) (< 0 index))
    (error "Cannot indent topmost item at this level"))
  (-let* (((head tail) (-split-at index tree))
          (target (-first-item tail))
          (head* (org-ml--map-last* (funcall fun target it) head)))
    (append head* (-drop 1 tail))))

;; headline

(defun org-ml-headline-indent-subtree (index headline)
  "Return HEADLINE node with child headline at INDEX indented.
Unlike `org-ml-headline-indent-subheadline' this will also indent the
indented headline node's children."
  (cl-flet
      ((append-indented
        (target-headline parent-headline)
        (let ((target-headline*
               (org-ml--headline-subtree-shift-level 1 target-headline)))
          (org-ml--map-children-nocheck
           (lambda (headline-children)
             (append headline-children (list target-headline*)))
           parent-headline))))
    (org-ml-headline-map-subheadlines
     (lambda (subheadlines)
       (org-ml--indent-members #'append-indented index subheadlines))
     headline)))

(defun org-ml-headline-indent-subheadline (index headline)
  "Return HEADLINE node with child headline at INDEX indented.
Unlike `org-ml-headline-indent-subtree' this will not indent the
indented headline node's children."
  (cl-flet
      ((append-indented
        (target-headline parent-headline)
        (let ((target-headline*
               (->> target-headline
                    (org-ml-headline-map-subheadlines #'ignore)
                    (org-ml--headline-shift-level 1)))
              (headlines-in-target
               (org-ml-headline-get-subheadlines target-headline)))
          (org-ml--map-children-nocheck
           (lambda (children)
             (append children (list target-headline*) headlines-in-target))
           parent-headline))))
    (org-ml-headline-map-subheadlines
     (lambda (subheadlines)
       (org-ml--indent-members #'append-indented index subheadlines))
     headline)))

;; plain-list

(defun org-ml-plain-list-indent-item-tree (index plain-list)
  "Return PLAIN-LIST node with child item at INDEX indented.
Unlike `org-ml-item-indent-item' this will also indent the indented item
node's children."
  (cl-flet
      ((append-indented
        (target-item parent-item)
        (let ((target-item* (org-ml-build-plain-list target-item)))
          (org-ml--map-children-nocheck
           (lambda (item-children) (append item-children (list target-item*)))
           parent-item))))
    (org-ml--map-children-nocheck
     (lambda (items)
       (org-ml--indent-members #'append-indented index items))
     plain-list)))

(defun org-ml-plain-list-indent-item (index plain-list)
  "Return PLAIN-LIST node with child item at INDEX indented.
Unlike `org-ml-item-indent-item-tree' this will not indent the indented
item node's children."
  (cl-flet
      ((append-indented
        (target-item parent-item)
        (let ((target-item*
               (->> target-item
                    (org-ml--map-children-nocheck
                      (lambda (items)
                        (--remove (org-ml-is-type 'plain-list it) items)))
                    (org-ml-build-plain-list)))
              (items-in-target
               (->> (org-ml-get-children target-item)
                    (--filter (org-ml-is-type 'plain-list it)))))
          (org-ml--map-children-nocheck
           (lambda (item-children)
             ;; TODO technically the target-item* should go in an
             ;; existing plain list but I don't this matters (for now)
             (append item-children (list target-item*) items-in-target))
           parent-item))))
    (org-ml--map-children-nocheck
     (lambda (items)
       (org-ml--indent-members #'append-indented index items))
     plain-list)))

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

(defun org-ml--unindent-members (index trim-fun extract-fun tree)
  "Return TREE with children under INDEX unindented.
TRIM-FUN is a unary function that is applied to the child list
under INDEX and returns a modified child list with the unindented
members removed. EXTRACT-FUN is a unary function that is applied to
the child list under INDEX and returns the unindented children that
will be spliced after INDEX."
  (-let* (((head tail) (-split-at index tree))
          (parent (-first-item tail))
          (parent* (funcall trim-fun parent))
          (unindented (funcall extract-fun parent)))
    (append head (list parent*) unindented (-drop 1 tail))))

;; headline

(defun org-ml-headline-unindent-all-subheadlines (index headline)
  "Return HEADLINE node with all child headlines under INDEX unindented."
  (cl-flet
      ((trim
        (parent)
        (org-ml-headline-map-subheadlines #'ignore parent))
       (extract
        (parent)
        (->> (org-ml-get-children parent)
             (--map (org-ml--headline-subtree-shift-level -1 it)))))
    (org-ml-headline-map-subheadlines
     (lambda (subheadlines)
       (org-ml--unindent-members index #'trim #'extract subheadlines))
     headline)))

;; plain-list

(defun org-ml-plain-list-unindent-all-items (index plain-list)
  "Return PLAIN-LIST node with all child items under INDEX unindented."
  (cl-flet
      ((trim
        (parent)
        (org-ml--map-children-nocheck
         (lambda (children)
           (--remove-first (org-ml-is-type 'plain-list it) children))
         parent))
       (extract
        (parent)
        (->> (org-ml-get-children parent)
             (--first (org-ml-is-type 'plain-list it))
             (org-ml-get-children))))
    (org-ml--map-children-nocheck
     (lambda (items)
       (org-ml--unindent-members index #'trim #'extract items))
     plain-list)))

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

;; TODO this is a bit sketchy...it depends on the indentation function to make
;; the children list one element shorter, which is usually true but makes a
;; really hard error to catch when it fails
(defun org-ml--indent-after (indent-fun index node)
  "Return NODE with INDENT-FUN applied to all child nodes after INDEX."
  (if (< index (1- (length (org-ml-get-children node))))
      (->> (funcall indent-fun (1+ index) node)
           (org-ml--indent-after indent-fun index))
    node))

;; headline

(defun org-ml-headline-unindent-subheadline (index child-index headline)
  "Return HEADLINE node with a child headline under INDEX unindented.
The specific child headline to unindent is selected by CHILD-INDEX."
  (cl-flet
      ((trim
        (parent)
        (org-ml-headline-map-subheadlines
         (lambda (subheadlines) (-take child-index subheadlines))
         parent))
       (extract
        (parent)
        (->> (org-ml--indent-after #'org-ml-headline-indent-subtree
                                    child-index parent)
             (org-ml-get-children)
             (-drop child-index)
             (--map (org-ml--headline-subtree-shift-level -1 it)))))
    (org-ml-headline-map-subheadlines
     (lambda (subheadlines)
       (org-ml--unindent-members index #'trim #'extract subheadlines))
     headline)))

;; plain-list

(defun org-ml-plain-list-unindent-item (index child-index plain-list)
  "Return PLAIN-LIST node with a child item under INDEX unindented.
The specific child item to unindent is selected by CHILD-INDEX."
  (cl-flet
      ((trim
        (parent)
        (org-ml--map-children-nocheck
         (lambda (children)
           (if (= 0 index)
               (--remove-first (org-ml-is-type 'plain-list it) children)
             (--map-first (org-ml-is-type 'plain-list it)
                          (org-ml--map-children-nocheck
                           (lambda (items) (-take child-index items)) it)
                          children)))
         parent))
       (extract
        (parent)
        (->>
         (org-ml-get-children parent)
         (--first (org-ml-is-type 'plain-list it))
         (org-ml--indent-after #'org-ml-plain-list-indent-item-tree
                                child-index)
         (org-ml-get-children)
         (-drop child-index))))
    (org-ml--map-children-nocheck
     (lambda (items)
       (org-ml--unindent-members index #'trim #'extract items))
     plain-list)))

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

(defconst org-ml--rm-if-empty
  '(table plain-list bold italic radio-target strike-through
          superscript subscript table-cell underline)
  "Nodes that will be blank if printed and empty.
This is a workaround for a bug")

(defconst org-ml--blank-if-empty
  '(center-block drawer dynamic-block property-drawer quote-block
                 special-block verse-block)
  "Branch element nodes that require \"\" to correctly print empty.
This is a workaround for a bug.")

(defun org-ml--filter-non-zero-length (node)
  "Return NODE if it is not an empty node type from `org-ml--rm-if-empty'.
The exception is rule-typed table-row nodes which are supposed to be
empty."
  (unless (and (org-ml-is-childless node)
               (or (org-ml-is-any-type org-ml--rm-if-empty node)
                   (org-ml--is-table-row node)))
    node))

(defun org-ml--clean (node)
  "Return NODE with empty child nodes from `org-ml--rm-if-empty' removed."
  (->> (org-ml--map-children-nocheck
         (lambda (children)
           (-non-nil (-map #'org-ml--clean children)))
         node)
       (org-ml--filter-non-zero-length)))

(defun org-ml--blank (node)
  "Return NODE with empty child nodes `org-ml--blank-if-empty' set to contain \"\"."
  (if (org-ml-is-childless node)
      (if (org-ml-is-any-type org-ml--blank-if-empty node)
          (org-ml--set-blank-children node)
        node)
    (org-ml--map-children-nocheck
      (lambda (children)
        (-map #'org-ml--blank children))
      node)))

;;; print functions

(defun org-ml-to-string (node)
  "Return NODE as an interpreted string without text properties."
  (->> node
       ;; Some objects and greater elements should be removed if blank. Table
       ;; and plain list will error, and the others make no sense if they are
       ;; empty.
       (org-ml--clean)
       ;; Some greater elements will print "nil" in their children if they are
       ;; empty. The workaround for this is to set the children to a single
       ;; blank string if empty
       (org-ml--blank)
       (org-element-interpret-data)
       (substring-no-properties)))

(defun org-ml-to-trimmed-string (node)
  "Like `org-ml-to-string' but strip whitespace when returning NODE."
  (-some->> (org-ml-to-string node) (s-trim)))

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

(defun org-ml--get-children-indexed (node)
  "Return list of children from NODE (if any) with index annotations."
  (let* ((children (org-ml-get-children node))
         (len (- (length children))))
    (--map-indexed (cons `(,it-index . ,(+ len it-index)) it) children)))

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
         (get-children
          (if (not end?) '(org-ml--get-children-indexed (cdr it))
            '(reverse (org-ml--get-children-indexed (cdr it)))))
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
    (if end? body `(reverse ,body))))

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

(defun org-ml--match-make-lambda-form (pattern)
  "Return callable lambda form for PATTERN.
NODE is the node to be matched."
  (let ((body (org-ml--match-make-slicer-form pattern)))
    `(lambda (it) (let ((it (cons nil it)) (acc)) ,body))))

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
be (SUB *), (SUB ?), and ((nil | SUB))."
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
    `(cl-labels
         ((rec
           (node)
           (if (not (org-ml-is-branch-node node)) node
             (org-ml-map-children*
               (->> (--map (rec it) it)
                    (funcall (lambda (it) ,form)))
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

(org-ml--defun* org-ml-match-map (pattern fun node)
  "Return NODE with FUN applied to children matching PATTERN.
FUN is a unary function that takes a node and returns a new node
which will replace the original.

PATTERN follows the same rules as `org-ml-match'."
  (-if-let (targets (org-ml-match pattern node))
      (org-ml--modify-children node
        (--map-when (member it targets) (funcall fun it) it))
    node))

;;; mapcat

(org-ml--defun* org-ml-match-mapcat (pattern fun node)
  "Return NODE with FUN applied to children matching PATTERN.
FUN is a unary function that takes a node and returns a list of new
nodes which will be spliced in place of the original node.

PATTERN follows the same rules as `org-ml-match'."
  (-if-let (targets (org-ml-match pattern node))
      (org-ml--modify-children node
        (--mapcat (if (member it targets)
                      (funcall fun it) (list it))
                  it))
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

(org-ml--defun* org-ml-match-do (pattern fun node)
  "Like `org-ml-match-map' but for side effects only.
FUN is a unary function that has side effects and is applied to the
matches from NODE using PATTERN. This function itself returns nil.

PATTERN follows the same rules as `org-ml-match'."
  (-when-let (targets (org-ml-match pattern node))
      (--each targets (funcall fun it))))

;;; BUFFER PARSING

;;; parse at specific point

;; TODO add test for plain-text parsing
(defun org-ml-parse-object-at (point)
  "Return object node under POINT or nil if not on an object."
  (save-excursion
    (goto-char point)
    (-let* ((context (org-element-context))
            ((offset nesting) (cl-case (org-ml-get-type context)
                                ((superscript subscript) '(-1 (0 1)))
                                (table-cell '(-1 (0 0 0)))
                                (t '(0 (0 0)))))
            ((&plist :begin :end) (org-ml--get-all-properties context))
            (tree (org-element--parse-elements
                   (+ begin offset) end 'first-section nil nil nil nil)))
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
    (let*
        ((node (org-element-at-point))
         (node-type (org-ml-get-type node)))
      ;; NOTE this will not filter by type if it is a leaf node
      (if (not (memq node-type org-ml-branch-nodes)) node
        ;; need to parse again if branch-node since
        ;; `org-element-at-point' does not parse children
        (-let* (((&plist :begin :end) (org-ml--get-all-properties node))
                (tree (car (org-element--parse-elements
                            begin end 'first-section nil nil nil nil)))
                (nesting (cl-case node-type
                           (headline nil)
                           ;; `org-element-at-point' will return a table if on
                           ;; the first row of a table, and a table-row
                           ;; otherwise
                           (table-row '(0 0))
                           (table (if (eq type 'table-row) '(0 0) '(0)))
                           (plain-list (if (eq type 'item) '(0 0) '(0)))
                           (t '(0)))))
          (--> (org-ml--get-descendent nesting tree)
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
                   (let ((orig-point (point)))
                     ;; this function won't move if we are on the last headline.
                     ;; Check if point has moved, and if not, return the max
                     ;; point
                     (org-forward-heading-same-level 1 t)
                     (if (= (point) orig-point)
                         (point-max) (point)))
                 (or (outline-next-heading) (point-max)))))
        (car (org-element--parse-elements b e 'first-section
                                          nil nil nil nil))))))

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
        (org-element--parse-elements
         (point-min) (or (outline-next-heading) (point-max))
         'first-section nil nil nil nil))))))

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
  (if (and (listp nodes) (-all? #'org-ml--is-node nodes))
      (s-join "" (-map #'org-ml-to-string nodes))
    (org-ml-to-string nodes)))

(defun org-ml-insert (point node)
  "Convert NODE to a string and insert at POINT in the current buffer.
NODE may be a node or a list of nodes. Return NODE."
  (save-excursion
    (goto-char point)
    (insert (org-ml--nodes-to-string-maybe node)))
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

(org-ml--defun* org-ml-update (fun node)
  "Replace NODE in the current buffer with a new one.
FUN is a unary function that takes NODE and returns a modified node
or list of nodes. This modified node is then written in place of the
old node in the current buffer."
  ;; if node is of type 'org-data' it will have no props
  (let* ((begin (org-ml--get-property-nocheck :begin node))
         (end (org-ml--get-property-nocheck :end node))
         (ov-cmd (->>
                  (overlays-in begin end)
                  (--filter (eq 'outline (overlay-get it 'invisible)))
                  (--map (list :start (overlay-start it)
                               :end (overlay-end it)
                               :props (overlay-properties it)))
                  (list 'apply 'org-ml--apply-overlays)))
         ;; do all computation before modifying buffer
         (node0 (org-ml-clone-node node))
         (node* (funcall fun node)))
    (unless (equal node0 node*)
      ;; hacky way to add overlays to undo tree
      (setq-local buffer-undo-list (cons ov-cmd buffer-undo-list))
      (delete-region begin end)
      (org-ml-insert begin node*)
      nil)))

;; generate all update functions for corresponding parse functions
;; since all take function args, also generate anaphoric forms
(eval-when-compile
  (defun org-ml--autodef-update-node-forms (name)
    "Return defun and defmacro forms for NAME."
    (let* ((update-at (intern (format "org-ml-update-%s-at" name)))
           (update-this (intern (format "org-ml-update-this-%s" name)))
           (update-at-doc
            (--> (list "Update %1$s under POINT using FUN."
                       "FUN takes an %1$s and returns a modified %1$s")
                 (s-join "\n" it)
                 (format it name)))
           (update-this-doc
            (--> (list "Update %1$s under current point using FUN."
                       "FUN takes an %1$s and returns a modified %1$s")
                 (s-join "\n" it)
                 (format it name)))
           (call (intern (format "org-ml-parse-%s-at" name)))
           (update-at-body `(org-ml-update fun (,call point)))
           (update-this-body `(,update-at (point) fun)))
      (list `(org-ml--defun* ,update-at (point fun)
               ,update-at-doc
               ,update-at-body)
            `(org-ml--defun* ,update-this (fun)
               ,update-this-doc
               ,update-this-body))))

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

;;; headline iteration

(defun org-ml--do-headlines-where (where fun-forward fun-backward
                                     fun-region)
  "Call functions depending on WHERE.
FUN-FORWARD is a function to be applied for indices in the forward
direction, FUN-BACKWARD is a function to be applied for indices in the
backward direction, and FUN-REGION is a function to be applied between
regions. All take two arguments (the bounds of the application)."
  (declare (indent 1))
  (cl-flet
      ((int-or-nil-p
        (x)
        (or (null x) (integerp x))))
    (pcase where
      ;; parse N
      ((and (pred integerp) n)
       (if (<= 0 n) (funcall fun-forward 0 n)
         (funcall fun-backward 0 (1- (- n)))))
      ;; parse M-N
      (`(,(and (pred integerp) m) ,(and (pred integerp) n))
       (cond
        ((<= 0 m n) (funcall fun-forward m n))
        ((<= m n -1) (funcall fun-backward (1- (- n)) (1- (- m))))
        ((< n m) (org-ml--arg-error "M must be less than or equal to N"))
        (t (org-ml--arg-error "M and N must be the same sign"))))
      ;; parse region between A and B
      (`[,(and (pred int-or-nil-p) a) ,(and (pred int-or-nil-p) b)]
       (let ((a (or a (point-min)))
             (b (or b (point-max))))
         (funcall fun-region a b)))
      (e (org-ml--arg-error "Invalid 'where' specification: Got %S" e)))))

(eval-when-compile
  (defmacro org-ml--apply-n (m n re backward? form)
    "Apply FORM to matching strings a buffer.
RE is a regular expression string, and FORM will be executed when
the point is over the M to N matches (inclusive). If BACKWARD? is
t, start searching backward from the end of the buffer. Note that
RE is assumed to match lines, and thus should begin with a
\"^\"."
    (declare (indent 4))
    (let ((start (if backward? '(point-max) '(point-min)))
          (iterate-form
           (if backward?
               `(save-match-data
                  (re-search-backward ,re nil t))
             `(save-match-data
                ;; avoid matching the current match already on one
                ;; NOTE this assumes that `RE' contains the beginning of the line
                (when (and (bolp) (not (eobp))) (forward-char 1))
                (when (re-search-forward ,re nil t)
                  (goto-char (match-beginning 0)))))))
      `(save-excursion
         (goto-char ,start)
         ;; iterate match(es) if we start on a match or can move to a match
         (when (or (looking-at ,re) ,iterate-form)
           (let ((i 0))
             ;; apply form to the first if we want it
             (when (= 0 ,m) ,form)
             (setq i (1+ i))
             ;; loop through the rest and apply form when appropriate
             (while (and ,iterate-form (<= i ,n))
               (when (<= ,m i) ,form)
               (setq i (1+ i))))))))

  (defmacro org-ml--apply-region (begin end re form)
    "Apply FORM to a region in a buffer.
RE is a regular expression string, and FORM will be execrated when the
point on any match between BEGIN and END points in the buffer."
    (declare (indent 3))
    (let ((iterate-form `(re-search-backward ,re nil t)))
      `(save-excursion
         (goto-char ,end)
         (when ,iterate-form
           ,form
           ;; loop through the rest
           (while (and ,iterate-form (<= ,begin (point)))
             ,form))))))

(defun org-ml-get-some-headlines (where)
  "Return list of headline nodes from current buffer.

WHERE describes the location of headlines to be parsed and is one
of the following:
- N: parse up to index N headlines (where 0 is the first); if negative
  start counting from the last headline (where -1 refers to the last)
- (M N): like N but parse after index M headlines; M and N may both
  be similarly negative
- [A B]: parse all headlines whose first point falls between points
  A and B in the buffer; if A and B are nil, use `point-min' and
  `point-max' respectively.

Each headline is obtained with `org-ml-parse-headline-at'."
  (cl-flet
      ((apply-n-forward
        (m n)
        (let ((acc))
          (org-ml--apply-n m n "^\\*" nil
            (setq acc (cons (org-ml-parse-this-headline) acc)))
          (nreverse acc)))
       (apply-n-backward
        (m n)
        (let ((acc))
          (org-ml--apply-n m n "^\\*" t
            (setq acc (cons (org-ml-parse-this-headline) acc)))
          acc))
       (apply-region
        (begin end)
        (let ((acc))
          (org-ml--apply-region begin end "^\\*"
            (setq acc (cons (org-ml-parse-this-headline) acc)))
          acc)))
    (org-ml--do-headlines-where where
      #'apply-n-forward
      #'apply-n-backward
      #'apply-region)))

(defun org-ml-get-headlines ()
  "Return list of all headline nodes from current buffer.
Each headline is obtained with `org-ml-parse-headline-at'."
  (org-ml-get-some-headlines [nil nil]))

(defun org-ml-get-some-subtrees (where)
  "Return list of subtree nodes from current buffer.

See `org-ml-get-some-headlines' for the meaning of WHERE.

Each subtree is obtained with `org-ml-parse-subtree-at'."
  (cl-flet
      ((apply-n-forward
        (m n)
        (let ((acc))
          (org-ml--apply-n m n "^\\* " nil
            (setq acc (cons (org-ml-parse-this-subtree) acc)))
          (nreverse acc)))
       (apply-n-backward
        (m n)
        (let ((acc))
          (org-ml--apply-n m n "^\\* " t
            (setq acc (cons (org-ml-parse-this-subtree) acc)))
          acc))
       (apply-region
        (begin end)
        (let ((acc))
          (org-ml--apply-region begin end "^\\* "
            (setq acc (cons (org-ml-parse-this-subtree) acc)))
          acc)))
    (org-ml--do-headlines-where where
      #'apply-n-forward
      #'apply-n-backward
      #'apply-region)))

(defun org-ml-get-subtrees ()
  "Return list of all subtree nodes from current buffer.

Each subtree is obtained with `org-ml-parse-subtree-at'."
  (org-ml-get-some-subtrees [nil nil]))

(org-ml--defun* org-ml-do-some-headlines (where fun)
  "Update some headlines in the current using FUN.

See `org-ml-get-some-headlines' for the meaning of WHERE.

Headlines are updated using `org-ml-update-this-headline' (see this for
use and meaning of FUN)."
  (cl-flet
      ((apply-n-forward
        (m n)
        (org-ml--apply-n m n "^\\*" nil
          (org-ml-update-this-headline fun)))
       (apply-n-backward
        (m n)
        (org-ml--apply-n m n "^\\*" t
          (org-ml-update-this-headline fun)))
       (apply-region
        (begin end)
        (org-ml--apply-region begin end "^\\*"
          (org-ml-update-this-headline fun))))
    (org-ml--do-headlines-where where
      #'apply-n-forward
      #'apply-n-backward
      #'apply-region)))

(org-ml--defun* org-ml-do-headlines (fun)
  "Update all headlines in the current buffer using FUN.

Headlines are updated using `org-ml-update-this-headline' (see this for
use and meaning of FUN)."
  (org-ml-do-some-headlines [nil nil] fun))

(org-ml--defun* org-ml-do-some-subtrees (where fun)
  "Update some toplevel subtrees in the current buffer using FUN.

See `org-ml-get-some-headlines' for the meaning of WHERE.

Subtrees are updated using `org-ml-update-this-subtree' (see this for use
and meaning of FUN)."
  (cl-flet
      ((apply-n-forward
        (m n)
        (org-ml--apply-n m n "^\\* " nil
          ;; (re-search-forward "^\\* " nil t)
          (org-ml-update-this-subtree fun)))
       (apply-n-backward
        (m n)
        (org-ml--apply-n m n "^\\* " t
          ;; (re-search-backward "^\\* " nil t)
          (org-ml-update-this-subtree fun)))
       (apply-region
        (begin end)
        (org-ml--apply-region begin end "^\\* "
          ;; (re-search-backward "^\\* " nil t)
          (org-ml-update-this-subtree fun))))
    (org-ml--do-headlines-where where
      #'apply-n-forward
      #'apply-n-backward
      #'apply-region)))

(org-ml--defun* org-ml-do-subtrees (fun)
  "Update all toplevel subtrees in the current buffer using FUN.

Subtrees are updated using `org-ml-update-this-subtree' (see this for use
and meaning of FUN)."
  (org-ml-do-some-subtrees [nil nil] fun))

(provide 'org-ml)
;;; org-ml.el ends here
