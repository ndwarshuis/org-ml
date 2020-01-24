;;; om.el --- Functional Org Mode API -*- lexical-binding: t; -*-

;; Copyright (C) 2019  Nathan Dwarshuis

;; Author: Nathan Dwarshuis <ndwar@yavin4.ch>
;; Keywords: org-mode, outlines
;; Homepage: https://github.com/ndwarshuis/om.el
;; Package-Requires: ((emacs "26.1") (dash "2.15") (s "1.12"))
;; Version: 1.2.0

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

;; This is a functional API for org-mode primarily using the
;; `org-element' library. `org-element.el' provides the means for
;; converting an org buffer to a parse-tree data structure. This
;; library contains functions to modify this parse-tree in a
;; more-or-less 'purely' functional manner (with the exception of
;; parsing from the buffer and writing back to the buffer). For the
;; purpose of this package, the resulting parse tree is composed of
;; 'nodes'

;; This library exposes the following types of functions:
;; - builder: build new nodes to be inserted into a parse tree
;; - property functions: return either property values (get) or
;;   nodes with modified properties (set and map)
;; - children functions: return either children of nodes (get) or
;;   return a node with modified children (set and map)
;; - node predicates: return t if node meets a condition
;; - pattern matching: return nodes based on a pattern that matches
;;   the parse tree (and perform operations on those nodes depending
;;   on the function)
;; - parsers: parse a buffer (optionally at current point) and return
;;   a parse tree
;; - writers: insert/update the contents of a buffer given a parse
;;   tree

;; For examples please see full documentation at:
;; https://github.com/ndwarshuis/om.el

;;; Code:

(require 'org-element)
(require 'dash)
(require 's)
(require 'om-macs)

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
(defconst om-elements
  (cons 'org-data org-element-all-elements)
  "List of all element types including 'org-data'.")

(defconst om-objects
  (cons 'plain-text org-element-all-objects)
  "List of all object types including 'plain-text'.")

(defconst om-nodes
  (append om-elements om-objects)
  "List of all node types.")

(defvaralias 'om-branch-nodes-permitting-child-objects
  'org-element-object-containers
  "List of node types that can have objects as children.
These are also known as \"object containers\" in `org-element.el'")

(defconst om-branch-elements-permitting-child-objects
  (-intersection om-branch-nodes-permitting-child-objects om-elements)
  "List of element types that can have objects as children.")

(defconst om-branch-elements-permitting-child-elements
  (cons 'org-data org-element-greater-elements)
  "List of element types that can have elements as children.
These are also known as \"greater elements\" in `org-element.el'")

(defconst om-branch-elements
  (append om-branch-elements-permitting-child-objects
          om-branch-elements-permitting-child-elements)
  "List of element types that can have children.")

(defvaralias 'om-branch-objects
  'org-element-recursive-objects
  "List of object types that can have objects as children.
These are also known as \"recursive objects\" in `org-element.el'")

(defconst om-branch-nodes
  (append om-branch-elements om-branch-objects)
  "List of node types that can have children.")

;;; BRANCH NODE CHILD TYPE RESTRICTIONS

;; `org-element.el' specifies which object nodes may be children of
;; other object nodes but does not have the same thing for element
;; nodes; implement my own restrictions here

(defconst om--object-restrictions
  (->> org-element-object-restrictions
       ;; remove non-object nodes
       (--remove (memq (car it) '(inlinetask item headline keyword)))
       ;; add plain-text type to everything except table-row
       (--map-when (not (eq (car it) 'table-row)) (-snoc it 'plain-text)))
  "Alist of object node type restrictions for object branch nodes.
The types in the cdr of each entry may be children of the type held at
the car.")

(defconst om--element-restrictions
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

(defconst om--node-restrictions
  (append om--element-restrictions om--object-restrictions)
  "Alist of all restrictions for containers.")

(defconst om--item-tag-restrictions
  (->> org-element-object-restrictions
       (alist-get 'item)
       (cons 'plain-text))
  "List of node types which may be used in item node tag properties.")

(defconst om--headline-title-restrictions
  (->> org-element-object-restrictions
       (alist-get 'headline)
       (cons 'plain-text))
  "List of node types which may be used in item headline title properties.")

;;; LIST OPERATIONS (EXTENDING DASH.el)

(defun om--pad-or-truncate (length pad list)
  "Return padded or truncated list starting from LIST.

If length of LIST is greater than LENGTH, truncate LIST to LENGTH
and return. If LIST is longer than LENGTH, add PAD to the end
of LIST until it's length equals LENGTH and return. Do nothing if
length of LIST is equal to LENGTH initially."
  (let ((blanks (- length (length list))))
    (if (< blanks 0) (-take length list)
      (append list (-repeat blanks pad)))))

;;; plist operations

(defun om--plist-get-keys (plist)
  "Get the keys for PLIST."
  (-slice plist 0 nil 2))

(defun om--plist-get-vals (plist)
  "Get the values for PLIST."
  (-slice plist 1 nil 2))

(defun om--plist-map-values (fun plist)
  "Map FUN over over the values in PLIST.
FUN is a unary function that returns a modified value."
  (let ((keys (om--plist-get-keys plist)))
    (->> (om--plist-get-vals plist)
         (--map (funcall fun it))
         (-interleave keys))))

(defun om--is-plist (list)
  "Return t if LIST is a plist."
  (and
   (listp list)
   (cl-evenp (length list))
   (-all? #'keywordp (-slice list 0 nil 2))))

(defun om--plist-remove (key plist)
  "Return PLIST with KEY and its value removed."
  (->> (-partition 2 plist) (--remove (eq (car it) key)) (-flatten-n 1)))

;;; inter-index operations

;; The "inter-index" alludes to the fact that these list operations
;; use an index value that refers to spaces between list members.
;; These functions are enhanced versions of what is provided in
;; `dash.el' and native emacs that handle negative indices and have
;; switches to handle out of bounds errors

(defun om--convert-inter-index (n list &optional use-oor)
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
     (t (om--arg-error
         "Index (%s) out of range; must be between %s and %s"
         n lower upper)))))

(defun om--insert-at (n x list &optional use-oor)
  "Like `-insert-at' but can insert X at negative indices N in LIST.
See `om--convert-inter-index' for the meaning of N and USE-OOR."
  (-insert-at (om--convert-inter-index n list use-oor) x list))

(defun om--split-at (n list &optional use-oor)
  "Like `-split-at' except allow negative indices in LIST.
See `om--convert-inter-index' for the meaning of N and USE-OOR."
  (let ((n* (om--convert-inter-index n list use-oor)))
    (when list
      (-split-at n* list))))

(defun om--splice-at (n list* list &optional use-oor)
  "Return LIST with LIST* spliced at index N.
See `om--convert-inter-index' for the meaning of N and USE-OOR."
  (--> (-map #'list list)
       (om--insert-at n list* it use-oor)
       (apply #'append it)))

;;; intra-index operations

;; The "intra-index" alludes to the fact that these list operations
;; use an index value that refers to explicit list members.
;; These functions are enhanced versions of what is provided in
;; `dash.el' and native emacs that handle negative indices and have
;; switches to handle out of bounds errors

(defun om--convert-intra-index (n list &optional use-oor)
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
     (t (om--arg-error
         "Index (%s) out of range; must be between %s and %s"
         n lower upper)))))

(defun om--remove-at (n list &optional use-oor)
  "Like `-remove-at' but honors negative indices N in LIST.
See `om--convert-intra-index' for the meaning of N and USE-OOR."
  (-some-> (om--convert-intra-index n list use-oor)
           (-remove-at list)))

(defun om--replace-at (n x list &optional use-oor)
  "Like `-replace-at' but can substitute X at negative indices N in LIST.
See `om--convert-intra-index' for the meaning of N and USE-OOR."
  (-some-> (om--convert-intra-index n list use-oor)
           (-replace-at x list)))

(defun om--nth (n list &optional use-oor)
  "Like `nth' but honors negative indices N in LIST.
See `om--convert-intra-index' for the meaning of N and USE-OOR."
  (-some-> (om--convert-intra-index n list use-oor)
           (nth list)))

;; functors

(om--defun* om--map-first (fun list)
  "Return LIST with FUN applied to the first member.
FUN is a unary function that returns a modified member."
  (when list
    (cons (funcall fun (car list)) (cdr list))))

(om--defun* om--map-last (fun list)
  "Return LIST with FUN applied to the last member.
FUN is a unary function that returns a modified member."
  (-some->> list (nreverse) (om--map-first fun) (nreverse)))

(om--defun* om--map-at (n fun list)
  "Return LIST with FUN applied to the member at index N.
FUN is a unary function that returns a modified member."
  (--> (nth n list) (funcall fun it) (-replace-at n it list)))

;;; INTERNAL TYPE FUNCTIONS

(define-error 'arg-type-error "Argument type error")

(defun om--arg-error (string &rest args)
  "Signal an `arg-type-error'.
STRING and ARGS are analogous to `error'."
    (signal 'arg-type-error `(,(apply #'format-message string args))))

(defun om--is-node (list)
  "Return t if LIST is a node."
  (om-is-any-type om-nodes list))

(defun om--is-table-row (node)
  "Return t if NODE is a standard table-row node."
  (and (om-is-type 'table-row node)
       (om--property-is-eq :type 'standard node)))

(defun om--filter-type (type node)
  "Return NODE if it is TYPE or nil otherwise."
  (and (om-is-type type node) node))

(defun om--filter-types (types node)
  "Return NODE if it is one of TYPES or nil otherwise."
  (and (om-is-any-type types node) node))

;;; MISC HELPER FUNCTIONS

(defun om--get-head (node)
  "Return the type and properties cells of NODE."
  (if (stringp node) node
    (-take 2 node)))

(defun om--construct (type props children)
  "Make a new node list structure of TYPE, PROPS, and CHILDREN.
TYPE is a symbol, PROPS is a plist, and CHILDREN is a list or nil."
  `(,type ,props ,@children))

(defun om--from-string (string)
  "Convert STRING to org-element representation."
  (with-temp-buffer
    (insert string)
    (-> (om-parse-this-buffer) (om-get-children) (car))))

;;; INTERNAL PREDICATES

(defun om--is-oneline-string (x)
  "Return t if X is a string with no newlines."
  (and (stringp x) (not (s-contains? "\n" x))))

(defun om--is-oneline-string-or-nil (x)
  "Return t if X is a string with no newlines or nil."
  (or (null x) (om--is-oneline-string x)))

(defun om--is-non-neg-integer (x)
  "Return t if X is a non-negative integer."
  (and (integerp x) (<= 0 x)))

(defun om--is-non-neg-integer-or-nil (x)
  "Return t if X is a non-negative integer or nil."
  (or (null x) (om--is-non-neg-integer x)))

(defun om--is-pos-integer (x)
  "Return t if X is a positive integer."
  (and (integerp x) (< 0 x)))

(defun om--is-pos-integer-or-nil (x)
  "Return t if X is a positive integer or nil."
  (or (null x) (om--is-pos-integer x)))

(defun om--is-string-list (x)
  "Return t if X is a list of strings without newlines or nil."
  (or (null x) (and (listp x) (-all? #'om--is-oneline-string x))))

;;; INTERNAL NODE PROPERTY FUNCTIONS

(defun om--get-property-nocheck (prop node)
  "Return PROP from NODE."
  (if (and (stringp node) (eq prop :post-blank))
      (length (car (s-match "[ ]*$" node)))
    (org-element-property prop node)))

(defun om--get-all-properties (node)
  "Return the properties list of NODE."
  (if (stringp node) (text-properties-at 0 node) (nth 1 node)))

(defun om--get-parent (node)
  "Return the parent of NODE."
  (om--get-property-nocheck :parent node))

(defun om--get-parent-headline (node)
  "Return the most immediate parent headline node of NODE."
  (-when-let (parent (om--get-parent node))
    (if (om-is-type 'headline parent) parent
      (om--get-parent-headline parent))))

(defun om--set-property-nocheck (prop value node)
  "Set PROP in NODE to VALUE."
  (if (stringp node)
      (if (eq prop :post-blank)
          (->> (s-trim-right node) (s-append (s-repeat value " ")))
        (org-add-props node nil prop value))
    (om--construct
     (om-get-type node)
     (plist-put (om--get-all-properties node) prop value)
     (om-get-children node))))

(defun om--set-properties-nocheck (plist node)
  "Set all properties in NODE to the values corresponding to PLIST.
PLIST is a list of property-value pairs that correspond to the
property list in NODE."
  (if (om--is-plist plist)
      (let ((props (om--get-all-properties node)))
        (om--construct
         (om-get-type node)
         (->> (-partition 2 plist)
              (--reduce-from (apply #'plist-put acc it) props))
         (om-get-children node)))
    (om--arg-error "Not a plist: %S" plist)))

(defun om--set-property-nocheck-nil (prop node)
  "Set PROP to nil in NODE."
  (om--set-property-nocheck prop nil node))

(defun om--set-properties-nocheck-nil (props node)
  "Set all PROPS to new in NODE."
  (let ((plist (--mapcat (list it nil) props)))
    (om--set-properties-nocheck plist node)))

(om--defun* om--map-property-nocheck (prop fun node)
  "Return NODE with FUN applied to the value in PROP.
FUN is a unary function that returns a modified value."
  (--> (om--get-property-nocheck prop node)
       (funcall fun it)
       (om--set-property-nocheck prop it node)))

(defun om--property-is-nil (prop node)
  "Return t if PROP in NODE is nil."
  (not (om--get-property-nocheck prop node)))

(defun om--property-is-eq (prop val node)
  "Return t if PROP in NODE is `eq' to VAL."
  (eq val (om--get-property-nocheck prop node)))

(om--defun* om--property-is-predicate (prop fun node)
  "Return t if FUN applied to the value of PROP in NODE results not nil.
FUN is a predicate function that takes one argument."
  (and (funcall fun (om--get-property-nocheck prop node)) t))

;;; NODE PROPERTY TRANSLATION AND CHECKING FRAMEWORK

;; This code provides the internal framework for the following
;; operations where NODE is any node, PROP is a property of NODE,
;; and VALUE is the value of PROP:

;; Get: f(PROP NODE) -> VALUE
;; Set: f(PROP VALUE NODE) -> NODE'
;; Map: f(PROP FUN NODE) -> NODE' where FUN is a function that
;;      modifies the value of PROP in NODE and is like:
;;      f(VALUE) -> VALUE'

;; Unfortunately, `org-element.el' stores the values for some
;; properties in an inconvenient way; (like some values are clearly
;; plists but are actually plists converted to strings). Therefore,
;; this framework makes a distinction between VALUE and its internal
;; representation IVALUE (which is actually the value stored in the
;; node list and understood by `org-element.el'), where VALUE may not
;; always be `equal' to IVALUE. When performing any of the operations
;; above, this framework will transparently translate between VALUE
;; and IVALUE (using so called encoders and decodes). Furthermore, the
;; VALUE for any PROP must conform to a 'type' which is enforced by
;; this framework.

;; The center of this framework is the constant
;; `om--node-property-alist' which holds the relationship of all node
;; types and their properties, type checkers, and encoders/decoders.
;; This alist has the following structure:

;; - car of each member is the type of NODE
;; - cdr of each member is the property alist for the node type
;;   - the car of the property alist is the keyword for PROP
;;   - the cdr of the property alist is an attribute plist, and the
;;     keys of this plist include:
;;     - :pred - a predicate function that returns t if VALUE is the
;;       correct type for PROP
;;     - :type-desc - a string describing the data type for PROP
;;     - :encode - a unary function that converts VALUE to IVALUE; if
;;       this not given then VALUE and IVALUE are `equal'
;;     - :decode - a function that inverts the function at :encode
;;     - :cis - a unary function that takes NODE and returns a
;;       modified NODE; the point of this it so 'update' other
;;       properties when PROP is changed
;;     - :const - a value that PROP should always have
;;     - :shift - a binary function that shifts PROP; the first
;;       argument takes an integer describing the magnitude and
;;       direction of the shift and the second argument is VALUE for
;;       PROP; return a new VALUE; this only makes sense the type
;;       of PROP is an integer
;;     - :require - a boolean telling if PROP is required to be
;;       specified when creating a NODE of this type
;;     - :string-list - a boolean telling if the type of PROP is a
;;       list of strings
;;     - :plist - a boolean telling if the type of PROP is a plist
;;     - :toggle - a boolean telling if the type of PROP is a boolean

;; In terms of property attributes, the three property operations
;; can be described by the following pseudo code:

;; get: GET(PROP VALUE) -> NODE
;; 1) DECODE(IGET(PROP, NODE)) -> VALUE where IGET retrieves the
;;    IVALUE of PROP from NODE

;; set: SET(PROP VALUE NODE) -> NODE
;; 1) if PROP(VALUE) -> t, proceed to 2), else throw error
;; 2) ISET(PROP, ENCODE(VALUE), NODE)) -> NODE' where ISET sets the
;;    PROP of NODE to IVALUE
;; 3) If CIS is non-nil, run CIS(NODE') -> NODE'', else return NODE'

;; map: MAP(PROP FUN NODE) -> NODE'
;; 1) GET(PROP NODE) -> VALUE
;; 2) FUN(VALUE) -> VALUE'
;; 3) if PRED(VALUE') -> t proceed to 4), else throw error
;; 4) SET(PROP VALUE' NODE) -> NODE'

;;; property value predicates (type specific)

(defun om--is-valid-link-format (x)
  "Return t if X is an allowed value for a link node format property."
  (memq x '(nil plain angle bracket)))

(defun om--is-valid-link-type (x)
  "Return t if X is an allowed value for a link node type property."
  (->> '("coderef" "custom-id" "file" "id" "radio" "fuzzy")
       (append (org-link-types))
       (member x)))

(defun om--is-valid-item-checkbox (x)
  "Return t if X is an allowed value for an item node checkbox property."
  (memq x '(nil on off trans)))

(defun om--is-valid-item-tag (x)
  "Return t if X is an allowed value for an item node tag property."
  (and (listp x)
       (--all? (om-is-any-type om--item-tag-restrictions it) x)))

(defun om--is-valid-item-bullet (x)
  "Return t if X is an allowed value for a item node bullet property."
  ;; NOTE org mode 9.1.9 has the following limitations:
  ;; - "+" will be converted to "-" when interpreted
  ;; - "1)" will be converted to "1." when interpreted
  ;; - alphanumeric symbols make the interpreter crash
  ;; TODO what to do about these limitations???
  ;; some valid org buffers might be parsed, but then can't be
  ;; use in this library because...org element is inconsistent
  (pcase x ((or '- (pred integerp)) t)))

(defun om--is-valid-clock-timestamp (x)
  "Return t if X is an allowed value for a clock node value property."
  (and (om-is-type 'timestamp x)
       (om--property-is-predicate :type
         (lambda (it) (memq it '(inactive inactive-range))) x)
       (om--property-is-nil :repeater-type x)))

(defun om--is-valid-planning-timestamp (x)
  "Return t if X is an allowed value for a planning node timestamp property."
  (or (null x) (and (om-is-type 'timestamp x)
                    (om--property-is-eq :type 'active x))))

(defun om--is-valid-entity-name (x)
  "Return t if X is an allowed value for an entity node name property."
  (org-entity-get x))

(defun om--is-valid-headline-tags (x)
  "Return t if X is an allowed value for a headline node tags property."
  (and (listp x)
       (-all? #'om--is-oneline-string x)
       (not (member org-archive-tag x))))

(defun om--is-valid-headline-priority (x)
  "Return t if X is an allowed value for a headline node priority property."
  (or (null x) (and (integerp x)
                    (>= org-lowest-priority x org-highest-priority))))

(defun om--is-valid-headline-title (x)
  "Return t if X is an allowed value for a headline node title property."
  (and
   (listp x)
   (--all? (om-is-any-type om--headline-title-restrictions it) x)))

(defun om--is-valid-timestamp-type (x)
  "Return t if X is an allowed value for a timestamp node type property."
  (memq x '(inactive inactive-range active active-range)))

(defun om--is-valid-timestamp-repeater-type (x)
  "Return t if X is an allowed value for a timestamp node repeater-type property."
  (memq x '(nil catch-up restart cumulate)))

(defun om--is-valid-timestamp-warning-type (x)
  "Return t if X is an allowed value for a timestamp node warning-type property."
  (memq x '(nil all first)))

(defun om--is-valid-timestamp-unit (x)
  "Return t if X is an allowed value for a timestamp node unit property."
  (memq x '(nil year month week day hour)))

(defun om--is-valid-latex-environment-value (x)
  "Return t if X is an allowed value for a latex-environment node value property."
  (pcase x
    ((or `(,(pred om--is-oneline-string))
         `(,(pred om--is-oneline-string) ,(pred stringp)))
     t)))

(defun om--is-valid-statistics-cookie-value (x)
  "Return t if X is an allowed value for a statistics-cookie node value property."
  (pcase x
    ((or `(nil) `(nil nil)) t)
    (`(,(and (pred integerp) percent))
     (<= 0 percent 100))
    (`(,(and (pred integerp) numerator)
       ,(and (pred integerp) denominator))
     (and (om--is-non-neg-integer numerator)
          (om--is-non-neg-integer denominator)
          (<= numerator denominator)))))

(defun om--is-valid-diary-sexp-value (x)
  "Return t if X is an allowed value for a diary-sexp node value property."
  (or (null x) (listp x)))

;;; encode/decode (general)

(defun om--decode-boolean (bool)
  "Return BOOL as either t or nil."
  (and bool t))

(defun om--encode-string-or-nil (string)
  "Return STRING as either itself or \"\" if nil."
  (if (null string) "" string))

(defun om--decode-string-or-nil (string)
  "Return STRING without text properties if not nil."
  (unless (null string)
    (substring-no-properties string)))

(defun om--encode-string-list-delim (string-list delim)
  "Return STRING-LIST as string joined by DELIM."
  (-some->> string-list (s-join delim)))

(defun om--decode-string-list-delim (string delim)
  "Return STRING as list of strings split by DELIM."
  (-some->> string (s-split delim)))

(defun om--encode-string-list-space-delim (string-list)
  "Return STRING-LIST as string joined by spaces."
  (om--encode-string-list-delim string-list " "))

(defun om--decode-string-list-space-delim (string)
  "Return STRING as list of strings split by spaces."
  (om--decode-string-list-delim string " "))

(defun om--encode-string-list-comma-delim (string-list)
  "Return STRING-LIST as string joined by commas."
  (om--encode-string-list-delim string-list ","))

(defun om--decode-string-list-comma-delim (string)
  "Return STRING as list of strings split by commas."
  (om--decode-string-list-delim string ","))

(defun om--encode-plist (plist)
  "Return PLIST as string joined by spaces."
  (-some->> (--map (format "%S" it) plist) (s-join " ")))

(defun om--decode-plist (string)
  "Return STRING as plist split by spaces."
  (-map #'intern (om--decode-string-list-space-delim string)))

;;; encode/decode (type specific)

(defun om--encode-latex-environment-value (value)
  "Return VALUE as a string representing a latex-environment.
VALUE is a list conforming to `om--is-valid-latex-environment-value'."
  (-let (((env body) value))
    (if body (format "\\begin{%1$s}\n%2$s\n\\end{%1$s}" env body)
      (format "\\begin{%1$s}\n\\end{%1$s}" env))))

(defun om--decode-latex-environment-value (value)
  "Return VALUE as a list representing a latex-environment.
The return value is a list conforming to
`om--is-valid-latex-environment-value'."
  (let ((m (car (s-match-strings-all "\\\\begin{\\(.+\\)}\n\\(.*\\)\n?\\\\end{\\(.+\\)}" value))))
    (list (nth 1 m) (nth 2 m))))

(defun om--encode-item-bullet (bullet)
  "Return BULLET as a formatted string.
BULLET must conform to `om--is-valid-item-bullet'."
  ;; assume bullet conforms to pcase statement below
  (pcase bullet
    ('- "- ")
    ((pred integerp) (format "%s. " bullet))
    (_ (error "This should not happen"))))

(defun om--decode-item-bullet (bullet)
  "Return BULLET as a symbol from a formatted string.
Return value will conform to `om--is-valid-item-bullet'."
  ;; NOTE this must conform to the full range of item bullets since
  ;; anything could be parsed from an org file. Anything "invalid"
  ;; should be converted to it's closest "element legal" bullet
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
          (om--arg-error "Invalid bullet found: %s" bullet)))))

(defun om--decode-headline-tags (tags)
  "Return TAGS with `org-archive-tag' removed."
  (remove org-archive-tag tags))

(defun om--encode-statistics-cookie-value (value)
  "Return VALUE as formatted string representing the cookie.
VALUE must conform to `om--is-valid-statistics-cookie-value'."
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

(defun om--decode-statistics-cookie-value (value)
  "Return VALUE as a list representing the cookie.
Return value will conform to `om--is-valid-statistics-cookie-value'."
  (cond
   ((equal "[%]" value) '(nil))
   ((equal "[/]" value) '(nil nil))
   (t
    (->>
     (or (s-match-strings-all "\\[\\([0-9]+\\)/\\([0-9]+\\)\\]" value)
         (s-match-strings-all "\\[\\([0-9]+\\)%\\]" value)
         (om--arg-error "Invalid stats-cookie: %s" value))
     (cdar)
     (-map #'string-to-number)))))

;; TODO this will make quotes turn to (quote )
(defun om--encode-diary-sexp-value (value)
  "Return VALUE as a string.
VALUE must conform to `om--is-valid-diary-sexp-value'."
  (if value (format "%%%%%S" value) "%%()"))

(defun om--decode-diary-sexp-value (value)
  "Return VALUE as a form.
Return value will conform to `om--is-valid-diary-sexp-value'."
  (->> (s-chop-prefix "%%" value) (read)))

;;; cis functions

(defun om--update-macro-value (macro)
  "Return MACRO node with its value property updated.
This will be based on MACRO's key and value properties."
  (let* ((k (om--get-property-nocheck :key macro))
         (as (om--get-property-nocheck :args macro))
         (v (if as (format "%s(%s)" k (s-join "," as)) k)))
    (om--set-property-nocheck :value (format "{{{%s}}}" v) macro)))

(defun om--update-clock-duration (clock)
  "Return CLOCK node with its duration and status properties updated.
This will be based on CLOCK's value property."
  (let* ((ts (om--get-property-nocheck :value clock))
         (seconds (om--timestamp-get-range ts))
         (plist
          (if (< 0 seconds)
              (let* ((h (-> seconds (/ 3600) (floor)))
                     (m (-> seconds (- (* h 3600)) (/ 60) (floor))))
                `(:duration ,(format "%2d:%02d" h m) :status running))
            '(:duration nil :status closed))))
    (om--set-properties-nocheck plist clock)))

(defun om--update-headline-tags (headline)
  "Return HEADLINE node with its tags updated.
This will be based on HEADLINE's archivedp property."
  (cl-flet
      ((add-archive-tag-maybe
        (tags)
        (let ((tags* (remove org-archive-tag tags)))
          (if (om--get-property-nocheck :archivedp headline)
              (-snoc tags* org-archive-tag) tags*))))
    (om--map-property-nocheck :tags #'add-archive-tag-maybe headline)))

;;; shifters

(defun om--shift-pos-integer (n x)
  "Return X shifted by N (both are integers).
If the value to return is less than 1, return 1."
  (when x
    (let ((x* (+ x n)))
      (if (< 0 x*) x* 1))))

(defun om--shift-non-neg-integer (n x)
  "Return X shifted by N (both are integers).
If the value to return is less than 0, return 0."
  (when x
    (let ((x* (+ x n)))
      (if (<= 0 x*) x* 0))))

(defun om--shift-headline-priority (n priority)
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

(eval-and-compile
  (defconst om--node-property-alist
    (let ((bool (list :pred #'booleanp
                      :decode 'om--decode-boolean
                      :type-desc "nil or t"
                      :toggle t))
          (pos-int (list :pred #'om--is-pos-integer
                         :type-desc "a positive integer"))
          (pos-int-nil (list :pred #'om--is-pos-integer-or-nil
                             :type-desc "a positive integer or nil"))
          (nn-int (list :pred #'om--is-non-neg-integer
                        :type-desc "a non-negative integer"))
          (nn-int-nil (list :pred #'om--is-non-neg-integer-or-nil
                            :type-desc "a non-negative integer or nil"))
          (str (list :pred #'stringp
                     :type-desc "a string"))
          (str-nil (list :pred #'string-or-null-p
                         :type-desc "a string or nil"))
          (ol-str (list :pred #'om--is-oneline-string
                        :type-desc "a oneline string"))
          (ol-str-nil (list :pred #'om--is-oneline-string-or-nil
                            :type-desc "a oneline string or nil"))
          (plist (list :encode 'om--encode-plist
                       :pred #'om--is-plist
                       :decode 'om--decode-plist
                       :plist t
                       :type-desc "a plist"))
          (slist (list :pred #'om--is-string-list
                       :string-list t
                       :type-desc "a list of oneline strings"))
          (slist-com (list :encode 'om--encode-string-list-comma-delim
                           :decode 'om--decode-string-list-comma-delim
                           :pred #'om--is-string-list
                           :string-list t
                           :type-desc "a list of oneline strings"))
          (slist-spc (list :encode 'om--encode-string-list-space-delim
                           :decode 'om--decode-string-list-space-delim
                           :pred #'om--is-string-list
                           :string-list t
                           :type-desc "a list of oneline strings"))
          (planning (list :pred #'om--is-valid-planning-timestamp
                          :type-desc "a zero-range, active timestamp node"))
          (ts-unit (list :pred #'om--is-valid-timestamp-unit
                         :type-desc '("nil or a symbol from `year' `month'"
                                      "`week' `day', or `hour'"))))
      `((babel-call (:call ,@ol-str :require t)
                    (:inside-header ,@plist)
                    (:arguments ,@slist-com)
                    (:end-header ,@plist)
                    (:value))
        (bold)
        (center-block)
        (clock (:value :pred om--is-valid-clock-timestamp
                       :cis om--update-clock-duration
                       :type-desc ("an unranged, inactive timestamp"
                                   "node with no warning or repeater")
                       :require t)
               (:status)
               (:duration))
        (code (:value ,@str :require t))
        (comment (:value ,@str :require t))
        (comment-block (:value ,@str :decode s-trim-right :require ""))
        (drawer (:drawer-name ,@ol-str :require t))
        (diary-sexp (:value :encode om--encode-diary-sexp-value
                            :pred om--is-valid-diary-sexp-value
                            :decode om--decode-diary-sexp-value
                            :type-desc "a list form or nil"))
        (dynamic-block (:arguments ,@plist)
                       (:block-name ,@ol-str :require t))
        (entity (:name :pred om--is-valid-entity-name
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
        (footnote-definition (:label ,@ol-str :require t))
        (footnote-reference (:label ,@ol-str-nil)
                            (:type))
        (headline (:archivedp ,@bool :cis om--update-headline-tags)
                  (:commentedp ,@bool)
                  (:footnote-section-p ,@bool)
                  (:level ,@pos-int
                          :shift om--shift-pos-integer
                          :require 1)
                  (:pre-blank ,@nn-int
                              :shift om--shift-non-neg-integer
                              :require 0)
                  (:priority :pred om--is-valid-headline-priority
                             :shift om--shift-headline-priority
                             :type-desc ("an integer between (inclusive)"
                                         "`org-highest-priority' and"
                                         "`org-lowest-priority'"))
                  (:tags :pred om--is-valid-headline-tags
                         :decode om--decode-headline-tags
                         :cis om--update-headline-tags
                         :type-desc "a string list"
                         :string-list t)
                  (:title :pred om--is-valid-headline-title
                          :type-desc "a secondary string")
                  (:todo-keyword ,@ol-str-nil
                                 :decode om--decode-string-or-nil) ; TODO restrict this?
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
        (item (:bullet :encode om--encode-item-bullet
                       :pred om--is-valid-item-bullet
                       :decode om--decode-item-bullet
                       :type-desc ("a positive integer (ordered)"
                                   "or the symbol `-' (unordered)")
                       :require '-)
              (:checkbox :pred om--is-valid-item-checkbox
                         :type-desc "nil or the symbols `on', `off', or `trans'")
              (:counter ,@pos-int-nil :shift om--shift-pos-integer)
              (:tag :pred om--is-valid-item-tag
                    :type-desc "a secondary string")
              (:structure))
        (keyword (:key ,@ol-str :require t)
                 (:value ,@ol-str :require t))
        (latex-environment (:value :encode om--encode-latex-environment-value
                                   :pred om--is-valid-latex-environment-value
                                   :decode om--decode-latex-environment-value
                                   :type-desc "a list of strings like (ENV BODY) or (ENV)"
                                   :require t))
        (latex-fragment (:value ,@str :require t))
        (line-break)
        (link (:path ,@ol-str :require t)
              (:format :pred om--is-valid-link-format
                       :type-desc "the symbol `plain', `bracket' or `angle'")
              (:type :pred om--is-valid-link-type
                     ;; TODO make this desc better
                     :type-desc ("a oneline string from `org-link-types'"
                                 "or \"coderef\", \"custom-id\","
                                 "\"file\", \"id\", \"radio\", or"
                                 "\"fuzzy\"")
                     ;; TODO is fuzzy a good default?
                     :require "fuzzy")
              (:raw-link) ; TODO update children through this?
              (:application)
              (:search-option))
        (macro (:args ,@slist :cis om--update-macro-value)
               (:key ,@ol-str :cis om--update-macro-value :require t)
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
                            :encode om--encode-statistics-cookie-value
                            :pred om--is-valid-statistics-cookie-value
                            :decode om--decode-statistics-cookie-value
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
        (timestamp (:type :pred om--is-valid-timestamp-type
                          :type-desc ("a symbol from `inactive',"
                                      "`active', `inactive-ranged', or"
                                      "`active-ranged'")
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
                   (:repeater-type :pred om--is-valid-timestamp-repeater-type
                                   :type-desc ("nil or a symbol from"
                                               "`catch-up', `restart',"
                                               "or `cumulate'"))
                   (:repeater-unit ,@ts-unit)
                   (:repeater-value ,@pos-int-nil)
                   (:warning-type :pred om--is-valid-timestamp-warning-type
                                  :type-desc ("nil or a symbol from"
                                              "`all' or `first'"))
                   (:warning-unit ,@ts-unit)
                   (:warning-value ,@pos-int-nil)
                   (:raw-value))
        (underline)
        (verbatim (:value ,@str :require t))
        (verse-block)))))

;; add post-blank functions to all entries
(let ((post-blank-funs '(:post-blank :pred om--is-non-neg-integer
                                     :shift om--shift-non-neg-integer)))
  (setq om--node-property-alist
        (--map (-snoc it post-blank-funs) om--node-property-alist)))

;;; node property operations

;; alist functions

(defun om--get-property-attribute (attribute type prop)
  "Return ATTRIBUTE for PROP of node TYPE.
If NOERROR is non-nil, return nil instead of signaling an error
if PROP in TYPE does not have ATTRIBUTE."
  (-if-let (type-list (alist-get type om--node-property-alist))
      (if (assoc prop type-list)
          (-when-let (plist (alist-get prop type-list))
            (plist-get plist attribute))
        (om--arg-error "Type '%s' does not have property '%s'"
                       type prop))
        ;; (unless noerror
        ;;   (let* ((msg1 "Property '%s' has no attribute '%s' for type '%s'")
        ;;          (msg2
        ;;           (or
        ;;            (-some->>
        ;;             type-list
        ;;             (--filter (plist-get (cdr it) attribute))
        ;;             (--map (symbol-name (car it)))
        ;;             (s-join ", ")
        ;;             (format "properties with this attribute are: %s"))
        ;;            "this type has no properties with this attribute"))
        ;;          (msg (format "%s; %s" msg1 msg2)))
        ;;     (om--arg-error msg prop attribute type))))
    (om--arg-error "Tried to query property '%s' for non-existent type '%s'" prop type)))

(defun om--get-property-encoder (type prop)
  "Return the encoder function for PROP of node TYPE."
  (om--get-property-attribute :encode type prop))

(defun om--get-property-decoder (type prop)
  "Return the decoder function for PROP of node TYPE."
  (om--get-property-attribute :decode type prop))

(defun om--get-property-cis-function (type prop)
  "Return the cis function for PROP of node TYPE."
  (om--get-property-attribute :cis type prop))

(defun om--get-property-type-desc (type prop)
  "Return the type-description string for PROP of node TYPE."
  (let ((desc (om--get-property-attribute :type-desc type prop)))
    (if (listp desc) (s-join " " desc) desc)))

;;; INTERNAL BRANCH/CHILD MANIPULATION

(defun om--get-descendent (indices node)
  "Return the nested children of NODE as given by INDICES.
INDICES is a list of integers specifying the index and level of the
nested element to return."
  (if (not indices) node
    (->> (om-get-children node)
         (nth (car indices))
         (om--get-descendent (cdr indices)))))

(defun om--set-children-nocheck (children node)
  "Return NODE with children set to CHILDREN."
   (let ((head (om--get-head node)))
     (if children (append head children) head)))

(om--defun* om--map-children-nocheck (fun node)
  "Return NODE with FUN applied to its children.

FUN is a unary function that takes a list of children and returns
a modified list of children."
  (--> (om-get-children node)
       (funcall fun it)
       (om--set-children-nocheck it node)))

(defun om--set-childen-throw-error (type child-types illegal)
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
      (om--arg-error fmt type illegal child-types))))

;;; BASE BUILDER FUNCTIONS

;;; build helpers

(defconst om--object-properties
  '(:begin :end :parent)
  "Minimum properties for objects.")

(defconst om--recursive-object-properties
  (append om--object-properties '(:contents-begin :contents-end))
  "Minimum properties for recursive objects.")

(defconst om--element-properties
  (cons :post-affiliated om--object-properties)
  "Minimum properties for elements.")

(defconst om--container-element-properties
  (cons :post-affiliated om--recursive-object-properties)
  "Minimum properties for container elements.")

(defun om--init-properties (props)
  "Return a plist where the keys are PROPS and all values are nil."
  (--mapcat (list it nil) props))

(defun om--build (type post-blank props)
  "Return a new node assembled from TYPE, POST-BLANK, and PROPS.
TYPE is a symbol, POST-BLANK is a postive integer, and PROPS is a
plist of properties for the node."
  (->> (om-set-property :post-blank (or post-blank 0) `(,type nil))
       (om--set-properties-nocheck-nil props)))

(defun om--build-object (type post-blank)
  "Return a new object-typed node from TYPE and POST-BLANK."
  (om--build type post-blank om--object-properties))

(defun om--build-recursive-object (type post-blank children)
  "Return a new branch object-typed node from TYPE, POST-BLANK, and CHILDREN."
  (->> om--recursive-object-properties
       (om--build type post-blank)
       (om-set-children children)))

(defun om--build-element (type post-blank)
  "Return a new element-typed node from TYPE and POST-BLANK."
  (om--build type post-blank om--element-properties))

(defun om--build-container-element (type post-blank children)
  "Return a new branch element-typed node from TYPE, POST-BLANK, and CHILDREN."
  (->> om--container-element-properties
       (om--build type post-blank)
       (om-set-children children)))

;;; base builders

;; define all base builders using this automated monstrosity

(eval-and-compile
  (defun om--kwd-to-sym (keyword)
    "Return KEYWORD as a string with no leading colon."
    (->> (symbol-name keyword) (s-chop-prefix ":") (intern)))

  (defun om--prepend-article (string)
    "Return STRING starting with \"a\" or \"an\" depending on first word."
    (let ((a (--> (symbol-name string)
                  (s-left 1 it)
                  (if (member it '("a" "e" "i" "o" "u")) "an" "a"))))
      (format "%s %s" a string)))

  (--each (--remove (eq 'plain-text (car it)) om--node-property-alist)
    (let* ((type (car it))
           (element? (memq type org-element-all-elements))
           (name (intern (format "om-build-%s" type)))
           (props (->> (cdr it)
                       (--remove (eq :post-blank (car it)))
                       (-non-nil)
                       (--group-by
                        (-let (((&plist :require :pred :const) (cdr it)))
                          (cond
                           (const 'const)
                           ((not pred) 'null)
                           ((eq require t) 'req)
                           (t 'key))))))
           (pos-args (->> (alist-get 'req props)
                          (--map (om--kwd-to-sym (car it)))))
           (kw-args (->> (alist-get 'key props)
                         (--map
                          (let ((prop (om--kwd-to-sym (car it)))
                                (default (plist-get (cdr it) :require)))
                            (if default `(,prop ,default) prop)))))
           (rest-arg (cond
                      ((memq type org-element-greater-elements) 'element-nodes)
                      ((memq type org-element-object-containers) 'object-nodes)))
           (args
            (let ((a `(,@pos-args &key ,@kw-args post-blank)))
              (if rest-arg `(,@a &rest ,rest-arg) a)))
           (const-props
            (-some--> (alist-get 'const props)
                      (--mapcat
                       (let ((p (car it))
                             (c (plist-get (cdr it) :const)))
                         (list p c))
                       it)
                      (if (= 2 (length it))
                          `(om--set-property-nocheck ,@it)
                        `(om--set-properties-nocheck (list ,@it)))))
           (nil-props
            (-some--> (alist-get 'null props)
                      (-map #'car it)
                      (if (= 1 (length it))
                          `(om--set-property-nocheck-nil ,@it)
                        `(om--set-properties-nocheck-nil (list ,@it)))))
           (strict-props
            (-some-->
             (append (alist-get 'key props) (alist-get 'req props))
             (-map #'car it)
             (--mapcat (list it (om--kwd-to-sym it)) it)
             (if (= 2 (length it))
                 `(om-set-property ,@it)
               `(om-set-properties (list ,@it)))))
           (doc
            (let ((class (if element? "element" "object"))
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
               (format "Build %s %s node" (om--prepend-article type) class)
               end
               "\n\nThe following properties are settable:\n"
               prop "\n- POST-BLANK: a non-negative integer")))
           (builder
            (let ((a `(',type post-blank)))
              (cond
               ((and element? rest-arg)
                `(om--build-container-element ,@a ,rest-arg))
               (element?
                `(om--build-element ,@a))
               (rest-arg
                `(om--build-recursive-object ,@a ,rest-arg))
               (t
                `(om--build-object ,@a)))))
           (body (if (or strict-props nil-props const-props)
                     `(->> ,@(-non-nil (list builder const-props
                                             nil-props strict-props)))
                   builder)))
      (eval `(om--defun-kw ,name ,args ,doc ,body)))))

;; INTERNAL TYPE-SPECIFIC PROPERTY FUNCTIONS

;;; object nodes
;;
;; statistics-cookie

(defun om--statistics-cookie-get-format (statistics-cookie)
  "Return format of STATISTICS-COOKIE as a symbol.
If fractional cookie, return `fraction'; if percentage cookie return
`percent', else throw error (which should never happen)."
  (let ((value (om--get-property-nocheck :value statistics-cookie)))
    (cond ((s-contains? "/" value) 'fraction)
          ((s-contains? "%" value) 'percent)
          (t (om--arg-error "Unparsable statistics cookie: %s" value)))))

;; timestamp (auxiliary functions)

(defun om--time-is-long (time)
  "Return t if TIME is a long format time list."
  (pcase time
    (`(,(pred integerp) ,(pred integerp) ,(pred integerp)
       ,(pred integerp) ,(pred integerp))
     t)))

;; randomly make these public, not sure where else to put them
(defun om-time-to-unixtime (time)
  "Return the unix time (integer seconds) of time list TIME.
The returned value is dependent on the time zone of the operating
system."
  (let ((encoded
         (if (om--time-is-long time)
             (apply #'encode-time 0 (nreverse time))
           (apply #'encode-time 0 0 0 (nreverse (-take 3 time))))))
    (round (float-time encoded))))

(defun om-unixtime-to-time-long (unixtime)
  "Return the long time list of UNIXTIME.
The list will be formatted like (YEAR MONTH DAY HOUR MIN)."
  (nreverse (-slice (decode-time unixtime) 1 6)))

(defun om-unixtime-to-time-short (unixtime)
  "Return the short time list of UNIXTIME.
The list will be formatted like (YEAR MONTH DAY nil nil)."
  (append (-take 3 (om-unixtime-to-time-long unixtime))
          '(nil nil)))

(defun om--time-truncate (time)
  "Return the short time format of TIME regardless of input format."
  (-take 3 time))

(defun om--time-shift (n unit time)
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
           (om--arg-error "Invalid unit for short timestamps: %S" unit))
          (t (om--arg-error "Invalid time unit: %S" unit))))
       (get-shifts-long
        (n unit)
        (cl-case unit
          (minute `(0 0 0 0 ,n))
          (hour `(0 0 0 ,n 0))
          (t (get-shifts-short n unit))))
       (apply-shifts
        (shifts time)
        (->> (-zip-with #'+ time shifts)
             (nreverse)
             (apply #'encode-time 0)
             (decode-time))))
    (if (om--time-is-long time)
        (let ((shifts (get-shifts-long n unit)))
          (nreverse (-slice (apply-shifts shifts time) 1 6)))
      (let ((shifts (get-shifts-short n unit))
            (time* (-replace nil 0 time)))
        (->> (-slice (apply-shifts shifts time*) 3 6)
             (append '(nil nil))
             (nreverse))))))

(defun om--time-format-props (time suffix)
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
                  (_ (om--arg-error "Invalid time given: %s" time)))))
    (-interleave props time*)))

(defun om--decorator-format (dec dtype valid-types)
  "Return plist representing a timestamp warning or repeater (decorators).

DEC is a list like (TYPE VALUE UNIT) of the decorator, DTYPE is either
`warning' or `repeater', and VALID-TYPES are the allowed values for
TYPE given in DEC."
  (let ((props (->> '(type value unit)
                    (--map (intern (format ":%s-%s" dtype it))))))
    (if (not dec) (om--init-properties props)
      (-let (((type value unit) dec))
        (unless (memq type valid-types)
          (om--arg-error "Invalid %s type: %s" dtype type))
        (unless (integerp value)
          (om--arg-error "Invalid %s value: %s" dtype value))
        (unless (memq unit '(year month week day hour))
          (om--arg-error "Invalid %s unit: %s" dtype value))
        (-interleave props (list type value unit))))))

;; timestamp (regular)

(defun om--timestamp-get-start-time (timestamp)
  "Return the time list of the start time in TIMESTAMP."
  (-let (((&plist :minute-start n :hour-start h :day-start d
                  :month-start m :year-start y)
          (om--get-all-properties timestamp)))
    `(,y ,m ,d ,h ,n)))

(defun om--timestamp-get-end-time (timestamp)
  "Return the time list of the end time in TIMESTAMP."
  (-let (((&plist :minute-end n :hour-end h :day-end d
                  :month-end m :year-end y)
          (om--get-all-properties timestamp)))
    `(,y ,m ,d ,h ,n)))

(defun om--timestamp-get-start-unixtime (timestamp)
  "Return the unixtime of the start time in TIMESTAMP."
  (->> (om--timestamp-get-start-time timestamp)
       (om-time-to-unixtime)))

(defun om--timestamp-get-end-unixtime (timestamp)
  "Return the unixtime of the end time in TIMESTAMP."
  (->> (om--timestamp-get-end-time timestamp)
       (om-time-to-unixtime)))

(defun om--timestamp-get-range (timestamp)
  "Return the range of TIMESTAMP in seconds."
  (- (om--timestamp-get-end-unixtime timestamp)
     (om--timestamp-get-start-unixtime timestamp)))

(defun om--timestamp-is-active (timestamp)
  "Return t if TIMESTAMP is an active type."
  (memq (om--get-property-nocheck :type timestamp) '(active active-range)))

(defun om--timestamp-is-ranged (timestamp)
  "Return t if TIMESTAMP has a range greater than 0 seconds."
  (/= 0 (om--timestamp-get-range timestamp)))

(defun om--timestamp-is-ranged-lowres (timestamp)
  "Return t if TIMESTAMP is range according to only year, month, and day."
  (-let* (((l s) (-split-at 3 (om--timestamp-get-start-time timestamp)))
          ((L S) (-split-at 3 (om--timestamp-get-end-time timestamp))))
    ;; lowres if Y/M/D is different and Min/Hour are the same
    ;; but only if Min/Hour are both not nil
    (and (not (equal l L)) (or (equal s S)
                               (memq nil s)
                               (memq nil S)))))

(defun om--timestamp-set-start-time-nocheck (time timestamp)
  "Set the start TIME of TIMESTAMP. Does not set type."
  (let ((time* (om--time-format-props time 'start)))
      (om--set-properties-nocheck time* timestamp)))

(defun om--timestamp-set-start-time (time timestamp)
  "Return TIMESTAMP with start time properties set according to time list TIME."
  (->> (om--timestamp-set-start-time-nocheck time timestamp)
       (om--timestamp-update-type-ranged)))

(defun om--timestamp-set-end-time-nocheck (time timestamp)
  "Set the end TIME of TIMESTAMP. Does not set type."
  (if time
      (-> (om--time-format-props time 'end)
          (om--set-properties-nocheck timestamp))
    (-> (om--timestamp-get-start-time timestamp)
        (om--time-format-props 'end)
        (om--set-properties-nocheck timestamp))))

(defun om--timestamp-set-end-time (time timestamp)
  "Return TIMESTAMP with end time properties set according to time list TIME."
  (let ((ts* (om--timestamp-set-end-time-nocheck time timestamp)))
    (if time (om--timestamp-update-type-ranged ts*)
      (om--timestamp-set-type-ranged nil ts*))))

(defun om--timestamp-set-single-time (time timestamp)
  "Return TIMESTAMP with start/end properties set to time list TIME."
  (->> (om--timestamp-set-start-time-nocheck time timestamp)
       (om--timestamp-set-end-time-nocheck time)
       (om--timestamp-set-type-ranged nil)))

(defun om--timestamp-set-double-time (time1 time2 timestamp)
  "Return TIMESTAMP with start and end properties set to time lists TIME1 and TIME2."
  (->> (om--timestamp-set-start-time-nocheck time1 timestamp)
       (om--timestamp-set-end-time-nocheck time2)
       (om--timestamp-update-type-ranged)))

(defun om--timestamp-set-range (range timestamp)
  "Return TIMESTAMP with end time shifted to RANGE seconds from start time."
  (let* ((start (om--timestamp-get-start-time timestamp))
         (long? (om--time-is-long start))
         (range (* range (if long? 60 86400)))
         (t2 (--> (om-time-to-unixtime start)
                  (+ it range)
                  (if long? (om-unixtime-to-time-long it)
                    (om-unixtime-to-time-short it)))))
    (->> (om--timestamp-set-end-time-nocheck t2 timestamp)
         (om--timestamp-update-type-ranged))))

(defun om--timestamp-update-type-ranged (timestamp)
  "Return TIMESTAMP with updated type based on if it is ranged."
  (-> (om--timestamp-is-ranged-lowres timestamp)
      (om--timestamp-set-type-ranged timestamp)))

(defun om--timestamp-set-type-ranged (ranged? timestamp)
  "Return TIMESTAMP with type set according to RANGED?."
  (cl-flet
      ((update-range
       (type)
       (cl-case type
         ((active active-range)
          (if ranged? 'active-range 'active))
         ((inactive inactive-range)
          (if ranged? 'inactive-range 'inactive))
         (t (om--arg-error "Invalid timestamp type: %s" type)))))
    (om--map-property-nocheck :type #'update-range timestamp)))

(defun om--timestamp-set-active (flag timestamp)
  "Return TIMESTAMP with active type if FLAG is t."
  (let* ((type (if (om--timestamp-is-ranged-lowres timestamp)
                   (if flag 'active-range 'inactive-range)
                 (if flag 'active 'inactive))))
    (om--set-property-nocheck :type type timestamp)))

(defun om--timestamp-set-warning (warning timestamp)
  "Return TIMESTAMP with warning properties set to WARNING list."
  (let ((types '(all first)))
    (-> (om--decorator-format warning 'warning types)
        (om--set-properties-nocheck timestamp))))

(defun om--timestamp-set-repeater (repeater timestamp)
  "Return TIMESTAMP with warning properties set to REPEATER list."
  (let ((types '(catch-up restart cumulate)))
    (-> (om--decorator-format repeater 'repeater types)
        (om--set-properties-nocheck timestamp))))

(defun om--timestamp-shift-start (n unit timestamp)
  "Return TIMESTAMP with start time shifted N UNIT's."
  (let ((time* (->> (om--timestamp-get-start-time timestamp)
                    (om--time-shift n unit))))
    (->> (om--timestamp-set-start-time time* timestamp)
         (om--timestamp-update-type-ranged))))

(defun om--timestamp-shift-end (n unit timestamp)
  "Return TIMESTAMP with end time shifted N UNIT's."
  (let ((time* (->> (om--timestamp-get-end-time timestamp)
                    (om--time-shift n unit))))
    (->> (om--timestamp-set-end-time time* timestamp)
         (om--timestamp-update-type-ranged))))

;; timestamp (diary sexp)

;;; element nodes
;;
;; headline

(defun om--headline-shift-level (n headline)
  "Return HEADLINE node with the level property shifted by N.
If the level is less then one after shifting, set level to one."
  (om--map-property-nocheck* :level (om--shift-pos-integer n it) headline))

(defun om--headline-set-statistics-cookie (value headline)
  "Return HEADLINE node with statistics cookie set by VALUE.
VALUE is a list conforming to `om--is-valid-statistics-cookie-value'
or nil to erase the statistics cookie if present."
  (om--map-property-nocheck*
   :title
   (let ((last? (om-is-type 'statistics-cookie (-last-item it))))
     (cond
      ((and last? value)
       (om--map-last* (om-set-property :value value it) it))
      ((and last? (not value))
       (-drop-last 1 it))
      (value
       (-snoc it (om-build-statistics-cookie value)))
      (t it)))
   headline))

(defun om--headline-set-statistics-cookie-fraction (done total headline)
  "Return HEADLINE node with statistics cookie set by DONE and TOTAL.

DONE and TOTAL are integers representing the numerator and denominator
respectively of the statistics-cookie's fractional value. Both must
be greater than zero, and DONE must be less than or equal to TOTAL."
  (-if-let (cookie (om-headline-get-statistics-cookie headline))
      (let* ((format (om--statistics-cookie-get-format cookie))
             (value (if (eq 'fraction format) `(,done ,total)
                      (-> (float done)
                          (/ total)
                          (* 100)
                          (round)
                          (list)))))
        (om--headline-set-statistics-cookie value headline))
    headline))

;; planning

(defun om--planning-list-to-timestamp (planning-list)
  "Return timestamp node from PLANNING-LIST.
See `om-build-planning!' for syntax of PLANNING-LIST."
  (when planning-list
    (let* ((p (-partition-before-pred
               (lambda (it) (memq it '(&warning &repeater)))
               planning-list)))
      (om-build-timestamp! (car p)
                           :active t
                           :warning (alist-get '&warning p)
                           :repeater (alist-get '&repeater p)))))

;;; INTERNAL TYPE-SPECIFIC BRANCH/CHILD FUNCTIONS

;;; headline

(defun om--headline-subtree-shift-level (n headline)
  "Return HEADLINE node with its level shifted by N.
Also shift all HEADLINE node's child headline nodes by N.
If the final shifted level is less one, set level to one (for parent
and child nodes)."
  (->> (om--headline-shift-level n headline)
       (om-headline-map-subheadlines
        (lambda (headlines)
          (--map (om--headline-subtree-shift-level n it)
                 headlines)))))

(defun om--headline-set-level (level headline)
  "Return HEADLINE node with its level set to LEVEL.
Additionally set all child headline nodes to be (+ 1 level) for
first layer, (+ 2 level for second, and so on."
  (->> (om-set-property :level level headline)
       (om--map-children-nocheck*
         (--map (om--headline-set-level (1+ level) it) it))))

;;; table

(defun om--table-get-width (table)
  "Return the width of TABLE as an integer.
This effectively is the maximum of all table-row lengths."
  (->> (om-get-children table)
       (--map (length (om-get-children it)))
       (-max)))

(defun om--table-pad-or-truncate (length list)
  "Pad or truncate LIST of table-cell nodes by LENGTH.
Behavior is the same as `om--pad-or-truncate' where the padded value
is a blank table-cell node."
  (let ((pad (om-build-table-cell "")))
    (om--pad-or-truncate length pad list)))

(defun om--column-map-down-rows (fun column-index table)
  "Return TABLE node with FUN applied down the rows at COLUMN-INDEX.

FUN is a unary function that takes a table-cell node and returns
a modified table-cell node."
  (cl-flet*
      ((zip-into-rows
        (row new-cell)
        (if (om--property-is-eq :type 'rule row) row
          (om--map-children-nocheck
           (lambda (cells) (funcall fun new-cell cells))
           row)))
       (map-rows
        (rows)
        (->> rows
             (--find-indices (om--property-is-eq :type 'rule it))
             (--reduce-from (-insert-at it nil acc) column-index)
             (om--table-pad-or-truncate (length rows))
             (-zip-with #'zip-into-rows rows))))
    (om--map-children-nocheck #'map-rows table)))

(defun om--table-get-row (row-index table)
  "Return the table-row node at ROW-INDEX within TABLE.
Rule-type table-row nodes do not factor when counting the index."
  (-some->> (om-get-children table)
            (--filter (om--property-is-eq :type 'standard it))
            (om--nth row-index)))

(defun om--table-replace-column (column-index column-cells table)
  "Return TABLE with COLUMN-CELLS in place of original cells at COLUMN-INDEX."
  (om--column-map-down-rows
   (lambda (new-cell cells) (om--replace-at column-index new-cell cells))
   column-cells
   table))

(defun om--table-row-pad-maybe (table table-row)
  "Return TABLE-ROW with row truncated or padded.
See `om--table-pad-or-truncate' for how padding and truncation is
performed. TABLE is used to get the table width."
  (if (om--property-is-eq :type 'rule table-row) table-row
    (let ((width (om--table-get-width table)))
      (om--map-children-nocheck*
        (om--table-pad-or-truncate width it)
        table-row))))

(defun om--table-replace-row (row-index table-row table)
  "Return TABLE node with row at ROW-INDEX replaced by TABLE-ROW."
  (let ((table-row (om--table-row-pad-maybe table table-row)))
    (om--map-children-nocheck* (om--replace-at row-index table-row it) table)))

(defun om--table-clear-row (row-index table)
  "Return TABLE with table-cells in row at ROW-INDEX filled with blanks."
  (om--table-replace-row row-index (om-build-table-row! '(" ")) table))

(defun om--table-clear-column (column-index table)
  "Return TABLE with table-cells in column at COLUMN-INDEX filled with blanks."
  (om--table-replace-column column-index `(,(om-build-table-cell " ")) table))

;;; COMPOSITE BUILDERS

;;; misc builders

;; these are nodes that cannot and should not be built with
;; the 'normal' build functions because they are too weird. TBH they
;; should probably be their own types but that's not what
;; `org-element.el' does

;; ...and this is here because I can't think of where else to put it
(defun om-clone-node (node)
  "Return copy of NODE, which may be a circular tree.

This is only necessary to copy nodes parsed using any of parsing
functions from this this package (example, `om-parse-this-headline')
because these retain parent references which makes the node a circular
list. None of the builder functions add parent references, so
`copy-tree' will be a faster alternative to this function."
  (let ((print-circle t))
    (->> node (format "%S") (read))))

(om--defun-kw om-build-timestamp-diary (form &key post-blank)
  "Return a new diary-sexp timestamp node from FORM.
Optionally set POST-BLANK (a positive integer)."
  (->> (om--build-object 'timestamp post-blank)
       (om--set-property-nocheck :type 'diary)
       (om-timestamp-diary-set-value form)
       (om--set-properties-nocheck-nil
        (list :repeater-type :repeater-unit :repeater-value
              :warning-type :warning-unit :warning-value :year-start
              :month-start :day-start :hour-start :minute-start
              :year-end :month-end :day-end :hour-end :minute-end))))

(om--defun-kw om-build-table-row-hline (&key post-blank)
  "Return a new rule-typed table-row node.
Optionally set POST-BLANK (a positive integer)."
  (->> (om--build-container-element 'table-row post-blank nil)
       (om--set-property-nocheck :type 'rule)))

;;; shorthand builders

;; These function offer a shorter and more convenient way of building
;; nodes. They all end in '!' (and all associated functions later
;; that replicate their syntax here do the same)

(defun om-build-secondary-string! (string)
  "Return a secondary string (list of object nodes) from STRING.
STRING is any string that contains a textual representation of
object nodes. If the string does not represent a list of object nodes,
throw an error."
  ;; fool parser to always parse objects, bold will parse to headlines
  ;; because of the stars
  (-if-let (ss (->> (om--from-string (concat " " string))
                    (om--get-descendent '(0))
                    (om-get-children)))
      (cond
       ((--any? (om-is-any-type om-elements it) ss)
        (om--arg-error "Secondary string must only contain objects"))
       ((equal (car ss) " ")
        (-drop 1 ss))
       (t (om--map-first* (substring it 1) ss)))
    (om--arg-error "Could not make secondary string from %S" string)))

(om--defun-kw om-build-timestamp! (start &key end active repeater
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
and -unit properties in `om-build-timestamp'.

Building a diary sexp timestamp is not possible with this function."
  (->> (om--build-object 'timestamp post-blank)
       (om--timestamp-set-start-time-nocheck start)
       (om--timestamp-set-end-time-nocheck end)
       (om--timestamp-set-active active)
       (om--timestamp-set-warning warning)
       (om--timestamp-set-repeater repeater)
       (om--set-property-nocheck-nil :raw-value)))

(om--defun-kw om-build-clock! (start &key end post-blank)
  "Return a new clock node.

START and END follow the same rules as their respective arguments in
`om-build-timestamp!'."
  (let ((ts (om-build-timestamp! start :end end)))
    (om-build-clock ts :post-blank post-blank)))

(om--defun-kw om-build-planning! (&key closed deadline scheduled
                                       post-blank)
  "Return a new planning node.

CLOSED, DEADLINE, and SCHEDULED are lists with the following structure
\(brackets denote optional members):

\(YEAR MINUTE DAY [HOUR] [MIN]
 [&warning TYPE VALUE UNIT]
 [&repeater TYPE VALUE UNIT])

In terms of arguments supplied to `om-build-timestamp!', the first
five members correspond to the list supplied as TIME, and the TYPE,
VALUE, and UNIT fields correspond to the lists supplied to WARNING and
REPEATER arguments. The order of warning and repeater does not
matter."
  (om-build-planning
   :closed (om--planning-list-to-timestamp closed)
   :deadline (om--planning-list-to-timestamp deadline)
   :scheduled (om--planning-list-to-timestamp scheduled)
   :post-blank post-blank))

;; TODO check keyvals somehow
(om--defun-kw om-build-property-drawer! (&key post-blank &rest keyvals)
  "Return a new property-drawer node.

Each member in KEYVALS is a list of symbols like (KEY VAL), where each
list will generate a node-property node in the property-drawer node
like \":key: val\"."
  (->> keyvals
       (--map (let ((key (symbol-name (car it)))
                    (val (symbol-name (cadr it))))
                (om-build-node-property key val)))
       (apply #'om-build-property-drawer :post-blank post-blank)))

(om--defun-kw om-build-headline! (&key (level 1) title-text
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
`om-build-planning!'. Up to all three planning types can be used
in the same list like (:closed ARGS :deadline ARGS :scheduled ARGS).

STATISTICS-COOKIE is a list following the same format as
`om-build-statistics-cookie'.

SECTION-CHILDREN is a list of elements that will go in the headline
section.

SUBHEADLINES contains zero or more headlines that will go under the
created headline. The level of all members in SUBHEADLINES will
automatically be adjusted to LEVEL + 1.

All arguments not mentioned here follow the same rules as
`om-build-headline'"
  (let* ((planning (-some->> planning (apply #'om-build-planning!)))
         (section (-some->>
                   (append `(,planning) section-children)
                   (-non-nil)
                   (apply #'om-build-section)))
         (nodes (->> subheadlines
                     (--map (om--headline-set-level (1+ level) it))
                     (append (list section))
                     (-non-nil))))
    (->> (apply #'om-build-headline
                :todo-keyword todo-keyword
                :level level
                :tags tags
                :post-blank post-blank
                :pre-blank pre-blank
                :priority priority
                :commentedp commentedp
                :archivedp archivedp
                nodes)
         (om-headline-set-title! title-text statistics-cookie))))

(om--defun-kw om-build-paragraph! (string &key post-blank)
  "Return a new paragraph node from STRING.

STRING is the text to be parsed into a paragraph and must contain
valid textual representations of object nodes."
  (->> (om-build-secondary-string! string)
       (apply #'om-build-paragraph :post-blank (or post-blank 0))))

(om--defun-kw om-build-item! (&key post-blank bullet checkbox tag
                                   paragraph counter &rest children)
  "Return a new item node.

TAG is a string representing the tag (make with
`om-build-secondary-string!') .

PARAGRAPH is a string that will be the initial text in the item
\(made with `om-build-paragraph!').

CHILDREN contains the nodes that will go under this item after
PARAGRAPH.

All other arguments follow the same rules as `om-build-item'."
  (let ((children* (or (-some-> paragraph
                                (om-build-paragraph!)
                                (cons children))
                       children))
        (tag (-some->> tag (om-build-secondary-string!))))
    (apply #'om-build-item
           :post-blank post-blank
           :bullet bullet
           :checkbox checkbox
           :counter counter
           :tag tag
           children*)))

(defun om-build-table-cell! (string)
  "Return a new table-cell node.

STRING is the text to be contained in the table-cell node. It must
contain valid textual representations of objects that are allowed in
table-cell nodes."
  (apply #'om-build-table-cell (om-build-secondary-string! string)))

(defun om-build-table-row! (row-list)
  "Return a new table-row node.

ROW-LIST is a list of strings to be built into table-cell nodes via
`om-build-table-cell!' (see that function for restrictions).
Alternatively, ROW-LIST may the symbol `hline' instead of a string to
create a rule-typed table-row."
  (if (eq row-list 'hline) (om-build-table-row-hline)
    (->> (-map #'om-build-table-cell! row-list)
         (apply #'om-build-table-row))))

(om--defun-kw om-build-table! (&key tblfm post-blank &rest row-lists)
  "Return a new table node.

ROW-LISTS is a list of lists where each member list will be converted
to a table-row node via `om-build-table-row!' (see that function for
restrictions).

All other arguments follow the same rules as `om-build-table'."
  (->> (--map (om-build-table-row! it) row-lists)
       (apply #'om-build-table :tblfm tblfm :post-blank post-blank)))

;;; logbook items

;; internal

(defun om--log-replace (placeholder string heading)
  "Return HEADING with PLACEHOLDER replaced by STRING."
  (->> (cons placeholder string)
       (list)
       (org-replace-escapes heading)))

(defun om--log-replace-new (string heading)
  "Return HEADING with placeholder \"%s\" replaced by STRING."
  (--> (format "\"%s\"" string)
       (om--log-replace "%s" it heading)))

(defun om--log-replace-old (string heading)
  "Return HEADING with placeholder \"%S\" replaced by STRING."
  (--> (format "\"%s\"" string)
       (om--log-replace "%S" it heading)))

(defun om--log-replace-new-state (state heading)
  "Return HEADING with placeholder \"%s\" replaced by string STATE."
  (om--log-replace-new state heading))

(defun om--log-replace-old-state (state heading)
  "Return HEADING with placeholder \"%S\" replaced by string STATE."
  (om--log-replace-old state heading))

(defun om--log-replace-new-timestamp (timestamp heading)
  "Return HEADING with placeholder \"%s\" replaced by TIMESTAMP.
TIMESTAMP is a timestamp node and will be converted to an inactive
timestamp if active."
  (-> (om-timestamp-set-active nil timestamp)
      (om-to-string)
      (om--log-replace-new heading)))

(defun om--log-replace-old-timestamp (timestamp heading)
  "Return HEADING with placeholder \"%S\" replaced by TIMESTAMP.
TIMESTAMP is a timestamp node and will be converted to an inactive
timestamp if active."
  (-> (om-timestamp-set-active nil timestamp)
      (om-to-string)
      (om--log-replace-old heading)))

(defun om--log-replace-timestamp (unixtime active-p long-p heading)
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
        (time (if long-p (om-unixtime-to-time-long unixtime)
                (om-unixtime-to-time-short unixtime))))
    (--> (om-build-timestamp! time :active active-p)
         (om-to-string it)
         (om--log-replace key it heading))))

(defun om--log-replace-username (username heading)
  "Return HEADING with \"%u\" replaced by symbol USERNAME."
  (om--log-replace "%u" username heading))

(defun om--log-replace-full-username (full-username heading)
  "Return HEADING with \"%U\" replaced by symbol FULL-USERNAME."
  (om--log-replace "%U" full-username heading))

(defun om--log-get (type)
  "Return the log heading associated with symbol TYPE.
This function will only use the default value of
`org-log-note-headings' and is thus a pure function."
  (alist-get type (default-value 'org-log-note-headings)))

(defun om--build-log-item (note heading)
  "Return an item with string HEADING as its first line.
If string NOTE is supplied, append this after a newline object node
in the first paragraph of the returned item."
  (->> (if note (format "%s \\\\\n  %s" heading note) heading)
       (om-build-paragraph!)
       (om-build-item)))

(defun om--build-log-item-trans (type unixtime old-timestamp note)
  "Return an item for any of the transition log entry types.
These are re/del-schedule/deadline (specified with TYPE) transitioning
from OLD-TIMESTAMP at UNIXTIME with optionally supplied NOTE."
  (->> (om--log-get type)
       (om--log-replace-old-timestamp old-timestamp)
       (om--log-replace-timestamp unixtime nil t)
       (om--build-log-item note)))

;; public

(defun om-build-log-done (unixtime &optional note)
  "Return an item node for a done log entry.

This will format the log entry from the default value for the
'done` cell in `org-log-note-headings'.

UNIXTIME is an integer representing the time to be used for all
timestamp nodes.

If string NOTE is supplied, append a note to the log entry."
  (->> (om--log-get 'done)
       (om--log-replace-timestamp unixtime nil t)
       (om--build-log-item note)))

(defun om-build-log-state (unixtime new-state old-state &optional note)
  "Return an item node for a state change log entry.

This will format the log entry from the default value for the
'state` cell in `org-log-note-headings'.

UNIXTIME is an integer representing the time to be used for all
timestamp nodes.

NEW-STATE and OLD-STATE are strings for the new and old todo keywords
respectively.

If string NOTE is supplied, append a note to the log entry."
  (->> (om--log-get 'state)
       (om--log-replace-new-state new-state)
       (om--log-replace-old-state old-state)
       (om--log-replace-timestamp unixtime nil t)
       (om--build-log-item note)))

(defun om-build-log-note (unixtime note)
  "Return an item node for a new note log entry.

This will format the log entry from the default value for the
'note` cell in `org-log-note-headings'.

UNIXTIME is an integer representing the time to be used for all
timestamp nodes.

NOTE is a string for the note text."
  (->> (om--log-get 'note)
       (om--log-replace-timestamp unixtime nil t)
       (om--build-log-item note)))

(defun om-build-log-reschedule (unixtime old-timestamp &optional note)
  "Return an item node for a new schedule log entry.

This will format the log entry from the default value for the
'reschedule` cell in `org-log-note-headings'.

UNIXTIME is an integer representing the time to be used for all
timestamp nodes.

OLD-TIMESTAMP is a timestamp node of the schedule that is being
deleted. It will always be converted to an inactive timestamp.

If string NOTE is supplied, append a note to the log entry."
  (om--build-log-item-trans 'reschedule unixtime old-timestamp note))

(defun om-build-log-delschedule (unixtime old-timestamp &optional note)
  "Return an item node for a delete schedule log entry.

This will format the log entry from the default value for the
'delschedule` cell in `org-log-note-headings'.

UNIXTIME is an integer representing the time to be used for all
timestamp nodes.

OLD-TIMESTAMP is a timestamp node of the schedule that is being
deleted. It will always be converted to an inactive timestamp.

If string NOTE is supplied, append a note to the log entry."
  (om--build-log-item-trans 'delschedule unixtime old-timestamp note))

(defun om-build-log-redeadline (unixtime old-timestamp &optional note)
  "Return an item node for a new deadline log entry.

This will format the log entry from the default value for the
'redeadline` cell in `org-log-note-headings'.

UNIXTIME is an integer representing the time to be used for all
timestamp nodes.

OLD-TIMESTAMP is a timestamp node of the deadline that is being
deleted. It will always be converted to an inactive timestamp.

If string NOTE is supplied, append a note to the log entry."
  (om--build-log-item-trans 'redeadline unixtime old-timestamp note))

(defun om-build-log-deldeadline (unixtime old-timestamp &optional note)
  "Return an item node for a delete deadline log entry.

This will format the log entry from the default value for the
'deldeadline` cell in `org-log-note-headings'.

UNIXTIME is an integer representing the time to be used for all
timestamp nodes.

OLD-TIMESTAMP is a timestamp node of the deadline that is being
deleted. It will always be converted to an inactive timestamp.

If string NOTE is supplied, append a note to the log entry."
  (om--build-log-item-trans 'deldeadline unixtime old-timestamp note))

(defun om-build-log-refile (unixtime &optional note)
  "Return an item node for a refile log entry.
This will format the log entry from the default value for the
'deldeadline` cell in `org-log-note-headings'.

UNIXTIME is an integer representing the time to be used for all
timestamp nodes.

If string NOTE is supplied, append a note to the log entry."
  (->> (om--log-get 'refile)
       (om--log-replace-timestamp unixtime nil t)
       (om--build-log-item note)))

(om--defun-kw om-build-log-type (type &key old new unixtime username
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
                   ((om-is-type 'timestamp rep)
                    (intern (format "om--log-replace-%s-timestamp" type)))
                   ((stringp rep)
                    (intern (format "om--log-replace-%s-state" type)))
                   (t
                    (om--arg-error "Must be string or timestamp: Got %S" rep)))))
            (funcall fun rep note))))
       (replace-timestamps
        (heading)
        (if (not unixtime) heading
          (->> heading
               (om--log-replace-timestamp unixtime nil nil)
               (om--log-replace-timestamp unixtime t nil)
               (om--log-replace-timestamp unixtime nil t)
               (om--log-replace-timestamp unixtime t t)))))
    (--> (alist-get type org-log-note-headings)
         (replace-timestamps it)
         (replace-note t old it)
         (replace-note nil new it)
         (if username (om--log-replace-username username it) it)
         (if full-username
             (om--log-replace-full-username full-username it)
           it)
         (om--build-log-item note it))))

;;; PUBLIC TYPE FUNCTIONS

(defun om-get-type (node)
  "Return the type of NODE."
  (org-element-type node))

(defun om-is-type (type node)
  "Return t if the type of NODE is TYPE (a symbol)."
  (unless (memq type om-nodes)
    (om--arg-error "Argument 'type' must be in `om-nodes': Got %s" type))
  (eq (om-get-type node) type))

(defun om-is-any-type (types node)
  "Return t if the type of NODE is in TYPES (a list of symbols)."
  (-some->>
   (-difference types om-nodes)
   (om--arg-error
    "All in 'types' must be in `om-nodes'; these were not: %s"))
  (if (memq (om-get-type node) types) t))

(defun om-is-element (node)
  "Return t if NODE is an element class."
  (om-is-any-type om-elements node))

(defun om-is-branch-node (node)
  "Return t if NODE is a branch node."
  (om-is-any-type om-branch-nodes node))

(defun om-node-may-have-child-objects (node)
  "Return t if NODE is a branch node that may have child objects."
  (om-is-any-type om-branch-nodes-permitting-child-objects node))

(defun om-node-may-have-child-elements (node)
  "Return t if NODE is a branch node that may have child elements.

Note this implies that NODE is also of class element since only
elements may have other elements as children."
  (om-is-any-type om-branch-elements-permitting-child-elements node))

;;; PUBLIC PROPERTY FUNCTIONS

;;; polymorphic

(defun om-contains-point-p (point node)
  "Return t if POINT is within the boundaries of NODE."
  (-let (((&plist :begin :end) (om--get-all-properties node)))
    (if (and (integerp begin) (integerp end))
        (<= begin point end)
      (error "Node boundaries are not defined"))))

(defun om-set-property (prop value node)
  "Return NODE with PROP set to VALUE.

See builder functions for a list of properties and their rules for
each type."
  (let ((type (om-get-type node)))
    (-if-let (pred (om--get-property-attribute :pred type prop))
        (if (funcall pred value)
            (let* ((encode-fun (om--get-property-encoder type prop))
                   (update-fun (om--get-property-cis-function type prop)))
              (-->
               (if encode-fun (funcall encode-fun value) value)
               (om--set-property-nocheck prop it node)
               (if update-fun (funcall update-fun it) it)))
          (om--arg-error
           "Property '%s' in node of type '%s' must be %s. Got '%S'"
           prop type (om--get-property-type-desc type prop) value))
      (om--arg-error "Property '%s' is unsettable for type '%s'"
                     prop type))))

(defun om-set-properties (plist node)
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
          (-if-let (pred (om--get-property-attribute :pred type prop))
              (if (funcall pred value)
                  (let ((encode-fun (om--get-property-encoder type prop)))
                    (->> (if encode-fun (funcall encode-fun value) value)
                         (funcall #'plist-put acc prop)))
                (om--arg-error
                 "Property '%s' in node of type '%s' must be %s. Got '%S'"
                 prop type (om--get-property-type-desc type prop) value))
            (om--arg-error
             "Property '%s' is unsettable for type '%s'"
             prop type)))))
    (if (om--is-plist plist)
        (let* ((cur-props (om--get-all-properties node))
               (type (om-get-type node))
               (keyvals (-partition 2 plist))
               (update-funs
                (->> (-map #'car keyvals)
                     (--map (om--get-property-cis-function type it))
                     (-uniq)
                     (-non-nil)))
               (node*
                (om--construct
                 (om-get-type node)
                 (--reduce-from (filter acc it type) cur-props keyvals)
                 (om-get-children node))))
          (if (not update-funs) node*
            (--reduce-from (funcall it acc) node* update-funs)))
      (om--arg-error "Not a plist: %S" plist))))

;; TODO add plural version of this...
(defun om-get-property (prop node)
  "Return the value of PROP of NODE."
  (let ((decoder-fun (-> (om-get-type node)
                         (om--get-property-decoder prop)))
        (value (om--get-property-nocheck prop node)))
    (if decoder-fun (funcall decoder-fun value) value)))

(om--defun* om-map-property (prop fun node)
  "Return NODE with FUN applied to the value of PROP.

FUN is a unary function which takes the current value of PROP and
returns a new value to which PROP will be set.

See builder functions for a list of properties and their rules for
each type."
  (--> (om-get-property prop node)
       (funcall fun it)
       (om-set-property prop it node)))

(defun om-map-properties (plist node)
  "Return NODE with functions applied to the values of properties.

PLIST is a property list where the keys are properties of NODE and
its values are unary functions to be mapped to these properties.

See builder functions for a list of properties and their rules for
each type."
  (cond
   ((not plist) node)
   ((om--is-plist plist)
    (->> (om-map-property (nth 0 plist) (nth 1 plist) node)
         (om-map-properties (-drop 2 plist))))
   (t (om--arg-error "Not a plist: %s" plist))))

(defmacro om-map-properties* (plist node)
  "Anaphoric form of `om-map-properties'.

PLIST is a property list where the keys are properties of NODE and
its values are forms to be mapped to these properties."
  (declare (debug (form form)))
  (let ((p (make-symbol "plist*")))
    `(let ((,p (om--plist-map-values (lambda (form) `(lambda (it) ,form)) ',plist)))
       (om-map-properties ,p ,node))))

(defun om-toggle-property (prop node)
  "Return NODE with the value of PROP flipped.

This function only applies to properties that are booleans."
  (let ((type (om-get-type node)))
    (if (om--get-property-attribute :toggle type prop)
        (om-map-property prop #'not node)
      (om--arg-error "Not a toggle-able property"))))

(defun om-shift-property (prop n node)
  "Return NODE with PROP shifted by N (an integer).

This only applies the properties that are represented as integers."
  (let* ((type (om-get-type node))
         (fun (om--get-property-attribute :shift type prop)))
    (if fun
        (om-map-property* prop (funcall fun n it) node)
      (om--arg-error "Not a shiftable property"))))

(defun om-insert-into-property (prop index string node)
  "Return NODE with STRING inserted at INDEX into PROP.

This only applies to properties that are represented as lists of
strings."
  (cl-flet
      ((insert-at-maybe
        (string-list)
        (if (member string string-list) string-list
          (om--insert-at index string string-list))))
    (let ((type (om-get-type node)))
      (if (om--get-property-attribute :string-list type prop)
          (om-map-property prop #'insert-at-maybe node)
        (om--arg-error "Property '%s' in node of type '%s' is not a string-list"
                       prop type)))))

(defun om-remove-from-property (prop string node)
  "Return NODE with STRING removed from PROP if present.

This only applies to properties that are represented as lists of
strings.

See `om-insert-into-property' for a list of supported elements
and properties that may be used with this function."
  (let ((type (om-get-type node)))
    (if (om--get-property-attribute :string-list type prop)
        (om-map-property* prop (-remove-item string it) node)
      (om--arg-error "Property '%s' in node of type '%s' is not a string-list"
                     prop type))))

(defun om-plist-put-property (prop key value node)
  "Return NODE with VALUE corresponding to KEY inserted into PROP.

KEY is a keyword and VALUE is a symbol. This only applies to
properties that are represented as plists."
  (let ((type (om-get-type node)))
    (if (om--get-property-attribute :plist type prop)
        (om-map-property* prop (plist-put it key value) node)
      (om--arg-error "Not a plist property"))))

(defun om-plist-remove-property (prop key node)
  "Return NODE with KEY and its corresponding value removed from PROP.

KEY is a keyword. This only applies to properties that are
represented as plists.

See `om-plist-put-property' for a list of supported elements
and properties that may be used with this function."
  (let ((type (om-get-type node)))
    (if (om--get-property-attribute :plist type prop)
        (om-map-property* prop (om--plist-remove key it) node)
      (om--arg-error "Not a plist property"))))

;; update polymorphic property function documentation

;; TODO these docstrings suck :(
(defun om--get-types-with-property-attribute (attr)
  "Return alist of all nodes types that contain ATTR."
  (->> om--node-property-alist
       (--map (cons (car it) (--filter (plist-get (cdr it) attr) (cdr it))))
       (-filter #'cdr)))

(defun om--format-alist-operations (type-alist)
  "Return a formatted string of TYPE-ALIST."
  (->> type-alist
       (--map (cons (car it) (-map #'car (cdr it))))
       (--map (format "\n%s\n%s"
                      (car it)
                      (s-join "\n" (--map (format "- %S" it) (cdr it)))))
       (s-join "\n")))

(defun om--append-documentation (fun string)
  "Append STRING to the docstring of FUN."
  (let ((msg "\n\nThe following types and properties are supported:\n"))
    (--> (documentation fun)
         (concat it msg string)
         (function-put fun 'function-documentation it))))

(->> (om--get-types-with-property-attribute :toggle)
     (om--format-alist-operations)
     (om--append-documentation 'om-toggle-property))

(->> (om--get-types-with-property-attribute :shift)
     (--map (cons (car it) (--remove (eq :post-blank (car it)) (cdr it))))
     (-filter #'cdr)
     (om--format-alist-operations)
     (concat "\nall elements\n- :post-blank\n")
     (om--append-documentation 'om-shift-property))

(->> (om--get-types-with-property-attribute :string-list)
     (om--format-alist-operations)
     (om--append-documentation 'om-insert-into-property))

(->> (om--get-types-with-property-attribute :plist)
     (om--format-alist-operations)
     (om--append-documentation 'om-plist-put-property))

;;; object nodes
;;
;; entity

(defun om-entity-get-replacement (key entity)
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
      (->> (om-get-property :name entity)
           (org-entity-get)
           (cdr)
           (nth index))
    (om--arg-error "Invalid encoding requested: %s" index)))

;; statistics-cookie

(defun om-statistics-cookie-is-complete (statistics-cookie)
  "Return t is STATISTICS-COOKIE node is complete."
  (let ((val (om--get-property-nocheck :value statistics-cookie)))
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

(defun om-timestamp-get-start-time (timestamp)
  "Return the time list for start time of TIMESTAMP node.
The return value will be a list as specified by the TIME argument in
`om-build-timestamp!'."
  (om--timestamp-get-start-time timestamp))

(defun om-timestamp-get-end-time (timestamp)
  "Return the end time list for end time of TIMESTAMP or nil if not a range.
The return value will be a list as specified by the TIME argument in
`om-build-timestamp!'."
  (and (om--timestamp-is-ranged timestamp)
       (om--timestamp-get-end-time timestamp)))

(defun om-timestamp-get-range (timestamp)
  "Return the range of TIMESTAMP node in seconds as an integer.
If non-ranged, this function will return 0. If ranged but
the start time is in the future relative to end the time, return
a negative integer."
  (om--timestamp-get-range timestamp))

(defun om-timestamp-is-active (timestamp)
  "Return t if TIMESTAMP node is active."
  (or (om--property-is-eq :type 'active timestamp)
      (om--property-is-eq :type 'active-range timestamp)))

(defun om-timestamp-is-ranged (timestamp)
  "Return t if TIMESTAMP node is ranged."
  (or (om--property-is-eq :type 'active-range timestamp)
      (om--property-is-eq :type 'inactive-range timestamp)))

(defun om-timestamp-range-contains-p (unixtime timestamp)
  "Return t if UNIXTIME is between start and end time of TIMESTAMP node.
The boundaries are inclusive. If TIMESTAMP has a range of zero, then
only return t if UNIXTIME is the same as TIMESTAMP. TIMESTAMP will be
interpreted according to the localtime of the operating system."
  (let ((ut1 (om--timestamp-get-start-unixtime timestamp))
        (ut2 (om--timestamp-get-end-unixtime timestamp)))
    (<= ut1 unixtime ut2)))

(defun om-timestamp-set-start-time (time timestamp)
  "Return TIMESTAMP node with start time set to TIME.
TIME is a list analogous to the same argument specified in
`om-build-timestamp!'."
  (om--timestamp-set-start-time time timestamp))

(defun om-timestamp-set-end-time (time timestamp)
  "Return TIMESTAMP node with end time set to TIME.
TIME is a list analogous to the same argument specified in
`om-build-timestamp!'."
  (om--timestamp-set-end-time time timestamp))

(defun om-timestamp-set-single-time (time timestamp)
  "Return TIMESTAMP node with start and end times set to TIME.
TIME is a list analogous to the same argument specified in
`om-build-timestamp!'."
  (om--timestamp-set-single-time time timestamp))

(defun om-timestamp-set-double-time (time1 time2 timestamp)
  "Return TIMESTAMP node with start/end times set to TIME1/TIME2 respectively.
TIME1 and TIME2 are lists analogous to the TIME argument specified in
`om-build-timestamp!'."
  (om--timestamp-set-double-time time1 time2 timestamp))

(defun om-timestamp-set-range (range timestamp)
  "Return TIMESTAMP node with range set to RANGE.
If TIMESTAMP is ranged, keep start time the same and adjust the end
time. If not, make a new end time. The units for RANGE are in minutes
if TIMESTAMP is in long format and days if TIMESTAMP is in short
format."
  (om--timestamp-set-range range timestamp))

(defun om-timestamp-set-active (flag timestamp)
  "Return TIMESTAMP node with active type if FLAG is t."
  (om--timestamp-set-active flag timestamp))

(defun om-timestamp-shift (n unit timestamp)
  "Return TIMESTAMP node with time shifted by N UNIT's.

This function will move the start and end times together; therefore
ranged inputs will always output ranged timestamps and same for
non-ranged. To move the start and end time independently, use
`om-timestamp-shift-start' or `om-timestamp-shift-end'.

N is a positive or negative integer and UNIT is one of `minute',
`hour', `day', `month', or `year'. Overflows will wrap around
transparently; for instance, supplying `minute' for UNIT and 90 for N
will increase the hour property by 1 and the minute property by 30."
  (->> (om--timestamp-shift-start n unit timestamp)
       (om--timestamp-shift-end n unit)))

(defun om-timestamp-shift-start (n unit timestamp)
  "Return TIMESTAMP node with start time shifted by N UNIT's.

N and UNIT behave the same as those in `om-timestamp-shift'.

If TIMESTAMP is not range, the output will be a ranged timestamp with
the shifted start time and the end time as that of TIMESTAMP. If this
behavior is not desired, use `om-timestamp-shift'."
  (om--timestamp-shift-start n unit timestamp))

(defun om-timestamp-shift-end (n unit timestamp)
  "Return TIMESTAMP node with end time shifted by N UNIT's.

N and UNIT behave the same as those in `om-timestamp-shift'.

If TIMESTAMP is not range, the output will be a ranged timestamp with
the shifted end time and the start time as that of TIMESTAMP. If this
behavior is not desired, use `om-timestamp-shift'."
  (om--timestamp-shift-end n unit timestamp))

(defun om-timestamp-toggle-active (timestamp)
  "Return TIMESTAMP node with its active/inactive type flipped."
  (-> (om--timestamp-is-active timestamp)
      (not)
      (om--timestamp-set-active timestamp)))

(defun om-timestamp-truncate (timestamp)
  "Return TIMESTAMP node with start/end times forced to short format."
  (let ((t1 (->> (om--timestamp-get-start-time timestamp)
                    (om--time-truncate)))
        (t2 (->> (om--timestamp-get-end-time timestamp)
                  (om--time-truncate))))
    (om--timestamp-set-double-time t1 t2 timestamp)))

(defun om-timestamp-truncate-start (timestamp)
  "Return TIMESTAMP node with start time forced to short format."
  (let ((time (->> (om--timestamp-get-start-time timestamp)
                   (om--time-truncate))))
    (om--timestamp-set-start-time time timestamp)))

(defun om-timestamp-truncate-end (timestamp)
  "Return TIMESTAMP node with end time forced to short format."
  (let ((time (->> (om--timestamp-get-end-time timestamp)
                   (om--time-truncate))))
    (om--timestamp-set-end-time time timestamp)))

(defun om-timestamp-set-collapsed (flag timestamp)
  "Return TIMESTAMP with collapsed set to FLAG.

If timestamp is ranged but not outside of one day, it may be collapsed
\(FLAG is t) to short format like [yyyy-mm-dd xxx hh:mm-hh:mm] or
expanded (FLAG is nil) to long format like [yyyy-mm-dd xxx
hh:mm]--[yyyy-mm-dd xxx hh:mm]. If these conditions are not met,
return TIMESTAMP untouched regardless of FLAG.

Note: the default for all timestamp functions in `om.el' is to favor
collapsed format."
  (if (and (not (om--timestamp-is-ranged-lowres timestamp))
           (om--timestamp-is-ranged timestamp))
      (om--timestamp-set-type-ranged (not flag) timestamp)
    timestamp))

;; timestamp (diary)

(defun om-timestamp-diary-set-value (form timestamp-diary)
  "Return TIMESTAMP-DIARY node with value set to FORM.
The node must have a type `eq' to `diary'. FORM is a quoted list."
  (if (listp form)
      (om--set-property-nocheck :raw-value (format "<%%%%%S>" form)
                                timestamp-diary)
    (om--arg-error "Timestamp-diary node value must be a form: Got %S"
                   form)))

;;; element nodes
;;
;; clock

(defun om-clock-is-running (clock)
  "Return t if CLOCK element is running (eg is open)."
  (om--property-is-eq :status 'running clock))

;; headline

(defun om-headline-get-statistics-cookie (headline)
  "Return the statistics cookie node from HEADLINE if it exists."
  (->> (om--get-property-nocheck :title headline)
       (-last-item)
       (om--filter-type 'statistics-cookie)))

(defun om-headline-is-done (headline)
  "Return t if HEADLINE node has a done todo-keyword."
  (-> (om--get-property-nocheck :todo-keyword headline)
      (member org-done-keywords)
      (and t)))

(defun om-headline-has-tag (tag headline)
  "Return t if HEADLINE node is tagged with TAG."
  (if (member tag (om--get-property-nocheck :tags headline)) t))

(defun om-headline-set-title! (title-text stats-cookie-value headline)
  "Return HEADLINE node with new title.

TITLE-TEXT is a string to be parsed into object nodes for the title
via `om-build-secondary-string!' (see that function for restrictions)
and STATS-COOKIE-VALUE is a list described in
`om-build-statistics-cookie'."
  (let ((ss (om-build-secondary-string! title-text)))
    (if (not stats-cookie-value)
        (om-set-property :title ss headline)
      (let ((ss* (om--map-last*
                  (om--set-property-nocheck :post-blank 1 it) ss))
            (sc (om-build-statistics-cookie stats-cookie-value)))
        (om-set-property :title (-snoc ss* sc) headline)))))

;; item

(defun om-item-toggle-checkbox (item)
  "Return ITEM node with its checkbox state flipped.
This only affects item nodes with checkboxes in the `on' or `off'
states; return ITEM node unchanged if the checkbox property is `trans'
or nil."
  (cl-case (om-get-property :checkbox item)
    ((or trans nil) item)
    ('on (om-set-property :checkbox 'off item))
    ('off (om-set-property :checkbox 'on item))
    (t (error "This should not happen"))))

;; planning

(defun om-planning-set-timestamp! (prop planning-list planning)
  "Return PLANNING node with PROP set to PLANNING-LIST.

PROP is one of `:closed', `:deadline', or `:scheduled'. PLANNING-LIST
is the same as that described in `om-build-planning!'."
  (unless (memq prop '(:closed :deadline :scheduled))
    (om--arg-error "PROP must be ':closed', ':deadline', or ':scheduled'. Got %S" prop))
  (let ((ts (om--planning-list-to-timestamp planning-list)))
    (om-set-property prop ts planning)))

;;; PUBLIC BRANCH/CHILD FUNCTIONS

;;; polymorphic

(defun om-children-contain-point (point branch-node)
  "Return t if POINT is within the boundaries of BRANCH-NODE's children."
  (-let (((&plist :contents-begin :contents-end)
          (om--get-all-properties branch-node)))
    (if (and (integerp contents-begin) (integerp contents-end))
        (<= contents-begin point contents-end)
      (error "Node boundaries are not defined"))))

(defun om-get-children (branch-node)
  "Return the children of BRANCH-NODE as a list."
  (org-element-contents branch-node))

(defun om-set-children (children branch-node)
  "Return BRANCH-NODE with its children set to CHILDREN.
CHILDREN is a list of nodes; the types permitted in this list depend
on the type of NODE."
  (let ((type (om-get-type branch-node)))
    (-if-let (child-types (alist-get type om--node-restrictions))
        (-if-let (illegal (-difference (-map #'om-get-type children)
                                       child-types))
            (om--set-childen-throw-error type child-types illegal)
          (om--set-children-nocheck children branch-node))
      ;; this should not happen
      (error "Child type restrictions not found for %s" type))))

(om--defun* om-map-children (fun branch-node)
  "Return BRANCH-NODE with FUN applied to its children.
FUN is a unary function that takes the current list of children and
returns a modified list of children."
  (--> (om-get-children branch-node)
       (funcall fun it)
       (om-set-children it branch-node)))

(defun om-is-childless (branch-node)
  "Return t if BRANCH-NODE has no children."
  (not (om-get-children branch-node)))

;;; objects

(defun om--normalize-secondary-string (secondary-string)
  "Return SECONDARY-STRING with all adjacent strings concatenated."
  (cl-flet
      ((concat-maybe
        (acc node)
        (let ((last (car acc)))
          (if (and (om-is-type 'plain-text last)
                   (om-is-type 'plain-text node))
              (cons (concat last node) (cdr acc))
            (cons node acc)))))
    (reverse (-reduce-from #'concat-maybe nil secondary-string))))

(defmacro om--mapcat-normalize (form secondary-string)
  "Return mapped, concatenated, and normalized SECONDARY-STRING.
FORM is a form supplied to `--mapcat'."
  (declare (debug (def-form form)))
  `(->> (--mapcat ,form ,secondary-string)
        (om--normalize-secondary-string)))

(defun om-unwrap (object-node)
  "Return the children of OBJECT-NODE as a secondary string.
If OBJECT-NODE is a plain-text node, wrap it in a list and return.
Else add the post-blank property of OBJECT-NODE to the last member
of its children and return children as a secondary string."
  (if (om-is-type 'plain-text object-node)
      (list object-node)
    (let ((children (om-get-children object-node))
          (post-blank (om--get-property-nocheck :post-blank object-node)))
      (om--map-last* (om-map-property* :post-blank
                       (+ it post-blank) it)
        children))))

(defun om-unwrap-types-deep (types object-node)
  "Return the children of OBJECT-NODE as a secondary string.
If OBJECT-NODE is a plain-text node, wrap it in a list and return.
Else recursively descend into the children of OBJECT-NODE and splice
the children of nodes with type in TYPES in place of said node and
return the result as a secondary string."
  (cond
   ((om-is-type 'plain-text object-node)
    (list object-node))
   ((om-is-any-type types object-node)
    (let* ((children (om-get-children object-node))
           (post-blank (om--get-property-nocheck :post-blank object-node)))
      (->> children
           (om--mapcat-normalize (om-unwrap-types-deep types it))
           (om--map-last* (om-map-property* :post-blank
                            (+ it post-blank) it)))))
   (t
    (->> object-node
         (om-map-children*
           (om--mapcat-normalize (om-unwrap-types-deep types it) it))
         (list)))))

(defun om-unwrap-deep (object-node)
  "Return the children of OBJECT-NODE as plain-text wrapped in a list."
  (om-unwrap-types-deep om-nodes object-node))

;;; secondary strings

(defun om-flatten (secondary-string)
  "Return SECONDARY-STRING with its first level unwrapped.
The unwrap operation will be done with `om-unwrap'."
  (om--mapcat-normalize (om-unwrap it) secondary-string))

(defun om-flatten-types-deep (types secondary-string)
  "Return SECONDARY-STRING with object nodes in TYPES unwrapped.
The unwrap operation will be done with `om-unwrap-types-deep'."
  (om--mapcat-normalize (om-unwrap-types-deep types it) secondary-string))

(defun om-flatten-deep (secondary-string)
  "Return SECONDARY-STRING with all object nodes unwrapped to plain-text.
The unwrap operation will be done with `om-unwrap-deep'."
  (om--mapcat-normalize (om-unwrap-deep it) secondary-string))

;;; headline

(defun om-headline-get-section (headline)
  "Return children of section node in HEADLINE node or nil if none."
  (-some->> (om-get-children headline)
            (assoc 'section)
            (om-get-children)))

(defun om-headline-set-section (children headline)
  "Return HEADLINE with section node containing CHILDREN.
If CHILDREN is nil, return HEADLINE with no section node."
  (om--map-children-nocheck*
    (let ((subheadlines (--filter (om-is-type 'headline it) it)))
      (if children
          (cons (apply #'om-build-section children) subheadlines)
        subheadlines))
    headline))

(om--defun* om-headline-map-section (fun headline)
  "Return HEADLINE node with child section node modified by FUN.

FUN is a unary function that takes a section node's children as a list
returns a modified child list."
  (--> (om-headline-get-section headline)
       (funcall fun it)
       (om-headline-set-section it headline)))

(defun om-headline-get-subheadlines (headline)
  "Return list of child headline nodes in HEADLINE node or nil if none."
  (-some->> (om-get-children headline)
            (--filter (om-is-type 'headline it))))

(defun om-headline-set-subheadlines (subheadlines headline)
  "Return HEADLINE node with SUBHEADLINES set to child subheadlines."
  (om--map-children-nocheck*
    (-if-let (section (assoc 'section it))
        (cons section subheadlines)
      subheadlines)
    headline))

(om--defun* om-headline-map-subheadlines (fun headline)
  "Return HEADLINE node with child headline nodes modified by FUN.

FUN is a unary function that takes a list of headlines and returns
a modified list of headlines."
  (--> (om-headline-get-subheadlines headline)
       (funcall fun it)
       (om-headline-set-subheadlines it headline)))

;;; headline (metadata)

;; planning

(defun om-headline-get-planning (headline)
  "Return the planning node in HEADLINE or nil if none."
  (-some--> (om-headline-get-section headline)
            (car it)
            (when (om-is-type 'planning it) it)))

(defun om-headline-set-planning (planning headline)
  "Return HEADLINE node with planning components set to PLANNING node."
  (if planning
      (om-headline-map-section*
        ;; if no section, build new section with planning in it
        (if (not it) (list planning)
          ;; if section, test if planning already in front and override
          ;; as needed
          (let ((r (if (om-is-type 'planning (car it)) (cdr it) it)))
            (cons planning r)))
        headline)
    ;; if `PLANNING' is nil, remove planning from section if present
    (om-headline-map-section*
      (--remove-first (om-is-type 'planning it) it)
      headline)))

(om--defun* om-headline-map-planning (fun headline)
  "Return HEADLINE node with planning node modified by FUN.

FUN is a unary function that takes a planning node and returns a
modified planning node."
   (--> (om-headline-get-planning headline)
        (funcall fun it)
        (om-headline-set-planning it headline)))

;; node-properties (eg the entire property drawer)

(defun om-headline-get-node-properties (headline)
  "Return a list of node-properties nodes in HEADLINE or nil if none."
  (-some--> (om-headline-get-section headline)
            ;; assume the property drawer is the first or second
            ;; child of section
            (if (om-is-type 'planning (car it)) (cdr it) it)
            (car it)
            (when (om-is-type 'property-drawer it)
              (om-get-children it))))

(defun om-headline-set-node-properties (node-properties headline)
  "Return HEADLINE node with property drawer containing NODE-PROPERTIES.
NODE-PROPERTIES is a list of node-property nodes."
  (if node-properties
      (om-headline-map-section*
        (let ((pd (apply #'om-build-property-drawer node-properties)))
          ;; if no section, build new section with prop-drwr in it
          (if (not it) (list pd)
            ;; the prop-drwr could either be the first child or second
            ;; if planning is in front
            (let ((first (nth 0 it))
                  (second (nth 1 it)))
              (cond
               ((and (om-is-type 'planning first)
                     (om-is-type 'property-drawer second))
                (-replace-at 1 pd it))
               ((om-is-type 'property-drawer first)
                (-replace-at 0 pd it))
               ((om-is-type 'planning first)
                (-insert-at 1 pd it))
               (t
                (cons pd it))))))
        headline)
    ;; if `NODE-PROPERTIES' is nil, remove from section if present
    (om-headline-map-section*
      (--remove-first (om-is-type 'property-drawer it) it)
      headline)))

(om--defun* om-headline-map-node-properties (fun headline)
  "Return HEADLINE node with property-drawer node modified by FUN.

FUN is a unary function that takes a property-drawer node and returns
a modified property-drawer node."
   (--> (om-headline-get-node-properties headline)
        (funcall fun it)
        (om-headline-set-node-properties it headline)))

;; node-property

(defun om-headline-get-node-property (key headline)
  "Return value of property with KEY in HEADLINE or nil if not found.
If multiple properties with KEY are present, only return the first."
  (->> (om-headline-get-node-properties headline)
       (--first (equal key (om-get-property :key it)))
       (om-get-property :value)))

(defun om-headline-set-node-property (key value headline)
  "Return HEADLINE with node property matching KEY set to VALUE.
If a property matching KEY is present, set it to VALUE. If multiple
properties matching KEY are present, only set the first."
  (om-headline-map-node-properties*
    (-if-let (np (-some->> value (om-build-node-property key)))
        (if (not it) (list np)
          ;; replace first np matching `KEY' or add to the front of
          ;; np's if not found
          (-if-let (i (--find-index (equal key (om-get-property :key it)) it))
              (-replace-at i np it)
            (cons np it)))
      ;; remove first property matching `KEY' if `VALUE' is nil
      (--remove-first (equal key (om-get-property :value it)) it))
    headline))

(om--defun* om-headline-map-node-property (key fun headline)
  "Return HEADLINE node with property value matching KEY modified by FUN.

FUN is a unary function that takes a node-property value and returns
a modified node-property value."
   (--> (om-headline-get-node-property key headline)
        (funcall fun it)
        (om-headline-set-node-property key it headline)))

;; logbook

(defun om-headline-get-logbook (headline)
  "Return the children of the logbook drawer of HEADLINE.
This function assumes that the logbook entries are in a drawer
immediately after planning and/or property-drawer nodes named
via `org-log-into-drawer'. If `org-log-into-drawer' is nil, always
return nil."
  ;; TODO this will not inherit the log-into-drawer property
  (-when-let (drawer-name (org-log-into-drawer))
    (-some-->
     (om-headline-get-section headline)
     (if (om-is-type 'planning (car it)) (cdr it) it)
     (if (om-is-type 'property-drawer (car it)) (cdr it) it)
     (car it)
     (and (om-is-type 'drawer it)
          (equal drawer-name (om-get-property :drawer-name it))
          (om-get-children it)))))

(defun om-headline-set-logbook (children headline)
  "Return HEADLINE with logbook drawer filled with CHILDREN.
CHILDREN must be a list of plain-list and/or clock nodes.

This function assumes that the logbook entries will be stored in a
drawer immediately after planning and/or property-drawer nodes named
via `org-log-into-drawer'. If `org-log-into-drawer' is nil, return
HEADLINE unmodified."
  (unless (--all? (om-is-any-type '(plain-list clock) it) children)
    (om--arg-error
     "Logbook must only contain clock or plain-list nodes. Got %s"
     children))
  (-if-let (drawer-name (org-log-into-drawer))
      (cl-flet
          ((is-logbook
            (node)
            (and (om-is-type 'drawer node)
                 (equal drawer-name (om-get-property :drawer-name node)))))
        (if children
            (om-headline-map-section*
              (let ((lb (apply #'om-build-drawer drawer-name children)))
                (if (not it) (list lb)
                  (-let (((n0 n1 n2) (-take 3 it)))
                    (cond
                     ((and (om-is-type 'planning n0)
                           (om-is-type 'property-drawer n1)
                           (is-logbook n2))
                      (-replace-at 2 lb it))
                     ((and (om-is-type 'planning n0)
                           (om-is-type 'property-drawer n1))
                      (-insert-at 2 lb it))
                     ((and (or (om-is-type 'planning n0)
                               (om-is-type 'property-drawer n0))
                           (is-logbook n1))
                      (-replace-at 1 lb it))
                     ((or (om-is-type 'planning n0)
                          (om-is-type 'property-drawer n0))
                      (-insert-at 1 lb it))
                     ((is-logbook n0)
                      (-replace-at 0 lb it))
                     (t
                      (cons lb it))))))
              headline)
          (om-headline-map-section*
            ;; TODO make this more specific
            (-remove-first #'is-logbook it)
            headline)))
    headline))

(om--defun* om-headline-map-logbook (fun headline)
  "Return HEADLINE node with property value matching KEY modified by FUN.

FUN is a unary function that takes a list of child nodes from the
logbook value and returns a modified list of child nodes.

This function assumes that the logbook entries will be stored in a
drawer immediately after planning and/or property-drawer nodes named
via `org-log-into-drawer'. If `org-log-into-drawer' is nil, return
HEADLINE unmodified."
  (if (org-log-into-drawer)
      (--> (om-headline-get-logbook headline)
           (funcall fun it)
           (om-headline-set-logbook it headline))
    headline))

(defun om-headline-logbook-append-entry (item headline)
  "Return HEADLINE with ITEM node appended to the front of its logbook.

The same assumptions and restrictions for `om-headline-map-logbook'
apply here."
  (om-headline-map-logbook*
    ;; if logbook starts with a plain-list, add item to front of
    ;; said plain list
    (if (om-is-type 'plain-list (car it))
        (om--map-first* (om-map-children* (cons item it) it) it)
      ;; else just append a new plain-list to the front
      (cons (om-build-plain-list item) it))
    headline))

(defun om-headline-logbook-append-open-clock (unixtime headline)
  "Return HEADLINE with an open clock append to front of its logbook.
UNIXTIME is an integer that will be used to build the clock node.

This does the functional equivalent of `org-clock-in' on the logbook."
  (om-headline-map-logbook*
    (-> (om-unixtime-to-time-long unixtime)
        (om-build-clock!)
        (cons it))
    headline))

(defun om-headline-logbook-close-open-clock (unixtime note headline)
  "Return HEADLINE with the first clock closed.

The clock will be closed to UNIXTIME, and NOTE will be appended
as a clock out note if supplied (as string). If no open clocks
are found, return HEADLINE unmodified.

This does the functional equivalent of `org-clock-out' on the logbook."
  (cl-flet
      ((close-clock
        (index logbook-children)
        (let ((time (om-unixtime-to-time-long unixtime)))
          (om--map-at* index
            (om-map-property* :value
              (->> (om-timestamp-set-end-time time it)
                   (om-timestamp-set-collapsed nil))
              it)
            logbook-children)))
       (add-note-maybe
        (index logbook-children)
        (if (not note) logbook-children
          (let* ((next (1+ index))
                 (target (nth next logbook-children))
                 (item (->> (om-build-paragraph note)
                            (om-build-item))))
            ;; if plain-list is after the clock being closed, add the
            ;; note to the front of the plain-list, otherwise insert
            ;; a new plain-list
            (if (om-is-type 'plain-list target)
                (om--map-at* next
                  (om-map-children* (cons item it) it)
                  logbook-children)
              (-insert-at next (om-build-plain-list item)
                          logbook-children))))))
    (om-headline-map-logbook*
      (-if-let (i (--find-index (and (om-is-type 'clock it)
                                     (om-clock-is-running it))
                                it))
          (->> it (close-clock i) (add-note-maybe i))
        it)
      headline)))

(defun om-headline-get-path (headline)
  "Return tree path of HEADLINE node.

The return value is a list of headline titles (including that from
HEADLINE) leading to the root node."
  (cl-labels
      ((get-path
        (hl)
        (let ((title (om--get-property-nocheck :raw-value hl)))
          (-if-let (parent (om--get-parent-headline hl))
              (cons title (get-path parent))
            (list title)))))
    (reverse (get-path headline))))

(defun om-headline-update-item-statistics (headline)
  "Return HEADLINE node with updated statistics cookie via items.

The percent/fraction will be computed as the number of checked items
over the number of items with checkboxes (non-checkbox items will
not be considered)."
  (let* ((items
          (->> (om-headline-get-section headline)
               (om-get-children)
               (--filter (om-is-type 'plain-list it))
               (-mapcat #'om-get-children)
               (--remove (om--property-is-nil :checkbox it))))
         (done (length (--filter (om--property-is-eq :checkbox 'on it)
                                 items)))
         (total (length items)))
    (om--headline-set-statistics-cookie-fraction done total headline)))

(defun om-headline-update-todo-statistics (headline)
  "Return HEADLINE node with updated statistics cookie via subheadlines.

The percent/fraction will be computed as the number of done
subheadlines over the number of todo subheadlines (eg non-todo
subheadlines will not be counted)."
  (let* ((subtodo (->> (om-headline-get-subheadlines headline)
                       (--filter (om--get-property-nocheck :todo-keyword it))))
         (done (length (-filter #'om-headline-is-done subtodo)))
         (total (length subtodo)))
    (om--headline-set-statistics-cookie-fraction done total headline)))

;;; plain-list

;; TODO there seems to be a bug in the org-interpeter that prevents
;; "+" bullets from being recognized (as of org-9.1.9 they are simply
;; read as "-")
(defun om-plain-list-set-type (type plain-list)
  "Return PLAIN-LIST node with type property set to TYPE.
TYPE is one of the symbols `unordered' or `ordered'."
  (cond
   ((eq type 'unordered)
    (om--map-children-nocheck*
      (--map (om-set-property :bullet '- it) it) plain-list))
   ((eq type 'ordered)
    ;; NOTE the org-interpreter seems to use the correct, ordered
    ;; numbers if any number is set here. This behavior may not be
    ;; reliable.
    (om--map-children-nocheck*
      (--map (om-set-property :bullet 1 it) it) plain-list))
   (t (om--arg-error "Invalid type: %s" type))))

;;; table

(defun om-table-get-cell (row-index column-index table)
  "Return table-cell node at ROW-INDEX and COLUMN-INDEX in TABLE node.
Rule-type rows do not count toward row indices."
  (-some->> (om--table-get-row row-index table)
            (om-get-children)
            (om--nth column-index)))

(defun om-table-delete-row (row-index table)
  "Return TABLE node with row at ROW-INDEX deleted."
  (om--map-children-nocheck* (om--remove-at row-index it) table))

(defun om-table-delete-column (column-index table)
  "Return TABLE node with column at COLUMN-INDEX deleted."
  (cl-flet*
      ((delete-cell
        (cells)
        (om--remove-at column-index cells))
       (map-row
        (row)
        (if (om--property-is-eq :type 'rule row) row
          (om--map-children-nocheck #'delete-cell row))))
    (om--map-children-nocheck* (-map #'map-row it) table)))

(defun om-table-insert-column! (column-index column-text table)
  "Return TABLE node with COLUMN-TEXT inserted at COLUMN-INDEX.

COLUMN-INDEX is the index of the column and COLUMN-TEXT is a list of
strings to be made into table-cells to be inserted following the same
syntax as `om-build-table-cell!'."
  (let ((column (-map #'om-build-table-cell! column-text)))
    (om--column-map-down-rows
     (lambda (new-cell cells) (om--insert-at column-index new-cell cells))
     column
     table)))

(defun om-table-insert-row! (row-index row-text table)
  "Return TABLE node with ROW-TEXT inserted at ROW-INDEX.

ROW-INDEX is the index of the column and ROW-TEXT is a list of strings
to be made into table-cells to be inserted following the same syntax
as `om-build-table-row!'."
  (if (not row-text) (om--table-clear-row row-index table)
    (let ((row (->> (om-build-table-row! row-text)
                    (om--table-row-pad-maybe table))))
      (om--map-children-nocheck* (om--insert-at row-index row it) table))))

(defun om-table-replace-cell! (row-index column-index cell-text table)
  "Return TABLE node with a table-cell node replaced by CELL-TEXT.

If CELL-TEXT is a string, it will replace the children of the
table-cell at ROW-INDEX and COLUMN-INDEX in TABLE. CELL-TEXT will be
processed the same as the argument given to `om-build-table-cell!'.

If CELL-TEXT is nil, it will set the cell to an empty string."
  (let* ((cell (if cell-text (om-build-table-cell! cell-text)
                 (om-build-table-cell "")))
         (row (->> (om--table-get-row row-index table)
                   (om--map-children-nocheck*
                     (om--replace-at column-index cell it)))))
    (om--table-replace-row row-index row table)))

(defun om-table-replace-column! (column-index column-text table)
  "Return TABLE node with the column at COLUMN-INDEX replaced by COLUMN-TEXT.

If COLUMN-TEXT is a list of strings, it will replace the table-cells
at COLUMN-INDEX. Each member of COLUMN-TEXT will be processed the
same as the argument given to `om-build-table-cell!'.

If COLUMN-TEXT is nil, it will clear all cells at COLUMN-INDEX."
  (if (not column-text) (om--table-clear-column column-index table)
    (let ((column-cells (-map #'om-build-table-cell! column-text)))
      (om--table-replace-column column-index column-cells table))))

(defun om-table-replace-row! (row-index row-text table)
  "Return TABLE node with the row at ROW-INDEX replaced by ROW-TEXT.

If ROW-TEXT is a list of strings, it will replace the cells at
ROW-INDEX. Each member of ROW-TEXT will be processed the same as
the argument given to `om-build-table-row!'.

If ROW-TEXT is nil, it will clear all cells at ROW-INDEX."
  (let ((row-cells (om-build-table-row! row-text)))
    (om--table-replace-row row-index row-cells table)))

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
;; In case 2, this is all that is needed since 1.0 is already a child
;; of 1. and will "autoindent" as 1. itself is moved.
;;
;; To make it "stay in place," as in case 1, remove 1.0 as a child of
;; 1., append it to the end of the list containing 2., and set this
;; list (with both 1. and 1.0.) as the child of 0.
;;
;; parameters for indenting:
;; - index of target to indent (1 in above example)

;; TODO throw error when index out of range???

(defun om--indent-members (fun index tree)
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
          (head* (om--map-last* (funcall fun target it) head)))
    (append head* (-drop 1 tail))))

;; headline

(defun om-headline-indent-subtree (index headline)
  "Return HEADLINE node with child headline at INDEX indented.
Unlike `om-headline-indent-subheadline' this will also indent the
indented headline node's children."
  (cl-flet
      ((append-indented
        (target-headline parent-headline)
        (let ((target-headline*
               (om--headline-subtree-shift-level 1 target-headline)))
          (om--map-children-nocheck
           (lambda (headline-children)
             (append headline-children (list target-headline*)))
           parent-headline))))
    (om-headline-map-subheadlines
     (lambda (subheadlines)
       (om--indent-members #'append-indented index subheadlines))
     headline)))

(defun om-headline-indent-subheadline (index headline)
  "Return HEADLINE node with child headline at INDEX indented.
Unlike `om-headline-indent-subtree' this will not indent the
indented headline node's children."
  (cl-flet
      ((append-indented
        (target-headline parent-headline)
        (let ((target-headline*
               (->> target-headline
                    (om-headline-map-subheadlines #'ignore)
                    (om--headline-shift-level 1)))
              (headlines-in-target
               (om-headline-get-subheadlines target-headline)))
          (om--map-children-nocheck
           (lambda (children)
             (append children (list target-headline*) headlines-in-target))
           parent-headline))))
    (om-headline-map-subheadlines
     (lambda (subheadlines)
       (om--indent-members #'append-indented index subheadlines))
     headline)))

;; plain-list

(defun om-plain-list-indent-item-tree (index plain-list)
  "Return PLAIN-LIST node with child item at INDEX indented.
Unlike `om-item-indent-item' this will also indent the indented item
node's children."
  (cl-flet
      ((append-indented
        (target-item parent-item)
        (let ((target-item* (om-build-plain-list target-item)))
          (om--map-children-nocheck
           (lambda (item-children) (append item-children (list target-item*)))
           parent-item))))
    (om--map-children-nocheck
     (lambda (items)
       (om--indent-members #'append-indented index items))
     plain-list)))

(defun om-plain-list-indent-item (index plain-list)
  "Return PLAIN-LIST node with child item at INDEX indented.
Unlike `om-item-indent-item-tree' this will not indent the indented
item node's children."
  (cl-flet
      ((append-indented
        (target-item parent-item)
        (let ((target-item*
               (->> target-item
                    (om--map-children-nocheck*
                     (--remove (om-is-type 'plain-list it) it))
                    (om-build-plain-list)))
              (items-in-target
               (->> (om-get-children target-item)
                    (--filter (om-is-type 'plain-list it)))))
          (om--map-children-nocheck
           (lambda (item-children)
             ;; TODO technically the target-item* should go in an
             ;; existing plain list but I don't this matters (for now)
             (append item-children (list target-item*) items-in-target))
           parent-item))))
    (om--map-children-nocheck
     (lambda (items)
       (om--indent-members #'append-indented index items))
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
;; We want to unindent everything under 1. So just take all children
;; of 1. and splice them into the top-level list between 1. and 2.
;; In this case 1.1.0 will remain a child of 1.1 but it will be
;; unindented as well because its parent is being unindented
;;
;; parameters for unindenting a tree:
;; - the index whose children are to be unindented

(defun om--unindent-members (index trim-fun extract-fun tree)
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

(defun om-headline-unindent-all-subheadlines (index headline)
  "Return HEADLINE node with all child headlines under INDEX unindented."
  (cl-flet
      ((trim
        (parent)
        (om-headline-map-subheadlines #'ignore parent))
       (extract
        (parent)
        (->> (om-get-children parent)
             (--map (om--headline-subtree-shift-level -1 it)))))
    (om-headline-map-subheadlines
     (lambda (subheadlines)
       (om--unindent-members index #'trim #'extract subheadlines))
     headline)))

;; plain-list

(defun om-plain-list-unindent-all-items (index plain-list)
  "Return PLAIN-LIST node with all child items under INDEX unindented."
  (cl-flet
      ((trim
        (parent)
        (om--map-children-nocheck
         (lambda (children)
           (--remove-first (om-is-type 'plain-list it) children))
         parent))
       (extract
        (parent)
        (->> (om-get-children parent)
             (--first (om-is-type 'plain-list it))
             (om-get-children))))
    (om--map-children-nocheck
     (lambda (items)
       (om--unindent-members index #'trim #'extract items))
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
;; We want to unindent 1.1. First, indent everything after 1.1 (in
;; this case only 1.2, which will be appended to the list starting
;; with 1.1.0). Then move 1.1 (with 1.1.0 and 1.2 as children) between
;; 1 and 2 in the toplevel list.
;;
;; parameters for unindenting:
;; - parent index (in this case 1 for 1.)
;; - child index (in this case 1 for 1.1)

;; TODO this is a bit sketchy...it depends on the indentation
;; function to make the children list one element shorter, which
;; is usually true but makes a really hard error to catch when it
;; fails
(defun om--indent-after (indent-fun index node)
  "Return NODE with INDENT-FUN applied to all child nodes after INDEX."
  (if (< index (1- (length (om-get-children node))))
      (->> (funcall indent-fun (1+ index) node)
           (om--indent-after indent-fun index))
    node))

;; headline

(defun om-headline-unindent-subheadline (index child-index headline)
  "Return HEADLINE node with a child headline under INDEX unindented.
The specific child headline to unindent is selected by CHILD-INDEX."
  (cl-flet
      ((trim
        (parent)
        (om-headline-map-subheadlines
         (lambda (subheadlines) (-take child-index subheadlines))
         parent))
       (extract
        (parent)
        (->> (om--indent-after #'om-headline-indent-subtree
                                    child-index parent)
             (om-get-children)
             (-drop child-index)
             (--map (om--headline-subtree-shift-level -1 it)))))
    (om-headline-map-subheadlines
     (lambda (subheadlines)
       (om--unindent-members index #'trim #'extract subheadlines))
     headline)))

;; plain-list

(defun om-plain-list-unindent-item (index child-index plain-list)
  "Return PLAIN-LIST node with a child item under INDEX unindented.
The specific child item to unindent is selected by CHILD-INDEX."
  (cl-flet
      ((trim
        (parent)
        (om--map-children-nocheck
         (lambda (children)
           (if (= 0 index)
               (--remove-first (om-is-type 'plain-list it) children)
             (--map-first (om-is-type 'plain-list it)
                          (om--map-children-nocheck
                           (lambda (items) (-take child-index items)) it)
                          children)))
         parent))
       (extract
        (parent)
        (->>
         (om-get-children parent)
         (--first (om-is-type 'plain-list it))
         (om--indent-after #'om-plain-list-indent-item-tree
                                child-index)
         (om-get-children)
         (-drop child-index))))
    (om--map-children-nocheck
     (lambda (items)
       (om--unindent-members index #'trim #'extract items))
     plain-list)))

;;; PRINTING FUNCTIONS

;; For the most part, printing a node only involves
;; `org-element-interpret-data', except that this function is buggy
;; and fails in several ways when branch nodes are childless:
;; - printing the string 'nil' where there should be a blank string
;; - printing the node when it should not be printed at all
;; - throwing an error when blank

;; To get around this, we need some workarounds... :/

;;; printing workaround functions

(defun om--set-blank-children (node)
  "Set the children of NODE to a blank string (\"\")."
  (om--set-children-nocheck '("") node))

(defconst om--rm-if-empty
  '(table plain-list bold italic radio-target strike-through
          superscript subscript table-cell underline)
  "Nodes that will be blank if printed and empty.
This is a workaround for a bug")

(defconst om--blank-if-empty
  '(center-block drawer dynamic-block property-drawer quote-block
                 special-block verse-block)
  "Branch element nodes that require \"\" to correctly print empty.
This is a workaround for a bug.")

(defun om--filter-non-zero-length (node)
  "Return NODE if it is not an empty node type from `om--rm-if-empty'.
The exception is rule-typed table-row nodes which are supposed to be
empty."
  (unless (and (om-is-childless node)
               (or (om-is-any-type om--rm-if-empty node)
                   (om--is-table-row node)))
    node))

(defun om--clean (node)
  "Return NODE with empty child nodes from `om--rm-if-empty' removed."
  (->> (om--map-children-nocheck* (-non-nil (-map #'om--clean it)) node)
       (om--filter-non-zero-length)))

(defun om--blank (node)
  "Return NODE with empty child nodes `om--blank-if-empty' set to contain \"\"."
  (if (om-is-childless node)
      (if (om-is-any-type om--blank-if-empty node)
          (om--set-blank-children node)
        node)
    (om--map-children-nocheck* (-map #'om--blank it) node)))

;;; print functions

(defun om-to-string (node)
  "Return NODE as an interpreted string without text properties."
  (->> node
       ;; Some objects and greater elements should be removed if
       ;; blank. Table and plain list will error, and the others make
       ;; no sense if they are empty. This is an org mode bug, they
       ;; should not be printed by the interpreter by default
       (om--clean)
       ;; Some greater elements will print "nil" in their children if
       ;; they are empty. This is likely an org bug, since it means
       ;; that the element <-> string conversion is not 100%
       ;; reproducible. The workaround for this is to set the children
       ;; to a single blank string if empty
       (om--blank)
       (org-element-interpret-data)
       (substring-no-properties)))

(defun om-to-trimmed-string (node)
  "Like `om-to-string' but strip whitespace when returning NODE."
  (-some->> (om-to-string node) (s-trim)))

;;; PATTERN MATCHING

;; This is a framework for applying "pattern matching" on node trees.
;; All these functions search through the node tree and return (and
;; sometimes operate on) a list of matches much like the UNIX find
;; function for searching filesystems.

;; Patterns are composed of the following parts:
;; conditions - match a node based on its type, properties, and index
;; wildcards - keywords that match one of more nodes regardless of
;;   type, properties, and index
;; slicers - keywords with arguments that limit the returned match
;;   list to a subset all matches (such as first match or 2nd - 5th
;;   matches)
;;
;; Of the above, only conditions are required in the pattern

;; When a pattern is fed into any of the match functions, it will
;; first be 'compiled' into a lambda function that will walk through
;; the node tree and accumulate/return the results the pattern
;; requests. All possible functions operate on the same data structure
;; which is a list of children in the tree at each level with the
;; indices cons'ed to them like ((L . R) . CHILD) where L is the left
;; index and R is the right index (starting from -1 and counting down
;; to the left given a list of children in a node). The right index is
;; necessary for negative index matching.

;; This data structure ensures that any child has the information
;; necessary for a condition form to determine if a match is successful
;; (if this data structure wasn't used, index matching would fail as
;; it would require knowledge of the entire list when the match is
;; made)

;; For slicers, two tricks are used to ensure that work is minimized.
;; The first is that searches are limited to the maximum number of
;; matches needed. If only the first match is needed, the search will
;; stop after one match. If the 2nd to 5th matches are needed, the
;; search will stop after 5 matches and return this with the first
;; match dropped. The second trick is that the search is reversed if
;; the slicer requests negatively indexed results. If the last match
;; is needed, reverse the tree and return the first result. These
;; tricks are possible/easier with the indexed-children data structure
;; described above as it ensures that indexing information is
;; preserved even when the children are reversed, and ensures that
;; matching can be made on one child node at a time, which guarantees
;; the limit will never be overshot.

(defun om--get-children-indexed (node)
  "Return list of children from NODE (it any) with index annotations."
  (let* ((children (om-get-children node))
         (len (- (length children))))
    (--map-indexed (cons `(,it-index . ,(+ len it-index)) it) children)))

(defmacro om--reduce-from-while (pred form initial-value list)
  "Like `--reduce-from' but only reduce LIST while PRED is t.
FORM and INITIAL-VALUE work the same way, and the exposed symbols `it'
and `acc' carry the same meaning."
  (declare (debug (form form form form)))
  `(let ((acc ,initial-value))
     (--each-while ,list ,pred (setq acc ,form))
     acc))

(defun om--match-make-condition-form (condition)
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
       (om--arg-error "Condition cannot be nil"))
      ;;
      ;; quote is invalid (may be accidentally in condition)
      (`(quote . ,_)
       (om--arg-error "'quote' not allowed in condition"))
      ;;
      ;; function is invalid (may be accidentally in condition)
      (`(function . ,_)
       (om--arg-error "'function' not allowed in condition"))
      ;;
      ;; literal node
      ((and (pred om--is-node) pattern-node)
       `(equal ,it-node ',pattern-node))
      ;;
      ;; type
      ((and (pred (lambda (y) (memq y om-nodes))) type)
       `(om-is-type ',type ,it-node))
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
       `(not ,(om--match-make-condition-form p)))
      ;; 
      ;; and
      (`(:and . ,(and (pred and) p))
       `(and ,@(-map #'om--match-make-condition-form p)))
      ;; 
      ;; or
      (`(:or . ,(and (pred and) p))
       `(or ,@(-map #'om--match-make-condition-form p)))
      ;;
      ;; property
      ;; NOTE: this must go last if we don't want :pred/:and/:or/:not
      ;; to be interpreted as a property
      (`(,(and (pred keywordp) prop) . (,val . nil))
       `(equal (om--get-property-nocheck ,prop ,it-node) ,val))
      ;;
      (p (om--arg-error "Invalid condition: %s" p)))))

(defun om--match-make-inner-pattern-form (end? limit pattern)
  "Return matching form for PATTERN.
END? is a boolean describing if the search should be made in reverse.
If t, reverse all children when obtaining from any given node. LIMIT
is an integer or nil describing the number of matches at which the
search should terminate. If nil, don't perform any checks and
terminate only when the entire tree is searched within PATTERN."
  (let* ((accum '(cons (cdr it) acc))
         (get-children
          (if (not end?) '(om--get-children-indexed (cdr it))
            '(reverse (om--get-children-indexed (cdr it)))))
         (reduce (if (not limit) '(--reduce-from)
                   `(om--reduce-from-while (< (length acc) ,limit)))))
    (pcase pattern
      ;; slicers should not be here
      (`(,(or :first :last :nth :slice) . ,_)
       (om--arg-error "Slicers can only appear at the front of pattern"))
      ;;
      ;; :many! - if node matches add to accumulator, if not descend
      ;;   into node's children and keep searching
      (`(:many! . (,condition . nil))
       (let ((pred (om--match-make-condition-form condition))
             (callback `(get-many acc ,get-children)))
         `(cl-labels
              ((get-many
                (acc children)
                (,@reduce (if ,pred ,accum ,callback) acc children)))
            ,callback)))
      ;;
      ;; :many - flatten all children into a giant list and walk
      ;;   through list, adding children to accumulator if they match
      (`(:many . (,condition . nil))
       (let ((pred (om--match-make-condition-form condition))
             (callback (if end? '(-snoc (flatten-children it) it)
                         '(cons it (flatten-children it)))))
         `(cl-labels
              ((flatten-children
                (indexed-node)
                (--mapcat ,callback ,get-children)))
            (,@reduce (if ,pred ,accum acc) acc (flatten-children it)))))
      ;;
      ;; :many and :many! should only have one condition after them
      (`(,(and (or :many :many!) wildcard) . ,_)
       (om--arg-error "Exactly one condition should follow %s" wildcard))
      ;;
      ;; :any (at end of pattern) - add all nodes to accumulator
      (`(:any . nil)
       `(,@reduce ,accum acc ,get-children))
      ;;
      ;; :any (with subpatterns after) - descend into the children
      ;;   of all nodes and continue searching
      (`(:any . ,ps)
       (let ((inner (om--match-make-inner-pattern-form end? limit ps)))
         `(,@reduce ,inner acc ,get-children)))
      ;;
      ;; condition (at end of pattern) - add node to accumulator if
      ;;   it matches
      (`(,condition . nil)
       (let ((pred (om--match-make-condition-form condition)))
         `(,@reduce (if ,pred ,accum acc) acc ,get-children)))
      ;;
      ;; condition (with subpatterns after) - descend into the
      ;;   children of matching nodes and continue searching
      (`(,condition . ,ps)
       (let ((pred (om--match-make-condition-form condition))
             (inner (om--match-make-inner-pattern-form end? limit ps)))
         `(,@reduce (if ,pred ,inner acc) acc ,get-children)))
      ;;
      (ps (om--arg-error "Invalid pattern: %s" ps)))))

(defun om--match-make-expanded-pattern-form (pattern node)
  "Return explicitly expanded PATTERN given a toplevel TYPE.
NODE is the target NODE to be matched"
  (let ((target-type (om-get-type node)))
    (cl-labels
        ((expand-node
          (acc node)
          (-if-let (cur-type (-some-> node (om-get-type)))
              (if (om-is-type target-type node) acc
                (expand-node (cons cur-type acc)
                             (om--get-parent node)))
            (error "Node in pattern is not in target node"))))
      (let ((first (car pattern))
            (rest (cdr pattern)))
        (cond
         ((and (om--is-node first) (-any? #'om--is-node rest))
          (error "Multiple nodes in pattern"))
         ((-any? #'om--is-node rest)
          (error "Nodes must be first non-slicer in pattern"))
         ((om--is-node first)
          (let ((path (expand-node nil (om--get-parent first))))
            `(,@path ,first ,@rest)))
         (t pattern))))))

(defun om--match-make-pattern-form (end? limit pattern node)
  "Return non-slicer matching form for PATTERN.
See `om--match-make-inner-pattern-form' for meaning of END? and LIMIT
which are passed directly through this function.
NODE is the target node to be matched"
  
  (let ((body (->> (om--match-make-expanded-pattern-form pattern node)
                   (om--match-make-inner-pattern-form end? limit))))
    ;; NOTE: the accumulator is assembled in reverse due to the nature
    ;; of linked lists. Consing to the front is a linear operation,
    ;; while appending to the back is a quadratic operation since the
    ;; list needs to be fully traversed with each append and the list
    ;; is growing. This means that the list here is reversed if `END?'
    ;; is nil (which means we want the list in forward-order) and left
    ;; reversed if `END?' is t (meaning backward order)
    (if end? body `(reverse ,body))))

(defun om--make-make-slicer-form (pattern node)
  "Return matching form with slicer operations for PATTERN.
NODE is the node to be matched."
  (pcase pattern
    ;; :first - search until one match found and return that
    (`(:first . ,ps)
     (om--match-make-pattern-form nil 1 ps node))
    ;;
    ;; :last - search backwards until one match found and return that
    (`(:last . ,ps)
     (om--match-make-pattern-form t 1 ps node))
    ;;
    ;; :nth - search until N matches found and return Nth; note that
    ;;   nil will be returned if N refers to anything outside the
    ;;   results list
    (`(:nth . (,n . ,ps))
     (unless (integerp n)
       (om--arg-error ":nth argument must be an integer"))
     (if (<= 0 n)
         `(-drop ,n ,(om--match-make-pattern-form nil (1+ n) ps node))
       `(-drop-last ,(1- (- n)) ,(om--match-make-pattern-form t (- n) ps node))))
    ;;
    ;; :sub - search until B matches found, drop A+1, and return;
    ;;   note that if B is longer than the results then all results
    ;;   will be dropped and nil will be ultimately returned
    (`(:sub . (,a . (,b . ,ps)))
     (cond
      ((not (and (integerp a) (integerp b)))
       (om--arg-error ":sub arguments must be an integers"))
      ((> a b)
       (om--arg-error ":sub left index must be less than right index"))
      ((and (<= 0 a) (<= 0 b))
       `(-drop ,a ,(om--match-make-pattern-form nil (1+ b) ps node)))
      ((and (< a 0) (< b 0))
       `(-drop-last ,(1- (- b)) ,(om--match-make-pattern-form t (- a) ps node)))
      (t
       (om--arg-error "Both indices must be on the same side of zero"))))
    ;;
    ;; no slicer - search without limit and return all
    (ps (om--match-make-pattern-form nil nil ps node))))

(defun om--match-make-lambda-form (pattern node)
  "Return callable lambda form for PATTERN.
NODE is the node to be matched."
  (let ((body (om--make-make-slicer-form pattern node)))
    `(lambda (it) (let ((it (cons nil it)) (acc)) ,body))))

;;; match

(defun om-match (pattern node)
  "Return a list of child nodes matching PATTERN in NODE.

PATTERN is a list like ([SLICER [ARG1] [ARG2]] [PNODE] SUB1 [SUB2 ...]).

SLICER is an optional prefix to the pattern describing how many
and which matches to return. If not given, all matches are returned.
Possible values are:

- `:first' - return the first match
- `:last' - return the last match
- `:nth' ARG1 - return the nth match where ARG1 is an integer denoting
  the index to return (starting at 0). ARG1 may be a negative number
  to start counting at the end of the match list, in which case -1 is
  the last index. Using 0 and -1 for ARG1 is equivalent to using
  `:first' and `:last' respectively
- `:sub' ARG1 ARG2 - return a sublist between indices ARG1 and ARG2.
  ARG1 may not be greater than ARG2, and both must either be
  non-negative integers or negative integers. In the case of negative
  integers, the indices refer to the same counterparts as described in
  `:nth'. If ARG1 and ARG2 are equal, this slicer has the same
  behavior as `:nth'.

PNODE is an optional literal node to be matched. If given, the
matching process will start within PNODE wherever it happens to be in
NODE. This is useful for 'reusing' previous matches without repeating
the same pattern.

SUBX denotes subpatterns that that match nodes in the parse tree.
Subpatterns may either be wildcards or conditions.

Conditions match exactly one level of the node tree being searched
based on the node's type (the symbol returned by `om-get-type'),
properties (the value returned by `om-get-property' for a valid
property keyword), and index (the position of the node in the list
returned by `om-get-children'). For index, both left indices (where
zero refers to the left end of the list) and right indices (where -1
refers to the right end of the list) are understood. Conditions may
either be atomic or compound, where compound conditions are themselves
composed of atomic or compound conditions.

The types of atomic conditions are:

- TYPE - match when the node's type is `eq' to TYPE (a symbol)
- INDEX - match when the node's index is `=' to INDEX (an integer)
- (OP INDEX) - match when (OP NODE-INDEX INDEX) returns t. OP is
  one of `<', `>', `<=', or `>=' and NODE-INDEX is the index of the
  node being evaluated
- (PROP VAL) - match nodes whose property PROP (a keyword) is `equal'
  to VAL; VAL is obtained by evaluating `om-get-property' with PROP
  and the current node; if PROP is invalid, an error will be thrown
- (:pred PRED) - match when PRED evaluates to t; PRED is a symbol for
  a unary function that takes the current node as its argument

Compound conditions start with an operator followed by their component
conditions. In the syntax below, CX refers to a condition. The types
of compound conditions are:

- (:and C1 C2 [C3 ...]) - match when all conditions are true
- (:or C1 C2 [C3 ...]) - match when at least one condition is true
- (:not C) - match when condition is not true

In addition to conditions, SUBX may be a wildcard keyword to match
nodes independent of their type, properties, and index. The types of
wildcards are:

- `:any' - always match exactly one node
- `:many' SUB - match SUB to nodes that are zero or more levels down
  in the tree; SUB is a subpattern of either a condition or the `:any'
  wildcard; only one subpattern is allowed after `:many'
- `:many!' SUB - like `:many' but do not match within other matches"
  (let ((match-fun (om--match-make-lambda-form pattern node)))
    (funcall match-fun node)))

;;; generalized tree modification

;; this macro provides the means of using a list of matches returned
;; from `om--match' for other operations that use the match list
;; as targets for modifying the original tree

(defmacro om--modify-children (node form)
  "Recursively modify the children of NODE using FORM.
FORM returns a list of element or object nodes as the new children,
and the variable `it' is bound to the original children."
  (declare (debug (form def-form)))
  (declare (indent 1))
  `(cl-labels
       ((rec
         (node)
         (if (not (om-is-branch-node node)) node
           (om-map-children*
             (->> (--map (rec it) it)
                  (funcall (lambda (it) ,form)))
             node))))
     (rec ,node)))

;;; delete

(defun om--delete-targets (node targets)
  "Return NODE without children in TARGETS (a list of nodes)."
  (om--modify-children node
    (--remove (member it targets) it)))

(defun om-match-delete (pattern node)
  "Return NODE without children matching PATTERN.

PATTERN follows the same rules as `om-match'."
  (-if-let (targets (om-match pattern node))
      (om--delete-targets node targets)
    node))

;;; extract

(defun om-match-extract (pattern node)
  "Remove nodes matching PATTERN from NODE.
Return cons cell where the car is a list of all removed nodes and
the cdr is the modified NODE.

PATTERN follows the same rules as `om-match'."
  (-if-let (targets (om-match pattern node))
      (cons targets (om--delete-targets node targets))
    node))

;;; map

(om--defun* om-match-map (pattern fun node)
  "Return NODE with FUN applied to children matching PATTERN.
FUN is a unary function that takes a node and returns a new node
which will replace the original.

PATTERN follows the same rules as `om-match'."
  (-if-let (targets (om-match pattern node))
      (om--modify-children node
        (--map-when (member it targets) (funcall fun it) it))
    node))

;;; mapcat

(om--defun* om-match-mapcat (pattern fun node)
  "Return NODE with FUN applied to children matching PATTERN.
FUN is a unary function that takes a node and returns a list of new
nodes which will be spliced in place of the original node.

PATTERN follows the same rules as `om-match'."
  (-if-let (targets (om-match pattern node))
      (om--modify-children node
        (--mapcat (if (member it targets)
                      (funcall fun it) (list it))
                  it))
    node))

;;; replace

(defun om-match-replace (pattern node* node)
  "Return NODE with NODE* in place of children matching PATTERN.

PATTERN follows the same rules as `om-match'."
  (declare (indent 1))
  (-if-let (targets (om-match pattern node))
      (om--modify-children node
        (--map-when (member it targets) node* it))
    node))

;;; insert-before

(defun om-match-insert-before (pattern node* node)
  "Return NODE with NODE* inserted before children matching PATTERN.

PATTERN follows the same rules as `om-match'."
  (declare (indent 1))
  (-if-let (targets (om-match pattern node))
      (om--modify-children node
        (--mapcat (if (member it targets) (list node* it) (list it)) it))
    node))

;;; insert-after

(defun om-match-insert-after (pattern node* node)
  "Return NODE with NODE* inserted after children matching PATTERN.

PATTERN follows the same rules as `om-match'."
  (declare (indent 1))
  (-if-let (targets (om-match pattern node))
      (om--modify-children node
        (--mapcat (if (member it targets) (list it node*) (list it)) it))
    node))

;;; insert-within

(defun om-match-insert-within (pattern index node* node)
  "Return NODE with NODE* inserted at INDEX in children matching PATTERN.

PATTERN follows the same rules as `om-match' with the exception
that PATTERN may be nil. In this case NODE* will be inserted at INDEX
in the immediate, top level children of NODE."
  (declare (indent 2))
  (if pattern
      (-if-let (targets (om-match pattern node))
          (om--modify-children node
            (--map-when
             (member it targets)
             (om-map-children*
               (om--insert-at index node* it t)
               it)
             it))
        node)
    (om-map-children* (om--insert-at index node* it t) node)))

;;; splice

(defun om-match-splice (pattern nodes* node)
  "Return NODE with NODES* spliced in place of children matching PATTERN.
NODES* is a list of nodes.

PATTERN follows the same rules as `om-match'."
  (declare (indent 1))
  (-if-let (targets (om-match pattern node))
      (om--modify-children node
        (--mapcat (if (member it targets) nodes* (list it)) it))
    node))

;;; splice-before

(defun om-match-splice-before (pattern nodes* node)
  "Return NODE with NODES* spliced before children matching PATTERN.
NODES* is a list of nodes.

PATTERN follows the same rules as `om-match'."
  (declare (indent 1))
  (-if-let (targets (om-match pattern node))
      (om--modify-children node
        (--mapcat (if (member it targets)
                      (append nodes* (list it))
                    (list it))
                  it))
    node))

;;; splice-after

(defun om-match-splice-after (pattern nodes* node)
  "Return NODE with NODES* spliced after children matching PATTERN.
NODES* is a list of nodes.

PATTERN follows the same rules as `om-match'."
  (declare (indent 1))
  (-if-let (targets (om-match pattern node))
      (om--modify-children node
        (--mapcat (if (member it targets) (cons it nodes*) (list it)) it))
    node))

;;; splice-within

(defun om-match-splice-within (pattern index nodes* node)
  "Return NODE with NODES* spliced at INDEX in children matching PATTERN.
NODES* is a list of nodes.

PATTERN follows the same rules as `om-match' with the exception
that PATTERN may be nil. In this case NODES* will be inserted at INDEX
in the immediate, top level children of NODE."
  (declare (indent 2))
  (if pattern
      (-if-let (targets (om-match pattern node))
          (om--modify-children node
            (--map-when
             (member it targets)
             (om-map-children*
               (om--splice-at index nodes* it t)
               it)
             it))
        node)
    (om-map-children* (om--splice-at index nodes* it t) node)))

;;; side-effects

(om--defun* om-match-do (pattern fun node)
  "Like `om-match-map' but for side effects only.
FUN is a unary function that has side effects and is applied to the
matches from NODE using PATTERN. This function itself returns nil.

PATTERN follows the same rules as `om-match'."
  (-when-let (targets (om-match pattern node))
      (--each targets (funcall fun it))))

;;; BUFFER PARSING

;;; parse at specific point

;; TODO add test for plain-text parsing
(defun om-parse-object-at (point)
  "Return object node under POINT or nil if not on an object."
  (save-excursion
    (goto-char point)
    (-let* ((context (org-element-context))
            ((offset nesting) (cl-case (om-get-type context)
                                ((superscript subscript) '(-1 (0 1)))
                                (table-cell '(-1 (0 0 0)))
                                (t '(0 (0 0)))))
            ((&plist :begin :end) (om--get-all-properties context))
            (tree (org-element--parse-elements
                   (+ begin offset) end 'first-section nil nil nil nil)))
      (->> (car tree)
           (om--get-descendent nesting)
           (om--filter-types om-objects)))))

(defun om--parse-element-at (point &optional type)
  "Return element node immediately under POINT.
For a list of all possible return types refer to `om-elements'; this
will return everything in this list except 'section' which is
ambiguous when referring to a single point.
\(see `om-parse-section-at').

If TYPE is supplied, only return nil if the object under point is not
of that type. TYPE is a symbol from `om-elements'. Furthermore,
setting TYPE to 'table-row' will prefer table-row elements over table
elements and likewise when setting TYPE to 'item' for plain-list
elements vs item elements."
  (save-excursion
    (goto-char point)
    (let*
        ((node (org-element-at-point))
         (node-type (om-get-type node)))
      ;; NOTE this will not filter by type if it is a leaf node
      (if (not (memq node-type om-branch-nodes)) node
        ;; need to parse again if branch-node since
        ;; `org-element-at-point' does not parse children
        (-let* (((&plist :begin :end) (om--get-all-properties node))
                (tree (car (org-element--parse-elements
                            begin end 'first-section nil nil nil nil)))
                (nesting (cl-case node-type
                           (headline nil)
                           (table (if (eq type 'table-row) '(0 0) '(0)))
                           (plain-list (if (eq type 'item) '(0 0) '(0)))
                           (t '(0)))))
          (--> (om--get-descendent nesting tree)
               (if type (om--filter-type type it) it)))))))

(defun om-parse-element-at (point)
  "Return element node under POINT or nil if not on an element.

This function will return every element available in `om-elements'
with the exception of `section', `item', and `table-row'. To
specifically parse these, use the functions `om-parse-section-at',
`om-parse-item-at', and `om-parse-table-row-at'."
  (om--parse-element-at point))

(defun om-parse-table-row-at (point)
  "Return table-row node under POINT or nil if not on a table-row."
  (save-excursion
    (goto-char point)
    (beginning-of-line)
    (om--parse-element-at (point) 'table-row)))

(defun om-parse-item-at (point)
  "Return item node under POINT or nil if not on an item.
This will return the item node even if POINT is not at the beginning
of the line."
  (save-excursion
    (goto-char point)
    (beginning-of-line)
    (om--parse-element-at (point) 'item)))

(defun om--parse-headline-subtree-at (point subtree)
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
                     ;; this function won't move if we are on the
                     ;; last headline. Check if point has moved, and
                     ;; if not, return the max point
                     (org-forward-heading-same-level 1 t)
                     (if (= (point) orig-point)
                         (point-max) (point)))
                 (or (outline-next-heading) (point-max)))))
        (car (org-element--parse-elements b e 'first-section
                                          nil nil nil nil))))))

(defun om-parse-headline-at (point)
  "Return headline node under POINT or nil if not on a headline.
POINT does not need to be on the headline itself. Only the headline
and its section will be returned. To include subheadlines, use
`om-parse-subtree-at'."
  (om--parse-headline-subtree-at point nil))

(defun om-parse-subtree-at (point)
  "Return headline node under POINT or nil if not on a headline.
POINT does not need to be on the headline itself. Unlike
`om-parse-headline-at', the returned node will include
child headlines."
  (om--parse-headline-subtree-at point t))

(defun om-parse-section-at (point)
  "Return section node under POINT or nil if not on a section.
If POINT is on or within a headline, return the section under that
headline. If POINT is before the first headline (if any), return
the section at the top of the org buffer."
  (save-excursion
    (goto-char point)
    (om--get-descendent
     '(0)
     (condition-case nil
         (progn
           (org-back-to-heading)
           (om--parse-headline-subtree-at point nil))
       (error
        (org-element--parse-elements
         (point-min) (or (outline-next-heading) (point-max))
         'first-section nil nil nil nil))))))

;;; parse at current point

(eval-and-compile
  (-> '(object element table-row item headline subtree section)
      (--each
          (let* ((name (intern (format "om-parse-this-%s" it)))
                 (call (intern (format "om-parse-%s-at" it)))
                 (doc (format "Call `%s' with the current point." call))
                 (body `(,call (point))))
            (eval `(defun ,name () ,doc ,body))))))

(defun om-parse-this-toplevel-section ()
  "Return section node corresponding to the top of the current buffer.
If there is no such section, return nil."
  (save-excursion
    (goto-char (point-min))
    (unless (= ?* (char-after))
      (om-parse-this-section))))

(defun om-parse-this-buffer ()
  "Return org-data document tree for the current buffer.
Contrary to the org-element specification, the org-data element
returned from this function will have :begin and :end properties."
  (let* ((c (om-get-children (org-element-parse-buffer)))
         (b (if c (om--get-property-nocheck :begin (-first-item c)) 1))
         (e (if c (om--get-property-nocheck :end (-last-item c)) 1)))
    (om--construct 'org-data `(:begin ,b :end ,e) c)))

(defun om-this-buffer-has-headlines ()
  "Return t if the current buffer has headlines, else return nil."
  (save-excursion
    (goto-char (point-min))
    (and (re-search-forward "^\\*" nil t) t)))

;;; BUFFER SIDE EFFECTS

;;; insert

(defun om--nodes-to-string-maybe (nodes)
  "Return NODES as a string.
NODES may either be a single node or a list of nodes."
  (if (and (listp nodes) (-all? #'om--is-node nodes))
      (s-join "" (-map #'om-to-string nodes))
    (om-to-string nodes)))

(defun om-insert (point node)
  "Convert NODE to a string and insert at POINT in the current buffer.
NODE may be a node or a list of nodes. Return NODE."
  (save-excursion
    (goto-char point)
    (insert (om--nodes-to-string-maybe node)))
  node)

(defun om-insert-tail (point node)
  "Like `om-insert' but insert NODE at POINT and move to end of insertion."
  (let ((s (om--nodes-to-string-maybe node)))
    (save-excursion
      (goto-char point)
      (insert s))
    (goto-char (+ point (length s))))
  node)

;;; update

(defun om--apply-overlays (os)
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

(om--defun* om-update (fun node)
  "Replace NODE in the current buffer with a new one.
FUN is a unary function that takes NODE and returns a modified node
or list of nodes. This modified node is then written in place of the
old node in the current buffer."
  ;; if node is of type 'org-data' it will have no props
  (let* ((begin (om--get-property-nocheck :begin node))
         (end (om--get-property-nocheck :end node))
         (ov-cmd (->>
                  (overlays-in begin end)
                  (--filter (eq 'outline (overlay-get it 'invisible)))
                  (--map (list :start (overlay-start it)
                               :end (overlay-end it)
                               :props (overlay-properties it)))
                  (list 'apply 'om--apply-overlays)))
         ;; do all computation before modifying buffer
         (node0 (om-clone-node node))
         (node* (funcall fun node)))
    (unless (equal node0 node*)
      ;; hacky way to add overlays to undo tree
      (setq-local buffer-undo-list (cons ov-cmd buffer-undo-list))
      (delete-region begin end)
      (om-insert begin node*)
      nil)))

;; generate all update functions for corresponding parse functions
;; since all take function args, also generate anaphoric forms
(eval-and-compile
  (--each '(object element table-row item headline subtree section)
    (let* ((update-at
            (intern (format "om-update-%s-at" it)))
           (update-this
            (intern (format "om-update-this-%s" it)))
           (update-at-doc
            (-as-> (list "Update %1$s under POINT using FUN."
                         "FUN takes an %1$s and returns a modified %1$s")
                   fmt
                   (s-join "\n" fmt)
                   (format fmt it)))
           (update-this-doc
            (-as-> (list "Update %1$s under current point using FUN."
                         "FUN takes an %1$s and returns a modified %1$s")
                   fmt
                   (s-join "\n" fmt)
                   (format fmt it)))
           (call (intern (format "om-parse-%s-at" it)))
           (update-at-body `(om-update fun (,call point)))
           (update-this-body `(,update-at (point) fun)))
      (eval `(om--defun* ,update-at (point fun)
               ,update-at-doc
               ,update-at-body))
      (eval `(om--defun* ,update-this (fun)
               ,update-this-doc
               ,update-this-body)))))

(om--defun* om-update-this-buffer (fun)
  "Apply FUN to the contents of the current buffer.
FUN is a unary function that takes a node of type 'org-data' and
returns a modified node."
  (om-update fun (om-parse-this-buffer)))

;;; fold

;; TODO this will fold items improperly
(defun om--flag-elem-contents (flag node)
  "Set folding of buffer contents in NODE to FLAG."
  (let ((b (om--get-property-nocheck :contents-begin node))
        (e (cl-case (om-get-type node)
             ((property-drawer drawer) (om--get-property-nocheck :end node))
             (t (om--get-property-nocheck :contents-end node)))))
    (outline-flag-region (1- b) (1- e) flag)))

(defun om-fold (node)
  "Fold the children of NODE if they exist."
  (om--flag-elem-contents t node))

(defun om-unfold (node)
  "Unfold the children of NODE if they exist."
  (om--flag-elem-contents nil node))

;;; headline iteration

(defun om--do-headlines-where (where fun-forward fun-backward
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
        ((< n m) (om--arg-error "M must be less than or equal to N"))
        (t (om--arg-error "M and N must be the same sign"))))
      ;; parse region between A and B
      (`[,(and (pred int-or-nil-p) a) ,(and (pred int-or-nil-p) b)]
       (let ((a (or a (point-min)))
             (b (or b (point-max))))
         (funcall fun-region a b)))
      (e (om--arg-error "Invalid 'where' specification: Got %S" e)))))

(defmacro om--apply-n (m n re backward? parse-form)
  (declare (indent 4))
  (let ((start (if backward? '(point-max) '(point-min)))
        (iterate-form
         (if backward?
             `(save-match-data
                (re-search-backward ,re nil t))
           `(save-match-data
              ;; avoid matching the current headline if already there
              (when (and (bolp) (not (eobp))) (forward-char 1))
              (when (re-search-forward ,re nil t)
                (goto-char (match-beginning 0)))))))
    `(save-excursion
       (goto-char ,start)
       ;; parse headline(s) if we start on a headline or can move to
       ;; a headline
       (when (or (looking-at ,re) ,iterate-form)
         (let ((i 0))
           ;; parse the first if we want it
           (when (= 0 ,m) ,parse-form)
           (setq i (1+ i))
           ;; loop through the rest and parse when appropriate
           (while (and ,iterate-form (<= i ,n))
             (when (<= ,m i) ,parse-form)
             (setq i (1+ i))))))))

(defmacro om--apply-region (begin end re parse-form)
  (declare (indent 3))
  (let ((iterate-form `(re-search-backward ,re nil t)))
    `(save-excursion
       (goto-char ,end)
       (when ,iterate-form
         ,parse-form
         ;; loop through the rest
         (while (and ,iterate-form (<= ,begin (point)))
           ,parse-form)))))

(defun om-get-some-headlines (where)
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

Each headline is obtained with `om-parse-headline-at'."
  (cl-flet
      ((apply-n-forward
        (m n)
        (let ((acc))
          (om--apply-n m n "^\\*" nil
            (setq acc (cons (om-parse-this-headline) acc)))
          (nreverse acc)))
       (apply-n-backward
        (m n)
        (let ((acc))
          (om--apply-n m n "^\\*" t
            (setq acc (cons (om-parse-this-headline) acc)))
          acc))
       (apply-region
        (begin end)
        (let ((acc))
          (om--apply-region begin end "^\\*"
            (setq acc (cons (om-parse-this-headline) acc)))
          acc)))
    (om--do-headlines-where where
      #'apply-n-forward
      #'apply-n-backward
      #'apply-region)))

(defun om-get-headlines ()
  "Return list of all headline nodes from current buffer.
Each headline is obtained with `om-parse-headline-at'."
  (om-get-some-headlines [nil nil]))

(defun om-get-some-subtrees (where)
  "Return list of subtree nodes from current buffer.

See `om-get-some-headlines' for the meaning of WHERE.

Each subtree is obtained with `om-parse-subtree-at'."
  (cl-flet
      ((apply-n-forward
        (m n)
        (let ((acc))
          (om--apply-n m n "^\\* " nil
            (setq acc (cons (om-parse-this-subtree) acc)))
          (nreverse acc)))
       (apply-n-backward
        (m n)
        (let ((acc))
          (om--apply-n m n "^\\* " t
            (setq acc (cons (om-parse-this-subtree) acc)))
          acc))
       (apply-region
        (begin end)
        (let ((acc))
          (om--apply-region begin end "^\\* "
            (setq acc (cons (om-parse-this-subtree) acc)))
          acc)))
    (om--do-headlines-where where
      #'apply-n-forward
      #'apply-n-backward
      #'apply-region)))

(defun om-get-subtrees ()
  "Return list of all subtree nodes from current buffer.

Each subtree is obtained with `om-parse-subtree-at'."
  (om-get-some-subtrees [nil nil]))

(om--defun* om-do-some-headlines (where fun)
  "Update some headlines in the current using FUN.

See `om-get-some-headlines' for the meaning of WHERE.

Headlines are updated using `om-update-this-headline' (see this for
use and meaning of FUN)."
  (cl-flet
      ((apply-n-forward
        (m n)
        (om--apply-n m n "^\\*" nil
          (om-update-this-headline fun)))
       (apply-n-backward
        (m n)
        (om--apply-n m n "^\\*" t
          (om-update-this-headline fun)))
       (apply-region
        (begin end)
        (om--apply-region begin end "^\\*"
          (om-update-this-headline fun))))
    (om--do-headlines-where where
      #'apply-n-forward
      #'apply-n-backward
      #'apply-region)))

(om--defun* om-do-headlines (fun)
  "Update all headlines in the current buffer using FUN.

Headlines are updated using `om-update-this-headline' (see this for
use and meaning of FUN)."
  (om-do-some-headlines [nil nil] fun))

(om--defun* om-do-some-subtrees (where fun)
  "Update some toplevel subtrees in the current buffer using FUN.

See `om-get-some-headlines' for the meaning of WHERE.

Subtrees are updated using `om-update-this-subtree' (see this for use
and meaning of FUN)."
  (cl-flet
      ((apply-n-forward
        (m n)
        (om--apply-n m n "^\\* " nil
          ;; (re-search-forward "^\\* " nil t)
          (om-update-this-subtree fun)))
       (apply-n-backward
        (m n)
        (om--apply-n m n "^\\* " t
          ;; (re-search-backward "^\\* " nil t)
          (om-update-this-subtree fun)))
       (apply-region
        (begin end)
        (om--apply-region begin end "^\\* "
          ;; (re-search-backward "^\\* " nil t)
          (om-update-this-subtree fun))))
    (om--do-headlines-where where
      #'apply-n-forward
      #'apply-n-backward
      #'apply-region)))

(om--defun* om-do-subtrees (fun)
  "Update all toplevel subtrees in the current buffer using FUN.

Subtrees are updated using `om-update-this-subtree' (see this for use
and meaning of FUN)."
  (om-do-some-subtrees [nil nil] fun))

(provide 'om)
;;; om.el ends here
