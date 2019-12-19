;;; om.el --- Functional Org Mode API -*- lexical-binding: t; -*-

;; Copyright (C) 2019  Nathan Dwarshuis

;; Author: Nathan Dwarshuis <natedwarshuis@gmail.com>
;; Keywords: org-mode, outlines
;; Homepage: https://github.com/ndwarshuis/om.el
;; Package-Requires: ((emacs "26.1") (dash "2.15") (s "1.12"))
;; Version: 0.0.1

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

;; You are a...puppet
;; You are a...puppet
;; You are a...puppet
;; You are a...puppet

;; I am a...puppet
;; I am a...puppet
;; I am a...puppet
;; I am a...puppet

;; WE ARE ALL...PUPPETS

;;; Code:

(require 'org)
(require 'dash)
(require 's)

;;; NODE SET AND RELATIONAL CONSTANTS

(defconst om-object-restrictions
  (->> org-element-object-restrictions
       ;; remove non-objects
       (--remove (memq (car it) '(inlinetask item headline keyword)))
       ;; add plain-text type
       (--map-when (not (eq (car it) 'table-row)) (-snoc it 'plain-text)))
  "Alist of object restrictions for object containers.
Unlike `org-element-object-restrictions', this only includes objects
and object containers and includes the 'plain-text' type.")

(defconst om-element-restrictions
  ;; TODO add inlinetask
  ;; this includes all elements except those that are restricted
  ;; (see comments below)
  (let ((standard '(babel-call center-block clock comment
                          comment-block diary-sexp drawer
                          dynamic-block example-block
                          export-block fixed-width footnote-definition
                          horizontal-rule
                          keyword latex-environment
                          paragraph
                          plain-list planning property-drawer
                          quote-block special-block
                          src-block table verse-block)))
    ;; TODO center blocks can't be in themselves
    `((center-block ,@standard)
      ;; TODO drawers can't be in themselves
      (drawer ,@standard)
      ;; TODO dynamic blocks can't be in themselves
      (dynamic-block ,@standard)
      ;; TODO cannot contain itself
      (footnote-definition ,@standard)
      ;; headlines and sections can only be in headlines
      (headline headline section)
      (item ,@standard)
      ;; items can only be in plain-lists
      (plain-list item)
      ;; node-properties can only be in property-drawers
      (property-drawer node-property)
      ;; TODO cannot contain itself
      (quote-block ,@standard)
      (section ,@standard)
      (special-block ,@standard)
      ;; table-rows can only be in tables
      (table table-row)))
  "Alist of element restrictions for greater elements.")

(defconst om-node-restrictions
  (append om-element-restrictions om-object-restrictions)
  "Alist of all restrictions for containers.")

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

(defconst om-leaf-elements
  (-difference om-elements om-branch-elements)
  "List of element types that are leaves.")

(defconst om-leaf-objects
  (-difference om-objects om-branch-objects)
  "List of object types that are leaves.")

(defconst om-node-leaves
  (append om-leaf-objects om-leaf-elements)
  "List of node types that are leaves.")

(defconst om--item-tag-restrictions
  (->> org-element-object-restrictions
       (alist-get 'item)
       (cons 'plain-text)))

(defconst om--headline-title-restrictions
  (->> org-element-object-restrictions
       (alist-get 'headline)
       (cons 'plain-text)))

;;; INTERNAL TYPE FUNCTIONS

(defalias 'om--get-type 'org-element-type)
;; (defalias 'om--get-class 'org-element-class)

(defun om--is-type-p (type node)
  "Return t if NODE's type is `eq' to TYPE (a symbol)."
  ;; TODO ensure type is valid?
  (eq (om--get-type node) type))

(defun om--is-any-type-p (types node)
  "Return t if NODE's type is any in TYPES (a list of symbols)."
  (if (memq (om--get-type node) types) t))

(defun om--is-node-p (list)
  "Return t is LIST is a node."
  (om--is-any-type-p om-nodes list))

(defun om--is-branch-node-p (list)
  "Return t is LIST is a branch node."
  (om--is-any-type-p om-branch-nodes list))

;;; INTERNAL PREDICATES

(defun om--is-oneline-string-p (x)
  (and (stringp x) (not (s-contains? "\n" x))))

(defun om--is-oneline-string-or-nil-p (x)
  (or (null x) (om--is-oneline-string-p x)))

(defun om--is-non-neg-integer-p (x)
  (and (integerp x) (<= 0 x)))

(defun om--is-non-neg-integer-or-nil-p (x)
  (or (null x) (om--is-non-neg-integer-p x)))

(defun om--is-pos-integer-p (x)
  (and (integerp x) (< 0 x)))

(defun om--is-pos-integer-or-nil-p (x)
  (or (null x) (om--is-pos-integer-p x)))

(defun om--is-string-list-p (x)
  (or (null x) (and (listp x) (-all? #'om--is-oneline-string-p x))))

;;; BOILERPLATE MACROS

;; better cl-defun
;; some functions here require a clean way to use &rest and &key
;; at the same time, which `cl-defun' does not do...

(eval-when-compile
  (defun om--symbol-to-keyword (symbol)
    "Convert SYMBOL to keyword if not already."
    (if (keywordp symbol) symbol
      (->> (symbol-name symbol)
           (s-prepend ":")
           (intern))))

  (defun om--verify-pos-args (pos-args)
    (if (-all? #'symbolp pos-args) pos-args
      (error "All positional arguments must be symbols")))

  (defun om--verify-rest-arg (resarg)
    (if (and (>= 1 (length resarg)) (symbolp (car resarg)))
        (car resarg)
      (error "Rest argument must only have one member")))

  (defun om--make-kwarg-let (kwarg)
    "For each in KWARGS, return a plist."
    (cl-flet
        ((make-plist
          (arg kw init)
          (when (and kw (not (keywordp kw)))
            (error "Must use keyword for kw-arg, not %s" kw))
          (let* ((kw (or kw (om--symbol-to-keyword arg)))
                 (kw-get `(cadr (plist-member --kw-args ',kw)))
                 (val (if init `(or ,kw-get ,init) kw-get)))
            (cons kw `(,arg ,val)))))
      (pcase kwarg
        (`((,(and (pred keywordp) kw) ,arg) ,init)
         (make-plist arg kw init))
        (`((,(and (pred keywordp) kw) ,arg))
         (make-plist arg kw nil))
        (`(,arg ,init) (make-plist arg nil init))
        ((and (pred symbolp) arg)
         (make-plist arg nil nil))
        (_ (error "Invalid keyword argument: %s" kwarg)))))

  (defun om--partition-rest-args (args kws use-rest?)
    (if (not kws) (list nil args)
      (-let* (((kwargs restargs)
               (->> (-partition-all 2 args)
                    (--split-with (keywordp (car it)))))
              (restargs (apply #'append restargs)))
        ;; ensure only valid keywords are used
        (-some->> (-difference (--map (car it) kwargs) kws)
                  (--map (format "%S" it))
                  (s-join ", ")
                  (error "Invalid keyword(s) found: %s"))
        ;; ensure keywords are only used once per call
        (-some->> (-group-by #'car kwargs)
                  (--filter (< 1 (length (cdr it))))
                  (--map (format "%S" (car it)))
                  (s-join ", ")
                  (error "Keyword(s) used multiple times: %s"))
        ;; ensure that keyword pairs are only used immediately after
        ;; positional arguments
        (-some->>
         (-filter #'keywordp restargs)
         (--map (format "%S" (car it)))
         (s-join ", ")
         (error (s-join " "
                        '("Keyword-value pairs must be immediately"
                          "after positional arguments. These keywords"
                          "were interpreted as rest arguments: %s"))))
        (when (and restargs (not use-rest?))
          (error "Too many arguments supplied"))
        (list (apply #'append kwargs) restargs))))

  (defun om--make-header (body args)
    (let ((header (caar (macroexp-parse-body body))))
      ;; Macro expansion can take place in the middle of
      ;; apparently harmless computation, so it should not
      ;; touch the match-data.
      (save-match-data
        (let ((print-gensym nil)
              (print-quoted t)
              (print-escape-newlines t))
          (->> (cl--make-usage-args args)
               (cons 'fn)
               (format "%S")
               (help--docstring-quote)
               (help-add-fundoc-usage header))))))

  (defun om--transform-lambda (args body name)
    "Transform ARGS and BODY to a block bound to NAME."
    (let* ((partargs (-partition-before-pred
                      (lambda (it) (memq it '(&pos &rest &key)))
                      (cons '&pos args)))
           (kw-lets (->> (alist-get '&key partargs)
                         (-map #'om--make-kwarg-let)))
           (rest-arg (->> (alist-get '&rest partargs)
                          (om--verify-rest-arg)))
           (header (om--make-header body args))
           ;; (car (macroexp-parse-body body)))
           (body (->> (macroexp-parse-body body)
                      (cdr)
                      (append `(cl-block ,name))))
           (pos-args (->> (alist-get '&pos partargs)
                          (om--verify-pos-args)))
           (arg-form (if (not (or kw-lets rest-arg))
                         `(,@pos-args)
                       `(,@pos-args &rest --rest-args)))
           (let-forms
            (when (or rest-arg kw-lets)
              (let ((keys (-map #'car kw-lets))
                    (rest-let (when rest-arg `((,rest-arg (nth 1 s)))))
                    (lets (append (-map #'cdr kw-lets)))
                    (kw-setter (and kw-lets '((--kw-args (nth 0 s))))))
                ;; TODO there is probably a more efficient way to do this...
                `((s (om--partition-rest-args
                      --rest-args (quote ,keys)
                      ,(and rest-arg t)))
                  ,@kw-setter
                  ,@rest-let
                  ,@lets)))))
      ;; mercilessly stolen from cl--transform-whatever
      `(,arg-form
        ,header
        ,(macroexp-let*
          let-forms
          (macroexp-progn `(,body))))))

  ;; TODO catch duplicate keys
  (defmacro om--defun-kw (name args &rest body)
    "Define NAME as a function.

   ((VAR [PRED])...
    [&rest (VAR [PRED]]
    [&key (([KEYWORD] VAR) [[PRED] [INITFORM]])...])"
    ;; TODO wtf does this stuff do...???
    (declare (debug
              ;; Same as defun but use cl-lambda-list.
              (&define [&or name ("setf" :name setf name)]
                       cl-lambda-list
                       cl-declarations-or-string
                       [&optional ("interactive" interactive)]
                       def-body))
             (doc-string 3)
             (indent 2))
    (let ((res (om--transform-lambda args body name)))
      `(defun ,name ,@res))))

;; defun which also makes anaphoric form

(defmacro om--verify (&rest args)
  (let ((tests
         (->>
          (-partition 2 args)
          (--map
           (let ((arg (car it))
                 (pred (cadr it)))
             `(unless (funcall #',pred ,arg)
                (error "Arg %s with value %s failed predicate %s"
                       ',arg ,arg ',pred)))))))
    `(progn ,@tests)))

(defmacro om--defun-with-docstring-body (args &rest rest)
  ;; the third arg is a docstring if it is a string and there
  ;; is at least one thing after it to be the body
  (declare (indent 1))
  `(-let (((docstring body)
           (if (and (stringp (car ,args)) (< 1 (length ,args)))
               (list (car ,args) (-drop 1 ,args))
             (list nil ,args))))
     ,@rest))

(defmacro om--defun* (name arglist &rest args)
  (declare (doc-string 3) (indent 2))
  (om--defun-with-docstring-body args
    (-let* ((name* (intern (format "%s*" name)))
            (arglist* (-replace 'fun 'form arglist))
            (docstring* (format "Anaphoric form of `%s'." name))
            (funargs (--map (if (eq it 'form) '(lambda (it) (\, form))
                              (cons '\, (list it)))
                            arglist*))
            (body* (cdr (backquote-process (backquote (,name ,@funargs)))))
            (indent* `(declare (indent ,(-elem-index 'form arglist*)))))
      `(progn
         (defmacro ,name* ,arglist*
           ,docstring*
           ,indent*
           (om--verify form listp)
           ,body*)
         (defun ,name ,arglist
           ,docstring
           ,indent*
           (om--verify fun functionp)
           ,@body)))))

;; defun with runtime type checking

(defmacro om--defun-test-node (arglist)
  (-let* ((type (-last-item arglist))
          (pre (if (= 1 (length arglist)) "Argument" "Last argument"))
          ((post test)
           (cl-case type
             (node '("node" (om--is-node-p)))
             (branch-node '("branch node" (om--is-branch-node-p)))
             (t `(,(format "node of type %s" type)
                  (om--is-type-p ',type)))))
          (msg (format "%s must be a %s" pre post)))
    `(unless (,@test ,type) (error ,msg))))

(defmacro om--defun-node (name arglist &rest args)
  (declare (doc-string 3) (indent 2))
  (om--defun-with-docstring-body args
    `(defun ,name ,arglist
       ,docstring
       ,`(om--defun-test-node ,arglist)
       ,@body)))

;; combine type checking and anaphoric form generation

(defmacro om--defun-node* (name arglist &rest args)
  (declare (doc-string 3) (indent 2))
  (om--defun-with-docstring-body args
    `(om--defun* ,name ,arglist
       ,docstring
       ,`(om--defun-test-node ,arglist)
       ,@body)))

;; defun for weirdly-typed nodes

(defmacro om--defun-timestamp (name arglist &rest args)
  (declare (doc-string 3) (indent 2))
  (om--defun-with-docstring-body args
    (let* ((last (-last-item arglist))
           (pre (if (= 1 (length arglist)) "Argument" "Last argument"))
           (msg (format "%s must be a non-diary timestamp node" pre)))
      `(defun ,name ,arglist
         ,docstring
         (unless
             (and (om--is-type-p 'timestamp ,last)
                  (not (om--property-is-eq-p :type 'diary ,last)))
           (error ,msg))
         ,@body))))

;;; LIST OPERATIONS (EXTENDING DASH.el)

(defun om--pad-or-truncate (length pad list)
  (let ((blanks (- length (length list))))
    (if (< blanks 0) (-slice list 0 (1- length))
      (append list (-repeat blanks pad)))))

(defun om--plist-get-keys (plist)
  (-slice plist 0 nil 2))

(defun om--plist-get-vals (plist)
  (-slice plist 1 nil 2))

(defun om--plist-non-nil (plist)
  (->> (-partition 2 plist) (-filter #'cadr) (apply #'append)))

(defun om--plist-map-values (fun plist)
  (let ((keys (om--plist-get-keys plist)))
    (->> (om--plist-get-vals plist)
         (--map (funcall fun it))
         (-interleave keys))))

(defun om--is-plist-p (obj)
  "Return t if OBJ is a plist."
  (and
   (listp obj)
   (cl-evenp (length obj))
   (-all? #'symbolp (-slice obj 0 nil 2))))

(defun om--plist-remove (key plist)
  (->> (-partition 2 plist) (--remove (eq (car it) key)) (-flatten-n 1)))

(defun om--convert-intra-index (n list &optional permit-error)
  (let* ((N (length list))
         (upper (1- N))
         (lower (- N)))
    (cond
     ((<= 0 n upper) n)
     ((>= -1 n lower) (+ N n))
     (t (unless permit-error
          (error "Index (%s) out of range; must be between %s and %s"
                 n lower upper))))))

(defun om--convert-inter-index (n list &optional permit-error)
  (let* ((N (length list))
         (upper N)
         (lower (- (- N) 1)))
    (cond
     ((<= 0 n upper) n)
     ((>= -1 n lower) (+ 1 N n))
     (t (unless permit-error
          (error "Index (%s) out of range; must be between %s and %s"
                 n lower upper))))))

(defun om--insert-at (n x list)
  "Like `-insert-at' but honors negative indices N.
Negative indices count from the end of the list, with -1 inserting
X after the last element in LIST. Will give an error if N refers to
a non-existent index."
  (-insert-at (om--convert-inter-index n list) x list))

(defun om--remove-at (n list)
  "Like `-remove-at' but honors negative indices N.
Negative indices count from the end of the list, with -1 inserting
X after the last element in LIST. Will give an error if N refers to
a non-existent index."
  (-remove-at (om--convert-intra-index n list) list))

(defun om--replace-at (n x list)
  (-replace-at (om--convert-intra-index n list) x list))

(defun om--nth (n list)
  (nth (om--convert-intra-index n list) list))

(om--defun* om--map-first (fun list)
  (->> (cdr list) (cons (funcall fun (car list)))))

(om--defun* om--map-last (fun list)
  (->> (nreverse list) (om--map-first fun) (nreverse)))

;;; MISC HELPER FUNCTIONS

(defun om--get-head (node)
  "Return the type and properties cells of NODE."
  (if (stringp node) node
    (-take 2 node)))

(defun om--construct (type props children)
  "Make a new org element list structure of TYPE, PROPS, and CHILDREN.
TYPE is a symbol, PROPS is a plist, and CHILDREN is a list or nil."
  `(,type ,props ,@children))

(defun om--from-string (string)
  "Convert STRING to org-element representation."
  (with-temp-buffer
    (insert string)
    (-> (om-parse-this-buffer) (om--get-children) (car))))

(defun om--build-secondary-string (string)
  "Return a list of elements from STRING as a secondary string."
  ;; fool parser to always parse objects, bold will parse to headlines
  ;; because of the stars
  (-if-let (ss (->> (om--from-string (concat " " string))
                      (om--get-descendent '(0))
                      (om--get-children)))
      (if (equal (car ss) " ") (-drop 1 ss)
        (om--map-first* (substring it 1) ss))
    (error "Could not make secondary string from %S" string)))

;;; STRICT PROPERTY CHECKING

;; filters

(defun om--filter-type (type node)
  "Return NODE if it is TYPE or nil otherwise."
  (and (om--is-type-p type node) node))

(defun om--filter-types (types node)
  "Return NODE if it is one of TYPES or nil otherwise."
  (and (om--is-any-type-p types node) node))

;; property value predicates (type specific)

(defun om--is-valid-link-format-p (x)
  (memq x '(nil plain angle bracket)))

(defun om--is-valid-link-type-p (x)
  ;; TODO allow nil here for fuzzy?
  (->> '("coderef" "custom-id" "file" "id" "radio" "fuzzy")
       (append (org-link-types))
       (member x)))

(defun om--is-valid-item-checkbox-p (x)
  (memq x '(nil on off trans)))

(defun om--is-valid-item-tag-p (x)
  (--all? (om--is-any-type-p om--item-tag-restrictions it) x))

(defun om--is-valid-clock-timestamp-p (x)
  (and (om--is-type-p 'timestamp x)
       (om--property-is-predicate-p :type
         (lambda (it) (memq it '(inactive inactive-range))) x)
       (om--property-is-nil-p :repeater-type x)))

(defun om--is-valid-planning-timestamp-p (x)
  (or (null x) (and (om--is-type-p 'timestamp x)
                    (om--property-is-eq-p :type 'inactive x))))

(defun om--is-valid-entity-name-p (x)
  (org-entity-get x))

(defun om--is-valid-headline-tags-p (x)
  (and (-all? #'om--is-oneline-string-p x)
       (not (member org-archive-tag x))))

(defun om--is-valid-headline-priority-p (x)
  (or (null x) (and (integerp x)
                    (>= org-lowest-priority x org-highest-priority))))

(defun om--is-valid-headline-title-p (x)
  (--all? (om--is-any-type-p om--headline-title-restrictions it) x))

(defun om--is-valid-timestamp-type-p (x)
  ;; TODO allow diary here?
  (memq x '(inactive inactive-range active active-range)))

(defun om--is-valid-timestamp-repeater-type-p (x)
  (memq x '(nil catch-up restart cumulate)))

(defun om--is-valid-timestamp-warning-type-p (x)
  (memq x '(nil all first)))

(defun om--is-valid-timestamp-unit-p (x)
  (memq x '(nil year month week day hour)))

(defun om--is-valid-latex-environment-value-p (x)
  (pcase x
    ((or `(,(pred om--is-oneline-string-p))
         `(,(pred om--is-oneline-string-p) ,(pred stringp)))
     t)))

(defun om--is-valid-item-bullet-p (x)
  ;; NOTE org mode 9.1.9 has the following limitations:
  ;; - "+" will be converted to "-" when interpreted
  ;; - "1)" will be converted to "1." when interpreted
  ;; - alphanumeric symbols make the interpreter crash
  ;; TODO what to do about these limitations???
  ;; some valid org buffers might be parsed, but then can't be
  ;; use in this library because...org element is inconsistent
  (pcase x ((or '- (pred integerp)) t)))

(defun om--is-valid-statistics-cookie-value-p (x)
  (pcase x
    ((or `(nil) `(nil nil)) t)
    (`(,(and (pred integerp) percent))
     (<= 0 percent 100))
    (`(,(and (pred integerp) numerator)
       ,(and (pred integerp) denominator))
     (and (om--is-non-neg-integer-p numerator)
          (om--is-non-neg-integer-p denominator)
          (<= numerator denominator)))))

(defun om--is-valid-diary-sexp-value-p (x)
  (or (null x) (listp x)))

;; encode/decode (general)

(defun om--decode-boolean (bool)
  (and bool t))

(defun om--encode-string-or-nil (string)
  (if (null string) "" string))

(defun om--encode-string-list-delim (string-list delim)
  (-some->> string-list (s-join delim)))

(defun om--decode-string-list-delim (string delim)
  (-some->> string (s-split delim)))

(defun om--encode-string-list-space-delim (string-list)
  (om--encode-string-list-delim string-list " "))

(defun om--decode-string-list-space-delim (string)
  (om--decode-string-list-delim string " "))

(defun om--encode-string-list-comma-delim (string-list)
  (om--encode-string-list-delim string-list ","))

(defun om--decode-string-list-comma-delim (string)
  (om--decode-string-list-delim string ","))

(defun om--encode-plist (plist)
  (-some->> (--map (format "%S" it) plist) (s-join " ")))

(defun om--decode-plist (string)
  (-map #'intern (om--decode-string-list-space-delim string)))

;; encode/decode (type specific)

(defun om--encode-latex-environment-value (value)
  (-let (((env body) value))
    (if body (format "\\begin{%1$s}\n%2$s\n\\end{%1$s}" env body)
      (format "\\begin{%1$s}\n\\end{%1$s}" env))))

(defun om--decode-latex-environment-value (value)
  ;; TODO ensure that the output is correct?
  (let ((m (car (s-match-strings-all "\\\\begin{\\(.+\\)}\n\\(.*\\)\n?\\\\end{\\(.+\\)}" value))))
    (list (nth 1 m) (nth 2 m))))

(defun om--encode-item-bullet (bullet)
  ;; NOTE see `om--is-valid-item-bullet-p' for org mode limitations
  ;; relating to this function
  ;; assume bullet conforms to pcase statement below
  (pcase bullet
    ('- "- ")
    ((pred integerp) (format "%s. " bullet))
    (_ (error "This should not happen"))))

(defun om--decode-item-bullet (bullet)
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
          (error "Invalid bullet found: %s" bullet)))))

(defun om--decode-item-tag (tag)
  (om--build-secondary-string tag))

(defun om--decode-headline-tags (tags)
  (remove org-archive-tag tags))

(defun om--encode-statistics-cookie-value (value)
  ;; assumes value is a list conforming to pcase statement below
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
  (cond
   ((equal "[%]" value) '(nil))
   ((equal "[/]" value) '(nil nil))
   (t
    (->>
     (or (s-match-strings-all "\\[\\([0-9]+\\)/\\([0-9]+\\)\\]" value)
         (s-match-strings-all "\\[\\([0-9]+\\)%\\]" value)
         (error "Invalid stats-cookie: %s" value))
     (cdar)
     (-map #'string-to-number)))))

;; TODO this will make quotes turn to (quote )
(defun om--encode-diary-sexp-value (value)
  ;; assumes value is a form or nil
  (if value (format "%%%%%S" value) "%%()"))

(defun om--decode-diary-sexp-value (value)
  (->> (s-chop-prefix "%%" value) (read)))

;; cis-update functions

(defun om--update-macro-value (macro)
  (let* ((k (om--get-property :key macro))
         (as (om--get-property :args macro))
         (v (if as (format "%s(%s)" k (s-join "," as)) k)))
    (om--set-property :value (format "{{{%s}}}" v) macro)))

(defun om--update-clock-duration (clock)
  (let* ((ts (om--get-property :value clock))
         (plist
          ;; TODO this is redundant
          (if (om--timestamp-is-ranged-p ts)
              (let* ((seconds (om--timestamp-get-range ts))
                     (h (-> seconds (/ 3600) (floor)))
                     (m (-> seconds (- (* h 3600)) (/ 60) (floor))))
                `(:duration ,(format "%2d:%02d" h m) :status running))
            '(:duration nil :status closed))))
    (om--set-properties plist clock)))

(defun om--update-headline-tags (headline)
  (cl-flet
      ((add-archive-tag-maybe
        (tags)
        (let ((tags* (remove org-archive-tag tags)))
          (if (om--get-property :archivedp headline)
              (-snoc tags* org-archive-tag) tags*))))
    (om--map-property :tags #'add-archive-tag-maybe headline)))

;; shifters

(defun om--shift-pos-integer (n x)
  (when x
    (let ((x* (+ x n)))
      (if (< 0 x*) x* 1))))

(defun om--shift-non-neg-integer (n x)
  (when x
    (let ((x* (+ x n)))
      (if (<= 0 x*) x* 0))))

(defun om--shift-headline-priority (n priority)
  (when priority
    (let ((diff (1+ (- org-lowest-priority org-highest-priority)))
          (offset (- priority org-highest-priority)))
      (--> (- offset n)
           (mod it diff)
           (- it offset)
           (+ priority it)))))

(eval-when-compile
  (defconst om--type-alist
    (let ((bool (list :pred #'booleanp
                      :decode 'om--decode-boolean
                      :type-desc "nil or t"
                      :toggle t))
          (pos-int (list :pred #'om--is-pos-integer-p
                         :type-desc "a positive integer"))
          (pos-int-nil (list :pred #'om--is-pos-integer-or-nil-p
                             :type-desc "a positive integer or nil"))
          (nn-int (list :pred #'om--is-non-neg-integer-p
                        :type-desc "a non-negative integer"))
          (nn-int-nil (list :pred #'om--is-non-neg-integer-or-nil-p
                            :type-desc "a non-negative integer or nil"))
          (str (list :pred #'stringp
                     :type-desc "a string"))
          (str-nil (list :pred #'string-or-null-p
                         :type-desc "a string or nil"))
          (ol-str (list :pred #'om--is-oneline-string-p
                        :type-desc "a oneline string"))
          (ol-str-nil (list :pred #'om--is-oneline-string-or-nil-p
                            :type-desc "a oneline string or nil"))
          (plist (list :encode 'om--encode-plist
                       :pred #'om--is-plist-p
                       :decode 'om--decode-plist
                       :plist t
                       :type-desc "a plist"))
          (slist (list :pred #'om--is-string-list-p
                       :string-list t
                       :type-desc "a list of oneline strings"))
          (slist-com (list :encode 'om--encode-string-list-comma-delim
                           :decode 'om--decode-string-list-comma-delim
                           :pred #'om--is-string-list-p
                           :string-list t
                           :type-desc "a list of oneline strings"))
          (slist-spc (list :encode 'om--encode-string-list-space-delim
                           :decode 'om--decode-string-list-space-delim
                           :pred #'om--is-string-list-p
                           :string-list t
                           :type-desc "a list of oneline strings"))
          (planning (list :pred #'om--is-valid-planning-timestamp-p
                          :type-desc "a zero-range, inactive timestamp object"))
          (ts-unit (list :pred #'om--is-valid-timestamp-unit-p
                         :type-desc '("nil or a symbol from `year' `month'"
                                      "`week' `day', or `hour'"))))
      `((babel-call (:call ,@ol-str :require t)
                    (:inside-header ,@plist)
                    (:arguments ,@slist-com)
                    (:end-header ,@plist)
                    (:value))
        (bold)
        (center-block)
        (clock (:value :pred om--is-valid-clock-timestamp-p
                       :cis om--update-clock-duration
                       :type-desc "an unranged, inactive timestamp with no warning or repeater"
                       :require t)
               (:status)
               (:duration))
        (code (:value ,@str :require t))
        (comment (:value ,@ol-str :require t))
        (comment-block (:value ,@str :decode s-trim-right :require ""))
        (drawer (:drawer-name ,@ol-str :require t))
        (diary-sexp (:value :encode om--encode-diary-sexp-value
                            :pred om--is-valid-diary-sexp-value-p
                            :decode om--decode-diary-sexp-value
                            :type-desc "a list form or nil"))
        (dynamic-block (:arguments ,@plist)
                       (:block-name ,@ol-str :require t))
        (entity (:name :pred om--is-valid-entity-name-p
                       :type-desc "a string that makes `org-entity-get' return non-nil"
                       :require t)
                (:use-brackets-p ,@bool)
                ;; TODO these are useful just part of the list that
                ;; `org-entity-get' returns, might as well use them
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
                  (:priority :pred om--is-valid-headline-priority-p
                             :shift om--shift-headline-priority
                             :type-desc ("an integer between (inclusive)"
                                         "`org-highest-priority' and"
                                         "`org-lowest-priority'"))
                  (:tags :pred om--is-valid-headline-tags-p
                         :decode om--decode-headline-tags
                         :cis om--update-headline-tags
                         :type-desc "a string list"
                         :string-list t)
                  (:title :pred om--is-valid-headline-title-p
                          :type-desc "a secondary string")
                  (:todo-keyword ,@ol-str-nil) ; TODO restrict this?
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
                       :pred om--is-valid-item-bullet-p
                       :decode om--decode-item-bullet
                       :type-desc ("a positive integer (ordered)"
                                   "or the symbol `-' (unordered)")
                       :require '-)
              (:checkbox :pred om--is-valid-item-checkbox-p
                         :type-desc "nil or the symbols `on', `off', or `trans'")
              (:counter ,@pos-int-nil :shift om--shift-pos-integer)
              (:tag :pred om--is-valid-item-tag-p
                    :type-desc "a secondary string")
              (:structure))
        (keyword (:key ,@ol-str :require t)
                 (:value ,@ol-str :require t))
        (latex-environment (:value :encode om--encode-latex-environment-value
                                   :pred om--is-valid-latex-environment-value-p
                                   :decode om--decode-latex-environment-value
                                   :type-desc "a list of strings like (ENV BODY) or (ENV)"
                                   :require t))
        (latex-fragment (:value ,@str :require t))
        (line-break)
        (link (:path ,@ol-str :require t)
              (:format :pred om--is-valid-link-format-p
                       :type-desc "the symbol `plain', `bracket' or `angle'")
              (:type :pred om--is-valid-link-type-p
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
                            :pred om--is-valid-statistics-cookie-value-p
                            :decode om--decode-statistics-cookie-value
                            :type-desc ("a list of non-neg integers"
                                        "like (PERC) or (NUM DEN)"
                                        "which make [NUM/DEN] and"
                                        ;; TODO this is formatted weirdly
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
        (timestamp (:type :pred om--is-valid-timestamp-type-p
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
                   (:repeater-type :pred om--is-valid-timestamp-repeater-type-p
                                   :type-desc ("nil or a symbol from"
                                               "`catch-up', `restart',"
                                               "or `cumulate'"))
                   (:repeater-unit ,@ts-unit)
                   (:repeater-value ,@pos-int-nil)
                   (:warning-type :pred om--is-valid-timestamp-warning-type-p
                                  :type-desc ("nil or a symbol from"
                                              "`all' or `first'"))
                   (:warning-unit ,@ts-unit)
                   (:warning-value ,@pos-int-nil)
                   (:raw-value))
        (underline)
        (verbatim (:value ,@str :require t))
        (verse-block)))))

;; add post-blank functions to all entries
(let ((post-blank-funs '(:post-blank :pred om--is-non-neg-integer-p
                                     :shift om--shift-non-neg-integer)))
  (setq om--type-alist
        (--map (-snoc it post-blank-funs) om--type-alist)))

;;; INTERNAL POLYMORPHIC PROPERTY FUNCTIONS

;; property manipulation (non-strict)

(defun om--get-property (prop node)
  (if (and (stringp node) (eq prop :post-blank))
      (length (car (s-match "[ ]*$" node)))
    (org-element-property prop node)))

(defun om--get-all-properties (node)
  "Return the properties list of NODE."
  (if (stringp node) (text-properties-at 0 node) (nth 1 node)))

(defun om--get-parent (node)
  "Return the parent of NODE."
  (om--get-property :parent node))

(defun om--get-parent-headline (node)
  "Return the most immediate parent headline of NODE."
  (-when-let (parent (om--get-parent node))
    (if (om--is-type-p 'headline parent) parent
      (om--get-parent-headline parent))))

(defun om--set-property (prop value node)
  "Set property PROP in element NODE to VALUE."
  (if (stringp node)
      (if (eq prop :post-blank)
          (->> (s-trim-right node) (s-append (s-repeat value " ")))
        (org-add-props node nil prop value))
    (om--construct
     (om--get-type node)
     (plist-put (om--get-all-properties node) prop value)
     (om--get-children node))))

(defun om--set-properties (plist node)
  "Set all properties in NODE to the values corresponding to PLIST.
PLIST is a list of property-value pairs that correspond to the
property list in NODE."
  (if (om--is-plist-p plist)
      (let ((props (om--get-all-properties node)))
        (om--construct
         (om--get-type node)
         (->> (-partition 2 plist)
              (--reduce-from (apply #'plist-put acc it) props))
         (om--get-children node)))
    (error "Not a plist: %S" plist)))

(defun om--set-property-nil (prop node)
  "Set property PROP to nil in NODE."
  (om--set-property prop nil node))

(defun om--set-properties-nil (props node)
  "Set all properties PROPS to new in NODE."
  (let ((plist (--mapcat (list it nil) props)))
    (om--set-properties plist node)))

(om--defun* om--map-property (prop fun node)
  (let ((value (funcall fun (om--get-property prop node))))
    (om--set-property prop value node)))

(defun om--map-properties (plist node)
  (cond
   ((not plist) node)
   ((om--is-plist-p plist)
    (->> (om--map-property (nth 0 plist) (nth 1 plist) node)
         (om--map-properties (-drop 2 plist))))
   (t (error "Not a plist: %s" plist))))

(defmacro om--map-properties* (plist node)
  `(let ((plist*
          (-map-indexed
           (lambda (index item) (if (cl-evenp index) item `(lambda (it) ,item)))
           ,plist)))
     (om--map-properties new-plist ,node)))

(defun om--property-is-nil-p (prop node)
  "Return t if PROP in NODE is nil."
  (not (om--get-property prop node)))

(defun om--property-is-non-nil-p (prop node)
  "Return t if PROP in NODE is not nil."
  (if (om--get-property prop node) t))

(defun om--property-is-eq-p (prop val node)
  "Return t if PROP in NODE is `eq' to VAL."
  (eq val (om--get-property prop node)))

(defun om--property-is-equal-p (prop val node)
  "Return t if PROP in NODE is `equal' to VAL."
  (equal val (om--get-property prop node)))

(om--defun* om--property-is-predicate-p (prop fun node)
  "Return t if FUN applied to the value of PROP in NODE results not nil.
FUN is a predicate function that takes one argument."
  (and (funcall fun (om--get-property prop node)) t))

;; property manipulation (strict)

(defun om--get-strict-function (operation type prop)
  (-if-let (type-list (alist-get type om--type-alist))
      (-if-let (plist (alist-get prop type-list))
          (plist-get plist operation)
        (error "Unsettable property '%s' for type '%s' requested; settable properties are %s"
               prop type (->> (--map (symbol-name (car it)) type-list)
                              (s-join ", "))))
    (error "Tried to get property for non-existent type %s" type)))

(defun om--get-setter-function (type prop)
  (om--get-strict-function :encode type prop))

(defun om--get-getter-function (type prop)
  (om--get-strict-function :decode type prop))

(defun om--get-update-function (type prop)
  (om--get-strict-function :cis type prop))

(defun om--get-type-desc (type prop)
  (let ((desc (om--get-strict-function :type-desc type prop)))
    (if (listp desc) (s-join " " desc) desc)))

(defun om--set-property-strict (prop value node)
  (let* ((type (om--get-type node))
         (pred (om--get-strict-function :pred type prop)))
    (if (funcall pred value)
        (let* ((encode-fun (om--get-setter-function type prop))
               (update-fun (om--get-update-function type prop)))
          (-->
           (if encode-fun (funcall encode-fun value) value)
           (om--set-property prop it node)
           (if update-fun (funcall update-fun it) it)))
      (error "Property '%s' in node of type '%s' must be %s. Got '%S'"
             prop type (om--get-type-desc type prop) value))))

(defun om--set-properties-strict (plist node)
  (cl-flet
      ((filter
        (acc keyval type)
        (-let* (((prop value) keyval)
                (pred (om--get-strict-function :pred type prop)))
          (if (funcall pred value)
              (let ((encode-fun (om--get-setter-function type prop)))
                (->> (if encode-fun (funcall encode-fun value) value)
                     (funcall #'plist-put acc prop)))
            (error "Property '%s' in node of type '%s' must be %s. Got '%S'"
                   prop type (om--get-type-desc type prop) value)))))
    (if (om--is-plist-p plist)
        (let* ((cur-props (om--get-all-properties node))
               (type (om--get-type node))
               (keyvals (-partition 2 plist))
               (update-funs
                (->> (-map #'car keyvals)
                     (--map (om--get-update-function type it))
                     (-uniq)
                     (-non-nil)))
               (node*
                (om--construct
                 (om--get-type node)
                 (--reduce-from (filter acc it type) cur-props keyvals)
                 (om--get-children node))))
          (if (not update-funs) node*
            (--reduce-from (funcall it acc) node* update-funs)))
      (error "Not a plist: %S" plist))))

(defun om--get-property-strict (prop node)
  (let ((filter-fun (-> (om--get-type node)
                        (om--get-getter-function prop)))
        (value (om--get-property prop node)))
    (if filter-fun (funcall filter-fun value) value)))

(om--defun* om--map-property-strict (prop fun node)
  (let ((value (funcall fun (om--get-property-strict prop node))))
    (om--set-property-strict prop value node)))

(defun om--map-properties-strict (plist node)
  (cond
   ((not plist) node)
   ((om--is-plist-p plist)
    (->> (om--map-property-strict (nth 0 plist) (nth 1 plist) node)
         (om--map-properties-strict (-drop 2 plist))))
   (t (error "Not a plist: %s" plist))))

;; branch/child manipulation

(defalias 'om--get-children 'org-element-contents)

(defun om--get-descendent (indices node)
  "Return the nested children of NODE as given by INDICES.
INDICES is a list of integers specifying the index and level of the
nested element to return."
  (if (not indices) node
    (->> (om--get-children node)
         (nth (car indices))
         (om--get-descendent (cdr indices)))))

(defun om--is-childless-p (node)
  "Return t if NODE has no children."
  (not (om--get-children node)))

(defun om--set-children (children node)
   (let ((head (om--get-head node)))
     (if children (append head children) head)))

(om--defun* om--map-children (fun node)
  (let ((children (om--get-children node)))
    ;; TODO check the types of children after they are mapped?
    (om--set-children (funcall fun children) node)))

(defun om--set-children-restricted (types children node)
  (-when-let (illegal (-some->> (-map #'om--get-type children)
                                (--remove (memq it types))
                                (-map #'symbol-name)
                                (s-join ", ")))
    (error "Illegal types found: %s; allowed types are: %s"
           illegal (s-join ", " (-map #'symbol-name types))))
  (om--set-children children node))

(defun om--set-children-by-type (branch-type children node)
  ;; TODO there may be additional restrictions, such as newlines
  ;; in strings not being allowed
  (-if-let (types (alist-get branch-type om-node-restrictions))
      (om--set-children-restricted types children node)
    (error "Invalid branch type requested: %s" branch-type)))

;;; BASE BUILDER FUNCTIONS

;; build helpers

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
  (->> (om--set-property-strict :post-blank (or post-blank 0) `(,type nil))
       (om--set-properties-nil props)))

(defun om--build-object (type post-blank)
  (om--build type post-blank om--object-properties))

(defun om--build-recursive-object (type post-blank objs)
  (->> om--recursive-object-properties
       (om--build type post-blank)
       (om--set-children-by-type type objs)))

(defun om--build-element (type post-blank)
  (om--build type post-blank om--element-properties))

(defun om--build-container-element (type post-blank nodes)
  (->> om--container-element-properties
       (om--build type post-blank)
       (om--set-children-by-type type nodes)))

;; define all base builders using this automated monstrosity

(eval-when-compile
  (defun om--kwd-to-sym (keyword)
    (->> (symbol-name keyword) (s-chop-prefix ":") (intern)))

  (defun om--prepend-article (string)
    (let ((a (--> (symbol-name string)
                  (s-left 1 it)
                  (if (member it '("a" "e" "i" "o" "u")) "an" "a"))))
      (format "%s %s" a string)))

  (--each (--remove (eq 'plain-text (car it)) om--type-alist)
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
                          `(om--set-property ,@it)
                        `(om--set-properties (list ,@it)))))
           (nil-props
            (-some--> (alist-get 'null props)
                      (-map #'car it)
                      (if (= 1 (length it))
                          `(om--set-property-nil ,@it)
                        `(om--set-properties-nil (list ,@it)))))
           (strict-props
            (-some-->
             (append (alist-get 'key props) (alist-get 'req props))
             (-map #'car it)
             (--mapcat (list it (om--kwd-to-sym it)) it)
             (if (= 2 (length it))
                 `(om--set-property-strict ,@it)
               `(om--set-properties-strict (list ,@it)))))
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

;; INTERNAL NODE-SPECIFIC PROPERTY FUNCTIONS

;;; objects
;;
;; statistics-cookie

(defun om--statistics-cookie-is-complete-p (statistics-cookie)
  (let ((val (om--get-property :value statistics-cookie)))
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

(defun om--statistics-cookie-get-format (statistics-cookie)
  (let ((value (om--get-property :value statistics-cookie)))
    (cond ((s-contains? "/" value) 'fraction)
          ((s-contains? "%" value) 'percent)
          (t (error "Unparsable statistics cookie: %s" value)))))

;; timestamp (auxiliary functions)

(defun om--time-is-long-p (time)
  (pcase time
    (`(,(pred integerp) ,(pred integerp) ,(pred integerp)
       ,(pred integerp) ,(pred integerp))
     t)))

(defun om--time-to-unixtime (time)
  (let ((encoded
         (if (om--time-is-long-p time)
             (apply #'encode-time 0 (nreverse time))
           (apply #'encode-time 0 0 0 (nreverse (-take 3 time))))))
    (round (float-time encoded))))

(defun om--unixtime-to-time-long (unixtime)
  (nreverse (-slice (decode-time unixtime) 1 6)))

(defun om--unixtime-to-time-short (unixtime)
  (append (-take 3 (om--unixtime-to-time-long unixtime))
          '(nil nil)))

(defun om--time-truncate (time)
  (-take 3 time))

(defun om--time-shift (n unit time)
  (cl-flet*
      ((get-shifts-short
        (n unit)
        (cl-case unit
          (day `(0 0 ,n 0 0))
          (week `(0 0 ,(* 7 n) 0 0))
          (month `(0 ,n 0 0 0))
          (year `(,n 0 0 0 0))
          ((minute hour)
           (error "Invalid unit for short timestamps: %S" unit))
          (t (error "Invalid time unit: %S" unit))))
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
    (if (om--time-is-long-p time)
        (let ((shifts (get-shifts-long n unit)))
          (nreverse (-slice (apply-shifts shifts time) 1 6)))
      (let ((shifts (get-shifts-short n unit))
            (time* (-replace nil 0 time)))
        (->> (-slice (apply-shifts shifts time*) 3 6)
             (append '(nil nil))
             (nreverse))))))

(defun om--time-format-props (time suffix)
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
                  (_ (error "Invalid time given: %s" time)))))
    (-interleave props time*)))

(defun om--decorator-format (dec dtype valid-types)
  (let ((props (->> '(type value unit)
                    (--map (intern (format ":%s-%s" dtype it))))))
    (if (not dec) (om--init-properties props)
      (-let (((type value unit) dec))
        (unless (memq type valid-types)
          (error "Invalid %s type: %s" dtype type))
        (unless (integerp value)
          (error "Invalid %s value: %s" dtype value))
        (unless (memq unit '(year month week day hour))
          (error "Invalid %s unit: %s" dtype value))
        (-interleave props (list type value unit))))))

;; timestamp (regular)

(defun om--timestamp-get-start-time (timestamp)
  (-let (((&plist :minute-start n :hour-start h :day-start d
                  :month-start m :year-start y)
          (om--get-all-properties timestamp)))
    `(,y ,m ,d ,h ,n)))

(defun om--timestamp-get-end-time (timestamp)
  (-let (((&plist :minute-end n :hour-end h :day-end d
                  :month-end m :year-end y)
          (om--get-all-properties timestamp)))
    `(,y ,m ,d ,h ,n)))

(defun om--timestamp-get-start-timestamp (timestamp)
  (if (not (om--timestamp-is-ranged-fast-p timestamp)) timestamp
    (om--timestamp-set-end-time nil timestamp)))

(defun om--timestamp-get-end-timestamp (timestamp)
  (when (om--timestamp-is-ranged-fast-p timestamp)
    (-> (om--timestamp-get-end-time timestamp)
        (om--timestamp-set-single-time timestamp))))

(defun om--timestamp-get-start-unixtime (timestamp)
  (->> (om--timestamp-get-start-time timestamp)
       (om--time-to-unixtime)))

(defun om--timestamp-get-end-unixtime (timestamp)
  (->> (om--timestamp-get-end-time timestamp)
       (om--time-to-unixtime)))

(defun om--timestamp-get-range (timestamp)
  (- (om--timestamp-get-end-unixtime timestamp)
     (om--timestamp-get-start-unixtime timestamp)))

(defun om--timestamp-is-active-p (timestamp)
   (memq (om--get-property :type timestamp) '(active active-range)))

(defun om--timestamp-is-ranged-p (timestamp)
  (/= 0 (om--timestamp-get-range timestamp)))

(defun om--timestamp-is-ranged-lowres-p (timestamp)
  (-let* (((l s) (-split-at 3 (om--timestamp-get-start-time timestamp)))
          ((L S) (-split-at 3 (om--timestamp-get-end-time timestamp))))
    ;; lowres if Y/M/D is different and Min/Hour are the same
    ;; but only if Min/Hour are both not nil
    (and (not (equal l L)) (or (equal s S)
                               (memq nil s)
                               (memq nil S)))))

(defun om--timestamp-start-is-long-p (timestamp)
  (->> (om--timestamp-get-start-time timestamp)
       (om--time-is-long-p)))

(defun om--timestamp-end-is-long-p (timestamp)
  (->> (om--timestamp-get-end-time timestamp)
       (om--time-is-long-p)))

(defun om--timestamp-start-is-less-than-p (unixtime timestamp)
  (< (om--timestamp-get-start-unixtime timestamp) unixtime))

(defun om--timestamp-start-is-greater-than-p (unixtime timestamp)
  (> (om--timestamp-get-start-unixtime timestamp) unixtime))

(defun om--timestamp-start-is-equal-to-p (unixtime timestamp)
  (= (om--timestamp-get-start-unixtime timestamp) unixtime))

(defun om--timestamp-end-is-less-than-p (unixtime timestamp)
  (< (om--timestamp-get-end-unixtime timestamp) unixtime))

(defun om--timestamp-end-is-greater-than-p (unixtime timestamp)
  (> (om--timestamp-get-end-unixtime timestamp) unixtime))

(defun om--timestamp-end-is-equal-to-p (unixtime timestamp)
  (= (om--timestamp-get-end-unixtime timestamp) unixtime))

(defun om--timestamp-is-ranged-fast-p (timestamp)
  "Like `om--timestamp-is-ranged-p' but faster.
This only looks at TIMESTAMP's :type property rather than computing
float-times, which assumes the :type property is valid."
  (memq (om--get-property :type timestamp)
        '(active-range inactive-range)))

(defun om--timestamp-set-start-time-nocheck (time timestamp)
  "Set the start TIME of TIMESTAMP."
  (let ((time* (om--time-format-props time 'start)))
      (om--set-properties time* timestamp)))

(defun om--timestamp-set-start-time (time timestamp)
  (->> (om--timestamp-set-start-time-nocheck time timestamp)
       (om--timestamp-update-type-ranged)))

(defun om--timestamp-set-end-time-nocheck (time timestamp)
  "Set the end TIME of TIMESTAMP."
  (if time
      (-> (om--time-format-props time 'end)
          (om--set-properties timestamp))
    (-> (om--timestamp-get-start-time timestamp)
        (om--time-format-props 'end)
        (om--set-properties timestamp))))

(defun om--timestamp-set-end-time (time timestamp)
  (let ((ts* (om--timestamp-set-end-time-nocheck time timestamp)))
    (if time (om--timestamp-update-type-ranged ts*)
      (om--timestamp-set-type-ranged nil ts*))))

(defun om--timestamp-set-single-time (time timestamp)
  "Set the start TIME of TIMESTAMP."
  (->> (om--timestamp-set-start-time-nocheck time timestamp)
       (om--timestamp-set-end-time-nocheck time)
       (om--timestamp-set-type-ranged nil)))

(defun om--timestamp-set-double-time (time1 time2 timestamp)
  (->> (om--timestamp-set-start-time-nocheck time1 timestamp)
       (om--timestamp-set-end-time-nocheck time2)
       (om--timestamp-update-type-ranged)))

(defun om--timestamp-set-range (range timestamp)
  (let* ((start (om--timestamp-get-start-time timestamp))
         (long? (om--time-is-long-p start))
         (range (* range (if long? 60 86400)))
         (t2 (--> (om--time-to-unixtime start)
                  (+ it range)
                  (if long? (om--unixtime-to-time-long it)
                    (om--unixtime-to-time-short it)))))
    (->> (om--timestamp-set-end-time-nocheck t2 timestamp)
         (om--timestamp-update-type-ranged))))

(defun om--timestamp-update-type-ranged (timestamp)
  (-> (om--timestamp-is-ranged-lowres-p timestamp)
      (om--timestamp-set-type-ranged timestamp)))

(defun om--timestamp-set-type-ranged (ranged? timestamp)
  (cl-flet
      ((update-range
       (type)
       (cl-case type
         ((active active-range)
          (if ranged? 'active-range 'active))
         ((inactive inactive-range)
          (if ranged? 'inactive-range 'inactive))
         (t (error "Invalid timestamp type: %s" type)))))
    (om--map-property :type #'update-range timestamp)))

(defun om--timestamp-set-type (type timestamp)
  (let* ((range? (om--timestamp-is-ranged-lowres-p timestamp))
         (type* (cl-case type
                  (active (if range? 'active-range 'active))
                  (inactive (if range? 'inactive-range 'inactive))
                  (t (error "Invalid timestamp type: %s" type)))))
    (om--set-property :type type* timestamp)))

(defun om--timestamp-set-warning (warning timestamp)
  (let ((types '(all first)))
    (-> (om--decorator-format warning 'warning types)
        (om--set-properties timestamp))))

(defun om--timestamp-set-repeater (repeater timestamp)
  (let ((types '(catch-up restart cumulate)))
    (-> (om--decorator-format repeater 'repeater types)
        (om--set-properties timestamp))))

(defun om--timestamp-shift-start (n unit timestamp)
  (let ((time* (->> (om--timestamp-get-start-time timestamp)
                    (om--time-shift n unit))))
    (->> (om--timestamp-set-start-time time* timestamp)
         (om--timestamp-update-type-ranged))))

(defun om--timestamp-shift-end (n unit timestamp)
  (let ((time* (->> (om--timestamp-get-end-time timestamp)
                    (om--time-shift n unit))))
    (->> (om--timestamp-set-end-time time* timestamp)
         (om--timestamp-update-type-ranged))))

(defun om--timestamp-shift-range (n unit timestamp)
  (->> (om--timestamp-shift-start n unit timestamp)
       (om--timestamp-shift-end n unit)))

(defun om--timestamp-toggle-active (timestamp)
  (--> (om--timestamp-is-active-p timestamp)
       (if it 'inactive 'active)
       (om--timestamp-set-type it timestamp)))

;; timestamp (diary sexp)

(defun om--timestamp-diary-set-value (form timestamp)
  (om--verify form listp)
  (om--set-property :raw-value (format "<%%%%%S>" form) timestamp))

;;; elements
;;
;; headline

(defun om--headline-set-title! (string stat-ckie headline)
  (let ((ss (om--build-secondary-string string)))
    (if (not stat-ckie)
        (om--set-property-strict :title ss headline)
      (let ((ss* (om--map-last*
                  (om--set-property :post-blank 1 it) ss))
            (sc (om-build-statistics-cookie stat-ckie)))
        (om--set-property-strict :title (-snoc ss* sc) headline)))))

(defun om--headline-shift-level (n headline)
  (om--verify n integerp)
  (om--map-property* :level (om--shift-pos-integer n it) headline))

;; item

;; (defun om--item-set-tag! (raw-tag item)
;;   (-> (om--build-secondary-string raw-tag)
;;       (om--item-set-tag item)))

(defun om--item-toggle-checkbox (item)
  (cl-case (om--get-property :checkbox item)
    ((or trans nil) item)
    ('on (om--set-property :checkbox 'off item))
    ('off (om--set-property :checkbox 'on item))
    (t (error "This should not happen"))))

;; planning

(defun om--planning-list-to-timestamp (planning-list)
  (when planning-list
    (let* ((p (-partition-before-pred
               (lambda (it) (memq it '(&warning &repeater)))
               planning-list)))
      (om-build-timestamp! 'inactive (car p)
                                :warning (alist-get '&warning p)
                                :repeater (alist-get '&repeater p)))))

;;; INTERNAL BRANCH/CHILD FUNCTIONS
;; operations on children of branch nodes


;; headline

(defun om--headline-get-subheadlines (headline)
  (-some->> (om--get-children headline)
            (--filter (om--is-type-p 'headline it))))

(defun om--headline-get-section (headline)
  (-some->> (om--get-children headline) (assoc 'section)))

(defun om--headline-get-statistics-cookie (headline)
  (->> (om--get-property :title headline)
       (-last-item)
       (om--filter-type 'statistics-cookie)))

(defun om--headline-get-properties-drawer (headline)
  (-some->>
   (om--headline-get-section headline)
   (--first (om--is-type-p 'property-drawer it))))

(defun om--headline-get-node-properties (headline)
  (-some->>
   (om--headline-get-properties-drawer headline)
   (om--get-children)
   (--filter (om--is-type-p 'node-property it))))

(defun om--headline-get-planning (headline)
  (-some->> (om--headline-get-section headline)
            (om--get-children)
            (--first (om--is-type-p 'planning it))))

(defun om--headline-map-subheadlines (fun headline)
  (om--map-children
   (lambda (children)
     (let ((section (assoc 'section children))
           (subheadlines (-some->>
                          (--filter (om--is-type-p 'headline it) children)
                          (funcall fun))))
       (cond
        ((and section subheadlines) (cons section subheadlines))
        (section section)
        (t subheadlines))))
   headline))

;; (defun om--map-or-build (map-fun build-fun pred-fun pos node)
;;   (om--map-children
;;    (lambda (children)
;;      (-if-let (target (--first (funcall pred-fun it) children))
;;          ;; TODO this is probably not the most efficient
;;          (-replace target (funcall map-fun target) children)
;;        (let ((pos
;;               (cond
;;                ((integerp pos)
;;                 pos)
;;                ((functionp pos)
;;                 (or (--find-index (funcall pos it) children) 0))
;;                (t (error "Invalid pos given: %S" pos))))
;;              (new (funcall build-fun)))
;;          (-insert-at pos new children))))
;;    node))

;; (defmacro om--map-or-build-nested (map-form &rest args)
;;   ;; forms are of form (pred builder pos-fun)
;;   (declare (indent 1))
;;   (let* ((node (-last-item args))
;;          (forms (-drop-last 1 args))
;;          (first (car args))
;;          (rem (cdr forms))
;;          (pred-fun `(lambda (it) ,(nth 0 first)))
;;          (build-fun `(lambda () (->> ,@(nreverse (--map (nth 1 it) forms)))))
;;          (pos-fun `(lambda (it) ,(nth 2 first)))
;;          (map-fun
;;           (if (not rem) `(lambda (it) ,map-form)
;;             `(lambda (inner)
;;                (om--map-or-build-nested ,map-form ,@rem inner)))))
;;     `(om--map-or-build ,map-fun ,build-fun ,pred-fun ,pos-fun ,node)))

(defun om--headline-subtree-shift-level (n headline)
  (->> (om--headline-shift-level n headline)
       (om--headline-map-subheadlines
        (lambda (headlines)
          (--map (om--headline-subtree-shift-level n it)
                 headlines)))))

;; (defun om--headline-set-section (section headline)
;;   (let ((subheadlines (om--headline-get-subheadlines headline)))
;;     (om--set-children (cons section subheadlines) headline)))

;; (defun om--headline-set-property-drawer (property-drawer headline)
;;   (om--headline-set-section (om-build-section property-drawer)))

;; (defun om--headline-set-node-property (key value headline)
;;   (om--map-or-build-nested (om--set-property-strict :value value it)
;;     ((om-is-section-p it)
;;      (om-build-section)
;;      0)
;;     ((om-is-property-drawer-p it)
;;      (om-build-property-drawer)
;;      (-if-let (i (-find-index (om-is-planning-p it) it)) ((1+ i) it)))
;;     ((and (om-is-node-property-p it)
;;           (om--property-is-equal-p :value key it))
;;      (om-build-node-property key value) 
;;      0)
;;     headline))

;; (defun om--headline-set-planning (planning headline)
;;   ;; TODO what if we give this a nil?
;;   (om--map-or-build-nested (om--set-property-strict :value value it)
;;     ((om-is-section-p it) (om-build-section) 0)
;;     ((om-is-planning-p it) (apply #'om-build-planning planning) 0)
;;     headline))

(defun om--headline-set-statistics-cookie (value headline)
  (om--map-property*
   :title
   (let ((last? (om--is-type-p 'statistics-cookie (-last-item it))))
     (cond
      ((and last? value)
       (om--map-last* (om--set-property-strict :value value it) it))
      ((and last? (not value))
       (-drop-last 1 it))
      (value 
       (-snoc it (om-build-statistics-cookie value)))
      (t it)))
   headline))

(defun om--headline-set-statistics-cookie-fraction (done total headline)
  (-if-let (cookie (om--headline-get-statistics-cookie headline))
      (let* ((format (om--statistics-cookie-get-format cookie))
             (value (if (eq 'fraction format) `(,done ,total)
                      (-> (float done)
                          (/ total)
                          (* 100)
                          (round)
                          (list)))))
        (om--headline-set-statistics-cookie value headline))
    headline))

;; table

(defun om--table-pad-or-truncate (length list)
  (let ((pad (om-build-table-cell "")))
    (om--pad-or-truncate length pad list)))

(defun om--table-delete-column (index table)
  (om--verify index integerp)
  (cl-flet*
      ((delete-cell
        (cells)
        (om--remove-at index cells))
       (map-row 
        (row)
        (if (om--property-is-eq-p :type 'rule row) row
          (om--map-children #'delete-cell row))))
    (om--map-children* (-map #'map-row it) table)))

(defun om--table-delete-row (index table)
  (om--verify index integerp)
  (om--map-children* (om--remove-at index it) table))

(defun om--column-map-down-rows (fun column table)
  (cl-flet*
      ((zip-into-rows
        (row new-cell)
        (if (om--property-is-eq-p :type 'rule row) row
          (om--map-children
           (lambda (cells) (funcall fun new-cell cells))
           row)))
       (map-rows
        (rows)
        (->> rows
             (--find-indices (om--property-is-eq-p :type 'rule it))
             (--reduce-from (-insert-at it nil acc) column)
             (om--table-pad-or-truncate (length rows))
             (-zip-with #'zip-into-rows rows))))
    (om--map-children #'map-rows table)))

(defun om--table-insert-column (index column table)
  (om--verify index integerp)
  (om--column-map-down-rows
   (lambda (new-cell cells) (om--insert-at index new-cell cells))
   column
   table))

(defun om--table-insert-row (index row table)
  (om--verify index integerp)
  (let ((row (if (om--property-is-eq-p :type 'rule row) row
               (let ((width (om--table-get-width table)))
                 (om--map-children*
                  (om--table-pad-or-truncate width it)
                  row)))))
    (om--map-children* (om--insert-at index row it) table)))

(defun om--table-get-column (column table)
  (-some->> (om--get-children table)
            (--filter (om--property-is-eq-p :type 'standard it))
            (--map (->> (om--get-children it)
                        (om--nth column)))))

(defun om--table-get-row (row table)
  (-some->> (om--get-children table)
            (--filter (om--property-is-eq-p :type 'standard it))
            (om--nth row)))

(defun om--table-get-cell (row column table)
  "Return table-cell element at ROW and COLUMN indices in TABLE element.
Hlines do not count toward row indices, and all indices are
zero-indexed."
  (-some->> (om--table-get-row row table)
            (om--get-children)
            (om--nth column)))

(defun om--table-replace-column (index column table)
  (om--verify index integerp)
  (om--column-map-down-rows
   (lambda (new-cell cells) (om--replace-at index new-cell cells))
   column
   table))

;; TODO this is not dry...
(defun om--table-replace-row (index row table)
  (om--verify index integerp)
  (let ((row (if (om--property-is-eq-p :type 'rule row) row
               (let ((width (om--table-get-width table)))
                 (om--map-children*
                  (om--table-pad-or-truncate width it) row)))))
    (om--map-children* (om--replace-at index row it) table)))

(defun om--table-replace-cell (row-index column-index cell table)
  (let ((row (->> (om--table-get-row row-index table)
                  (om--map-children*
                   (om--replace-at column-index cell it)))))
    (om--table-replace-row row-index row table)))

(defun om--table-clear-cell (row-index column-index table)
  (om--table-replace-cell row-index column-index (om-build-table-cell " ") table))

(defun om--table-clear-row (index table)
  ;; this assumes the blank cell will be padded with other blank cells
  (om--table-replace-row index (om-build-table-row (om-build-table-cell " ")) table))

(defun om--table-clear-column (index table)
  ;; this assumes the blank cell will be padded with other blank cells
  (om--table-replace-column index (list (om-build-table-cell "")) table))

(defun om--table-get-height (table)
  (length (om--get-children table)))

(defun om--table-get-width (table)
  (->> (om--get-children table)
       (--map (length (om--get-children it)))
       (-max)))

;;; INTERNAL INDENTATION

;;; helper functions

;; TODO this is a bit sketchy...it depends on the indentation
;; function to make the children list one element shorter, which
;; is usually true but makes a really hard error to catch when it
;; fails
(defun om--indent-after (indent-fun index node)
  (unless (and (integerp index) (<= 0 index))
    (error "Index must be non-negative integer"))
  (if (< index (1- (length (om--get-children node))))
      (->> (funcall indent-fun (1+ index) node)
           (om--indent-after indent-fun index))
    node))

(defun om--indent-members (fun index members)
  (unless (and (integerp index) (< 0 index))
    (error "Cannot indent topmost item at this level"))
  (-let* (((head tail) (-split-at index members))
          (target (-first-item tail))
          (head* (om--map-last* (funcall fun target it) head)))
    (append head* (-drop 1 tail))))

(defun om--unindent-members (index parent-fun unindent-fun list)
  (unless (and (integerp index) (<= 0 index))
    (error "Index must be non-negative integer"))
  (-let* (((head tail) (-split-at index list))
          (parent (-first-item tail))
          (parent* (funcall parent-fun parent))
          (unindented (funcall unindent-fun parent)))
    (append head (list parent*) unindented (-drop 1 tail))))

;;; elements functions

;; headline

(defun om--headline-set-level (level headline)
  (->> (om--set-property-strict :level level headline)
       (om--map-children*
         (--map (om--headline-set-level (1+ level) it) it))))

;; TODO throw error when index out of range

(defun om--headline-indent-subtree (index headline)
  (cl-flet
      ((append-indented
        (target-headline parent-headline)
        (let ((target-headline*
               (om--headline-subtree-shift-level 1 target-headline)))
          (om--map-children
           (lambda (headline-children)
             (append headline-children (list target-headline*)))
           parent-headline))))
    (om--headline-map-subheadlines
     (lambda (subheadlines)
       (om--indent-members #'append-indented index subheadlines))
     headline)))

(defun om--headline-indent-subheadline (index headline)
  (cl-flet
      ((append-indented
        (target-headline parent-headline)
        (let ((target-headline*
               (->> target-headline
                    (om--headline-map-subheadlines #'ignore)
                    (om--headline-shift-level 1)))
              (headlines-in-target
               (om--headline-get-subheadlines target-headline))) 
          (om--map-children
           (lambda (children)
             (append children (list target-headline*) headlines-in-target))
           parent-headline))))
    (om--headline-map-subheadlines
     (lambda (subheadlines)
       (om--indent-members #'append-indented index subheadlines))
     headline)))

(defun om--headline-unindent-subheadline (index child-index headline)
  (cl-flet
      ((trim
        (parent)
        (om--headline-map-subheadlines
         (lambda (subheadlines) (-take child-index subheadlines))
         parent))
       (extract
        (parent)
        (->> (om--indent-after #'om-headline-indent-subtree
                                    child-index parent)
             (om--get-children)
             (-drop child-index)
             (--map (om--headline-subtree-shift-level -1 it)))))
    (om--headline-map-subheadlines
     (lambda (subheadlines)
       (om--unindent-members index #'trim #'extract subheadlines))
     headline)))

(defun om--headline-unindent-subtree (index headline)
  (cl-flet
      ((trim
        (parent)
        (om--headline-map-subheadlines #'ignore parent))
       (extract
        (parent)
        (->> (om--get-children parent)
             (--map (om--headline-subtree-shift-level -1 it)))))
    (om--headline-map-subheadlines
     (lambda (subheadlines)
       (om--unindent-members index #'trim #'extract subheadlines))
     headline)))

;; plain-list

(defun om--plain-list-indent-item-tree (index plain-list)
  (cl-flet
      ((append-indented
        (target-item parent-item)
        (let ((target-item* (om-build-plain-list target-item)))
          (om--map-children
           (lambda (item-children) (append item-children (list target-item*)))
           parent-item))))
    (om--map-children
     (lambda (items)
       (om--indent-members #'append-indented index items))
     plain-list)))

(defun om--plain-list-indent-item (index plain-list)
  (cl-flet
      ((append-indented
        (target-item parent-item)
        (let ((target-item*
               (->> target-item
                    (om--map-children*
                     (--remove (om--is-type-p 'plain-list it) it))
                    (om-build-plain-list)))
              (items-in-target
               (->> (om--get-children target-item)
                    (--filter (om--is-type-p 'plain-list it)))))
          (om--map-children
           (lambda (item-children)
             ;; TODO technically the target-item* should go in an
             ;; existing plain list but I don't this matters (for now)
             (append item-children (list target-item*) items-in-target))
           parent-item))))
    (om--map-children
     (lambda (items)
       (om--indent-members #'append-indented index items))
     plain-list)))

(defun om--plain-list-unindent-item (index child-index plain-list)
  (cl-flet
      ((trim
        (parent)
        (om--map-children
         (lambda (children)
           (if (= 0 index)
               (--remove-first (om--is-type-p 'plain-list it) children)
             (--map-first (om--is-type-p 'plain-list it)
                          (om--map-children
                           (lambda (items) (-take child-index items)) it)
                          children)))
         parent))
       (extract
        (parent)
        (->>
         (om--get-children parent)
         (--first (om--is-type-p 'plain-list it))
         (om--indent-after #'om--plain-list-indent-item-tree
                                child-index)
         (om--get-children)
         (-drop child-index))))
    (om--map-children
     (lambda (items)
       (om--unindent-members index #'trim #'extract items))
     plain-list)))

(defun om--plain-list-unindent-items (index plain-list)
  (cl-flet
      ((trim
        (parent)
        (om--map-children
         (lambda (children)
           (--remove-first (om--is-type-p 'plain-list it) children))
         parent))
       (extract
        (parent)
        (->> (om--get-children parent)
             (--first (om--is-type-p 'plain-list it))
             (om--get-children))))
    (om--map-children
     (lambda (items)
       (om--unindent-members index #'trim #'extract items))
     plain-list)))

;;; COMPOSITE BUILDERS

;; misc builders

(om--defun-kw om-build-timestamp-diary (form &key post-blank)
  "Build a diary-sexp timestamp element from FORM.
Optionally set POST-BLANK (a positive integer)."
  (->> (om--build-object 'timestamp post-blank)
       (om--set-property :type 'diary)
       (om--timestamp-diary-set-value form)
       (om--set-properties-nil
        (list :repeater-type :repeater-unit :repeater-value
              :warning-type :warning-unit :warning-value :year-start
              :month-start :day-start :hour-start :minute-start
              :year-end :month-end :day-end :hour-end :minute-end))))

(om--defun-kw om-build-table-row-hline (&key post-blank)
  "Build a table-row element with the 'rule' type.
Optionally set POST-BLANK (a positive integer)."
  (->> (om--build-container-element 'table-row post-blank nil)
       (om--set-property :type 'rule)))

;; shorthand builders

(defun om-build-secondary-string! (string)
  "Build a secondary string (list of object nodes) from STRING.
STRING is any string that contains a textual representation of
object nodes. If this is not true, and error will be thrown."
  (om--verify string stringp)
  (om--build-secondary-string string))

(om--defun-kw om-build-timestamp! (type start &key end
                                               repeater
                                               warning
                                               post-blank)
  "Build a timestamp object.

TYPE is the symbol `active' or `inactive' (the range suffix will be
added if an end time is supplied).

START specifies the start time and is a list of integers in one of
the following forms:
- (YEAR MONTH DAY): short form
- (YEAR MONTH DAY nil nil) short form
- (YEAR MONTH DAY HOUR MINUTE) long form

END (if supplied) will add the ending time, and follows the same
formatting rules as START.

REPEATER and WARNING are lists formatted as (TYPE VALUE UNIT) where
the three members correspond to the :repeater/warning-type, -value,
and -unit properties in `om-build-timestamp'.

Building a diary sexp timestamp is not possible with this function."
  (->> (om--build-object 'timestamp post-blank)
       (om--timestamp-set-start-time-nocheck start)
       (om--timestamp-set-end-time-nocheck end)
       (om--timestamp-set-type type)
       (om--timestamp-set-warning warning)
       (om--timestamp-set-repeater repeater)
       (om--set-property-nil :raw-value)))

(om--defun-kw om-build-clock! (start &key end post-blank)
  "Build a clock object.

START and END follow the same rules as their respective arguments in
`om-build-timestamp!'."
  (let ((ts (om-build-timestamp! 'inactive start :end end)))
    (om-build-clock ts :post-blank post-blank)))

(om--defun-kw om-build-planning! (&key closed deadline
                                              scheduled post-blank)
  "Build a planning element using shorthand arguments.
CLOSED, DEADLINE, and SCHEDULED are lists with the following structure
(brackets denote optional members):

(YEAR MINUTE DAY [HOUR] [MIN]
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

(om--defun-kw om-build-property-drawer! (&key post-blank &rest
                                                     keyvals)
  "Create a property drawer element.

Each member in KEYVALS is a list of symbols like (KEY VAL), where each
list will generate a node property in the property drawer like \":key:
val\"."
  (->> keyvals
       (--map (let ((key (symbol-name (car it)))
                    (val (symbol-name (cadr it))))
                (om-build-node-property key val)))
       (apply #'om-build-property-drawer :post-blank post-blank)))

;; TODO should the properties be kept here? they don't go in a predefined place
(om--defun-kw om-build-headline! (&key (level 1) title-text
                                       todo-keyword tags pre-blank
                                       priority commentedp archivedp
                                       post-blank planning properties
                                       statistics-cookie
                                       section-children
                                       &rest
                                       subheadlines)
  "Build a headline element.

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
  (let* ((planning (-some->>
                    planning
                    (apply #'om-build-planning!)))
         (property-drawer (-some->>
                           properties
                           (apply #'om-build-property-drawer!)))
         (section (-some->>
                   (append `(,planning) `(,property-drawer) section-children)
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
         (om--headline-set-title! title-text statistics-cookie))))

(om--defun-kw om-build-item! (&key post-blank bullet checkbox
                                          tag paragraph counter
                                          &rest nodes)
  "Build an item element.

TAG is a string representing the tag.

PARAGRAPH is a string that will be the initial text in the item.

NODES contains the nodes that will go under this item after
PARAGRAPH.

All other arguments follow the same rules as `om-build-item'."
  (let ((paragraph* (-some->> paragraph (om-build-paragraph!)))
        (tag (-some->> tag (om--build-secondary-string))))
    ;; TODO this restricts all subitems to plain lists...there are
    ;; other things we can put into lists
    (->> (append (list paragraph*) nodes)
         (-non-nil)
         (apply #'om-build-item
                :post-blank post-blank
                :bullet bullet
                :checkbox checkbox
                :counter counter
                :tag tag))))

(om--defun-kw om-build-paragraph! (string &key post-blank)
  "Build a paragraph element.

STRING is the text to be parsed into a paragraph. It must contain valid
formatting (eg, text that will be formatted into objects)."
  ;; TODO this can be simplified?
  (let ((p (->> (om--from-string string)
                (om--get-descendent '(0)))))
    (if (om--is-type-p 'paragraph p)
        (om--set-property-strict :post-blank (or post-blank 0) p)
      (error "String could not be parsed to a paragraph: %s" string))))

(om--defun-kw om-build-table-cell! (string &key post-blank)
  "Build a table-cell node.

STRING is the text to be contained in the table cell. It must contain
valid formatting."
  (-if-let (ss (om--build-secondary-string string))
      (apply #'om-build-table-cell :post-blank post-blank ss)
    (error "Could not create valid secondary string from '%s'" string)))

(om--defun-kw om-build-table-row! (string-list &key post-blank)
  "Build a table-row node.

STRING-LIST is a list of strings to be contained in the table-cells
within the table-row, or it is the symbol `hline' for a rule-typed
table-row. If list of strings, each string follows the same rules as
described in `om-build-table-cell!'."
  (if (eq string-list 'hline)
      (om-build-table-row-hline :post-blank post-blank)
    (->> (-map #'om-build-table-cell! string-list)
         (apply #'om-build-table-row :post-blank post-blank))))

(om--defun-kw om-build-table! (&key tblfm post-blank &rest row-lists)
  "Build a table node.

ROW-LISTS is a list of lists where each member is either a string
to be put in a table cell or the symbol `hline' which represents
a rule-typed table-row.

All other arguments follow the same rules as `om-build-table'."
  (->> (--map (om-build-table-row! it) row-lists)
       (apply #'om-build-table :tblfm tblfm :post-blank post-blank)))

;;; PUBLIC TYPE FUNCTIONS

(om--defun-node om-get-type (node)
  "Return the type of NODE."
  (om--get-type node))

(om--defun-node om-is-type-p (type node)
  "Return t if the type of NODE is TYPE (a symbol)."
  (om--is-type-p type node))

(om--defun-node om-is-any-type-p (types node)
  "Return t if the type of NODE is in TYPES (a list of symbols)."
  (om--is-any-type-p types node))

(om--defun-node om-is-element-p (node)
  "Return t if NODE is an element class."
  (om--is-any-type-p om-elements node))

(om--defun-node om-is-branch-node-p (node)
  "Return t if NODE is a branch node."
  (om--is-branch-node-p node))

(om--defun-node om-node-may-have-child-objects-p (node)
  "Return t if NODE is a branch node that may have child objects."
  (om--is-any-type-p om-branch-nodes-permitting-child-objects node))

(om--defun-node om-node-may-have-child-elements-p (node)
  "Return t if NODE is a branch node that may have child elements.
Note this implies that NODE is also of class element since only
elements may have other elements as children."
  (om--is-any-type-p om-branch-elements-permitting-child-elements node))

;;; PUBLIC PROPERTY FUNCTIONS

;;; polymorphic

(om--defun-node om-contains-point-p (point node)
  "Return t if POINT is within the boundaries of NODE."
  ;; TODO point should be a positive integer only
  (om--verify point integerp)
  (-let (((&plist :begin :end) (om--get-all-properties node)))
    (if (and (integerp begin) (integerp end))
        (<= begin point end)
      (error "Node boundaries are not defined"))))

(om--defun-node om-set-property (prop value node)
  "Set property PROP to VALUE of NODE.

See builder functions for a list of properties and their rules for
each type."
  (om--set-property-strict prop value node))

(om--defun-node om-set-properties (plist node)
  "Set all properties of NODE to the values corresponding to PLIST.
PLIST is a list of property-value pairs that corresponds to the
property list in NODE.

See builder functions for a list of properties and their rules for
each type."
  (om--set-properties-strict plist node))

;; TODO add plural version of this...
(om--defun-node om-get-property (prop node)
  "Return the value or property PROP of NODE.

See builder functions for a list of properties and their rules for
each type."
  (om--get-property-strict prop node))

(om--defun-node* om-map-property (prop fun node)
  "Apply FUN to the value of property PROP of NODE.
FUN is a unary function which takes the current value of PROP and
returns a new value to which PROP will be set.

See builder functions for a list of properties and their rules for
each type."
  (om--map-property-strict prop fun node))

(om--defun-node om-map-properties (plist node)
  "Alter property values of NODE in place.
PLIST is a property list where the keys are properties of NODE and
its values are functions to be mapped to these properties.

See builder functions for a list of properties and their rules for
each type."
  (om--map-properties-strict plist node))

(defmacro om-map-properties* (plist node)
  "Anaphoric form of `om-map-properties'.
PLIST is a property list where the keys are properties of NODE and
its values are forms to be mapped to these properties."
  (om--verify node om--is-node-p)
  `(let ((plist* (om--plist-map-values (lambda (form) `(lambda (it) ,form)) ',plist)))
    (om--map-properties-strict plist* ,node)))

(om--defun-node om-toggle-property (prop node)
  "Flip the value of property PROP of NODE.
This function only applies to properties that are booleans.

The following elements and properties are supported:"
  (let* ((type (om--get-type node))
         (flag (om--get-strict-function :toggle type prop)))
    (if flag
        (om--map-property-strict prop #'not node)
      (error "Not a toggle-able property"))))

(om--defun-node om-shift-property (prop n node)
  "Shift property PROP by N (an integer) units of NODE.
This only applies the properties that are represented as integers.

The following elements and properties are supported:"
  (let* ((type (om--get-type node))
         (fun (om--get-strict-function :shift type prop)))
    (if fun
        (om--map-property-strict* prop (funcall fun n it) node)
      (error "Not a shiftable property"))))

(om--defun-node om-insert-into-property (prop index string node)
  "Insert STRING into PROP at INDEX of NODE if not already there.
This only applies to properties that are represented as lists of
strings.

The following elements and properties are supported:"
  (cl-flet
      ((insert-at-maybe
        (string-list)
        (if (member string string-list) string-list
          (om--insert-at index string string-list))))
    (let* ((type (om--get-type node))
           (flag (om--get-strict-function :string-list type prop)))
      (if flag
          (om--map-property-strict prop #'insert-at-maybe node)
        (error "Property '%s' in node of type '%s' is not a string-list"
               prop type)))))

(om--defun-node om-remove-from-property (prop string node)
  "Remove STRING from PROP of NODE.
This only applies to properties that are represented as lists of 
strings.

See `om-insert-into-property' for a list of supported elements
and properties that may be used with this function."
  (let* ((type (om--get-type node))
         (flag (om--get-strict-function :string-list type prop)))
    (if flag
        (om--map-property-strict* prop (-remove-item string it) node)
      (error "Property '%s' in node of type '%s' is not a string-list"
             prop type))))

(om--defun-node om-plist-put-property (prop key value node)
  "Insert KEY and VALUE pair into PROP of NODE.
KEY is a keyword and VALUE is a symbol. This only applies to 
properties that are represented as plists.

The following elements and properties are supported:."
  (let* ((type (om--get-type node))
         (flag (om--get-strict-function :plist type prop)))
    (if flag
        (om--map-property-strict* prop (plist-put it key value) node)
      (error "Not a plist property"))))

(om--defun-node om-plist-remove-property (prop key node)
  "Remove KEY and its value from PROP of NODE.
KEY is a keyword. This only applies to properties that are
represented as plists.

See `om-plist-put-property' for a list of supported elements
and properties that may be used with this function."
  (let* ((type (om--get-type node))
         (flag (om--get-strict-function :plist type prop)))
    (if flag
        (om--map-property-strict* prop (om--plist-remove key it) node)
      (error "Not a plist property"))))

;;; polymorphic (documentation)

(defun om--append-documentation (fun string)
  (--> (documentation fun)
       (concat it "\n" string)
       (function-put fun 'function-documentation it)))

(defun om--get-type-alist-operation (op)
  (->> om--type-alist
       (--map (cons (car it) (--filter (plist-get (cdr it) op) (cdr it))))
       (-filter #'cdr)))

(defun om--format-alist-operations (ops)
  (->> ops
       (--map (cons (car it) (-map #'car (cdr it))))
       (--map (format "\n%s\n%s"
                      (car it)
                      (s-join "\n" (--map (format "- %S" it) (cdr it)))))
       (s-join "\n")))

(->> (om--get-type-alist-operation :toggle)
     (om--format-alist-operations)
     (om--append-documentation 'om-toggle-property))

(->> (om--get-type-alist-operation :shift)
     (--map (cons (car it) (--remove (eq :post-blank (car it)) (cdr it))))
     (-filter #'cdr)
     (om--format-alist-operations)
     (concat "\nall elements\n- :post-blank\n")
     (om--append-documentation 'om-shift-property))

(->> (om--get-type-alist-operation :string-list)
     (om--format-alist-operations)
     (om--append-documentation 'om-insert-into-property))

(->> (om--get-type-alist-operation :plist)
     (om--format-alist-operations)
     (om--append-documentation 'om-plist-put-property))

;;; objects
;;
;; statistics-cookie

(om--defun-node om-statistics-cookie-is-complete-p (statistics-cookie)
  "Return t is STATISTICS-COOKIE element is complete."
  (om--statistics-cookie-is-complete-p statistics-cookie))

;; timestamp

(om--defun-timestamp om-timestamp-get-start-time (timestamp)
  "Return the time list of TIMESTAMP or start time if a range.
The return value will be a list as specified by the TIME argument in
`om-build-timestamp!'."
  (om--timestamp-get-start-time timestamp))

(om--defun-timestamp om-timestamp-get-end-time (timestamp)
  "Return the end time list of TIMESTAMP end or nil if not a range.
The return value will be a list as specified by the TIME argument in
`om-build-timestamp!'."
  (and (om--timestamp-is-ranged-fast-p timestamp)
       (om--timestamp-get-end-time timestamp)))

(om--defun-timestamp om-timestamp-get-range (timestamp)
  "Return the range of TIMESTAMP in seconds as an integer.
If non-ranged, this function will return 0. If ranged but
the start time is in the future relative to end the time, return
a negative integer."
  (om--timestamp-get-range timestamp))

(om--defun-timestamp om-timestamp-is-active-p (timestamp)
  "Return t if TIMESTAMP is active."
  (or (om--property-is-eq-p :type 'active timestamp)
      (om--property-is-eq-p :type 'active-range timestamp)))

(om--defun-timestamp om-timestamp-is-ranged-p (timestamp)
  "Return t if TIMESTAMP is ranged."
  (or (om--property-is-eq-p :type 'active-range timestamp)
      (om--property-is-eq-p :type 'inactive-range timestamp)))

(om--defun-timestamp om-timestamp-range-contains-p (unixtime timestamp)
  "Return t if UNIXTIME is between start and end of TIMESTAMP node."
  (let ((ut1 (om--timestamp-get-start-unixtime timestamp))
        (ut2 (om--timestamp-get-end-unixtime timestamp)))
    (< ut1 unixtime ut2)))

(om--defun-timestamp om-timestamp-set-start-time (time timestamp)
  "Set start time of TIMESTAMP element to TIME.
TIME is a list analogous to the same argument specified in
`om-build-timestamp!'."
  (om--timestamp-set-start-time time timestamp))

(om--defun-timestamp om-timestamp-set-end-time (time timestamp)
  "Set end time of TIMESTAMP element to TIME.
TIME is a list analogous to the same argument specified in
`om-build-timestamp!'."
  (om--timestamp-set-end-time time timestamp))

(om--defun-timestamp om-timestamp-set-single-time (time timestamp)
  "Set start time of TIMESTAMP to TIME, and remove the end time.
TIME is a list analogous to the same argument specified in
`om-build-timestamp!'."
  (om--timestamp-set-single-time time timestamp))

(om--defun-timestamp om-timestamp-set-double-time (time1 time2 timestamp)
  "Set start and end time of TIMESTAMP to TIME1 and TIME2 respectively.
TIME1 and TIME2 are lists analogous to the TIME argument specified in
`om-build-timestamp!'."
  (om--timestamp-set-double-time time1 time2 timestamp))

(om--defun-timestamp om-timestamp-set-range (range timestamp)
  "Set the RANGE of TIMESTAMP.
If TIMESTAMP is ranged, keep start time the same and adjust the end
time. If not, make a new end time. The units for RANGE are in minutes
if TIMESTAMP is in long format and days if TIMESTAMP is in short
format."
  (om--timestamp-set-range range timestamp))

(om--defun-timestamp om-timestamp-set-type (type timestamp)
  "Set type of TIMESTAMP element to TYPE.
TYPE can be either `active' or `inactive'."
  (om--timestamp-set-type type timestamp))

(om--defun-timestamp om-timestamp-shift (n unit timestamp)
  "Shift TIMESTAMP time by N UNIT's.

This function will move the start and end times together; therefore
ranged inputs will always output ranged timestamps and same for
non-ranged. To move the start and end time independently, use
`om-timestamp-shift-start' or `om-timestamp-shift-end'.

N is a positive or negative integer and UNIT is one of `minute',
`hour', `day', `month', or `year'. Overflows will wrap around
transparently; for instance, supplying `minute' for UNIT and 90 for N
will increase the hour property by 1 and the minute property by 30."
  (om--timestamp-shift-range n unit timestamp))

(om--defun-timestamp om-timestamp-shift-start (n unit timestamp)
  "Shift TIMESTAMP start time by N UNIT's.

N and UNIT behave the same as those in `om-timestamp-shift'.

If TIMESTAMP is not range, the output will be a ranged timestamp with
the shifted start time and the end time as that of TIMESTAMP. If this
behavior is not desired, use `om-timestamp-shift'."
  (om--timestamp-shift-start n unit timestamp))

(om--defun-timestamp om-timestamp-shift-end (n unit timestamp)
  "Shift TIMESTAMP end time by N UNIT's.

N and UNIT behave the same as those in `om-timestamp-shift'.

If TIMESTAMP is not range, the output will be a ranged timestamp with
the shifted end time and the start time as that of TIMESTAMP. If this
behavior is not desired, use `om-timestamp-shift'."
  (om--timestamp-shift-end n unit timestamp))

(om--defun-timestamp om-timestamp-toggle-active (timestamp)
  "Toggle the active/inactive type of TIMESTAMP element."
  (om--timestamp-toggle-active timestamp))

(om--defun-timestamp om-timestamp-truncate (timestamp)
  "Return TIMESTAMP node with start and end times forced to short format."
  (let ((t1 (->> (om--timestamp-get-start-time timestamp)
                    (om--time-truncate)))
        (t2 (->> (om--timestamp-get-end-time timestamp)
                  (om--time-truncate))))
    (om--timestamp-set-double-time t1 t2 timestamp)))

(om--defun-timestamp om-timestamp-truncate-start (timestamp)
  "Return TIMESTAMP node with start time forced to short format."
  (let ((time (->> (om--timestamp-get-start-time timestamp)
                   (om--time-truncate))))
    (om--timestamp-set-start-time time timestamp)))

(om--defun-timestamp om-timestamp-truncate-end (timestamp)
  "Return TIMESTAMP node with end time forced to short format."
  (let ((time (->> (om--timestamp-get-end-time timestamp)
                   (om--time-truncate))))
    (om--timestamp-set-end-time time timestamp)))

(om--defun-timestamp om-timestamp-set-condensation (flag timestamp)
  "Return TIMESTAMP with condensation set to FLAG.

If timestamp is ranged but not outside of one day, it may be condensed
(FLAG is t) to short format like [yyyy-mm-dd xxx hh:mm-hh:mm] or
decondensed (FLAG is nil) to long format like [yyyy-mm-dd xxx
hh:mm]--[yyyy-mm-dd xxx hh:mm]. If these conditions are not met,
return TIMESTAMP untouched regardless of FLAG.

Note: the default for all timestamp functions in `om.el' is to favor 
condensed format."
  (if (and (not (om--timestamp-is-ranged-lowres-p timestamp))
           (om--timestamp-is-ranged-p timestamp))
      (om--timestamp-set-type-ranged (not flag) timestamp)
    timestamp))

(defun om-timestamp-diary-set-value (form timestamp)
  "Set the value of TIMESTAMP node to FORM.
TIMESTAMP must have a type `eq' to `diary'. FORM is a quoted list."
  (unless (and (om--is-type-p 'timestamp timestamp)
               (om--property-is-eq-p :type 'diary timestamp))
    (error "Last argument must be a diary timestamp node"))
  (om--timestamp-diary-set-value form timestamp))

;;; elements
;;
;; clock

(om--defun-node om-clock-is-running-p (clock)
  "Return t if CLOCK element is running (eg is open)."
  (om--property-is-eq-p :status 'running clock))

;; headline

(om--defun-node om-headline-get-statistics-cookie (headline)
  "Return the statistics cookie object from HEADLINE if it exists."
  (om--headline-get-statistics-cookie headline))

(om--defun-node om-headline-is-done-p (headline)
  "Return t if HEADLINE element has a done todo keyword."
  (-> (om--get-property :todo-keyword headline)
      (member org-done-keywords)
      (and t)))

(om--defun-node om-headline-is-archived-p (headline)
  "Return t if HEADLINE element is archived."
  (om--property-is-non-nil-p :archivedp headline))

(om--defun-node om-headline-is-commented-p (headline)
  "Return t if HEADLINE element is commented."
  (om--property-is-non-nil-p :commentedp headline))

(om--defun-node om-headline-has-tag-p (tag headline)
  "Return t if HEADLINE element is tagged with TAG."
  (if (member tag (om--get-property :tags headline)) t))

(om--defun-node om-headline-set-title! (text stats headline)
  (om--headline-set-title! text stats headline))

(om--defun-node om-headline-update-item-statistics (headline)
  "Update the statistics cookie for HEADLINE.
The percent/fraction will be computed as the number of checked items
over the number of items with checkboxes (non-checkbox items will
not be considered)."
  (let* ((items
          (->> (om--headline-get-section headline)
               (om--get-children)
               (--filter (om-is-type-p 'plain-list it))
               (-mapcat #'om--get-children)
               (--remove (om--property-is-nil-p :checkbox it))))
         (done (length (-filter #'om-item-is-checked-p items)))
         (total (length items)))
    (om--headline-set-statistics-cookie-fraction done total headline)))

(om--defun-node om-headline-update-todo-statistics (headline)
  "Update the statistics cookie for HEADLINE.
The percent/fraction will be computed as the number of done
subheadlines over the number of todo subheadlines (eg non-todo
subheadlines will not be counted)."
  (let* ((subtodo (->> (om--headline-get-subheadlines headline)
                       (--filter (om--get-property :todo-keyword it))))
         (done (length (-filter #'om-headline-is-done-p subtodo)))
         (total (length subtodo)))
    (om--headline-set-statistics-cookie-fraction done total headline)))

;; item

(om--defun-node om-item-is-checked-p (item)
  "Return t if ITEM element is checked."
  (om--property-is-eq-p :checkbox 'on item))

(om--defun-node om-item-is-unchecked-p (item)
  "Return t if ITEM element is unchecked."
  (om--property-is-eq-p :checkbox 'off item))

(om--defun-node om-item-is-trans-p (item)
  "Return t if ITEM element is transitional."
  (om--property-is-eq-p :checkbox 'trans item))

(om--defun-node om-item-toggle-checkbox (item)
  "Toggle the checked/unchecked state of ITEM element."
  (om--item-toggle-checkbox item))

;; planning

(om--defun-node om-planning-set-timestamp! (prop planning-list planning)
  "Set the timestamp of PLANNING matching PROP.

PROP is one of `:closed', `:deadline', or `:scheduled'. PLANNING-LIST
is the same as that described in `om-build-planning!'."
  (unless (memq prop '(:closed :deadline :scheduled))
    (error "PROP must be ':closed', ':deadline', or ':scheduled'. Got %S" prop))
  (let ((ts (om--planning-list-to-timestamp planning-list)))
    (om--set-property-strict prop ts planning)))

;;; PUBLIC BRANCH/CHILD FUNCTIONS

;; polymorphic

(om--defun-node om-children-contain-point-p (point branch-node)
  "Return t if POINT is within the boundaries of BRANCH-NODE's children."
  ;; TODO point should be a positive integer only
  (om--verify point integerp)
  (-let (((&plist :contents-begin :contents-end)
          (om--get-all-properties branch-node)))
    (if (and (integerp contents-begin) (integerp contents-end))
        (<= contents-begin point contents-end)
      (error "Node boundaries are not defined"))))

(om--defun-node om-get-children (branch-node)
  "Return the children of BRANCH-NODE as a list."
  (om--get-children branch-node))

(om--defun-node om-set-children (children branch-node)
  "Set the children of BRANCH-NODE to CHILDREN.
CHILDREN is a list of nodes; the types permitted in this list depend
on the type of NODE."
  (let ((type (om--get-type branch-node)))
    (om--set-children-by-type type children branch-node)))

(om--defun-node* om-map-children (fun branch-node)
  "Apply FUN to the children of BRANCH-NODE. 
FUN is a function that takes the current children as a list and
returns a modified children as a list."
  (om--map-children fun branch-node))

(om--defun-node om-is-childless-p (branch-node)
  "Return t if BRANCH-NODE is empty.
This will throw an error if BRANCH-NODE is not a branch type."
  (om--is-childless-p branch-node))

;; (defun om--wrap (type args)
;;   (-> (format "om-build-%s" type) (intern) (apply args)))

;; (defun om-wrap-object (type &rest args)
;;   "Call build function of TYPE to wrap objects contained in ARGS.
;; ARGS is actually a list if keywords and objects that will be
;; passed to the builder function. For example with TYPE of 'bold'
;; and ARGS of ':post-blank 2 \"foo\"', the function `om-build-bold'
;; will be called with keyword argument ':postblank 2' and \"foo\" in
;; the rest args slot."
;;   (if (memq type (append org-element-recursive-objects
;;                          org-element-object-containers))
;;       (om--wrap type args)
;;     (error "Invalid type: %s" type)))

;; (defun om-wrap-element (type &rest args)
;;   (if (memq type org-element-greater-elements)
;;       (om--wrap type args)
;;     (error "Invalid type: %s" type)))

;; ;; TODO is this a meaningful distinction?
;; (defun om-unwrap (obj)
;;   "Remove the children of recursive/container object or greater element OBJ."
;;   (if (om-plain-list-p obj) (list obj)
;;     (let* ((children (om--get-children obj))
;;            (post-blank (om--get-property :post-blank obj))
;;            (first (-drop-last 1 children))
;;            (last* (->> (-last-item children)
;;                        (om-set-post-blank post-blank)
;;                        (list))))
;;       (append first last*))))

;; (defun om-unwrap-deep (types obj)
;;   "Remove the children of all objects of type in TYPES from OBJ.
;; Return a list of objects."
;;   (cond
;;    ((om--is-any-type-p types obj) 
;;     (let* ((children (om--get-children obj))
;;            (post-blank (om--get-property :post-blank obj))
;;            (first (-drop-last 1 children))
;;            (last* (->> (-last-item children)
;;                        (om-set-post-blank post-blank)
;;                        (list))))
;;       (--mapcat (om-unwrap-deep types it) (append first last*))))
;;    ((om--is-plain-text-p obj) (list obj))
;;    (t (list obj))))

;; (defun om-remove-formatting (types node)
;;   "Remove all recursive formatting TYPES from NODE."
;;   (om-match-map* `(:many! (:or ,@types))
;;                 (om-unwrap-deep types it) node))

;; (defun om-remove-all-formatting (node)
;;   "Remove all recursive formatting from NODE."
;;   (om-remove-formatting org-element-all-objects node))

;; elements
;;
;; headline

(om--defun-node om-headline-get-subheadlines (headline)
  "Return list of subheadline nodes for HEADLINE node or nil if none."
  (om--headline-get-subheadlines headline))

(om--defun-node om-headline-get-section (headline)
  "Return section node for headline HEADLINE node or nil if none."
  (om--headline-get-section headline))

(om--defun-node om-headline-get-properties-drawer (headline)
  "Return the properties drawer node in HEADLINE.
If multiple are present (there shouldn't be) the first will be 
returned."
  (om--headline-get-properties-drawer headline))

(om--defun-node om-headline-get-node-properties (headline)
  "Return a list of node-properties nodes in HEADLINE or nil if none."
  (om--headline-get-node-properties headline))

(om--defun-node om-headline-get-planning (headline)
  "Return the planning node in HEADLINE or nil if none."
  (om--headline-get-planning headline))

(om--defun-node om-headline-get-path (headline)
  "Return tree path of HEADLINE node.
The return value is a list of headline titles (including that from
HEADLINE) leading to the root node."
  (cl-labels
      ((get-path
        (hl)
        (let ((title (om--get-property :raw-value hl)))
          (-if-let (parent (om--get-parent-headline hl))
              (cons title (get-path parent))
            (list title)))))
    (reverse (get-path headline))))


;; plain-list

;; TODO there seems to be a bug in the org-interpeter that prevents
;; "+" bullets from being recognized (as of org-9.1.9 they are simply
;; read as "-")
(om--defun-node om-plain-list-set-type (type plain-list)
  "Set the type of PLAIN-LIST greater element to TYPE.
TYPE is one of the symbols `unordered' or `ordered'."
  (cond
   ((eq type 'unordered)
    (om--map-children*
      (--map (om--set-property-strict :bullet '- it) it) plain-list))
   ((eq type 'ordered)
    ;; NOTE the org-interpreter seems to use the correct, ordered
    ;; numbers if any number is set here. This behavior may not be
    ;; reliable.
    (om--map-children*
      (--map (om--set-property-strict :bullet 1 it) it) plain-list))
   (t (error "Invalid type: %s" type))))

;; table

(om--defun-node om-table-get-cell (row-index column-index table)
  "Return table-cell node at ROW-INDEX and COLUMN-INDEX in TABLE node.
Rule-type rows do not count toward row indices."
  (om--table-get-cell row-index column-index table))

(om--defun-node om-table-delete-row (row-index table)
  "Delete the row at ROW-INDEX in TABLE node."
  (om--table-delete-row row-index table))

(om--defun-node om-table-delete-column (column-index table)
  "Delete the column at COLUMN-INDEX in TABLE node."
  (om--table-delete-column column-index table))

(om--defun-node om-table-insert-column! (column-index column-text table)
  "Insert COLUMN-TEXT at COLUMN-INDEX in TABLE node.

COLUMN-INDEX is the index of the column and COLUMN-TEXT is a list of
strings to be made into table-cells to be inserted following the same
syntax as `om-build-table-cell!'."
  (let ((column (-map #'om-build-table-cell! column-text)))
    (om--table-insert-column column-index column table)))

(om--defun-node om-table-insert-row! (row-index row-text table)
  "Insert ROW-TEXT at ROW-INDEX in TABLE node.

ROW-INDEX is the index of the column and ROW-TEXT is a list of strings
to be made into table-cells to be inserted following the same syntax
as `om-build-table-row!'."
  (if (not row-text) (om--table-clear-row row-index table)
    (let ((row (om-build-table-row! row-text)))
      (om--table-insert-row row-index row table))))

(om--defun-node om-table-replace-cell! (row-index column-index
                                                  cell-text table)
  "Replace a table-cell node in TABLE node with CELL-TEXT.

If CELL-TEXT is a string, it will replace the children of the
table-cell at ROW-INDEX and COLUMN-INDEX. CELL-TEXT will be processed 
the same as the argument given to `om-build-table-cell!'.

If CELL-TEXT is nil, it will set the cell to an empty string."
  (let ((cell (if cell-text (om-build-table-cell! cell-text)
                (om-build-table-cell ""))))
    (om--table-replace-cell row-index column-index cell table)))

(om--defun-node om-table-replace-column! (column-index column-text table)
  "Replace column at COLUMN-INDEX in TABLE node with COLUMN-TEXT.

If COLUMN-TEXT is a list of strings, it will replace the table-cells
at COLUMN-INDEX. Each member of COLUMN-TEXT will be processed the
same as the argument given to `om-build-table-cell!'.

If COLUMN-TEXT is nil, it will clear all cells at COLUMN-INDEX."
  (if (not column-text) (om--table-clear-column column-index table)
    (let ((column-cells (-map #'om-build-table-cell! column-text)))
      (om--table-replace-column column-index column-cells table))))

(om--defun-node om-table-replace-row! (row-index row-text table)
  "Replace row at ROW-INDEX in TABLE node with ROW-TEXT.

If ROW-TEXT is a list of strings, it will replace the cells at
ROW-INDEX. Each member of ROW-TEXT will be processed the same as
the argument given to `om-build-table-row!'.

If ROW-TEXT is nil, it will clear all cells at ROW-INDEX."
  (let ((row-cells (om-build-table-row! row-text)))
    (om--table-replace-row row-index row-cells table)))

;; PUBLIC INDENTATION FUNCTIONS

;; headline

(om--defun-node om-headline-indent-subtree (index headline)
  "Indent the subheadline and its children at INDEX within HEADLINE."
  (om--headline-indent-subtree index headline))

(om--defun-node om-headline-indent-subheadline (index headline)
  "Indent the subheadline without moving its children at INDEX within HEADLINE."
  (om--headline-indent-subheadline index headline))

(om--defun-node om-headline-unindent-subheadline (index child-index headline)
  "Unindent subheadline at CHILD-INDEX in the subheadline at INDEX in HEADLINE.
This will not move the children under the headline at CHILD-INDEX."
  (om--headline-unindent-subheadline index child-index headline))

(om--defun-node om-headline-unindent-subtree (index headline)
  "Unindent all subheadlines under the subheadline at INDEX in HEADLINE."
  (om--headline-unindent-subtree index headline))

;; plain-list

(om--defun-node om-plain-list-indent-item-tree (index plain-list)
  "Indent the subitem at INDEX in PLAIN-LIST and move items below it."
  (om--plain-list-indent-item-tree index plain-list))

(om--defun-node om-plain-list-indent-item (index plain-list)
  "Indent the subitem at INDEX in PLAIN-LIST without moving items below it."
  (om--plain-list-indent-item index plain-list))

(om--defun-node om-plain-list-unindent-item (index child-index plain-list)
  "Unindent subitem at CHILD-INDEX in the subitem at INDEX in PLAIN-LIST.
This will not move the children under the item at CHILD-INDEX."
  (om--plain-list-unindent-item index child-index plain-list))

(om--defun-node om-plain-list-unindent-items (index plain-list)
  "Unindent all items under the item at INDEX in PLAIN-LIST."
  (om--plain-list-unindent-items index plain-list))

;;; printing functions

(defun om--set-blank-children (node)
  "Set the children of NODE to a blank string (\"\")."
  (om--set-children '("") node))

(defconst om--rm-if-empty
  '(table plain-list bold italic radio-target strike-through
          superscript subscript table-cell underline)
  "Elements/objects that will be blank if printed and empty.")

(defconst om--blank-if-empty
  '(center-block drawer dynamic-block property-drawer quote-block
                 special-block verse-block)
  "Elements that require children of \"\" to correctly print empty.
This is a workaround for a bug.")

(defun om--filter-non-zero-length (node)
  (unless (and (om--is-childless-p node)
               (or (om--is-any-type-p om--rm-if-empty node)
                   (and (om--is-type-p 'table-row node)
                        (om--property-is-eq-p :type 'standard node))))
    node))

(defun om--clean (node)
  (->> node
       (om--map-children* (-non-nil (-map #'om--clean it)))
       (om--filter-non-zero-length)))

(defun om--blank (node)
  (if (om--is-childless-p node)
      (if (om--is-any-type-p om--blank-if-empty node)
          (om--set-blank-children node)
        node)
    (om--map-children* (-map #'om--blank it) node)))

(defun om-to-string (node)
  "Return NODE as an interpreted string without text properties."
  ;; TODO verify node (or nil)
  (->> node
       ;; some objects and greater elements should be removed if blank
       ;; table and plain list will error, and the others make no
       ;; sense if they are empty. This is an org mode bug, they
       ;; should not be printed by the interpreter by default
       (om--clean)
       ;; some greater elements will print "nil" in their children if
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

(defmacro om--modify-children (node form)
  "Recursively modify the children of NODE using FORM.
FORM is a form that returns a list of elements or objects as the
new children, and the variable 'it' is available to represent the
original children to be modified."
  (declare (indent 1))
  `(cl-labels
       ((rec
         (node)
         (let ((type (om--get-type node)))
           (if (eq type 'plain-text) node
             (->>
              (om--get-children node)
              (funcall (lambda (it) ,form))
              (--map (rec it))
              (om--construct type (nth 1 node)))))))
     (rec ,node)))

(defun om--match-filter (pattern children)
  (pcase pattern
    ;; quote (may be accidentally in pattern
    (`(quote . ,_)
     (error "'quote' not allowed in pattern"))

    ;; function (may be accidentally in pattern
    (`(function . ,_)
     (error "'function' not allowed in pattern"))
    
    ;; index
    ((and (pred integerp) index)
     (-some->
      (if (< index 0)
          (nth (- (* -1 index) 1) (nreverse children))
        (nth index children))
      (list)))

    ;; type
    ((and (pred (lambda (y) (memq y om-nodes))) type)
     (--filter (om--is-type-p type it) children))

    ;; relative index
    (`(,(and (or '< '<= '> '>=) f)
       ,(and (pred integerp) i))
     ;; TODO what if they give a negative index?
     (-when-let (i* (om--convert-intra-index i children t))
       (->> children
            (--map-indexed (when (funcall f it-index i*) it))
            (-non-nil))))

    ;; predicate
    ;; ((and (pred functionp) fun)
    (`(:pred . (,p . nil))
     (--filter (funcall p it) children))

    ;; not
    (`(:not . (,p . nil))
     (->> (om--match-filter p children)
          (-difference children)))

    ;; or
    (`(:or . ,(and (pred and) p))
     (->> (--mapcat (om--match-filter it children) p)
          (-distinct)))

    ;; and
    (`(:and . ,(and (pred and) p))
     (->> (--map (om--match-filter it children) p)
          (-reduce #'-intersection)))

    ;; properties
    ;; NOTE: this must go last if we don't want :and/:or/:not to
    ;; be interpreted as a plist
    ((and (pred om--is-plist-p) plist)
     (cl-flet
         ((all-props-match?
           (node props)
           (->> (-partition 2 (om--get-all-properties node))
                (-difference (-partition 2 props))
                (not))))
       (--filter (all-props-match? it plist) children)))
    (_ (error "Invalid pattern: %s" pattern))))

;; TODO this is inefficient
(defun om--match-pattern (reverse? count pattern node)
  (let ((children (--> (om--get-children node)
                       (if reverse? (reverse it) it))))
    (pcase pattern
      (`(,(and p (guard (memq p '(:first :last :nth :slice)))) . ,_)
       (error "Slicer detected: %s" p))
      (`(:many! . (,p . nil))
       (let ((found (om--match-filter p children)))
         (->> (-difference children found)
              (--mapcat (om--match-pattern reverse? count `(:many! ,p) it))
              (append found))))
      (`(:many! . ,_)
       (error "Query with :many! must have one target"))
      (`(:many . (,p . nil))
       (let ((found (om--match-filter p children))
             (p* (list :many p)))
         (->> children
              (--mapcat (om--match-pattern reverse? count p* it))
              (append found))))
      (`(:many . ,_)
       (error "Query with :many must have one target"))
      (`(:any . ,(and (pred and) ps))
       (--mapcat (om--match-pattern reverse? count ps it) children))
      (`(:any . nil)
       children)
      (`(,p . nil)
       (om--match-filter p children))
      (`(,p . ,ps)
       (->> (om--match-filter p children)
            (--mapcat (om--match-pattern reverse? count ps it))))
      (_ (error "Invalid query")))))

;; TODO this is inefficient
(defun om--match-slicer (pattern node)
  (pcase pattern
    (`(:first . ,ps)
     (-take 1 (om--match-pattern nil 1 ps node)))
    (`(:last . ,ps)
     (-take 1 (om--match-pattern t 1 ps node)))
    (`(:nth . (,(and (pred integerp) n) . ,ps))
     (if (< n 0) (reverse (om--match-pattern t (abs n) ps node))
       (list (nth n (om--match-pattern nil (1+ n) ps node)))))
    (`(:sub
       . (,(and (pred integerp) a)
          . (,(and (pred integerp) b)
             . ,ps)))
     (let* ((sum (+ a b))
            (asum (abs sum)))
       (cond
        ((or (< asum a) (< asum b))
         (error "Both indices must be on the same side of zero"))
        ((< (abs b) (abs a))
         (error "Second index must be further from zero than first"))
        (t
         (let ((a* (if (< a 0) (1- (abs a)) a))
               (b* (if (< b 0) (abs b) (1+ b) )))
           (->>
            (if (<= 0 sum) (om--match-pattern nil b* ps node)
              (reverse (om--match-pattern t b* ps node)))
            (-take b*)
            (-drop a*)))))))
    (_ (om--match-pattern nil nil pattern node))))

;; match

(om--defun-node om-match (pattern node)
  "Return a list of all nodes matching PATTERN in NODE.

PATTERN is a list of form ([SLICER [ARG1] [ARG2]] COND1 [COND2 ...]).

SLICER is an optional prefix to the pattern describing how many
and which matches to return. If not given, all matches are
returned. Possible values are:

- `:first' - return the first match
- `:last' - return the last match
- `:nth' ARG1 - return the nth match where ARG1 is an integer denoting
  the index to return (starting at 0). It may be a negative number
  to start counting at the end of the match list, in which case -1 is
  the last index
- `:sub' ARG1 ARG2 - return a sublist between indices ARG1 and ARG2.
  ARG1 and ARG2 follow the same rules as `:nth'

CONDX denotes conditions that that match nodes in the parse
tree. This first condition will select matches within the
children of NODE, the next condition will select matches within
the matches from the first condition, and so on. The types of
conditions are:

- PRED - match when PRED evaluates to t; PRED is a unary function that
  takes the current node as its argument
- TYPE - match when the node's type is `eq' to TYPE (a symbol)
- INDEX - match when the node's index is `=' to INDEX (an integer).
  The first index is zero. If INDEX is negative, start counting
  backward from the end of children where -1 is the last node
- (OP INDEX) - match when (OP NODE-INDEX INDEX) returns t. OP is
  one of `<', `>', `<=', or `>=' and NODE-INDEX is the index of the
  node being evaluated. If INDEX is negative, count from the last
  node and evaluate OP.
- PLIST - match nodes with the same properties and values as PLIST
- `:many' - match zero or more levels, must have at least one
  sub-pattern after it
- `:many!' - like `:many' but do not match within other matches
- `:any' - always match exactly one node

Additionally, conditions may be further refined using boolean forms:

- (:and C1 C2 [C3 ...]) - match when all conditions are true
- (:or C1 C2 [C3 ...]) - match when at least one condition is true
- (:not C) - match when condition is not true

The CX members in the forms above are one of any of the condition
types except `:many', `:many!', and `:any'. Boolean forms may be
nested within each other."
  (om--match-slicer pattern node))

;; find-parent

;; (defun om-match-parent-query (parent query)
;;   (pcase query
;;     ;; type
;;     ((and (pred symbolp) type)
;;      ;; TODO check for valid type?
;;      (and (om--is-type-p type parent) parent))
;;     ;; compound (or)
;;     ;; (`(:or . ,(and (pred and) q)))
;;     ;; compound (and)
;;     ;; (`(:and . ,(and (pred and) q)))
;;     ;; properties (must go after compound)
;;     ((pred om--is-plist-p)
;;      (cl-flet
;;          ((all-props-match?
;;            (node props)
;;            (->> (-slice props 0 nil 2)
;;                 (--map (equal (plist-get props it)
;;                               (om--get-property it node)))
;;                 (-none? #'null))))
;;        (and (all-props-match? parent query) parent)))
;;     (_ (error "Invalid query: %s" query))))

;; (defun om-match-parent (node &rest queries)
;;   ;; TODO validate node (should be any valid element or object)
;;   (unless node (error "No element given"))
;;   ;; the non-nil is required for cases where we may get
;;   ;; a nil for queries instead of no argument
;;   (let ((queries (-non-nil queries)))
;;     (when queries
;;       (let ((parent (om--get-parent node)))
;;         (pcase queries
;;           (`(:many . (,q . nil))
;;            (or (om-match-parent-query parent q)
;;                (om-match-parent parent q)))
;;           (`(:many . ,_)
;;            (error "Query with :many must have one target"))
;;           (`(:any . ,(and (pred and) qs))
;;            (om-match-parent parent q))
;;           (`(:any . nil)
;;            (error "Query with :any must have at least one target"))
;;           (`(,q . nil)
;;            (om-match-parent-query parent q))
;;           (`(,q . ,qs)
;;            (-> (om-match-parent-query parent q)
;;                (om-match-parent qs)))
;;           (_ (error "Invalid query")))))))

;; delete

(defun om--delete-targets (node targets)
  "Delete TARGETS in the children of NODE."
  (om--modify-children node
    (--remove (member it targets) it)))

(defun om-match-delete (pattern node)
  "Remove nodes matching PATTERN from NODE and return modified NODE.

PATTERN follows the same rules as `om-match'."
  (-if-let (targets (om-match pattern node))
      (om--delete-targets node targets)
    node))

;; extract

(defun om-match-extract (pattern node)
  "Remove nodes matching PATTERN from NODE.
Return cons cell where the car is a list of all removed nodes and
the cdr is the modified NODE.

PATTERN follows the same rules as `om-match'."
  (-if-let (targets (om-match pattern node))
      (cons targets (om--delete-targets node targets))
    node))

;; map

(om--defun* om-match-map (pattern fun node)
  "Apply FUN to nodes matching PATTERN in NODE.
FUN is a unary function that takes a node and returns a new node
which will replace the original.

PATTERN follows the same rules as `om-match'."
  (-if-let (targets (om-match pattern node))
      (om--modify-children node
        (--map-when (member it targets) (funcall fun it) it))
    node))

;; mapcat

(om--defun* om-match-mapcat (pattern fun node)
  "Apply FUN to nodes matching PATTERN in NODE.
FUN is a unary function that takes a node and returns a list of new
nodes which will be spliced in place of the original node.

PATTERN follows the same rules as `om-match'."
  (-if-let (targets (om-match pattern node))
      (om--modify-children node
        (--mapcat (if (member it targets)
                      (funcall fun it) (list it))
                  it))
    node))

;; replace

(defun om-match-replace (pattern node* node)
  "Replace nodes matching PATTERN with NODE* within NODE.
Return modified NODE.

PATTERN follows the same rules as `om-match'."
  (declare (indent 1))
  (-if-let (targets (om-match pattern node))
      (om--modify-children node
        (--map-when (member it targets) node* it))
    node))

;; insert-before

(defun om-match-insert-before (pattern node* node)
  "Insert NODE* before every node matching PATTERN in NODE.
Return modified NODE.

PATTERN follows the same rules as `om-match'."
  (declare (indent 1))
  (-if-let (targets (om-match pattern node))
      (om--modify-children node
        (--mapcat (if (member it targets) (list node* it) (list it)) it))
    node))

;; insert-after

(defun om-match-insert-after (pattern node* node)
  "Insert NODE* after every node matching PATTERN in NODE.
Return modified NODE.

PATTERN follows the same rules as `om-match'."
  (declare (indent 1))
  (-if-let (targets (om-match pattern node))
      (om--modify-children node
        (--mapcat (if (member it targets) (list it node*) (list it)) it))
    node))

;; insert-within

(defun om--normalize-insert-index (index list)
  "Return a positive integer from INDEX relative to front of LIST.
INDEX represents the position in between members of LIST where
something may be inserted or a split may occur. If INDEX is positive,
do nothing. If negative, '-1' is assumed to represent the position
behind the last member of LIST and decreasing integers move toward the
front."
  (if (<= 0 index) index (+ (length list) index 1)))

(defun om--insert-in (node node* index)
  "Insert NODE* into the children of NODE at INDEX."
  (let* ((children (om--get-children node)))
      (om--construct
       (nth 0 node)
       (nth 1 node)
       ;; TODO should we throw an error here?
       (om--insert-at index node* children))))

(defun om-match-insert-within (pattern index node* node)
  "Insert new NODE* at INDEX into nodes matching PATTERN in NODE.
Return modified NODE.

PATTERN follows the same rules as `om-match' with the exception
that PATTERN may be nil. In this case NODE* will be inserted at INDEX
in the immediate, top level children of NODE."
  (declare (indent 2))
  (if (-non-nil pattern)
      (-if-let (targets (om-match pattern node))
          (om--modify-children node
            (if (not (member node targets)) it
              (om--insert-in it node* index)))
        node)
    (om--insert-in node node* index)))

;; splice

(defun om-match-splice (pattern nodes* node)
  "Splice nodes matching PATTERN in NODE with NODES*.
Return modified NODE. NODES* is a list of nodes.

PATTERN follows the same rules as `om-match'."
  (declare (indent 1))
  (-if-let (targets (om-match pattern node))
      (om--modify-children node
        (--mapcat (if (member it targets) nodes* (list it)) it))
    node))

;; splice-before

(defun om-match-splice-before (pattern nodes* node)
  "Splice NODES* before every nodes matching PATTERN in NODE.
Return modified NODE. NODES* is a list of nodes.

PATTERN follows the same rules as `om-match'."
  (declare (indent 1))
  (-if-let (targets (om-match pattern node))
      (om--modify-children node
        (--mapcat (if (member it targets)
                      (append nodes* (list it))
                    (list it))
                  it))
    node))

;; splice-after

(defun om-match-splice-after (pattern nodes* node)
  "Splice NODES* after every nodes matching PATTERN in NODE.
Return modified NODE. NODES* is a list of nodes.

PATTERN follows the same rules as `om-match'."
  (declare (indent 1))
  (-if-let (targets (om-match pattern node))
      (om--modify-children node
        (--mapcat (if (member it targets) (cons it nodes*) (list it)) it))
    node))

;; splice-within

(defun om--splice-at (node nodes* index)
  "Splice NODES* into the children of NODE at INDEX."
  (let* ((children (om--get-children node))
         (i (om--normalize-insert-index index children)))
    (om--construct
     (nth 0 node)
     (nth 1 node)
     ;; TODO make negative index version of this
     (->> (-split-at i children)
          (-insert-at 1 nodes*)
          (apply #'append)))))

(defun om-match-splice-within (pattern index nodes* node)
  "Splice new NODES* at INDEX into nodes matching PATTERN in NODE.
Return modified NODE. NODES* is a list of nodes.

PATTERN follows the same rules as `om-match' with the exception
that PATTERN may be nil. In this case NODES* will be inserted at INDEX
in the immediate, top level children of NODE."
  (declare (indent 2))
  (if (-non-nil pattern)
      (-if-let (targets (om-match pattern node))
          (om--modify-children node
            (if (not (member node targets)) it
              (om--splice-at it nodes* index)))
        node)
    (om--splice-at node nodes* index)))

;; misc

(defun om-clean (node)
  "Recursively remove all empty elements from NODE.
Has no effect on 'plain-text' elements."
  (cl-labels
      ((clean-rec
        (node)
        (let ((type (om--get-type node)))
          (if (eq type 'plain-text) node
            (->> (om--get-children node)
                 (--remove
                  (and (om--is-childless-p it)
                       (om--is-any-type-p '(section plain-list) it)))
                 (--map (clean-rec it))
                 (append (list type (nth 1 node))))))))
    (clean-rec node)))

;; side-effects

(defun om-match-do (pattern fun node)
  "Like `om-match-map' but for side effects only.
FUN is function that side effects and takes on argument, the matches
from NODE using PATTERN. This function itself returns nil.

PATTERN follows the same rules as `om-match'."
  (-when-let (targets (om-match pattern node))
      (--each targets (funcall fun it))))


;;; BUFFER PARSING

;; parse at specific point

(defun om-parse-object-at (point)
  "Return the object tree under POINT or nil if not on an object.

If TYPE is supplied, only return nil if the object under point is
not of that type. TYPE is a symbol from `om-objects'."
  (save-excursion
    (goto-char point)
    (let* ((context (org-element-context))
           (type (om--get-type context))
           (offset (cond
                    ((memq type '(superscript subscript)) -1)
                    ((eq type 'table-cell) -1)
                    (t 0)))
           (nesting (cond
                     ((memq type '(superscript subscript)) '(0 1))
                     ((eq type 'table-cell) '(0 0 0))
                     (t '(0 0)))))
      (-let* (((&plist :begin :end) (om--get-all-properties context))
              (tree (org-element--parse-elements (+ begin offset) end 'first-section
                                                 nil nil nil nil)))
        (--> (car tree)
             (om--get-descendent nesting it)
             (om--filter-types org-element-all-objects it)
             (if type (om--filter-type type it) it))))))

(defun om--parse-element-at (point &optional type)
  "Return element immediately under POINT.
For a list of all possible return types refer to
`org-element-all-elements'; this will return everything in this list
except 'section' which is ambiguous when referring to a single point.
(see `om-parse-section-at').

If TYPE is supplied, only return nil if the object under point is
not of that type. TYPE is a symbol from `org-element-all-elements'.
Furthermore, setting TYPE to 'table-row' will prefer table-row
elements over table elements and likewise when setting TYPE to 'item'
for plain-list elements vs item elements."
  (save-excursion
    (goto-char point)
    (let*
        ((node (org-element-at-point))
         (elem-type (om--get-type node)))
      (if (not
           (memq elem-type (append org-element-greater-elements
                                   org-element-object-containers)))
          node
        (-let* (((&plist :begin :end) (om--get-all-properties node))
                (tree (car (org-element--parse-elements
                            begin end 'first-section nil nil nil nil)))
                (nesting (cl-case elem-type
                           (headline nil)
                           (table (if (eq type 'table-row) '(0 0) '(0)))
                           (plain-list (if (eq type 'item) '(0 0) '(0)))
                           (t '(0)))))
          (--> (om--get-descendent nesting tree)
               (if type (om--filter-type type it) it)))))))

(defun om-parse-element-at (point)
  "Return element under POINT or nil if not on an element.

This function will return every element available in `om-elements'
with the exception of `section', `item', and `table-row'. To
specifically parse these, use the functions `om-parse-section-at',
`om-parse-item-at', and `om-parse-table-row-at'."
  (om--parse-element-at point))

(defun om-parse-table-row-at (point)
  "Return table-row element under POINT or nil if not on a table-row."
  (save-excursion
    (goto-char point)
    (beginning-of-line)
    (om--parse-element-at (point) 'table-row)))

(defun om-parse-item-at (point)
  "Return item element under POINT or nil if not on an item.
This will return the item even if POINT is not at the beginning of
the line."
  (save-excursion
    (goto-char point)
    (beginning-of-line)
    (om--parse-element-at (point) 'item)))

(defun om--parse-headline-subtree-at (point subtree)
  (save-excursion
    (goto-char point)
    (when (ignore-errors (org-back-to-heading))
      (let ((b (point))
            (e (if subtree (org-end-of-subtree)
                 (or (outline-next-heading) (point-max)))))
        (car (org-element--parse-elements b e 'first-section
                                          nil nil nil nil))))))

(defun om-parse-headline-at (point)
  "Return headline tree under POINT or nil if not on a headline.
POINT does not need to be on the headline itself. Only the headline
and its section will be returned. To include subheadlines, use
`om-parse-subtree-at'."
  (om--parse-headline-subtree-at point nil))

(defun om-parse-subtree-at (point)
  "Return headline tree under POINT or nil if not on a headline.
POINT does not need to be on the headline itself. Unlike
`om-parse-headline-at', the returned tree will include
subheadlines."
  (om--parse-headline-subtree-at point t))

(defun om-parse-section-at (point)
  "Return tree of the section under POINT or nil if not on a section.
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
           ;; TODO this is redundant
           (om--parse-headline-subtree-at point nil))
       (error
        (progn
          (org-element--parse-elements
           (point-min) (or (outline-next-heading) (point-max))
           'first-section nil nil nil nil)))))))

;; parse at current point

(-> '(object element table-row item headline subtree section)
    (--each
        (let* ((name (intern (format "om-parse-this-%s" it)))
               (call (intern (format "om-parse-%s-at" it)))
               (doc (format "Call `%s' with the current point." call))
               (body `(,call (point))))
          (eval `(defun ,name () ,doc ,body)))))

(defun om-parse-this-buffer ()
  "Return org-data document tree for the current buffer.
Contrary to the org-element specification, the org-data element
returned from this function will have :begin and :end properties."
  (let* ((c (om--get-children (org-element-parse-buffer)))
         (b (if c (om--get-property :begin (-first-item c)) 1))
         (e (if c (om--get-property :end (-last-item c)) 1)))
    (om--construct 'org-data `(:begin ,b :end ,e) c)))

;;; BUFFER SIDE EFFECTS

;; insert

(om--defun-node om-insert (point node)
  "Convert NODE to a string and insert at POINT in the current buffer.
Return NODE."
  (om--verify point integerp)
  (save-excursion
    (goto-char point)
    (insert (om-to-string node)))
  node)

(om--defun-node om-insert-tail (point node)
  "Like `om-insert' but insert NODE at POINT and move to end of insertion."
  (om--verify point integerp)
  (let ((s (om-to-string node)))
    (save-excursion
      (goto-char point)
      (insert s))
    (goto-char (+ point (length s))))
  node)

;; update

(defun om--apply-overlays (os)
  (cl-flet
      ((apply-overlays
        (o)
        (let* ((beg (plist-get o :start))
               (end (plist-get o :end))
               (props (plist-get o :props))
               (o* (make-overlay beg end)))
          (--each (-partition 2 props) (apply #'overlay-put o* it)))))
    (-each os #'apply-overlays)))

(om--defun-node* om-update (fun node)
  "Replace NODE in the current buffer with a new one. 
FUN is a function that takes NODE as its only argument and returns a
modified NODE. This modified element is then written in place of the
old element in the current buffer."
  ;; if node is of type 'org-data' it will have no props
  (let* ((begin (or (om--get-property :begin node) (point-min)))
         (end (or (om--get-property :end node) (point-max)))
         ;; get the outline overlays that make text invisible
         (ov-cmd (->>
                  (overlays-in begin end)
                  (--filter (eq 'outline (overlay-get it 'invisible)))
                  (--map (list :start (overlay-start it)
                               :end (overlay-end it)
                               :props (overlay-properties it)))
                  (list 'apply 'om--apply-overlays))))
    ;; hacky way to add overlays to undo tree
    (setq-local buffer-undo-list (cons ov-cmd buffer-undo-list))
    (delete-region begin end)
    (->> (funcall fun node) (om-insert begin))
    nil))

;; generate all update functions for corresponding parse functions
;; since all take function args, also generate anaphoric forms
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
             ,update-this-body))))

(om--defun* om-update-this-buffer (fun)
  "Apply FUN to the contents of the current buffer.
FUN is a unary function that takes a node of type 'org-data' and
returns a modified node."
  (om-update fun (om-parse-this-buffer)))

;; fold

;; TODO this will fold items improperly
(defun om--flag-elem-contents (flag node)
  (om--verify flag booleanp
                   node om--is-node-p)
  (-let (((&plist :contents-begin :contents-end)
          (om--get-all-properties node)))
    (outline-flag-region (1- contents-begin) (1- contents-end) flag)))

(om--defun-node om-fold (node)
  "Fold the children of NODE if they exist."
  (om--flag-elem-contents t node))

(om--defun-node om-unfold (node)
  "Unfold the children of NODE if they exist."
  (om--flag-elem-contents nil node))

;;; MISC FUNCTIONS

(defun om-now ()
  "Return list representing the current time without hours and minutes.
This is meant to be used as input for functions such as
`om-build-timestamp'."
  (->> (decode-time) (-select-by-indices '(3 4 5)) (reverse)))

(defun om-now-long ()
  "Return list representing the current time with hours and minutes.
This is meant to be used as input for functions such as
`om-build-timestamp'."
  (->> (decode-time) (-select-by-indices '(1 2 3 4 5)) (reverse)))

(provide 'om)
;;; om.el ends here

