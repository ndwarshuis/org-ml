;;; om-elem.el --- Org Mode Functional API (elements) -*- lexical-binding: t; -*-

;; Copyright (C) 2019  Nathan Dwarshuis

;; Author: Nathan Dwarshuis <natedwarshuis@gmail.com>
;; Keywords: org-mode, outlines
;; Homepage: https://github.com/ndwarshuis/org-x
;; Package-Requires: ((emacs "26.1") (dash "2.15"))
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

;;; Code:

;; (require 'cl-lib)
(require 'org)
(require 'dash)
(require 's)

;;; BETTER CL-DEFUN
;; some functions here require a clean way to use &rest and &key
;; at the same time, which `cl-defun' does not do...let's roll our own

(defun om-elem--symbol-to-keyword (symbol)
  "Convert SYMBOL to keyword if not already."
  (if (keywordp symbol) symbol
    (->> (symbol-name symbol)
         (s-prepend ":")
         (intern))))

(defun om-elem--verify-pos-args (pos-args)
  (if (-all? #'symbolp pos-args) pos-args
    (error "All positional arguments must be symbols")))

(defun om-elem--verify-rest-arg (resarg)
  (if (and (>= 1 (length resarg)) (symbolp (car resarg)))
      (car resarg)
    (error "Rest argument must only have one member")))
 
(defun om-elem--make-optarg-let (optarg index)
  (cl-flet
      ((make-plist
        (arg init)
        (let* ((opt-get `(nth ,index --opt-args))
               (val (if init `(or ,opt-get ,init) opt-get)))
          `(,arg ,val))))
    (pcase optarg
      (`(,arg ,init) (make-plist arg init))
      ((and (pred symbolp) arg) (make-plist arg nil))
      (_ (error "Invalid optional argument: %s" optarg)))))

(defun om-elem--make-kwarg-let (kwarg)
  "For each in KWARGS, return a plist."
  (cl-flet
      ((make-plist
        (arg kw init)
        (when (and kw (not (keywordp kw)))
          (error "Must use keyword for kw-arg, not %s" kw))
        (let* ((kw (or kw (om-elem--symbol-to-keyword arg)))
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

(defun om-elem--partition-rest-args (args opt-len kws use-rest?)
  (if (= opt-len (length args)) (list args nil nil)
    (-let [(optargs restargs)
           (if (= 0 opt-len) (list nil args) (-split-at opt-len args))]
      (if (not kws)
          (if use-rest? (list optargs nil restargs)
            (error "Too many arguments supplied"))
        (-let* (((kwargs restargs)
                 (->> (-partition-all 2 restargs)
                      (--split-with (keywordp (car it)))))
                (restargs (apply #'append restargs)))
          (-some->> (-difference (--map (car it) kwargs) kws)
                    (error "Invalid keyword(s) found: %s"))
          (when (-filter #'keywordp restargs)
            (error "Keywords not allowed in rest arguments when kw-args used"))
          (when (and restargs (not use-rest?))
            (error "Too many arguments supplied"))
          (list optargs (apply #'append kwargs) restargs))))))

(defun om-elem--make-header (body args)
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

(defun om-elem--transform-lambda (args body name)
  "Transform ARGS and BODY to a block bound to NAME."
  (let* ((partargs (-partition-before-pred
                    (lambda (it) (memq it '(&pos &rest &optional &key)))
                    (cons '&pos args)))
         (opt-lets
          (->> (alist-get '&optional partargs)
               (--map-indexed (om-elem--make-optarg-let it it-index))))
         (kw-lets (->> (alist-get '&key partargs)
                       (-map #'om-elem--make-kwarg-let)))
         (rest-arg (->> (alist-get '&rest partargs)
                        (om-elem--verify-rest-arg)))
         (header (om-elem--make-header body args))
         ;; (car (macroexp-parse-body body)))
         (body (->> (macroexp-parse-body body)
                    (cdr)
                    (append `(cl-block ,name))))
         (pos-args (->> (alist-get '&pos partargs)
                        (om-elem--verify-pos-args)))
         (arg-form (if (not (or opt-lets kw-lets rest-arg))
                       `(,@pos-args)
                     `(,@pos-args &rest --rest-args)))
         (let-forms
          (when (or opt-lets rest-arg kw-lets)
            (let ((opt-len (length opt-lets))
                  (keys (-map #'car kw-lets))
                  (rest-let (when rest-arg `((,rest-arg (nth 2 s)))))
                  (lets (append opt-lets (-map #'cdr kw-lets))))
              `((s (om-elem--partition-rest-args
                    --rest-args ,opt-len (quote ,keys)
                    ,(not (null rest-arg))))
                (--opt-args (nth 0 s))
                (--kw-args (nth 1 s))
                ,@rest-let
                ,@lets)))))
    ;; mercilessly stolen from cl--transform-whatever
    `(defun ,name ,arg-form
       ,header
       ,(macroexp-let*
         let-forms
         (macroexp-progn `(,body))))))

;; TODO catch duplicate keys
(defmacro om-elem--defun (name args &rest body)
  "Define NAME as a function.

   ((VAR [PRED])...
    [&optional ((VAR [PRED]) [INITFORM])...]
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
  (om-elem--transform-lambda args body name))

;;; MISC HELPER FUNCTIONS

(defun om-elem--construct (type props contents)
  "Make a new org element list structure of TYPE, PROPS, and CONTENTS.
TYPE is a symbol, PROPS is a plist, and CONTENTS is a list or nil."
  `(,type ,props ,@contents))

(defmacro om-elem--verify (&rest args)
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

(defun om-elem--non-neg-integer-p (i)
  (and (integerp i) (<= 0 i)))

(defun om-elem--from-string (string)
  "Convert STRING to org-element representation."
  (with-temp-buffer
    (insert string)
    (-> (om-elem-parse-this-buffer) (om-elem--get-contents) (car))))

(defun om-elem--build-secondary-string (string)
  "Return a list of elements from STRING as a secondary string."
  (->> (om-elem--from-string string)
       (om-elem--get-nested-contents '(0))
       (om-elem--get-contents)))

(defun om-elem--gen-anaphoric-form (fun &optional docstring)
  "Generate the anaphoric form of FUN where FUN points to a function.
Optionally supply DOCSTRING to override the generic docstring."
  (let* ((fun-name (intern (format "%s*" fun)))
         (arglist (->> (help-function-arglist fun)
                       (-replace 'fun 'form)))
         (doc-string (format "Anaphoric form of `%s'" fun))
         (funargs (->> (help-function-arglist fun)
                       (--map (if (eq it 'fun)
                                  "(lambda (it) ,form)"
                                (format ",%s" it)))
                       (-map #'read)))
         (body `(backquote (,fun ,@funargs))))
    (eval `(defmacro ,fun-name ,arglist ,doc-string ,body))))

;;; LIST OPERATIONS (EXTENDING DASH)

(defun om-elem--pad-or-truncate (length pad list)
  (let ((blanks (- length (length list))))
    (if (< blanks 0) (-slice list 0 (1- length))
      (append list (-repeat blanks pad)))))

(defun om-elem--plist-get-keys (plist)
  (-slice plist 0 nil 2))

(defun om-elem--plist-get-vals (plist)
  (-slice plist 1 nil 2))

(defun om--plist-non-nil (plist)
  (->> (-partition 2 plist) (-filter #'cadr) (apply #'append)))

(defun om-elem--plist-map-values (fun plist)
  (let ((keys (om-elem--plist-get-keys plist)))
    (->> (om-elem--plist-get-vals plist)
         (--map (funcall fun it))
         (-interleave keys))))

(defun om-elem--is-plist-p (obj)
  "Return t if OBJ is a plist."
  (and
   (listp obj)
   (cl-evenp (length obj))
   (-all? #'symbolp (-slice obj 0 nil 2))))

(defun om-elem--plist-remove (key plist)
  (->> (-partition 2 plist) (--remove (eq (car it) key)) (-flatten-n 1)))

(defun om-elem--convert-intra-index (n list)
  (let* ((N (length list))
         (upper (1- N))
         (lower (- N)))
    (cond
     ((<= 0 n upper) n)
     ((>= -1 n lower) (+ N n))
     (t (error "Index (%s) out of range; must be between %s and %s"
               n lower upper)))))

(defun om-elem--convert-inter-index (n list)
  (let* ((N (length list))
         (upper N)
         (lower (- (- N) 1)))
    (cond
     ((<= 0 n upper) n)
     ((>= -1 n lower) (+ 1 N n))
     (t (error "Index (%s) out of range; must be between %s and %s"
               n lower upper)))))

(defun om-elem--insert-at (n x list)
  "Like `-insert-at' but honors negative indices N.
Negative indices count from the end of the list, with -1 inserting
X after the last element in LIST. Will give an error if N refers to
a non-existent index."
  (-insert-at (om-elem--convert-inter-index n list) x list))

(defun om-elem--remove-at (n list)
  "Like `-remove-at' but honors negative indices N.
Negative indices count from the end of the list, with -1 inserting
X after the last element in LIST. Will give an error if N refers to
a non-existent index."
  (-remove-at (om-elem--convert-intra-index n list) list))

(defun om-elem--replace-at (n x list)
  (-replace-at (om-elem--convert-intra-index n list) x list))

(defun om-elem--nth (n list)
  (nth (om-elem--convert-intra-index n list) list))

(defun om-elem--map-first (fun list)
  (om-elem--verify fun functionp)
  (->> (cdr list) (cons (funcall fun (car list)))))

(om-elem--gen-anaphoric-form #'om-elem--map-first)

(defun om-elem--map-last (fun list)
  (->> (nreverse list) (om-elem--map-first fun) (nreverse)))

(om-elem--gen-anaphoric-form #'om-elem--map-last)

;;; INTERNAL CONSTANTS

(defconst om-elem-object-restrictions
  (->> org-element-object-restrictions
       ;; remove non-objects
       (--remove (memq (car it) '(inlinetask item headline keyword)))
       ;; add plain-text type
       (--map-when (not (eq (car it) 'table-row)) (-snoc it 'plain-text)))
  "Alist of object restrictions for object containers.
Unlike `org-element-object-restrictions', this only includes objects
and object containers and includes the 'plain-text' type.")

(defconst om-elem-element-restrictions
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
    `((center-block ,@standard)
      (drawer ,@standard)
      (dynamic-block ,@standard)
      (footnote-definition ,@standard)
      ;; headlines and sections can only be in headlines
      (headline headline section)
      (item ,@standard)
      ;; items can only be in plain-lists
      (plain-list item)
      ;; node-properties can only be in property-drawers
      (property-drawer node-property)
      (quote-block ,@standard)
      (section ,@standard)
      (special-block ,@standard)
      ;; table-rows can only be in tables
      (table table-row)))
  "Alist of element restrictions for greater elements.")

(defconst om-elem-restrictions
  (append om-elem-element-restrictions om-elem-object-restrictions)
  "Alist of all restrictions for containers.")

;;; INTERNAL TYPE FUNCTIONS

(defvaralias 'om-elem-object-containers 'org-element-object-containers)
(defvaralias 'om-elem-recursive-objects 'org-element-recursive-objects)

(defconst om-elem-elements
  (cons 'org-data org-element-all-elements))

(defconst om-elem-greater-elements
  (cons 'org-data org-element-greater-elements))

(defconst om-elem-objects
  (cons 'plain-text org-element-all-objects)
  "List of all object types including 'plain-text'.")

(defconst om-elem-elements-and-objects
  (append om-elem-elements om-elem-objects)
  "List of all elements and objects.")

(defconst om-elem-containers
  (append om-elem-greater-elements om-elem-object-containers)
  "List of elements/objects that can hold other elements/objects.")

(defconst om-elem-container-elements
  (append om-elem-greater-elements om-elem-object-containers)
  "List of elements/objects that can hold other elements/objects.")

;; TODO this naming is stupid
(defconst om-elem-atomic-elements
  (-> om-elem-elements
      (-difference om-elem-greater-elements)
      (-difference om-elem-object-containers))
  "List of elements that are not containers.")

(defconst om-elem-atomic-objects
  (-> om-elem-objects (-difference om-elem-recursive-objects))
  "List of objects that are not containers.")

(defconst om-elem-atoms
  (append om-elem-atomic-objects om-elem-atomic-elements)
  "List of objects and elements that are not containers.")

(defalias 'om-elem--get-type 'org-element-type)
(defalias 'om-elem--get-class 'org-element-class)

(defun om-elem--gen-type-predicate (fun)
  (let ((fun-name (intern (format "om-elem-is-%s-p" fun)))
        (doc-string
         (format "Return t if ELEM is an org element of type %s" fun)))
    (eval `(defun ,fun-name (elem)
             ,doc-string
             (eq ',fun (om-elem--get-type elem))))))

(-each om-elem-elements-and-objects #'om-elem--gen-type-predicate)

(defun om-elem--is-type-p (type elem)
  "Return t if ELEM's type is `eq' to TYPE (a symbol)."
  (eq (om-elem--get-type elem) type))

(defun om-elem--is-any-type-p (types elem)
  "Return t if ELEM's type is any in TYPES (a list of symbols)."
  (if (memq (om-elem--get-type elem) types) t))

(defun om-elem--is-plain-text-p (elem)
  "Return t if org element ELEM is of type 'plain-text'."
  (eq 'plain-text (om-elem--get-type elem)))

;; (defun om-elem-is-object-p (elem)
;;   "Return t is ELEM is an object."
;;   (om-elem--is-any-type-p om-elem-objects elem))

;; (defun om-elem-is-element-p (elem)
;;   "Return t is ELEM is an element."
;;   (om-elem--is-any-type-p om-elem-elements elem))

(defun om-elem--is-element-or-object-p (elem)
  "Return t is ELEM is an element or an object."
  (om-elem--is-any-type-p om-elem-elements-and-objects elem))

;; (defun om-elem-is-greater-element-p (elem)
;;   "Return t is ELEM is a greater element."
;;   (om-elem--is-any-type-p om-elem-greater-elements elem))

;; (defun om-elem-is-container-p (elem)
;;   "Return t is ELEM is a container."
;;   (om-elem--is-any-type-p om-elem-containers elem))

;; (defun om-elem-is-recursive-object-p (elem)
;;   "Return t is ELEM is a recursive object."
;;   (om-elem--is-any-type-p om-elem-recursive-objects elem))

;; (defun om-elem-is-allowed-object-p (container-type elem)
;;   "Return t if object ELEM is allowed to be in CONTAINER-TYPE."
;;   (-if-let (r (alist-get container-type om-elem-object-restrictions))
;;       (om-elem--is-any-type-p r elem)
;;     (error "Invalid container type requested: %s" container-type)))

;; filters

(defun om-elem--filter-type (type elem)
  "Return ELEM if it is TYPE or nil otherwise."
  (and (om-elem--is-type-p type elem) elem))

(defun om-elem--filter-types (types elem)
  "Return ELEM if it is one of TYPES or nil otherwise."
  (and (om-elem--is-any-type-p types elem) elem))

(defun om-elem--oneline-string-p (s)
  (and (stringp s) (not (s-contains? "\n" s))))

(defun om-elem--oneline-string-or-null-p (s)
  (or (null s) (om-elem--oneline-string-p s)))

;; filters (general)

(defun om-elem--filter (value prop elem msg pred)
  (declare (indent 4))
  (if (funcall pred value) value
    (error
     "Property '%s' in element/object of type '%s' must be a %s. Got '%S'"
     prop (om-elem--get-type elem) msg value)))

(defun om-elem--filter-string (v p eo)
  (om-elem--filter v p eo "string" #'stringp))

(defun om-elem--filter-string-or-nil (v p eo)
  (om-elem--filter v p eo "string or nil" #'string-or-null-p))

(defun om-elem--filter-oneline-string (v p eo)
  (om-elem--filter v p eo "oneline string" #'om-elem--oneline-string-p))

(defun om-elem--filter-oneline-string-or-nil (v p eo)
  (om-elem--filter v p eo "oneline string or nil"
    #'om-elem--oneline-string-or-null-p))

(defun om-elem--filter-boolean (v p eo)
  (om-elem--filter v p eo "t or nil" #'booleanp))

(defun om-elem--filter-non-neg-integer (v p eo)
  (om-elem--filter v p eo "non-negative integer"
    (lambda (x) (and (integerp x) (<= 0 x)))))

(defun om-elem--filter-non-neg-integer-or-nil (v p eo)
  (om-elem--filter v p eo "non-negative integer or nil"
    (lambda (x) (or (null x) (and (integerp x) (<= 0 x))))))

(defun om-elem--filter-pos-integer (v p eo)
  (om-elem--filter v p eo "positive integer"
    (lambda (x) (and (integerp x) (<= 0 x)))))

(defun om-elem--filter-pos-integer-or-nil (v p eo)
  (om-elem--filter v p eo "positive integer or nil"
    (lambda (x) (or (null x) (and (integerp x) (<= 0 x))))))

(defun om-elem--filter-string-list (v p eo)
  (om-elem--filter v p eo "list of oneline strings"
    (lambda (x)
      (or (null x)
          (and (listp x) (-all? #'om-elem--oneline-string-p x))))))

(defun om-elem--filter-symbols (v p eo syms)
  (om-elem--filter v p eo (format "symbol from %S" syms)
    (lambda (x) (memq x syms))))

;; filters (type specific)

(defun om-elem--filter-link-format (v p eo)
  (om-elem--filter-symbols v p eo '(nil plain angle bracket)))

(defun om-elem--filter-link-type (v p eo)
  (let* ((builtin '("coderef" "custom-id" "file" "id" "radio" "fuzzy"))
         (msg (format "string from built-in types %S or `org-link-types'"
                      builtin)))
    (om-elem--filter v p eo msg
      ;; TODO allow nil here for fuzzy?
      (lambda (type) (member type (append builtin (org-link-types)))))))

(defun om-elem--filter-item-checkbox (v p eo)
  (om-elem--filter-symbols v p eo '(nil on off trans)))

(defun om-elem--filter-item-tag (v p eo)
  (om-elem--filter v p eo "secondary-string that follows `om-elem--item-tag-restrictions'"
    (lambda (x)
      (--all? (om-elem--is-any-type-p om-elem--item-tag-restrictions it) x))))

(defun om-elem--filter-clock-timestamp (v p eo)
  (om-elem--filter v p eo "(ranged) inactive timestamp with no warning/repeater"
    (lambda (ts)
      (and (om-elem--is-type-p 'timestamp ts)
           (om-elem--property-is-predicate-p*
            :type (memq it '(inactive inactive-range)) ts)
           (om-elem--property-is-nil-p :repeater-type ts)))))

(defun om-elem--filter-planning-timestamp (v p eo)
  (om-elem--filter v p eo "an zero-range, inactive timestamp object"
    (lambda (ts)
      (or (null ts)
          (and (om-elem--is-type-p 'timestamp ts)
               (om-elem--property-is-eq-p :type 'inactive ts))))))

(defun om-elem--filter-entity-name (v p eo)
  (om-elem--filter v p eo "string that makes `org-entity-get' return non-nil"
    (lambda (n) (org-entity-get n))))

(defun om-elem--filter-headline-tags (v p eo)
  (om-elem--filter v p eo "list of oneline strings without `org-archive-tag'"
    (lambda (x) (and (-all? #'om-elem--oneline-string-p x) 
                (not (member org-archive-tag x))))))

(defun om-elem--filter-headline-priority (v p eo)
  (om-elem--filter v p eo "integer between `org-lowest-priority' and `org-highest-priority'"
    (lambda (x)
      (or (null x)
          (and (integerp x)
               (>= org-lowest-priority x org-highest-priority))))))

(defun om-elem--filter-headline-title (v p eo)
  (om-elem--filter v p eo "secondary-string that follows `om-elem--headline-title-restrictions'"
    (lambda (x)
      (--all? (om-elem--is-any-type-p om-elem--headline-title-restrictions it) x))))

(defun om-elem--filter-timestamp-type (v p eo)
  (om-elem--filter-symbols v p eo '(inactive inactive-range active
                                            active-range diary)))

(defun om-elem--filter-timestamp-repeater-type (v p eo)
  (om-elem--filter-symbols v p eo '(nil catch-up restart cumulate)))

(defun om-elem--filter-timestamp-warning-type (v p eo)
  (om-elem--filter-symbols v p eo '(nil all first)))

(defun om-elem--filter-timestamp-unit (v p eo)
  (om-elem--filter-symbols v p eo '(nil year month week day hour)))

;; encode/decode (general)

(defun om-elem--encode-string-list-delim (v p eo delim)
  (-some->> (om-elem--filter-string-list v p eo) (s-join delim)))

(defun om-elem--decode-string-list-delim (v delim)
  (and v (s-split delim v)))

(defun om-elem--encode-string-list-space-delim (v p eo)
  (om-elem--encode-string-list-delim v p eo " "))

(defun om-elem--decode-string-list-space-delim (v)
  (om-elem--decode-string-list-delim v " "))

(defun om-elem--encode-string-list-comma-delim (v p eo)
  (om-elem--encode-string-list-delim v p eo ","))

(defun om-elem--decode-string-list-comma-delim (v)
  (om-elem--decode-string-list-delim v ","))

(defun om-elem--encode-plist (v p eo)
  (-some->> (om-elem--filter v p eo "plist" #'om-elem--is-plist-p)
            (--map (format "%S" it))
            (s-join " ")))

(defun om-elem--decode-plist (v)
  (-map #'intern (om-elem--decode-string-list-space-delim v)))

;; encode/decode (type specific)

(defun om-elem--encode-latex-environment-value (v p eo)
  (let ((msg ))
    (-let (((env body) v))
      (cond
       ((and (om-elem--oneline-string-p env) (stringp body))
        (format "\\begin{%1$s}\n%2$s\n\\end{%1$s}" env body))
       ((and (om-elem--oneline-string-p env) (null body))
        (format "\\begin{%1$s}\n\\end{%1$s}" env))
       (t
        (let ((fmt
               (s-join
                " "
                (list "Latex environment value must be a list of strings"
                      "like (ENV BODY) or (ENV) where ENV is"
                      "a oneline string and BODY is a string. Got %S"))))
          (error fmt v))))))) 

(defun om-elem--decode-latex-environment-value (v)
  ;; TODO ensure that the output is correct?
  (let ((m (car (s-match-strings-all "\\\\begin{\\(.+\\)}\n\\(.*\\)\n?\\\\end{\\(.+\\)}" v))))
    (list (nth 1 m) (nth 2 m))))

;; NOTE org mode 9.1.9 will crash when given an alphabetic symbol
(defun om-elem--encode-item-bullets (bullet p eo)
  (if (memq bullet '(- +)) (format "%s " bullet)
    (-if-let (c (->> (if (listp bullet) (car bullet) bullet)
                     (om-elem--item-validate-counter)))
        (format (if (listp bullet) "%s) " "%s. ") c)
      ;; TODO need better message
      (error "Invalid bullet: %s" bullet))))

(defun om-elem--decode-item-bullets (bullet)
  ;; TODO refactor this
  (if (s-matches? "^\\(-\\|+\\)" bullet)
      (intern (s-left 1 bullet))
    (let* ((case-fold-search nil) ; need case-sensitivity
           (n (or (-some->> (s-match "^[0-9]+" bullet)
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
                  (error "Invalid bullet found: %s" bullet))))
      (cond
       ((s-matches? "^[a-zA-Z0-9]+." bullet) n)
       ((s-matches? "^[a-zA-Z0-9]+)" bullet) (list n))
       (t (error "Invalid bullet found: %s" bullet))))))

(defun om-elem--decode-item-tag (v)
  (om-elem--build-secondary-string v))

(defun om-elem--decode-headline-tags (v)
  (remove org-archive-tag v))

(defun om-elem--encode-statistics-cookie-value (v p eo)
  ;; TODO need better error messages
  (cl-flet
      ((mk-stat
        (v)
        (pcase v
          (`(nil) "%")
          (`(nil nil) "/")
          (`(,(and (pred integerp) percent))
           (if (< 100 percent) (error "Percent greater than 100")
             (format "%s%%" percent)))
          (`(,(and (pred integerp) numerator)
             ,(and (pred integerp) denominator))
           (if (> numerator denominator)
               (error "Numerator greater than denominator")
             (format "%s/%s" numerator denominator)))
          (_ (error "Invalid stat-cookie value: %S" v)))))
    (format "[%s]" (mk-stat v))))

(defun om-elem--decode-statistics-cookie-value (v)
  (cond
   ((equal "[%]" v) '(nil))
   ((equal "[/]" v) '(nil nil))
   (t
    (->> (or (s-match-strings-all "\\[\\([0-9]+\\)/\\([0-9]+\\)\\]" v)
             (s-match-strings-all "\\[\\([0-9]+\\)%\\]" v)
             (error "Invalid stats-cookie: %s" v))
         (car)
         (cdr)
         (-map #'string-to-number)))))

(defun om-elem--encode-diary-sexp-value (v p eo)
  (->> (om-elem--filter v p eo "list form" #'listp) (format "%%%%%S")))

(defun om-elem--decode-diary-sexp-value (v p eo)
  (->> (s-chop-prefix "%%" v) (read)))

;; cis-update functions

(defun om-elem--update-macro-value (macro)
  (let* ((k (om-elem--get-property :key macro))
         (as (om-elem--get-property :args macro))
         (v (if as (format "%s(%s)" k (s-join "," as)) k)))
    (om-elem--set-property :value (format "{{{%s}}}" v) macro)))

(defun om-elem--update-clock-duration (clock)
  (let* ((ts (om-elem--get-property :value clock))
         (plist
          (if (om-elem--timestamp-is-ranged-fast-p ts)
              (let* ((seconds (om-elem--timestamp-get-range ts))
                     (h (-> seconds (/ 3600) (floor)))
                     (m (-> seconds (- (* h 3600)) (/ 60) (floor))))
                `(:duration ,(format "%2d:%02d" h m) :status running))
            '(:duration nil :status closed))))
    (om-elem--set-properties plist clock)))

(defun om-elem--update-headline-tags (headline)
  (cl-flet
      ((add-archive-tag-maybe
        (tags)
        (let ((tags* (remove org-archive-tag tags)))
          (if (om-elem--get-property :archivedp headline)
              (-snoc tags* org-archive-tag) tags*))))
    (om-elem--map-property :tags #'add-archive-tag-maybe headline)))

;; shifters

(defun om-elem--shift-pos-integer (n x)
  (when x
    (let ((x* (+ x n)))
      (if (< 0 x*) x* 1))))

(defun om-elem--shift-non-neg-integer (n x)
  (when x
    (let ((x* (+ x n)))
      (if (<= 0 x*) x* 0))))

(defun om-elem--shift-headline-priority (n priority)
  (when priority
    (let ((diff (1+ (- org-lowest-priority org-highest-priority)))
          (offset (- priority org-highest-priority)))
      (--> (- offset n)
           (mod it diff)
           (- it offset)
           (+ priority it)))))

(defconst om-elem--type-alist
  (let ((bool (list :set 'om-elem--filter-boolean
                    :type-desc "nil or t"
                    :toggle t))
        (pos-int (list :set 'om-elem--filter-pos-integer
                       :type-desc "a positive integer"))
        (pos-int-nil (list :set 'om-elem--filter-pos-integer-or-nil
                           :type-desc "a positive integer or nil"))
        (nn-int (list :set 'om-elem--filter-non-neg-integer
                      :type-desc "a non-negative integer"))
        (nn-int-nil (list :set 'om-elem--filter-non-neg-integer-or-nil
                               :type-desc "a non-negative integer or nil"))
        (str (list :set 'om-elem--filter-string
                   :type-desc "a string"))
        (str-nil (list :set 'om-elem--filter-string-or-nil
                       :type-desc "a string or nil"))
        (ol-str (list :set 'om-elem--filter-oneline-string
                      :type-desc "a oneline string"))
        (ol-str-nil (list :set 'om-elem--filter-oneline-string-or-nil
                          :type-desc "a oneline string or nil"))
        (plist (list :set 'om-elem--encode-plist
                     :get 'om-elem--decode-plist
                     :plist t
                     :type-desc "a plist"))
        (slist (list :set 'om-elem--filter-string-list
                     :string-list t
                     :type-desc "a list of oneline strings"))
        (slist-com (list :set 'om-elem--encode-string-list-comma-delim
                         :get 'om-elem--decode-string-list-comma-delim
                         :string-list t
                         :type-desc "a list of oneline strings"))
        (slist-spc (list :set 'om-elem--encode-string-list-space-delim
                         :get 'om-elem--decode-string-list-space-delim
                         :string-list t
                         :type-desc "a list of oneline strings"))
        (planning (list :set 'om-elem--filter-planning-timestamp
                        :type-desc "a zero-range, inactive timestamp object"))
        (ts-unit (list :set 'om-elem--filter-timestamp-unit
                       :type-desc '("nil or a symbol from 'year' 'month'"
                                    "'week' 'day', or 'hour'"))))
    `((babel-call (:call ,@ol-str :require t)
                  (:inside-header ,@plist)
                  (:arguments ,@slist-com)
                  (:end-header ,@plist)
                  (:value))
      (bold)
      (center-block)
      (clock (:value :set om-elem--filter-clock-timestamp
                     :cis om-elem--update-clock-duration
                     :type-desc "an unranged, inactive timestamp with no warning or repeater"
                     :require t)
             (:status)
             (:duration))
      (code (:value ,@ol-str :require t))
      (comment (:value ,@ol-str :require t)) ; TODO this isn't actually required?
      (comment-block (:value ,@ol-str :get s-trim-right :require t)) ; TODO is this actually required?
      (drawer (:drawer-name ,@ol-str :require t))
      (diary-sexp (:value :set om-elem--encode-diary-sexp-value
                          :get om-elem--decode-diary-sexp-value
                          :type-desc "a list form"
                          ;; TODO is this actually required?
                          :require t))
      (dynamic-block (:arguments ,@plist)
                     (:block-name ,@ol-str :require t))
      (entity (:name :set om-elem--filter-entity-name
                     :type-desc "a string that makes `org-entity-get' return non-nil"
                     :require t)
              (:use-brackets-p ,@bool)
              ;; TODO what do these do?
              (:latex)
              (:latex-math-p)
              (:html)
              (:ascii)
              (:latin1)
              (:utf-8))
      (example-block (:preserve-indent ,@bool)
                     (:switches ,@slist-spc)
                     ;; TODO is this required?
                     (:value ,@str :get s-trim-right :require t)
                     ;; TODO how many of these are tied to switches?
                     (:number-lines)
                     (:retain-labels)
                     (:use-labels)
                     (:label-fmt))
      (export-block (:type ,@ol-str :require t)
                    (:value ,@str :require t))
      (export-snippet (:back-end ,@ol-str :require t)
                      (:value ,@str :require t))
      (fixed-width (:value ,@ol-str :get s-trim-right :require t))
      (footnote-definition (:label ,@ol-str-nil :require t))
      (footnote-reference (:label ,@ol-str-nil)
                          (:type))
      (headline (:archivedp ,@bool :cis om-elem--update-headline-tags)
                (:commentedp ,@bool)
                (:footnote-section-p ,@bool)
                (:level ,@pos-int
                        :shift om-elem--shift-pos-integer
                        :require 1)
                (:pre-blank ,@nn-int
                            :shift om-elem--shift-non-neg-integer
                            :require 0)
                (:priority :set om-elem--filter-headline-priority
                           :shift om-elem--shift-headline-priority
                           :type-desc ("an integer between (inclusive)"
                                       "`org-highest-priority' and"
                                       "`org-lowest-priority'"))
                (:tags :set om-elem--filter-headline-tags
                       :get om-elem--decode-headline-tags
                       :cis om-elem--update-headline-tags
                       :type-desc "a string list"
                       :string-list t)
                (:title :set om-elem--filter-headline-title
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
                        ;; TODO should this be required?
                        (:value ,@ol-str :require t))
      ;; (inlinetask)
      (italic)
      (item (:bullet :set om-elem--encode-item-bullets
                     :get om-elem--decode-item-bullets
                     :type-desc ("a positive integer (for '1.'),"
                                 "a positive integer in a list"
                                 "(for '1)'), a '-', or a '+'")
                     :require '-)
            (:checkbox :set om-elem--filter-item-checkbox
                       :type-desc "nil or the symbols 'on', 'off', or 'trans'")
            (:counter ,@pos-int-nil :shift om-elem--shift-pos-integer)
            (:tag :set om-elem--filter-item-tag
                  :type-desc "a secondary string")
            (:structure))
      (keyword (:key ,@ol-str :require t)
               (:value ,@ol-str :require t))
      (latex-environment (:value :set om-elem--encode-latex-environment-value
                                 :get om-elem--decode-latex-environment-value
                                 :type-desc "list of strings like (ENV BODY) or (ENV)"
                                 :require t))
      (latex-fragment (:value ,@str :require t))
      (line-break)
      (link (:path ,@ol-str :require t)
            (:format :set om-elem--filter-link-format
                     :type-desc "the symbol 'plain', 'bracket' or 'angle'")
            (:type :set om-elem--filter-link-type
                   ;; TODO make this desc better
                   :type-desc ("a oneline string from `org-link-types'"
                               "or \"coderef\", \"custom-id\","
                               "\"file\", \"id\", \"radio\", or"
                               "\"fuzzy\"")
                   ;; TODO is fuzzy a good default?
                   :require "fuzzy")
            (:raw-link) ; update contents through this?
            (:application)
            (:search-option))
      (macro (:args ,@slist :cis om-elem--update-macro-value)
             (:key ,@ol-str :cis om-elem--update-macro-value :require t)
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
      (radio-target (:value))
      (section)
      (special-block (:type ,@ol-str :require t))
      (src-block (:value ,@str :get s-trim-right :require t) ; TODO should this actually be required? nil should = ""
                 (:language ,@str-nil)
                 (:parameters ,@plist)
                 (:preserve-indent ,@bool)
                 (:switches ,@slist-spc)
                 (:number-lines)
                 (:retain-labels)
                 (:use-labels)
                 (:label-fmt))
      (statistics-cookie (:value
                          :set om-elem--encode-statistics-cookie-value
                          :get om-elem--decode-statistics-cookie-value
                          :type-desc ("a list of non-neg integers"
                                      "like (PERC) or (NUM DEN)"
                                      "which make [NUM/DEN] and"
                                      ;; TODO this is formatted weirdly
                                      "[PERC %] respectively")
                          :require t))
      (strike-through)
      (subscript (:use-brackets-p ,@bool))
      (superscript (:use-brackets-p ,@bool))
      (table (:tblfm ,@slist)
             (:type :const 'org)
             (:value))
      (table-cell)
      (table-row (:type :const 'standard))
      (target (:value ,@ol-str :require t))
      (timestamp (:type :set om-elem--filter-timestamp-type
                        :type-desc ("a symbol from 'inactive',"
                                    "'active', 'inactive-ranged', or"
                                    "'active-ranged'")
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
                 (:repeater-type :set om-elem--filter-timestamp-repeater-type
                                 :type-desc ("nil or a symbol from"
                                             "'catch-up', 'restart',"
                                             "or 'cumulate'"))
                 (:repeater-unit ,@ts-unit)
                 (:repeater-value ,@pos-int-nil)
                 (:warning-type :set om-elem--filter-timestamp-warning-type
                                :type-desc ("nil or a symbol from"
                                            "'all' or 'first'"))
                 (:warning-unit ,@ts-unit)
                 (:warning-value ,@pos-int-nil)
                 (:raw-value))
      (underline)
      (verbatim (:value ,@ol-str :require t))
      (verse-block))))

;; add post-blank functions to all entries
(let ((post-blank-funs '(:post-blank :set om-elem--filter-non-neg-integer
                                     :shift om-elem--shift-non-neg-integer)))
  (setq om-elem--type-alist
        (--map (-snoc it post-blank-funs) om-elem--type-alist)))

(defun om-elem--get-strict-function (operation type prop)
  (-if-let (type-list (alist-get type om-elem--type-alist))
      (-if-let (plist (alist-get prop type-list))
          (plist-get plist operation)
        (error "Unsettable property '%s' for type '%s' requested"
               prop type))
    (error "Tried to get property for non-existent type %s" type)))

(defun om-elem--get-setter-function (type prop)
  (om-elem--get-strict-function :set type prop))

(defun om-elem--get-getter-function (type prop)
  (om-elem--get-strict-function :get type prop))

(defun om-elem--get-update-function (type prop)
  (om-elem--get-strict-function :cis type prop))

(defun om-elem--set-property-strict (prop value elem)
  (let* ((type (om-elem--get-type elem))
         (filter-fun (om-elem--get-setter-function type prop))
         (update-fun (om-elem--get-update-function type prop))
         (elem* (--> (funcall filter-fun value prop elem)
                     (om-elem--set-property prop it elem))))
    (if update-fun (funcall update-fun elem*) elem*)))

(defun om-elem--set-properties-strict (plist elem)
  (cl-flet
      ((filter
        (acc prop-value type)
        (-let* (((prop value) prop-value)
                (filter-fun (om-elem--get-setter-function type prop)))
          (->> (funcall filter-fun value prop elem)
               (funcall #'plist-put acc prop)))))
    (if (om-elem--is-plist-p plist)
        (let* ((cur-props (om-elem--get-properties elem))
               (type (om-elem--get-type elem))
               (prop-values (-partition 2 plist))
               (update-funs
                (->> (-map #'car prop-values)
                     (--map (om-elem--get-update-function type it))
                     (-uniq)
                     (-non-nil)))
               (elem*
                (om-elem--construct
                 (om-elem--get-type elem)
                 (--reduce-from (filter acc it type) cur-props prop-values)
                 (om-elem--get-contents elem))))
          (if (not update-funs) elem*
            (--reduce-from (funcall it acc) elem* update-funs)))
      (error "Not a plist: %S" plist))))

(defun om-elem--get-property-strict (prop elem)
  (let ((filter-fun (-> (om-elem--get-type elem)
                        (om-elem--get-getter-function prop)))
        (value (om-elem--get-property prop elem)))
    (if filter-fun (funcall filter-fun value) value)))

(defun om-elem--map-property-strict (prop fun elem)
  (om-elem--verify fun functionp)
  (let ((value (funcall fun (om-elem--get-property-strict prop elem))))
    (om-elem--set-property-strict prop value elem)))

(om-elem--gen-anaphoric-form #'om-elem--map-property-strict)

(defun om-elem--map-properties-strict (plist elem)
  (cond
   ((not plist) elem)
   ((om-elem--is-plist-p plist)
    (->> (om-elem--map-property-strict (nth 0 plist) (nth 1 plist) elem)
         (om-elem--map-properties-strict (-drop 2 plist))))
   (t (error "Not a plist: %s" plist))))

;; (defmacro om-elem--map-properties-strict* (plist elem)
;;   (let ((plist (om-elem--plist-map-values (lambda (form) `(lambda (it) ,form)) plist)))
;;     `(om-elem--map-properties-strict ,plist ,elem)))

;;; INTERNAL PROPERTY FUNCTIONS

;;; generic

;; (defalias 'om-elem--get-property 'org-element-property)
(defun om-elem--get-property (prop elem)
  (if (and (stringp elem) (eq prop :post-blank))
      (length (car (s-match "[ ]*$" elem)))
    (org-element-property prop elem)))

(defun om-elem--get-properties (elem)
  "Return the properties list of ELEM."
  (if (stringp elem) (text-properties-at 0 elem) (nth 1 elem)))

(defun om-elem--get-parent (elem)
  "Return the parent of ELEM."
  (om-elem--get-property :parent elem))

(defun om-elem--get-parent-headline (elem)
  "Return the most immediate parent headline of ELEM."
  (-when-let (parent (om-elem--get-parent elem))
    (if (om-elem--is-type-p 'headline parent) parent
      (om-elem--get-parent-headline parent))))

;; (defun om-elem--get-parent-item (elem)
;;   "Return the parent item element for ELEM."
;;   (om-elem-match-parent elem :many 'item))

(defun om-elem--set-property (prop value elem)
  "Set property PROP in element ELEM to VALUE."
  ;; TODO validate that prop exists in elem first?
  (if (stringp elem)
      (if (eq prop :post-blank)
          (->> (s-trim-right elem) (s-append (s-repeat value " ")))
        (org-add-props elem nil prop value))
    (om-elem--construct
     (om-elem--get-type elem)
     (plist-put (om-elem--get-properties elem) prop value)
     (om-elem--get-contents elem))))

(defun om-elem--set-properties (plist elem)
  "Set all properties in ELEM to the values corresponding to PLIST.
PLIST is a list of property-value pairs that correspond to the
property list in ELEM."
  (if (om-elem--is-plist-p plist)
      (let ((props (om-elem--get-properties elem)))
        (om-elem--construct
         (om-elem--get-type elem)
         (->> (-partition 2 plist)
              (--reduce-from (apply #'plist-put acc it) props))
         (om-elem--get-contents elem)))
    (error "Not a plist: %S" plist)))

(defun om-elem--set-property-nil (prop elem)
  "Set property PROP to nil in ELEM."
  (om-elem--set-property prop nil elem))

(defun om-elem--set-properties-nil (props elem)
  "Set all properties PROPS to new in ELEM."
  (let ((plist (--mapcat (list it nil) props)))
    (om-elem--set-properties plist elem)))

(defun om-elem--map-property (prop fun elem)
  (om-elem--verify fun functionp)
  (let ((value (funcall fun (om-elem--get-property prop elem))))
    (om-elem--set-property prop value elem)))

(om-elem--gen-anaphoric-form #'om-elem--map-property)

(defun om-elem--map-properties (plist elem)
  (cond
   ((not plist) elem)
   ((om-elem--is-plist-p plist)
    (->> (om-elem--map-property (nth 0 plist) (nth 1 plist) elem)
         (om-elem--map-properties (-drop 2 plist))))
   (t (error "Not a plist: %s" plist))))

(defmacro om-elem--map-properties* (plist elem)
  `(let ((plist*
          (-map-indexed
           (lambda (index item) (if (cl-evenp index) item `(lambda (it) ,item)))
           ,plist)))
     (om-elem--map-properties new-plist ,elem)))

(defun om-elem--property-is-nil-p (prop elem)
  "Return t if PROP in ELEM is nil."
  (not (om-elem--get-property prop elem)))

(defun om-elem--property-is-non-nil-p (prop elem)
  "Return t if PROP in ELEM is not nil."
  (if (om-elem--get-property prop elem) t))

(defun om-elem--property-is-eq-p (prop val elem)
  "Return t if PROP in ELEM is `eq' to VAL."
  (eq val (om-elem--get-property prop elem)))

(defun om-elem--property-is-equal-p (prop val elem)
  "Return t if PROP in ELEM is `equal' to VAL."
  (equal val (om-elem--get-property prop elem)))

(defun om-elem--property-is-predicate-p (prop fun elem)
  "Return t if FUN applied to the value of PROP in ELEM results not nil.
FUN is a predicate function that takes one argument."
  (->> (om-elem--get-property prop elem) (funcall fun) (and)))

(om-elem--gen-anaphoric-form #'om-elem--property-is-predicate-p)

;;; objects
;;
;; statistics-cookie

(defun om-elem--statistics-cookie-is-complete-p (statistics-cookie)
  (let ((val (om-elem--get-property :value statistics-cookie)))
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

(defun om-elem--statistics-cookie-get-format (statistics-cookie)
  (cond ((s-contains? "/" it) 'fraction)
        ((s-contains? "%" it) 'percent)
        (t (error "Unparsable statistics cookie value: %s"
                  (om-elem--get-property :value)))))

;; timestamp (auxiliary functions)

(defun om-elem--time-is-long-p (time)
  (pcase time
    (`(,(pred integerp) ,(pred integerp) ,(pred integerp)
       ,(pred integerp) ,(pred integerp))
     t)))

(defun om-elem--time-to-unixtime (time)
  (let ((encoded
         (if (om-elem--time-is-long-p time)
             (apply #'encode-time 0 (nreverse time))
           (apply #'encode-time 0 0 0 (nreverse (-take 3 time))))))
    (round (float-time encoded))))

(defun om-elem--unixtime-to-time-long (unixtime)
  (nreverse (-slice (decode-time unixtime) 1 6)))

(defun om-elem--unixtime-to-time-short (unixtime)
  (append (-take 3 (om-elem--unixtime-to-time-long unixtime))
          '(nil nil)))

(defun om-elem--time-shift (n unit time)
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
    (if (om-elem--time-is-long-p time)
        (let ((shifts (get-shifts-long n unit)))
          (nreverse (-slice (apply-shifts shifts time) 1 6)))
      (let ((shifts (get-shifts-short n unit))
            (time* (-replace nil 0 time)))
        (->> (-slice (apply-shifts shifts time*) 3 6)
             (append '(nil nil))
             (nreverse))))))

(defun om-elem--time-format-props (time suffix)
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

(defun om-elem--decorator-format (dec dtype valid-types)
  (let ((props (->> '(type value unit)
                    (--map (intern (format ":%s-%s" dtype it))))))
    (if (not dec) (om-elem--init-properties props)
      (-let (((type value unit) dec))
        (unless (memq type valid-types)
          (error "Invalid %s type: %s" dtype type))
        (unless (integerp value)
          (error "Invalid %s value: %s" dtype value))
        (unless (memq unit '(year month week day hour))
          (error "Invalid %s unit: %s" dtype value))
        (-interleave props (list type value unit))))))

;; timestamp (regular)

(defun om-elem--timestamp-get-start-time (timestamp)
  (-let (((&plist :minute-start n :hour-start h :day-start d
                  :month-start m :year-start y)
          (om-elem--get-properties timestamp)))
    `(,y ,m ,d ,h ,n)))

(defun om-elem--timestamp-get-end-time (timestamp)
  (-let (((&plist :minute-end n :hour-end h :day-end d
                  :month-end m :year-end y)
          (om-elem--get-properties timestamp)))
    `(,y ,m ,d ,h ,n)))

(defun om-elem--timestamp-get-start-timestamp (timestamp)
  (if (not (om-elem--timestamp-is-ranged-fast-p timestamp)) timestamp
    (om-elem--timestamp-set-end-time nil timestamp)))

(defun om-elem--timestamp-get-end-timestamp (timestamp)
  (when (om-elem--timestamp-is-ranged-fast-p timestamp)
    (-> (om-elem--timestamp-get-end-time timestamp)
        (om-elem--timestamp-set-single-time timestamp))))

(defun om-elem--timestamp-get-start-unixtime (timestamp)
  (->> (om-elem--timestamp-get-start-time timestamp)
       (om-elem--time-to-unixtime)))

(defun om-elem--timestamp-get-end-unixtime (timestamp)
  (->> (om-elem--timestamp-get-end-time timestamp)
       (om-elem--time-to-unixtime)))

(defun om-elem--timestamp-get-range (timestamp)
  (- (om-elem--timestamp-get-end-unixtime timestamp)
     (om-elem--timestamp-get-start-unixtime timestamp)))

(defun om-elem--timestamp-is-active-p (timestamp)
   (memq (om-elem--get-property :type timestamp) '(active active-range)))

(defun om-elem--timestamp-is-ranged-p (timestamp)
  (/= 0 (om-elem--timestamp-get-range timestamp)))

(defun om-elem--timestamp-start-is-long-p (timestamp)
  (->> (om-elem--timestamp-get-start-time timestamp)
       (om-elem--time-is-long-p)))

(defun om-elem--timestamp-end-is-long-p (timestamp)
  (->> (om-elem--timestamp-get-end-time timestamp)
       (om-elem--time-is-long-p)))

(defun om-elem--timestamp-start-is-less-than-p (unixtime timestamp)
  (< (om-elem--timestamp-get-start-unixtime timestamp) unixtime))

(defun om-elem--timestamp-start-is-greater-than-p (unixtime timestamp)
  (> (om-elem--timestamp-get-start-unixtime timestamp) unixtime))

(defun om-elem--timestamp-start-is-equal-to-p (unixtime timestamp)
  (= (om-elem--timestamp-get-start-unixtime timestamp) unixtime))

(defun om-elem--timestamp-end-is-less-than-p (unixtime timestamp)
  (< (om-elem--timestamp-get-end-unixtime timestamp) unixtime))

(defun om-elem--timestamp-end-is-greater-than-p (unixtime timestamp)
  (> (om-elem--timestamp-get-end-unixtime timestamp) unixtime))

(defun om-elem--timestamp-end-is-equal-to-p (unixtime timestamp)
  (= (om-elem--timestamp-get-end-unixtime timestamp) unixtime))

(defun om-elem--timestamp-is-ranged-fast-p (timestamp)
  "Like `om-elem--timestamp-is-ranged-p' but faster.
This only looks at TIMESTAMP's :type property rather than computing
float-times, which assumes the :type property is valid."
  (memq (om-elem--get-property :type timestamp)
        '(active-range inactive-range)))

(defun om-elem--timestamp-set-start-time-nocheck (time timestamp)
  "Set the start TIME of TIMESTAMP."
  (let ((time* (om-elem--time-format-props time 'start)))
      (om-elem--set-properties time* timestamp)))

(defun om-elem--timestamp-set-start-time (time timestamp)
  (->> (om-elem--timestamp-set-start-time-nocheck time timestamp)
       (om-elem--timestamp-update-type-ranged)))

(defun om-elem--timestamp-set-end-time-nocheck (time timestamp)
  "Set the end TIME of TIMESTAMP."
  (if time
      (-> (om-elem--time-format-props time 'end)
          (om-elem--set-properties timestamp))
    (-> (om-elem--timestamp-get-start-time timestamp)
        (om-elem--time-format-props 'end)
        (om-elem--set-properties timestamp))))

(defun om-elem--timestamp-set-end-time (time timestamp)
  (let ((ts* (om-elem--timestamp-set-end-time-nocheck time timestamp)))
    (if time (om-elem--timestamp-update-type-ranged ts*)
      (om-elem--timestamp-set-type-ranged nil ts*))))

(defun om-elem--timestamp-set-single-time (time timestamp)
  "Set the start TIME of TIMESTAMP."
  (->> (om-elem--timestamp-set-start-time-nocheck time timestamp)
       (om-elem--timestamp-set-end-time-nocheck time)
       (om-elem--timestamp-set-type-ranged nil)))

(defun om-elem--timestamp-set-double-time (time1 time2 timestamp)
  (->> (om-elem--timestamp-set-start-time-nocheck time1 timestamp)
       (om-elem--timestamp-set-end-time-nocheck time2)
       (om-elem--timestamp-update-type-ranged)))

(defun om-elem--timestamp-set-range (range timestamp)
  (let* ((start (om-elem--timestamp-get-start-time timestamp))
         (long? (om-elem--time-is-long-p start))
         (range (* range (if long? 60 86400)))
         (t2 (--> (om-elem--time-to-unixtime start)
                  (+ it range)
                  (if long? (om-elem--unixtime-to-time-long it)
                    (om-elem--unixtime-to-time-short it)))))
    (->> (om-elem--timestamp-set-end-time-nocheck t2 timestamp)
         (om-elem--timestamp-set-type-ranged (/= 0 range)))))

(defun om-elem--timestamp-update-type-ranged (timestamp)
  (-> (om-elem--timestamp-is-ranged-p timestamp)
      (om-elem--timestamp-set-type-ranged timestamp)))

(defun om-elem--timestamp-set-type-ranged (ranged? timestamp)
  (cl-flet
      ((update-range
       (type)
       (cl-case type
         ((active active-range)
          (if ranged? 'active-range 'active))
         ((inactive inactive-range)
          (if ranged? 'inactive-range 'inactive))
         (t (error "Invalid timestamp type: %s" type)))))
    (om-elem--map-property :type #'update-range timestamp)))

(defun om-elem--timestamp-set-type (type timestamp)
  (let* ((range? (om-elem--timestamp-is-ranged-p timestamp))
         (type* (cl-case type
                  (active (if range? 'active-range 'active))
                  (inactive (if range? 'inactive-range 'inactive))
                  (t (error "Invalid timestamp type: %s" type)))))
    (om-elem--set-property :type type* timestamp)))

(defun om-elem--timestamp-set-warning (warning timestamp)
  (let ((types '(all first)))
    (-> (om-elem--decorator-format warning 'warning types)
        (om-elem--set-properties timestamp))))

(defun om-elem--timestamp-set-repeater (repeater timestamp)
  (let ((types '(catch-up restart cumulate)))
    (-> (om-elem--decorator-format repeater 'repeater types)
        (om-elem--set-properties timestamp))))

(defun om-elem--timestamp-shift-start (n unit timestamp)
  (let ((time* (->> (om-elem--timestamp-get-start-time timestamp)
                    (om-elem--time-shift n unit))))
    (->> (om-elem--timestamp-set-start-time time* timestamp)
         (om-elem--timestamp-update-type-ranged))))

(defun om-elem--timestamp-shift-end (n unit timestamp)
  (let ((time* (->> (om-elem--timestamp-get-end-time timestamp)
                    (om-elem--time-shift n unit))))
    (->> (om-elem--timestamp-set-end-time time* timestamp)
         (om-elem--timestamp-update-type-ranged))))

(defun om-elem--timestamp-shift-range (n unit timestamp)
  (->> (om-elem--timestamp-shift-start n unit timestamp)
       (om-elem--timestamp-shift-end n unit)))

(defun om-elem--timestamp-toggle-active (timestamp)
  (--> (om-elem--timestamp-is-active-p timestamp)
       (if it 'inactive 'active)
       (om-elem--timestamp-set-type it timestamp)))

;; timestamp (diary sexp)

(defun om-elem--timestamp-set-diary-sexp (form timestamp)
  (om-elem--verify form listp)
  (om-elem--set-property :raw-value (format "<%%%%%S>" form) timestamp))

;;; elements

;; clock

;; (defun om-elem--clock-map-timestamp (fun clock)
;;   (->> (om-elem--map-property :value fun clock)))

;; (om-elem--gen-anaphoric-form #'om-elem--clock-map-timestamp)

;; headline

(defconst om-elem--headline-title-restrictions
  (->> org-element-object-restrictions
       (alist-get 'headline)
       (cons 'plain-text)))

(defun om-elem--headline-set-title! (string stat-ckie headline)
  (let* ((ss (om-elem--build-secondary-string string)))
    (if (not stat-ckie)
        (om-elem--set-property-strict :title ss headline)
      (let ((ss* (om-elem--map-last*
                  (om-elem--set-property :post-blank 1 it) ss))
            (sc (om-elem-build-statistics-cookie stat-ckie)))
        (om-elem--set-property-strict :title (-snoc ss* sc) headline)))))

(defun om-elem--headline-shift-level (n headline)
  (om-elem--verify n integerp)
  (om-elem--map-property* :level (om-elem--shift-pos-integer n it)
                          headline))

;; item

(defun om-elem--item-is-unordered (item)
  (and (member (om-elem--get-property :bullet item) '("- " "+ ")) t))

;; NOTE the org element parser currently does not honor 1) or a) type
;; bullets
(defun om-elem--item-validate-counter (counter)
  (if (integerp counter) counter
    (let ((s (symbol-name counter)))
      (when (s-matches? "^[a-zA-z]$" s)
        (if org-list-allow-alphabetical counter
          (error "Set `org-list-allow-alphabetical' to t to use alphabetical bullets"))))))

(defconst om-elem--item-tag-restrictions
  (->> org-element-object-restrictions
       (alist-get 'item)
       (cons 'plain-text)))

(defun om-elem--item-set-tag! (raw-tag item)
  (-> (om-elem--build-secondary-string raw-tag)
      (om-elem--item-set-tag item)))

(defun om-elem--item-toggle-checkbox (item)
  (cl-case (om-elem--get-property :checkbox item)
    ((or trans nil) item)
    ('on (om-elem--set-property :checkbox 'off item))
    ('off (om-elem--set-property :checkbox 'on item))
    (t (error "This should not happen"))))

;; latex environment


;; planning

(defun om-elem--planning-list-to-timestamp (planning-list)
  (when planning-list
    (let* ((p (-partition-before-pred
               (lambda (it) (memq it '(&warning &repeater)))
               planning-list)))
      (om-elem-build-timestamp! 'inactive (car p)
                                :warning (alist-get '&warning p)
                                :repeater (alist-get '&repeater p)))))


;;; BUILDER FUNCTIONS

;; build helpers

(defconst om-elem--object-properties
  '(:begin :end :parent)
  "Minimum properties for objects.")

(defconst om-elem--recursive-object-properties
  (append om-elem--object-properties '(:contents-begin :contents-end))
  "Minimum properties for recursive objects.")

(defconst om-elem--element-properties
  (cons :post-affiliated om-elem--object-properties)
  "Minimum properties for elements.")

(defconst om-elem--container-element-properties
  (cons :post-affiliated om-elem--recursive-object-properties)
  "Minimum properties for container elements.")

(defun om-elem--init-properties (props)
  "Return a plist where the keys are PROPS and all values are nil."
  (--splice 't (list it nil) props))

(defun om-elem--build (type post-blank props)
  (->> (om-elem--set-property-strict :post-blank (or post-blank 0) `(,type nil))
       (om-elem--set-properties-nil props)))

(defun om-elem--build-object (type post-blank)
  (om-elem--build type post-blank om-elem--object-properties))

(defun om-elem--build-recursive-object (type post-blank objs)
  (->> om-elem--recursive-object-properties
       (om-elem--build type post-blank)
       (om-elem--set-contents-by-type type objs)))

(defun om-elem--build-element (type post-blank)
  (om-elem--build type post-blank om-elem--element-properties))

(defun om-elem--build-container-element (type post-blank elems)
  (->> om-elem--container-element-properties
       (om-elem--build type post-blank)
       (om-elem--set-contents-by-type type elems)))

;; define all builders using this automated monstrosity

(defun om-elem--kwd-to-sym (keyword)
  (->> (symbol-name keyword) (s-chop-prefix ":") (intern)))

(--each om-elem--type-alist
  (let* ((type (car it))
         (element? (memq type om-elem-elements))
         (name (intern (format "om-elem-build-%s" type)))
         (props (->> (cdr it)
                     (--remove (eq :post-blank (car it)))
                     (-non-nil)))
         (props (->> props
                     (--group-by
                      (-let (((&plist :require :set :const) (cdr it)))
                        (cond
                         (const 'const)
                         ((not set) 'null)
                         ((eq require t) 'req)
                         (t 'key))))))
         (pos-args (->> (alist-get 'req props)
                        (--map (om-elem--kwd-to-sym (car it)))))
         (kw-args (->> (alist-get 'key props)
                       (--map
                        (let ((prop (om-elem--kwd-to-sym (car it)))
                              (default (plist-get (cdr it) :require)))
                          (if default `(,prop ,default) prop)))))
         (rest-arg (cond
                    ((memq type om-elem-greater-elements) 'elems)
                    ((memq type om-elem-object-containers) 'objs)))
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
                        `(om-elem--set-property ,@it)
                      `(om-elem--set-properties (list ,@it)))))
         (nil-props
          (-some--> (alist-get 'null props)
                    (-map #'car it)
                    (if (= 1 (length it))
                        `(om-elem--set-property-nil ,@it)
                      `(om-elem--set-properties-nil (list ,@it)))))
         (strict-props
          (-some--> 
           (append (alist-get 'key props) (alist-get 'req props))
           (-map #'car it)
           (--mapcat (list it (om-elem--kwd-to-sym it)) it)
           (if (= 2 (length it))
               `(om-elem--set-property-strict ,@it)
             `(om-elem--set-properties-strict (list ,@it)))))
         (doc
          (let ((class (if element? "element" "object"))
                (end (if (not rest-arg) "."
                       (->> (symbol-name rest-arg)
                            (s-upcase)
                            (format " with %s as contents."))))
                (post-blank (if element? "newlines" "spaces"))
                (prop
                 (-some->>
                  (append (alist-get 'req props) (alist-get 'key props))
                  (--map (let ((p (->> (car it)
                                       (symbol-name)
                                       (s-chop-prefix ":")
                                       (s-upcase)))
                               (d (plist-get (cdr it) :type-desc)))
                           (unless d
                             (error "No type-desc: %s %s" type p))
                           (->> (if (listp d) (s-join " " d) d)
                                (format "- %s: %s" p))))
                  (s-join "\n"))))
            (concat
             ;; TODO use a/an here
             (format "Build a %s %s" type class) end
             "\n\nThe following properties are settable:\n"
             prop "\n- POST-BLANK: a non-negative integer")))
         (builder
          (let ((a `(',type post-blank)))
            (cond
             ((and element? rest-arg)
              `(om-elem--build-container-element ,@a ,rest-arg))
             (element?
              `(om-elem--build-element ,@a))
             (rest-arg
              `(om-elem--build-recursive-object ,@a ,rest-arg))
             (t
              `(om-elem--build-object ,@a)))))
         (body (if (or strict-props nil-props const-props)
                   `(->> ,@(-non-nil (list builder const-props
                                           nil-props strict-props)))
                 builder)))
    (eval `(om-elem--defun ,name ,args ,doc ,body))))

;; misc builders

(om-elem--defun om-elem-build-timestamp-diary-sexp (form &key post-blank)
  "Build a diary-sexp timestamp element from STRING.
STRING is a lisp form as a string."
  (->> (om-elem--build-object 'timestamp post-blank)
       (om-elem--set-property :type 'diary)
       (om-elem--timestamp-set-diary-sexp form)
       (om-elem--set-properties-nil
        (list :repeater-type :repeater-unit :repeater-value
              :warning-type :warning-unit :warning-value :year-start
              :month-start :day-start :hour-start :minute-start
              :year-end :month-end :day-end :hour-end :minute-end))))

(om-elem--defun om-elem-build-table-row-hline (&key post-blank)
  "Build a table-row element with the 'rule' type."
  (->> (om-elem--build-container-element 'table-row post-blank nil)
       (om-elem--set-property :type 'rule)))

;;; shortcut builders

(om-elem--defun om-elem-build-timestamp! (type start &key end
                                               repeater
                                               warning
                                               post-blank)
  "Build a timestamp object.

TYPE is one if 'active' or 'inactive' (the range suffix will be added
if an end time is supplied).

START specifies the start time and is a list of integers in one of 
the following forms:
- (year month day): short form
- (year month day nil nil) short form
- (year month day hour minute) long form

END (if supplied) will add the ending time, and follows the same 
formatting rules as START.

REPEATER and WARNING are lists formatted as (TYPE VALUE UNIT) where
the three members correspond to the :repeater/warning-type, -value,
and -unit properties in `om-elem-build-timestamp'.

Building a diary sexp timestamp is not possible with this function."
  (->> (om-elem--build-object 'timestamp post-blank)
       (om-elem--timestamp-set-start-time-nocheck start)
       (om-elem--timestamp-set-end-time-nocheck end)
       (om-elem--timestamp-set-type type)
       (om-elem--timestamp-set-warning warning)
       (om-elem--timestamp-set-repeater repeater)
       (om-elem--set-property-nil :raw-value)))

(om-elem--defun om-elem-build-clock! (start &key end post-blank)
  "Build a clock object.

START and END follow the same rules as their respective arguments in
`om-elem-build-timestamp!'."
  (let ((ts (om-elem-build-timestamp! 'inactive start :end end)))
    (om-elem-build-clock ts :post-blank post-blank)))

(om-elem--defun om-elem-build-planning! (&key closed deadline
                                              scheduled post-blank)
  "Build a planning element using shorthand arguments.
CLOSED, DEADLINE, and SCHEDULED are lists with the following structure
(brackets denote optional members):

'(year minute day [hour] [min]
  [&warning type value unit])
  [&repeater type value unit])'

In terms of arguments supplied to `om-elem-build-timestamp!', the
first five members correspond to the list supplied as TIME, and the
type/value/unit correspond to the lists supplied to WARNING and
REPEATER. The order of warning and repeater does not matter."
  (om-elem-build-planning
   :closed (om-elem--planning-list-to-timestamp closed)
   :deadline (om-elem--planning-list-to-timestamp deadline)
   :scheduled (om-elem--planning-list-to-timestamp scheduled)
   :post-blank post-blank))

(om-elem--defun om-elem-build-property-drawer! (&key post-blank &rest
                                                     keyvals)
  "Create a property drawer element.

Each member in KEYVALS is a list of symbols like (key val), where each
list will generate a node property in the property drawer like ':Key:
Val'."
  (->> keyvals
       (--map (let ((key (symbol-name (car it)))
                    (val (symbol-name (cadr it))))
                (om-elem-build-node-property key val)))
       (apply #'om-elem-build-property-drawer :post-blank post-blank)))

(om-elem--defun om-elem-build-headline! (&key (level 1) title-text
                                              todo-keyword tags
                                              pre-blank priority
                                              commentedp archivedp
                                              post-blank planning
                                              properties
                                              statistics-cookie
                                              section-contents
                                              &rest
                                              subheadlines)
  "Build a headline element.

TITLE-TEXT is a oneline string for the title of the headline.

PLANNING is a list like ('planning-type' 'args' ...) where
'planning-type' is one of :closed, :deadline, or :scheduled, and
'args' are the args supplied to any of the planning types in
`om-elem-build-planning!'. Up to all three planning types can be used
in the same list like (:closed args :deadline args :scheduled).

STATISTICS-COOKIE is a list following the same format as 
`om-elem-build-statistics-cookie'.

SECTION-CONTENTS is a list of elements that will go in the headline
section.

SUBHEADLINES contains zero or more headlines that will go under the
created headline.

All arguments not mentioned here follow the same rules as
`om-elem-build-headline'"
  (let* ((planning (-some->>
                    planning
                    (apply #'om-elem-build-planning!)))
         (property-drawer (-some->>
                           properties
                           (apply #'om-elem-build-property-drawer!)))
         (section (-some->>
                   (append `(,planning) `(,property-drawer) section-contents)
                   (-non-nil)
                   (apply #'om-elem-build-section)))
         ;; TODO need to ensure the all subheadlines are level + 1
         (elems (-non-nil (append (list section) subheadlines))))
    (->> (apply #'om-elem-build-headline
                :level level
                :post-blank post-blank
                :pre-blank pre-blank
                :priority priority
                :commentedp commentedp
                :archivedp archivedp
                elems)
         (om-elem--headline-set-title! title-text statistics-cookie))))

(om-elem--defun om-elem-build-item! (&key post-blank bullet checkbox
                                          tag paragraph counter
                                          &rest subitems)
  "Build an item element.

TAG is a string representing the tag.

PARAGRAPH is a string that will be the initial text in the item.

SUBITEMS contains the items that will go under this item.

All other arguments follow the same rules as `om-elem-build-item'."
  (let ((paragraph* (-some->> paragraph (om-elem-build-paragraph!)))
        (tag (-some->> tag (om-elem--build-secondary-string))))
    ;; TODO this restricts all subitems to plain lists...there are
    ;; other things we can put into lists
    (->> (apply #'om-elem-build-plain-list subitems)
         (list)
         (append (list paragraph*))
         (-non-nil)
         (apply #'om-elem-build-item
                :post-blank post-blank
                :bullet bullet
                :checkbox checkbox
                :counter counter
                :tag tag))))

(om-elem--defun om-elem-build-paragraph! (string &key post-blank)
  "Build a paragraph element.

STRING is the text to be parsed into a paragraph. It must contain valid
formatting (eg, text that will be formatted into objects)."
  ;; TODO this can be simplified?
  (let ((p (->> (om-elem--from-string string)
                (om-elem--get-nested-contents '(0)))))
    (if (om-elem--is-type-p 'paragraph p)
        (om-elem--set-property-strict :post-blank (or post-blank 0) p)
      (error "String could not be parsed to a paragraph: %s" string))))

(om-elem--defun om-elem-build-table! (&key tblfm post-blank &rest row-lists)
  "Build a table element.

ROW-LISTS is a list of lists where each member is a string to be put
in a table cell or the symbol 'hline' which represents a horizontal
line.

All other arguments follow the same rules as `om-elem-build-table'."
  (cl-flet
      ((convert
        (r)
        (cond
         ((listp r)
          (->> r
               (-map #'om-elem-build-table-cell)
               (apply #'om-elem-build-table-row)))
         ((eq r 'hline) (om-elem-build-table-row-hline))
         (t (error "Unknown table row %s" r)))))
    (->> (-map #'convert row-lists)
         (apply #'om-elem-build-table
                :tblfm tblfm
                :post-blank post-blank))))

;;; INTERNAL CONTENT FUNCTIONS
;; operations on contents of containers

;; generic

(defalias 'om-elem--get-contents 'org-element-contents)

(defun om-elem--get-nested-contents (indices elem)
  "Return the nested contents of ELEM as given by INDICES.
INDICES is a list of integers specifying the index and level of the
nested element to return."
  (if (not indices) elem
    (->> (om-elem--get-contents elem)
         (nth (car indices))
         (om-elem--get-nested-contents (cdr indices)))))

(defun om-elem--get-head (elem)
  "Return the type and properties cells of ELEM."
  (if (stringp elem) elem
    (-take 2 elem)))

(defun om-elem--is-empty-p (elem)
  "Return t if ELEM has no contents."
  (not (om-elem--get-contents elem)))

(defun om-elem--map-contents (fun elem)
  (let ((contents (om-elem--get-contents elem)))
    (om-elem--set-contents (funcall fun contents) elem)))

(om-elem--gen-anaphoric-form #'om-elem--map-contents)

(defun om-elem--map-contained (pred fun elem)
  (om-elem--map-contents*
   (--map-when (funcall pred it) (funcall fun it) it)
   elem))

(defun om-elem--map-contained-first (pred fun elem)
  (om-elem--map-contents*
   (--map-first (funcall pred it) (funcall fun it) it)
   elem))

(defun om-elem--set-contents (contents elem)
   (let ((head (om-elem--get-head elem)))
     (if contents (append head contents) head)))

(defun om-elem--set-contents-restricted (types contents elem)
  ;; TODO this should recursively dig up all types in contents
  ;; even if they are nested
  (-when-let (illegal (-some->> (-map #'om-elem--get-type contents)
                                (--remove (memq it types))
                                (-map #'symbol-name)
                                (s-join ", ")))
    (error "Illegal types found: %s; allowed types are: %s"
           illegal (s-join ", " (-map #'symbol-name types))))
  (om-elem--set-contents contents elem))

(defun om-elem--set-contents-by-type (container-type contents elem)
  ;; TODO there may be additional restrictions, such as newlines
  ;; in strings not being allowed
  (-if-let (types (alist-get container-type om-elem-restrictions))
      (om-elem--set-contents-restricted types contents elem)
    (error "Invalid container type requested: %s" container-type)))

;; headline

(defun om-elem--headline-get-subheadlines (headline)
  (-some->> (om-elem--get-contents headline)
            (--filter (om-elem-is-headline-p it))))

(defun om-elem--headline-get-section (headline)
  (-some->> (om-elem--get-contents headline) (assoc 'section)))

(defun om-elem--headline-get-statistics-cookie (headline)
  (->> (om-elem--get-property :title headline)
       (-last-item)
       (om-elem--filter-type 'statistics-cookie)))

(defun om-elem--headline-get-drawer (name headline)
  "Return first drawer with NAME in HEADLINE element or nil if none."
  (om-elem--verify name stringp)
  (-some->>
   (om-elem--headline-get-section headline)
   (--first (and (om-elem--is-type-p 'drawer it)
                 (om-elem--property-is-equal-p :drawer-name name it)))))

(defun om-elem--headline-get-properties-drawer (headline)
  (-some->>
   (om-elem--headline-get-section headline)
   (--first (om-elem--is-type-p 'property-drawer it))))

(defun om-elem--headline-get-node-properties (headline)
  (-some->>
   (om-elem--headline-get-properties-drawer headline)
   (--filter (om-elem--is-type-p 'node-property it))))

(defun om-elem--headline-get-planning (headline)
  (-some->> (om-elem--headline-get-section headline)
            (om-elem--get-contents)
            (--first (om-elem--is-type-p 'planning it))))

(defun om-elem--headline-get-path (headline)
  "Return path of headline HEADLINE element as a list of strings."
  (cl-labels
      ((get-path
        (hl)
        (let ((title (om-elem--get-property :raw-value hl)))
          (-if-let (parent (om-elem--get-parent-headline hl))
              (cons title (get-path parent))
            (list title)))))
    (reverse (get-path headline))))

(defun om-elem--headline-map-subheadlines (fun headline)
  (om-elem--map-contents
   (lambda (contents)
     (let ((section (assoc 'section contents))
           (subheadlines (-some->>
                          (-filter #'om-elem-is-headline-p contents)
                          (funcall fun))))
       (cond
        ((and section subheadlines) (cons section subheadlines))
        (section section)
        (t subheadlines))))
   headline))

(defun om-elem-headline-map-node-property (key fun headline)
  (om-elem--verify key stringp headline om-elem-is-headline-p)
  (om-elem-match-map-first*
   `(section property-drawer (:and node-property (:key ,key)))
    (om-elem--node-property-map-value fun it) headline))

(defun om-elem--map-or-build (map-fun build-fun pred-fun pos elem)
  (om-elem--map-contents
   (lambda (contents)
     (-if-let (target (--first (funcall pred-fun it) contents))
         ;; TODO this is probably not the most efficient
         (-replace target (funcall map-fun target) contents)
       (let ((pos
              (cond
               ((integerp pos)
                pos)
               ((functionp pos)
                (or (--find-index (funcall pos it) contents) 0))
               (t (error "Invalid pos given: %S" pos))))
             (new (funcall build-fun)))
         (-insert-at pos new contents))))
   elem))

(defmacro om-elem--map-or-build-nested (map-form &rest args)
  ;; forms are of form (pred builder pos-fun)
  (declare (indent 1))
  (let* ((elem (-last-item args))
         (forms (-drop-last 1 args))
         (first (car args))
         (rem (cdr forms))
         (pred-fun `(lambda (it) ,(nth 0 first)))
         (build-fun `(lambda () (->> ,@(nreverse (--map (nth 1 it) forms)))))
         (pos-fun `(lambda (it) ,(nth 2 first)))
         (map-fun
          (if (not rem) `(lambda (it) ,map-form)
            `(lambda (inner)
               (om-elem--map-or-build-nested ,map-form ,@rem inner)))))
    `(om-elem--map-or-build ,map-fun ,build-fun ,pred-fun ,pos-fun ,elem)))

(defun om-elem--headline-subtree-shift-level (n headline)
  (->> (om-elem--headline-shift-level n headline)
       (om-elem--headline-map-subheadlines
        (lambda (headlines)
          (--map (om-elem--headline-subtree-shift-level n it)
                 headlines)))))

(defun om-elem--headline-set-section (section headline)
  (let ((subheadlines (om-elem-headline-get-subheadlines headline)))
    (om-elem--set-contents (cons section subheadlines) headline)))

(defun om-elem--headline-set-property-drawer (property-drawer headline)
  (om-elem--headline-set-section (om-elem-build-section property-drawer)))

(defun om-elem--headline-set-node-property (key value headline)
  (om-elem--map-or-build-nested (om-elem--set-property-strict :value value it)
    ((om-elem-is-section-p it)
     (om-elem-build-section)
     0)
    ((om-elem-is-property-drawer-p it)
     (om-elem-build-property-drawer)
     (-if-let (i (-find-index (om-elem-is-planning-p it) it)) ((1+ i) it)))
    ((and (om-elem-is-node-property-p it)
          (om-elem--property-is-equal-p :value key it))
     (om-elem-build-node-property key value) 
     0)
    headline))

(defun om-elem--headline-set-planning (planning headline)
  ;; TODO what if we give this a nil?
  (om-elem--map-or-build-nested (om-elem--set-property-strict :value value it)
    ((om-elem-is-section-p it) (om-elem-build-section) 0)
    ((om-elem-is-planning-p it) (apply #'om-elem-build-planning planning) 0)
    headline))

(defun om-elem--headline-set-statistics-cookie (value headline)
  (om-elem--map-property*
   :title
   (let ((last? (om-elem--is-type-p 'statistics-cookie (-last-item it))))
     (cond
      ((and last? value)
       (om-elem--map-last* (om-elem--statistics-cookie-set-value value) it))
      ((and last? (not value))
       (-drop-last 1 it))
      (value 
       (-snoc it (om-elem-build-statistics-cookie value)))
      (t it)))
   headline))

(defun om-elem--headline-set-statistics-cookie-fraction (done total headline)
  (om-elem--verify headline om-elem-is-headline-p)
  (let* ((format (->>
                  (om-elem--headline-get-statistics-cookie headline)
                  (om-elem--statistics-cookie-get-format)))
         (value (if (eq 'percent format) `(done total)
                  (-> (float done)
                      (/ total)
                      (* 100)
                      (round)
                      (list)))))
    (om-elem--headline-set-statistics-cookie value)))


;; item

(defun om-elem--item-get-level (item)
  "Return the level of ITEM element item (1 indexed)."
  (cl-labels
      ((get-level
        (item acc)
        (let ((parent (->> (om-elem--get-property :parent item)
                           (om-elem--get-property :parent))))
          (if (om-elem-item-p parent)
              (get-level parent (+ 1 acc))
            acc))))
    (get-level item 1)))

(defun om-elem--item-get-sublist (item)
  "Return plain-list under ITEM element or nil if none."
  (-some->> (om-elem--get-contents item) (assoc 'plain-list)))

(defun om-elem--item-get-paragraph (item)
  "Return paragraph under ITEM element or nil if none."
  (-some->> (om-elem--get-contents item) (assoc 'paragraph)))

;; table

(defun om-elem--table-pad-or-truncate (length list)
  (let ((pad (om-elem-build-table-cell "")))
    (om-elem--pad-or-truncate length pad list)))

;; TODO if I feel like being super extra I can add pivot operators :)

(defun om-elem--table-delete-column (index table)
  (om-elem--verify index integerp)
  (cl-flet*
      ((delete-cell
        (cells)
        (om-elem--remove-at index cells))
       (map-row 
        (row)
        (if (om-elem--property-is-eq-p :type 'rule row) row
          (om-elem--map-contents #'delete-cell row))))
    (om-elem--map-contents* (-map #'map-row it) table)))

(defun om-elem--table-delete-row (index table)
  (om-elem--verify index integerp)
  (om-elem--map-contents* (om-elem--remove-at index it) table))

(defun om-elem--column-map-down-rows (fun column table)
  (cl-flet*
      ((zip-into-rows
        (row new-cell)
        (if (om-elem--property-is-eq-p :type 'rule row) row
          (om-elem--map-contents
           ;; (lambda (cells) (om-elem--insert-at index new-cell cells))
           (lambda (cells) (funcall fun new-cell cells))
           row)))
       (map-rows
        (rows)
        (->> rows
             (--find-indices (om-elem--property-is-eq-p :type 'rule it))
             (--reduce-from (-insert-at it nil acc) column)
             (om-elem--table-pad-or-truncate (length rows))
             (-zip-with #'zip-into-rows rows))))
    (om-elem--map-contents #'map-rows table)))

(defun om-elem--table-insert-column (index column table)
  (om-elem--verify index integerp)
  (om-elem--column-map-down-rows
   (lambda (new-cell cells) (om-elem--insert-at index new-cell cells))
   column
   table))

(defun om-elem--table-insert-row (index row table)
  (om-elem--verify index integerp)
  ;; (print (om-elem-to-string row))
  (let ((row (if (om-elem--property-is-eq-p :type 'rule row) row
               (let ((width (om-elem--table-get-width table)))
                 (om-elem--table-pad-or-truncate width row)))))
    ;; (print (om-elem-to-string row))
    (om-elem--map-contents* (om-elem--insert-at index (apply #'om-elem-build-table-row row) it) table)))

(defun om-elem--table-get-column (column table)
  (-some->> (om-elem--get-contents table)
            (--filter (om-elem--property-is-eq-p :type 'standard it))
            (--map (->> (om-elem--get-contents it)
                        (om-elem--nth column)))))

(defun om-elem--table-get-row (row table)
  (-some->> (om-elem--get-contents table)
            (--filter (om-elem--property-is-eq-p :type 'standard it))
            (om-elem--nth row)))

(defun om-elem--table-get-cell (row column table)
  "Return table-cell element at ROW and COLUMN indices in TABLE element.
Hlines do not count toward row indices, and all indices are
zero-indexed."
  (-some->> (om-elem--table-get-row row table)
            (om-elem--get-contents)
            (om-elem--nth column)))

(defun om-elem--table-replace-column (index column table)
  (om-elem--verify index integerp)
  (om-elem--column-map-down-rows
   (lambda (new-cell cells) (om-elem--replace-at index new-cell cells))
   column
   table))

;; TODO this is not dry...
(defun om-elem--table-replace-row (index row table)
  (om-elem--verify index integerp)
  (let ((row (if (om-elem--property-is-eq-p :type 'rule row) row
               (let ((width (om-elem--table-get-width table)))
                 (om-elem--map-contents*
                  (om-elem--table-pad-or-truncate width it) row)))))
    (om-elem--map-contents* (om-elem--replace-at index row it) table)))

(defun om-elem--table-replace-cell (row-index column-index cell table)
  (let ((row (->> (om-elem--table-get-row row-index table)
                  (om-elem--map-contents*
                   (om-elem--replace-at column-index cell it)))))
    (om-elem--table-replace-row row-index row table)))

(defun om-elem--table-clear-cell (row-index column-index table)
  (om-elem--table-replace-cell row-index column-index table))

;; TODO make replace/clear cell

(defun om-elem--table-clear-row (index table)
  ;; this assumes the blank cell will be padded with other blank cells
  (om-elem--table-replace-row index (om-elem-build-table-row (om-elem-build-table-cell " ")) table))

(defun om-elem--table-clear-column (index table)
  ;; this assumes the blank cell will be padded with other blank cells
  (om-elem--table-replace-column index (list (om-elem-build-table-cell "")) table))

(defun om-elem--table-get-height (table)
  (length (om-elem--get-contents table)))

(defun om-elem--table-get-width (table)
  (->> (om-elem--get-contents table)
       (--map (length (om-elem--get-contents it)))
       (-max)))

;;; INTERNAL INDENTATION

;;; helper functions

;; TODO this is a bit sketchy...it depends on the indentation
;; function to make the contents list one element shorter, which
;; is usually true but makes a really hard error to catch when it
;; fails
(defun om-elem--indent-after (indent-fun index elem)
  (unless (and (integerp index) (<= 0 index))
    (error "Index must be non-negative integer"))
  (if (< index (1- (length (om-elem--get-contents elem))))
      (->> (funcall indent-fun (1+ index) elem)
           (om-elem--indent-after indent-fun index))
    elem))

(defun om-elem--indent-members (fun index members)
  (unless (and (integerp index) (< 0 index))
    (error "Cannot indent topmost item at this level"))
  (-let* (((head tail) (-split-at index members))
          (target (-first-item tail))
          (head* (om-elem--map-last* (funcall fun target it) head)))
    (append head* (-drop 1 tail))))

(defun om-elem--unindent-members (index parent-fun unindent-fun list)
  (unless (and (integerp index) (<= 0 index))
    (error "Index must be non-negative integer"))
  (-let* (((head tail) (-split-at index list))
          (parent (-first-item tail))
          (parent* (funcall parent-fun parent))
          (unindented (funcall unindent-fun parent)))
    (append head (list parent*) unindented (-drop 1 tail))))

;;; elements functions

;; headline

(defun om-elem--headline-indent-subtree (index headline)
  (cl-flet
      ((append-indented
        (target-headline parent-headline)
        (let ((target-headline*
               (om-elem--headline-subtree-shift-level 1 target-headline)))
          (om-elem--map-contents
           (lambda (headline-contents)
             (append headline-contents (list target-headline*)))
           parent-headline))))
    (om-elem--headline-map-subheadlines
     (lambda (subheadlines)
       (om-elem--indent-members #'append-indented index subheadlines))
     headline)))

(defun om-elem--headline-indent-subheadline (index headline)
  (cl-flet
      ((append-indented
        (target-headline parent-headline)
        (let ((target-headline*
               (->> target-headline
                    (om-elem--headline-map-subheadlines #'ignore)
                    (om-elem--headline-shift-level 1)))
              (headlines-in-target
               (om-elem--headline-get-subheadlines target-headline))) 
          (om-elem--map-contents
           (lambda (contents)
             (append contents (list target-headline*) headlines-in-target))
           parent-headline))))
    (om-elem--headline-map-subheadlines
     (lambda (subheadlines)
       (om-elem--indent-members #'append-indented index subheadlines))
     headline)))

(defun om-elem--headline-unindent-subheadline (index child-index headline)
  (cl-flet
      ((trim
        (parent)
        (om-elem--headline-map-subheadlines
         (lambda (subheadlines) (-take child-index subheadlines))
         parent))
       (extract
        (parent)
        (->> (om-elem--indent-after #'om-elem-headline-indent-subtree
                                    child-index parent)
             (om-elem--get-contents)
             (-drop child-index)
             (--map (om-elem--headline-subtree-shift-level -1 it)))))
    (om-elem--headline-map-subheadlines
     (lambda (subheadlines)
       (om-elem--unindent-members index #'trim #'extract subheadlines))
     headline)))

(defun om-elem--headline-unindent-subtree (index headline)
  (cl-flet
      ((trim
        (parent)
        (om-elem--headline-map-subheadlines #'ignore parent))
       (extract
        (parent)
        (->> (om-elem--get-contents parent)
             (--map (om-elem--headline-subtree-shift-level -1 it)))))
    (om-elem--headline-map-subheadlines
     (lambda (subheadlines)
       (om-elem--unindent-members index #'trim #'extract subheadlines))
     headline)))

;; plain-list

(defun om-elem--plain-list-indent-item-tree (index plain-list)
  (cl-flet
      ((append-indented
        (target-item parent-item)
        (let ((target-item* (om-elem-build-plain-list target-item)))
          (om-elem--map-contents
           (lambda (item-contents) (append item-contents (list target-item*)))
           parent-item))))
    (om-elem--map-contents
     (lambda (items)
       (om-elem--indent-members #'append-indented index items))
     plain-list)))

(defun om-elem--plain-list-indent-item (index plain-list)
  (cl-flet
      ((append-indented
        (target-item parent-item)
        (let ((target-item*
               (->> target-item
                    (om-elem--map-contents*
                     (-remove #'om-elem-is-plain-list-p it))
                    (om-elem-build-plain-list)))
              (items-in-target
               (->> (om-elem--get-contents target-item)
                    (-filter #'om-elem-is-plain-list-p))))
          (om-elem--map-contents
           (lambda (item-contents)
             ;; TODO technically the target-item* should go in an
             ;; existing plain list but I don't this matters (for now)
             (append item-contents (list target-item*) items-in-target))
           parent-item))))
    (om-elem--map-contents
     (lambda (items)
       (om-elem--indent-members #'append-indented index items))
     plain-list)))

(defun om-elem--plain-list-unindent-item (index child-index plain-list)
  (cl-flet
      ((trim
        (parent)
        (om-elem--map-contents
         (lambda (contents)
           (if (= 0 index)
               (-remove-first #'om-elem-is-plain-list-p contents)
             (--map-first (om-elem-is-plain-list-p it)
                          (om-elem--map-contents
                           (lambda (items) (-take child-index items)) it)
                          contents)))
         parent))
       (extract
        (parent)
        (->>
         (om-elem--get-contents parent)
         (-first #'om-elem-is-plain-list-p)
         (om-elem--indent-after #'om-elem--plain-list-indent-item-tree
                                child-index)
         (om-elem--get-contents)
         (-drop child-index))))
    (om-elem--map-contents
     (lambda (items)
       (om-elem--unindent-members index #'trim #'extract items))
     plain-list)))

(defun om-elem--plain-list-unindent-items (index plain-list)
  (cl-flet
      ((trim
        (parent)
        (om-elem--map-contents
         (lambda (contents)
           (-remove-first #'om-elem-is-plain-list-p contents))
         parent))
       (extract
        (parent)
        (->> (om-elem--get-contents parent)
             (-first #'om-elem-is-plain-list-p)
             (om-elem--get-contents))))
    (om-elem--map-contents
     (lambda (items)
       (om-elem--unindent-members index #'trim #'extract items))
     plain-list)))

;;; PUBLIC TYPE FUNCTIONS

(defun om-elem-is-type-p (type elem)
  "Return t if the type of ELEM is TYPE (a symbol)."
  (om-elem--verify elem om-elem--is-element-or-object-p)
  (om-elem--is-type-p type elem))

(defun om-elem-is-any-type-p (types elem)
  "Return t if the type of ELEM is in TYPES (a list of symbols)."
  (om-elem--verify elem om-elem--is-element-or-object-p)
  (om-elem--is-any-type-p types elem))

(defun om-elem-is-element-p (elem)
  "Return t if ELEM is an element type."
  (om-elem--verify elem om-elem--is-element-or-object-p)
  (om-elem--is-any-type-p om-elem-elements elem))

(defun om-elem-is-container-p (elem)
  "Return t if ELEM is a container.
Containers are elements or objects that may contain other elements
or objects."
  (om-elem--verify elem om-elem--is-element-or-object-p)
  (om-elem--is-any-type-p om-elem-containers elem))

(defun om-elem-is-object-container-p (elem)
  "Return t if ELEM is an object container.
Object containers are elements or objects that may contain objects."
  (om-elem--verify elem om-elem--is-element-or-object-p)
  (om-elem--is-any-type-p om-elem-object-containers elem))

(defun om-elem-is-greater-element-p (elem)
  "Return t if ELEM is a greater element.
Greater elements are elements that may contain other elements."
  (om-elem--verify elem om-elem--is-element-or-object-p)
  (om-elem--is-any-type-p om-elem-greater-elements elem))

;;; PUBLIC PROPERTY FUNCTIONS

(defun om-elem--append-documentation (fun string)
  (--> (documentation fun)
       (concat it "\n" string)
       (function-put fun 'function-documentation it)))

(defun om-elem--get-type-alist-operation (op)
  (->> om-elem--type-alist
       (--map (cons (car it) (--filter (plist-get (cdr it) op) (cdr it))))
       (-filter #'cdr)))

(defun om-elem--format-alist-operations (ops)
  (->> ops
       (--map (cons (car it) (-map #'car (cdr it))))
       (--map (format "\n%s\n%s"
                      (car it)
                      (s-join "\n" (--map (format "- %S" it) (cdr it)))))
       (s-join "\n")))

;;; polymorphic

;; set

(defun om-elem-set-property (prop value elem)
  "Set property PROP to VALUE in ELEM.

See builder functions for a list of properties and their rules for
each type."
  (om-elem--set-property-strict prop value elem))

;; (->> (om-elem--get-type-alist-operation :set)
;;      (om-elem--format-alist-operations)
;;      (om-elem--append-documentation 'om-elem-set-property))

(defun om-elem-set-properties (plist elem)
  "Set all properties in ELEM to the values corresponding to PLIST.
PLIST is a list of property-value pairs that corresponds to the
property list in ELEM.

See builder functions for a list of properties and their rules for
each type."
  (om-elem--verify elem om-elem--is-element-or-object-p)
  (om-elem--set-properties-strict plist elem))

;; get

(defun om-elem-get-property (prop elem)
  "Return the value or property PROP in ELEM.

See builder functions for a list of properties and their rules for
each type."
  (om-elem--get-property-strict prop elem))

;; TODO add plural version of this...

;; map

(defun om-elem-map-property (prop fun elem)
  "Apply function FUN to the value of property PROP in ELEM.
FUN takes one argument (the current value of PROP) and returns
a new value to which PROP will be set.

See builder functions for a list of properties and their rules for
each type."
  (om-elem--map-property-strict prop fun elem))

(om-elem--gen-anaphoric-form #'om-elem-map-property)

(defun om-elem-map-properties (plist elem)
  "Alter the values of properties in place within ELEM.
PLIST is a property list where the keys are properties in ELEM and
its values are functions to be mapped to these properties.

See builder functions for a list of properties and their rules for
each type."
  (om-elem--verify elem om-elem--is-element-or-object-p)
  (om-elem--map-properties-strict plist elem))

(defmacro om-elem-map-properties* (plist elem)
  "Anaphoric form of `om-elem-map-properties'.
PLIST is a property list where the keys are properties in ELEM and
its values are forms to be mapped to these properties."
  `(let ((plist* (om-elem--plist-map-values (lambda (form) `(lambda (it) ,form)) ',plist)))
    (om-elem--map-properties-strict plist* ,elem)))

;; toggle

(defun om-elem-toggle-property (prop elem)
  "Flip the value of property PROP in ELEM.
This function only applies to properties that are booleans.

The following elements and properties are supported:"
  (let* ((type (om-elem--get-type elem))
         (flag (om-elem--get-strict-function :toggle type prop)))
    (if flag
        (om-elem--map-property-strict prop #'not elem)
      (error "Not a toggle-able property"))))

(->> (om-elem--get-type-alist-operation :toggle)
     (om-elem--format-alist-operations)
     (om-elem--append-documentation 'om-elem-toggle-property))

;; shift

(defun om-elem-shift-property (prop n elem)
  "Shift property PROP by N (an integer) units within ELEM.
This only applies the properties that are represented as integers.

The following elements and properties are supported:"
  (let* ((type (om-elem--get-type elem))
         (fun (om-elem--get-strict-function :shift type prop)))
    (if fun
        (om-elem--map-property-strict* prop (funcall fun n it) elem)
      (error "Not a shiftable property"))))

(->> (om-elem--get-type-alist-operation :shift)
     (--map (cons (car it) (--remove (eq :post-blank (car it)) (cdr it))))
     (-filter #'cdr)
     (om-elem--format-alist-operations)
     (concat "\nall elements\n- :post-blank\n")
     (om-elem--append-documentation 'om-elem-shift-property))

;; insert

(defun om-elem-insert-into-property (prop index string elem)
  "Insert STRING into PROP at INDEX within ELEM if it is not already there.
This only applies to properties that are represented as lists of strings.

The following elements and properties are supported:"
  (cl-flet
      ((insert-at-maybe
        (string-list)
        (if (member string string-list) string-list
          (om-elem--insert-at index string string-list))))
    (let* ((type (om-elem--get-type elem))
           (flag (om-elem--get-strict-function :string-list type prop)))
      (if flag
          (om-elem--map-property-strict prop #'insert-at-maybe elem)
        (error "Property '%s' in elem of type '%s' is not a string-list"
               prop type)))))

(->> (om-elem--get-type-alist-operation :string-list)
     (om-elem--format-alist-operations)
     (om-elem--append-documentation 'om-elem-insert-into-property))

;; remove

(defun om-elem-remove-from-property (prop string elem)
  "Remove string STRING from list PROP within ELEM.
This only applies to properties that are represented as lists of 
strings.

See `om-elem-insert-into-property' for a list of supported elements
and properties that may be used with this function."
  (let* ((type (om-elem--get-type elem))
         (flag (om-elem--get-strict-function :string-list type prop)))
    (if flag
        (om-elem--map-property-strict* prop (-remove-item string it) elem)
      (error "Property '%s' in elem of type '%s' is not a string-list"
             prop type))))

;; plist-put

(defun om-elem-plist-put-property (prop key value elem)
  "Insert KEY and VALUE pair into PROP within ELEM.
KEY is a keyword and VALUE is a symbol. This only applies to 
properties that are represented as plists.

The following elements and properties are supported:."
  (let* ((type (om-elem--get-type elem))
         (flag (om-elem--get-strict-function :plist type prop)))
    (if flag
        (om-elem--map-property-strict* prop (plist-put it key value) elem)
      (error "Not a plist property"))))

(->> (om-elem--get-type-alist-operation :plist)
     (om-elem--format-alist-operations)
     (om-elem--append-documentation 'om-elem-plist-put-property))

;; plist-remove

(defun om-elem-plist-remove-property (prop key elem)
  "Remove KEY and its value from PROP within ELEM.
KEY is a keyword. This only applies to properties that are
represented as plists.

See `om-elem-plist-put-property' for a list of supported elements
and properties that may be used with this function."
  (let* ((type (om-elem--get-type elem))
         (flag (om-elem--get-strict-function :plist type prop)))
    (if flag
        (om-elem--map-property-strict* prop (om-elem--plist-remove key it) elem)
      (error "Not a plist property"))))

;;; generic

;; TODO do these even work?


;;; objects
;;
;; statistics-cookie

;; TODO make a test for this?
(defun om-elem-headline-get-statistics-cookie (headline)
  "Return the statistics cookie object from HEADLINE if it exists."
  (om-elem--verify headline om-elem-is-headline-p)
  (om-elem--headline-get-statistics-cookie headline))

(defun om-elem-statistics-cookie-is-complete-p (statistics-cookie)
  "Return t is STATISTICS-COOKIE element is complete."
  (om-elem--verify statistics-cookie om-elem-is-statistics-cookie-p)
  (om-elem--statistics-cookie-is-complete-p statistics-cookie))

;; timestamp

;; (defun om-elem-timestamp-get-start-timestamp (timestamp)
;;   "Return the start of TIMESTAMP as a timestamp element.
;; If not a range, this will simply return TIMESTAMP unmodified."
;;   (om-elem--verify timestamp om-elem-is-timestamp-p)
;;   (om-elem--timestamp-get-start-timestamp timestamp))

;; (defun om-elem-timestamp-get-end-timestamp (timestamp)
;;   "Return the end of TIMESTAMP as a timestamp element.
;; If not a range, return nil."
;;   (om-elem--verify timestamp om-elem-is-timestamp-p)
;;   (and (om-elem--timestamp-is-ranged-fast-p timestamp)
;;        (om-elem--timestamp-get-end-timestamp timestamp)))

(defun om-elem-timestamp-get-start-time (timestamp)
  "Return the time list of TIMESTAMP or start time if a range.
The return value will be a list as specified by the TIME argument in
`om-elem-build-timestamp!'."
  (om-elem--verify timestamp om-elem-is-timestamp-p)
  (om-elem--timestamp-get-start-time timestamp))

(defun om-elem-timestamp-get-end-time (timestamp)
  "Return the end time list of TIMESTAMP end or nil if not a range.
The return value will be a list as specified by the TIME argument in
`om-elem-build-timestamp!'."
  (om-elem--verify timestamp om-elem-is-timestamp-p)
  (and (om-elem--timestamp-is-ranged-fast-p timestamp)
       (om-elem--timestamp-get-end-time timestamp)))

(defun om-elem-timestamp-get-range (timestamp)
  "Return the range of TIMESTAMP in seconds as an integer.
If non-ranged, this function will return 0. If ranged but
the start time is in the future relative to end the time, return
a negative integer."
  (om-elem--timestamp-get-range timestamp))

(defun om-elem-timestamp-is-active-p (timestamp)
  "Return t if TIMESTAMP is active."
  (om-elem--verify timestamp om-elem-is-timestamp-p)
  (or (om-elem--property-is-eq-p :type 'active timestamp)
      (om-elem--property-is-eq-p :type 'active-range timestamp)))

;; (defun om-elem-timestamp-is-inactive-p (timestamp)
;;   "Return t if TIMESTAMP elem is inactive."
;;   (om-elem--verify timestamp om-elem-is-timestamp-p)
;;   (or (om-elem--property-is-eq-p :type 'inactive timestamp)
;;       (om-elem--property-is-eq-p :type 'inactive-range timestamp)))

(defun om-elem-timestamp-is-ranged-p (timestamp)
  "Return t if TIMESTAMP is ranged."
  (om-elem--verify timestamp om-elem-is-timestamp-p)
  (or (om-elem--property-is-eq-p :type 'active-range timestamp)
      (om-elem--property-is-eq-p :type 'inactive-range timestamp)))

;; TODO not sure how I feel about these :(
;; (defun om-elem-timestamp-start-is-less-than-p (unixtime timestamp)
;;   "Return t if TIMESTAMP start time is less than UNIXTIME."
;;   (om-elem--verify timestamp om-elem-is-timestamp-p)
;;   (om-elem--timestamp-start-is-less-than-p unixtime timestamp))

;; (defun om-elem-timestamp-start-is-greater-than-p (unixtime timestamp)
;;   "Return t if TIMESTAMP start time is greater than UNIXTIME."
;;   (om-elem--verify timestamp om-elem-is-timestamp-p)
;;   (om-elem--timestamp-start-is-greater-than-p unixtime timestamp))

;; (defun om-elem-timestamp-start-is-equal-to-p (unixtime timestamp)
;;   "Return t if TIMESTAMP start time is equal to UNIXTIME."
;;   (om-elem--verify timestamp om-elem-is-timestamp-p)
;;   (om-elem--timestamp-start-is-equal-to-p unixtime timestamp))

;; TODO what happens if not a range?
;; (defun om-elem-timestamp-end-is-less-than-p (unixtime timestamp)
;;   "Return t if TIMESTAMP end time is less than UNIXTIME."
;;   (om-elem--verify timestamp om-elem-is-timestamp-p)
;;   (om-elem--timestamp-end-is-less-than-p unixtime timestamp))

;; (defun om-elem-timestamp-end-is-greater-than-p (unixtime timestamp)
;;   "Return t if TIMESTAMP end time is greater than UNIXTIME."
;;   (om-elem--verify timestamp om-elem-is-timestamp-p)
;;   (om-elem--timestamp-end-is-greater-than-p unixtime timestamp))

;; (defun om-elem-timestamp-end-is-equal-to-p (unixtime timestamp)
;;   "Return t if TIMESTAMP end time is equal to UNIXTIME."
;;   (om-elem--verify timestamp om-elem-is-timestamp-p)
;;   (om-elem--timestamp-end-is-equal-to-p unixtime timestamp))

(defun om-elem-timestamp-range-contains-p (unixtime timestamp)
  "Return t if UNIXTIME is between start and end of TIMESTAMP elem."
  (om-elem--verify timestamp om-elem-is-timestamp-p)
  (let ((ut1 (om-elem--timestamp-get-start-unixtime timestamp))
        (ut2 (om-elem--timestamp-get-end-unixtime timestamp)))
    (< ut1 unixtime ut2)))

(defun om-elem-timestamp-set-start-time (time timestamp)
  "Set start time of TIMESTAMP element to TIME.
TIME is a list analogous to the same argument specified in
`om-elem-build-timestamp!'."
  (om-elem--verify timestamp om-elem-is-timestamp-p)
  (om-elem--timestamp-set-start-time time timestamp))

(defun om-elem-timestamp-set-end-time (time timestamp)
  "Set end time of TIMESTAMP element to TIME.
TIME is a list analogous to the same argument specified in
`om-elem-build-timestamp!'."
  (om-elem--verify timestamp om-elem-is-timestamp-p)
  (om-elem--timestamp-set-end-time time timestamp))

(defun om-elem-timestamp-set-single-time (time timestamp)
  "Set start time of TIMESTAMP to TIME, and remove the end time.
TIME is a list analogous to the same argument specified in
`om-elem-build-timestamp!'."
  (om-elem--verify timestamp om-elem-is-timestamp-p)
  (om-elem--timestamp-set-single-time time timestamp))

(defun om-elem-timestamp-set-double-time (time1 time2 timestamp)
  "Set start and end time of TIMESTAMP to TIME1 and TIME2 respectively.
TIME1 and TIME2 are lists analogous to the TIME argument specified in
`om-elem-build-timestamp!'."
  (om-elem--verify timestamp om-elem-is-timestamp-p)
  (om-elem--timestamp-set-double-time time1 time2 timestamp))

(defun om-elem-timestamp-set-range (range timestamp)
  "Set the RANGE of TIMESTAMP.
If TIMESTAMP is ranged, keep start time the same and adjust the end
time. If not, make a new end time. The units for RANGE are in minutes
if TIMESTAMP is in long format and days if TIMESTAMP is in short
format."
  (om-elem--verify timestamp om-elem-is-timestamp-p)
  (om-elem--timestamp-set-range range timestamp))

(defun om-elem-timestamp-set-type (type timestamp)
  "Set type of TIMESTAMP element to TYPE.
TYPE can be either 'active' or 'inactive'."
  (om-elem--verify timestamp om-elem-is-timestamp-p)
  (om-elem--timestamp-set-type type timestamp))

(defun om-elem-timestamp-shift (n unit timestamp)
  "Shift TIMESTAMP time by N UNITS.

This function will move the start and end times together; therefore
ranged inputs will always output ranged timestamps and same for
non-ranged. To move the start and end time independently, use
`om-elem-timestamp-shift-start' or `om-elem-timestamp-shift-end'.

N is a positive or negative integer and UNIT is one of 'minute',
'hour', 'day', 'month', or 'year'. Overflows will wrap around
transparently; for instance, supplying 'minute' for UNIT and 90 for N
will increase the hour property by 1 and the minute property by 30."
  (om-elem--verify timestamp om-elem-is-timestamp-p)
  (om-elem--timestamp-shift-range n unit timestamp))

(defun om-elem-timestamp-shift-start (n unit timestamp)
  "Shift TIMESTAMP start time by N UNITS.

N and UNIT behave the same as those in `om-elem-timestamp-shift'.

If TIMESTAMP is not range, the output will be a ranged timestamp with
the shifted start time and the end time as that of TIMESTAMP. If this
behavior is not desired, use `om-elem-timestamp-shift'."
  (om-elem--verify timestamp om-elem-is-timestamp-p)
  (om-elem--timestamp-shift-start n unit timestamp))

(defun om-elem-timestamp-shift-end (n unit timestamp)
  "Shift TIMESTAMP end time by N UNITS.

N and UNIT behave the same as those in `om-elem-timestamp-shift'.

If TIMESTAMP is not range, the output will be a ranged timestamp with
the shifted end time and the start time as that of TIMESTAMP. If this
behavior is not desired, use `om-elem-timestamp-shift'."
  (om-elem--verify timestamp om-elem-is-timestamp-p)
  (om-elem--timestamp-shift-end n unit timestamp))

(defun om-elem-timestamp-toggle-active (timestamp)
  "Toggle the active/inactive type of TIMESTAMP element."
  (om-elem--verify timestamp om-elem-is-timestamp-p)
  (om-elem--timestamp-toggle-active timestamp))

;;; elements
;;
;; clock

(defun om-elem-clock-is-running-p (clock)
  "Return t if CLOCK element is running (eg is open)."
  (om-elem--verify clock om-elem-is-clock-p)
  (om-elem--property-is-eq-p :status 'running clock))

(defun om-elem-clock-map-timestamp (fun clock)
  "Apply FUN to timestamp in CLOCK.
FUN is a function that takes the current timestamp and returns
a modified timestamp. The returned timestamp must be inactive and
cannot contain any warnings or repeaters."
  (om-elem--verify clock om-elem-is-clock-p)
  (om-elem-map-property :value fun clock))

(om-elem--gen-anaphoric-form 'om-elem-clock-map-timestamp)

;; headline

(defun om-elem-headline-is-done-p (headline)
  "Return t if HEADLINE element has a DONE todo keyword."
  (om-elem--verify headline om-elem-is-headline-p)
  (-> (om-elem--get-property :todo-keyword headline)
      (member org-done-keywords)
      (and t)))

(defun om-elem-headline-is-archived-p (headline)
  "Return t if HEADLINE element is archived."
  (om-elem--verify headline om-elem-is-headline-p)
  (om-elem--property-is-non-nil-p :archivedp headline))

(defun om-elem-headline-is-commented-p (headline)
  "Return t if HEADLINE element is commented."
  (om-elem--verify headline om-elem-is-headline-p)
  (om-elem--property-is-non-nil-p :commentedp headline))

;; TODO refactor this to be in terms of property-list functions
(defun om-elem-headline-has-tag-p (tag headline)
  "Return t if HEADLINE element is tagged with TAG."
  (om-elem--verify headline om-elem-is-headline-p)
  (if (member tag (om-elem--get-property :tags headline)) t))

(defun om-elem-headline-set-title! (text stats headline)
  (om-elem--verify headline om-elem-is-headline-p)
  (om-elem--headline-set-title! text stats headline))

(defun om-elem-headline-update-item-statistics (headline)
  (let ((items (om-elem-match '(section plain-list item) headline))
        (done (length (-filter #'om-elem-item-is-checked-p items)))
        (total (length items)))
    (om-elem--headline-set-statistics-cookie-fraction done total headline)))

(defun om-elem-headline-update-todo-statistics (headline)
  ;; TODO make this private
  (let ((subtodo (->> (om-elem-headline--get-subheadlines headline)
                      (--filter (om-elem--get-property :todo-keyword it)))
        (done (length (-filter #'om-elem-headline-is-done-p subtodo)))
        (total (length subtodo)))
    (om-elem--headline-set-statistics-cookie-fraction done total headline))))

;; item

(defun om-elem-item-is-checked-p (item)
  "Return t if ITEM element is checked."
  (om-elem--verify item om-elem-is-item-p)
  (om-elem--property-is-eq-p :checkbox 'on item))

(defun om-elem-item-is-unchecked-p (item)
  "Return t if ITEM element is unchecked."
  (om-elem--verify item om-elem-is-item-p)
  (om-elem--property-is-eq-p :checkbox 'off item))

(defun om-elem-item-is-trans-p (item)
  "Return t if ITEM element is transitional."
  (om-elem--verify item om-elem-is-item-p)
  (om-elem--property-is-eq-p :checkbox 'trans item))

(defun om-elem-item-toggle-checkbox (item)
  "Toggle the checked/unchecked state of ITEM element."
  (om-elem--verify item om-elem-is-item-p)
  (om-elem--item-toggle-checkbox item))

;; planning

(defun om-elem-planning-set-timestamp (prop planning-list planning)
  "Set the timestamp of PLANNING matching PROP.

PROP is one of :closed, :deadline, or :scheduled. PLANNING-LIST is the
same as that described in `om-elem-build-planning!'."
  (om-elem--verify planning om-elem-is-planning-p)
  (unless (memq prop '(:closed :deadline :scheduled))
    (error "PROP must be ':closed', ':deadline', or ':scheduled'. Got %S" prop))
  (let ((ts (om-elem--planning-list-to-timestamp planning-list)))
    (om-elem--set-property-strict prop ts planning)))

;; TODO this is a bit redundant...
(defun om-elem-planning-map-timestamp (prop fun planning)
  "Modify timestamp matching PROP in place in PLANNING using FUN.

PROP is one of :closed, :deadline, or :scheduled. FUN must return a
timestamp conforming to that described in `om-elem-build-planning'.

The only difference between using this function and using 
`om-elem-map-property' is that the former will silently no-op if PROP
is nil. The latter will throw an error unless FUN is able to handle
nil values."
  (om-elem--verify planning om-elem-is-planning-p)
  (unless (memq prop '(:closed :deadline :scheduled))
    (error "PROP must be ':closed', ':deadline', or ':scheduled'. Got %S" prop))
  (-if-let (ts (om-elem--get-property-strict prop planning))
      (om-elem--set-property-strict prop (funcall fun ts) planning)
    planning))

(om-elem--gen-anaphoric-form 'om-elem-planning-map-timestamp)

;;; PUBLIC CONTENT FUNCTIONS

;; generic

(defun om-elem-contains-point-p (point elem)
  "Return t if integer POINT is within the beginning and end of ELEM."
  (<= (om-elem--get-property :begin elem) point
      (om-elem--get-property :end elem)))

(defun om-elem-contents-contains-point-p (point elem)
  "Return t if integer POINT is within the beginning and end of ELEM's contents."
  (<= (om-elem--get-property :contents-begin elem) point
      (om-elem--get-property :contents-end elem)))

(defun om-elem--wrap (type args)
  (-> (format "om-elem-build-%s" type) (intern) (apply args)))

(defun om-elem-wrap-object (type &rest args)
  "Call build function of TYPE to wrap objects contained in ARGS.
ARGS is actually a list if keywords and objects that will be
passed to the builder function. For example with TYPE of 'bold'
and ARGS of ':post-blank 2 \"foo\"', the function `om-elem-build-bold'
will be called with keyword argument ':postblank 2' and \"foo\" in
the rest args slot."
  (if (memq type (append org-element-recursive-objects
                         org-element-object-containers))
      (om-elem--wrap type args)
    (error "Invalid type: %s" type)))

(defun om-elem-wrap-element (type &rest args)
  (if (memq type org-element-greater-elements)
      (om-elem--wrap type args)
    (error "Invalid type: %s" type)))

;; TODO is this a meaningful distinction?
(defun om-elem-unwrap (obj)
  "Remove the contents of recursive/container object or greater element OBJ."
  (if (om-elem-plain-list-p obj) (list obj)
    (let* ((contents (om-elem--get-contents obj))
           (post-blank (om-elem--get-property :post-blank obj))
           (first (-drop-last 1 contents))
           (last* (->> (-last-item contents)
                       (om-elem-set-post-blank post-blank)
                       (list))))
      (append first last*))))

(defun om-elem-unwrap-deep (types obj)
  "Remove the contents of all objects of type in TYPES from OBJ.
Return a list of objects."
  (cond
   ((om-elem--is-any-type-p types obj) 
    (let* ((contents (om-elem--get-contents obj))
           (post-blank (om-elem--get-property :post-blank obj))
           (first (-drop-last 1 contents))
           (last* (->> (-last-item contents)
                       (om-elem-set-post-blank post-blank)
                       (list))))
      (--mapcat (om-elem-unwrap-deep types it) (append first last*))))
   ((om-elem--is-plain-text-p obj) (list obj))
   (t (list obj))))

(defun om-elem-remove-formatting (types elem)
  "Remove all recursive formatting TYPES from ELEM."
  (om-elem-match-map* `(:many! (:or ,@types))
                (om-elem-unwrap-deep types it) elem))

(defun om-elem-remove-all-formatting (elem)
  "Remove all recursive formatting from ELEM."
  (om-elem-remove-formatting org-element-all-objects elem))

;; objects
;;
;; link

(defun om-elem-link-set-description (desc link)
  (om-elem--verify link om-elem-is-link-p)
  (om-elem--set-contents-by-type 'link desc link))

;; elements
;;
;; headline

(defun om-elem-headline-get-subheadlines (headline)
  "Return list of subheadlines for HEADLINE element or nil if none."
  (om-elem--verify headline om-elem-is-headline-p)
  (om-elem--headline-get-subheadlines headline))

(defun om-elem-headline-get-section (headline)
  "Return section for headline HEADLINE element or nil if none."
  (om-elem--verify headline om-elem-is-headline-p)
  (om-elem--headline-get-section headline))

(defun om-elem-headline-get-drawer (name headline)
  "Return the first drawer element in HEADLINE named NAME."
  (om-elem--verify headline om-elem-is-headline-p)
  (om-elem--headline-get-drawer name headline))

(defun om-elem-headline-is-closed-p (headline)
  "Return t if HEADLINE element is closed."
  (om-elem--verify headline om-elem-is-headline-p)
  (and (->> (om-elem--headline-get-planning headline)
            (om-elem--get-property :closed))
       t))

(defun om-elem-headline-is-deadlined-p (headline)
  "Return t if HEADLINE element has a deadline."
  (om-elem--verify headline om-elem-is-headline-p)
  (and (->> (om-elem--headline-get-planning headline)
            (om-elem--get-property :deadline))
       t))

(defun om-elem-headline-is-scheduled-p (headline)
  "Return t if HEADLINE element is scheduled."
  (om-elem--verify headline om-elem-is-headline-p)
  (and (->> (om-elem--headline-get-planning headline)
            (om-elem--get-property :scheduled))
       t))

;; (defun om-elem-set-planning (planning-plist headline)
;;   (om-elem--verify headline om-elem-is-headline-p)
;;   (let ((keys (om-elem--plist-get-keys planning-plist)))
;;     (--> (om-elem--plist-get-vals planning-plist)
;;          (--map (-some->> it (om-elem-build-timestamp 'inactive)) it)
;;          (-interleave keys it)
;;          (om-elem-set-properties it headline))))

;; item

(defun om-elem-item-get-paragraph (item)
  "Return the paragraph immediately within ITEM or nil if none."
  (om-elem--verify item om-elem-is-item-p)
  (om-elem--item-get-paragraph item))

(defun om-elem-item-get-sublist (item)
  "Return the plain-list immediately within ITEM or nil if none."
  (om-elem--verify item om-elem-is-item-p)
  (om-elem--item-get-sublist item))

;; plain-list

;; TODO there seems to be a bug in the org-interpeter that prevents
;; "+" bullets from being recognized (as of org-9.1.9 they are simply
;; read as "-")
(defun om-elem-plain-list-set-type (type plain-list)
  "Set the type of PLAIN-LIST greater element to TYPE.
TYPE is '-', '+', or 'ordered'."
  (om-elem--verify plain-list om-elem-is-plain-list-p)
  (cond
   ((memq type '(+ -))
    (om-elem-match-map* '(item) (om-elem--set-property-strict :bullet type it) plain-list))
   ((eq type 'ordered)
    ;; NOTE the org-interpreter seems to use the correct, ordered
    ;; numbers if any number is set here. This behavior may not be
    ;; reliable.
    (om-elem-match-map* '(item) (om-elem--set-property-strict :bullet 1 it) plain-list))
   (t (error "Invalid type: %s" type))))

;; table

(defun om-elem-table-get-cell (row-index column-index table)
  "Return table-cell at ROW-INDEX and COLUMN-INDEX in TABLE element.
H-lines do not count toward row indices, and all indices are
zero-indexed."
  (om-elem--verify table om-elem-is-table-p)
  (om-elem--table-get-cell row-index column-index table))

(defun om-elem-table-replace-cell (row-index column-index cell table)
  "Replace a cell in TABLE with CELL (a table-cell element).
ROW-INDEX and COLUMN-INDEX are zero-indexed integers pointing to the
position of the cell to be replaced."
  (om-elem--verify table om-elem-is-table-p)
  (om-elem--table-replace-cell row-index column-index cell table))

;; TOTO use shortcut table-cell builder
(defun om-elem-table-replace-cell! (row-index column-index cell-text
                                              table)
  "Replace a cell in TABLE with CELL-TEXT.
CELL-TEXT is a string which will replace the contents of the cell at
ROW-INDEX and COLUMN-INDEX (zero-indexed integers)."
  (om-elem--verify table om-elem-is-table-p)
  (let ((cell (om-elem-build-table-cell cell-text)))
    (om-elem--table-replace-cell row-index column-index cell table)))

(defun om-elem-table-clear-cell (row-index column-index table)
  "Clear a cell in TABLE.
ROW-INDEX and COLUMN-INDEX are zero-indexed integers pointing to the
position of the cell to be replaced."
  (om-elem--verify table om-elem-is-table-p)
  (let ((cell (om-elem-build-table-cell " ")))
    (om-elem--table-replace-cell row-index column-index cell table)))

(defun om-elem-table-delete-row (row-index table)
  "Delete the row at ROW-INDEX in TABLE."
  (om-elem--verify table om-elem-is-table-p)
  (om-elem--table-delete-row row-index table))

(defun om-elem-table-delete-column (column-index table)
  "Delete the column at COLUMN-INDEX in TABLE."
  (om-elem--verify table om-elem-is-table-p)
  (om-elem--table-delete-column column-index table))

(defun om-elem-table-insert-column (column-index column-cells table)
  "Insert COLUMN-CELLS at COLUMN-INDEX in TABLE."
  (om-elem--verify table om-elem-is-table-p)
  (unless (-all? #'om-elem-is-table-cell-p column-cells)
    (error "All members of column must be table cells"))
  (om-elem--table-insert-column column-index column-cells table))

(defun om-elem-table-insert-column! (column-index column-text table)
  "Insert COLUMN-TEXT at COLUMN-INDEX in TABLE."
  (om-elem--verify table om-elem-is-table-p)
  ;; TOOD use shorthand cell builder
  (let ((column (-map #'om-elem-build-table-cell column-text)))
    (om-elem--table-insert-column column-index column table)))

(defun om-elem-table-clear-column (column-index table)
  "Clear the column at COLUMN-INDEX in TABLE."
  (om-elem--verify table om-elem-is-table-p)
  (om-elem--table-clear-column column-index table))

(defun om-elem-table-insert-row (row-index row table)
  "Insert ROW at ROW-INDEX in TABLE."
  (om-elem--verify table om-elem-is-table-p)
  (om-elem--table-insert-row row-index row table))

(defun om-elem-table-insert-row! (row-index row-text table)
  "Insert ROW-TEXT at ROW-INDEX in TABLE."
  (om-elem--verify table om-elem-is-table-p)
  ;; TODO use shorthand row builder
  (let ((row (if (eq row-text 'hline) (om-elem-build-table-row-hline)
               (-map #'om-elem-build-table-cell row-text))))
    (om-elem--table-insert-row row-index row table)))

(defun om-elem-table-clear-row (row-index table)
  "Clear the row at ROW-INDEX in TABLE."
  (om-elem--verify table om-elem-is-table-p)
  (om-elem--table-clear-row row-index table))

(defun om-elem-table-replace-column (column-index column-cells table)
  "Replace column at COLUMN-INDEX in TABLE with COLUMN-CELLS.
COLUMN-INDEX is the index of the column (starting at zero) and
COLUMN-CELLS is a list of table-cell objects."
  (om-elem--verify table om-elem-is-table-p)
  (om-elem--table-replace-column column-index column-cells table))

(defun om-elem-table-replace-column! (column-index column-text table)
  "Replace column at COLUMN-INDEX in TABLE with COLUMN-TEXT.
COLUMN-INDEX is the index of the column (starting at zero) and
COLUMN-TEXT is a list of text to be made into table-cell objects."
  (om-elem--verify table om-elem-is-table-p)
  ;; TODO use shorthand cell builder here
  (let ((column-cells (-map #'om-elem-build-table-cell column-text)))
    (om-elem--table-replace-column column-index column-cells table)))

(defun om-elem-table-replace-row (row-index row-cells table)
  "Replace row at ROW-INDEX in TABLE with ROW-CELLS.
ROW-INDEX is the index of the row (starting at zero) and
ROW-CELLS is a list of table-cell objects."
  (om-elem--verify table om-elem-is-table-p)
  (om-elem--table-replace-row row-index row-cells table))

(defun om-elem-table-replace-row! (row-index row-text table)
  "Replace row at ROW-INDEX in TABLE with ROW-TEXT.
ROW-INDEX is the index of the row (starting at zero) and
ROW-TEXT is a list of text to be made into table-cell objects."
  (om-elem--verify table om-elem-is-table-p)
  ;; TODO use shorthand row builder here
  (let ((row-cells (->> (-map #'om-elem-build-table-cell row-text)
                        (apply #'om-elem-build-table-row))))
    (om-elem--table-replace-row row-index row-cells table)))

;; PUBLIC INDENTATION FUNCTIONS

;; headline

(defun om-elem-headline-indent-subtree (index headline)
  "Indent the subheadline and its contents at INDEX within HEADLINE."
  (om-elem--verify headline om-elem-is-headline-p)
  (om-elem--headline-indent-subtree index headline))

(defun om-elem-headline-indent-subheadline (index headline)
  "Indent the subheadline without moving its contents at INDEX within HEADLINE."
  (om-elem--verify headline om-elem-is-headline-p)
  (om-elem--headline-indent-subheadline index headline))

(defun om-elem-headline-unindent-subheadline (index child-index headline)
  "Unindent subheadline at CHILD-INDEX in the subheadline at INDEX in HEADLINE.
This will not move the contents under the headline at CHILD-INDEX."
  (om-elem--verify headline om-elem-is-headline-p)
  (om-elem--headline-unindent-subheadline index child-index headline))

(defun om-elem-headline-unindent-subtree (index headline)
  "Unindent all subheadlines under the subheadline at INDEX in HEADLINE."
  (om-elem--verify headline om-elem-is-headline-p)
  (om-elem--headline-unindent-subtree index headline))

;; plain-list

(defun om-elem-plain-list-indent-item-tree (index plain-list)
  "Indent the subitem at INDEX in PLAIN-LIST and move items below it."
  (om-elem--verify plain-list om-elem-is-plain-list-p)
  (om-elem--plain-list-indent-item-tree index plain-list))

(defun om-elem-plain-list-indent-item (index plain-list)
  "Indent the subitem at INDEX in PLAIN-LIST without moving items below it."
  (om-elem--verify plain-list om-elem-is-plain-list-p)
  (om-elem--plain-list-indent-item index plain-list))

(defun om-elem-plain-list-unindent-item (index child-index plain-list)
  "Unindent subitem at CHILD-INDEX in the subitem at INDEX in PLAIN-LIST.
This will not move the contents under the item at CHILD-INDEX."
  (om-elem--verify plain-list om-elem-is-plain-list-p)
  (om-elem--plain-list-unindent-item index child-index plain-list))

(defun om-elem-plain-list-unindent-items (index plain-list)
  "Unindent all items under the item at INDEX in PLAIN-LIST."
  (om-elem--verify plain-list om-elem-is-plain-list-p)
  (om-elem--plain-list-unindent-items index plain-list))

;;; printing functions

(defun om-elem--set-blank-contents (elem)
  "Set the contents of ELEM to a blank string (\"\")."
  (om-elem--set-contents '("") elem))

(defun om-elem-is-zero-length-p (elem)
  "Return t if ELEM will print as a blank string."
  (-if-let (contents (om-elem--get-contents elem))
      (-all? #'om-elem-is-zero-length-p contents)
    (and (om-elem--is-any-type-p om-elem--rm-if-empty elem)
         (om-elem--is-empty-p elem))))

(defconst om-elem--rm-if-empty
  '(table plain-list bold italic radio-target strike-through
          superscript subscript table-cell underline)
  "Elements/objects that will be blank if printed and empty.")

(defconst om-elem--blank-if-empty
  '(center-block drawer dynamic-block property-drawer quote-block
                 special-block verse-block)
  "Elements that require contents of \"\" to correctly print empty.
This is a workaround for a bug.")

(defun om-elem--filter-non-zero-length (elem)
  (unless (and (om-elem--is-empty-p elem)
               (or (om-elem--is-any-type-p om-elem--rm-if-empty elem)
                   (and (om-elem--is-type-p 'table-row elem)
                        (om-elem--property-is-eq-p :type 'standard elem))))
    elem))

(defun om-elem--clean (elem)
  (->> elem
       (om-elem--map-contents* (-non-nil (-map #'om-elem--clean it)))
       (om-elem--filter-non-zero-length)))

(defun om-elem--blank (elem)
  (if (om-elem--is-empty-p elem)
      (if (om-elem--is-any-type-p om-elem--blank-if-empty elem)
          (om-elem--set-blank-contents elem)
        elem)
    (om-elem--map-contents* (-map #'om-elem--blank it) elem)))

(defun om-elem-to-string (elem)
  "Return ELEM as an interpreted string without text properties."
  (->> elem
       ;; some objects and greater elements should be removed if blank
       ;; table and plain list will error, and the others make no
       ;; sense if they are empty. This is an org mode bug, they
       ;; should not be printed by the interpreter by default
       (om-elem--clean)
       ;; some greater elements will print "nil" in their contents if
       ;; they are empty. This is likely an org bug, since it means
       ;; that the element <-> string conversion is not 100%
       ;; reproducible. The workaround for this is to set the contents
       ;; to a single blank string if empty
       (om-elem--blank)
       (org-element-interpret-data)
       (substring-no-properties)))

(defun om-elem-to-trimmed-string (elem)
  "Like `om-elem-to-string' but strip whitespace when returning ELEM."
  (-some->> (om-elem-to-string elem) (s-trim)))

;;; parsing functions

;; point functions

(defun om-elem-parse-object-at (point)
  "Return the object tree under POINT or nil if not on an object.

If TYPE is supplied, only return nil if the object under point is
not of that type. TYPE is a symbol from `om-elem-objects'."
  (save-excursion
    (goto-char point)
    (let* ((context (org-element-context))
           (type (om-elem--get-type context))
           (offset (cond
                    ((memq type '(superscript subscript)) -1)
                    ((eq type 'table-cell) -1)
                    (t 0)))
           (nesting (cond
                     ((memq type '(superscript subscript)) '(0 1))
                     ((eq type 'table-cell) '(0 0 0))
                     (t '(0 0)))))
      (-let* (((&plist :begin :end) (om-elem--get-properties context))
              (tree (org-element--parse-elements (+ begin offset) end 'first-section
                                                 nil nil nil nil)))
        (--> (car tree)
             (om-elem--get-nested-contents nesting it)
             (om-elem--filter-types org-element-all-objects it)
             (if type (om-elem--filter-type type it) it))))))

(defun om-elem--parse-element-at (point &optional type)
  "Return element immediately under POINT.
For a list of all possible return types refer to
`org-element-all-elements'; this will return everything in this list
except 'section' which is ambiguous when referring to a single point.
(see `om-elem-parse-section-at').

If TYPE is supplied, only return nil if the object under point is
not of that type. TYPE is a symbol from `org-element-all-elements'.
Furthermore, setting TYPE to 'table-row' will prefer table-row
elements over table elements and likewise when setting TYPE to 'item'
for plain-list elements vs item elements."
  (save-excursion
    (goto-char point)
    (let*
        ((elem (org-element-at-point))
         (elem-type (om-elem--get-type elem)))
      (if (not
           (memq elem-type (append org-element-greater-elements
                                   org-element-object-containers)))
          elem
        (-let* (((&plist :begin :end) (om-elem--get-properties elem))
                (tree (car (org-element--parse-elements
                            begin end 'first-section nil nil nil nil)))
                (nesting (cl-case elem-type
                           (headline nil)
                           (table (if (eq type 'table-row) '(0 0) '(0)))
                           (plain-list (if (eq type 'item) '(0 0) '(0)))
                           (t '(0)))))
          (--> (om-elem--get-nested-contents nesting tree)
               (if type (om-elem--filter-type type it) it)))))))

(defun om-elem-parse-element-at (point)
  "Return element under POINT or nil if not on an element.

This function will return every element available in `om-elem-elements'
with the exception of 'section', 'item', and 'table-row'. To
specifically parse these, use the functions `om-elem-parse-section-at',
`om-elem-parse-item-at', and `om-elem-parse-table-row-at'."
  (om-elem--parse-element-at point))

(defun om-elem-parse-table-row-at (point)
  "Return table-row element under POINT or nil if not on a table-row."
  (save-excursion
    (goto-char point)
    (beginning-of-line)
    (om-elem--parse-element-at (point) 'table-row)))

(defun om-elem-parse-item-at (point)
  "Return item element under POINT or nil if not on an item.
This will return the item even if POINT is not at the beginning of
the line."
  (save-excursion
    (goto-char point)
    (beginning-of-line)
    (om-elem--parse-element-at (point) 'item)))

(defun om-elem--parse-headline-subtree-at (point subtree)
  (save-excursion
    (goto-char point)
    (when (ignore-errors (org-back-to-heading))
      (let ((b (point))
            (e (if subtree (org-end-of-subtree)
                 (or (outline-next-heading) (point-max)))))
        (car (org-element--parse-elements b e 'first-section
                                          nil nil nil nil))))))

(defun om-elem-parse-headline-at (point)
  "Return headline tree under POINT or nil if not on a headline.
POINT does not need to be on the headline itself. Only the headline
and its section will be returned. To include subheadlines, use
`om-elem-parse-headline-subtree-at'."
  (om-elem--parse-headline-subtree-at point nil))

(defun om-elem-parse-subtree-at (point)
  "Return headline tree under POINT or nil if not on a headline.
POINT does not need to be on the headline itself. Unlike
`om-elem-parse-headline-at', the returned tree will include
subheadlines."
  (om-elem--parse-headline-subtree-at point t))

(defun om-elem-parse-section-at (point)
  "Return tree of the section under POINT or nil if not on a section.
If POINT is on or within a headline, return the section under that
headline. If POINT is before the first headline (if any), return
the section at the top of the org buffer."
  (save-excursion
    (goto-char point)
    (om-elem--get-nested-contents
     '(0)
     (condition-case nil
         (progn
           (org-back-to-heading)
           ;; TODO this is redundant
           (om-elem--parse-headline-subtree-at point nil))
       (error
        (progn
          (org-element--parse-elements
           (point-min) (or (outline-next-heading) (point-max))
           'first-section nil nil nil nil)))))))

;; parse at current point

(-> '(object element table-row item headline subtree section)
    (--each
        (let* ((name (intern (format "om-elem-parse-this-%s" it)))
               (call (intern (format "om-elem-parse-%s-at" it)))
               (doc (format "Call `%s' with the current point." call))
               (body `(,call (point))))
          (eval `(defun ,name () ,doc ,body)))))

(defun om-elem-parse-this-buffer ()
  "Return org-data document tree for the current buffer.
Contrary to the org-element specification, the org-data element
returned from this function will have :begin and :end properties."
  (let* ((c (om-elem--get-contents (org-element-parse-buffer)))
         (b (if c (om-elem--get-property :begin (-first-item c)) 1))
         (e (if c (om-elem--get-property :end (-last-item c)) 1)))
    (om-elem--construct 'org-data `(:begin ,b :end ,e) c)))

;;; side effects

;; write

(defun om-elem-insert (point elem)
  "Convert ELEM to a string and insert at POINT in the current buffer.
Return ELEM."
  (om-elem--verify point integerp
                   elem om-elem--is-element-or-object-p)
  (save-excursion
    (goto-char point)
    (insert (om-elem-to-string elem)))
  elem)

(defun om-elem-insert-tail (point elem)
  "Like `om-elem-insert' but insert ELEM at POINT and move to the end of inserted string."
  (om-elem--verify point integerp
                   elem om-elem--is-element-or-object-p)
  (let ((s (om-elem-to-string elem)))
    (save-excursion
      (goto-char point)
      (insert s))
    (goto-char (+ point (length s))))
  elem)

(defun om-elem--apply-overlays (os)
  (cl-flet
      ((apply-overlays
        (o)
        (let* ((beg (plist-get o :start))
               (end (plist-get o :end))
               (props (plist-get o :props))
               (o* (make-overlay beg end)))
          (--each (-partition 2 props) (apply #'overlay-put o* it)))))
    (-each os #'apply-overlays)))

(defun om-elem-update (fun elem)
  "Replace ELEM in the current buffer with a new one. 
FUN is a function that takes ELEM as its only argument and returns a
modified ELEM. This modified element is then written in place of the
old element in the current buffer."
  (om-elem--verify fun functionp
                   elem om-elem--is-element-or-object-p)
  ;; if elem is of type 'org-data' it will have no props
  (let* ((begin (or (om-elem--get-property :begin elem) (point-min)))
         (end (or (om-elem--get-property :end elem) (point-max)))
         ;; get the outline overlays that make text invisible
         (ov-cmd (->>
                  (overlays-in begin end)
                  (--filter (eq 'outline (overlay-get it 'invisible)))
                  (--map (list :start (overlay-start it)
                               :end (overlay-end it)
                               :props (overlay-properties it)))
                  (list 'apply 'om-elem--apply-overlays))))
    ;; hacky way to add overlays to undo tree
    (setq-local buffer-undo-list (cons ov-cmd buffer-undo-list))
    (delete-region begin end)
    (->> (funcall fun elem) (om-elem-insert begin))
    nil))

(defmacro om-elem-update* (form elem)
  "Anaphoric form of `om-elem-update'.
IN-FORM and PROC-FORM are forms corresponding to 'in-fun' and 
'proc-fun'. The latter has the variable 'it' available to it, which
holds the element returned from IN-FORM."
  (declare (indent 1))
  `(om-elem-update (lambda () ,form) elem))

;; generate all update functions for corresponding parse functions
;; since all take function args, also generate anaphoric forms
(--each '(object element table-row item headline subtree section)
  (let* ((update-at
          (intern (format "om-elem-update-%s-at" it)))
         (update-this
          (intern (format "om-elem-update-this-%s" it)))
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
         (call (intern (format "om-elem-parse-%s-at" it)))
         (update-at-body `(om-elem-update fun (,call point)))
         (update-this-body `(,update-at (point) fun)))
    (eval `(defun ,update-at (point fun)
             ,update-at-doc
             ,update-at-body))
    (om-elem--gen-anaphoric-form update-at)
    (eval `(defun ,update-this (fun)
             ,update-this-doc
             ,update-this-body))
    (om-elem--gen-anaphoric-form update-this)))

(defun om-elem-update-this-buffer (fun)
  (om-elem-update fun (om-elem-parse-this-buffer)))

(om-elem--gen-anaphoric-form 'om-elem-update-this-buffer)

;; fold
(defun om-elem--flag-elem-contents (flag elem)
  (om-elem--verify flag booleanp
                   elem om-elem--is-element-or-object-p)
  (-let (((&plist :contents-begin :contents-end) (om-elem--get-properties elem)))
    (outline-flag-region (- contents-begin 1) (- contents-end 1) flag)))

(defun om-elem-fold-contents (elem)
  "Fold the contents of ELEM if they exist."
  (om-elem--flag-elem-contents t elem))

(defun om-elem-unfold-contents (elem)
  "Unfold the contents of ELEM if they exist."
  (om-elem--flag-elem-contents nil elem))

;;; misc functions

(defun om-elem-length (elem)
  "Return the character length of ELEM."
  (if (not elem) 0
    (let ((b (om-elem--get-property :begin elem))
          (e (om-elem--get-property :end elem)))
      (if (and b e) (- e b)
        (error "Can't determine element length")))))

(defun om-elem-now ()
  "Return list representing the current time without hours and minutes.
This is meant to be used as input for functions such as
`om-elem-build-timestamp'."
  (->> (decode-time) (-select-by-indices '(3 4 5)) (reverse)))

(defun om-elem-now-long ()
  "Return list representing the current time with hours and minutes.
This is meant to be used as input for functions such as
`om-elem-build-timestamp'."
  (->> (decode-time) (-select-by-indices '(1 2 3 4 5)) (reverse)))

(defun om-elem-get-type (elem)
  (om-elem--verify elem om-elem--is-element-or-object-p)
  (om-elem--get-type elem))

;;; generalized CRUD operations

(defmacro om-elem--modify-contents (elem form)
  "Recursively modify the contents of ELEM using FORM.
FORM is a form that returns a list of elements or objects as the
new contents, and the variable 'it' is available to represent the
original contents to be modified."
  `(cl-labels
       ((rec
         (elem)
         (let ((type (om-elem--get-type elem)))
           (if (eq type 'plain-text) elem
             (->>
              (om-elem--get-contents elem)
              (funcall (lambda (it) ,form))
              (--map (rec it))
              (om-elem--construct type (nth 1 elem)))))))
     (rec elem)))

;; find

(defun om-elem-filter-query (query contents)
  (pcase query
    ;; quote (may be accidentally in query
    (`(quote . ,_)
     (error "'quote' not allowed in query"))

    ;; function (may be accidentally in query
    (`(function . ,_)
     (error "'function' not allowed in query"))
    
    ;; index
    ((and (pred integerp) index)
     (-some->
      (if (< index 0)
          (nth (- (* -1 index) 1) (nreverse contents))
        (nth index contents))
      (list)))

    ;; type
    ((and (pred (lambda (y) (memq y om-elem-elements-and-objects))) type)
     (--filter (om-elem--is-type-p type it) contents))

    ;; relative index
    (`(,(and (or '< '<= '> '>=) f)
       ,(and (pred integerp) i))
     ;; TODO what if they give a negative index?
     (->> contents
          (--map-indexed (when (funcall f it-index i) it))
          (-non-nil)))

    ;; predicate
    ;; ((and (pred functionp) fun)
    (`(:pred . (,q . nil))
     (--filter (funcall q it) contents))

    ;; not
    (`(:not . (,q . nil))
     (->> (om-elem-filter-query q contents)
          (-difference contents)))

    ;; or
    (`(:or . ,(and (pred and) q))
     (->> (--mapcat (om-elem-filter-query it contents) q)
          (-distinct)))

    ;; and
    (`(:and . ,(and (pred and) q))
     (->> (--map (om-elem-filter-query it contents) q)
          (-reduce #'-intersection)))

    ;; properties
    ;; NOTE: this must go last if we don't want :and/:or/:not to
    ;; be interpreted as a plist
    ((and (pred om-elem--is-plist-p) plist)
     (cl-flet
         ((all-props-match?
           (elem props)
           (->> (-partition 2 (om-elem--get-properties elem))
                (-difference (-partition 2 props))
                (not))))
       (--filter (all-props-match? it plist) contents)))
    (_ (error "Invalid query: %s" query))))

(defun om-elem-match (queries elem)
  "Find all objects in ELEM that match QUERIES.

This will return a list of all successful matches. See
`om-elem-match-first' and `om-elem-match-last' to limit the return to
one match.

QUERIES consists of one or more criteria that is used to match
targets. The basic queries are:
FUN  - a predicate function that selects targets when true
TYPE - a symbol corresponding to the type of the element to match
INDEX - in integer corresponding to index of the element to match
PROPS - a plist that matches targets with the same property values

INDEX can be additionally qualified using comparison operators in a
two-membered list such as '(< INDEX)' which will match an element with
indices less than INDEX. Supported operators are '<', '>', '<=', and
'>=', and their function intuitively follows their names.

In addition, the above operators can be combined with boolean
operators ':and', ':or', and ':not' using a list starting with the
operators. For example, '(:or headline timestamp)' would match
headline or timestamp types. Each operator supports multiple criteria
after the initial list cell except :not, which only supports one (eg
'(:not headline timestamp)' is invalid).

The first query given to the function call will match against ELEM's
contents, and the next query will match the contents of the matched
contents of ELEM, and so forth for all queries. In this way, each
query can be thought to match one 'level' of contents within ELEM.

For example, if ELEM is a headline, the queries 'section paragraph'
would match the section immediately in ELEM's contents, and then match
the paragraph(s) within the section.

Special keywords can be supplied as queries that function as
wildcards for levels:
:many - matches zero or more levels
:many! - matches zero or more levels, but does not descend further
         into a match
:any - matches exactly one level

In the case of :many and :many!, only one additional query may follow
the keyword, where :any can be followed by at least one.

In the example above, ':any paragraph' would return the same match,
assuming that the ELEM has only one section."
  (om-elem--verify elem om-elem--is-element-or-object-p)
  ;; the non-nil is required for cases where we may get
  ;; a nil for queries instead of no argument
  (let ((queries (-non-nil queries)))
    (when queries
      (let ((contents (om-elem--get-contents elem)))
        (pcase queries
          (`(:many! . (,q . nil))
           (let ((found (om-elem-filter-query q contents))
                 (q* (list :many! q)))
             (->> (-difference contents found)
                  (--mapcat (om-elem-match q* it))
                  (append found))))
          (`(:many! . ,_)
           (error "Query with :many! must have one target"))
          (`(:many . (,q . nil))
           (let ((found (om-elem-filter-query q contents))
                 (q* (list :many q)))
             (->> (--mapcat (om-elem-match q* it) contents)
                  (append found))))
          (`(:many . ,_)
           (error "Query with :many must have one target"))
          (`(:any . ,(and (pred and) qs))
           (--mapcat (om-elem-match qs it) contents))
          (`(:any . nil)
           contents)
          (`(,q . nil)
           (om-elem-filter-query q contents))
          (`(,q . ,qs)
           (->> (om-elem-filter-query q contents)
                (--mapcat (om-elem-match qs it))))
          (_ (error "Invalid query")))))))

(defun om-elem-match-first (queries elem )
  "Find first object in ELEM matching QUERIES.
The rules for QUERIES are the same as `om-elem-match'"
  (-first-item (om-elem-match queries elem)))

(defun om-elem-match-last (queries elem)
  "Find last object in ELEM matching QUERIES.
The rules for QUERIES are the same as `om-elem-match'"
  (-last-item (om-elem-match queries elem)))

;; find-parent

(defun om-elem-match-parent-query (parent query)
  (pcase query
    ;; type
    ((and (pred symbolp) type)
     ;; TODO check for valid type?
     (and (om-elem--is-type-p type parent) parent))
    ;; compound (or)
    ;; (`(:or . ,(and (pred and) q)))
    ;; compound (and)
    ;; (`(:and . ,(and (pred and) q)))
    ;; properties (must go after compound)
    ((pred om-elem--is-plist-p)
     (cl-flet
         ((all-props-match?
           (elem props)
           (->> (-slice props 0 nil 2)
                (--map (equal (plist-get props it)
                              (om-elem--get-property it elem)))
                (-none? #'null))))
       (and (all-props-match? parent query) parent)))
    (_ (error "Invalid query: %s" query))))

(defun om-elem-match-parent (elem &rest queries)
  ;; TODO validate elem (should be any valid element or object)
  (unless elem (error "No element given"))
  ;; the non-nil is required for cases where we may get
  ;; a nil for queries instead of no argument
  (let ((queries (-non-nil queries)))
    (when queries
      (let ((parent (om-elem--get-parent elem)))
        (pcase queries
          (`(:many . (,q . nil))
           (or (om-elem-match-parent-query parent q)
               (om-elem-match-parent parent q)))
          (`(:many . ,_)
           (error "Query with :many must have one target"))
          (`(:any . ,(and (pred and) qs))
           (om-elem-match-parent parent q))
          (`(:any . nil)
           (error "Query with :any must have at least one target"))
          (`(,q . nil)
           (om-elem-match-parent-query parent q))
          (`(,q . ,qs)
           (-> (om-elem-match-parent-query parent q)
               (om-elem-match-parent qs)))
          (_ (error "Invalid query")))))))

;; delete

(defun om-elem--delete-targets (elem targets)
  "Delete TARGETS in the contents of ELEM."
  (om-elem--modify-contents
   elem (--remove (member it targets) it)))

(defun om-elem-match-delete (queries elem)
  "Remove matching targets from contents of ELEM.

QUERIES follows the same rules as `om-elem-match'."
  (-if-let (targets (om-elem-match queries elem))
      (om-elem--delete-targets elem targets)
    elem))

(defun om-elem-match-delete-first (queries elem)
  "Remove first matching target from contents of ELEM.

QUERIES follows the same rules as `om-elem-match'."
  (-if-let (targets (om-elem-match queries elem))
      (om-elem--delete-targets elem (-take 1 targets))
    elem))

(defun om-elem-match-delete-last (queries elem)
  "Remove last matching target from contents of ELEM.

QUERIES follows the same rules as `om-elem-match'."
  (-if-let (targets (om-elem-match queries elem))
      (om-elem--delete-targets elem (-take-last 1 targets))
    elem))

;; extract

(defun om-elem-match-extract (queries elem)
  "Remove matching targets from contents of ELEM.
Return cons cell where the car is a list of all removed targets
and the cdr is the modified ELEM with targets removed.

QUERIES follows the same rules as `om-elem-match'."
  (-if-let (targets (om-elem-match queries elem))
      (cons targets (om-elem--delete-targets elem targets))
    elem))

(defun om-elem-match-extract-first (queries elem)
  "Remove first matching target from contents of ELEM.
Return cons cell where the car is the removed target and the cdr is
the modified ELEM with targets removed.

QUERIES follows the same rules as `om-elem-match'."
  (-if-let (targets (om-elem-match queries elem))
      (let ((target (-take 1 targets)))
        (cons (car target) (om-elem--delete-targets elem target)))
    elem))

(defun om-elem-match-extract-last (queries elem)
  "Remove last matching target from contents of ELEM.
Return cons cell where the car is the removed target and the cdr is
the modified ELEM with targets removed.

QUERIES follows the same rules as `om-elem-match'."
  (-if-let (targets (om-elem-match queries elem))
      (let ((target (-take 1 targets)))
        (cons (car target) (om-elem--delete-targets elem target)))
    elem))

;; map

(defun om-elem--map-targets (elem fun targets)
  "Apply FUN to TARGETS in the contents of ELEM."
  (om-elem--modify-contents
   elem (--map-when (member it targets) (funcall fun it) it)))

(defun om-elem-match-map (queries fun elem)
  "Apply FUN to targets matching QUERIES in the contents of ELEM.
FUN is a function that takes a single argument (the target element or
object) and returns a new element or object which will replace the
original.

QUERIES follows the same rules as `om-elem-match'."
  (-if-let (targets (om-elem-match queries elem))
      (om-elem--map-targets elem fun targets)
    elem))

(defun om-elem-match-map-first (queries fun elem)
  "Apply FUN to first target matching QUERIES in the contents of ELEM.
FUN is a function that takes a single argument (the target element or
object) and returns a new element or object which will replace the
original.

QUERIES follows the same rules as `om-elem-match'."
  (-if-let (targets (om-elem-match queries elem))
      (om-elem--map-targets elem fun (-take 1 targets))
    elem))

(defun om-elem-match-map-last (queries fun elem)
  "Apply FUN to last target matching QUERIES in the contents of ELEM.
FUN is a function that takes a single argument (the target element or
object) and returns a new element or object which will replace the
original.

QUERIES follows the same rules as `om-elem-match'."
  (-if-let (targets (om-elem-match queries elem))
      (om-elem--map-targets elem fun (-take-last 1 targets))
    elem))

;; (defmacro om-elem-match-map* (elem form &rest queries)
;;   `(om-elem-match-map ,elem (lambda (it) ,form) ,@queries))

;; (defmacro om-elem-match-map-first* (elem form &rest queries)
;;   `(om-elem-match-map-first ,elem (lambda (it) ,form) ,@queries))

;; (defmacro om-elem-match-map-last* (elem form &rest queries)
;;   `(om-elem-match-map-last ,elem (lambda (it) ,form) ,@queries))

;; mapcat

(defun om-elem--mapcat-targets (elem fun targets)
  "Apply FUN to TARGETS in the contents of ELEM."
  (om-elem--modify-contents
   elem (--mapcat (if (member it targets)
                      (funcall fun it) (list it))
                  it)))

(defun om-elem-match-mapcat (queries fun elem)
  "Apply FUN over ELEM and return modified ELEM.
FUN takes an element/object as its only argument and returns
a list of elements/objects. Targets within ELEM are found that match
QUERIES, FUN is applied to each target, and the resulting list is
spliced in place of the original target (as opposed to `om-elem-match-map'
which replaces the original target with a modified target).

QUERIES follows the same rules as `om-elem-match'."
  (-if-let (targets (om-elem-match queries elem))
      (om-elem--mapcat-targets elem fun targets)
    elem))

(defun om-elem-match-mapcat-first (queries fun elem)
  "Like `om-elem-match-mapcat' but only apply FUN to first match in ELEM.

QUERIES follows the same rules as `om-elem-match'."
  (-if-let (targets (om-elem-match queries elem))
      (om-elem--mapcat-targets elem fun (-take 1 targets))
    elem))

(defun om-elem-match-mapcat-last (queries fun elem)
  "Like `om-elem-match-mapcat' but only apply FUN to last match in ELEM.

QUERIES follows the same rules as `om-elem-match'."
  (-if-let (targets (om-elem-match queries elem))
      (om-elem--mapcat-targets elem fun (-take-last 1 targets))
    elem))

;; (defmacro om-elem-match-mapcat* (elem form &rest queries)
;;   "Anaphoric form of `om-elem-mmapcat'."
;;   `(om-elem-match-mapcat ,elem (lambda (it) ,form) ,@queries))

;; (defmacro om-elem-match-mapcat-first* (elem form &rest queries)
;;   "Anaphoric form of `om-elem-mmapcat-first'."
;;   `(om-elem-match-mapcat-first ,elem (lambda (it) ,form) ,@queries))

;; (defmacro om-elem-match-mapcat-last* (elem form &rest queries)
;;   "Anaphoric form of `om-elem-mmapcat-last'."
;;   `(om-elem-match-mapcat-last ,elem (lambda (it) ,form) ,@queries))

;; replace

(defun om-elem--replace-targets (elem rep targets)
  "Replace TARGETS with REP in the contents of ELEM."
  (om-elem--modify-contents
   elem (--map-when (member it targets) rep it)))

(defun om-elem-match-replace (queries rep elem)
  "Replace matching targets in ELEM with REP.

QUERIES follows the same rules as `om-elem-match'."
  (-if-let (targets (om-elem-match queries elem))
      (om-elem--replace-targets elem rep targets)
    elem))

(defun om-elem-match-replace-first (queries rep elem)
  "Replace first matching target in ELEM with REP.

QUERIES follows the same rules as `om-elem-match'."
  (-if-let (targets (om-elem-match queries elem))
      (om-elem--replace-targets elem rep (-take 1 targets))
    elem))

(defun om-elem-match-replace-last (queries rep elem)
  "Replace last matching target in ELEM with REP.

QUERIES follows the same rules as `om-elem-match'."
  (-if-let (targets (om-elem-match queries elem))
      (om-elem--replace-targets elem rep (-take-last 1 targets))
    elem))

;; insert-before

(defun om-elem--insert-targets-before (elem elem* targets)
  "Insert ELEM* before TARGETS in the contents of ELEM."
  (om-elem--modify-contents
   elem (--mapcat (if (member it targets) (list elem* it) (list it)) it)))

(defun om-elem-match-insert-before (queries elem* elem)
  "Insert ELEM* before every target matched by QUERIES in ELEM.

QUERIES follows the same rules as `om-elem-match'."
  (-if-let (targets (om-elem-match queries elem))
      (om-elem--insert-targets-before elem elem* targets)
    elem))

;; insert after

(defun om-elem--insert-targets-after (elem elem* targets)
  "Insert ELEM* after TARGETS in the contents of ELEM."
  (om-elem--modify-contents
   elem (--mapcat (if (member it targets) (list it elem*) (list it)) it)))

(defun om-elem-match-insert-after (queries elem* elem)
  "Insert ELEM* after every target matched by QUERIES in ELEM.

QUERIES follows the same rules as `om-elem-match'."
  (-if-let (targets (om-elem-match queries elem))
      (om-elem--insert-targets-after elem elem* targets)
    elem))

;; insert-within

(defun om-elem--insert-in (elem elem* index)
  "Insert ELEM* into the contents of ELEM at INDEX."
  (let* ((contents (om-elem--get-contents elem))
         (i (om-elem--normalize-insert-index index contents)))
      (om-elem--construct
       (nth 0 elem)
       (nth 1 elem)
       (-insert-at index elem* contents))))

(defun om-elem--normalize-insert-index (index list)
  "Return a positive integer from INDEX relative to front of LIST.
INDEX represents the position in between members of LIST where
something may be inserted or a split may occur. If INDEX is positive,
do nothing. If negative, '-1' is assumed to represent the position
behind the last member of LIST and decreasing integers move toward the
front."
  (if (<= 0 index) index (+ (length list) index 1)))

(defun om-elem--insert-within-targets (elem elem* index targets)
  "Insert ELEM* at INDEX within TARGETS in the contents of ELEM."
  (om-elem--modify-contents
   elem (if (not (member elem targets)) it
          (om-elem--insert-in it elem* index))))

(defun om-elem-match-insert-within (queries index elem* elem)
  "Insert new element ELEM* into the contents of ELEM at INDEX.
Will insert into any target matched by QUERIES. If QUERIES is not
supplied, ELEM* will be inserted directly into the toplevel contents
of ELEM.

QUERIES follows the same rules as `om-elem-match'."
  (if (-non-nil queries)
      (-if-let (targets (om-elem-match queries elem))
          (om-elem--insert-within-targets elem elem* index targets)
        elem)
    (om-elem--insert-in elem elem* index)))

;; splice

(defun om-elem--splice-targets (elem elems* targets)
  "Splice TARGETS with ELEMS* in the contents of ELEM."
  (om-elem--modify-contents
   elem (--mapcat (if (member it targets) elems* (list it)) it)))

(defun om-elem-match-splice (queries elem* elem)
  "Splice matching targets in ELEM with ELEMS*.

QUERIES follows the same rules as `om-elem-match'."
  (-if-let (targets (om-elem-match queries elem))
      (om-elem--splice-targets elem elems* targets)
    elem))

(defun om-elem-match-splice-first (queries elem* elem)
  "Splice first matching target in ELEM with ELEMS*.

QUERIES follows the same rules as `om-elem-match'."
  (-if-let (targets (om-elem-match queries elem))
      (om-elem--splice-targets elem elems* (-take 1 targets))
    elem))

(defun om-elem-match-splice-last (queries elem* elem)
  "Splice last matching target in ELEM with ELEMS*.

QUERIES follows the same rules as `om-elem-match'."
  (-if-let (targets (om-elem-match queries elem))
      (om-elem--splice-targets elem elems* (-take-last 1 targets))
    elem))

;; splice-before

(defun om-elem--splice-targets-before (elem elems* targets)
  "Splice ELEMS* before TARGETS in the contents of ELEM."
  (om-elem--modify-contents
   elem (--mapcat (if (member it targets)
                      (append elems* (list it))
                    (list it))
                  it)))

(defun om-elem-match-splice-before (queries elem* elem)
  "Splice ELEMS* before every target matched by QUERIES in ELEM.

QUERIES follows the same rules as `om-elem-match'."
  (-if-let (targets (om-elem-match queries elem))
      (om-elem--splice-targets-before elem elems* targets)
    elem))

;; splice-after

(defun om-elem--splice-targets-after (elem elems* targets)
  "Splice ELEMS* after TARGETS in the contents of ELEM."
  (om-elem--modify-contents
   elem (--mapcat (if (member it targets) (cons it elems*) (list it)) it)))

(defun om-elem-match-splice-after (queries elem* elem)
  "Splice ELEMS* after every target matched by QUERIES in ELEM.

QUERIES follows the same rules as `om-elem-match'."
  (-if-let (targets (om-elem-match queries elem))
      (om-elem--splice-targets-after elem elems* targets)
    elem))

;; splice-within

(defun om-elem--splice-at (elem elems* index)
  "Splice ELEMS* into the contents of ELEM at INDEX."
  (let* ((contents (om-elem--get-contents elem))
         (i (om-elem--normalize-insert-index index it)))
    (om-elem--construct
     (nth 0 elem)
     (nth 1 elem)
     (->> (split-at i contents)
          (-insert-at 1 elems*)
          (apply #'append)))))

(defun om-elem--splice-within-targets (elem elems* index targets)
  "Insert ELEM* at INDEX within TARGETS in the contents of ELEM."
  (om-elem--modify-contents
   elem (if (not (member elem targets)) it
            (om-elem--splice-at elems* index contents))))

(defun om-elem-match-splice-within (queries index elem* elem)
  "Insert list of ELEMS* into the contents of ELEM at INDEX.
Will insert into any target matched by QUERIES. If QUERIES is not
supplied, ELEM* will be inserted directly into the toplevel contents
of ELEM.

QUERIES follows the same rules as `om-elem-match'."
  (if (-non-nil queries)
      (-if-let (targets (om-elem-match queries elem))
          (om-elem--splice-within-targets elem elem* index targets)
        elem)
    (om-elem--splice-at elem elems* index)))

;; (defun om-elem-match-delete-in-place (elem query &rest queries)
;;   (let ((begin (om-elem--get-property :begin elem))
;;         (end (om-elem--get-property :end elem))
;;         (new-text (-> (apply #'om-elem-match-delete elem query queries)
;;                       (org-element-interpret-data)
;;                       (s-trim))))
;;     (delete-region begin end)
;;     (save-excursion
;;       (goto-char begin)
;;       (insert new-text))))

;; misc

(defun om-elem-clean (elem)
  "Recursively remove all empty elements from ELEM.
Has no effect on 'plain-text' elements."
  (cl-labels
      ((clean-rec
        (elem)
        (let ((type (om-elem--get-type elem)))
          (if (eq type 'plain-text) elem
            (->> (om-elem--get-contents elem)
                 (--remove
                  (and (om-elem--is-empty-p it)
                       (om-elem--is-any-type-p '(section plain-list) it)))
                 (--map (clean-rec it))
                 (append (list type (nth 1 elem))))))))
    (clean-rec elem)))

;; side-effects

(defun om-elem-match-do (queries fun elem)
  "Like `om-elem-match-map' but for side effects only.
FUN is function that side effects and takes on argument, the matches
from ELEM using QUERIES. This function itself returns nil.

QUERIES follows the same rules as `om-elem-match'."
  (-when-let (targets (om-elem-match queries elem))
      (--each (funcall fun it) targets)))

;; anaphoric forms

(--each '(om-elem-match-map
          om-elem-match-do
          om-elem-match-map-first
          om-elem-match-map-last
          om-elem-match-mapcat
          om-elem-match-mapcat-first
          om-elem-match-mapcat-last)
  (let ((fun-name (intern (format "%s*" it)))
        (doc-string (format "Anaphoric form of `%s'" it))
        (body `(backquote (,it ,',queries (lambda (it) ,',form ) ,',elem))))
    (eval `(defmacro ,fun-name (queries form elem)
                 ,doc-string ,body))))

(provide 'om-elem)
;;; om-elem.el ends here

