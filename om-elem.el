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

;;; better cl-defun
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

;;; type predicates

;; make a predicate function for all org elements and objects
(-> (append org-element-all-elements org-element-all-objects)
    (-distinct)
    (--each
        (let ((fun-name (intern (format "om-elem-is-%s-p" it)))
              (doc-string (format "Return t if E is an org element of type %s" it)))
          (eval `(defun ,fun-name (e) ,doc-string (eq ',it (org-element-type e)))))))

(--each org-element-object-restrictions
  (let ((fun-name
         (intern (format "om-elem-%s-allowed-object-p" (car it))))
        (doc-string
         (format "Return t if '%s' element is allowed to contain object O"
                 (car it)))
        (allowed (cdr it)))
    (eval `(defun ,fun-name (o)
             ,doc-string
             (memq (org-element-type o) (quote (plain-text ,@allowed)))))))

(defun om-elem-plain-text-p (elem)
  "Return t if org element ELEM is of type 'plain-text'."
  (eq 'plain-text (om-elem-type elem)))

(defun om-elem-element-list-p (l)
  "Return t is all in L are org elements (and not objects)."
  (--all? (eq 'element (org-element-class it)) l))

(defun om-elem-non-negative-integer-p (x)
  "Return t if X is a positive integer or 0."
  (and (integerp x) (<= 0 x)))

(defun om-elem-in-list-p (list m)
  "Return t if M is in LIST using EQ."
  (not (null (memq m list))))

(defun om-elem-valid-bullet-p (b)
  (or (integerp b) (memq b '(- +))))

;;; documentation functions

(defun om-elem--set-blank-contents (elem)
  "Set the contents of ELEM to a blank string (\"\")."
  (om-elem--set-contents '("") elem))

(defun om-elem-is-zero-length-p (elem)
  "Return t if ELEM will print as a blank string."
  (-if-let (contents (om-elem-contents elem))
      (-all? #'om-elem-is-zero-length-p contents)
    (and (om-elem-is-any-type-p om-elem--rm-if-empty elem)
         (om-elem-is-empty-p elem))))

(defconst om-elem--rm-if-empty
  '(table plain-list bold italic radio-target strike-through
          superscript subscript table-cell underline)
  "Elements/objects that will be blank if printed and empty.")

(defconst om-elem--blank-if-empty
  '(center-block drawer dynamic-block property-drawer quote-block
                 special-block verse-block)
  "Elements that require contents of \"\" to correctly print empty.
This is a workaround for a bug.")

(defun om-elem--allow-non-zero-length (elem)
  (unless (and (om-elem-is-empty-p elem)
               (or (om-elem-is-any-type-p om-elem--rm-if-empty elem)
                   (and (om-elem-is-type-p 'table-row elem)
                        (om-elem-property-is-eq-p :type 'standard elem))))
    elem))

(defun om-elem--clean (elem)
  (->> elem
       (om-elem--map-contents* (-non-nil (-map #'om-elem--clean it)))
       (om-elem--allow-non-zero-length)))

(defun om-elem--blank (elem)
  (if (om-elem-is-empty-p elem)
      (if (om-elem-is-any-type-p om-elem--blank-if-empty elem)
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

(defun om-elem-remove-properties (elem &rest props)
  (if (stringp elem)
      (progn
        (remove-text-properties
         0 (length elem) (--mapcat (list it nil) props) elem)
        elem)
    (let ((new-props
           (->> (om-elem-properties elem)
                (-partition 2)
                (--map (unless (memq (car it) props) it))
                (-non-nil)
                (apply #'append))))
      (append
       (list (om-elem-type elem) new-props)
       (om-elem-contents elem)))))

(defun om-elem-strip-parent (elem)
  (om-elem-remove-properties elem :parent))

(defun om-elem-strip-text-properties (elem)
  (cl-labels
      ((strip-props
        (e)
        (if (stringp e) e
          (let ((new-props
                 (->>
                  (om-elem-properties e)
                  (--map
                   ;; I think this covers everything? (strings and list
                   ;; of strings?)
                   (cond
                    ((stringp it)
                     (substring-no-properties it))
                    ((listp it)
                     (-map-when #'stringp #'substring-no-properties it))
                    (t it))))))
            (append (list (om-elem-type e) new-props)
                    (org-element-contents e)))))
       (map-rec
        (e)
        (if (stringp e) (substring-no-properties e)
          (->>
           (om-elem-contents e)
           (--map (strip-props it))
           (--map (map-rec it))
           (append (list (om-elem-type e) (nth 1 e)))))))
    (->> (strip-props elem) (map-rec))))

(defun om-elem-print (elem)
  (->> (om-elem-strip-parent elem)
       (om-elem-strip-text-properties)
       (format "%S")))

(defun om-elem-pretty-print (elem)
  (->> (om-elem-remove-properties elem :begin :end :contents-begin
                                  :contents-end :post-affiliated)
       (om-elem-print)))

(defun om-elem--indent-string (s)
  "Indent string S by 2 spaces.
Return new string.  If S is the empty string, return it."
  (if (equal "" s) s (replace-regexp-in-string "^ *\\S-" " \\&" s)))

;; TODO this is a terrible way to strip the properties and such
(defun om-elem-indent-print (elem)
  (let ((rem-props '(:begin :end :contents-begin :contents-end
                            :post-affiliated :parent :structure)))
    (cl-labels
        ((print-rec
          (e)
          (if (stringp e)
              (->> (s-trim e) (om-elem-print))
            (let ((head-str (format "%S %S" (org-element-type e)
                                    (nth 1 e))))
              (-if-let (contents (org-element-contents e))
                  (->> (--map (apply #'om-elem-remove-properties
                                     it rem-props)
                              contents)
                       (--map (print-rec it))
                       (s-join "\n")
                       (om-elem--indent-string)
                       (format "(%s\n%s)" head-str))
                (->> (format "(%s)" head-str)))))))
                     ;; (om-elem--indent-string)))))))
      (->> (apply #'om-elem-remove-properties elem rem-props)
           (om-elem-strip-text-properties)
           (print-rec)))))

;;; general element functions

(defalias 'om-elem-property 'org-element-property)
(defalias 'om-elem-contents 'org-element-contents)
(defalias 'om-elem-type 'org-element-type)
(defalias 'om-elem-class 'org-element-class)

(defun om-elem-head (elem)
  "Return the type and properties cells of ELEM."
  (if (stringp elem) elem
    (-take 2 elem)))

(defun om-elem-properties (elem)
  "Return the properties list of ELEM."
  (if (stringp elem) (text-properties-at 0 elem)
    (nth 1 elem)))

(defun om-elem-parent (elem)
  "Return the parent of ELEM."
  (om-elem-property :parent elem))

(defun om-elem-get-nested-content (indices elem)
  "Return the nested contents of ELEM as given by INDICES.
INDICES is a list of integers specifying the index and level of the
nested element to return."
  (if (not indices) elem
    (->> (om-elem-contents elem)
         (nth (car indices))
         (om-elem-get-nested-content (cdr indices)))))

(defun om-elem-from-string (string)
  "Convert STRING to org-element representation."
  (with-temp-buffer
    (insert string)
    (-> (om-elem-parse-this-buffer) (om-elem-contents) (car))))

(defun om-elem-build-secondary-string (string)
  "Return a list of elements from STRING as a secondary string."
  (->> (om-elem-from-string string)
       (om-elem-get-nested-content '(0))
       (om-elem-contents)))

;; argument verification

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

;;; helper functions

(defun om-elem--is-plist-p (obj)
  "Return t if OBJ is a plist."
  (and
   (listp obj)
   (cl-evenp (length obj))
   (-all? #'symbolp (-slice obj 0 nil 2))))

(defun om-elem--has-hour-min (time)
  (let ((len (length time)))
    (cond
     ((= len 3) nil)
     ((= len 5) t)
     (t (error "Invalid timestamp given:" time)))))

(defun om-elem-nullify-parent (elem)
  (om-elem-set-property :parent nil elem))


;;; internal setters

;; generic

(defun om-elem--set-property (prop value elem)
  "Set property PROP in element ELEM to VALUE."
  ;; TODO validate that prop exists in elem first?
  (if (stringp elem) (org-add-props elem nil prop value)
    (om-elem--elem-list
     (om-elem-type elem)
     (plist-put (om-elem-properties elem) prop value)
     (om-elem-contents elem))))

(defun om-elem--set-properties (plist elem)
  "Set all properties in ELEM to the values corresponding to PLIST.
PLIST is a list of property-value pairs that correspond to the
property list in ELEM."
  (cond
   ((not plist) elem)
   ((om-elem--is-plist-p plist)
    (->> (om-elem--set-property (nth 0 plist) (nth 1 plist) elem)
         (om-elem--set-properties (-drop 2 plist))))
   (t (error "Not a plist: %s" plist))))

(defun om-elem--set-property-nil (prop elem)
  "Set property PROP to nil in ELEM."
  (om-elem--set-property prop nil elem))

(defun om-elem--set-properties-nil (props elem)
  "Set all properties PROPS to new in ELEM."
  (let ((plist (--mapcat (list it nil) props)))
    (om-elem--set-properties plist elem)))

(defun om-elem--set-property-pred (fun prop value elem)
  (eval `(om-elem--verify value ,fun) `((value . ,value)))
  (om-elem--set-property prop value elem))

(defun om-elem--set-properties-pred (pred plist elem)
  "Set all properties in ELEM to the values corresponding to PLIST.
PLIST is a list of property-value pairs that correspond to the
property list in ELEM."
  (cond
   ((not plist) elem)
   ((om-elem--is-plist-p plist)
    (->> (om-elem--set-property-pred pred (nth 0 plist) (nth 1 plist) elem)
         (om-elem--set-properties-pred pred (-drop 2 plist))))
   (t (error "Not a plist: %s" plist))))

;; contents

(defun om-elem--set-contents (contents elem)
   (let ((head (om-elem-head elem)))
     (if contents (append head contents) head)))

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

(defun om-elem--set-contents-restricted (types contents elem)
  ;; TODO this should recursively dig up all types in contents
  ;; even if they are nested
  (-when-let (illegal (-some->> (-map #'om-elem-type contents)
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

;; misc

(defun om-elem--set-post-blank (post-blank elem)
  "Set the :post-blank property of ELEM to POST-BLANK."
  (om-elem--verify post-blank om-elem--non-neg-integer-p)
  (if (stringp elem) (s-append (s-repeat post-blank " ") elem)
    (om-elem--set-property :post-blank post-blank elem)))

(defun om-elem--set-brackets (flag elem)
  (om-elem--set-property-pred 'booleanp :use-brackets-p flag elem))

(defun om-elem--set-indent (flag elem)
  (om-elem--set-property-pred 'booleanp :preserve-indent flag elem))

(defun om-elem--set-key (key elem)
  "Set the key of ELEM element to KEY (a string)."
  (om-elem--set-property-pred 'stringp :key key elem))

(defun om-elem--set-value (value elem)
  "Set the value of ELEM element to VALUE (a string)."
  (om-elem--set-property-pred 'stringp :value value elem))

;; TODO make the inverse of this
(defun om-elem--set-property-list-string (prop args delim elem)
  (unless (and (listp args) (-all? #'stringp args))
    (error "Arguments must be supplied as a list of strings."))
  (let ((s (and args (s-join delim args))))
      (om-elem--set-property prop s elem)))

;; TODO make the inverse of this
(defun om-elem--set-property-plist (props plist elem)
  (unless (om-elem--is-plist-p plist)
    (error "Invalid plist given: %S" plist))
  (unless (->> (-slice plist 1 nil 2) (-all? #'symbolp))
    (error "All plist values must be symbols: %S" plist))
  (let ((s (-some->> (-map #'symbol-name plist) (s-join " "))))
      (om-elem--set-property props s elem)))

(defun om-elem--set-property-strings (prop strings elem)
  (om-elem--verify strings (lambda (ss) (-all? #'stringp ss)))
  (om-elem--set-property prop strings elem))

;; clock

;; (defun om-elem--clock-set-timestamp (start end clock)
;;   (let ((ts* (om-elem-build-timestamp 'inactive start :end end)))
;;     (om-elem--set-property :value ts* clock)))

(defun om-elem--clock-update-duration (clock)
  (cl-flet*
      ((encode-epoch
        (min hour day month year)
        (-> (encode-time 0 (or min 0) (or hour 0) day month year)
            (float-time)
            (round)))
       (format-duration
        (seconds)
        (let* ((h (-> seconds (/ 3600) (floor)))
               (m (-> seconds (- (* h 3600)) (/ 60) (floor))))
          (format "%2d:%02d" h m)))
       (get-duration
        (timestamp)
        (-let (((&plist :year-start y :year-end Y
                        :month-start m :month-end M
                        :day-start d :day-end D
                        :hour-start h :hour-end H
                        :minute-start n :minute-end N)
                (om-elem-properties timestamp)))
          (-> (- (encode-epoch N H D M Y) (encode-epoch n h d m y))
              (format-duration)))))
    (let ((ts (om-elem-property :value clock)))
      (if (om-elem-property-is-eq-p :type 'inactive-range ts)
          (om-elem--set-property :duration (get-duration ts) clock)
        (om-elem--set-property :duration nil clock)))))

;; diary-sexp

(defun om-elem--diary-sexp-set-value (string elem)
  (om-elem--verify string stringp)
  (om-elem--set-property :value (format "%%%%(%s)" string) elem))

;; entity

(defun om-elem--entity-set-name (name elem)
  (unless (or (assoc name org-entities-user) (assoc name org-entities)) 
    (error "Invalid entity: %S" name))
  (om-elem--set-property :name name elem))

;; headline

(defun om-elem--headline-set-pre-blank (pre-blank headline)
  ;; unlike post-blank, we assume this will never be needed for
  ;; plain text, so don't test for stringp here
  (om-elem--set-property-pred 'om-elem--non-neg-integer-p :pre-blank
                              pre-blank headline))

(defun om-elem--headline-set-todo (todo headline)
  "Set the todo keyword of HEADLINE element to TODO."
  (om-elem--set-property-pred 'string-or-null-p :todo-keyword todo
                              headline))

(defun om-elem--headline-set-level (level elem)
  (om-elem--verify level (lambda (L) (< 0 L)))
  (om-elem--set-property :level level elem))

(defun om-elem--headline-set-archived (flag headline)
  "Set the archived flag of HEADLINE element to FLAG."
  ;; TODO does the archive flag need to be set?
  ;; TODO abstract this in a set-tags function
  (let ((new-tags (--> (om-elem-property :tags headline)
                       (if flag (cons org-archive-tag it)
                         (-remove-item org-archive-tag it)))))
    (->> headline
         (om-elem--set-property-pred 'booleanp :archivedp flag)
         (om-elem--set-property :tags new-tags))))

(defun om-elem--headline-set-commented (flag headline)
  "Set the commented flag of HEADLINE element to FLAG."
  (om-elem--set-property-pred 'booleanp :commentedp flag headline))

(defun om-elem--headline-set-footnote-section (flag headline)
  "Set the footnote section flag of HEADLINE element to FLAG."
  (om-elem--set-property-pred 'booleanp :footnote-section-p flag
                              headline))

(defun om-elem--headline-set-priority (priority headline)
  "Set the priority of HEADLINE element to PRIORITY."
  (if (or (null priority) (and (>= org-lowest-priority priority)
                               (>= priority org-highest-priority)))
      (om-elem--set-property :priority priority headline)
    (error "Invalid priority given: %S" priority)))

(defconst om-elem--headline-title-restrictions
  (->> org-element-object-restrictions
       (alist-get 'headline)
       (cons 'plain-text)))

(defun om-elem--headline-set-title (title headline)
  "Set the title of HEADLINE element to TITLE."
  (unless
      (--all?
       (om-elem-is-any-type-p om-elem--headline-title-restrictions it)
       title)
    (error "Invalid title: %s" title))
  (om-elem--set-property :title title headline))

(defun om-elem--headline-set-title! (string stats headline)
  (let* ((ss (om-elem-build-secondary-string string))
         (title (if (not stats) ss
                  (-snoc ss (om-elem-build-statistics-cookie)))))
    (om-elem--headline-set-title title headline)))

;; item

(defun om-elem--item-set-checkbox (state item)
  "Set the checkbox of ITEM element to STATE.
STATE is one of 'on', 'off', 'trans'. Setting to nil removes the
checkbox."
  (unless (memq state '(nil on off trans))
    (error ("Invalid checkbox state: %s" state)))
  (om-elem--set-property :checkbox state item))

;; NOTE the org element parser currently does not honor 1) or a) type
;; bullets
(defun om-elem--item-validate-counter (counter)
  (if (integerp counter) counter
    (let ((s (symbol-name counter)))
      (when (s-matches? "^[a-zA-z]$" s)
        (if org-list-allow-alphabetical counter
          (error "Set `org-list-allow-alphabetical' to t to use alphabetical bullets"))))))

(defun om-elem--item-set-bullet (bullet item)
  (let ((b (if (memq bullet '(- +)) (format "%s " bullet)
             (-if-let (c (->> (if (listp bullet) (car bullet) bullet)
                              (om-elem--item-validate-counter)))
                 (format (if (listp bullet) "%s) " "%s. ") c)
               (error "Invalid bullet: %s" bullet)))))
    (om-elem--set-property :bullet b item)))

(defconst om-elem--item-tag-restrictions
  (->> org-element-object-restrictions
       (alist-get 'item)
       (cons 'plain-text)))

(defun om-elem--item-set-tag (tag item)
  "Set the tag of ITEM element to TAG where TAG is a string or nil."
  (unless (--all?
           (om-elem-is-any-type-p om-elem--item-tag-restrictions it)
           tag)
    (error "Invalid tag: %s" tag))
  (om-elem--set-property :tag tag item))

;; NOTE org mode 9.1.9 will crash when given an alphabetic symbol
(defun om-elem--item-set-counter (counter item)
  "Set the tag of ITEM element to COUNTER."
  (unless (or (null counter) (integerp counter))
  ;; (unless (or (null counter) (om-elem--item-validate-counter counter))
    (error "Invalid counter: %s" counter))
  (om-elem--set-property :counter counter item))

;; latex environment

(defun om-elem--latex-environment-set-value (env body latex-environment)
  (om-elem--verify env stringp body stringp)
  (let ((v (format "\\begin{%1$s}\n%2$s\n\\end{%1$s}" env body)))
    (om-elem--set-property :value v latex-environment)))

;; link

(defun om-elem--link-set-path (path link)
  "Set the path of LINK element to PATH (a string)."
  (om-elem--set-property-pred 'stringp :path path link))

(defun om-elem--link-set-type (type link)
  "Set the type of LINK element to TYPE (a symbol).
Setting TYPE to nil will result in a 'fuzzy' type link."
  (let ((valid-types (append (org-link-types)
                             (list "coderef" "custom-id" "file"
                                   "id" "radio" "fuzzy"))))
    (cond
     ((not type)
      (om-elem--set-property :type "fuzzy" link))
     ((member type valid-types)
      (om-elem--set-property :type type link))
     (t (error "Invalid link type: %S" type)))))

(defun om-elem--link-set-format (format link)
  (if (memq format '(nil plain angle bracket))
      (om-elem--set-property :format format link)
    (error "Invalid link format: %S" format)))

;; macro

(defun om-elem--macro-update-value (macro)
  (let* ((k (om-elem-property :key macro))
         (as (om-elem-property :args macro))
         (v (if as (format "%s(%s)" k (s-join "," as)) k)))
    (om-elem--set-property :value (format "{{{%s}}}" v) macro)))

;; planning

;; TODO add repeater/warning to this
(defun om-elem--planning-set-property (prop time planning)
  (if (not time) (om-elem--set-property prop nil planning)
    (let ((ts (om-elem-build-timestamp 'inactive time)))
      (om-elem--set-property prop ts planning))))

(defun om-elem--planning-set-closed (time planning)
  (om-elem--planning-set-property :closed time planning))

(defun om-elem--planning-set-deadline (time planning)
  (om-elem--planning-set-property :deadline time planning))

(defun om-elem--planning-set-scheduled (time planning)
  (om-elem--planning-set-property :scheduled time planning))

;; statistics cookie

(defun om-elem--statistics-cookie-set-value (value statistics-cookie)
  "Set the value or STATISTICS-COOKIE object with VALUE.
This is the internal version of `om-elem-statistics-cookie-set-value'
without element verification."
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
    (let ((value* (format "[%s]" (mk-stat value))))
      (om-elem--set-property :value value* statistics-cookie))))

(defun om-elem--statistics-cookie-get-format (statistics-cookie)
  (cond ((s-contains? "/" it) 'fraction)
        ((s-contains? "%" it) 'percent)
        (t (error "Unparsable statistics cookie value: %s"
                  (om-elem-property :value)))))

(defun om-elem--pad-time-maybe (time)
  (pcase time
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
    (_ (error "Invalid time: %S" time))))


;;; internal mappers
;; TODO, use setters here to ensure functions return the right type?

;; generic

(defun om-elem--map-first (fun list)
  (om-elem--verify fun functionp)
  (->> (cdr list) (cons (funcall fun (car list)))))

(defun om-elem--map-last (fun list)
  (->> (nreverse list) (om-elem--map-first fun) (nreverse)))

(defun om-elem--map-property (prop fun elem)
  (om-elem--verify fun functionp)
  (let ((value (funcall fun (om-elem-property prop elem))))
    (om-elem--set-property prop value elem)))

(defun om-elem--map-properties (plist elem)
  (cond
   ((not plist) elem)
   ((om-elem--is-plist-p plist)
    (->> (om-elem--map-property (nth 0 plist) (nth 1 plist) elem)
         (om-elem--map-properties (-drop 2 plist))))
   (t (error "Not a plist: %s" plist))))

(defun om-elem--shift-property (prop n elem)
  "Shift PROP of ELEM by N where N is a positive or negative integer."
  (om-elem--verify n integerp)
  (om-elem--map-property* prop (+ n it) elem))

;; contents

(defun om-elem--map-contents (fun elem)
  (let ((contents (om-elem-contents elem)))
    (om-elem--set-contents (funcall fun contents) elem)))

(defun om-elem--map-contained (pred fun elem)
  (om-elem--map-contents*
   (--map-when (funcall pred it) (funcall fun it) it)
   elem))

(defun om-elem--map-contained-first (pred fun elem)
  (om-elem--map-contents*
   (--map-first (funcall pred it) (funcall fun it) it)
   elem))

;; headline

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

(defun om-elem--headline-shift-level (n headline)
  (om-elem--verify n integerp)
  (cl-flet
      ((shift-level
        (cur-level)
        (let ((new-level (+ n cur-level))) ; of cooooonfideeeeence...
          (if (< 1 new-level) new-level 1)))) ; and pooooooweeeeer...
    (om-elem--map-property :level #'shift-level headline)))

(defun om-elem--headline-subtree-shift-level (n headline)
  (->> (om-elem--headline-shift-level n headline)
       (om-elem--headline-map-subheadlines
        (lambda (headlines)
          (--map (om-elem--headline-subtree-shift-level n it)
                 headlines)))))

;; node properties

(defun om-elem--node-property-map-value (fun node-property)
  (om-elem--map-property :value fun node-property))

;; anaphoric forms

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

(-each '(om-elem--map-first
         om-elem--map-last
         om-elem--map-property
         om-elem--map-contents
         om-elem--node-property-map-value)
  #'om-elem--gen-anaphoric-form)

(defmacro om-elem--map-properties* (plist elem)
  `(let ((plist*
          (-map-indexed
           (lambda (index item) (if (cl-evenp index) item `(lambda (it) ,item)))
           ,plist)))
     (om-elem--map-properties new-plist ,elem)))

;;; internal timestamp functions

;; time

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

(defun om-elem--unixtime-to-time-long (time)
  (nreverse (-slice (decode-time time) 1 6)))

(defun om-elem--unixtime-to-time-short (time)
  (-take 3 (om-elem--unixtime-to-time-long time)))

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

;; getters

(defun om-elem--timestamp-get-start-timestamp (timestamp)
  ;; TODO this is inefficient
  (let ((type (om-elem-property :type timestamp)))
    (if (not (memq type '(inactive-range active-range))) timestamp
      (org-timestamp-split-range timestamp))))

(defun om-elem--timestamp-get-end-timestamp (timestamp)
  ;; TODO this is inefficient
  (let ((type (om-elem-property :type timestamp)))
    (if (not (memq type '(inactive-range active-range))) nil
      (org-timestamp-split-range timestamp t))))

(defun om-elem--timestamp-get-start-time (timestamp)
  (-let (((&plist :minute-start n :hour-start h :day-start d
                  :month-start m :year-start y)
          (om-elem-properties timestamp)))
    `(,y ,m ,d ,h ,n)))

(defun om-elem--timestamp-get-end-time (timestamp)
  (-let (((&plist :minute-end n :hour-end h :day-end d
                  :month-end m :year-end y)
          (om-elem-properties timestamp)))
    `(,y ,m ,d ,h ,n)))

(defun om-elem--timestamp-get-start-unixtime (timestamp)
  (->> (om-elem--timestamp-get-start-time timestamp)
       (om-elem--time-to-unixtime)))

(defun om-elem--timestamp-get-end-unixtime (timestamp)
  (->> (om-elem--timestamp-get-end-time timestamp)
       (om-elem--time-to-unixtime)))

(defun om-elem--timestamp-is-ranged (timestamp)
  (/= (om-elem--timestamp-get-start-unixtime timestamp)
      (om-elem--timestamp-get-end-unixtime timestamp)))

;; setters

(defun om-elem--timestamp-set-type (type timestamp)
  (let* ((range? (om-elem--timestamp-is-ranged timestamp))
         (type* (cl-case type
                  (active (if range? 'active-range 'active))
                  (inactive (if range? 'inactive-range 'inactive))
                  (t (error "Invalid timestamp type: %s" type)))))
    (om-elem--set-property :type type* timestamp)))

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

(defun om-elem--timestamp-set-time (time timestamp)
  "Set the start TIME of TIMESTAMP."
  (-> (om-elem--time-format-props time 'start)
      (om-elem--set-properties timestamp)))

(defun om-elem--timestamp-set-time-end (time timestamp)
  "Set the end TIME of TIMESTAMP."
  (if time
      (-> (om-elem--time-format-props time 'end)
          (om-elem-set-properties timestamp))
    (-> (om-elem--timestamp-get-start-time timestamp)
        (om-elem--time-format-props 'end)
        (om-elem-set-properties timestamp))))

(defun om-elem--decorator-format (dec dtype valid-types)
  (let ((props (->> '(type value unit)
                    (--map (intern (format ":%s-%s" dtype it))))))
    (if (not dec) (om-elem--init-properties props)
      (-let (((type value unit) dec))
        (unless (memq type '(all first))
          (error "Invalid %s type: %s" dtype type))
        (unless (integerp value)
          (error "Invalid %s value: %s" dtype value))
        (unless (memq unit '(year month week day hour))
          (error "Invalid %s unit: %s" dtype value))
        (-interleave props (list type value unit))))))

(defun om-elem--timestamp-set-warning (warning timestamp)
  (let ((types '(all first)))
    (-> (om-elem--decorator-format warning 'warning types)
        (om-elem-set-properties timestamp))))

(defun om-elem--timestamp-set-repeater (repeater timestamp)
  (let ((types '(catch-up restart cumulative)))
    (-> (om-elem--decorator-format repeater 'repeater types)
        (om-elem-set-properties timestamp))))

(defun om-elem--timestamp-set-diary-sexp (string timestamp)
  (om-elem--verify string stringp)
  (om-elem--set-property :raw-value (format "<%%%%%s>" string)))

;; shifters

(defun om-elem--timestamp-shift-time-start (n unit timestamp)
  ;; TODO what if the start time is greater than the end time?
  (let ((time* (->> (om-elem--timestamp-get-start-time timestamp)
                    (om-elem--time-shift n unit))))
    (om-elem--timestamp-set-time time* timestamp)))

(defun om-elem--timestamp-shift-time-end (n unit timestamp)
  ;; TODO what if the end time is less than the start time?
  (let ((time-end (om-elem--timestamp-get-end-time timestamp)))
    (-> (om-elem--time-shift n unit time-end)
        (om-elem--timestamp-set-time-end timestamp))))

;;; internal content getters

;; headline

(defun om-elem--headline-get-subheadlines (headline)
  (-some->> (om-elem-contents headline)
            (--filter (om-elem-is-headline-p it))))

(defun om-elem--headline-get-section (headline)
  (-some->> (om-elem-contents headline) (assoc 'section)))

(defun om-elem--headline-get-statistics-cookie (headline)
  (->> (om-elem-property :title headline)
       (-last-item)
       (om-elem-allow-type 'statistics-cookie)))

;; table

(defun om-elem--table-get-height (table)
  (length (om-elem-contents table)))

(defun om-elem--table-get-width (table)
  (->> (om-elem-contents table) (-map #'length) (-max)))

;;; internal indentation

;; generic

;; TODO this is a bit sketchy...it depends on the indentation
;; function to make the contents list one element shorter, which
;; is usually true but makes a really hard error to catch when it
;; fails
(defun om-elem--indent-after (indent-fun index elem)
  (unless (and (integerp index) (<= 0 index))
    (error "Index must be non-negative integer"))
  (if (< index (1- (length (om-elem-contents elem))))
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
             (om-elem-contents)
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
        (->> (om-elem-contents parent)
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
               (->> (om-elem-contents target-item)
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
         (om-elem-contents parent)
         (-first #'om-elem-is-plain-list-p)
         (om-elem--indent-after #'om-elem--plain-list-indent-item-tree
                                child-index)
         (om-elem-contents)
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
        (->> (om-elem-contents parent)
             (-first #'om-elem-is-plain-list-p)
             (om-elem-contents))))
    (om-elem--map-contents
     (lambda (items)
       (om-elem--unindent-members index #'trim #'extract items))
     plain-list)))

;;; internal content setters

;; headline

(defun om-elem--headline-set-section (section headline)
  (let ((subheadlines (om-elem-headline-get-subheadlines headline)))
    (om-elem--set-contents (cons section subheadlines) headline)))

(defun om-elem--headline-set-property-drawer (property-drawer headline)
  (om-elem--headline-set-section (om-elem-build-section property-drawer)))

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

(defun om-elem--headline-set-node-property (key value headline)
  (om-elem--map-or-build-nested (om-elem--set-value value it)
    ((om-elem-is-section-p it)
     (om-elem-build-section)
     0)
    ((om-elem-is-property-drawer-p it)
     (om-elem-build-property-drawer)
     (-if-let (i (-find-index (om-elem-is-planning-p it) it)) ((1+ i) it)))
    ((and (om-elem-is-node-property-p it)
          (om-elem-property-is-equal-p :value key it))
     (om-elem-build-node-property key value) 
     0)
    headline))

(defun om-elem--headline-set-planning (planning headline)
  ;; TODO what if we give this a nil?
  (om-elem--map-or-build-nested (om-elem--set-value value it)
    ((om-elem-is-section-p it) (om-elem-build-section) 0)
    ((om-elem-is-planning-p it) (apply #'om-elem-build-planning planning) 0)
    headline))

(defun om-elem--headline-set-statistics-cookie (value headline)
  (om-elem--map-property*
   :title
   (let ((last? (om-elem-is-type-p 'statistics-cookie (-last-item it))))
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

;; table

;; TODO if I feel like being super extra I can add pivot operators :)

(defun om-elem--pad-or-truncate (length pad list)
  (let ((blanks (- length (length list))))
    (if (< 0 blanks) (-slice list 0 (1- length))
      (append list (-repeat blanks pad)))))

(defun om-elem--table-pad-or-truncate (length list)
  (let ((pad (om-elem-build-table-cell "")))
    (om-elem--pad-or-truncate length pad list)))

(defun om-elem--table-delete-column (index table)
  (om-elem--verify index integerp)
  (cl-flet*
      ((delete-cell
        (cells)
        (-remove-at index cells))
       (map-row 
        (row)
        (if (om-elem-property-is-eq-p :type 'rule row) row
          (om-elem--map-contents #'delete-cell row))))
    (om-elem--map-contents* (-map #'map-row it) table)))

(defun om-elem--table-delete-row (index table)
  (om-elem--verify index integerp)
  (om-elem--map-contents* (-remove-at index it) table))

(defun om-elem--table-insert-column (index column table)
  (om-elem--verify index integerp)
  (cl-flet*
      ((zip-into-rows
        (row new-cell)
        (if (om-elem-property-is-eq-p :type 'rule row) row
          (om-elem--map-contents
           (lambda (cells) (-insert-at index new-cell cells))
           row)))
       (map-rows
        (rows)
        (->> rows
             (--find-indices (om-elem-property-is-eq-p :type 'rule it))
             (--reduce-from (-insert-at it nil acc) column)
             (om-elem--table-pad-or-truncate (length rows))
             (-zip-with #'zip-into-rows rows))))
    (om-elem--map-contents #'map-rows table)))

(defun om-elem--table-insert-row (index row table)
  (om-elem--verify index integerp)
  (let ((row (if (om-elem-property-is-eq-p :type 'rule row) row
               (let ((width (om-elem--table-get-width table)))
                 (om-elem--table-pad-or-truncate width row)))))
    (om-elem--map-contents* (-insert-at index row it) table)))

;;; builders

;; build helpers

(defconst om-elem--object-properties
  '(:begin :end :post-blank :parent)
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
  (->> (om-elem--set-post-blank (or post-blank 0) `(,type nil))
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

;; objects

(om-elem--defun om-elem-build-code (value &key post-blank)
  "Build a code object from VALUE."
  (->> (om-elem--build-object 'code post-blank)
       (om-elem--set-value value)))

;; TODO this needs to be validated against `org-entity-get'
(om-elem--defun om-elem-build-entity (name &key use-brackets-p post-blank)
  "Build a entity object from NAME."
  (let ((init '(:html :ascii :latex :latex-math-p :latin1 :utf-8)))
    (->> (om-elem--build-object 'entity post-blank)
         (om-elem--set-brackets use-brackets-p)
         (om-elem--entity-set-name name)
         (om-elem--set-properties-nil init))))

(om-elem--defun om-elem-build-export-snippet (back-end value &key post-blank)
  "Build an export-block element with BACK-END and TYPE."
  (->> (om-elem--build-object 'export-snippet post-blank)
       (om-elem--set-value value)
       (om-elem--set-property-pred 'stringp :back-end back-end)))

(om-elem--defun om-elem-build-inline-babel-call (call &key post-blank
                                                      arguments
                                                      inside-header
                                                      end-header)
  "Build an inline-babel-call element for NAME.
Optionally provide ARGS, inside header args INSIDE, and end header
args END."
  (->>
   (om-elem--build-object 'inline-babel-call post-blank)
   (om-elem--set-property-pred 'stringp :call call)
   (om-elem--set-property-list-string :arguments arguments ",")
   (om-elem--set-property-plist :inside-header inside-header)
   (om-elem--set-property-plist :end-header end-header)
   (om-elem--set-property-nil :value)))

(om-elem--defun om-elem-build-inline-src-block (language value
                                                         &key
                                                         parameters
                                                         post-blank)
  "Build an inline-src-block object with LANGUAGE and VALUE.
Optionally provide PARAMETERS."
  (->> (om-elem--build-object 'inline-src-block post-blank)
       (om-elem--set-value value)
       (om-elem--set-property-pred 'stringp :language language)
       (om-elem--set-property-plist :parameters parameters)))

(om-elem--defun om-elem-build-latex-fragment (value &key post-blank)
  "Build a latex fragment object"
  (->> (om-elem--build-object 'latex-fragment post-blank)
       (om-elem--set-value value)))

(om-elem--defun om-elem-build-line-break (&key post-blank)
  "Build a line-break object."
  (om-elem--build-object 'line-break post-blank))

(om-elem--defun om-elem-build-macro (key &key args post-blank)
  "Build a macro object with KEY and optional ARGS."
  (->> (om-elem--build-object 'macro post-blank)
       (om-elem--set-key key)
       (om-elem--set-property-strings :args args)
       (om-elem--macro-update-value)))

(om-elem--defun om-elem-build-statistics-cookie (value &key post-blank)
  "Build a statistics cookie object with NUMBER and DENOMINATOR."
  (->> (om-elem--build-object 'statistics-cookie post-blank)
       (om-elem--statistics-cookie-set-value value)))

(om-elem--defun om-elem-build-target (value &key post-blank)
  "Build a target object with VALUE."
  (->> (om-elem--build-object 'target post-blank)
       (om-elem--set-value value)))

(om-elem--defun om-elem-build-timestamp (type start &key end
                                              repeater
                                              warning
                                              post-blank)
  "Build a timestamp..."
  (->> (om-elem--build-object 'timestamp post-blank)
       (om-elem--timestamp-set-time start)
       (om-elem--timestamp-set-time-end end)
       (om-elem--timestamp-set-type type)
       (om-elem--timestamp-set-warning warning)
       (om-elem--timestamp-set-repeater repeater)
       (om-elem--set-property-nil :raw-value)))

(om-elem--defun om-elem-build-diary-sexp-timestamp (string &key post-blank)
  "Build a diary-sexp timestamp element from STRING.
STRING is a lisp form as a string."
  (om-elem--verify string stringp)
  (let ((init :repeater-type :repeater-unit :repeater-value
               :warning-type :warning-unit :warning-value :year-start
               :month-start :day-start :hour-start :minute-start
               :year-end :month-end :day-end :hour-end :minute-end))
    (->> (om-elem--build-object 'timestamp post-blank)
         (om-elem--set-property :type 'diary)
         (om-elem--timestamp-set-diary-sexp string)
         (om-elem--init-properties init))))
        
(om-elem--defun om-elem-build-verbatim (value &key post-blank)
  "Build a verbatim object with VALUE."
  (->> (om-elem--build-object 'verbatim post-blank)
       (om-elem--set-value value)))

;; recursive objects

(om-elem--defun om-elem-build-bold (&key post-blank &rest objs)
  "Build a bold object containing OBJS."
  (om-elem--build-recursive-object 'bold post-blank objs))

(om-elem--defun om-elem-build-footnote-reference (&key label
                                                       post-blank
                                                       &rest objs)
  "Build a footnote reference object to TARGET."
  (->>
   (om-elem--build-recursive-object 'footnote-reference post-blank objs)
   (om-elem--set-property-pred 'string-or-null-p :label label)
   (om-elem--set-property-nil :type)))

(om-elem--defun om-elem-build-italic (&key post-blank &rest objs)
  "Build an italic object from STRING."
  (om-elem--build-recursive-object 'italic post-blank objs))

(om-elem--defun om-elem-build-link (path &key type format post-blank
                                         &rest objs)
  "Build a link object from TARGET with OBJS as the description."
  (let ((init '(:raw-link :application :search-option)))
    (->> (om-elem--build-recursive-object 'link post-blank objs)
         (om-elem--link-set-path path)
         (om-elem--link-set-type type)
         (om-elem--link-set-format format)
         (om-elem--set-properties-nil init))))

(om-elem--defun om-elem-build-radio-target (&key post-blank &rest objs)
  "Build a radio target object from STRING."
  (->> (om-elem--build-recursive-object 'radio-target post-blank objs)
       (om-elem--set-property-nil :value)))

(om-elem--defun om-elem-build-strike-through (&key post-blank &rest objs)
  "Build a strike-through object from STRING."
  (om-elem--build-recursive-object 'strike-through post-blank objs))

(om-elem--defun om-elem-build-superscript (&key use-brackets-p
                                                post-blank
                                                &rest objs)
  "Build a superscript object from STRING."
  (->> (om-elem--build-recursive-object 'superscript post-blank objs)
       (om-elem--set-brackets use-brackets-p)))

(om-elem--defun om-elem-build-subscript (&key use-brackets-p
                                              post-blank
                                              &rest objs)
  "Build a subscript object from STRING."
  (->> (om-elem--build-recursive-object 'subscript post-blank objs)
       (om-elem--set-brackets use-brackets-p)))

(om-elem--defun om-elem-build-table-cell (&key post-blank &rest objs)
  "Build a table cell object containing TEXT."
  (om-elem--build-recursive-object 'table-cell post-blank objs))

(om-elem--defun om-elem-build-underline (&key post-blank &rest objs)
  "Build an underline object from STRING."
  (om-elem--build-recursive-object 'underline post-blank objs))

;; elements

(om-elem--defun om-elem-build-babel-call (call &key arguments
                                               inside-header
                                               end-header post-blank)
  "Build a babel-call element for NAME."
  (->> (om-elem--build-element 'babel-call post-blank)
       (om-elem--set-property-pred 'stringp :call call)
       (om-elem--set-property-list-string :arguments arguments ",")
       (om-elem--set-property-plist :inside-header inside-header)
       (om-elem--set-property-plist :end-header end-header)
       (om-elem--set-property-nil :value)))

(om-elem--defun om-elem-build-clock (start &key end post-blank)
  "Build a clock element with TIME1.
Optionally supply TIME2 to create a closed clock."
  (let ((ts (om-elem-build-timestamp 'inactive start :end end)))
    (->> (om-elem--build-element 'clock post-blank)
         (om-elem--set-property :value ts)
         (om-elem--clock-update-duration)
         (om-elem--set-property-nil :status))))

(om-elem--defun om-elem-build-comment (value &key post-blank)
  "Build a comment element with VALUE."
  (->> (om-elem--build-element 'comment post-blank)
       (om-elem--set-value value)))

(om-elem--defun om-elem-build-comment-block (value &key post-blank)
  "Build a comment block element from VALUE."
  (->> (om-elem--build-element 'comment-block post-blank)
       (om-elem--set-value value)))

(om-elem--defun om-elem-build-diary-sexp (string &key post-blank)
  "Build a diary sexp element from VALUE.
VALUE is the part inside the '%%(value)' part of the sexp."
  (->> (om-elem--build-element 'diary-sexp post-blank)
       (om-elem--diary-sexp-set-value string)))

(om-elem--defun om-elem-build-example-block (value &key switches
                                                   preserve-indent
                                                   post-blank)
  "Build a example block element from STRING."
  (let ((init '(:number-lines :retain-labels :use-labels :label-fmt)))
    (->> (om-elem--build-element 'example-block post-blank)
         (om-elem--set-value (org-element-normalize-string value))
         (om-elem--set-property-list-string :switches switches " ")
         (om-elem--set-indent preserve-indent)
         (om-elem--set-properties-nil init))))

(om-elem--defun om-elem-build-export-block (type value &key post-blank)
  "Build an export-block element with TYPE and VALUE."
  (->> (om-elem--build-element 'export-block post-blank)
       (om-elem--set-value value)
       (om-elem--set-property-pred 'stringp :type type)))

(om-elem--defun om-elem-build-fixed-width (value &key post-blank)
  "Build a fixed-width element from STRING."
  (->> (om-elem--build-element 'fixed-width post-blank)
       (om-elem--set-value value)))

(om-elem--defun om-elem-build-horizontal-rule (&key post-blank)
  "Build a horizontal-rule element."
  (om-elem--build-element 'horizontal-rule post-blank))

(om-elem--defun om-elem-build-keyword (key value &key post-blank)
  "Build keyword element with keyword KEY and value VAL."
  (->> (om-elem--build-element 'keyword post-blank)
       (om-elem--set-key key)
       (om-elem--set-value value)))

(om-elem--defun om-elem-build-latex-environment (env body &key post-blank)
  "Build a latex-environment element with environment ENV and TEXT."
  (->> (om-elem--build-element 'latex-environment post-blank)
       (om-elem--latex-environment-set-value env body)))

(om-elem--defun om-elem-build-node-property (key value &key post-blank)
  "Build a node property object with KEY and VAL."
  (->> (om-elem--build-element 'node-property post-blank)
       (om-elem--set-key key)
       (om-elem--set-value value)))

(om-elem--defun om-elem-build-planning (&key closed scheduled deadline
                                             post-blank)
  "Build planning element with TYPE and TIME."
  (->> (om-elem--build-element 'planning post-blank)
       (om-elem--planning-set-closed closed)
       (om-elem--planning-set-deadline deadline)
       (om-elem--planning-set-scheduled scheduled)))

(om-elem--defun om-elem-build-src-block (value &key language switches
                                               parameters
                                               preserve-indent
                                               post-blank)
  (let ((init (list :preserve-indent :number-lines :retain-labels
                    :use-labels :label-fmt)))
    (->>
     (om-elem--build-element 'src-block post-blank)
     (om-elem--set-value value)
     (om-elem--set-indent preserve-indent)
     (om-elem--set-property-pred 'string-or-null-p :language language)
     (om-elem--set-property-list-string :switches switches " ")
     (om-elem--set-property-plist :parameters parameters)
     (om-elem--set-properties-nil init))))

;; container elements

(om-elem--defun om-elem-build-paragraph (&key post-blank &rest objs)
  "Build a paragraph container element with OBJECTS as contents."
  (om-elem--build-container-element 'paragraph post-blank objs))

(om-elem--defun om-elem-build-table-row (&key post-blank &rest objs)
  "Build a table-row container element with OBJECTS as contents."
  (->> (om-elem--build-container-element 'table-row post-blank objs)
       (om-elem--set-property :type 'standard)))

(om-elem--defun om-elem-build-table-row-hline (&key post-blank)
  (->> (om-elem--build-container-element 'table-row post-blank nil)
       (om-elem--set-property :type 'rule)))

(om-elem--defun om-elem-build-verse-block (&key post-blank &rest objs)
  "Build a verse-block container element with OBJECTS as contents."
  (om-elem--build-container-element 'verse-block post-blank objs))

;; greater elements

(om-elem--defun om-elem-build-center-block (&key post-blank &rest elems)
  "Build a center block greater element with ELEMS as contents."
  (om-elem--build-container-element 'center-block post-blank elems))

(om-elem--defun om-elem-build-drawer (drawer-name &key post-blank
                                                  &rest elems)
  "Create drawer greater element with NAME and ELEMS as contents."
  (->> (om-elem--build-container-element 'drawer post-blank elems)
       (om-elem--set-property-pred #'stringp :drawer-name drawer-name)))

(om-elem--defun om-elem-build-dynamic-block (block-name
                                             arguments
                                             &key post-blank
                                             &rest elems)
  "Build a dynamic block greater element called NAME with PARAMS.
PARAMS is s list of cons cells for each key/val pair. Optionally
provide ELEMS as contents."
  (->> (om-elem--build-container-element 'dynamic-block post-blank elems)
       (om-elem--set-property-pred #'stringp :block-name block-name)
       (om-elem--set-property-plist :arguments arguments)))

(om-elem--defun om-elem-build-footnote-definition (label
                                                   &key post-blank
                                                   &rest elems)
  "Build a footnote-definition greater element for LABEL.
Optionally provide ELEMS as contents."
  (->>
   (om-elem--build-container-element 'footnote-definition post-blank elems)
   (om-elem--set-property-pred #'stringp :label label)))

(om-elem--defun om-elem-build-headline (&key title (level 1)
                                             (pre-blank 0) todo-keyword
                                             tags priority
                                             footnote-section-p
                                             commentedp archivedp
                                             post-blank
                                             &rest elems)
  "Build a headline."
  (->> (om-elem--build-container-element 'headline post-blank elems)
       (om-elem--headline-set-pre-blank pre-blank)
       (om-elem--headline-set-todo todo-keyword)
       (om-elem--headline-set-title title)
       (om-elem--headline-set-level level)
       (om-elem--headline-set-priority priority)
       (om-elem--set-property-strings :tags tags)
       (om-elem--headline-set-footnote-section footnote-section-p)
       ;; this must go after setting tags since it alters the tags
       (om-elem--headline-set-archived archivedp)
       (om-elem--headline-set-commented commentedp)
       (om-elem--set-properties-nil '(:todo-type :raw-value))))

;; TODO add inline text

(om-elem--defun om-elem-build-item (&key (bullet '-) checkbox tag
                                         counter post-blank
                                         &rest elems)
  "Build a plain-list greater element with ELEMS as contents."
  (->> (om-elem--build-container-element 'item post-blank elems)
       (om-elem--item-set-bullet bullet)
       (om-elem--item-set-checkbox checkbox)
       (om-elem--item-set-tag tag)
       (om-elem--item-set-counter counter)
       (om-elem--set-property-nil :structure)))

(om-elem--defun om-elem-build-plain-list (&key post-blank &rest items)
  "Build a plain-list greater element with ELEMS as contents."
   (->> (om-elem--build-container-element 'plain-list post-blank items)
        (om-elem--set-properties-nil '(:structure :type))))

(om-elem--defun om-elem-build-property-drawer (&key post-blank &rest
                                                    node-properties)
  "Build a property-drawer greater element containing NODE-PROPERTIES."
  (om-elem--build-container-element 'property-drawer post-blank
                                    node-properties))

(om-elem--defun om-elem-build-quote-block (&key post-blank &rest elems)
  "Build a quote-block greater element with ELEMS as contents."
  (om-elem--build-container-element 'quote-block post-blank elems))

(om-elem--defun om-elem-build-section (&key post-blank &rest elems)
  "Build a section grater element with ELEMS as contents."
  (om-elem--build-container-element 'section post-blank elems))

(om-elem--defun om-elem-build-special-block (type &key post-blank
                                                  &rest elems)
  "Build a special block greater element with ELEMS as contents."
  (->>
   (om-elem--build-container-element 'special-block post-blank elems)
   (om-elem--set-property-pred #'stringp :type type)))

(om-elem--defun om-elem-build-table (&key tblfm post-blank &rest
                                          table-rows)
  "Build a section grater element containing TABLE-ROWS."
  ;; TODO this only deals with org tables for now
  (->> (om-elem--build-container-element 'table post-blank table-rows)
       (om-elem--set-property-strings :tblfm tblfm)
       (om-elem--set-property :type 'org)
       (om-elem--set-property-nil :value)))

;; shortcut builders

(om-elem--defun om-elem-build-headline! (&key (level 1) title-text
                                              todo-keyword tags
                                              pre-blank priority
                                              commentedp archivedp
                                              post-blank planning
                                              properties
                                              statistics-cookie
                                              section-contents
                                              subheadlines)
  "Build a headline..."
  (let* ((planning (-some->> planning (apply #'om-elem-build-planning)))
         (property-drawer (-some->> properties (apply #'om-elem-build-property-drawer!)))
         (section (-some->>
                   (append `(,planning) `(,property-drawer) section-contents)
                   (-non-nil)
                   (apply #'om-elem-build-section)))
         (elems (-non-nil (append (list section) subheadlines))))
    ;; TODO set these things using setter functions
    (->> (apply #'om-elem-build-headline
                :post-blank post-blank
                :pre-blank pre-blank
                :priority priority
                :commentedp commentedp
                :archivedp archivedp
                :title nil
                elems)
         (om-elem--headline-set-title! text statistics-cookie))))

(om-elem--defun om-elem-build-item! (&key post-blank bullet checkbox
                                          tag paragraph counter
                                          &rest subitems)
  "Build an item..."
  (let ((paragraph* (-some->> paragraph (om-elem-build-paragraph!)))
        (tag (-some->> tag (om-elem-build-secondary-string))))
    (->> (append (list paragraph* subitems))
         (-non-nil)
         (apply #'om-elem-build-item
                :post-blank post-blank
                :bullet bullet
                :checkbox checkbox
                :counter counter
                :tag tag))))

(defun om-elem-build-paragraph! (string)
  (let ((p (->> (om-elem-from-string string)
                (om-elem-get-nested-content '(0)))))
    (if (om-elem-is-type-p 'paragraph p) p
      (error "String could not be parsed to a paragraph: %s" string))))

(om-elem--defun om-elem-build-property-drawer! (&key post-blank &rest keyvals)
  "Create a property drawer org-element object from KEYVALS.
KEYVALS is a list of cons cells like (KEY . VAL) which will be
represented like ':KEY: VAL'."
  (->> (--map (om-elem-build-node-property (car it) (cdr it)) keyvals)
       (apply #'om-elem-build-property-drawer :post-blank post-blank)))

(om-elem--defun om-elem-build-table! (&key tblfm post-blank &rest rows)
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
    (->> (-map #'convert rows)
         (apply #'om-elem-build-table
                :tblfm tblfm
                :post-blank post-blank))))

;;; generalized CRUD operations

(defmacro om-elem--modify-contents (elem form)
  "Recursively modify the contents of ELEM using FORM.
FORM is a form that returns a list of elements or objects as the
new contents, and the variable 'it' is available to represent the
original contents to be modified."
  `(cl-labels
       ((rec
         (elem)
         (let ((type (om-elem-type elem)))
           (if (eq type 'plain-text) elem
             (->>
              (om-elem-contents elem)
              (funcall (lambda (it) ,form))
              (--map (rec it))
              (om-elem--elem-list type (nth 1 elem)))))))
     (rec elem)))

(defun om-elem--elem-list (type props contents)
  "Make a new org element list structure of TYPE, PROPS, and CONTENTS.
TYPE is a symbol, PROPS is a plist, and CONTENTS is a list or nil."
  `(,type ,props ,@contents))

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
     (--filter (om-elem-is-type-p type it) contents))

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
           (->> (-partition 2 (om-elem-properties elem))
                (-difference (-partition 2 props))
                (not))))
       (--filter (all-props-match? it plist) contents)))
    (_ (error "Invalid query: %s" query))))

(defun om-elem-find (queries elem)
  "Find all objects in ELEM that match QUERIES.

This will return a list of all successful matches. See
`om-elem-find-first' and `om-elem-find-last' to limit the return to
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
  ;; TODO validate elem (should be any valid element or object)
  (unless elem (error "No element given"))
  ;; the non-nil is required for cases where we may get
  ;; a nil for queries instead of no argument
  (let ((queries (-non-nil queries)))
    (when queries
      (let ((contents (om-elem-contents elem)))
        (pcase queries
          (`(:many! . (,q . nil))
           (let ((found (om-elem-filter-query q contents))
                 (q* (list :many! q)))
             (->> (-difference contents found)
                  (--mapcat (om-elem-find q* it))
                  (append found))))
          (`(:many! . ,_)
           (error "Query with :many! must have one target"))
          (`(:many . (,q . nil))
           (let ((found (om-elem-filter-query q contents))
                 (q* (list :many q)))
             (->> (--mapcat (om-elem-find q* it) contents)
                  (append found))))
          (`(:many . ,_)
           (error "Query with :many must have one target"))
          (`(:any . ,(and (pred and) qs))
           (--mapcat (om-elem-find qs it) contents))
          (`(:any . nil)
           contents)
          (`(,q . nil)
           (om-elem-filter-query q contents))
          (`(,q . ,qs)
           (->> (om-elem-filter-query q contents)
                (--mapcat (om-elem-find qs it))))
          (_ (error "Invalid query")))))))

(defun om-elem-find-first (queries elem )
  "Find first object in ELEM matching QUERIES.
The rules for QUERIES are the same as `om-elem-find'"
  (-first-item (om-elem-find queries elem)))

(defun om-elem-find-last (queries elem)
  "Find last object in ELEM matching QUERIES.
The rules for QUERIES are the same as `om-elem-find'"
  (-last-item (om-elem-find queries elem)))

;; find-parent

(defun om-elem-find-parent-query (parent query)
  (pcase query
    ;; type
    ((and (pred symbolp) type)
     ;; TODO check for valid type?
     (and (om-elem-is-type-p type parent) parent))
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
                              (om-elem-property it elem)))
                (-none? #'null))))
       (and (all-props-match? parent query) parent)))
    (_ (error "Invalid query: %s" query))))

(defun om-elem-find-parent (elem &rest queries)
  ;; TODO validate elem (should be any valid element or object)
  (unless elem (error "No element given"))
  ;; the non-nil is required for cases where we may get
  ;; a nil for queries instead of no argument
  (let ((queries (-non-nil queries)))
    (when queries
      (let ((parent (om-elem-parent elem)))
        (pcase queries
          (`(:many . (,q . nil))
           (or (om-elem-find-parent-query parent q)
               (om-elem-find-parent parent q)))
          (`(:many . ,_)
           (error "Query with :many must have one target"))
          (`(:any . ,(and (pred and) qs))
           (om-elem-find-parent parent q))
          (`(:any . nil)
           (error "Query with :any must have at least one target"))
          (`(,q . nil)
           (om-elem-find-parent-query parent q))
          (`(,q . ,qs)
           (-> (om-elem-find-parent-query parent q)
               (om-elem-find-parent qs)))
          (_ (error "Invalid query")))))))

;; delete

(defun om-elem--delete-targets (elem targets)
  "Delete TARGETS in the contents of ELEM."
  (om-elem--modify-contents
   elem (--remove (member it targets) it)))

(defun om-elem-delete (queries elem)
  "Remove matching targets from contents of ELEM.

QUERIES follows the same rules as `om-elem-find'."
  (-if-let (targets (om-elem-find queries elem))
      (om-elem--delete-targets elem targets)
    elem))

(defun om-elem-delete-first (queries elem)
  "Remove first matching target from contents of ELEM.

QUERIES follows the same rules as `om-elem-find'."
  (-if-let (targets (om-elem-find queries elem))
      (om-elem--delete-targets elem (-take 1 targets))
    elem))

(defun om-elem-delete-last (queries elem)
  "Remove last matching target from contents of ELEM.

QUERIES follows the same rules as `om-elem-find'."
  (-if-let (targets (om-elem-find queries elem))
      (om-elem--delete-targets elem (-take-last 1 targets))
    elem))

;; extract

(defun om-elem-extract (queries elem)
  "Remove matching targets from contents of ELEM.
Return cons cell where the car is a list of all removed targets
and the cdr is the modified ELEM with targets removed.

QUERIES follows the same rules as `om-elem-find'."
  (-if-let (targets (om-elem-find queries elem))
      (cons targets (om-elem--delete-targets elem targets))
    elem))

(defun om-elem-extract-first (queries elem)
  "Remove first matching target from contents of ELEM.
Return cons cell where the car is the removed target and the cdr is
the modified ELEM with targets removed.

QUERIES follows the same rules as `om-elem-find'."
  (-if-let (targets (om-elem-find queries elem))
      (let ((target (-take 1 targets)))
        (cons (car target) (om-elem--delete-targets elem target)))
    elem))

(defun om-elem-extract-last (queries elem)
  "Remove last matching target from contents of ELEM.
Return cons cell where the car is the removed target and the cdr is
the modified ELEM with targets removed.

QUERIES follows the same rules as `om-elem-find'."
  (-if-let (targets (om-elem-find queries elem))
      (let ((target (-take 1 targets)))
        (cons (car target) (om-elem--delete-targets elem target)))
    elem))

;; map

(defun om-elem--map-targets (elem fun targets)
  "Apply FUN to TARGETS in the contents of ELEM."
  (om-elem--modify-contents
   elem (--map-when (member it targets) (funcall fun it) it)))

(defun om-elem-map (queries fun elem)
  "Apply FUN to targets matching QUERIES in the contents of ELEM.
FUN is a function that takes a single argument (the target element or
object) and returns a new element or object which will replace the
original.

QUERIES follows the same rules as `om-elem-find'."
  (-if-let (targets (om-elem-find queries elem))
      (om-elem--map-targets elem fun targets)
    elem))

(defun om-elem-map-first (queries fun elem)
  "Apply FUN to first target matching QUERIES in the contents of ELEM.
FUN is a function that takes a single argument (the target element or
object) and returns a new element or object which will replace the
original.

QUERIES follows the same rules as `om-elem-find'."
  (-if-let (targets (om-elem-find queries elem))
      (om-elem--map-targets elem fun (-take 1 targets))
    elem))

(defun om-elem-map-last (queries fun elem)
  "Apply FUN to last target matching QUERIES in the contents of ELEM.
FUN is a function that takes a single argument (the target element or
object) and returns a new element or object which will replace the
original.

QUERIES follows the same rules as `om-elem-find'."
  (-if-let (targets (om-elem-find queries elem))
      (om-elem--map-targets elem fun (-take-last 1 targets))
    elem))

;; (defmacro om-elem-map* (elem form &rest queries)
;;   `(om-elem-map ,elem (lambda (it) ,form) ,@queries))

;; (defmacro om-elem-map-first* (elem form &rest queries)
;;   `(om-elem-map-first ,elem (lambda (it) ,form) ,@queries))

;; (defmacro om-elem-map-last* (elem form &rest queries)
;;   `(om-elem-map-last ,elem (lambda (it) ,form) ,@queries))

;; mapcat

(defun om-elem--mapcat-targets (elem fun targets)
  "Apply FUN to TARGETS in the contents of ELEM."
  (om-elem--modify-contents
   elem (--mapcat (if (member it targets)
                      (funcall fun it) (list it))
                  it)))

(defun om-elem-mapcat (queries fun elem)
  "Apply FUN over ELEM and return modified ELEM.
FUN takes an element/object as its only argument and returns
a list of elements/objects. Targets within ELEM are found that match
QUERIES, FUN is applied to each target, and the resulting list is
spliced in place of the original target (as opposed to `om-elem-map'
which replaces the original target with a modified target).

QUERIES follows the same rules as `om-elem-find'."
  (-if-let (targets (om-elem-find queries elem))
      (om-elem--mapcat-targets elem fun targets)
    elem))

(defun om-elem-mapcat-first (queries fun elem)
  "Like `om-elem-mapcat' but only apply FUN to first match in ELEM.

QUERIES follows the same rules as `om-elem-find'."
  (-if-let (targets (om-elem-find queries elem))
      (om-elem--mapcat-targets elem fun (-take 1 targets))
    elem))

(defun om-elem-mapcat-last (queries fun elem)
  "Like `om-elem-mapcat' but only apply FUN to last match in ELEM.

QUERIES follows the same rules as `om-elem-find'."
  (-if-let (targets (om-elem-find queries elem))
      (om-elem--mapcat-targets elem fun (-take-last 1 targets))
    elem))

;; (defmacro om-elem-mapcat* (elem form &rest queries)
;;   "Anaphoric form of `om-elem-mmapcat'."
;;   `(om-elem-mapcat ,elem (lambda (it) ,form) ,@queries))

;; (defmacro om-elem-mapcat-first* (elem form &rest queries)
;;   "Anaphoric form of `om-elem-mmapcat-first'."
;;   `(om-elem-mapcat-first ,elem (lambda (it) ,form) ,@queries))

;; (defmacro om-elem-mapcat-last* (elem form &rest queries)
;;   "Anaphoric form of `om-elem-mmapcat-last'."
;;   `(om-elem-mapcat-last ,elem (lambda (it) ,form) ,@queries))

;; replace

(defun om-elem--replace-targets (elem rep targets)
  "Replace TARGETS with REP in the contents of ELEM."
  (om-elem--modify-contents
   elem (--map-when (member it targets) rep it)))

(defun om-elem-replace (queries rep elem)
  "Replace matching targets in ELEM with REP.

QUERIES follows the same rules as `om-elem-find'."
  (-if-let (targets (om-elem-find queries elem))
      (om-elem--replace-targets elem rep targets)
    elem))

(defun om-elem-replace-first (queries rep elem)
  "Replace first matching target in ELEM with REP.

QUERIES follows the same rules as `om-elem-find'."
  (-if-let (targets (om-elem-find queries elem))
      (om-elem--replace-targets elem rep (-take 1 targets))
    elem))

(defun om-elem-replace-last (queries rep elem)
  "Replace last matching target in ELEM with REP.

QUERIES follows the same rules as `om-elem-find'."
  (-if-let (targets (om-elem-find queries elem))
      (om-elem--replace-targets elem rep (-take-last 1 targets))
    elem))

;; insert-before

(defun om-elem--insert-targets-before (elem elem* targets)
  "Insert ELEM* before TARGETS in the contents of ELEM."
  (om-elem--modify-contents
   elem (--mapcat (if (member it targets) (list elem* it) (list it)) it)))

(defun om-elem-insert-before (queries elem* elem)
  "Insert ELEM* before every target matched by QUERIES in ELEM.

QUERIES follows the same rules as `om-elem-find'."
  (-if-let (targets (om-elem-find queries elem))
      (om-elem--insert-targets-before elem elem* targets)
    elem))

;; insert after

(defun om-elem--insert-targets-after (elem elem* targets)
  "Insert ELEM* after TARGETS in the contents of ELEM."
  (om-elem--modify-contents
   elem (--mapcat (if (member it targets) (list it elem*) (list it)) it)))

(defun om-elem-insert-after (queries elem* elem)
  "Insert ELEM* after every target matched by QUERIES in ELEM.

QUERIES follows the same rules as `om-elem-find'."
  (-if-let (targets (om-elem-find queries elem))
      (om-elem--insert-targets-after elem elem* targets)
    elem))

;; insert-within

(defun om-elem--insert-at (elem elem* index)
  "Insert ELEM* into the contents of ELEM at INDEX."
  (let* ((contents (om-elem-contents elem))
         (i (om-elem--normalize-insert-index index contents)))
      (om-elem--elem-list
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
          (om-elem--insert-at it elem* index))))

(defun om-elem-insert-within (queries index elem* elem)
  "Insert new element ELEM* into the contents of ELEM at INDEX.
Will insert into any target matched by QUERIES. If QUERIES is not
supplied, ELEM* will be inserted directly into the toplevel contents
of ELEM.

QUERIES follows the same rules as `om-elem-find'."
  (if (-non-nil queries)
      (-if-let (targets (om-elem-find queries elem))
          (om-elem--insert-within-targets elem elem* index targets)
        elem)
    (om-elem--insert-at elem elem* index)))

;; splice

(defun om-elem--splice-targets (elem elems* targets)
  "Splice TARGETS with ELEMS* in the contents of ELEM."
  (om-elem--modify-contents
   elem (--mapcat (if (member it targets) elems* (list it)) it)))

(defun om-elem-splice (queries elem* elem)
  "Splice matching targets in ELEM with ELEMS*.

QUERIES follows the same rules as `om-elem-find'."
  (-if-let (targets (om-elem-find queries elem))
      (om-elem--splice-targets elem elems* targets)
    elem))

(defun om-elem-splice-first (queries elem* elem)
  "Splice first matching target in ELEM with ELEMS*.

QUERIES follows the same rules as `om-elem-find'."
  (-if-let (targets (om-elem-find queries elem))
      (om-elem--splice-targets elem elems* (-take 1 targets))
    elem))

(defun om-elem-splice-last (queries elem* elem)
  "Splice last matching target in ELEM with ELEMS*.

QUERIES follows the same rules as `om-elem-find'."
  (-if-let (targets (om-elem-find queries elem))
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

(defun om-elem-splice-before (queries elem* elem)
  "Splice ELEMS* before every target matched by QUERIES in ELEM.

QUERIES follows the same rules as `om-elem-find'."
  (-if-let (targets (om-elem-find queries elem))
      (om-elem--splice-targets-before elem elems* targets)
    elem))

;; splice-after

(defun om-elem--splice-targets-after (elem elems* targets)
  "Splice ELEMS* after TARGETS in the contents of ELEM."
  (om-elem--modify-contents
   elem (--mapcat (if (member it targets) (cons it elems*) (list it)) it)))

(defun om-elem-splice-after (queries elem* elem)
  "Splice ELEMS* after every target matched by QUERIES in ELEM.

QUERIES follows the same rules as `om-elem-find'."
  (-if-let (targets (om-elem-find queries elem))
      (om-elem--splice-targets-after elem elems* targets)
    elem))

;; splice-within

(defun om-elem--splice-at (elem elems* index)
  "Splice ELEMS* into the contents of ELEM at INDEX."
  (let* ((contents (om-elem-contents elem))
         (i (om-elem--normalize-insert-index index it)))
    (om-elem--elem-list
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

(defun om-elem-splice-within (queries index elem* elem)
  "Insert list of ELEMS* into the contents of ELEM at INDEX.
Will insert into any target matched by QUERIES. If QUERIES is not
supplied, ELEM* will be inserted directly into the toplevel contents
of ELEM.

QUERIES follows the same rules as `om-elem-find'."
  (if (-non-nil queries)
      (-if-let (targets (om-elem-find queries elem))
          (om-elem--splice-within-targets elem elem* index targets)
        elem)
    (om-elem--splice-at elem elems* index)))

;; (defun om-elem-delete-in-place (elem query &rest queries)
;;   (let ((begin (om-elem-property :begin elem))
;;         (end (om-elem-property :end elem))
;;         (new-text (-> (apply #'om-elem-delete elem query queries)
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
        (let ((type (om-elem-type elem)))
          (if (eq type 'plain-text) elem
            (->> (om-elem-contents elem)
                 (--remove
                  (and (om-elem-is-empty-p it)
                       (om-elem-is-any-type-p '(section plain-list) it)))
                 (--map (clean-rec it))
                 (append (list type (nth 1 elem))))))))
    (clean-rec elem)))

;; side-effects

(defun om-elem-find-do (queries fun elem)
  "Like `om-elem-map' but for side effects only.
FUN is function that side effects and takes on argument, the matches
from ELEM using QUERIES. This function itself returns nil.

QUERIES follows the same rules as `om-elem-find'."
  (-when-let (targets (om-elem-find queries elem))
      (--each (funcall fun it) targets)))

;; anaphoric forms

(--each '(om-elem-map
          om-elem-find-do
          om-elem-map-first
          om-elem-map-last
          om-elem-mapcat
          om-elem-mapcat-first
          om-elem-mapcat-last)
  (let ((fun-name (intern (format "%s*" it)))
        (doc-string (format "Anaphoric form of `%s'" it))
        (body `(backquote (,it ,',queries (lambda (it) ,',form ) ,',elem))))
    (eval `(defmacro ,fun-name (queries form elem)
                 ,doc-string ,body))))

;;; element meta-getters

;; generic

;; (defun om-elem-parent-get-headline (elem)
;;   "Return the most immediate parent headline of ELEM."
;;   (om-elem-find-parent elem :many 'headline))

;; (defun om-elem-parent-get-item (elem)
;;   "Return the parent item element for ELEM."
;;   (om-elem-find-parent elem :many 'item))

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
  "Return first drawer with NAME in HEADLINE element or nil if none."
  (om-elem--verify name stringp
                   headline om-elem-is-headline-p)
  (om-elem-find-first `(section (:drawer-name ,name)) headline))

;; (defun om-elem-headline-logbook (elem)
;;   "Return contents of headline ELEM's logbook drawer or nil if none."
;;   ;; TODO what if org-log-into-drawer is nil?
;;   (-some-->
;;    (om-elem-headline-section elem)
;;    (om-elem-find-first it `(:drawer-name ,org-log-into-drawer))
;;    (om-elem-contents it)))

(defun om-elem-headline-get-path (headline)
  "Return path of headline HEADLINE element as a list of strings."
  (om-elem--verify headline om-elem-is-headline-p)
  (cl-labels
      ((get-path
        (hl)
        (let ((title (om-elem-property :raw-value hl)))
          (-if-let (parent (om-elem-parent-get-headline hl))
              (cons title (get-path parent))
            (list title)))))
    (reverse (get-path headline))))

;; item

(defun om-elem-item-get-level (item)
  "Return the level of ITEM element item (1 indexed)."
  (om-elem--verify item om-elem-is-item-p)
  (cl-labels
      ((get-level
        (item acc)
        (let ((parent (->> (om-elem-property :parent item)
                           (om-elem-property :parent))))
          (if (om-elem-item-p parent)
              (get-level parent (+ 1 acc))
            acc))))
    (get-level item 1)))

(defun om-elem-item-get-sublist (item)
  "Return plain-list under ITEM element or nil if none."
  (om-elem--verify item om-elem-is-item-p)
  (-some->> (om-elem-contents item) (assoc 'plain-list)))

(defun om-elem-item-get-paragraph (item)
  "Return paragraph under ITEM element or nil if none."
  (om-elem--verify item om-elem-is-item-p)
  (-some->> (om-elem-contents item) (assoc 'paragraph)))

;; table

;; TODO add negative indices
(defun om-elem-table-get-cell (row column table)
  "Return table-cell element at ROW and COLUMN indices in TABLE element.
Hlines do not count toward row indices, and all indices are
zero-indexed."
  (om-elem--verify table om-elem-is-table-p)
  (-some->> (om-elem-contents table)
            (--filter (om-elem-property-is-eq-p :type 'standard it))
            (nth row)
            (om-elem-contents)
            (nth column)))

;; timestamp

(defun om-elem-timestamp-get-start-timestamp (timestamp)
  "Return the start of TIMESTAMP element."
  (om-elem--verify timestamp om-elem-is-timestamp-p)
  (om-elem--timestamp-get-start-timestamp timestamp))

(defun om-elem-timestamp-get-end-timestamp (timestamp)
  "Return the end of TIMESTAMP element or nil if not present."
  (om-elem--verify timestamp om-elem-is-timestamp-p)
  (if (not (om-elem--timestamp-is-ranged timestamp)) nil
    (om-elem--timestamp-get-end-timestamp timestamp)))

(defun om-elem-timestamp-get-start-unixtime (timestamp)
  "Return the unixtime value of TIMESTAMP element as an integer.
Note this only considers the start of the timestamp if it is range."
  (om-elem--verify timestamp om-elem-is-timestamp-p)
  (om-elem--timestamp-get-start-unixtime timestamp))

(defun om-elem-timestamp-get-end-unixtime (timestamp)
  "Return the unixtime value of TIMESTAMP's end as an integer."
  (om-elem--verify timestamp om-elem-is-timestamp-p)
  (if (not (om-elem--timestamp-is-ranged timestamp)) nil
    (om-elem--timestamp-get-end-unixtime)))

;;; element predicates

;; generic

(defun om-elem-is-empty-p (elem)
  "Return t if ELEM has no contents."
  (not (om-elem-contents elem)))

(defun om-elem-property-is-nil-p (prop elem)
  "Return t if PROP in ELEM is nil."
  (not (om-elem-property prop elem)))

(defun om-elem-property-is-non-nil-p (prop elem)
  "Return t if PROP in ELEM is not nil."
  (if (om-elem-property prop elem) t))

(defun om-elem-property-is-eq-p (prop val elem)
  "Return t if PROP in ELEM is `eq' to VAL."
  (eq val (om-elem-property prop elem)))

(defun om-elem-property-is-equal-p (prop val elem)
  "Return t if PROP in ELEM is `equal' to VAL."
  (equal val (om-elem-property prop elem)))

(defun om-elem-property-is-predicate-p (prop fun elem)
  "Return t if FUN applied to the value of PROP in ELEM results not nil.
FUN is a predicate function that takes one argument."
  (->> (om-elem-property prop elem) (funcall fun) (and)))

(defmacro om-elem-property-is-predicate-p* (prop form elem)
  "Anaphoric form of `om-elem-property-is-predicate-p'."
  `(om-elem-property-is-predicate-p ,prop (lambda (it) ,form) ,elem))

(defun om-elem-contains-point-p (point elem)
  "Return t if integer POINT is within the beginning and end of ELEM."
  (<= (om-elem-property :begin elem) point
      (om-elem-property :end elem)))

(defun om-elem-contents-contains-point-p (point elem)
  "Return t if integer POINT is within the beginning and end of ELEM's contents."
  (<= (om-elem-property :contents-begin elem) point
      (om-elem-property :contents-end elem)))

;; type

(defvaralias 'om-elem-elements 'org-element-all-elements)
(defvaralias 'om-elem-object-containers 'org-element-object-containers)
(defvaralias 'om-elem-recursive-objects 'org-element-recursive-objects)
(defvaralias 'om-elem-greater-elements 'org-element-greater-elements)

(defconst om-elem-objects (cons 'plain-text org-element-all-objects)
  "List of all object types including 'plain-text'.")

(defconst om-elem-elements-and-objects
  (append om-elem-elements om-elem-objects)
  "List of all elements and objects.")

(defconst om-elem-containers
  (append om-elem-greater-elements
          om-elem-object-containers)
  "List of elements/objects that can hold other elements/objects.")

(defconst om-elem-container-elements
  (append om-elem-greater-elements
          om-elem-object-containers)
  "List of elements/objects that can hold other elements/objects.")

;; TODO this naming is stupid
(defconst om-elem-atomic-elements
  (-> om-elem-elements
      (-difference om-elem-greater-elements)
      (-difference om-elem-object-containers))
  "List of elements that are not containers.")

(defconst om-elem-atomic-objects
  (-> om-elem-objects
      (-difference om-elem-recursive-objects))
  "List of objects that are not containers.")


(defconst om-elem-atoms
  (append om-elem-atomic-objects om-elem-atomic-elements)
  "List of objects and elements that are not containers.")

(defun om-elem-is-type-p (type elem)
  "Return t if ELEM's type is `eq' to TYPE (a symbol)."
  (eq (om-elem-type elem) type))

(defun om-elem-is-any-type-p (types elem)
  "Return t if ELEM's type is any in TYPES (a list of symbols)."
  (if (memq (om-elem-type elem) types) t))

(defun om-elem-is-object-p (elem)
  "Return t is ELEM is an object."
  (om-elem-is-any-type-p om-elem-objects elem))

(defun om-elem-is-element-p (elem)
  "Return t is ELEM is an element."
  (om-elem-is-any-type-p om-elem-elements elem))

(defun om-elem-is-element-or-object-p (elem)
  "Return t is ELEM is an element or an object."
  (om-elem-is-any-type-p om-elem-elements-and-objects elem))

(defun om-elem-is-greater-element-p (elem)
  "Return t is ELEM is a greater element."
  (om-elem-is-any-type-p om-elem-greater-elements elem))

(defun om-elem-is-container-p (elem)
  "Return t is ELEM is a container."
  (om-elem-is-any-type-p om-elem-containers elem))

(defun om-elem-is-recursive-object-p (elem)
  "Return t is ELEM is a recursive object."
  (om-elem-is-any-type-p om-elem-recursive-objects elem))

(defun om-elem-is-allowed-object-p (container-type elem)
  "Return t if object ELEM is allowed to be in CONTAINER-TYPE."
  (-if-let (r (alist-get container-type om-elem-object-restrictions))
      (om-elem-is-any-type-p r elem)
    (error "Invalid container type requested: %s" container-type)))

;; clock

(defun om-elem-clock-is-running-p (clock)
  "Return t if CLOCK element is running (eg is open)."
  (om-elem--verify clock om-elem-is-clock-p)
  (om-elem-property-is-eq-p :status 'running clock))

;; headline

(defun om-elem-headline-is-done-p (headline)
  "Return t if HEADLINE element has a DONE todo keyword."
  (om-elem--verify headline om-elem-is-headline-p)
  (om-elem-property-is-eq-p :todo-type 'done headline))

(defun om-elem-headline-is-scheduled-p (headline)
  "Return t if HEADLINE element is scheduled."
  (om-elem--verify headline om-elem-is-headline-p)
  (om-elem-property-is-non-nil-p :scheduled headline))

(defun om-elem-headline-is-deadlined-p (headline)
  "Return t if HEADLINE element has a deadline."
  (om-elem--verify headline om-elem-is-headline-p)
  (om-elem-property-is-non-nil-p :deadline headline))

(defun om-elem-headline-is-closed-p (headline)
  "Return t if HEADLINE element is closed."
  (om-elem--verify headline om-elem-is-headline-p)
  (om-elem-property-is-non-nil-p :closed headline))

;; (defun om-elem-headline-is-quoted-p (headline)
;;   "Return t if HEADLINE element is quoted."
;;   (om-elem-property-is-non-nil-p :quotedp headline))

(defun om-elem-headline-is-archived-p (headline)
  "Return t if HEADLINE element is archived."
  (om-elem--verify headline om-elem-is-headline-p)
  (om-elem-property-is-non-nil-p :archivedp headline))

(defun om-elem-headline-is-commented-p (headline)
  "Return t if HEADLINE element is commented."
  (om-elem--verify headline om-elem-is-headline-p)
  (om-elem-property-is-non-nil-p :commentedp headline))

(defun om-elem-headline-has-tag-p (tag headline)
  "Return t if HEADLINE element is tagged with TAG."
  (om-elem--verify headline om-elem-is-headline-p)
  (if (member tag (om-elem-property :tags headline)) t))

;; item

(defun om-elem-item-is-checked-p (item)
  "Return t if ITEM element is checked."
  (om-elem--verify item om-elem-is-item-p)
  (om-elem-property-is-eq-p :checkbox 'on item))

(defun om-elem-item-is-unchecked-p (item)
  "Return t if ITEM element is unchecked."
  (om-elem--verify item om-elem-is-item-p)
  (om-elem-property-is-eq-p :checkbox 'off item))

(defun om-elem-item-is-trans-p (item)
  "Return t if ITEM element is transitional."
  (om-elem--verify item om-elem-is-item-p)
  (om-elem-property-is-eq-p :checkbox 'trans item))

;; statistics cookie

(defun om-elem-statistics-cookie-is-complete-p (statistics-cookie)
  "Return t is STATISTICS-COOKIE element is complete."
  (om-elem--verify statistics-cookie om-elem-is-statistics-cookie-p)
  (let ((val (om-elem-property :value statistics-cookie)))
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

;; timestamp

(defun om-elem-timestamp-is-active-p (timestamp)
  "Return t if TIMESTAMP elem is active."
  (om-elem--verify timestamp om-elem-is-timestamp-p)
  (or (om-elem-property-is-eq-p :type 'active timestamp)
      (om-elem-property-is-eq-p :type 'active-range timestamp)))

(defun om-elem-timestamp-is-inactive-p (timestamp)
  "Return t if TIMESTAMP elem is inactive."
  (om-elem--verify timestamp om-elem-is-timestamp-p)
  (or (om-elem-property-is-eq-p :type 'inactive timestamp)
      (om-elem-property-is-eq-p :type 'inactive-range timestamp)))

(defun om-elem-timestamp-is-ranged-p (timestamp)
  "Return t if TIMESTAMP elem is ranged."
  (om-elem--verify timestamp om-elem-is-timestamp-p)
  (or (om-elem-property-is-eq-p :type 'active-range timestamp)
      (om-elem-property-is-eq-p :type 'inactive-range timestamp)))

(defun om-elem-timestamp-is-less-than-p (unixtime timestamp)
  "Return t if TIMESTAMP elem is less than UNIXTIME."
  (om-elem--verify timestamp om-elem-is-timestamp-p)
  (< (om-elem-timestamp-get-unixtime timestamp) unixtime))

(defun om-elem-timestamp-is-greater-than-p (unixtime timestamp)
  "Return t if TIMESTAMP elem is greater than UNIXTIME."
  (om-elem--verify timestamp om-elem-is-timestamp-p)
  (> (om-elem-timestamp-get-unixtime timestamp) unixtime))

(defun om-elem-timestamp-is-equal-p (unixtime timestamp)
  "Return t if TIMESTAMP elem is equal to UNIXTIME."
  (om-elem--verify timestamp om-elem-is-timestamp-p)
  (= (om-elem-timestamp-get-unixtime timestamp) unixtime))

(defun om-elem-timestamp-is-end-less-than-p (unixtime timestamp)
  "Return t if TIMESTAMP elem is less than UNIXTIME."
  (om-elem--verify timestamp om-elem-is-timestamp-p)
  (-when-let (end (om-elem-timestamp-get-end-unixtime timestamp))
    (< end unixtime)))

(defun om-elem-timestamp-is-end-greater-than-p (unixtime timestamp)
  "Return t if TIMESTAMP elem is greater than UNIXTIME."
  (om-elem--verify timestamp om-elem-is-timestamp-p)
  (-when-let (end (om-elem-timestamp-get-end-unixtime timestamp))
    (> end unixtime)))

(defun om-elem-timestamp-is-end-equal-p (unixtime timestamp)
  "Return t if TIMESTAMP elem is equal to UNIXTIME."
  (om-elem--verify timestamp om-elem-is-timestamp-p)
  (-when-let (end (om-elem-timestamp-get-end-unixtime timestamp))
    (= end unixtime)))

(defun om-elem-timestamp-is-in-range-p (unixtime timestamp)
  "Return t if UNIXTIME is between start and end of TIMESTAMP elem."
  (om-elem--verify timestamp om-elem-is-timestamp-p)
  (let ((ut1 (om-elem-timestamp-get-unixtime timestamp))
        (ut2 (om-elem-timestamp-get-end-unixtime timestamp)))
    (when ut2 (< ut1 unixtime ut2))))

;;; element setters

;; generic

(defun om-elem-set-property (prop value elem)
  "Set property PROP in element ELEM to VALUE."
  (om-elem--verify elem om-elem-is-element-or-object-p)
  (unless (plist-member (om-elem-properties elem) prop)
    (error "Property %s does not exist in %s" prop elem))
  (om-elem--set-property prop value elem))

(defun om-elem-set-properties (plist elem)
  "Set all properties in ELEM to the values corresponding to PLIST.
PLIST is a list of property-value pairs that correspond to the
property list in ELEM."
  (om-elem--verify elem om-elem-is-element-or-object-p)
  (om-elem--set-properties plist elem))

;; misc

(defun om-elem-set-post-blank (post-blank elem)
  "Set the :post-blank property of ELEM to POST-BLANK."
  (om-elem--verify elem om-elem-is-element-or-object-p)
  (om-elem--set-post-blank post-blank elem))

;; headline

(defun om-elem-headline-set-archived (flag headline)
  "Set the archived flag of HEADLINE element to FLAG."
  (om-elem--verify headline om-elem-is-headline-p)
  (om-elem--headline-set-archived flag headline))

(defun om-elem-headline-set-pre-blank (pre-blank headline)
  "Set the :pre-blank property of HEADLINE to PRE-BLANK."
  (om-elem--verify elem om-elem-is-headline-p)
  (om-elem--headline-set-pre-blank pre-blank headline))

(defun om-elem-headline-set-todo (todo headline)
  "Set the todo keyword of HEADLINE element to TODO."
  (om-elem--verify headline om-elem-is-headline-p)
  (om-elem--headline-set-todo todo headline))

(defun om-elem-headline-set-commented (flag headline)
  "Set the commented flag of HEADLINE element to FLAG."
  (om-elem--verify headline om-elem-is-headline-p)
  (om-elem--headline-set-commented flag headline))

(defun om-elem-headline-update-item-statistics (headline)
  (let ((items (om-elem-find '(section plain-list item) headline))
        (done (length (-filter #'om-elem-item-is-checked-p items)))
        (total (length items)))
    (om-elem--headline-set-statistics-cookie-fraction done total headline)))

(defun om-elem-headline-update-todo-statistics (headline)
  ;; TODO make this private
  (let ((subtodo (->> (om-elem-headline--get-subheadlines headline)
                      (--filter (om-elem-property :todo-keyword it)))
        (done (length (-filter #'om-elem-headline-is-done-p subtodo)))
        (total (length subtodo)))
    (om-elem--headline-set-statistics-cookie-fraction done total headline))))

(defun om-elem--to-relative-priority (p)
  "Convert P from character value to counting integer starting at 0."
  ;; ironically the highest priority is actually the lowest integer
  (when p
    (if (and (<= p org-lowest-priority) (>= p org-highest-priority))
        (- p org-highest-priority)
      (error "Absolute priority out of range: %s" p))))

(defun om-elem--to-absolute-priority (p)
  (when p
    (if (and (>= p 0)
             (<= p (abs (- org-highest-priority org-lowest-priority))))
        (+ org-highest-priority p)
      (error "Relative priority out of range: %s" priority))))

(defun om-elem-headline-set-priority (priority headline)
  "Set the priority of HEADLINE element to PRIORITY."
  (om-elem--verify headline om-elem-is-headline-p)
  (om-elem--headline-set-priority priority headline))

;; TODO make shortcut function for setting the headline title
(defun om-elem-headline-set-title (title headline)
  "Set the title of HEADLINE element to TITLE."
  (om-elem--verify headline om-elem-is-headline-p)
  (om-elem--headline-set-title title headline))

(defun om--plist-get-keys (plist)
  (-slice plist 0 nil 2))

(defun om--plist-get-vals (plist)
  (-slice plist 1 nil 2))

(defun om--plist-non-nil (plist)
  (->> (-partition 2 plist) (-filter #'cadr) (apply #'append)))

;; item

(defun om-elem-item-set-checkbox (state item)
  "Set the checkbox of ITEM element to STATE.
STATE is one of 'on', 'off', 'trans'. Setting to nil removes the
checkbox."
  (om-elem--verify item om-elem-is-item-p)
  (om-elem--item-set-checkbox state item))

(defun om-elem-item-set-bullet (bullet item)
  "Set the bullet of ITEM element to BULLET.
BULLET is either '-' or '+' or an integer greater than zero.
Note that `org-element-item-interpreter' currently does not interpret
'+' bullets properly and will render these as '-'."
  (om-elem--verify item om-elem-is-item-p)
  (om-elem--item-set-bullet bullet item))

(defun om-elem-item-set-tag (tag item)
  "Set the tag of ITEM element to TAG where TAG is a string or nil."
  (om-elem--verify item om-elem-is-item-p)
  (om-elem--item-set-tag tag item))

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
    (om-elem-map* '(item) (om-elem-item-set-bullet type it) plain-list))
   ((eq type 'ordered)
    ;; NOTE the org-interpreter seems to use the correct, ordered
    ;; numbers if any number is set here. This behavior may not be
    ;; reliable.
    (om-elem-map* '(item) (om-elem-item-set-bullet 1 it) plain-list))
   (t (error "Invalid type: %s" type))))

;; keywords

(defun om-elem-keyword-set-key (key keyword)
  "Set the key of KEYWORD element to KEY (a string)."
  (om-elem--verify keyword om-elem-is-keyword-p)
  (om-elem--set-key key keyword))

(defun om-elem-keyword-set-value (value keyword)
  "Set the value of KEYWORD element to VALUE (a string)."
  (om-elem--verify keyword om-elem-is-keyword-p)
  (om-elem--set-value value keyword))

;; node properties

(defun om-elem-node-property-set-key (key node-property)
  "Set the key of NODE-PROPERTY element to KEY (a string)."
  (om-elem--verify node-property om-elem-is-node-property-p)
  (om-elem--set-key key node-property))

(defun om-elem-node-property-set-value (value node-property)
  "Set the value of NODE-PROPERTY element to VALUE (a string)."
  (om-elem--verify node-property om-elem-is-node-property-p)
  (om-elem--set-value value node-property))

;; link

(defun om-elem-link-set-description (desc link)
  (om-elem--verify link om-elem-is-link-p)
  (om-elem--set-contents-by-type 'link desc link))

(defun om-elem-link-set-path (path link)
  "Set the path of LINK element to PATH (a string)."
  (om-elem--verify link om-elem-is-link-p)
  (om-elem--link-set-path path link))

(defun om-elem-link-set-type (type link)
  "Set the type of LINK element to TYPE (a symbol).
Setting TYPE to nil will result in a 'fuzzy' type link."
  (om-elem--verify link om-elem-is-link-p)
  (om-elem--link-set-type type link))

(defun om-elem-link-set-format (format link)
  "Set the type of LINK element to TYPE (a symbol).
Setting TYPE to nil will result in a 'fuzzy' type link."
  (om-elem--verify link om-elem-is-link-p)
  (om-elem--link-set-format format link))

;; statistics cookie

(defun om-elem-statistics-cookie-set-value (value statistics-cookie)
  "Set the value or STATISTICS-COOKIE object with VALUE.
VALUE can take four forms which determine the format of the value:
- integer X from 0 to 100 -> 'X%'
- nil -> '%'
- (integer X integer Y) -> 'X/Y'
- (nil) -> '/'"
  (om-elem--verify statistics-cookie om-elem-is-statistics-cookie-p)
  (om-elem--statistics-cookie-set-value value statistics-cookie))

;; timestamp

(defun om-elem-timestamp-set-time (time timestamp)
  "Set start time of TIMESTAMP element to TIME.
TIME is a list like '(year month day)' or '(year month day hour min)'."
  (om-elem--verify timestamp om-elem-is-timestamp-p)
  (om-elem--timestamp-set-time time timestamp))

(defun om-elem-timestamp-set-time-end (time timestamp)
  "Set end time of TIMESTAMP element to TIME.
TIME is a list like '(year month day)' or '(year month day hour min)'.
This will also change the type to (un)ranged as appropriate."
  (om-elem--verify timestamp om-elem-is-timestamp-p)
  ;; TODO this could be refactored
  (let ((cur-type (--> (om-elem-property :type timestamp)
                       (cl-case it
                         (inactive-range 'inactive)
                         (active-range 'active)
                         (t it)))))
    (->> (om-elem--timestamp-set-time-end time timestamp)
         ;; this will flip the range of the type if needed
         (om-elem--timestamp-set-type cur-type))))

(defun om-elem-timestamp-set-type (type timestamp)
  "Set type of TIMESTAMP element to TYPE.
TYPE can be either 'active' or 'inactive'."
  (om-elem--verify timestamp om-elem-is-timestamp-p)
  (om-elem--timestamp-set-type type timestamp))

;; TODO add a switch for precision
(defun om-elem-timestamp-set-time-unixtime (unixtime timestamp)
  "Set start time of TIMESTAMP element to UNIXTIME (an integer).
This assumes one wants HH:MM precision."
  (om-elem--verify timestamp om-elem-is-timestamp-p)
  (-> (decode-time unixtime)
      (-slice 1 6)
      (om-elem-timestamp-set-time timestamp)))

(defun om-elem-timestamp-set-time-end-unixtime (unixtime timestamp)
  "Set end time of TIMESTAMP element to UNIXTIME (an integer).
This assumes one wants HH:MM precision."
  (om-elem--verify timestamp om-elem-is-timestamp-p)
  (-> (decode-time unixtime)
      (-slice 1 6)
      (om-elem-timestamp-set-time-end timestamp)))

(defun om-elem--pad-list (list length fill)
  (let ((diff (- (length list) length)))
    (cond
     ((> 0 diff)
      (append list (-repeat (abs diff) fill)))
     ((< 0 diff)
      (-take length list))
     (t list))))

;; element mappers

;; generic

(defun om-elem-map-property (prop fun elem)
  (om-elem--verify elem om-elem-is-element-or-object-p)
  (om-elem--map-property prop fun elem))

(defun om-elem-map-properties (plist elem)
  (om-elem--verify elem om-elem-is-element-or-object-p)
  (om-elem--map-properties plist elem))

;; headline

(defun om-elem-headline-map-node-property (key fun headline)
  (om-elem--verify key stringp headline om-elem-is-headline-p)
  (om-elem-map-first*
   `(section property-drawer (:and node-property (:key ,key)))
    (om-elem--node-property-map-value fun it) headline))

;; node property

(defun om-elem-node-property-map-value (fun node-property)
  (om-elem--verify node-property om-elem-is-node-property-p)
  (om-elem--node-property-map-value fun node-property))

;; anaphoric forms

(-each '(om-elem-map-property
         om-elem-headline-map-node-property
         om-elem-node-property-map-value)
  #'om-elem--gen-anaphoric-form)

(defmacro om-elem-map-properties* (plist elem)
  `(om-elem--map-properties ,plist ,elem))

;;; element shifters
;; shift a numeric property

;; generic

(defun om-elem-shift-property (prop n elem)
  "Shift PROP of ELEM by N where N is a positive or negative integer."
  (om-elem--verify elem om-elem-is-element-or-object-p)
  (unless (plist-member (om-elem-properties elem) prop)
    (error "Property %s does not exist in %s" prop elem))
  (om-elem--shift-property prop n elem))

;; headline

(defun om-elem-headline-shift-priority (shift headline)
  "Shift the priority property of HEADLINE element by SHIFT.
SHIFT is a positive or negative integer."
  ;; positive goes up (B -> A) and vice versa
  (om-elem--verify headline om-elem-is-headline-p)
  (cl-flet
      ((fun
        (priority)
        (if (not shift) priority
          (let ((diff (+ 1 (abs (- org-lowest-priority
                                   org-highest-priority))))
                (relp (om-elem--to-relative-priority priority)))
            (--> (- relp shift)
                 (mod it diff)
                 (- it relp)
                 (+ priority it))))))
    (if (not (om-elem-property :priority headline)) headline
      (om-elem-map-property :priority #'fun headline))))

;; timestamp

(defun om-elem-timestamp-shift-time-start (n unit timestamp)
  "Shift the UNIT of TIMESTAMP element start time by VALUE.
VALUE is a positive or negative integer and UNIT is one of 'minute',
'hour', 'day', 'month', or 'year'. Value will wrap around larger units
as needed; for instance, supplying 'minute' for UNIT and 60 for VALUE
will increase the hour property by 1."
  (om-elem--verify timestamp om-elem-is-timestamp-p)
  (let ((type (om-elem-property :type timestamp)))
    (if (memq type '(active inactive))
        (om-elem-timestamp-shift-time n unit timestamp)
      (om-elem--timestamp-shift-time-start n unit timestamp))))

(defun om-elem-timestamp-shift-time-end (n unit timestamp)
  "Shift the UNIT of TIMESTAMP element end time by VALUE.
The behavior is analogous to `om-elem-timestamp-shift-time-start',
except that the timestamp will be unchanged if no ending time is
present."
  (om-elem--verify timestamp om-elem-is-timestamp-p)
  (let ((type (om-elem-property :type timestamp)))
    (if (memq type '(active inactive)) timestamp
      (om-elem--timestamp-shift-time-end n unit timestamp))))

(defun om-elem-timestamp-shift-time (n unit timestamp)
  "Shift the UNIT of TIMESTAMP element start and end time by VALUE.
The behavior is analogous to `om-elem-timestamp-shift-time-start' for
both timestamp halves."
  (om-elem--verify timestamp om-elem-is-timestamp-p)
  (->> (om-elem--timestamp-shift-time-start n unit timestamp)
       (om-elem--timestamp-shift-time-end n unit)))

;;; element toggles
;; flip a boolean (ish) property

;; generic

(defun om-elem-toggle-property (prop elem)
  "Toggle the state of PROP in ELEM."
  (om-elem-map-property prop #'not elem))

;; headline

(defun om-elem-headline-toggle-commented (headline)
  "Toggle the commented/uncommented state of HEADLINE element."
  (om-elem--verify headline om-elem-is-headline-p)
  (om-elem-toggle-property :commentedp headline))

;; (defun om-elem-headline-toggle-quoted (elem)
;;   (om-elem-toggle-property :quotedp elem))

;; TODO need a tag setter for this
;; (defun om-elem-headline-toggle-archived (headline)
;;   (let ((state (om-elem-property :archivedp headline)))
;;     (cl-flet
;;         ((toggle-tags
;;           (elem)
;;           (print  (om-elem-property :tags elem))
;;           (--> (om-elem-property :tags elem)
;;                (if (member org-archive-tag it)
;;                    (-remove-item org-archive-tag it)
;;                  (cons org-archive-tag it)))))
;;     (->> elem
;;          (om-elem-map-property :archivedp #'not)
;;          (om-elem-map-property :tags #'toggle-tags))))

;; item

(defun om-elem-item-toggle-checkbox (item)
  "Toggle the checked/unchecked state of ITEM element."
  (om-elem--verify item om-elem-is-item-p)
  (-if-let (cur-state (om-elem-property :checkbox item))
      (cl-case cur-state
        ('trans item)
        ('on (om-elem-set-property :checkbox 'off item))
        ('off (om-elem-set-property :checkbox 'on item))
        (t (error "This should not happen.")))
    item))

;; statistics cookie

;; TODO add stats cookie type toggle

;; timestamp

(defun om-elem-timestamp-toggle-active (timestamp)
  "Toggle the active/inactive type of TIMESTAMP element."
  (om-elem--verify timestamp om-elem-is-timestamp-p)
  (let ((cur-type (om-elem-property :type timestamp)))
    (cond
     ((memq cur-type '(inactive inactive-range))
      (om-elem-timestamp-set-type 'active timestamp))
     ((memq cur-type '(active active-range))
      (om-elem-timestamp-set-type 'inactive timestamp))
     ;; TODO puke out when given a diary timestamp
     (t timestamp))))

;;; content modification

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
    (let* ((contents (om-elem-contents obj))
           (post-blank (om-elem-property :post-blank obj))
           (first (-drop-last 1 contents))
           (last* (->> (-last-item contents)
                       (om-elem-set-post-blank post-blank)
                       (list))))
      (append first last*))))

(defun om-elem-unwrap-deep (types obj)
  "Remove the contents of all objects of type in TYPES from OBJ.
Return a list of objects."
  (cond
   ((om-elem-is-any-type-p types obj) 
    (let* ((contents (om-elem-contents obj))
           (post-blank (om-elem-property :post-blank obj))
           (first (-drop-last 1 contents))
           (last* (->> (-last-item contents)
                       (om-elem-set-post-blank post-blank)
                       (list))))
      (--mapcat (om-elem-unwrap-deep types it) (append first last*))))
   ((om-elem-plain-text-p obj) (list obj))
   (t (list obj))))

(defun om-elem-remove-formatting (types elem)
  "Remove all recursive formatting TYPES from ELEM."
  (om-elem-map* `(:many! (:or ,@types))
                (om-elem-unwrap-deep types it) elem))

(defun om-elem-remove-all-formatting (elem)
  "Remove all recursive formatting from ELEM."
  (om-elem-remove-formatting org-element-all-objects elem))

;; headline

(defun om-elem-set-planning (planning-plist headline)
  (om-elem--verify headline om-elem-is-headline-p)
  (let ((keys (om--plist-get-keys planning-plist)))
    (--> (om--plist-get-vals planning-plist)
         (--map (-some->> it (om-elem-build-timestamp 'inactive)) it)
         (-interleave keys it)
         (om-elem-set-properties it headline))))

;; (defun om-elem-headline-set-planning (planning-plist headline)
;;   (-if-let (plan-elem (-some->> (om--plist-non-nil planning-plist)
;;                                 (apply #'om-elem-build-planning)))
;;       (cond
;;        ((om-elem-find-first '(section planning) headline)
;;         (om-elem-map-first*
;;          '(section planning)
;;          (om-elem-set-planning planning-plist it)
;;          headline))
;;        ((om-elem-find-first '(section) headline)
;;         (om-elem-insert-within '(0 section) plan-elem headline))
;;        (t 
;;         (--> (om-elem-build-section plan-elem)
;;              (om-elem-insert-within-element it 0 headline))))
;;     (om-elem-delete-first '(section planning) headline)))

;; (defun om-elem-headline-set-node-property (key val headline)
;;   (let* ((section-query '(section))
;;          (drawer-query `(,@section-query property-drawer))
;;          (np-query `(,@drawer-query (:and node-property (:key ,key)))))
;;     ;; delete the property if val is nil
;;     (if (not val)
;;         (-> (apply #'om-elem-delete-first headline np-query)
;;             (om-elem-clean))
;;       (let ((np-elem (om-elem-build-node-property key val)))
;;         (cond
;;          ;; set node property if present
;;          ((apply #'om-elem-find-first headline np-query)
;;           (apply #'om-elem-map-first headline
;;                  (-partial #'om-elem-node-property-set-value val)
;;                  np-query))
;;          ;; add node property if drawer present
;;          ((apply #'om-elem-find-first headline drawer-query)
;;           (apply #'om-elem-insert-within headline np-elem 0 drawer-query))
;;          ;; make new drawer and node property if no drawer
;;          ((apply #'om-elem-find-first headline section-query)
;;           ;; TODO this will not always work, sometimes there are lists
;;           ;; and clocks before the property drawer for the logbook
;;           (let ((index
;;                  (cond
;;                   ((om-elem-find-first headline 'section 'planning) 1)
;;                   (t 0))))
;;             (--> (om-elem-build-property-drawer key val)
;;                  (apply #'om-elem-insert-within headline it index
;;                         section-query))))
;;           ;; make new section with drawer/property if not present
;;           (t (--> (om-elem-build-property-drawer key val)
;;                   (om-elem-build-section it)
;;                   (om-elem-insert-within-element headline it 0))))))))

;; ;; TODO make headline indent subtree (NOT this)

;; TODO make drawer, prop-drawer, planning, and section insertion


;; headline

(defun om-elem-headline-indent-subtree (index headline)
  (om-elem--verify headline om-elem-is-headline-p)
  (om-elem--headline-indent-subtree index headline))

(defun om-elem-headline-indent-subheadline (index headline)
  (om-elem--verify headline om-elem-is-headline-p)
  (om-elem--headline-indent-subheadline index headline))

(defun om-elem-headline-unindent-subheadline (index child-index headline)
  (om-elem--verify headline om-elem-is-headline-p)
  (om-elem--headline-unindent-subheadline index child-index headline))

(defun om-elem-headline-unindent-subtree (index headline)
  (om-elem--verify headline om-elem-is-headline-p)
  (om-elem--headline-unindent-subtree index headline))

;; plain-list

(defun om-elem-plain-list-indent-item-tree (index plain-list)
  (om-elem--verify plain-list om-elem-is-plain-list-p)
  (om-elem--plain-list-indent-item-tree index plain-list))

(defun om-elem-plain-list-indent-item (index plain-list)
  (om-elem--verify plain-list om-elem-is-plain-list-p)
  (om-elem--plain-list-indent-item index plain-list))

(defun om-elem-plain-list-unindent-item (index child-index plain-list)
  (om-elem--verify plain-list om-elem-is-plain-list-p)
  (om-elem--plain-list-unindent-item index child-index plain-list))

(defun om-elem-plain-list-unindent-items (index plain-list)
  (om-elem--verify plain-list om-elem-is-plain-list-p)
  (om-elem--plain-list-unindent-items index plain-list))

;; table

(defun om-elem-table-delete-row (index table)
  (om-elem--verify table om-elem-is-table-p)
  (om-elem--table-delete-row index table))

(defun om-elem-table-delete-column (index table)
  (om-elem--verify table om-elem-is-table-p)
  (om-elem--table-delete-column index table))

(defun om-elem-table-insert-column (index column table)
  (om-elem--verify table om-elem-is-table-p)
  (unless (-all? #'om-elem-is-table-cell-p column)
    (error "All members of column must be table cells"))
  (om-elem--table-insert-column index column table))

(defun om-elem-table-insert-column! (index column table)
  (om-elem--verify table om-elem-is-table-p)
  (let ((column (-map #'om-elem-build-table-cell column)))
    (om-elem--table-insert-column index column table)))

(defun om-elem-table-insert-row (index row table)
  (om-elem--verify row om-elem-is-table-row-p
                   table om-elem-is-table-p)
  (om-elem--table-insert-row index row table))

(defun om-elem-table-insert-row! (index row table)
  (om-elem--verify table om-elem-is-table-p)
  (let ((row (if (eq row 'hline) (om-elem-build-table-row-hline)
               (->> (-map #'om-elem-build-table-cell row)
                    (apply #'om-elem-build-table-row)))))
    (om-elem--table-insert-row index row table)))

;;; parsing functions

;; point functions

(defun om-elem-parse-object-at (point &optional type)
  "Return the object tree under POINT or nil if not on an object.

If TYPE is supplied, only return nil if the object under point is
not of that type. TYPE is a symbol from `org-element-all-objects'."
  (save-excursion
    (goto-char point)
    (let* ((context (org-element-context))
           (type (om-elem-type context))
           (offset (cond
                    ((memq type '(superscript subscript)) -1)
                    ((eq type 'table-cell) -1)
                    (t 0)))
           (nesting (cond
                     ((memq type '(superscript subscript)) '(0 1))
                     ((eq type 'table-cell) '(0 0 0))
                     (t '(0 0)))))
      (-let* (((&plist :begin :end) (om-elem-properties context))
              (tree (org-element--parse-elements (+ begin offset) end 'first-section
                                                 nil nil nil nil)))
        (--> (car tree)
             (om-elem-get-nested-content nesting it)
             (om-elem-allow-types org-element-all-objects it)
             (if type (om-elem-allow-type type it) it))))))

(defun om-elem-parse-element-at (point &optional type)
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
         (elem-type (om-elem-type elem)))
      (if (not
           (memq elem-type (append org-element-greater-elements
                                   org-element-object-containers)))
          elem
        (-let* (((&plist :begin :end) (om-elem-properties elem))
                (tree (car (org-element--parse-elements
                            begin end 'first-section nil nil nil nil)))
                (nesting (cl-case elem-type
                           (headline nil)
                           (table (if (eq type 'table-row) '(0 0) '(0)))
                           (plain-list (if (eq type 'item) '(0 0) '(0)))
                           (t '(0)))))
          (--> (om-elem-get-nested-content nesting tree)
               (if type (om-elem-allow-type type it) it)))))))

(defun om-elem-parse-table-row-at (point)
  "Return table-row element under POINT or nil if not found."
  (save-excursion
    (goto-char point)
    (beginning-of-line)
    (om-elem-parse-element-at (point) 'table-row)))

(defun om-elem-parse-item-at (point)
  "Return item element under POINT or nil if not found.
Unlike `om-elem-parse-element-at', this will return then item even if
POINT is not at the beginning of the line."
  (save-excursion
    (goto-char point)
    (beginning-of-line)
    (om-elem-parse-element-at (point) 'item)))

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
  "Return element tree of the headline under POINT or nil if none.
POINT does not need to be on the headline itself. Only the headline
and its section will be returned (no subheadlines)."
  (om-elem--parse-headline-subtree-at point nil))

(defun om-elem-parse-subtree-at (point)
  "Return element tree of the headline under POINT or nil if none.
POINT does not need to be on the headline itself. Unlike
`om-elem-parse-headline-at', the returned tree will include
subheadlines."
  (om-elem--parse-headline-subtree-at point t))

(defun om-elem-parse-section-at (point)
  "Return tree of the current section under POINT.
If POINT is on or within a headline, return the section under that
headline. If POINT is before the first headline (if any), return
the section at the top of the org buffer."
  (save-excursion
    (goto-char point)
    (om-elem-get-nested-content
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

(defun om-elem-parse-this-object (&optional type)
  "Call `om-elem-parse-object-at' with the current point.

If TYPE is supplied, only return nil if the object under point is
not of that type. TYPE is a symbol from `org-element-all-objects'."
  (om-elem-parse-object-at (point) type))

(defun om-elem-parse-this-element (&optional type)
  "Call `om-elem-parse-element-at' with the current point.

If TYPE is supplied, only return nil if the element under point is
not of that type. TYPE is a symbol from `org-element-all-elements'."
  (om-elem-parse-element-at (point) type))

(defun om-elem-parse-this-table-row ()
  "Call `om-elem-parse-table-row-at' with the current point."
  (om-elem-parse-table-row-at (point)))

(defun om-elem-parse-this-item ()
  "Call `om-elem-parse-item-at' with the current point."
  (om-elem-parse-item-at (point)))

(defun om-elem-parse-this-headline ()
  "Call `om-elem-parse-headline-at' with the current point."
  (om-elem-parse-headline-at (point)))

(defun om-elem-parse-this-subtree ()
  "Call `om-elem-parse-subtree-at' with the current point."
  (om-elem-parse-subtree-at (point)))

(defun om-elem-parse-this-section ()
  "Call `om-elem-parse-section-at' with the current point."
  (om-elem-parse-section-at (point)))

(defalias 'om-elem-parse-this-buffer 'org-element-parse-buffer)

;;; side effects

;; write

(defun om-elem-insert (point elem)
  "Convert ELEM to a string and insert at POINT in the current buffer.
Return ELEM."
  (om-elem--verify point integerp
                   elem om-elem-is-element-or-object-p)
  (save-excursion
    (goto-char point)
    (insert (om-elem-to-string elem)))
  elem)

(defun om-elem-insert-tail (point elem)
  "Like `om-elem-insert' but insert ELEM at POINT and move to the end of inserted string."
  (om-elem--verify point integerp
                   elem om-elem-is-element-or-object-p)
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
                   elem om-elem-is-element-or-object-p)
  ;; if elem is of type 'org-data' it will have no props
  (let* ((begin (or (om-elem-property :begin elem) (point-min)))
         (end (or (om-elem-property :end elem) (point-max)))
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

(defun om-elem-update-object-at (point fun &optional type)
  (om-elem-update fun (om-elem-parse-object-at point type)))

(defun om-elem-update-element-at (point fun &optional type)
  (om-elem-update fun (om-elem-parse-element-at point type)))

(defun om-elem-update-item-at (point fun)
  (om-elem-update fun (om-elem-parse-headline-at point)))

(defun om-elem-update-table-row-at (point fun)
  (om-elem-update fun (om-elem-parse-headline-at point)))

(defun om-elem-update-headline-at (point fun)
  (om-elem-update fun (om-elem-parse-headline-at point)))

(defun om-elem-update-subtree-at (point fun)
  (om-elem-update fun (om-elem-parse-subtree-at point)))

(defun om-elem-update-section-at (point fun)
  (om-elem-update fun (om-elem-parse-subtree-at point)))

;; TODO just make a function to create these from the point versions

(defun om-elem-update-this-object (fun &optional type)
  (om-elem-update-object-at (point) fun type))

(defun om-elem-update-this-element (fun &optional type)
  (om-elem-update-element-at (point) fun type))

(defun om-elem-update-this-item (fun)
  (om-elem-update-item-at (point) fun))

(defun om-elem-update-this-table-row (fun)
  (om-elem-update-table-row-at (point) fun))

(defun om-elem-update-this-headline (fun)
  (om-elem-update-headline-at (point) fun))

(defun om-elem-update-this-subtree (fun)
  (om-elem-update-subtree-at (point) fun))

(defun om-elem-update-this-section (fun)
  (om-elem-update-section-at (point) fun))

;; fold
(defun om-elem--flag-elem-contents (flag elem)
  (om-elem--verify flag booleanp
                   elem om-elem-is-element-or-object-p)
  (-let (((&plist :contents-begin :contents-end) (om-elem-properties elem)))
    (outline-flag-region (- contents-begin 1) (- contents-end 1) flag)))

(defun om-elem-fold-contents (elem)
  "Fold the contents of ELEM if they exist."
  (om-elem--flag-elem-contents t elem))

(defun om-elem-unfold-contents (elem)
  "Unfold the contents of ELEM if they exist."
  (om-elem--flag-elem-contents nil elem))

;;; misc functions

(defun om-elem-allow-type (type elem)
  "Return ELEM if it is TYPE or nil otherwise."
  (and (om-elem-is-type-p type elem) elem))

(defun om-elem-allow-types (types elem)
  "Return ELEM if it is one of TYPES or nil otherwise."
  (and (om-elem-is-any-type-p types elem) elem))

(defun om-elem-length (elem)
  "Return the character length of ELEM."
  (if (not elem) 0
    (let ((b (om-elem-property :begin elem))
          (e (om-elem-property :end elem)))
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

(provide 'om-elem)
;;; om-elem.el ends here
