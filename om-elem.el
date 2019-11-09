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
    ;; TODO need to remove the predicate functions from the args list
    ;; (setq header
    ;;       ;; Macro expansion can take place in the middle of
    ;;       ;; apparently harmless computation, so it should not
    ;;       ;; touch the match-data.
    ;;       (save-match-data
    ;;         (cons (help-add-fundoc-usage
    ;;                (if (stringp (car header)) (pop header))
    ;;                ;; Be careful with make-symbol and (back)quote,
    ;;                ;; see bug#12884.
    ;;                (help--docstring-quote
    ;;                 (let ((print-gensym nil) (print-quoted t)
    ;;                       (print-escape-newlines t))
    ;;                   (format "%S" (cons 'fn (cl--make-usage-args
    ;;                                           args))))))
    ;;               header)))
    `(defun ,name ,arg-form
       ,header
       ,(macroexp-let*
         let-forms
         (macroexp-progn `(,body))))))

;; TODO this docstring is wrong
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
(-> (append org-element-all-elements org-element-all-elements)
    (-distinct)
    (--each
        (let ((fun-name (intern (format "om-elem-%s-p" it)))
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
  (om-elem-set-recursive-content '("") elem))

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

;; TODO there is probably a more efficient way to do this...
(defun om-elem--clean (elem)
  "Remove elements from ELEM that are empty or recursively empty."
  (cond
   ((and (or (om-elem-is-any-type-p om-elem--rm-if-empty elem)
             (and (om-elem-is-type-p 'table-row elem)
                  (om-elem-property-is-eq-p :type 'standard elem)))
         ;; TODO need to test if elem is empty or if it is filled
         ;; with empty forbidden elements
         (om-elem-is-zero-length-p elem))
    nil)
   (t (-some->>
       elem
       ;; delete empty things
       (om-elem-delete `(:many! (:and (:pred om-elem-is-zero-length-p)
                                      (:or ,@om-elem--rm-if-empty
                                           (:and table-row (:type standard))))))))))

(defun om-elem--blank (elem)
  "Blank the contents of ELEM if empty."
  (cond
   ((and (om-elem-is-any-type-p om-elem--blank-if-empty elem)
         (om-elem-is-empty-p elem))
    (om-elem--set-blank-contents elem))
   (t (-some->>
       elem
       ;; insert blank in empty greater elements
       (om-elem-map* `(:many! (:and (:pred om-elem-is-empty-p)
                                    (:or ,@om-elem--blank-if-empty)))
                     (om-elem--set-blank-contents elem))))))

;; TODO should we return nil or "" here if ELEM evals to nil?
(defun om-elem-to-string (elem)
  "Return ELEM as an interpreted string without text properties."
  (-some->> elem
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
  ;; (let*
  ;;     ((rm-if-empty '(table plain-list bold italic radio-target
  ;;                           strike-through superscript subscript
  ;;                           table-cell underline))
  ;;      (blank-if-empty '(center-block drawer dynamic-block
  ;;                                     property-drawer quote-block
  ;;                                     special-block))
  ;;      (elem*
  ;;       (cond
  ;;        ((and (om-elem-is-any-type-p rm-if-empty elem)
  ;;              ;; TODO need to test if elem is empty or if it is filled
  ;;              ;; with empty forbidden elements
  ;;              (om-elem-is-zero-length-p elem))
  ;;         nil)
  ;;        ((and (om-elem-is-any-type-p blank-if-empty elem)
  ;;              (om-elem-is-empty-p elem))
  ;;         (om-elem-set-recursive-content '("") elem))
  ;;        (t (-some->>
  ;;            elem
  ;;            ;; delete empty things
  ;;            (om-elem-delete `(:many! (:and (:pred om-elem-is-zero-length-p)
  ;;                                           (:or ,@rm-if-empty))))
  ;;            ;; insert blank in empty greater elements
  ;;            (om-elem-map* `(:many! (:and (:pred om-elem-is-empty-p)
  ;;                                         (:or ,@blank-if-empty)))
  ;;                          (om-elem-set-recursive-content '("") it)))))))
  ;;   (and elem* (substring-no-properties
  ;;               (org-element-interpret-data elem*)))))
  ;; (and elem (substring-no-properties (org-element-interpret-data elem))))

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

;;; boundary normalization
;; functions to set :begin, :end, :contents-begin, and :contents-end

(defun om-elem--property-length (elem prop pad)
  "Return the length of PROP in ELEM and add PAD.
If PROP is nil, return 0."
  (-if-let (v (om-elem-property prop elem))
      (+ (length v) pad) 0))

(defun om-elem--properties-length (elem &rest pp)
  "Return the length of alternating property-pad pairs PP for ELEM.
This calls `om-elem--property-length' multiple times."
  (pcase pp
    (`(,prop . (,pad . nil))
     (om-elem--property-length elem prop pad))
    (`(,prop . (,pad . ,pp))
     (+ (om-elem--property-length elem prop pad)
        (apply #'om-elem--properties-length elem pp)))
    (_ (error "Invalid args: %s" pp))))

(defconst om-elem--objects-singular
  (-difference org-element-all-objects org-element-recursive-objects))

(defun om-elem--length (elem)
  "Return integer if the length of ELEM as an interpreted string.
This will only process elements and objects that are not containers
for other elements or objects."
  (let ((type (om-elem-type elem)))
    (if (eq type 'plain-text) (length elem)
      (+
       ;; add post-blank if `ELEM' is an object (this is number of
       ;; spaces for objects; post-blank for elements adds newlines
       ;; which is covered by other functions)
       (if (not (memq type om-elem--objects-singular)) 0
         (or (om-elem-property :post-blank elem) 0))
       ;; figure out how much else to add based on type
       (cl-case type
         ;; TODO add inline task
         ;; some types just have a number to add
         (line-break 3)                                          ; "\\\n"
         (horizontal-rule 5)                                     ; "-----"
         ;; some have a property with additional characters
         (babel-call
          (om-elem--properties-length elem
                                      :call 10                   ; "#+CALL: ()"
                                      :inside-header 2           ; "[]"
                                      :arguments 0
                                      :end-header 1))            ; " "
         ((comment code verbatim)
          (om-elem--property-length elem :value 2))              ; 2 for flanking characters
         ((diary-sexp latex-environment latex-fragment macro
                      statistics-cookie)
          (om-elem--property-length elem :value 0))
         (export-block
          (om-elem--properties-length elem
                                      :type 28                   ; "#+BEGIN_EXPORT \n" "#+END_EXPORT"
                                      :value 0))
         (export-snippet
          (om-elem--properties-length elem
                                      :back-end 3                ; "@@" and ":"
                                      :value 2))                 ; "@@"
         (inline-babel-call
          (om-elem--properties-length elem
                                      :call 7                    ; "call_()"
                                      :inside-header 2           ; "[]"
                                      :arguments 0
                                      :end-header 2))            ; "[]"
         (inline-src-block
          (om-elem--properties-length elem
                                      :language 6                ; "src_" and "{}"
                                      :parameters 2              ; "[]"
                                      :value 0))
         (keyword
          (om-elem--properties-length elem
                                      :key 4                     ; "#+" and ": "
                                      :value 0))
         (target
          (om-elem--property-length elem :value 4))              ; "<<" and ">>"
         ;; some are easier to just use the interpreter directly
         ;; NOTE this will not add any newlines (which is desired here)
         (node-property
          (length (org-element-node-property-interpreter elem nil)))
         (src-block
          (length (org-element-src-block-interpreter elem nil)))
         ;; the rest follow no pattern
         (clock
          (let ((d (om-elem-property :duration elem)))
            (+ (length org-clock-string) 1                       ; "CLOCK: "
               (om-elem--length (om-elem-property :value elem))
               (cond
                ((null d) 0)                                     ; no duration
                ((< 5 (length d)) (+ (length d) 4))              ; " => "
                (t 9)))))                                        ; " => %2s:%02s"
         (comment-block
          (->> (om-elem-property :value elem)
               (org-remove-indentation)
               (org-element-normalize-string)
               (length)
               (+ 29)))                                          ; "#+BEGIN_COMMENT\n" and "#+END_COMMENT"
         (entity
          (+ (om-elem--property-length elem :name 1)             ; "\"
             (if (om-elem-property :use-brackets-p elem) 2 0)))  ; {}
         (timestamp
          ;; TODO add warning/repeater type lengths
          (if (om-elem-property-is-eq-p :type 'diary elem)
              (om-elem--property-length elem :raw-value 0)
            (-let (((&plist :type type
                            :minute-start Ms
                            :hour-start Hs
                            :minute-end Me
                            :hour-end He
                            :day-end de
                            :month-end me
                            :year-end ye)
                    (om-elem-properties elem)))
              (+ 16                                              ; "[XXXX-XX-XX xxx]"
                 (if (and de me ye) 18 0)                        ; "--[XXXX-XX-XX xxx]"
                 (if (and Hs Ms) 6 0)                            ; " HH:MM"
                 (if (and He Me) 6 0)))))                        ; " HH:MM"
         (example-block
          (-let (((&plist :value v :preserve-indent p)
                  (om-elem-properties elem)))
            (+ 29                                                ; "#+BEGIN_EXAMPLE\n" and "#+END_EXAMPLE"
               (om-elem--property-length elem :switches 1)       ; " "
               (if (or org-src-preserve-indentation p)
                   (length v)
                 (length (org-remove-indentation v))))))
         (fixed-width
          (->> (om-elem-property :value elem)
               (s-chop-suffix "\n")                              ; remove newlines
               (length)
               (+ 2)))                                           ; ": "
         (planning
          (-let (((&plist :deadline d :scheduled s :closed c)
                  (om-elem-properties elem)))
            (+ (if d (+ (om-elem--length d) 10) 0)               ; "DEADLINE: "
               (if s (+ (om-elem--length s) 11) 0)               ; "SCHEDULED: "
               (if c (+ (om-elem--length c) 8) 0)                ; "CLOSED: "
               (1- (length (-non-nil (list d s c)))))))
         (t (error "Unknown type (length): %s" type)))))))

(defun om-elem--length-secondary-string (pad ss)
  (if (not ss) 0 (--reduce-from (+ acc (om-elem--length it)) pad ss)))

(defun om-elem--headline-length-head (headline)
  "Return the length of HEADLINE without contents."
  (let* ((headline-len
          (+ (om-elem-property :level headline) 1 ; stars + space
             (om-elem--property-length headline :todo-keyword 1) ; 1 for " "
             (if (om-elem-property :priority headline) 5 0) ; 4 for " [#" and "] "
             (if (not (om-elem-property :commentedp headline)) 0
               (+ (length org-comment-string) 1)) ; 1 for " "
             (->> (om-elem-property :title headline)
                  (om-elem--length-secondary-string 0))))
         (tags-len (-if-let (ts (om-elem-property :tags headline))
                       ;; add 1 to each and 1 at end for colons
                       (--reduce-from (+ 1 acc (length it)) 1 ts) 0))
         (pad-len
          (cond
           ((zerop tags-len) 0)
           ((zerop org-tags-column) 1)
           ((< org-tags-column 0)
            (max (- (+ org-tags-column headline-len tags-len)) 1))
           (t (max (- org-tags-column headline-len) 1))))
         ;; newlines the separate the first line from contents
         (newline-len
          (if (om-elem-is-empty-p headline) 0
            (1+ (or (om-elem-property :pre-blank headline) 0)))))
    (+ headline-len tags-len pad-len newline-len)))

(defun om-elem--item-length-head (item)
  "Return the length of ITEM without contents."
  (+ (om-elem--property-length item :bullet 0)
     (if (memq (om-elem-property :checkbox item) '(on off trans)) 4 0) ; "[Y] "
     (-if-let (c (om-elem-property :counter item))
         (+ 4 (1+ (floor (log c 10))))                                 ; 4 for "[@" and "] "
       0)
     (->> (om-elem-property :tag item)
          (om-elem--length-secondary-string 4))                       ; 4 for " :: "
     ;; add one for newline if item is not empty and its first item
     ;; is not a paragraph
     (-if-let (c (om-elem-contents item))
         (if (om-elem-is-type-p 'paragraph (-first-item c)) 0 1)
       0)))

(defun om-elem--link-length-head (link)
  (let ((type (om-elem-property :type link))
        (path (om-elem-property :path link)))
    (if (string= type "radio") (length path)
      (+ (length path)
         ;; add length for brackets-type things
         (if (om-elem-contents link) 4                             ; "[[" and "]["
           (pcase (om-elem-property :format link)
             ((or `bracket
                  `nil
                  (guard
                   (member
                    type
                    '("coderef" "custom-id" "fuzzy")))) 2)         ; "[["
             (`angle 1)                                            ; "<"
             (`plain 0)
             (f (error "Wrong format argument: %s") f)))
         ;; add length for the type and anything else with it
         (pcase type
           ("coderef" 2)                                           ; "(" and ")"
           ("custom-id" 1)                                         ; "#"
           ("file" (om-elem--properties-length link
                                               :application 1      ; "+"
                                               :type 1             ; ":"
                                               :search-option 2))  ; "::"
           ("fuzzy" 0)
           (_ (+ 1 (length type))))))))                            ; ":"

(defun om-elem--length-head (elem)
  (cl-case (om-elem-type elem)
    ;; TODO add inlinetask
    ((paragraph plain-list section table) 0)
    (macro 3)                                                  ; "{{{" or
    (property-drawer 13)                                       ; ":PROPERTIES:\n"
    ((quote-block verse-block) 14)                             ; "#+BEGIN_QUOTE\n" or
                                                               ; "#+BEGIN_VERSE\n"
    (center-block 15)                                          ; "#+BEGIN_CENTER\n"
    (table-row
     (and (om-elem-property-is-eq-p :type 'standard elem) 1))  ; "|"
    (drawer
     (om-elem--property-length elem :drawer-name 3))           ; ":" and ":\n"
    (dynamic-block
     (om-elem--properties-length elem
                                 :block-name 10                ; #+BEGIN: " and "\n"
                                 :arguments 1))                ; " "
    (footnote-definition
     (om-elem--property-length elem :label 6))                 ; "[fn:" and "] "
    (special-block
     (om-elem--property-length elem :type 9))                  ; "#+BEGIN_" "\n"
    (headline (om-elem--headline-length-head elem))
    (item (om-elem--item-length-head elem))
    (link (om-elem--link-length-head elem))
    ;; if empty, these objects should return 0 as they will not be parsed
    ((bold italic strike-through table-cell underline)
     (if (om-elem-is-empty-p elem) 0 1))                       ; single leading char
    (radio-target (if (om-elem-is-empty-p elem) 0 3))          ; <<<
    ((subscript superscript)
     (cond ((om-elem-is-empty-p elem) 0)
           ((om-elem-property :use-brackets-p elem) 2)         ; single leading char and "{"
           (t 1)))                                             ; single leading char
    (footnote-reference
     (+ (om-elem--property-length elem :label 4)
        (if (om-elem-is-empty-p elem) 0 1)))                   ; "[fn:"
    (e (error "Unknown type (head): %s" e))))

(defun om-elem--link-length-tail (link)
  (if (om-elem-property-is-equal-p :type "radio" link) 0
    (if (om-elem-contents link) 2                     ; "]]"
      (pcase (om-elem-property :format link)
        ((or `bracket
             `nil
             (guard
              (member
               type
               '("coderef" "custom-id" "fuzzy")))) 2) ; "]]"
        (`angle 1)                                    ; ">"
        (`plain 0)
        (f (error "Wrong format argument: %s") f)))))

(defconst om-elem--elements-singular
  (-> org-element-all-elements
      (-difference org-element-greater-elements)
      (-difference org-element-object-containers)))

(defun om-elem--length-newlines (elem)
  (cl-flet
      ((pb-add
        (elem extra)
        (+ extra (or (om-elem-property :post-blank elem) 0))))
    (let ((type (om-elem-type elem)))
      ;; TODO add inlinetask
      (cond
       ;; objects don't have newlines added to them
       ((or (memq type org-element-all-objects)
            (eq type 'plain-text))
        0)
       ;; singular elements have one newline + post blanks
       ((memq type om-elem--elements-singular) (pb-add elem 1))
       ;; most containers have one newline + post blanks
       ((memq type '(center-block drawer dynamic-block
                                  footnote-definition headline item
                                  property-drawer quote-block
                                  special-block table-row verse-block))
        (pb-add elem 1))
       ;; These elements should return 0 if they are empty as they
       ;; will not be parsed into anything.
       ((memq type '(plain-list table))
        (if (om-elem-is-empty-p elem) 0 (pb-add elem 1)))
       ;; `paragraph' and `section' should return 0 if empty as it
       ;; will be printed as a blank string and not be normalized
       ((memq type '(paragraph section))
        (pb-add elem (if (om-elem-is-empty-p elem) 0 1)))
       (t (error "Unknown type (newlines): %s" type))))))

(defun om-elem--length-tail (elem)
  (cl-flet
      ((pb-add
        (elem extra)
        (+ extra (or (om-elem-property :post-blank elem) 0))))
    (cl-case (om-elem-type elem)
      ;; TODO add inlinetask
      ((footnote-definition headline item paragraph plain-list
                            section table-row)
       0)
      (footnote-reference (pb-add elem 1))                                ; "]"
      (macro (pb-add elem 3))                                             ; "}}}"
      ((drawer property-drawer) 5)                                        ; ":END:"
      (dynamic-block 6)                                                   ; "#+END:"
      ((quote-block verse-block) 11)                                      ; "#+END_QUOTE" or "#+END_VERSE"
      (center-block 12)                                                   ; "#+END_CENTER"
      (special-block (om-elem--property-length elem :type 6))             ; "#+END_"
      (link (pb-add elem (om-elem--link-length-tail elem)))
      ;; These elements should return 0 if they are empty as they will
      ;; not be parsed into anything. If empty, return nil and test
      ;; for nil later to return 0
      ((bold italic strike-through underline)
       (if (om-elem-is-empty-p elem) 0 (pb-add elem 1)))                  ; single trailing char
      (radio-target
       (if (om-elem-is-empty-p elem) 0 (pb-add elem 3)))                  ; >>>
      ((subscript superscript)
       (if (om-elem-is-empty-p elem) 0
         (pb-add elem (if (om-elem-property :use-brackets-p elem) 1 0))))  ; "}"
      (table-cell (if (om-elem-is-empty-p elem) 0 (pb-add elem 2)))        ; " |"
      (table
       (if (om-elem-is-empty-p elem) 0
         (let ((fm (om-elem-property :tblfm elem)))
           (+ (--reduce-from (+ acc (length it) 9) 0 fm)                   ; "#+TBLFM: "
              (if fm (1- (length fm)) 0)))))
      (e (error "Unknown type (tail): %s" e)))))

(defun om-elem-map-contents (fun elem)
  (om-elem--elem-list
   (om-elem-type elem)
   (om-elem-properties elem)
   (funcall fun (om-elem-contents elem))))

(defmacro om-elem-map-contents* (form elem)
  `(om-elem-map-contents (lambda (it) ,form) ,elem))

(defun om-elem--table-cell-short-length (table-cell)
  "Return length of TABLE-CELL using only displayed text.
\(links will hide some of their text when displayed)."
  (cl-labels
      ((get-len
        (acc elem)
        (cond
         ((stringp elem) (+ acc (length elem)))
         ;; TODO links are not property parsed when inserted The
         ;; columns should be aligned on only the visible part but
         ;; instead they are aligned on the entire string as if it
         ;; were plain text. This is an issue with the org-element
         ;; interpreter
         ;; ((and (om-elem-is-type-p 'link elem)
         ;;       (om-elem-contents elem))
         ;;  (->> (om-elem-contents elem)
         ;;       (-reduce-from #'get-len 0)))
         ;; ((om-elem-is-type-p 'link elem)
         ;;  (length (om-elem-property :raw-link elem)))
         (t (let ((head (om-elem--length-head elem))
                  (tail (om-elem--length-tail elem)))
              (->> (om-elem-contents elem)
                   (-reduce-from #'get-len (+ head tail))
                   (+ acc)))))))
    ;; assume post-blank is zero
    (-reduce-from #'get-len 0 (om-elem-contents table-cell))))

(defun om-elem--transpose (lists)
  (cl-case (length lists)
    (1 (--map (list it) (car lists)))
    ;; TODO this won't be necessary once dash 3.0 is released
    (2 (-zip-with #'list (nth 0 lists) (nth 1 lists)))
    (t (apply #'-zip lists))))

(defun om-elem--table-align-columns (table)
  ;; tables should not have non-zero post-blanks anywhere
  ;; TODO only align table if it has more then one row
  (let* ((table (om-elem-map-contents*
                 (->>
                  ;; fill blank spaces with empty cells
                  (apply #'-pad (om-elem-build-table-cell "") it)
                  ;; set all post-blanks to 0
                  (--map
                   (->> (om-elem-set-post-blank 0 it)
                        (om-elem-map-contents*
                         (--map (om-elem-set-post-blank 0 it) it)))))
                 table))
         (len-matrix
          (->> (om-elem-contents table)
               (--remove (om-elem-property-is-eq-p :type 'rule it))
               (--map (om-elem-contents it))
               ;; transpose table
               (om-elem--transpose)
               (--map (-map #'om-elem--table-cell-short-length it))))
         ;; find the max width of each column
         (col-widths (-map #'-max len-matrix))
         ;; create matrix of spaces to pad cells according to max width
         (space-matrix
          (->>
           (-zip-with
            (lambda (col width) (--map (- width it) col))
            len-matrix col-widths)
           ;; transpose back to line up with original table
           (om-elem--transpose))))
    (cl-flet*
        ((pad
          (len obj)
          (if (stringp obj)
              (s-append (s-repeat len " ") obj)
            (om-elem-set-property :post-blank len obj)))
         (pad-last
          (len list)
          ;; TODO this can be refactored/generalized
          (cl-case (length list)
            (0 nil)
            (1 (->> (car list) (pad len) (list)))
            (t (let ((last (-last-item list)))
                 (-snoc (-drop-last 1 list) (pad len last)))))))
      (om-elem-map-contents
       (lambda (tbl-rows)
         (-zip-with
          (lambda (tbl-row space-row)
            (om-elem-map-contents
             (lambda (tbl-cells)
               (-zip-with
                (lambda (tbl-cell space-cell)
                  (pad-last space-cell tbl-cell))
                tbl-cells space-row))
             tbl-row))
          tbl-rows space-matrix))
       table))))

(defun om-elem--plain-list-repair-one (plain-list)
  "Set the numbering of PLAIN-LIST for one level."
  (let* ((bullet1 (->> (om-elem-contents plain-list)
                       (-first-item)
                       (om-elem-property :bullet)))
         (match1 (->> (s-trim bullet1)
                      (s-match "^\\([0-9A-z]+\\)\\(\\.\\|)\\)"))))
    (if match1
        ;; do ordered stuff
        ;; TODO this assumes that only numbers are going to be used
        ;; if `org-list-allow-alphabetical' is t, need to grab the
        ;; first char and convert to an int, then convert back
        ;; this also requires that the list being repaired has less
        ;; than 26 items (the number of alphabetic chars in either
        ;; case)
        (let* ((delim (nth 2 match1))
               (fmt (format "%%s%s " delim)))
          (om-elem-map-contents
           (lambda (items)
             (->> items
                  (-partition-before-pred (lambda (it) (om-elem-property :counter it)))
                  ;; TODO assume this will always be a number for now (obviously bad assumption)
                  (--map
                   (let ((n (or (om-elem-property :counter (-first-item it)) 0)))
                     (--map-indexed (om-elem-item-set-bullet (format fmt (+ n it-index)) it) it)))))
           plain-list))
      (om-elem-map-contents
       (lambda (items)
         (--map-when
          (om-elem-is-type-p 'item it)
          (om-elem-item-set-bullet (intern (s-trim bullet1)) it)
          items))
       plain-list))))

(defun om-elem--plain-list-repair (plain-list)
  "Set the numbering of PLAIN-LIST.
Note this will not produce accurate numbering, the only reason
for this function is to ensure that strings are the proper length
so boundaries can be calculated."
  (cl-labels
      ((map-item-contents
        (contents)
        (--map-when (om-elem-is-type-p 'plain-list it)
                    (om-elem--plain-list-repair-one it)
                    contents))
       (map-items
        (items)
        (--map (om-elem-map-contents #'map-item-contents it) items)))
    (->> (om-elem--plain-list-repair-one plain-list)
         (om-elem-map-contents #'map-items))))

        ;; (lambda (items)
        ;;   (--map
        ;;    (om-elem-map-contents
        ;;     (lambda (item-contents)
        ;;       (--map-when (om-elem-is-type-p 'plain-list it)
        ;;                   (om-elem--plain-list-repair-one it)
        ;;                   item-contents))
        ;;     it)
        ;;    items)))))
            

;; (defun om-elem--newline-count (elem)
;;   "Return the number of newlines ELEM will occupy when printed.
;; Note this is will only return an accurate number when the strings
;; and objects in ELEM are normalized for whitespace beforehand."
;;   (cl-flet
;;       ((count-newlines-property
;;         (elem prop extra)
;;         (let ((p (-some->> (om-elem-property prop elem)
;;                            (s-count-matches "\n"))))
;;           (+ (or p 0) extra))))
;;     (+
;;      ;; add one newline if `ELEM' is an element
;;      (if (eq 'element (om-elem-class elem)) 0 1)
;;      ;; add newlines from contents (if any)
;;      (-reduce-from #'om-elem--newline-count 0 (om-elem-contents elem))
;;      (cl-case (om-elem-type elem)
;;        (plain-text (s-count-matches "\n" elem))
;;        ;; TODO add inlinetask
;;        ;; assumptions:
;;        ;; - all properties are newline-free (including :value)
;;        ((babel-call bold code comment clock diary-sexp entity
;;                     footnote-reference footnote-definition
;;                     horizontal-rule inline-babel-call inline-src-call
;;                     italic keyword link node-property paragraph
;;                     plain-list planning section subscript radio-target
;;                     statistics-cookie strike-through superscript
;;                     table-cell table-row target timestamp underline
;;                     verbatim)
;;         0)
;;        ;; these are blocks or block-like greater elements that add one
;;        ;; newline for the first line
;;        ((center-block drawer dynamic-block quote-block
;;                       properties-drawer special-block verse-block
;;                       line-break)
;;         1)
;;        ;; these are elements/object that may have :value properties
;;        ;; that contain newlines. All other properties are assumed
;;        ;; to have no newlines
;;        ((export-snippet latex-environment latex-fragment macro)
;;         (count-newline-property elem :value 0))
;;        ;; export-blocks add any newlines in their :value property
;;        ;; and one for their header string
;;        (export-block (count-newlines-property elem :value 1))
;;        ;; tables add one newline for every formula they contain
;;        (table
;;         (or (-some-> (om-elem-property :tblfm elem) (length)) 0))
;;        ;; headlines add one newline for their header, and any
;;        ;; pre-blank newlines if they have contents
;;        (headline (1+ (if (om-elem-empty-p elem) 0
;;                        (om-elem-property :pre-blank elem))))
;;        ;; items add no newlines unless they have contents and the
;;        ;; first item in contents is not a paragraph
;;        (item
;;         (if ((or (om-elem-is-empty-p elem)
;;                  (->> (om-elem-contents elem)
;;                       (-first-item)
;;                       (om-elem-is-type-p 'paragraph)))
;;              0 1)))
;;        ;; fixed-width adds newlines from the :value property but chops
;;        ;; one newlines before
;;        (fixed-width (->> (om-elem-property :value elem)
;;                          (s-chop-suffix "\n")
;;                          (s-count-matches "\n")))
;;        ;; these blocks add one newline for their header and from their
;;        ;; :value property but only after being normalized. All other
;;        ;; properties are assumed to have no newlines
;;        ((comment-block example-block src-block)
;;         (->> (om-elem-property :value elem)
;;              (org-element-normalize-string)
;;              (s-count-matches "\n")
;;              (+ 1)))
;;        (t (error "Unknown type"))))))

;; TODO these are not recursive
(defun om-elem-shift-begin (shift elem)
  (if (om-elem-is-type-p 'plain-text elem) elem
    (om-elem-map-property* :begin (+ shift it) elem)))

(defun om-elem-shift-end (shift elem)
  (if (om-elem-is-type-p 'plain-text elem) elem
    (om-elem-map-property* :end (+ shift it) elem)))

(defun om-elem-shift-contents-begin (shift elem)
  (if (or (om-elem-is-type-p 'plain-text elem)
          (not (om-elem-property :contents-begin elem)))
          elem
    (om-elem-map-property* :contents-begin (+ shift it) elem)))

(defun om-elem-shift-contents-end (shift elem)
  (if (or (om-elem-is-type-p 'plain-text elem)
          (not (om-elem-property :contents-begin elem)))
      elem
    (om-elem-map-property* :contents-end (+ shift it) elem)))

(defun om-elem-shift-boundaries (shift elem)
  (if (om-elem-is-type-p 'plain-text elem) elem
    (->> (om-elem-shift-begin shift elem)
         (om-elem-shift-end shift)
         (om-elem-shift-contents-begin shift)
         (om-elem-shift-contents-end shift)
         (om-elem-map-contents*
          (--map (om-elem-shift-boundaries shift it) it)))))

(defun om-elem-shift-boundaries-to (point elem)
  (if (om-elem-is-type-p 'plain-text elem) elem
    (let ((diff (- point (om-elem-property :begin elem))))
      (om-elem-shift-boundaries diff elem))))

;; make a new elem with the contents indented if the head is indented
;; also need to shift these by the head shift
(defun om-elem--indent-greater-element (indent contents-indent
                                               head-lines tail-lines
                                               first-para? elem)
  (let ((head-shift (* head-lines indent))
        (tail-shift (* tail-lines indent)))
    (if (om-elem-is-empty-p elem)
        (om-elem-shift-end (+ head-shift tail-shift) elem)
      (cl-labels
          ((shift
            (e1 e2)
            (let ((p (om-elem-property :end e1)))
              (->> (om-elem-shift-boundaries-to p e2)
                   (om-elem--indent (+ indent contents-indent)))))
           (mapper
            (init contents)
            (if (not first-para?)
                (let ((dummy `(_ (:end ,init))))
                  (->> (-reductions-from #'shift dummy contents)
                       (-drop 1)))
              (let* ((first
                      (->>
                       (om-elem-contents elem)
                       (-first-item)
                       (om-elem-shift-boundaries-to init)
                       (om-elem--indent-paragraph (+ indent contents-indent) t)))
                     (dummy `(_ (:end ,(om-elem-property :end first)))))
                (->> (cdr contents)
                     (-reductions-from #'shift dummy)
                     (-drop 1)
                     (cons first))))))
        (let* ((init (+ head-shift (om-elem-property :contents-begin elem)))
               (elem* (om-elem-map-contents* (mapper init it) elem))
               ;; find the end of the contents to set contents end of
               ;; outermost element
               (contents-end-shift
                (--> (om-elem-contents elem*)
                     (-last-item it)
                     (om-elem-property :end it)
                     (- it (om-elem-property :contents-end elem)))))
          ;; find the amount of newlines after content and indent
          ;; that, need to do something similar for head
          (->> (om-elem-shift-contents-begin head-shift elem*)
               (om-elem-shift-contents-end contents-end-shift)
               (om-elem-shift-end (+ contents-end-shift tail-shift))))))))

(defun om-elem--indent-paragraph (indent skip-first? paragraph)
  (let ((shift
         (->>
          (om-elem-contents paragraph)
          (--reduce-from (if (stringp it) (s-count-matches "\n" it) 0) 0)
          (+ (if skip-first? 0 1))
          (* indent))))
    (->> (om-elem-shift-end shift paragraph)
         (om-elem-shift-contents-end shift))))

(defun om-elem--indent (indent elem)
  (let ((begin (om-elem-property :begin elem)))
    (cl-case (om-elem-type elem)
      ;; TODO add inlinetask
      ;; for single-lined elements, just move the end
      ;; assume newlines from post-blank should not be indented
      ((babel-call clock comment diary-sexp horizontal-rule keyword
                   node-property planning)
       (om-elem-shift-end indent elem))
      ;; TODO this code is wetter than donald trumps pants after he won
      ;; for multilined elements move end by (indent * newline number)
      (latex-environment
       (--> (om-elem-property :value elem)
            (s-count-matches "\n" it)
            (1+ it)
            (* indent it)
            (om-elem-shift-end it elem)))
      ;; just like above but with one more line to indent
      (export-block
       (--> (om-elem-property :value elem)
            (s-count-matches "\n" it)
            (+ 2 it)
            (* indent it)
            (om-elem-shift-end it elem)))
      ((comment-block example-block src-block)
       (--> (om-elem-property :value elem)
            (org-element-normalize-string it)
            (s-count-matches "\n" it)
            (+ 2 it)
            (* indent it)
            (om-elem-shift-end it elem)))
      (fixed-width
       (--> (om-elem-property :value elem)
            (s-chop-suffix "\n" it)
            (s-count-matches "\n" it)
            (1+ it)
            (* indent it)
            (om-elem-shift-end it elem)))
      ;; TODO these container functions currently will not shift any
      ;; of the objects inside, but this shouldn't hurt anything
      (paragraph (om-elem--indent-paragraph indent nil elem))
      (verse-block
       (let ((contents-shift
              (->>
               (om-elem-contents elem)
               (--reduce-from
                (+ acc (if (stringp it) (s-count-matches "\n" it) 0))
                0)
               (* indent))))
         (->> (om-elem-shift-contents-begin indent elem)
              (om-elem-shift-contents-end (+ contents-shift indent))
              (om-elem-shift-end (+ contents-shift (* 2 indent))))))
      ;; assume table rows are always on one line
      (table-row
       (->> (om-elem-shift-contents-begin indent elem)
            (om-elem-shift-contents-end indent)
            (om-elem-shift-end indent)))
            ;; (om-elem-map-contents*
             ;; (om-elem-shift-boundaries indent it))))
      ((center-block drawer dynamic-block property-drawer
                     quote-block special-block)
       (om-elem--indent-greater-element indent 0 1 1 nil elem))
      (footnote-definition
       (let ((first-para? (->> (om-elem-contents elem)
                               (-first-item)
                               (om-elem-is-type-p 'paragraph))))
         (if first-para?
             (om-elem--indent-greater-element indent 0 1 0 t elem)
           (om-elem--indent-greater-element indent 0 1 0 nil elem))))
      ;; TODO is headline needed here?
      ;; (headline)
      (item (om-elem--indent-item indent elem))
      ((plain-list section)
       (om-elem--indent-greater-element indent 0 0 0 nil elem))
      (table
       (let ((nformulas (length (om-elem-property :tblfm elem))))
         (om-elem--indent-greater-element indent 0 0 nformulas nil elem)))
      (e (error "Invalid type: %s" e)))))

(defun om-elem--indent-item (indent item)
  (let ((bullet-width (length (om-elem-property :bullet item)))
        (first-para? (->> (om-elem-contents item)
                          (-first-item)
                          (om-elem-is-type-p 'paragraph))))
    (if first-para?
        (om-elem--indent-greater-element indent bullet-width 1 0 t item)
      (om-elem--indent-greater-element indent bullet-width 1 0 nil item))))

;; (defun om-elem--indent-plain-list (plain-list)
;;   "Shift boundaries of PLAIN-LIST to reflect its printed form.
;; This will not add any whitespace, and will recursively traverse
;; PLAIN-LIST. This assumes PLAIN-LIST has been repaired and has valid
;; bullets."
;;   (let ((indent (->> (om-elem-contents plain-list)
;;                      (-first-item)
;;                      (om-elem-property :bullet)
;;                      (length))))
;;     (om-elem--indent indent plain-list)))

;; (defun om-elem--map-last-item (fun list)
;;   (-when-let (l (-last-item list))
;;     (append (-drop-last 1 list) (list (funcall fun l)))))

(defun om-elem--filter-zero-length (elem)
  "Remove contents from ELEM that are zero-length."
  (om-elem-map-contents* (-remove #'om-elem-is-zero-length-p it) elem))

;; elements with contents may need some touch up before
;; normalization, note that these things are done fully anyways
;; when the element is printed, but here some shortcuts are
;; possible since I only care about length
(defun om-elem--preprocess (elem)
  (cl-case (om-elem-type elem)
    ;; `table' needs spaces to be added to ensure alignment
    (table (om-elem--table-align-columns elem))
    ;; `plain-list' needs numbers/bullets to be "synchronized"
    (plain-list (om-elem--plain-list-repair elem))
    ;; any element containing objects or text needs proper
    ;; indentation. `table-row' doesn't count because they only
    ;; have one row
    ((paragraph verse-block) (org-element-normalize-contents elem))
    ;; anything else, just return unmodified
    (t elem)))

;; ;; elements have normalized newlines when printed. Deal with
;; ;; that here by ensuring that each element ends with the
;; ;; proper number of newlines and basing boundaries off this
;; (defun om-elem--normalize-newlines (elem)
;;   (cond
;;    ;; Greater elements can only contain other elements, all of which
;;    ;; must end with a single newline. Since greater elements also
;;    ;; must end with a single newline (excepting post-blank
;;    ;; whitespace) set the post-blank of the last element to 0
;;    ;; TODO what if the last element is also a greater element?
;;    ((om-elem-is-any-type-p org-element-greater-elements elem)
;;     (om-elem-map-contents
;;      (lambda (contents)
;;        (om-elem--map-last-item
;;         (lambda (it) (om-elem-set-property :post-blank 0 it))
;;         contents))
;;      elem))
;;    ;; recursive elements only contain objects but don't normalize
;;    ;; newlines when parsed so no need to modify anything
;;    ;; TODO recursive objects are not supposed to contain newlines
;;    ;; (this true in all cases?). Make function to remove them
;;    ((om-elem-is-any-type-p org-element-recursive-objects elem) elem)
;;    ;; Element containers only contain objects but also must end with a
;;    ;; single newline. For `paragraph' and `table-row' this means that
;;    ;; if the last object is a string it cannot end with any newlines
;;    ;; (the length for the one newline the element is supposed to have
;;    ;; is taken into account in `om-elem--length-tail')
;;    ((om-elem-is-any-type-p '(paragraph table-row) elem)
;;     (om-elem-map-contents
;;      (lambda (contents)
;;        (om-elem--map-last-item
;;         (lambda (it)
;;           (if (stringp it) (s-replace-regexp "\n+$" "" it) it))
;;         contents))
;;      elem))
;;    ;; In the case of `verse-block' the last line of the container is
;;    ;; is not an object so no need to worry about adjusting newlines
;;    ;; of the contents
;;    ((om-elem-is-type-p 'verse-block elem) elem)
;;    (t (error "Invalid type: %s" type))))

(defun om-elem--normalize-newlines* (parent-type elem)
  "Remove newlines from the end of ELEM depending on PARENT-TYPE.
This should only be called on the last item in the contents of the
parent elem who gives the PARENT-TYPE. This also implies that
PARENT-TYPE should refer to a container element."
  (cond
   ((memq parent-type org-element-greater-elements)
    (om-elem-set-property :post-blank 0 elem))
   ((memq parent-type org-element-recursive-objects) elem)
   ((memq parent-type '(paragraph table-row))
    (if (stringp elem) (s-replace-regexp "\n$" "" elem) elem))
    ;; (if (stringp elem) (s-trim-right elem)
    ;;   (om-elem-set-property :post-blank 0 elem)))
   ((eq parent-type 'verse-block) elem)
   (t (error "Invalid type: %s" parent-type))))

;; normalize contents recursively, and set the beginning of
;; the next element in contents based on the end of the last
(defun om-elem--set-boundaries-recursive (contents-begin elem)
  (let ((add-newlines?
         (cl-case (om-elem-type elem)
           ((footnote-definition headline inlinetask item plain-list
                                 section)
            nil)
           (table (and (om-elem-property :tblfm elem) t))
           (t t))))
    (cl-labels
        ((set-item
          (e1 e2)
          (let ((b (om-elem-property :end e1)))
            (om-elem--normalize-boundaries* b t e2)))
         (set
          (contents)
          (if (= 1 (length contents))
              (->> (car contents)
                   (om-elem--normalize-newlines* (om-elem-type elem))
                   (om-elem--normalize-boundaries* contents-begin
                                                   add-newlines?)
                   (list))
            ;; TODO if parent element is a recursive object, do this
            ;; if it is a greater element, normalize without including
            ;; newlines in the last element
            (let* ((last (-last-item contents))
                   ;; dummy element to initialize the list reduction
                   (dummy `(_ (:end ,contents-begin)))
                   (front (->> (-drop-last 1 contents)
                               (-reductions-from #'set-item dummy)
                               ;; drop the dummy
                               (-drop 1)))
                   (contents-end (->> (-last-item front)
                                      (om-elem-property :end))))
              (->> last
                   (om-elem--normalize-newlines* (om-elem-type elem))
                   (om-elem--normalize-boundaries* contents-end
                                                   add-newlines?)
                   ;; (set-item (-last-item front))
                   (-snoc front))))))
      (om-elem-map-contents #'set elem))))

;; some elements may need additional modification after
;; normalization. The only thing here now is `plain-list' which
;; needs the boundaries that it would have once printed
(defun om-elem--postprocess (elem)
  (cl-case (om-elem-type elem)
    (item (om-elem--indent-item 0 elem))
    (t elem)))

(defconst om-elem--containers
  (append org-element-greater-elements org-element-object-containers)
  "Elements and objects that can contain other elements/objects.")

(defun om-elem--normalize-container-boundaries (begin add-newlines? elem)
  (if (om-elem-is-empty-p elem)
      ;; TODO this is a complex test...refactor
      (unless (or (om-elem-is-any-type-p om-elem--rm-if-empty elem)
                  (and (om-elem-is-type-p 'table-row elem)
                       (om-elem-property-is-eq-p :type 'standard elem)))
        (let ((head (om-elem--length-head elem))
              (tail (+ (om-elem--length-tail elem)
                       (if add-newlines? (om-elem--length-newlines elem) 0))))
          (om-elem-set-properties (list :begin begin
                                        :end (+ begin head tail)
                                        :contents-begin nil
                                        :contents-end nil)
                                  elem)))
    ;; if `ELEM' has contents, make sure they are formatted
    ;; properly and set boundaries recursively
    (let* ((head (om-elem--length-head elem))
           (tail (+ (om-elem--length-tail elem)
                    (if add-newlines? (om-elem--length-newlines elem) 0)))
           (contents-begin (+ begin head))
           (elem*
            (->> (om-elem--preprocess elem)
                 ;; (om-elem--normalize-newlines)
                 (om-elem--set-boundaries-recursive contents-begin)))
                 ;; (om-elem--postprocess)))
           (contents-end (->> (om-elem-contents elem*)
                              (-last-item)
                              (om-elem-property :end))))
      (->>
       elem*
       (om-elem-set-properties
        (list :begin begin
              :end (+ contents-end tail)
              :contents-begin contents-begin
              :contents-end contents-end))))))
;; TODO move this to the recursive section
;; this isn't actually necessary but it makes things
;; easier to read when debugging, if speed is needed
;; take this out
;; (om-elem-map-contents*
;; (-map-when #'stringp #'substring-no-properties it)))))))

(defun om-elem--normalize-boundaries* (begin add-newlines? elem)
  ;; First determine if `ELEM' can possibly have contents. If not,
  ;; don't need to worry about contents-begin/end and can just
  ;; directly set the end by finding the length. If `ELEM' can have
  ;; contents, remove zero-len contents before processing.
  (if (om-elem-is-any-type-p om-elem--containers elem)
      (->> (om-elem--filter-zero-length elem)
           (om-elem--normalize-container-boundaries begin add-newlines?))
           ;; (om-elem--postprocess))
    (let ((end
           (+ begin
              (om-elem--length elem)
              (if add-newlines? (om-elem--length-newlines elem) 0))))
      (om-elem-set-properties `(:begin ,begin :end ,end) elem))))

(defun om-elem--normalize-boundaries (begin elem)
  ;; TODO is clone really necessary? it's here so that components
  ;; of the tree that are composed from common identifiers don't get
  ;; changed at the same time
  (->> (-clone elem)
       (om-elem--normalize-boundaries* begin t)
       (om-elem--postprocess)))

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

(defun om-elem-set-post-blank (post-blank elem)
  (om-elem-set-property :post-blank (or post-blank 0) elem))

(defun om-elem--add-generic-props (l post-blank)
  (let ((post-blank (or post-blank 0)))
    (append l (list :begin nil :end nil :parent nil
                    :post-affiliated nil :post-blank post-blank))))

(defun om-elem--add-contents-props (l)
  (append l (list :contents-begin nil :contents-end nil)))

(defun om-elem--build (props type &optional contents)
  (append (list type props) contents))

;;; builders

;; objects

(defun om-elem--build-object (props type post-blank)
  (om-elem--build (om-elem--add-generic-props props post-blank) type))

(om-elem--defun om-elem-build-code (value &key post-blank)
  "Build a code object from VALUE."
  (om-elem--build-object (list :value value) 'code post-blank))

(om-elem--defun om-elem-build-entity (name &key use-brackets-p post-blank)
  "Build a entity object from NAME."
  (om-elem--build-object (list :name name
                               :use-brackets-p use-brackets-p
                               ;; placeholders
                               :ascii nil
                               :latex nil
                               :latex-math-p nil
                               :latin1 nil
                               :utf-8 nil)
                         'entity
                         post-blank))

(om-elem--defun om-elem-build-export-snippet (back-end value &key post-blank)
  "Build an export-block element with BACK-END and TYPE."
  (om-elem--build-object (list :value value :back-end type)
                         'export-block
                         post-blank))

(om-elem--defun om-elem-build-inline-babel-call (call &key post-blank
                                                arguments
                                                inside-header
                                                end-header)
  "Build an inline-babel-call element for NAME.
Optionally provide ARGS, inside header args INSIDE, and end header
args END."
  (-> (list :call call
            :arguments arguments
            :inside-header inside-header
            :end-header end-header)
      (om-elem--build-object 'inline-babel-call post-blank)))

(om-elem--defun om-elem-build-inline-src-block (language value &key parameters
                                               post-blank)
  "Build an inline-src-block object with LANGUAGE and VALUE.
Optionally provide PARAMETERS."
  (-> (list :language language
            :value value
            :parameters parameters)
      (om-elem--build-object 'inline-src-block post-blank)))

;; TODO add latex-fragment

(om-elem--defun om-elem-build-line-break (&key post-blank)
  "Build a line-break object."
  (om-elem--build-object nil 'line-break post-blank))

(om-elem--defun om-elem-build-macro (key &key post-blank args)
  "Build a macro object with KEY and optional ARGS."
  (om-elem--build-object
   (list :value (->> (if args (format "%s(%s)" key (s-join "," args)) key)
                     (format "{{{%s}}}"))
         :args args
         :key key)
   'macro post-blank))

(om-elem--defun om-elem-build-statistics-cookie (&optional num dem &key post-blank)
  "Build a statistics cookie object with NUM and DEM."
  (let ((value
         (->> (cond
               ((and num dem) (format "%s/%s" num dem))
               (num (format "%s%%" num dem))
               (t "%"))
              (format "[%s]"))))
    (om-elem--build-object (list :value value)
                           'statistics-cookie post-blank)))

(om-elem--defun om-elem-build-target (value &key post-blank)
  "Build a target object with VALUE."
  (om-elem--build-object (list :value value) 'target post-blank))

(om-elem--defun om-elem-build-timestamp (type time1 &optional time2 &key
                                        post-blank
                                        repeater-type
                                        repeater-unit
                                        repeater-value
                                        warning-type
                                        warning-unit
                                        warning-value)
  "Build a timestamp object with TYPE, TIME1, and optionally TIME2.
TYPE is either 'inactive' or 'active', TIME1 and TIME2 are lists of
digits specifying the time formatted like '(year month day)' or
'(year month day hour minute)'. Supplying TIME2 will create a
timestamp range."
  (cl-flet
      ((match-time
        (time &optional end)
        (when time
          (let ((props (if end (list :year-end :month-end :day-end
                                     :hour-end :minute-end)
                         (list :year-start :month-start :day-start
                               :hour-start :minute-start))))
            (pcase time
              ((or `(,(pred integerp)
                     ,(pred integerp)
                     ,(pred integerp))
                   `(,(pred integerp)
                     ,(pred integerp)
                     ,(pred integerp)
                     ,(pred integerp)
                     ,(pred integerp)))
               (-interleave props time))
              (_ (error "Invalid time given: %s" time)))))))
    ;; TODO this can be cleaned up
    (let ((type (cond
                 ((and time2 (eq type 'active)) 'active-range)
                 ((and time2 (eq type 'inactive)) 'inactive-range)
                 (t type))))
      (-> (list :repeater-type nil
                :repeater-unit nil
                :repeater-value nil
                :type type
                :warning-type nil
                :warning-unit nil
                :warning-value nil)
          (append (match-time time1) (match-time time2 t))
          (om-elem--build-object 'timestamp post-blank)))))

(om-elem--defun om-elem-build-diary-sexp-timestamp (string &key post-blank)
  "Build a diary-sexp timestamp element from STRING.
STRING is a lisp form as a string."
  (-> (list :type 'diary
            :raw-value (format "<%%%%%s>" string)
            :repeater-type nil
            :repeater-unit nil
            :repeater-value nil
            :warning-type nil
            :warning-unit nil
            :warning-value nil
            :year-start nil
            :month-start nil
            :day-start nil
            :hour-start nil
            :minute-start nil
            :year-end nil
            :month-end nil
            :day-end nil
            :hour-end nil
            :minute-end nil)
      (om-elem--build-object 'timestamp post-blank)))
        
(om-elem--defun om-elem-build-verbatim (value &key post-blank)
  "Build a verbatim object with VALUE."
  (om-elem--build-object (list :value value) 'verbatim post-blank))

;; recursive objects

;; TODO all of these must have at least one in contents

(defun om-elem--build-recursive-object (props type post-blank objs)
  ;; TODO in all case objs must be a valid type (string or org list)
  (-> (om-elem--add-generic-props props post-blank)
      (om-elem--build type objs)))

(om-elem--defun om-elem-build-bold (&key post-blank &rest objs)
  "Build a bold object containing OBJS."
  (om-elem--build-recursive-object nil 'bold post-blank objs))

(om-elem--defun om-elem-build-footnote-reference (&key post-blank
                                                       (label "1")
                                                       &rest objs)
  "Build a footnote reference object to TARGET.
Optionally make the reference inline by setting INLINE to t."
  (om-elem--build-recursive-object
   (list :label label :type nil) 'footnote-reference post-blank objs))

(om-elem--defun om-elem-build-italic (&key post-blank &rest objs)
  "Build an italic object from STRING."
  (om-elem--build-recursive-object nil 'italic post-blank objs))

(om-elem--defun om-elem-build-link (path &key post-blank (type "fuzzy")
                                         format &rest objs)
  "Build a link object from TARGET with OBJS as the description."
  (om-elem--build-recursive-object
   (list :path path :type type :format format) 'link post-blank objs))

(om-elem--defun om-elem-build-radio-target (&key post-blank &rest objs)
  "Build a radio target object from STRING."
  (om-elem--build-recursive-object nil 'radio-target post-blank objs))

(om-elem--defun om-elem-build-strike-through (&key post-blank &rest objs)
  "Build a strike-through object from STRING."
  (om-elem--build-recursive-object nil 'strike-through post-blank objs))

(om-elem--defun om-elem-build-superscript (&key post-blank
                                                use-brackets-p
                                                &rest objs)
  "Build a superscript object from STRING."
  (om-elem--build-recursive-object `(:use-brackets-p ,use-brackets-p)
                                   'superscript post-blank objs))

(om-elem--defun om-elem-build-subscript (&key post-blank
                                              use-brackets-p
                                              &rest objs)
  "Build a subscript object from STRING."
  (om-elem--build-recursive-object `(:use-brackets-p ,use-brackets-p)
                                   'subscript post-blank objs))

(om-elem--defun om-elem-build-table-cell (&key post-blank &rest objs)
  "Build a table cell object containing TEXT."
  (om-elem--build-recursive-object nil 'table-cell post-blank objs))

(om-elem--defun om-elem-build-underline (&key post-blank &rest objs)
  "Build an underline object from STRING."
  (om-elem--build-recursive-object nil 'underline post-blank objs))

;; elements

(om-elem--defun om-elem-build-babel-call (call &key post-blank arguments
                                         inside-header end-header)
  "Build a babel-call element for NAME.
Optionally provide ARGS, inside header args INSIDE, and end header
args END."
  (-> (list :call call
            :arguments arguments
            :inside-header inside-header
            :end-header end-header)
      (om-elem--build-object 'babel-call post-blank)))

(om-elem--defun om-elem-build-clock (time1 &optional time2 &key post-blank)
  "Build a clock element with TIME1.
Optionally supply TIME2 to create a closed clock."
  (cl-flet
      ((format-duration
        (seconds)
        (let* ((h (-> seconds (/ 3600) floor))
               (m (-> seconds (- (* h 3600)) (/ 60) floor)))
          (format "%2d:%02d" h m)))
       (ts2ft
        (ts)
        (->> (om-elem-property :raw-value ts)
             (org-2ft)
             (round))))
    (let* ((ts (om-elem-build-timestamp 'inactive time1 time2))
           (duration
            (when time2
              (let ((ft1 (ts2ft (org-timestamp-split-range ts)))
                    (ft2 (ts2ft (org-timestamp-split-range ts t))))
                (-> (- ft2 ft1) (round) (format-duration))))))
      (-> (list :status nil
                :value ts
                :duration duration)
          (om-elem--build-object 'clock post-blank)))))

(om-elem--defun om-elem-build-comment (value &key post-blank)
  "Build a comment element with VALUE."
  (om-elem--build-object (list :value value) 'comment post-blank))

(om-elem--defun om-elem-build-comment-block (value &key post-blank)
  "Build a comment block element from VALUE."
  (om-elem--build-object (list :value value) 'comment-block post-blank))

(om-elem--defun om-elem-build-diary-sexp (value &key post-blank)
  "Build a diary sexp element from VALUE.
VALUE is the part inside the '%%(value)' part of the sexp."
  (let ((value (format "%%%%(%s)" value)))
    (om-elem--build-object (list :value value) 'diary-sexp post-blank)))

(om-elem--defun om-elem-build-example-block (value &key
                                                   post-blank
                                                   switches
                                                   preserve-indent)
  "Build a example block element from STRING."
  (-> (list :value (org-element-normalize-string value)
            :switches switches
            :preserve-indent preserve-indent)
      (om-elem--build-object 'example-block post-blank)))

(om-elem--defun om-elem-build-export-block (type value &key post-blank)
  "Build an export-block element with TYPE and VALUE."
  (om-elem--build-object (list :value value :type type) 'export-block
                         post-blank))

(om-elem--defun om-elem-build-fixed-width (value &key post-blank)
  "Build a fixed-width element from STRING."
  (om-elem--build-object (list :value value) 'fixed-width post-blank))

(om-elem--defun om-elem-build-horizontal-rule (&key post-blank)
  "Build a horizontal-rule element."
  (om-elem--build-object nil 'horizontal-rule post-blank))

(om-elem--defun om-elem-build-keyword (key value &key post-blank)
  "Build keyword element with keyword KEY and value VAL."
  (om-elem--build-object (list :key key :value value) 'keyword
                         post-blank))

(om-elem--defun om-elem-build-latex-environment (env text &key post-blank)
  "Build a latex-environment element with environment ENV and TEXT."
  (let ((value (format "\\begin{%1$s}\n%2$s\n\\end{%1$s}" env text)))
    (om-elem--build-object `(:value ,value) 'latex-environment post-blank)))

(om-elem--defun om-elem-build-node-property (key value &key post-blank)
  "Build a node property object with KEY and VAL."
  (om-elem--build-object (list :key key :value value) 'node-property
                         post-blank))

(om-elem--defun om-elem-build-planning (&key closed scheduled deadline post-blank)
  "Build planning element with TYPE and TIME."
  (->
   (list
    :closed (and closed (om-elem-build-timestamp 'inactive closed))
    :scheduled (and scheduled (om-elem-build-timestamp 'inactive scheduled))
    :deadline (and deadline (om-elem-build-timestamp 'inactive deadline)))
   (om-elem--build-object 'planning post-blank)))

(om-elem--defun om-elem-build-src-block (value &key language switches
                                        parameters preserve-indent
                                        post-blank)
  (-> (list :value value
            :language language
            :switches switches
            :parameters parameters
            :preserve-indent preserve-indent)
   (om-elem--build-object 'src-block post-blank)))

;; container elements

(om-elem--defun om-elem-build-paragraph (&key post-blank &rest objs)
  "Build a paragraph container element with OBJECTS as contents."
  (om-elem--build-recursive-object nil 'paragraph post-blank objs))

(om-elem--defun om-elem-build-table-row (&key post-blank &rest objs)
  "Build a table-row container element with OBJECTS as contents."
  ;; TODO this should only allow table rows
  (om-elem--build-recursive-object
   (list :type 'standard) 'table-row post-blank objs))

(om-elem--defun om-elem-build-table-row-hline (&key post-blank)
  (om-elem--build-object (list :type 'rule) 'table-row post-blank))

(om-elem--defun om-elem-build-verse-block (&key post-blank &rest objs)
  "Build a verse-block container element with OBJECTS as contents."
  (om-elem--build-recursive-object nil 'verse-block post-blank objs))

;; greater elements

(om-elem--defun om-elem-build-center-block (&key post-blank &rest elems)
  "Build a center block greater element with ELEMS as contents."
  ;; TODO need to hack this so that when elems is nil we insert
  ;; a blank paragraph. Otherwise the interpreter will put nil
  ;; as the value when we print it
  ;; TODO there are others affected by this, make more unit tests
  (om-elem--build-recursive-object nil 'center-block post-blank elems))

(om-elem--defun om-elem-build-drawer (drawer-name &key post-blank &rest elems)
  "Create drawer greater element with NAME and ELEMS as contents."
  (-> (list :drawer-name drawer-name)
      (om-elem--build-recursive-object 'drawer post-blank elems)))

(om-elem--defun om-elem-build-dynamic-block (block-name
                                             arguments
                                             &key post-blank
                                             &rest elems)
  "Build a dynamic block greater element called NAME with PARAMS.
PARAMS is s list of cons cells for each key/val pair. Optionally
provide ELEMS as contents."
  (-> (list :block-name block-name
            :arguments arguments)
      (om-elem--build-recursive-object 'dynamic-block post-blank elems)))

(om-elem--defun om-elem-build-footnote-definition (label
                                                   &key post-blank
                                                   &rest elems)
  "Build a footnote-definition greater element for LABEL.
Optionally provide ELEMS as contents."
  (om-elem--build-recursive-object
   (list :label label) 'footnote-definition post-blank elems))

(om-elem--defun om-elem-build-headline (&key title (level 1)
                                             post-blank pre-blank
                                             todo-keyword tags
                                             ;; TODO bound this by org-lowest/highest-priority
                                             priority
                                             footnote-section-p
                                             commentedp archivedp
                                             &rest elems)
  "Build a headline."
  (-> (list :title title
            :pre-blank pre-blank
            :level level
            :todo-keyword todo-keyword
            :tags tags
            :priority priority
            :footnote-section-p footnote-section-p
            :commentedp commentedp
            :archivedp archivedp
            ;; placeholders
            :todo-type nil
            :raw-value nil)
      (om-elem--build-recursive-object 'headline post-blank elems)))

;; TODO add inline text

(defun om-elem--format-bullet (bullet)
  (cond
   ((integerp bullet) (format "%s. " bullet))
   ((memq bullet '(- +)) (format "%s " bullet))
   ;; TODO use alphanumeric if org-list-allow-alphabetical = t
   ((and (stringp bullet)
         (s-matches? "[:space:]*[0-9]+\\(\\.\\|)\\)[:space:]*" bullet))
    bullet)
   (t (error "Invalid bullet: %s" bullet))))

(om-elem--defun om-elem-build-item (&key post-blank
                                         (bullet '-) checkbox tag
                                         counter &rest elems)
  "Build a plain-list greater element with ELEMS as contents."
  (-> (list :bullet (om-elem--format-bullet bullet)
            :checkbox checkbox
            :counter counter
            :tag tag
            ;; placeholders
            :structure nil)
      (om-elem--build-recursive-object 'item post-blank elems)))

(om-elem--defun om-elem-build-plain-list (&key post-blank &rest elems)
   ;; TODO only allow item elems
  "Build a plain-list greater element with ELEMS as contents."
  ;; all props are placeholders
  (om-elem--build-recursive-object
   (list :structure nil :type nil) 'plain-list post-blank elems))

(om-elem--defun om-elem-build-property-drawer (&key post-blank &rest elems)
  "Build a property-drawer greater element with ELEMS as contents."
  ;; TODO only allow node properties
  (om-elem--build-recursive-object nil 'property-drawer post-blank elems))

(om-elem--defun om-elem-build-quote-block (&key post-blank &rest elems)
  "Build a quote-block greater element with ELEMS as contents."
  (om-elem--build-recursive-object nil 'quote-block post-blank elems))

(om-elem--defun om-elem-build-section (&key post-blank &rest elems)
  "Build a section grater element with ELEMS as contents."
  (om-elem--build-recursive-object nil 'section post-blank elems))

(om-elem--defun om-elem-build-special-block (type &key post-blank
                                                  &rest elems)
  "Build a special block greater element with ELEMS as contents."
  (om-elem--build-recursive-object (list :type type) 'special-block
                                    post-blank elems))

(om-elem--defun om-elem-build-table (&key tblfm post-blank &rest elems)
  "Build a section grater element with ELEMS as contents."
  ;; TODO this only deals with org tables for now
  (-> (list :tblfm tblfm
            :type 'org
            :value nil)
      (om-elem--build-recursive-object 'table post-blank elems)))

;; shortcut builders

(om-elem--defun om-elem-build-headline! (&key (level 1) text todo-keyword tags
                                             post-blank pre-blank
                                             priority commentedp archivedp
                                             planning properties
                                             statistics-cookie
                                             section-contents
                                             subheadlines)
  "Build a headline..."
  (let* ((statistics-cookie (-some->>
                             statistics-cookie
                             (apply #'om-elem-build-statistics-cookie)))
         (title (append (om-elem-build-secondary-string text) statistics-cookie))
         (planning (-some->> planning (apply #'om-elem-build-planning)))
         (property-drawer (-some->> properties (apply #'om-elem-build-property-drawer!)))
         (section (-some->>
                   (append `(,planning) `(,property-drawer) section-contents)
                   (-non-nil)
                   (apply #'om-elem-build-section)))
         (elems (-non-nil (append (list section) subheadlines))))
    (apply #'om-elem-build-headline
           :post-blank post-blank
           :pre-blank pre-blank
           :priority priority
           :commentedp commentedp
           :archivedp archivedp
           :title title
           elems)))

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
    ;; index
    ((and (pred integerp) index)
     (-some->
      (if (< index 0)
          (nth (- (* -1 index) 1) (nreverse contents))
        (nth index contents))
      (list)))

    ;; type
    ((and (pred symbolp) type)
     ;; TODO check for valid type?
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
     ;; TODO refactor this with plist-member?
     (cl-flet
         ((all-props-match?
           (elem props)
           (->> (-slice props 0 nil 2)
                (--map (equal (plist-get props it)
                              (om-elem-property it elem)))
                (-none? #'null))))
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
  (-some->> (om-elem-contents headline)
            (--filter (om-elem-headline-p it))))

(defun om-elem-headline-get-section (headline)
  "Return section for headline HEADLINE element or nil if none."
  (-some->> (om-elem-contents headline) (assoc 'section)))

(defun om-elem-headline-get-drawer (name headline)
  "Return first drawer with NAME in HEADLINE element or nil if none."
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
  (-some->> (om-elem-contents item) (assoc 'plain-list)))

(defun om-elem-item-get-paragraph (item)
  "Return paragraph under ITEM element or nil if none."
  (-some->> (om-elem-contents item) (assoc 'paragraph)))

;; table

;; TODO add negative indices
(defun om-elem-table-get-cell (row column table)
  "Return table-cell element at ROW and COLUMN indices in TABLE element.
Hlines do not count toward row indices, and all indices are
zero-indexed."
  (-some->> (om-elem-contents table)
            (--filter (om-elem-property-is-eq-p :type 'standard it))
            (nth row)
            (om-elem-contents)
            (nth column)))

;; timestamp

(defun om-elem-timestamp-get-start (timestamp)
  "Return the start of TIMESTAMP element."
  (let ((type (om-elem-property :type timestamp)))
    (if (memq type '(inactive-range active-range))
        (org-timestamp-split-range timestamp)
      timestamp)))

(defun om-elem-timestamp-get-end (timestamp)
  "Return the end of TIMESTAMP element or nil if not present."
  (let ((type (om-elem-property :type timestamp)))
    (if (memq type '(inactive-range active-range))
        (org-timestamp-split-range timestamp t))))

(defun om-elem-timestamp-get-unixtime (timestamp)
  "Return the unixtime value of TIMESTAMP element as an integer.
Note this only considers the start of the timestamp if it is range."
  (let ((minute (or (om-elem-property :minute-start elem) 0))
        (hour (or (om-elem-property :hour-start elem) 0))
        (day (om-elem-property :day-start elem))
        (month (om-elem-property :month-start elem))
        (year (om-elem-property :year-start elem)))
    (-> (encode-time 0 minute hour day month year)
        (float-time)
        (round))))

(defun om-elem-timestamp-get-end-unixtime (elem)
  "Return the unixtime value of timestamp ELEM's end as an integer."
  (-some->> (om-elem-timestamp-get-end elem) (om-elem-timestamp-get-unixtime)))

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

(defun om-elem-is-type-p (type elem)
  "Return t if ELEM's type is `eq' to TYPE (a symbol)."
  (eq (om-elem-type elem) type))

(defun om-elem-is-any-type-p (types elem)
  "Return t if ELEM's type is any in TYPES (a list of symbols)."
  (if (memq (om-elem-type elem) types) t))

;; clock

(defun om-elem-clock-is-running-p (clock)
  "Return t if CLOCK element is running (eg is open)."
  (om-elem-property-is-eq-p :status 'running clock))

;; headline

(defun om-elem-headline-is-done-p (headline)
  "Return t if HEADLINE element has a DONE todo keyword."
  (om-elem-property-is-eq-p :todo-type 'done headline))

(defun om-elem-headline-is-scheduled-p (headline)
  "Return t if HEADLINE element is scheduled."
  (om-elem-property-is-non-nil-p :scheduled headline))

(defun om-elem-headline-is-deadlined-p (headline)
  "Return t if HEADLINE element has a deadline."
  (om-elem-property-is-non-nil-p :deadline headline))

(defun om-elem-headline-is-closed-p (headline)
  "Return t if HEADLINE element is closed."
  (om-elem-property-is-non-nil-p :closed headline))

;; (defun om-elem-headline-is-quoted-p (headline)
;;   "Return t if HEADLINE element is quoted."
;;   (om-elem-property-is-non-nil-p :quotedp headline))

(defun om-elem-headline-is-archived-p (headline)
  "Return t if HEADLINE element is archived."
  (om-elem-property-is-non-nil-p :archivedp headline))

(defun om-elem-headline-is-commented-p (headline)
  "Return t if HEADLINE element is commented."
  (om-elem-property-is-non-nil-p :commentedp headline))

(defun om-elem-headline-has-tag-p (tag headline)
  "Return t if HEADLINE element is tagged with TAG."
  (if (member tag (om-elem-property :tags headline)) t))

;; item

(defun om-elem-item-is-checked-p (item)
  "Return t if ITEM element is checked."
  (om-elem-property-is-eq-p :checkbox 'on item))

(defun om-elem-item-is-unchecked-p (item)
  "Return t if ITEM element is unchecked."
  (om-elem-property-is-eq-p :checkbox 'off item))

(defun om-elem-item-is-trans-p (item)
  "Return t if ITEM element is transitional."
  (om-elem-property-is-eq-p :checkbox 'trans item))

;; statistics cookie

(defun om-elem-statistics-cookie-is-complete-p (statistics-cookie)
  "Return t is STATISTICS-COOKIE element is complete."
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
  (or (om-elem-property-is-eq-p :type 'active timestamp)
      (om-elem-property-is-eq-p :type 'active-range timestamp)))

(defun om-elem-timestamp-is-inactive-p (timestamp)
  "Return t if TIMESTAMP elem is inactive."
  (or (om-elem-property-is-eq-p :type 'inactive timestamp)
      (om-elem-property-is-eq-p :type 'inactive-range timestamp)))

(defun om-elem-timestamp-is-ranged-p (timestamp)
  "Return t if TIMESTAMP elem is ranged."
  (or (om-elem-property-is-eq-p :type 'active-range timestamp)
      (om-elem-property-is-eq-p :type 'inactive-range timestamp)))

;; TODO these are all relative to localtime, need to control for that
(defun om-elem-timestamp-is-less-than-p (unixtime timestamp)
  "Return t if TIMESTAMP elem is less than UNIXTIME."
  (< (om-elem-timestamp-get-unixtime timestamp) unixtime))

(defun om-elem-timestamp-is-greater-than-p (unixtime timestamp)
  "Return t if TIMESTAMP elem is greater than UNIXTIME."
  (> (om-elem-timestamp-get-unixtime timestamp) unixtime))

(defun om-elem-timestamp-is-equal-p (unixtime timestamp)
  "Return t if TIMESTAMP elem is equal to UNIXTIME."
  (= (om-elem-timestamp-get-unixtime timestamp) unixtime))

(defun om-elem-timestamp-is-end-less-than-p (unixtime timestamp)
  "Return t if TIMESTAMP elem is less than UNIXTIME."
  (-when-let (end (om-elem-timestamp-get-end-unixtime timestamp))
    (< end unixtime)))

(defun om-elem-timestamp-is-end-greater-than-p (unixtime timestamp)
  "Return t if TIMESTAMP elem is greater than UNIXTIME."
  (-when-let (end (om-elem-timestamp-get-end-unixtime timestamp))
    (> end unixtime)))

(defun om-elem-timestamp-is-end-equal-p (unixtime timestamp)
  "Return t if TIMESTAMP elem is equal to UNIXTIME."
  (-when-let (end (om-elem-timestamp-get-end-unixtime timestamp))
    (= end unixtime)))

(defun om-elem-timestamp-is-in-range-p (unixtime timestamp)
  "Return t if UNIXTIME is between start and end of TIMESTAMP elem."
  (let ((ut1 (om-elem-timestamp-get-unixtime timestamp))
        (ut2 (om-elem-timestamp-get-end-unixtime timestamp)))
    (when ut2 (< ut1 unixtime ut2))))

;;; element setters

;; generic

(defun om-elem-set-property (prop value elem)
  "Set property PROP in element ELEM to VALUE."
  ;; TODO validate that prop exists in elem first?
  (if (stringp elem) (org-add-props elem nil prop value)
    (om-elem--elem-list
     (om-elem-type elem)
     (plist-put (om-elem-properties elem) prop value)
     (om-elem-contents elem))))

(defun om-elem-set-properties (plist elem)
  "Set all properties in ELEM to the values corresponding to PLIST.
PLIST is a list of property-value pairs that correspond to the
property list in ELEM."
  (cond
   ((not plist) elem)
   ((om-elem--is-plist-p plist)
    (->> (om-elem-set-property (nth 0 plist) (nth 1 plist) elem)
         (om-elem-set-properties (-drop 2 plist))))
   (t (error "Not a plist: %s" plist))))

(defun om-elem-set-recursive-content (content elem)
  ;; TODO this should only allow recursive types (eg bold, link, etc)
   (let ((head (om-elem-head elem)))
     (if content (append head content) head)))

(defun om-elem-set-post-blank (post-blank elem)
  "Set the :post-blank property of ELEM to POST-BLANK."
  (if (stringp elem) (s-append (s-repeat post-blank " ") elem)
    (om-elem-set-property :post-blank post-blank elem)))

;; headline

(defun om-elem-headline-set-todo (todo headline)
  "Set the todo keyword of HEADLINE element to TODO."
  (om-elem-set-property :todo-keyword todo headline))

;; (defun om-elem-headline-set-level (level elem)
;;   (om-elem-set-property :level level elem))

(defun om-elem-headline-set-archived (flag headline)
  "Set the archived flag of HEADLINE element to FLAG."
  ;; TODO do we actually need the archive flag to be set?
  (let ((new-tags
         (--> (om-elem-property :tags headline)
              (if flag (cons org-archive-tag it)
                (-remove-item org-archive-tag it)))))
    (->> headline
         (om-elem-set-property :archivedp flag)
         (om-elem-set-property :tags new-tags))))

(defun om-elem-headline-set-commented (flag headline)
  "Set the commented flag of HEADLINE element to FLAG."
  (om-elem-set-property :commentedp flag headline))

(defun om-elem-headline-update-statistics-cookie (todo headline)
  ;; TODO the todo arg is clunky
  ;; TODO make sure there is actually a cookie to modify
  (let* ((title (om-elem-property :title headline))
         (cookie (assoc 'statistics-cookie title))
         ;; TODO need to filter out non-todo headlines
         ;; TODO use subheadlines function for this
         (total (if todo (om-elem-find '(headline) headline)
                  (om-elem-find '(section plain-list item) headline)))
         ;; TODO these will error none found
         (complete (if todo (-filter #'om-elem-headline-is-done-p total)
                     (-filter #'om-elem-item-is-checked-p total)))
         (total-len (length total))
         (complete-len (length complete))
         (new-cookie
          (--> (om-elem-property :value cookie)
               (cond
                ((s-contains? "/" it)
                 (format "[%s/%s]" complete-len total-len))
                ((s-contains? "%" it)
                 (->> (float total-len)
                      (/ complete-len)
                      (* 100)
                      (round)
                      (format "[%s%%]")))
                (t (error "Invalid statistics cookie %s" it)))
               (om-elem-set-property :value it cookie)))
         (new-title (-replace-first cookie new-cookie title)))
    (om-elem-set-property :title new-title headline)))

(defun om--to-relative-priority (p)
  "Convert P from character value to counting integer starting at 0."
  ;; ironically the highest priority is actually the lowest integer
  (when p
    (if (and (<= p org-lowest-priority) (>= p org-highest-priority))
        (- p org-highest-priority)
      (error "Absolute priority out of range: %s" p))))

(defun om--to-absolute-priority (p)
  (when p
    (if (and (>= p 0)
             (<= p (abs (- org-highest-priority org-lowest-priority))))
        (+ org-highest-priority p)
      (error "Relative priority out of range: %s" priority))))

(defun om-elem-headline-set-priority (priority headline)
  "Set the priority of HEADLINE element to PRIORITY."
  ;; (let ((priority (om--to-absolute-priority priority)))
  (om-elem-set-property :priority priority headline))

(defun om-elem-headline-set-title (title headline)
  "Set the title of HEADLINE element to TITLE."
  (om-elem-set-property :title title headline))

;; TODO make shortcut function for setting the headline title

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
  (if (or (null state) (memq state '(off on trans)))
      (om-elem-set-property :checkbox state item)
    (error "Invalid state: %s" state)))

(defun om-elem-item-set-bullet (bullet item)
  "Set the bullet of ITEM element to BULLET.
BULLET is either '-' or '+' or an integer greater than zero.
Note that `org-element-item-interpreter' currently does not interpret
'+' bullets properly and will render these as '-'."
  (om-elem-set-property :bullet (om-elem--format-bullet bullet) item))

(defun om-elem-item-set-tag (tag item)
  "Set the tag of ITEM element to TAG where TAG is a string or nil."
  (unless (or (null tag) (stringp tag))
    (error ("Invalid tag: %s" tag)))
  (om-elem-set-property :tag tag item))

;; TODO there seems to be a bug in the org-interpeter that prevents
;; "+" bullets from being recognized (as of org-9.1.9 they are simply
;; read as "-")
(defun om-elem-plain-list-set-type (type plain-list)
  "Set the type of PLAIN-LIST greater element to TYPE.
TYPE is '-', '+', or 'ordered'."
  (cond
   ((memq type '(+ -))
    (om-elem-map* '(item) (om-elem-item-set-bullet type it) plain-list))
   ((eq type 'ordered)
    ;; TODO the org-interpreter seems to use the correct, ordered
    ;; numbers if any number is set here. This behavior is likely not
    ;; reliable. Need to make an om-elem-map-index for this
    (om-elem-map* '(item) (om-elem-item-set-bullet 1 it) plain-list))
   (t (error "Invalid type: %s" type))))

;; node properties

(defun om-elem-node-property-set-key (key node-property)
  "Set the key of NODE-PROPERTY element to KEY (a string)."
  (om-elem-set-property :key key node-property))

(defun om-elem-node-property-set-value (value node-property)
  "Set the value of NODE-PROPERTY element to VALUE (a string)."
  (om-elem-set-property :value value node-property))

;; link

;; TODO not sure how I feel about this here since it is not a
;; property-based function
(defun om-elem-link-set-description (desc link)
  (om-elem-set-recursive-content (list desc) link))

(defun om-elem-link-set-path (path link)
  "Set the path of LINK element to PATH (a string)."
  (om-elem-set-property :path path link))

(defun om-elem-link-set-type (type link)
  "Set the type of LINK element to TYPE (a symbol).
Setting TYPE to nil will result in a 'fuzzy' type link."
  (let ((valid-types (append (org-link-types)
                             (list "coderef" "custom-id" "file"
                                   "id" "radio" "fuzzy"))))
    (cond
     ((not type)
      (om-elem-set-property :type "fuzzy" link))
     ((member (symbol-name type) valid-types)
      (om-elem-set-property :type (symbol-name type) link))
     (t (error "Unknown type: %s" type)))))

;; timestamp

;; TODO split this into an ending timestamp
;; TODO make unixtime versions of this
(defun om-elem-timestamp-set-time (time timestamp)
  "Set start time of TIMESTAMP element to TIME.
TIME is a list like '(year month day)' or '(year month day hour min)'."
  (-let [(y m d H M) (if (om-elem--has-hour-min time) time
                        `(,@time nil nil))]
    (om-elem-set-properties (list :year-start y
                                  :month-start m
                                  :day-start d
                                  :hour-start H
                                  :minute-start M)
                            timestamp)))

;; TOOD add a switch for precision
(defun om-elem-timestamp-set-time-unixtime (unixtime timestamp)
  "Set start time of TIMESTAMP element to UNIXTIME (an integer).
This assumes one wants HH:MM precision."
  (-> (decode-time unixtime)
      (-slice 1 6)
      (om-elem-timestamp-set-time timestamp)))

(defun om-elem-timestamp-set-time-end (time timestamp)
  "Set end time of TIMESTAMP element to TIME.
TIME is a list like '(year month day)' or '(year month day hour min)'.
This will also change the type to (un)ranged as appropriate."
  (-let* (((y m d H M) (cond ((null time) (-repeat 5 nil))
                             ((om-elem--has-hour-min time) time)
                             (t `(,@time nil nil))))
          (type (--> (om-elem-property :type timestamp)
                     (symbol-name it)
                     (cond
                      ((and (not time) (s-ends-with? "range" it))
                       (s-chop-suffix "-range" it))
                      ((and time (not (s-ends-with? "range" it)))
                       (s-append "-range" it))
                      (t it))
                     (intern it))))
    (om-elem-set-properties (list :type type
                                  :year-end y
                                  :month-end m
                                  :day-end d
                                  :hour-end H
                                  :minute-end M)
                            timestamp)))

(defun om-elem-timestamp-set-time-end-unixtime (unixtime timestamp)
  "Set end time of TIMESTAMP element to UNIXTIME (an integer).
This assumes one wants HH:MM precision."
  (-> (decode-time unixtime)
      (-slice 1 6)
      (om-elem-timestamp-set-time-end timestamp)))

(defun om-elem-timestamp-set-type (type timestamp)
  "Set type of TIMESTAMP element to TYPE.
TYPE can be either 'active' or 'inactive'."
  (let* ((cur-type (om-elem-property :type timestamp))
         (is-ranged? (->> (symbol-name cur-type)
                          (s-ends-with? "-range")))
         (new-type
          (cl-case type
            (active (if is-ranged? 'active-range 'active))
            (inactive (if is-ranged? 'inactive-range 'inactive))
            ;; TODO, I honestly have no idea what a diary timestamp
            ;; is and if this makes any sense...
            (diary diary)
            (t (error "Invalid type %s" type)))))
    (om-elem-set-property :type new-type timestamp)))

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

(defun om-elem-map-property (property fun elem)
  (let ((value (->> (om-elem-property property elem)
                    (funcall fun))))
    (if (stringp elem) (org-add-props elem nil property value)
      (om-elem--elem-list
       (om-elem-type elem)
       (plist-put (nth 1 elem) property value)
       (-drop 2 elem)))))

(defun om-elem-map-properties (plist elem)
  (cond
   ((not plist) elem)
   ((om-elem--is-plist-p plist)
    (->> (om-elem-map-property (nth 0 plist) (nth 1 plist) elem)
         (om-elem-map-properties (-drop 2 plist))))
   (t (error "Not a plist: %s" plist))))

(defmacro om-elem-map-property* (property form elem)
  `(om-elem-map-property ,property (lambda (it) ,form) ,elem))

(defmacro om-elem-map-properties* (plist elem)
  `(let ((new-plist
          (-map-indexed
           (lambda (index item) (if (cl-evenp index) item `(lambda (it) ,item)))
           ,plist)))
     (om-elem-map-properties new-plist ,elem)))

;; headline

(defun om-elem-headline-map-node-property (key fun elem)
  (let ((qs `(section property-drawer (:and node-property
                                               (:key ,key)))))
    (if (om-elem-find-first qs elem)
        (om-elem-map-first*
         qs (om-elem-node-property-map-value fun it) elem)
      elem)))

(defmacro om-elem-headline-map-node-property* (key form elem)
  `(om-elem-headline-map-node-property ,key (lambda (it) ,form) ,elem))

;; node property

(defun om-elem-node-property-map-value (fun elem)
  (let ((val (om-elem-property :value elem)))
    (om-elem-set-property :value (funcall fun val) elem)))

(defmacro om-elem-node-property-map-value* (form elem)
  `(om-elem-node-property-map-value (lambda (it) ,form) ,elem))

;;; element shifters
;; shift a numeric property

;; generic

(defun om-elem-shift-property (prop n elem)
  "Shift PROP of ELEM by N where N is a positive or negative integer."
  (om-elem-map-property* prop (+ n it) elem))

;; headline

(defun om-elem-headline-shift-priority (shift headline)
  "Shift the priority property of HEADLINE element by SHIFT.
SHIFT is a positive or negative integer."
  ;; positive goes up (B -> A) and vice versa
  (cl-flet
      ((fun
        (priority)
        (if (not shift) priority
          (let ((diff (+ 1 (abs (- org-lowest-priority
                                   org-highest-priority))))
                (relp (om--to-relative-priority priority)))
            (--> (- relp shift)
                 (mod it diff)
                 (- it relp)
                 (+ priority it))))))
    (if (not (om-elem-property :priority headline)) headline
      (om-elem-map-property :priority #'fun headline))))

;; timestamp

;; TODO add week to this?
(defmacro om-elem--timestamp-shift-time (unit-alist unit value
                                                    timestamp)
  ;; TODO this will fail if any other functions after it
  ;; rely on the times being correct. If we give "month" as "15"
  ;; it will be interpreted correctly but the ts will be wrong
  `(let ((prop (cl-case ,unit
                 ,@unit-alist
                 (t (error "Invalid unit: %s" ,unit))))
         ;; TODO make mapper only work on non-nil values?
         (fun (lambda (it) (and it (+ ,value it)))))
     (om-elem-map-property prop fun ,timestamp)))

(defun om-elem--timestamp-shift-time-start (unit value timestamp)
  (om-elem--timestamp-shift-time
   ((minute :minute-start)
    (hour :hour-start)
    (day :day-start)
    (month :month-start)
    (year :year-start))
   unit value timestamp))

(defun om-elem--timestamp-shift-time-end (unit value timestamp)
  (om-elem--timestamp-shift-time
   ((minute :minute-end)
    (hour :hour-end)
    (day :day-end)
    (month :month-end)
    (year :year-end))
   unit value timestamp))

(defun om-elem-timestamp-shift-time-start (unit value timestamp)
  "Shift the UNIT of TIMESTAMP element start time by VALUE.
VALUE is a positive or negative integer and UNIT is one of 'minute',
'hour', 'day', 'month', or 'year'. Value will wrap around larger units
as needed; for instance, supplying 'minute' for UNIT and 60 for VALUE
will increase the hour property by 1."
  (let ((type (om-elem-property :type timestamp)))
    (if (memq type '(active inactive))
        (om-elem-timestamp-shift-time unit value timestamp)
      (om-elem--timestamp-shift-time-start unit value timestamp))))

(defun om-elem-timestamp-shift-time-end (unit value timestamp)
  "Shift the UNIT of TIMESTAMP element end time by VALUE.
The behavior is analogous to `om-elem-timestamp-shift-time-start',
except that the timestamp will be unchanged if no ending time is
present."
  (let ((type (om-elem-property :type timestamp)))
    (if (memq type '(active inactive)) timestamp
      (om-elem--timestamp-shift-time-end unit value timestamp))))

(defun om-elem-timestamp-shift-time (unit value timestamp)
  "Shift the UNIT of TIMESTAMP element start and end time by VALUE.
The behavior is analogous to `om-elem-timestamp-shift-time-start' for
both timestamp halves."
  (->> (om-elem--timestamp-shift-time-start unit value timestamp)
       (om-elem--timestamp-shift-time-end unit value)))

;;; element toggles
;; flip a boolean (ish) property

;; generic

(defun om-elem-toggle-property (prop elem)
  "Toggle the state of PROP in ELEM."
  (om-elem-map-property prop #'not elem))

;; headline

(defun om-elem-headline-toggle-commented (headline)
  "Toggle the commented/uncommented state of HEADLINE element."
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
  (let ((cur-type (om-elem-property :type timestamp)))
    (cond
     ((memq cur-type '(inactive inactive-range))
      (om-elem-timestamp-set-type 'active timestamp))
     ((memq cur-type '(active active-range))
      (om-elem-timestamp-set-type 'inactive timestamp))
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
  ;; TODO validate plist
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
;; (defun om-elem-headline-shift-level (level elem)
;;   (let ((level* (+ level (om-elem-property :level elem))))
;;     (if (< 1 level*) (om-elem-set-property :level level* elem)
;;       (om-elem-set-property :level 1 elem))))

;; TODO make drawer, prop-drawer, planning, and section insertion

;; plain-list

;; ;; TODO generalize this to multiple indents?
;; (defun om-elem-list-indent-item (index elem)
;;   (unless (< 0 index) (error "Index must be greater than 0"))
;;   (unless (> (length (om-elem-contents elem)) index)
;;     (error "Index higher than last item."))
;;   (let* ((prev-index (- index 1))
;;          (cell (om-elem-extract-first elem index))
;;          (extracted-item (car cell))
;;          (new-list (om-elem-build-plain-list extracted-item))
;;          (new-elem (cdr cell))
;;          (insert-index
;;           (if (om-elem-find-first extracted-item 'paragraph) 1 0)))
;;     (om-elem-insert-within new-elem new-list insert-index prev-index)))

(defun om-elem--indent-subsequent (index elem)
  (if (< index (- (length (om-elem-contents elem)) 1))
      (->> (om-elem-list-indent-item (+ 1 index) elem)
           (om-elem--indent-subsequent index))
    elem))

;; (defun om-elem-list-unindent-item (outer-index inner-index elem)
;;   ;; TODO validate indices
;;   ;; (unless (<= 0 outer-index) (error "Index must be positive"))
;;   ;; (unless (> (length (om-elem-contents elem)) index)
;;   ;; (error "Index higher than last item."))
;;   ;; indent all items after the target item and then unindent the
;;   ;; target item
;;   (let* ((cell
;;           (->> elem
;;                (om-elem-map-first* `(,outer-index plain-list)
;;                                    (om-elem--indent-subsequent inner-index it))
;;                (om-elem-extract-first `(,outer-index plain-list ,inner-index))))
;;          (new-item (car cell))
;;          (new-elem (om-elem-clean (cdr cell))))
;;     (om-elem-insert-within-element new-elem new-item (+ outer-index 1))))

;; table

;; ;; TODO use the pad function somewhere here to save lines of code
;; (defun om-elem-table-insert-table-row (row index elem)
;;   (let ((new-row
;;          (if (eq row 'hline)
;;              (om-elem-build-table-hline)
;;            (let* ((width (->>
;;                           (om-elem-find-first
;;                            elem :any
;;                            '(:and table-row (:type standard)))
;;                           (om-elem-contents)
;;                           (length)))
;;                   (row-len (length row)))
;;              (->>
;;               (cond
;;                ((> row-len width)
;;                 (error "Row length exceeds table width."))
;;                ((< row-len width)
;;                 (append row (- row-len width) (repeat "")))
;;                (t row))
;;               (--map (om-elem-build-table-cell it))
;;               (apply #'om-elem-build-table-row))))))
;;     (om-elem-insert-within-element elem new-row index)))

;; (defun om-elem-table-delete-table-row (index elem)
;;   (om-elem-delete-first elem index))

;; (defun om-elem-table-delete-table-column (index table)
;;   ;; TODO what if index out of range
;;   (om-elem-map* '(:and table-row (:type standard))
;;                 (om-elem-delete-first it index) table))

;; ;; TODO this is best done with some sort of zip function
;; (defun om-elem-table-insert-table-column (column index elem)
;;   (cl-labels
;;       ((insert-cells
;;         (col elem)
;;         (-if-let (cur-cell (car col))
;;             (let ((cur-row-index (- (length col) 1))
;;                   (new-cell (om-elem-build-table-cell cur-cell)))
;;               ;; TODO this currently does not skip hlines
;;               ;; need to modify the map function
;;               (->>
;;                (om-elem-map-first*
;;                 elem (om-elem-insert-within-element it new-cell index)
;;                 `(:and table-row (:type standard) ,cur-row-index))
;;                (insert-cells (cdr col))))
;;           elem)))
;;     (let* ((height (->> (om-elem-find
;;                          elem '(:and table-row (:type standard)))
;;                         (length)))
;;            (column (nreverse (om-elem--pad-list column height ""))))
;;       (insert-cells column elem))))

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
  (save-excursion
    (goto-char point)
    (insert (om-elem-to-string elem)))
  elem)

(defun om-elem-insert-tail (point elem)
  "Like `om-elem-insert' but insert ELEM at POINT and move to the end of inserted string."
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

(provide 'om-elem)
;;; om-elem.el ends here
