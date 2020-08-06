;;; org-ml-macs.el --- Macros for org-ml -*- lexical-binding: t; -*-

;; Author: Nathan Dwarshuis <ndwar@yavin4.ch>

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

;; This file contains macros essential for the `org-ml' package. The
;; following functionality is implemented:
;; - automatic anaphoric form generation (`org-ml--defun*'): this macro will
;;   define an anaphoric form along with a regular function definition
;; - defun with &rest + &keys support (`org-ml--defun-kw'): this macro allows
;;   writing function definitions that accept &key and &rest arguments at the
;;   same time, which `cl-defun' does not (and likely will never) support

;;; Code:

(require 'dash)
(require 's)

;;; ANAPHORIC FUNCTIONS

(defun org-ml--defun-partition-body (body)
  "Return ARGS as a list like (DOCSTRING DECLS BODY).
DOCSTRING is the first string in BODY if present and it succeeded by
more forms. DECLS is a list of declarations in the DECLARE statement
if present after the docstring. Everything else is BODY."
  ;; macroexp-parse-body doesn't seem to retain declare
  (cl-flet
      ((is-declare
        (form)
        (eq 'declare (car form))))
    (let ((first (car body))
          (second (cadr body))
          (rest (cddr body)))
      (cond
       ((and (stringp first) (is-declare second))
        (list first (cdr second) rest))
       ((and (stringp first) second)
        (list first nil (cons second rest)))
       ((and (is-declare first) second)
        (list nil (cdr first) (cons second rest)))
       (t
        (list nil nil body))))))

(defun org-ml--defun-make-indent-declare (decl pos)
  "Return declare form with indent set to POS if not present already.
DECL is a list of declarations."
  (let ((indent (or (assoc 'indent decl) `(indent ,pos)))
        (decl (--remove (eq 'indent (car it)) decl)))
    `(declare ,@decl ,indent)))

(defun org-ml--defun-make-anaphoric-docstring (name docstring)
  "Return DOCSTRING adapted for anaphoric version of definition NAME.
This includes adding a short string to the front indicating it is an
anaphoric version and replacing all instances of \"FUN\" with \"FORM\"."
  (->> (s-replace "FUN" "FORM" docstring)
       (format "Anaphoric form of `%s'.\n\n%s" name)))

(defmacro org-ml--defun* (name arglist &rest args)
  "Return a function definition for NAME, ARGLIST, and ARGS.
This will also make a mirrored anaphoric form macro definition. This
assumes that `fun' represents a unary function which will be used
somewhere in the definition's body. When making the anaphoric form,
`fun' will be replaced by the symbol `form', and `form' will be
wrapped in a lambda call binding the unary argument to the symbol
`it'."
  (declare (doc-string 3) (indent 2))
  (-let* (((docstring decls body) (org-ml--defun-partition-body args))
          (name* (intern (format "%s*" name)))
          (arglist* (-replace 'fun 'form arglist))
          (docstring* (org-ml--defun-make-anaphoric-docstring name docstring))
          (funargs (--map (if (eq it 'fun) '(lambda (it) (\, form))
                            (cons '\, (list it)))
                          arglist))
          (body* (cdr (backquote-process (backquote (,name ,@funargs)))))
          (debug* (->> arglist
                       (--map (if (eq it 'fun) 'def-form 'form))
                       (list 'debug)))
          (dec (org-ml--defun-make-indent-declare
                decls (-elem-index 'fun arglist)))
          (dec* (org-ml--defun-make-indent-declare
                 (cons debug* decls) (-elem-index 'fun arglist))))
    `(progn
       (defmacro ,name* ,arglist*
         ,docstring*
         ,dec*
         ,body*)
       (defun ,name ,arglist
         ,docstring
         ,dec
         ,@body))))

;;; BETTER CL-DEFUN

;; Some functions here require a clean way to use &rest and &key at the same
;; time, which `cl-defun' does not do. For a given external function signature
;; like (P1 ... &key K1 ... &rest R), this framework will make a function with
;; the internal signature (P1 ... &rest --rest-args) where PX are positional
;; arguments matching exactly those in the external signature and --rest-args
;; will bind the list contain the key-val pairs and rest arguments. This will be
;; partitioned into keyword arguments like KX VAL rest arguments R internally.

(defun org-ml--symbol-to-keyword (symbol)
  "Convert SYMBOL to keyword if not already."
  (if (keywordp symbol) symbol
    (->> (symbol-name symbol)
         (s-prepend ":")
         (intern))))

(defun org-ml--process-pos-args (pos-args)
  "Process POS-ARGS and return if valid."
  (if (--all? (or (symbolp it) (consp it)) pos-args) pos-args
    (error "Positional args must be either cons cells or symbols")))

(defun org-ml--process-rest-arg (rest-arg)
  "Process REST-ARG and return if valid."
  (pcase rest-arg
    (`(,(and (pred symbolp) sym) . nil) sym)
    (`nil nil)
    (_ (error "Rest argument must only have one symbol"))))

(defun org-ml--make-kwarg-let (kws-sym kwarg)
  "Return cell for KWARG like (KW . LET-FORM).
KWARG is a keyword argument in the signature of a function definition
\(see `org-ml--defun-kw' for valid configurations of this). In the returned
cell, KW is keyword representing the key to be used in a function
call, and LET-FORM is a form to be used in a let binding that will
retrieve the value for KW from a plist bound to KWS-SYM (which is
a non-interned symbol to be bound to the keywords in a function
call)."
  (cl-flet
      ((make-plist
        (arg init)
        (let* ((kw (org-ml--symbol-to-keyword arg))
               (kw-get `(cadr (plist-member ,kws-sym ',kw)))
               (val (if init `(or ,kw-get ,init) kw-get)))
          (cons kw `(,arg ,val)))))
    (pcase kwarg
      (`(,arg ,init)
       (make-plist arg init))
      ((and (pred symbolp) arg)
       (make-plist arg nil))
      (_ (error "Invalid keyword argument: %s" kwarg)))))

(defmacro org-ml--throw-kw-error (msg kws)
  "Throw an error with MSG with formatted list of KWS."
  `(when ,kws
     (->> (-map #'symbol-name ,kws)
          (s-join ", ")
          (error (concat ,msg ": %s")))))

(defmacro org-ml--partition-rest-args (args)
  "Partition ARGS into two keyword and rest argument lists.
The keyword list is determined by partitioning all keyword-value
pairs until this pattern is broken. Whatever is left is put into the
rest list. Return a list like (KEYARGS RESTARGS)."
  `(->> (-partition-all 2 ,args)
        (--split-with (keywordp (car it)))))

(defmacro org-ml--make-rest-partition-form (argsym kws use-rest?)
  "Return a form that will partition the args in ARGSYM.
ARGSYM is a symbol which is bound to the rest argument list of a
function call. KWS is a list of valid keywords to use when deciding
which in the argument values is a keyword-value pair, and USE-REST?
is a boolean that determines if rest arguments are to be considered."
  ;; these `make-symbol' calls probably aren't necessary but they
  ;; ensure the let bindings are leak-proof
  (let* ((p (make-symbol "--part"))
         (k (make-symbol "--kpart"))
         (y (make-symbol "--keys"))
         (r (make-symbol "--rpart"))
         (inv-msg "Invalid keyword(s) found")
         (dup-msg "Keyword(s) used multiple times")
         (rest-msg
          (s-join " " '("Keyword-value pairs must be immediately"
                        "after positional arguments. These keywords"
                        "were interpreted as rest arguments")))
         (tests
          ;; ensure that all keywords are valid
          `((->> (-difference ,y ',kws)
                 (org-ml--throw-kw-error ,inv-msg))
            ;; ensure keywords are only used once per call
            (->> (-group-by #'identity ,y)
                 (--filter (< 2 (length it)))
                 (org-ml--throw-kw-error ,dup-msg))
            ;; ensure that keyword pairs are only used
            ;; immediately after positional arguments
            (->> (-filter #'keywordp ,r)
                 (org-ml--throw-kw-error ,rest-msg))))
         ;; if rest arguments are used but not allowed in function
         ;; call, throw error
         (tests (if use-rest? tests
                  (-snoc
                   tests
                   `(when ,r
                      (error "Too many arguments supplied")))))
         ;; return a cons cell of (KEY REST) argument values or
         ;; just KEY if rest is not used in the function call
         (return (if (not use-rest?) `(apply #'append ,k)
                   `(cons (apply #'append ,k)
                          (apply #'append ,r)))))
    `(let* ((,p (org-ml--partition-rest-args ,argsym))
            (,k (car ,p))
            (,y (-map #'car ,k))
            (,r (cadr ,p)))
       ,@tests
       ,return)))

(defun org-ml--make-usage-args (arglist)
  "Return ARGLIST as it should appear in the usage signature.
This will uppercase all symbol names and remove all type keys."
  (cl-flet*
      ((ucase-sym
        (sym)
        (-> sym (symbol-name) (upcase) (make-symbol)))
       (unwrap-form-maybe
        (arg)
        (ucase-sym (if (consp arg) (cadr arg) arg)))
       (unwrap-kw-form-maybe
        (arg)
        (pcase arg
          ;; ((PRED KEY) INITFORM)
          (`((,(and (pred keywordp) _) ,arg) ,init)
           (list (ucase-sym arg) init))
          ;; ((PRED KEY))
          (`((,(and (pred keywordp) _) ,arg))
           (ucase-sym arg))
          ;; (KEY INITFORM)
          (`(,arg ,init)
           (list (ucase-sym arg) init))
          ;; KEY
          ((and (pred symbolp) arg)
           (ucase-sym arg))
          (_ (error "This shouldn't happen")))))
    (let* ((part(-partition-before-pred
                 (lambda (it) (memq it '(&pos &rest &key)))
                 (cons '&pos arglist)))
           (pos (-some->> (alist-get '&pos part)
                          (-map #'unwrap-form-maybe)))
           (kw (-some->> (alist-get '&key part)
                         (-map #'unwrap-kw-form-maybe)
                         (cons '&key)))
           (rest (-some->> (alist-get '&rest part)
                           (-map #'unwrap-form-maybe)
                           (cons '&rest))))
      (append pos kw rest))))

(defun org-ml--make-header (body arglist)
  "Return a header using docstring from BODY and ARGLIST."
  (let ((header (caar (macroexp-parse-body body))))
    ;; Macro expansion can take place in the middle of
    ;; apparently harmless computation, so it should not
    ;; touch the match-data.
    (save-match-data
      (let ((print-gensym nil)
            (print-quoted t)
            (print-escape-newlines t))
        (->> (org-ml--make-usage-args arglist)
             (cons 'fn)
             (format "%S")
             (help--docstring-quote)
             (help-add-fundoc-usage header))))))

(defun org-ml--transform-lambda (arglist body name)
  "Make a form for a keyword/rest composite function definition.
ARGLIST is the argument signature. BODY is the function body. NAME
is the NAME of the function definition.

This acts much like `cl-defun' except that it only considers &rest
and &key slots. The way the final function call will work beneath the
surface is that all positional arguments will be bound to their
symbols in ARGLIST (analogous to `defun' and `cl-defun'), and the key
and rest arguments will be captured in one rest argument to be
partitioned on the fly into key and rest bindings that can be used
in BODY."
  ;; assume &key will always be present if this function is called
  (let* ((a (make-symbol "--arg-cell"))
         (k (make-symbol "--kw-args"))
         (kr (make-symbol "--key-and-rest-args"))
         (partargs (-partition-before-pred
                    (lambda (it) (memq it '(&pos &rest &key)))
                    (cons '&pos arglist)))
         (pos-args (->> (alist-get '&pos partargs)
                        (org-ml--process-pos-args)))
         (kw-alist (->> (alist-get '&key partargs)
                        (--map (org-ml--make-kwarg-let k it))))
         (rest-arg (->> (alist-get '&rest partargs)
                        (org-ml--process-rest-arg)))
         (kws (-map #'car kw-alist))
         (kw-lets (-map #'cdr kw-alist))
         (arg-form `(,@pos-args &rest ,kr))
         (header (org-ml--make-header body arglist))
         (let-forms
          (if rest-arg
              `((,a (org-ml--make-rest-partition-form ,kr ,kws t))
                (,k (car ,a))
                (,rest-arg (cdr ,a))
                ,@kw-lets)
            `((,k (org-ml--make-rest-partition-form ,kr ,kws nil))
              ,@kw-lets)))
         (body (->> (macroexp-parse-body body)
                    (cdr)
                    (append `(cl-block ,name)))))
    ;; if &key is used but no keywords are actually used, slap the
    ;; programmer in the face
    (unless kw-alist (error "No keywords used"))
    `(,arg-form
      ,header
      ,(macroexp-let*
        let-forms
        (macroexp-progn `(,body))))))

(def-edebug-spec org-ml--defun-key
  ([&or arg (arg sexp)]))

(def-edebug-spec org-ml--defun-lambda-kw-list
  (([&rest arg]
    [&optional ["&key" org-ml--defun-key &rest org-ml--defun-key]]
    &optional ["&rest" arg])))

(defmacro org-ml--defun-kw (name arglist &rest body)
  "Define NAME as a function with BODY.

This is like `cl-defun' except it allows &key to be used in
conjunction with &rest without freaking out. ARGLIST can be specified
using the following syntax:

\([VAR] ...
 [&key (VAR [INITFORM])...]
 [&rest VAR])

where VAR is a symbol for the variable identifier and INITFORM is an
atom or form that will be the default value for keyword VAR if it is
not give in a function call.

When calling functions defined with this, keywords can be given in any
order as long as they are after all positional arguments, and rest
arguments will be interpreted as anything not belonging to a key-val
pair (but only if &rest was used to define the function). This implies
that keywords may not be used as values for the rest argument in
function calls."
  (declare (doc-string 3) (indent 2)
           (debug (&define name
                           org-ml--defun-lambda-kw-list
                           lambda-doc
                           [&optional ("declare" &rest sexp)]
                           def-body)))
  (if (memq '&key arglist)
      (let ((res (org-ml--transform-lambda arglist body name)))
        `(defun ,name ,@res))
    (error "&key not used, use regular defun")))

;; COMPILER MACROS

(defmacro org-ml--defconst (symbol form &optional docstring)
  "Like `defconst' but wrapped in `eval-and-compile'.
SYMBOL and DOCSTRING have the same meaning as `defconst'.
FORM is used to set the init value and is wrapped in
`eval-when-compile.'"
  (declare (indent 1))
  `(eval-and-compile (defconst ,symbol (eval-when-compile ,form) ,docstring)))

(defmacro org-ml--defvaralias (new-alias base-variable &optional docstring)
  "Like `defvaralias' but wrapped in `eval-and-compile'.
NEW-ALIAS, BASE-VARIABLE, and DOCSTRING have the same meaning as `defconst'."
  (declare (indent 1))
  `(eval-and-compile (defvaralias ,new-alias ,base-variable ,docstring)))

;; FUNCTORS

(defmacro org-ml--map-first* (form list)
  "Return LIST with FORM applied to the first member.
The first element is `it' in FORM which returns the modified member."
  `(when ,list
    (cons (funcall (lambda (it) ,form) (car ,list)) (cdr ,list))))

  (defmacro org-ml--map-last* (form list)
    "Return LIST with FORM applied to the last member.
The last element is `it' in FORM which returns the modified member."
    `(-some->> ,list (nreverse) (org-ml--map-first* ,form) (nreverse)))

(defmacro org-ml--map-at* (n form list)
  "Return LIST with FORM applied to the member at index N.
The nth element is `it' in FORM which returns the modified member."
  (declare (indent 1))
  `(--> (nth ,n ,list) (funcall (lambda (it) ,form) it) (-replace-at ,n it ,list)))

(provide 'org-ml-macs)
;;; org-ml-macs.el ends here
