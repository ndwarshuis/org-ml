;;; om-dev-test.el --- Tests for om

;;; Commentary:

;;; Code:

(require 'dash)
(require 'om-dev-examples-to-tests)
(require 'om-dev-examples)

;; hack to make edebug work with undercover
(put '->> 'edebug-form-spec
     '(form &rest symbolp (sexp &rest def-form)))

(defun should-have-equal-properties (e1 e2)
  (unless (eq (om--get-type e1) (om--get-type e2))
    (error "Type mismatch: %s\n\n%s" e1 e2))
  (cl-flet ((plist-get-keys (plist) (-slice plist 0 nil 2)))
    (let ((p1 (plist-get-keys (nth 1 e1)))
          (p2 (plist-get-keys (nth 1 e2))))
      ;; (print e1)
      ;; (print e2)
      ;; (print p1)
      ;; (print p2)
      (should-not (or (-difference p1 p2) (-difference p2 p1))))))


;; objects

(defun om--compare-object-props (elem string)
  (should-have-equal-properties
   elem
   (->> (om--from-string (concat " " string))
        (om--get-descendent '(0 1)))))

(ert-deftest om--code/valid-props ()
  (om--compare-object-props
   (om-build-code "value") "~code~"))

(ert-deftest om--entity/valid-props ()
  (om--compare-object-props
   (om-build-entity "pi") "\\pi"))

(ert-deftest om--export-snippet/valid-props ()
  (om--compare-object-props
   (om-build-export-snippet "backend" "value") "@@im:padme@@"))

(ert-deftest om--inline-babel-call/valid-props ()
  (om--compare-object-props
   (om-build-inline-babel-call "name") "call_name()"))

(ert-deftest om--inline-src-block/valid-props ()
  (om--compare-object-props
   (om-build-inline-src-block "lang") "src_lang{value}"))

;; TODO add latex fragment

(ert-deftest om--line-break/valid-props ()
  (om--compare-object-props
   (om-build-line-break) "\\\\\n"))

(ert-deftest om--macro/valid-props ()
  (om--compare-object-props
   (om-build-macro "value") "{{{value}}}"))

(ert-deftest om--statistics-cookie/valid-props ()
  (om--compare-object-props
   (om-build-statistics-cookie '(1)) "[/]"))

(ert-deftest om--target/valid-props ()
  (om--compare-object-props
   (om-build-target "value") "<<value>>"))

(ert-deftest om--timestamp/valid-props ()
  (om--compare-object-props
   (om-build-timestamp! '(2019 1 1))
   ;; TODO the timestamp parser does not add properties for warnings
   ;; or repeaters if they are not given, this appears to be a bug
   "[2019-01-01 Tue +1d -1d]"))

(ert-deftest om--verbatim/valid-props ()
  (om--compare-object-props
   (om-build-verbatim "value") "=value="))

;; recursive objects

(ert-deftest om--bold/valid-props ()
  (om--compare-object-props
   (om-build-bold) "*bold*"))

(ert-deftest om--footnote-reference/valid-props ()
  (om--compare-object-props
   (om-build-footnote-reference) "[fn:1]"))

(ert-deftest om--italic/valid-props ()
  (om--compare-object-props
   (om-build-italic) "/italic/"))

(ert-deftest om--link/valid-props ()
  (om--compare-object-props
   (om-build-link "path") "[[path]]"))

(ert-deftest om--radio-target/valid-props ()
  (om--compare-object-props
   (om-build-radio-target) "<<<target>>>"))

(ert-deftest om--strike-through/valid-props ()
  (om--compare-object-props
   (om-build-strike-through) "+bad+"))

(ert-deftest om--superscript/valid-props ()
  (should-have-equal-properties
   (om-build-superscript)
   (->> (om--from-string "thisis^super")
        (om--get-descendent '(0 1)))))

(ert-deftest om--subscript/valid-props ()
  (should-have-equal-properties
   (om-build-subscript)
   (->> (om--from-string "thisis_subpar")
        (om--get-descendent '(0 1)))))

(ert-deftest om--table-cell/valid-props ()
  (should-have-equal-properties
   (om-build-table-cell "cell")
   (->> (om--from-string "| cell |")
        (om--get-descendent '(0 0 0)))))

(ert-deftest om--underline/valid-props ()
  (om--compare-object-props
   (om-build-underline) "_bad_"))

;; elements

(defun om--compare-element-props (elem string)
  (should-have-equal-properties
   elem
   (->> (om--from-string string)
        (om--get-descendent '(0)))))

(ert-deftest om--babel-call/valid-props ()
  (om--compare-element-props
   (om-build-babel-call "call") "#+CALL: name()"))

(ert-deftest om--clock/valid-props ()
  (om--compare-element-props
   (om-build-clock (om-build-timestamp! '(2019 1 1)))
   "CLOCK: [2019-01-01 Tue]"))

(ert-deftest om--comment/valid-props ()
  (om--compare-element-props
   (om-build-comment "useless") "# useless"))

(ert-deftest om--comment-block/valid-props ()
  (om--compare-element-props
   (om-build-comment-block)
   "#+BEGIN_COMMENT\nuseless\n#+END_COMMENT"))

(ert-deftest om--diary-sexp/valid-props ()
  (om--compare-element-props
   (om-build-diary-sexp) "%%()"))

(ert-deftest om--example-block/valid-props ()
  (om--compare-element-props
   (om-build-example-block)
   "#+BEGIN_EXAMPLE\nuseless\n#+END_EXAMPLE"))

(ert-deftest om--export-block/valid-props ()
  (om--compare-element-props
   (om-build-export-block "type" "value")
   "#+BEGIN_EXPORT type\nuseless\n#+END_EXPORT"))

(ert-deftest om--fixed-width/valid-props ()
  (om--compare-element-props
   (om-build-fixed-width "value") ": value"))

(ert-deftest om--horizontal-rule/valid-props ()
  (om--compare-element-props
   (om-build-horizontal-rule) "-----"))

(ert-deftest om--keyword/valid-props ()
  (om--compare-element-props
   (om-build-keyword "key" "val") "#+KEY: val"))

(ert-deftest om--latex-environment/valid-props ()
  (om--compare-element-props
   (om-build-latex-environment '("gloves" "text"))
   "\\begin{env}\nvalue\n\\end{env}"))

(ert-deftest om--node-property/valid-props ()
  (should-have-equal-properties
   (om-build-node-property "key" "value")
   (->> (om--from-string "* dummy\n:PROPERTIES:\n:key: val\n:END:")
        (om--get-descendent '(0 0 0)))))

(ert-deftest om--planning/valid-props ()
  (should-have-equal-properties
   (om-build-planning :closed (om-build-timestamp! '(2019 1 1)))
   (->> (om--from-string "* dummy\nCLOSED: [2019-01-01 Tue]")
        (om--get-descendent '(0 0)))))

(ert-deftest om--src-block/valid-props ()
  (om--compare-element-props
   (om-build-src-block)
   "#+BEGIN_SRC\nuseless\n#+END_SRC"))

;; containers

(ert-deftest om--paragraph/valid-props ()
  (should-have-equal-properties
   (om-build-paragraph)
   (->> (om--from-string "text")
        (om--get-descendent '(0)))))

(ert-deftest om--table-row/valid-props ()
  (should-have-equal-properties
   (om-build-table-row)
   (->> (om--from-string "| row |")
        (om--get-descendent '(0 0)))))

(ert-deftest om--verse-block/valid-props ()
  (should-have-equal-properties
   (om-build-verse-block)
   (->> (om--from-string "#+BEGIN_VERSE\nthing\n#+END_VERSE")
        (om--get-descendent '(0)))))

(ert-deftest om--center-block/valid-props ()
  (om--compare-element-props
   (om-build-center-block)
   "#+BEGIN_CENTER\nuseless\n#+END_CENTER"))

(ert-deftest om--drawer/valid-props ()
  (om--compare-element-props
   (om-build-drawer "name")
   ":LOGBOOK:\nuseless\n:END:"))

(ert-deftest om--dynamic-block/valid-props ()
  (om--compare-element-props
   (om-build-dynamic-block "name" :arguments '(:key val))
   "#+BEGIN: name args\nuseless\n#+END:"))

(ert-deftest om--footnote-definition/valid-props ()
  (om--compare-element-props
   (om-build-footnote-definition "label") "[fn:label]\n"))

(ert-deftest om--headline/valid-props ()
  (should-have-equal-properties
   (om-build-headline)
   (om--from-string "* head")))

(ert-deftest om--item/valid-props ()
  (should-have-equal-properties
   (om-build-item)
   (->> (om--from-string "- head")
        (om--get-descendent '(0 0)))))

(ert-deftest om--plain-list/valid-props ()
  (om--compare-element-props
   (om-build-plain-list) "- item"))

(ert-deftest om--property-drawer/valid-props ()
  (should-have-equal-properties
   (om-build-property-drawer)
   (->> (om--from-string "* dummy\n:PROPERTIES:\n:END:")
        (om--get-descendent '(0 0)))))

(ert-deftest om--quote-block/valid-props ()
  (om--compare-element-props
   (om-build-quote-block)
   "#+BEGIN_QUOTE\n#+END_QUOTE"))

(ert-deftest om--section/valid-props ()
  (om--compare-element-props
   (om-build-section) "* dummy\nstuff"))

(ert-deftest om--special-block/valid-props ()
  (om--compare-element-props
   (om-build-special-block "type")
   "#+BEGIN_type:\n#+END_type:"))

(ert-deftest om--table/valid-props ()
  (om--compare-element-props
   (om-build-table) "| table |"))

(defun should-equal (string elem)
  (should (equal string (->> (om-to-string elem) (s-trim)))))

(defun should-match (regexp elem)
  (should (s-match regexp (->> (om-to-string elem) (s-trim)))))

(ert-deftest om--make-header ()
  (should (equal (om--make-header '("docstring" (print 'hi)) nil)
                 "docstring\n\n(fn)"))
  (should (equal (om--make-header '("docstring" (print 'hi)) '(one))
                 "docstring\n\n(fn ONE)"))
  (should (equal (om--make-header '("docstring" (print 'hi)) '(one two))
                 "docstring\n\n(fn ONE TWO)")))

;; (ert-deftest om--verify-pos-args/valid ()
;;   (should (equal '(one two) (om--verify-pos-args '(one two))))
;;   (should (equal nil (om--verify-pos-args nil))))

;; (ert-deftest om--verify-pos-args/error ()
;;   (should-error (om--verify-pos-args '(1)))
;;   (should-error (om--verify-pos-args '("one")))
;;   (should-error (om--verify-pos-args '((one)))))

;; (ert-deftest om--verify-rest-arg/valid ()
;;   (should (equal 'one (om--verify-rest-arg '(one))))
;;   (should (equal nil (om--verify-rest-arg nil))))

;; (ert-deftest om--verify-rest-arg/error ()
;;   ;; too long
;;   (should-error (om--verify-rest-arg '(one two)))
;;   ;; non-symbol
;;   (should-error (om--verify-rest-arg '(1)))
;;   (should-error (om--verify-rest-arg '("one")))
;;   (should-error (om--verify-rest-arg '((one)))))

;; (ert-deftest om--make-optarg-let/valid ()
;;   (should (equal (om--make-optarg-let 'one 0)
;;                  '(one (nth 0 --opt-args))))
;;   (should (equal (om--make-optarg-let '(one 1) 0)
;;                  '(one (or (nth 0 --opt-args) 1)))))

;; (ert-deftest om--make-optarg-let/error ()
;;   ;; arg should be a symbol
;;   (should-error (om--make-optarg-let 1 0))
;;   (should-error (om--make-optarg-let "one" 0))
;;   (should-error (om--make-optarg-let '(one) 0))
;;   ;; TODO this doesn't work yet
;;   ;; (should-error (om--make-optarg-let :one 0))
;;   ;; wrong list length
;;   (should-error (om--make-optarg-let '(one one 1) 0)))

;; (ert-deftest om--make-kwarg-let/valid ()
;;   (should
;;    (equal (om--make-kwarg-let 'k 'one)
;;           '(:one one (cadr (plist-member k (quote :one))))))
;;   ;; (should
;;   ;;  (equal (om--make-kwarg-let 'k '((:two one)))
;;   ;;         '(:two one (cadr (plist-member k (quote :two))))))
;;   (should
;;    (equal (om--make-kwarg-let 'k '(one 1))
;;           '(:one one (or (cadr (plist-member k (quote :one))) 1)))))
;;   ;; (should
;;   ;;  (equal (om--make-kwarg-let 'k '((:two one) 1))
;;   ;;         '(:two one (or (cadr (plist-member k (quote :two))) 1)))))

(ert-deftest om--make-kwarg-let/error ()
  ;; list too long
  (should-error (om--make-kwarg-let '(one two three)))
  ;; keyword slot must be a real keyword
  (should-error (om--make-kwarg-let '((one two))))
  (should-error (om--make-kwarg-let '((one two) three)))
  ;; single arg must be a symbol but not a keyword
  ;; TODO the keyword guard does not work yet
  ;; (should-error (om--make-kwarg-let :one))
  (should-error (om--make-kwarg-let 1))
  (should-error (om--make-kwarg-let "one"))
  (should-error (om--make-kwarg-let '(1))))

;; (ert-deftest om--make-rest-partition-form/kwargs ()
;;   (should (equal '((:one one) nil) (om--make-rest-partition-form '(:one one) '(:one) nil)))
;;   (should (equal '((:one one) nil) (om--make-rest-partition-form '(:one one) '(:one) t))))

;; (ert-deftest om--make-rest-partition-form/restargs ()
;;   ;; (should (equal '(nil (one)) (om--make-rest-partition-form '(one) nil nil)))
;;   (should (equal '(nil . (one)) (om--make-rest-partition-form '(one) nil t)))
;;   (should (equal '(nil . (one two)) (om--make-rest-partition-form '(one two) nil t))))

;; (ert-deftest om--make-rest-partition-form/combo ()
;;   (should (equal '((:one one) (two)) (om--make-rest-partition-form '(:one one two) '(:one) t))))

;; (ert-deftest om--make-rest-partition-form/error ()
;;   ;; invalid keywords
;;   (should-error (om--make-rest-partition-form '(:one one) '(:two) nil))
;;   ;; too many arguments
;;   (should-error (om--make-rest-partition-form '(:one one two) (:one) nil))
;;   ;; multiple keywords
;;   (should-error (om--make-rest-partition-form '(:one one :one three two) (:one) nil)))

;;; MATCH FRAMEWORK TESTING

;; These are tests for `om-match' and friends. Proceed with caution :)

(defmacro should-error-arg (form)
  "Make an ert error form to test if FORM signals an `arg-type-error'."
  `(should-error ,form :type 'arg-type-error))

(ert-deftest om--match-make-condition-form/error ()
  ;; Ensure `om--match-make-condition-form' will error when it
  ;; supposed to do so. All errors (in theory) should be tested here
  ;; so that we don't need to bother testing them anywhere else when
  ;; we test functions higher in the framework
  (unless (fboundp 'om--match-make-condition-form)
    (error "Function not defined"))
  (let ((fun #'om--match-make-condition-form))
    ;; quoted
    (should-error-arg (funcall fun '(quote bold)))
    (should-error-arg (funcall fun '(function bold)))
    ;; invalid type
    (should-error-arg (funcall fun 'protoss))
    ;; invalid operator
    (should-error-arg (funcall fun '(= 1)))
    (should-error-arg (funcall fun '(=/ 1)))
    ;; valid operator with non-integer
    (should-error-arg (funcall fun '(< "1")))
    ;; valid operator with too many arguments
    (should-error-arg (funcall fun '(< 1 2)))
    ;; pred with no arguments
    (should-error-arg (funcall fun '(:pred)))
    ;; pred with too many arguments
    (should-error-arg (funcall fun '(:pred stringp integerp)))
    ;; not with no arguments
    (should-error-arg (funcall fun '(:not)))
    ;; not with too many arguments
    (should-error-arg (funcall fun '(:not 1 3)))
    ;; and with no arguments
    (should-error-arg (funcall fun '(:and)))
    ;; and with nonsense
    (should-error-arg (funcall fun '(:and bold "2")))
    ;; or with no arguments
    (should-error-arg (funcall fun '(:or)))
    ;; or with nonsense
    (should-error-arg (funcall fun '(:or bold "2")))
    ;; properties with symbols instead of keywords
    (should-error-arg (funcall fun '(tags '("hi"))))
    ;; multiple properties
    (should-error-arg (funcall fun '(:tags '("hi") :todo-keyword "DONE")))
    ;; just wrong...
    (should-error-arg (funcall fun nil))
    (should-error-arg (funcall fun "1"))
    (should-error-arg (funcall fun :1))))

(ert-deftest om--match-make-inner-pattern-form/error ()
  ;; Ensure `om--match-make-inner-form' will error when it supposed to
  ;; do so. All errors (in theory) should be tested here so that
  ;; we don't need to bother testing them anywhere else when we test
  ;; functions higher in the framework
  ;;
  ;; Assume:
  ;; - all invalid patterns at the condition level will be caught by
  ;;   `om--match-make-condition-form/error'.
  ;; - these error paths are independent of `END?' and `LIMIT' so
  ;;   set them both to nil
  (unless (fboundp 'om--match-make-inner-pattern-form)
    (error "Function not defined"))
  (let ((fun (-partial #'om--match-make-inner-pattern-form nil nil)))
    ;; slicers present
    (should-error-arg (funcall fun '(:first bold)))
    (should-error-arg (funcall fun '(:last bold)))
    (should-error-arg (funcall fun '(:nth bold)))
    (should-error-arg (funcall fun '(:sub bold)))
    (should-error-arg (funcall fun '(bold :first)))
    (should-error-arg (funcall fun '(bold :last)))
    (should-error-arg (funcall fun '(bold :nth)))
    (should-error-arg (funcall fun '(bold :sub)))
    ;; :many by itself
    (should-error-arg (funcall fun '(:many)))
    ;; :many with too many arguments
    (should-error-arg (funcall fun '(:many bold italic)))
    ;; :many! by itself
    (should-error-arg (funcall fun '(:many!)))
    ;; :many! with too many arguments
    (should-error-arg (funcall fun '(:many! bold italic)))
    ;; just wrong...
    (should-error-arg (funcall fun nil))
    (should-error-arg (funcall fun '(:swaggart)))))

(ert-deftest om--make-make-slicer-form ()
  ;; Ensure `om--match-make-inner-form' will error when it supposed to
  ;; do so. All errors (in theory) should be tested here so that
  ;; we don't need to bother testing them anywhere else when we test
  ;; functions higher in the framework
  ;;
  ;; Assume that all invalid patterns at the predicate and wildcard
  ;; level will be caught by `om--match-make-condition-form/error' and
  ;; `om--match-make-inner-pattern-form/error'
  (unless (fboundp 'om--make-make-slicer-form)
    (error "Function not defined"))
  (let ((fun #'om--make-make-slicer-form))
    ;; slicers by themselves
    (should-error-arg (funcall fun '(:first)))
    (should-error-arg (funcall fun '(:last)))
    (should-error-arg (funcall fun '(:nth)))
    (should-error-arg (funcall fun '(:sub)))
    ;; nth with non-integer
    (should-error-arg (funcall fun '(:nth "1" bold)))
    ;; nth with integer but nothing after
    (should-error-arg (funcall fun '(:nth 1)))
    ;; sub with non-integers
    (should-error-arg (funcall fun '(:sub "1" 2 bold)))
    (should-error-arg (funcall fun '(:sub 1 "2" bold)))
    ;; sub with flipped integers
    (should-error-arg (funcall fun '(:sub 2 1 bold)))
    (should-error-arg (funcall fun '(:sub -1 -2 bold)))
    ;; sub with split integers
    (should-error-arg (funcall fun '(:sub -1 2 bold)))
    ;; sub with nothing after it
    (should-error-arg (funcall fun '(:sub 1 2)))))

(defmacro match-should-equal (node result &rest patterns)
  "Return form to test if all PATTERNS applied NODE return RESULT."
  (declare (indent 2))
  (let ((tests (--map
                `(should (equal ,result
                                (->> (om-match ',it ,node)
                                     (-map #'om-to-trimmed-string))))
                patterns)))
    `(progn ,@tests)))

(defmacro match-slicer-should-equal (node expected pattern)
  "Return form to test if PATTERN applied to NODE works with all slicers.
EXPECTED is a list of matches returned using PATTERN if no slicer is
applied."
  (declare (indent 1))
  ;; The basic behavior of slicers can be put in terms of -drop(-last)
  ;; and -take(-last). Additionally, some slicing operations have
  ;; multiple syntactical representations. Ensure equality of all
  ;; these specifications here
  `(progn
     ;; these slicers have multiple equivalent expressions
     ;;
     ;; first match
     (match-should-equal node (-take 1 ,expected)
       (:first ,@pattern) (:nth 0 ,@pattern)
       (:sub 0 0 ,@pattern))
     ;; last match
     (match-should-equal node (-take-last 1 ,expected)
       (:last ,@pattern) (:nth -1 ,@pattern)
       (:sub -1 -1 ,@pattern))
     ;; nth match positive
     (match-should-equal node (-drop 1 (-take 2 ,expected))
       (:nth 1 ,@pattern) (:sub 1 1 ,@pattern))
     ;; nth match negative
     (match-should-equal node (-drop-last 1 (-take-last 2 ,expected))
       (:nth -2 ,@pattern) (:sub -2 -2 ,@pattern))
     ;; out of range positive
     (match-should-equal node nil
       (:nth 100 ,@pattern) (:sub 100 100 ,@pattern))
     ;; out of range negative
     (match-should-equal node nil
       (:nth -100 ,@pattern) (:sub -100 -100 ,@pattern))
     ;; bounded to out of range
     (match-should-equal node ,expected
       (:sub 0 100 ,@pattern) (:sub -100 -1 ,@pattern))
     ;;
     ;; these slicers can only be expressed one way
     ;;
     ;; zero-bounded finite positive
     (match-should-equal node (-take 2 ,expected)
       (:sub 0 1 ,@pattern))
     ;; zero-bounded finite negative
     (match-should-equal node (-take-last 2 ,expected)
       (:sub -2 -1 ,@pattern))
     ;; floating finite positive
     (match-should-equal node (-drop 1 (-take 3 ,expected))
       (:sub 1 2 ,@pattern))
     ;; floating finite negative
     (match-should-equal node (-drop-last 1 (-take-last 3 ,expected))
       (:sub -3 -2 ,@pattern))
     ;; floating out of range positive
     (match-should-equal node (-drop 1 ,expected)
       (:sub 1 100 ,@pattern))
     ;; floating out of range negative
     (match-should-equal node (-drop-last 1 ,expected)
       (:sub -100 -2 ,@pattern))))

;; Here we test the following pattern combinations
;; - multi-level condition
;; - :any + condition
;; - condition + :any
;; - :many
;; - :many!
;;
;; The reason for choosing these combinations is that all of them
;; combined should hit each of the valid form-building switches in
;; `om--match-make-inner-pattern-form'. Since the behavior of these
;; depends on the value of `LIMIT' and `END?' and these are set
;; depending on the slicer, testing these combinations with all
;; reasonable slicer combination should ensure that every path with
;; every combination of `LIMIT' and `END?' is tested. Note this
;; assumes that `om--match-make-condition-form' is working correctly
;; as the following test only use a few combinations in this function.
;; However, `om--match-make-condition-form' is independent of the
;; chosen slicer so this should not matter

(ert-deftest om-match/slicer-predicate ()
  ;; test the single/multiple condition path with all slicers
  (let ((node (->> (s-join "\n"
                           '("* one"
                             "** TODO two"
                             "2"
                             "** COMMENT three"
                             "3"
                             "** four"
                             "4"
                             "** DONE five"
                             "5"))
                   (om--from-string))))
    (match-slicer-should-equal node
      '("2" "3" "4" "5") (headline section))))

(ert-deftest om-match/slicer-any-first ()
  ;; test the :any + condition path with all slicers
  (let ((node (om-build-paragraph!
               "*_1_* */2/* _*3*_ _/4/_ /*5*/ /_6_/")))
    (match-slicer-should-equal node
      '("/2/" "*3*" "/4/" "*5*") (:any (:or bold italic)))))

(ert-deftest om-match/slicer-any-last ()
  ;; test the condition + :any path with all slicers
  (let ((node (om-build-paragraph!
               "*_1_* */2/* _*3*_ _/4/_ /*5*/ /_6_/")))
    (match-slicer-should-equal node
      '("_1_" "/2/" "*5*" "_6_") ((:or bold italic) :any))))

(ert-deftest om-match/slicer-many ()
  ;; Test the :many and :many! paths with all slicers. Here the node
  ;; is chosen such that some values are nested and thus :many will
  ;; return them but :many! will not
  (let ((node (->> (s-join "\n"
                           '("* one"
                             "- 1"
                             "- 2"
                             "  - 3"
                             "** two"
                             "- 4"
                             "- 5"
                             "  - 6"
                             "** three"
                             "- 7"
                             "- 8"
                             "  - 9"))
                   (om--from-string)))
        (expected '("- 1" "- 2\n  - 3" "- 3" "- 4" "- 5\n  - 6"
                    "- 6" "- 7" "- 8\n  - 9" "- 9"))
        (expected! '("- 1" "- 2\n  - 3" "- 4" "- 5\n  - 6" "- 7"
                     "- 8\n  - 9")))
    (match-slicer-should-equal node expected (:many item))
    (match-slicer-should-equal node expected! (:many! item))))

(provide 'om-dev-test)
;;; om-dev-test.el ends here
