;;; om-test.el --- Tests for om

;;; Commentary:

;;; Code:

(require 'dash)

(defun should-have-equal-properties (e1 e2)
  (unless (eq (om-elem--get-type e1) (om-elem--get-type e2))
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

(defun om-elem--compare-object-props (elem string)
  (should-have-equal-properties
   elem
   (->> (om-elem--from-string (concat " " string))
        (om-elem--get-nested-contents '(0 1)))))

(ert-deftest om-elem--code/valid-props ()
  (om-elem--compare-object-props
   (om-elem-build-code "value") "~code~"))

(ert-deftest om-elem--entity/valid-props ()
  (om-elem--compare-object-props
   (om-elem-build-entity "pi") "\\pi"))

(ert-deftest om-elem--export-snippet/valid-props ()
  (om-elem--compare-object-props
   (om-elem-build-export-snippet "backend" "value") "@@im:padme@@"))

(ert-deftest om-elem--inline-babel-call/valid-props ()
  (om-elem--compare-object-props
   (om-elem-build-inline-babel-call "name") "call_name()"))

(ert-deftest om-elem--inline-src-block/valid-props ()
  (om-elem--compare-object-props
   (om-elem-build-inline-src-block "lang" "value") "src_lang{value}"))

;; TODO add latex fragment

(ert-deftest om-elem--line-break/valid-props ()
  (om-elem--compare-object-props
   (om-elem-build-line-break) "\\\\\n"))

(ert-deftest om-elem--macro/valid-props ()
  (om-elem--compare-object-props
   (om-elem-build-macro "value") "{{{value}}}"))

(ert-deftest om-elem--statistics-cookie/valid-props ()
  (om-elem--compare-object-props
   (om-elem-build-statistics-cookie '(1)) "[/]"))

(ert-deftest om-elem--target/valid-props ()
  (om-elem--compare-object-props
   (om-elem-build-target "value") "<<value>>"))

(ert-deftest om-elem--timestamp/valid-props ()
  (om-elem--compare-object-props
   (om-elem-build-timestamp! 'inactive '(2019 1 1))
   ;; TODO the timestamp parser does not add properties for warnings
   ;; or repeaters if they are not given, this appears to be a bug
   "[2019-01-01 Tue +1d -1d]"))

(ert-deftest om-elem--verbatim/valid-props ()
  (om-elem--compare-object-props
   (om-elem-build-verbatim "value") "=value="))

;; recursive objects

(ert-deftest om-elem--bold/valid-props ()
  (om-elem--compare-object-props
   (om-elem-build-bold) "*bold*"))

(ert-deftest om-elem--footnote-reference/valid-props ()
  (om-elem--compare-object-props
   (om-elem-build-footnote-reference) "[fn:1]"))

(ert-deftest om-elem--italic/valid-props ()
  (om-elem--compare-object-props
   (om-elem-build-italic) "/italic/"))

(ert-deftest om-elem--link/valid-props ()
  (om-elem--compare-object-props
   (om-elem-build-link "path") "[[path]]"))

(ert-deftest om-elem--radio-target/valid-props ()
  (om-elem--compare-object-props
   (om-elem-build-radio-target) "<<<target>>>"))

(ert-deftest om-elem--strike-through/valid-props ()
  (om-elem--compare-object-props
   (om-elem-build-strike-through) "+bad+"))

(ert-deftest om-elem--superscript/valid-props ()
  (should-have-equal-properties
   (om-elem-build-superscript)
   (->> (om-elem--from-string "thisis^super")
        (om-elem--get-nested-contents '(0 1)))))

(ert-deftest om-elem--subscript/valid-props ()
  (should-have-equal-properties
   (om-elem-build-subscript)
   (->> (om-elem--from-string "thisis_subpar")
        (om-elem--get-nested-contents '(0 1)))))

(ert-deftest om-elem--table-cell/valid-props ()
  (should-have-equal-properties
   (om-elem-build-table-cell "cell")
   (->> (om-elem--from-string "| cell |")
        (om-elem--get-nested-contents '(0 0 0)))))

(ert-deftest om-elem--underline/valid-props ()
  (om-elem--compare-object-props
   (om-elem-build-underline) "_bad_"))

;; elements

(defun om-elem--compare-element-props (elem string)
  (should-have-equal-properties
   elem
   (->> (om-elem--from-string string)
        (om-elem--get-nested-contents '(0)))))

(ert-deftest om-elem--babel-call/valid-props ()
  (om-elem--compare-element-props
   (om-elem-build-babel-call "call") "#+CALL: name()"))

(ert-deftest om-elem--clock/valid-props ()
  (om-elem--compare-element-props
   (om-elem-build-clock (om-elem-build-timestamp! 'inactive '(2019 1 1)))
   "CLOCK: [2019-01-01 Tue]"))

(ert-deftest om-elem--comment/valid-props ()
  (om-elem--compare-element-props
   (om-elem-build-comment "useless") "# useless"))

(ert-deftest om-elem--comment-block/valid-props ()
  (om-elem--compare-element-props
   (om-elem-build-comment-block "useless")
   "#+BEGIN_COMMENT\nuseless\n#+END_COMMENT"))

(ert-deftest om-elem--diary-sexp/valid-props ()
  (om-elem--compare-element-props
   (om-elem-build-diary-sexp '(value)) "%%(value)"))

(ert-deftest om-elem--example-block/valid-props ()
  (om-elem--compare-element-props
   (om-elem-build-example-block "useless")
   "#+BEGIN_EXAMPLE\nuseless\n#+END_EXAMPLE"))

(ert-deftest om-elem--export-block/valid-props ()
  (om-elem--compare-element-props
   (om-elem-build-export-block "type" "value")
   "#+BEGIN_EXPORT type\nuseless\n#+END_EXPORT"))

(ert-deftest om-elem--fixed-width/valid-props ()
  (om-elem--compare-element-props
   (om-elem-build-fixed-width "value") ": value"))

(ert-deftest om-elem--horizontal-rule/valid-props ()
  (om-elem--compare-element-props
   (om-elem-build-horizontal-rule) "-----"))

(ert-deftest om-elem--keyword/valid-props ()
  (om-elem--compare-element-props
   (om-elem-build-keyword "key" "val") "#+KEY: val"))

(ert-deftest om-elem--latex-environment/valid-props ()
  (om-elem--compare-element-props
   (om-elem-build-latex-environment "gloves" "text")
   "\\begin{env}\nvalue\n\\end{env}"))

(ert-deftest om-elem--node-property/valid-props ()
  (should-have-equal-properties
   (om-elem-build-node-property "key" "value")
   (->> (om-elem--from-string "* dummy\n:PROPERTIES:\n:key: val\n:END:")
        (om-elem--get-nested-contents '(0 0 0)))))

(ert-deftest om-elem--planning/valid-props ()
  (should-have-equal-properties
   (om-elem-build-planning :closed (om-elem-build-timestamp! 'inactive '(2019 1 1)))
   (->> (om-elem--from-string "* dummy\nCLOSED: [2019-01-01 Tue]")
        (om-elem--get-nested-contents '(0 0)))))

(ert-deftest om-elem--src-block/valid-props ()
  (om-elem--compare-element-props
   (om-elem-build-src-block "value")
   "#+BEGIN_SRC\nuseless\n#+END_SRC"))

;; containers

(ert-deftest om-elem--paragraph/valid-props ()
  (should-have-equal-properties
   (om-elem-build-paragraph)
   (->> (om-elem--from-string "text")
        (om-elem--get-nested-contents '(0)))))

(ert-deftest om-elem--table-row/valid-props ()
  (should-have-equal-properties
   (om-elem-build-table-row)
   (->> (om-elem--from-string "| row |")
        (om-elem--get-nested-contents '(0 0)))))

(ert-deftest om-elem--verse-block/valid-props ()
  (should-have-equal-properties
   (om-elem-build-verse-block)
   (->> (om-elem--from-string "#+BEGIN_VERSE\nthing\n#+END_VERSE")
        (om-elem--get-nested-contents '(0)))))

(ert-deftest om-elem--center-block/valid-props ()
  (om-elem--compare-element-props
   (om-elem-build-center-block)
   "#+BEGIN_CENTER\nuseless\n#+END_CENTER"))

(ert-deftest om-elem--drawer/valid-props ()
  (om-elem--compare-element-props
   (om-elem-build-drawer "name")
   ":LOGBOOK:\nuseless\n:END:"))

(ert-deftest om-elem--dynamic-block/valid-props ()
  (om-elem--compare-element-props
   (om-elem-build-dynamic-block "name" :arguments '(:key val))
   "#+BEGIN: name args\nuseless\n#+END:"))

(ert-deftest om-elem--footnote-definition/valid-props ()
  (om-elem--compare-element-props
   (om-elem-build-footnote-definition "label") "[fn:label]\n"))

(ert-deftest om-elem--headline/valid-props ()
  (should-have-equal-properties
   (om-elem-build-headline)
   (om-elem--from-string "* head")))

(ert-deftest om-elem--item/valid-props ()
  (should-have-equal-properties
   (om-elem-build-item)
   (->> (om-elem--from-string "- head")
        (om-elem--get-nested-contents '(0 0)))))

(ert-deftest om-elem--plain-list/valid-props ()
  (om-elem--compare-element-props
   (om-elem-build-plain-list) "- item"))

(ert-deftest om-elem--property-drawer/valid-props ()
  (should-have-equal-properties
   (om-elem-build-property-drawer)
   (->> (om-elem--from-string "* dummy\n:PROPERTIES:\n:END:")
        (om-elem--get-nested-contents '(0 0)))))

(ert-deftest om-elem--quote-block/valid-props ()
  (om-elem--compare-element-props
   (om-elem-build-quote-block)
   "#+BEGIN_QUOTE\n#+END_QUOTE"))

(ert-deftest om-elem--section/valid-props ()
  (om-elem--compare-element-props
   (om-elem-build-section) "* dummy\nstuff"))

(ert-deftest om-elem--special-block/valid-props ()
  (om-elem--compare-element-props
   (om-elem-build-special-block "type")
   "#+BEGIN_type:\n#+END_type:"))

(ert-deftest om-elem--table/valid-props ()
  (om-elem--compare-element-props
   (om-elem-build-table) "| table |"))

(defun should-equal (string elem)
  (should (equal string (->> (om-elem-to-string elem) (s-trim)))))

(defun should-match (regexp elem)
  (should (s-match regexp (->> (om-elem-to-string elem) (s-trim)))))

(ert-deftest om-elem--make-header ()
  (should (equal (om-elem--make-header '("docstring" (print 'hi)) nil)
                 "docstring\n\n(fn)"))
  (should (equal (om-elem--make-header '("docstring" (print 'hi)) '(one))
                 "docstring\n\n(fn ONE)"))
  (should (equal (om-elem--make-header '("docstring" (print 'hi)) '(one two))
                 "docstring\n\n(fn ONE TWO)")))

(ert-deftest om-elem--verify-pos-args/valid ()
  (should (equal '(one two) (om-elem--verify-pos-args '(one two))))
  (should (equal nil (om-elem--verify-pos-args nil))))

(ert-deftest om-elem--verify-pos-args/error ()
  (should-error (om-elem--verify-pos-args '(1)))
  (should-error (om-elem--verify-pos-args '("one")))
  (should-error (om-elem--verify-pos-args '((one)))))

(ert-deftest om-elem--verify-rest-arg/valid ()
  (should (equal 'one (om-elem--verify-rest-arg '(one))))
  (should (equal nil (om-elem--verify-rest-arg nil))))

(ert-deftest om-elem--verify-rest-arg/error ()
  ;; too long
  (should-error (om-elem--verify-rest-arg '(one two)))
  ;; non-symbol
  (should-error (om-elem--verify-rest-arg '(1)))
  (should-error (om-elem--verify-rest-arg '("one")))
  (should-error (om-elem--verify-rest-arg '((one)))))

(ert-deftest om-elem--make-optarg-let/valid ()
  (should (equal (om-elem--make-optarg-let 'one 0)
                 '(one (nth 0 --opt-args))))
  (should (equal (om-elem--make-optarg-let '(one 1) 0)
                 '(one (or (nth 0 --opt-args) 1)))))

(ert-deftest om-elem--make-optarg-let/error ()
  ;; arg should be a symbol
  (should-error (om-elem--make-optarg-let 1 0))
  (should-error (om-elem--make-optarg-let "one" 0))
  (should-error (om-elem--make-optarg-let '(one) 0))
  ;; TODO this doesn't work yet
  ;; (should-error (om-elem--make-optarg-let :one 0))
  ;; wrong list length
  (should-error (om-elem--make-optarg-let '(one one 1) 0)))

(ert-deftest om-elem--make-kwarg-let/valid ()
  (should
   (equal (om-elem--make-kwarg-let 'one)
          '(:one one (cadr (plist-member --kw-args (quote :one))))))
  (should
   (equal (om-elem--make-kwarg-let '((:two one)))
          '(:two one (cadr (plist-member --kw-args (quote :two))))))
  (should
   (equal (om-elem--make-kwarg-let '(one 1))
          '(:one one (or (cadr (plist-member --kw-args (quote :one))) 1))))
  (should
   (equal (om-elem--make-kwarg-let '((:two one) 1))
          '(:two one (or (cadr (plist-member --kw-args (quote :two))) 1)))))

(ert-deftest om-elem--make-kwarg-let/error ()
  ;; list too long
  (should-error (om-elem--make-kwarg-let '(one two three)))
  ;; keyword slot must be a real keyword
  (should-error (om-elem--make-kwarg-let '((one two))))
  (should-error (om-elem--make-kwarg-let '((one two) three)))
  ;; single arg must be a symbol but not a keyword
  ;; TODO the keyword guard does not work yet
  ;; (should-error (om-elem--make-kwarg-let :one))
  (should-error (om-elem--make-kwarg-let 1))
  (should-error (om-elem--make-kwarg-let "one"))
  (should-error (om-elem--make-kwarg-let '(1))))

(ert-deftest om-elem--partition-rest-args/optargs ()
  (should (equal '((one) nil nil) (om-elem--partition-rest-args '(one) 1 nil nil)))
  (should (equal '((one) nil nil) (om-elem--partition-rest-args '(one) 1 '(:one) nil)))
  (should (equal '((one) nil nil) (om-elem--partition-rest-args '(one) 1 nil t)))
  (should (equal '((one) nil nil) (om-elem--partition-rest-args '(one) 1 '(:one) t))))

(ert-deftest om-elem--partition-rest-args/kwargs ()
  (should (equal '(nil (:one one) nil) (om-elem--partition-rest-args '(:one one) 0 '(:one) nil)))
  (should (equal '(nil (:one one) nil) (om-elem--partition-rest-args '(:one one) 0 '(:one) t))))
(ert-deftest om-elem--partition-rest-args/restargs ()
  (should (equal '(nil nil (one)) (om-elem--partition-rest-args '(one) 0 nil t))))
  ;; optargs + kwargs

(ert-deftest om-elem--partition-rest-args/combo ()
  (should (equal '((one) (:two two) nil) (om-elem--partition-rest-args '(one :two two) 1 '(:two) nil)))
  ;; optargs + restargs
  (should (equal '((one) nil (two)) (om-elem--partition-rest-args '(one two) 1 nil t)))
  ;; kwargs + restargs
  (should (equal '(nil (:one one) (two)) (om-elem--partition-rest-args '(:one one two) 0 '(:one) t))))

(ert-deftest om-elem--partition-rest-args/error ()
  ;; invalid keywords
  (should-error (om-elem--partition-rest-args '(:one one) 0 '(:two) nil))
  ;; too many arguments
  (should-error (om-elem--partition-rest-args '(one) 0 nil nil))
  (should-error (om-elem--partition-rest-args '(one two) 1 nil nil))
  (should-error (om-elem--partition-rest-args '(:one one two) 0 (:one) nil)))

(ert-deftest om-elem-filter-query/index ()
  (let ((contents '("one" "two" "three")))
    (should (equal (om-elem-filter-query 0 contents) '("one")))
    (should (equal (om-elem-filter-query '(> 0) contents) '("two" "three")))
    (should (equal (om-elem-filter-query '(>= 1) contents) '("two" "three")))
    (should (equal (om-elem-filter-query '(< 2) contents) '("one" "two")))
    (should (equal (om-elem-filter-query '(<= 1) contents) '("one" "two")))
    (should (equal (om-elem-filter-query -1 contents) '("three")))
    (should-not (om-elem-filter-query 3 contents))
    (should-not (om-elem-filter-query -4 contents))))

(ert-deftest om-elem-filter-query/type ()
  (let ((contents (list (om-elem-build-bold "text1")
                        (om-elem-build-bold "text2")
                        (om-elem-build-italic "text3"))))
    (should (equal (->> (om-elem-filter-query 'bold contents)
                        (--map (om-elem-to-trimmed-string it)))
                   '("*text1*" "*text2*")))
    (should (equal (->> (om-elem-filter-query 'italic contents)
                        (--map (om-elem-to-trimmed-string it)))
                   '("/text3/")))
    (should-not (->> (om-elem-filter-query 'underline contents)
                     (--map (om-elem-to-trimmed-string it))))))

(ert-deftest om-elem-filter-query/property ()
  (let ((contents
         (list (om-elem-build-item :checkbox 'off :tag '("one"))
               (om-elem-build-item :checkbox 'on :tag '("two"))
               (om-elem-build-item :checkbox 'on :tag '("three")))))
    (should (equal (->> (om-elem-filter-query '(:checkbox off) contents)
                        (--map (om-elem-to-trimmed-string it)))
                   '("- [ ] one ::")))
    (should (equal (->> (om-elem-filter-query '(:checkbox on) contents)
                        (--map (om-elem-to-trimmed-string it)))
                   '("- [X] two ::" "- [X] three ::")))
    (should-not (->> (om-elem-filter-query '(:checkbox trans) contents)
                     (--map (om-elem-to-trimmed-string it))))))

(ert-deftest om-elem-filter-query/compound ()
  (let ((contents
         (list
          (om-elem-build-section (om-elem-build-paragraph "paragraph"))
          (om-elem-build-headline :title '("headline1") :todo-keyword "TODO")
          (om-elem-build-headline :title '("headline2") :todo-keyword "DONE"))))
    (should (equal (->> (om-elem-filter-query
                         '(:or section headline) contents)
                        (--map (om-elem-to-trimmed-string it)))
                   '("paragraph" "* TODO headline1" "* DONE headline2")))
    (should (equal (->> (om-elem-filter-query
                         '(:and headline (:todo-keyword "DONE")) contents)
                        (--map (om-elem-to-trimmed-string it)))
                   '("* DONE headline2")))
    (should-not (->> (om-elem-filter-query
                      '(:and headline (:todo-keyword "CANC")) contents)
                     (--map (om-elem-to-trimmed-string it))))))

(ert-deftest om-elem-filter-query/error ()
  (should-error (om-elem-filter-query "no-strings" t))
  (should-error (om-elem-filter-query :no-keywords t))
  ;; these should not be by themselves
  (should-error (om-elem-filter-query '(<) t))
  (should-error (om-elem-filter-query '(<=) t))
  (should-error (om-elem-filter-query '(>) t))
  (should-error (om-elem-filter-query '(>=) t))
  (should-error (om-elem-filter-query '(:and) t))
  (should-error (om-elem-filter-query '(:or) t))
  ;; numeric operators must be followed by integer
  (should-error (om-elem-filter-query '(< 1.0) t))
  (should-error (om-elem-filter-query '(< 'no-symbols) t))
  (should-error (om-elem-filter-query '(< :no-keywords) t))
  (should-error (om-elem-filter-query '(< "no-string") t))
  ;; only one index allowed
  (should-error (om-elem-filter-query '(< 1 2) t)))

(defmacro om-test--file-headline (path index)
  `(om-test-with-file
    ,path (nth ,index (om-test-parse-all-headlines))))

(ert-deftest om-elem-find/error ()
  (let ((dummy (om-elem-build-headline :title '("dummy"))))
    ;; TODO test invalid types
    ;; (should-error (om-elem-find dummy 'invalid))
    (should-error (om-elem-find '(:many) dummy))
    (should-error (om-elem-find '(:many section paragraph) dummy))))

;;; om-test.el ends here
