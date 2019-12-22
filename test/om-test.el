;;; om-test.el --- Tests for om

;;; Commentary:

;;; Code:

(require 'dash)

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

(ert-deftest om--verify-pos-args/valid ()
  (should (equal '(one two) (om--verify-pos-args '(one two))))
  (should (equal nil (om--verify-pos-args nil))))

(ert-deftest om--verify-pos-args/error ()
  (should-error (om--verify-pos-args '(1)))
  (should-error (om--verify-pos-args '("one")))
  (should-error (om--verify-pos-args '((one)))))

(ert-deftest om--verify-rest-arg/valid ()
  (should (equal 'one (om--verify-rest-arg '(one))))
  (should (equal nil (om--verify-rest-arg nil))))

(ert-deftest om--verify-rest-arg/error ()
  ;; too long
  (should-error (om--verify-rest-arg '(one two)))
  ;; non-symbol
  (should-error (om--verify-rest-arg '(1)))
  (should-error (om--verify-rest-arg '("one")))
  (should-error (om--verify-rest-arg '((one)))))

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

(ert-deftest om--make-kwarg-let/valid ()
  (should
   (equal (om--make-kwarg-let 'k 'one)
          '(:one one (cadr (plist-member k (quote :one))))))
  (should
   (equal (om--make-kwarg-let 'k '((:two one)))
          '(:two one (cadr (plist-member k (quote :two))))))
  (should
   (equal (om--make-kwarg-let 'k '(one 1))
          '(:one one (or (cadr (plist-member k (quote :one))) 1))))
  (should
   (equal (om--make-kwarg-let 'k '((:two one) 1))
          '(:two one (or (cadr (plist-member k (quote :two))) 1)))))

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

(ert-deftest om--make-rest-partition-form/restargs ()
  ;; (should (equal '(nil (one)) (om--make-rest-partition-form '(one) nil nil)))
  (should (equal '(nil . (one)) (om--make-rest-partition-form '(one) nil t)))
  (should (equal '(nil . (one two)) (om--make-rest-partition-form '(one two) nil t))))

;; (ert-deftest om--make-rest-partition-form/combo ()
;;   (should (equal '((:one one) (two)) (om--make-rest-partition-form '(:one one two) '(:one) t))))

(ert-deftest om--make-rest-partition-form/error ()
  ;; invalid keywords
  (should-error (om--make-rest-partition-form '(:one one) '(:two) nil))
  ;; too many arguments
  (should-error (om--make-rest-partition-form '(:one one two) (:one) nil))
  ;; multiple keywords
  (should-error (om--make-rest-partition-form '(:one one :one three two) (:one) nil)))

;; (ert-deftest om--match-filter/index ()
;;   (let ((contents '(0 1 2 3 4 5)))
;;     (should (equal (om--match-filter 0 contents) '(0)))
;;     (should (equal (om--match-filter '(> 0) contents) '(1 2 3 4 5)))
;;     (should (equal (om--match-filter '(>= 1) contents) '(1 2 3 4 5)))
;;     (should (equal (om--match-filter '(< 2) contents) '(0 1)))
;;     (should (equal (om--match-filter '(<= 1) contents) '(0 1)))
;;     (should (equal (om--match-filter '(> -2) contents) '(5)))
;;     (should (equal (om--match-filter '(>= -1) contents) '(5)))
;;     (should (equal (om--match-filter '(< -1) contents) '(0 1 2 3 4)))
;;     (should (equal (om--match-filter '(<= -2) contents) '(0 1 2 3 4)))
;;     (should (equal (om--match-filter -1 contents) '(5)))
;;     (should-not (om--match-filter 6 contents))
;;     (should-not (om--match-filter -7 contents))
;;     (should-not (om--match-filter '(< 0) contents))
;;     (should-not (om--match-filter '(> -1) contents))))

;; (ert-deftest om--match-filter/type ()
;;   (let ((contents (list (om-build-bold "text1")
;;                         (om-build-bold "text2")
;;                         (om-build-italic "text3"))))
;;     (should (equal (->> (om--match-filter 'bold contents)
;;                         (--map (om-to-trimmed-string it)))
;;                    '("*text1*" "*text2*")))
;;     (should (equal (->> (om--match-filter 'italic contents)
;;                         (--map (om-to-trimmed-string it)))
;;                    '("/text3/")))
;;     (should-not (->> (om--match-filter 'underline contents)
;;                      (--map (om-to-trimmed-string it))))))

;; (ert-deftest om--match-filter/property ()
;;   (let ((contents
;;          (list (om-build-item :checkbox 'off :tag '("one"))
;;                (om-build-item :checkbox 'on :tag '("two"))
;;                (om-build-item :checkbox 'on :tag '("three")))))
;;     (should (equal (->> (om--match-filter '(:checkbox off) contents)
;;                         (--map (om-to-trimmed-string it)))
;;                    '("- [ ] one ::")))
;;     (should (equal (->> (om--match-filter '(:checkbox on) contents)
;;                         (--map (om-to-trimmed-string it)))
;;                    '("- [X] two ::" "- [X] three ::")))
;;     (should-not (->> (om--match-filter '(:checkbox trans) contents)
;;                      (--map (om-to-trimmed-string it))))))

;; (ert-deftest om--match-filter/compound ()
;;   (let ((contents
;;          (list
;;           (om-build-section (om-build-paragraph "paragraph"))
;;           (om-build-headline :title '("headline1") :todo-keyword "TODO")
;;           (om-build-headline :title '("headline2") :todo-keyword "DONE"))))
;;     (should (equal (->> (om--match-filter
;;                          '(:or section headline) contents)
;;                         (--map (om-to-trimmed-string it)))
;;                    '("paragraph" "* TODO headline1" "* DONE headline2")))
;;     (should (equal (->> (om--match-filter
;;                          '(:and headline (:todo-keyword "DONE")) contents)
;;                         (--map (om-to-trimmed-string it)))
;;                    '("* DONE headline2")))
;;     (should-not (->> (om--match-filter
;;                       '(:and headline (:todo-keyword "CANC")) contents)
;;                      (--map (om-to-trimmed-string it))))))

(ert-deftest om--match-filter/error ()
  (should-error (om--match-filter "no-strings" t))
  (should-error (om--match-filter :no-keywords t))
  ;; these should not be by themselves
  (should-error (om--match-filter '(<) t))
  (should-error (om--match-filter '(<=) t))
  (should-error (om--match-filter '(>) t))
  (should-error (om--match-filter '(>=) t))
  (should-error (om--match-filter '(:and) t))
  (should-error (om--match-filter '(:or) t))
  ;; numeric operators must be followed by integer
  (should-error (om--match-filter '(< 1.0) t))
  (should-error (om--match-filter '(< 'no-symbols) t))
  (should-error (om--match-filter '(< :no-keywords) t))
  (should-error (om--match-filter '(< "no-string") t))
  ;; only one index allowed
  (should-error (om--match-filter '(< 1 2) t)))

(defmacro om-test--file-headline (path index)
  `(om-test-with-file
    ,path (nth ,index (om-test-parse-all-headlines))))

(ert-deftest om-find/error ()
  (let ((dummy (om-build-headline :title '("dummy"))))
    ;; TODO test invalid types
    ;; (should-error (om-find dummy 'invalid))
    (should-error (om-find '(:many) dummy))
    (should-error (om-find '(:many section paragraph) dummy))))

;;; om-test.el ends here
