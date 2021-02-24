;;; org-ml-test-internal.el --- Internal tests for org-ml

;;; Commentary:

;;; Code:

(require 's)
(require 'dash)
(require 'org-ml)
(require 'org-ml-macs)
(require 'org-ml-test-common)

;;; LIST OPERATIONS

(describe "internal list functions"
  (describe "org-ml--pad-or-truncate"
    (before-each
      (setq finite-list '(1 2 3)))
    (it "zero length list with zero length"
      (expect (org-ml--pad-or-truncate 0 'x nil) :to-equal nil))
    (it "zero length list with positive length"
      (expect (org-ml--pad-or-truncate 1 'x nil) :to-equal '(x)))
    (it "positive length list; length is less"
      (expect (org-ml--pad-or-truncate 2 'x finite-list) :to-equal '(1 2)))
    (it "positive length list; length is equal"
      (expect (org-ml--pad-or-truncate 3 'x finite-list) :to-equal '(1 2 3)))
    (it "positive length list; length is greater"
      (expect (org-ml--pad-or-truncate 4 'x finite-list) :to-equal '(1 2 3 x)))
    (it "positive length list; length is zero"
      (expect (org-ml--pad-or-truncate 0 'x finite-list) :to-equal nil)))

  ;; TODO add plist-get-keys?
  ;; TODO add plist-get-vals?
  ;; TODO add plist-map-values?

  (describe "org-ml--is-plist"
    (it "finite plist"
      (expect (org-ml--is-plist '(:one one :two 2 :three "3")) :to-be-truthy))
    (it "zero-length plist"
      (expect (org-ml--is-plist nil) :to-be-truthy))
    (it "symbols instead of keywords"
      (expect (org-ml--is-plist '(one one two 2 three "3")) :not :to-be-truthy))
    (it "incomplete"
      (expect (org-ml--is-plist '(:one one :two 2 :three)) :not :to-be-truthy))
    (it "not list"
      (expect (org-ml--is-plist ":one one :two 2 :three") :not :to-be-truthy))))

;; TODO add plist-remove?

;;; inter-list operations

(defmacro org-ml--inter-list-ops-test (fun input output-single
                                           output-upper output-lower)
  "Return form to test intra-index list operations using FUN.
INPUT is an input list, OUTPUT-SINGLE is a list made as if FUN were
applied to an empty list, OUTPUT-UPPER is the input list with FUN
applied as if it was given the highest possible index, and OUTPUT-LOWER
is the converse."
  (declare (indent 1))
  `(progn
     (it "zero length list at 0"
       (expect ,output-single :to-equal (funcall ,fun 0 nil))
       (expect ,output-single :to-equal (funcall ,fun -1 nil)))
     (it "zero length list (overrange)"
       (should-error (funcall fun 100 nil))
       (expect ,output-single :to-equal (funcall ,fun 100 nil t)))
     (it "zero length list (underrange)"
       (should-error (funcall fun -100 nil))
       (expect ,output-single :to-equal (funcall ,fun -100 nil t)))
     (it "finite list (in range)"
       (expect ,output-lower :to-equal (funcall ,fun 0 ,input))
       (expect ,output-upper :to-equal (funcall ,fun -1 ,input)))
     (it "finite list (overrange)"
       (should-error (funcall fun 100 '(1 2)))
       (expect ,output-upper :to-equal (funcall ,fun 100 ,input t)))
     (it "finite list (underrange)"
       (should-error (funcall fun -100 '(1 2)))
       (expect ,output-lower :to-equal (funcall ,fun -100 ,input t)))))

(defmacro org-ml--intra-list-ops-test (fun input output-upper output-lower)
  "Return form to test intra-index list operations using FUN.
INPUT is an input list, OUTPUT-UPPER is the input list with FUN
applied as if it was given the highest possible index, and OUTPUT-LOWER
is the converse."
  (declare (indent 1))
  `(progn
     (it "index 0 in an empty list"
       (should-error (funcall ,fun 0 nil))
       (should-error (funcall ,fun 0 nil t))
       (expect (funcall ,fun 0 nil 'permit-empty) :not :to-be-truthy))
     (it "overrange in empty list"
       (should-error (funcall ,fun 100 nil))
       (should-error (funcall ,fun 100 nil t))
       (expect (funcall ,fun 100 nil 'permit-empty) :not :to-be-truthy))
     (it "underrange in empty list"
       (should-error (funcall ,fun -100 nil))
       (should-error (funcall ,fun -100 nil t))
       (expect (funcall ,fun -100 nil 'permit-empty) :not :to-be-truthy))
     (it "positive in finite list"
       (expect ,output-lower :to-equal (funcall ,fun 0 ,input)))
     (it "negative in finite list"
       (expect ,output-upper :to-equal (funcall ,fun -1 ,input)))
     (it "positive overrange in finite list"
       (expect ,output-upper :to-equal (funcall ,fun 100 ,input t))
       (should-error (funcall ,fun 100 input)))
     (it "negative underrange in finite list"
       (expect ,output-lower :to-equal (funcall ,fun -100 ,input t))
       (should-error (funcall fun -100 ,input)))))

(describe "test consistency of internal list function index references"
  (describe "inter-member references"
    ;; These functions operate using indices that refer to spaces between list
    ;; members. As such there is no such thing as a nonsensical index. Since
    ;; there will always be the option to add to the front or the back of the
    ;; list, even an empty list has a logical index that points to these
    ;; locations (they just happen to be the same). Therefore, the only errors
    ;; we need to catch here are those that refer to out of range indices.

    (describe "org-ml--insert-at"
      (org-ml--inter-list-ops-test (lambda (n list &optional p)
                                     (org-ml--insert-at n 'x list p))
        '(1 2) '(x) '(1 2 x) '(x 1 2)))
    (describe "org-ml--split-at"
      (org-ml--inter-list-ops-test #'org-ml--split-at
        '(1 2) nil '((1 2) nil) '(nil (1 2))))
    (describe "org-ml--splice-at"
      (org-ml--inter-list-ops-test (lambda (n list &optional p)
                                     (org-ml--splice-at n '(x y) list p))
        '(1 2) '(x y) '(1 2 x y) '(x y 1 2))))

  (describe "intra-member references"
    ;; These functions operate using indices that refer to explicit members of a
    ;; list. As such there will be no possible integers that will be valid for
    ;; an empty list. This provides one extra error case to test, which is the
    ;; possibility that we cannot operate on the list and thus return nil. All
    ;; else is the same relative to the inter-list operations tests above

    (describe "org-ml--remove-at/properties"
      (org-ml--intra-list-ops-test #'org-ml--remove-at '(1 2 3) '(1 2) '(2 3)))

    (describe "org-ml--replace-at/properties"
      (org-ml--intra-list-ops-test (lambda (n list &optional p)
                                     (org-ml--replace-at n 'x list p))
                                   '(1 2 3) '(1 2 x) '(x 2 3)))

    (describe "org-ml--nth/properties"
      (org-ml--intra-list-ops-test #'org-ml--nth '(1 2 3) 3 1))))

(defmacro org-ml--test-list-functor (fun map-fun single-a single-b multi-a multi-b)
  (declare (indent 2))
  `(progn
    (it "mapping empty list should return empty list"
      (expect (,fun (,map-fun it) nil) :not :to-be-truthy))
    (it "mapping list with one member should return that member modified"
      (expect ,single-a :to-equal (,fun (,map-fun it) ,single-b)))
    (it "mapping list with multiple members should only modify one member"
      (expect ,multi-a :to-equal (,fun (,map-fun it) ,multi-b)))
    (it "identity should hold true for any length list (0, 1, and 1+)"
      (--each '(nil (1) (1 2))
        (expect it :to-equal (,fun (identity it) it))))))

(describe "list functors"
  (describe "org-ml--map-first"
    (org-ml--test-list-functor org-ml--map-first* upcase
      '("X") '("x") '("A" "b" "c") '("a" "b" "c")))
  (describe "org-ml--map-last"
    (org-ml--test-list-functor org-ml--map-last* upcase
      '("X") '("x") '("a" "b" "C") '("a" "b" "c"))))

;;; FROM STRING CONVERSTION

(defun org-ml--plist-equal-p (exclude-props plist1 plist2)
  (cl-flet
      ((partition-plist
        (props plist)
        (->> (-partition 2 plist)
             (--remove (memq (car it) props)))))
    (let ((a (partition-plist exclude-props plist1))
          (b (partition-plist exclude-props plist2)))
      (and (equal (length a) (length b))
           (cl-subsetp a b :test #'equal)
           (cl-subsetp a b :test #'equal)))))

(defun org-ml--equal~ (exclude-props node1 node2)
  (if (and (stringp node1) (stringp node2))
      `(expect ,node1 :to-equal ,node2)
    (-let (((type1 . (props1 . children1)) node1)
           ((type2 . (props2 . children2)) node2))
      `(progn
         (expect ',type1 :to-be ',type2)
         (expect (org-ml--plist-equal-p ',exclude-props ',props1 ',props2))
         (and (eq ',type1 ',type2)
              (->> (-zip-fill nil ',children1 ',children2)
                   (--all? (org-ml--equal~ ',exclude-props (car it) (cdr it)))))))))

(defun org-ml--test-from-string (omit-props &rest specs)
  (declare (indent 1))
  (let ((props (append omit-props '(:begin :contents-begin :end :contents-end
                                           :parent :post-affiliated :name
                                           :plot :header :results :caption))))
    (->> (-partition 2 specs)
         (--map (-let* (((node string) it)
                        (type (org-ml-get-type node)))
                  `(it ,(format "%s - %s" type (s-replace "\n" "\\n" string))
                     ,(org-ml--equal~ props node (org-ml-from-string type string))))))))

(defmacro describe-many (header &rest forms)
  (declare (indent 1))
  (let ((it-forms (-flatten-n 1 (-map #'eval forms))))
    `(describe ,header ,@it-forms)))

(describe "converting from string"
  (describe-many "object leaf nodes"
    (org-ml--test-from-string nil
      (org-ml-build-code "code") "~code~")
    (org-ml--test-from-string '(:latex :latex-math-p :ascii :html :latin1 :utf-8)
      (org-ml-build-entity "pi") "\\pi")
    (org-ml--test-from-string nil
      (org-ml-build-export-snippet "be" "val") "@@be:val@@")
    (org-ml--test-from-string '(:value)
      (org-ml-build-inline-babel-call "ktulu") "call_ktulu()")
    (org-ml--test-from-string '(:value)
      (org-ml-build-inline-src-block "python") "src_python{}")
    (org-ml--test-from-string nil
      (org-ml-build-line-break) "\\\\\n")
    (org-ml--test-from-string nil
      (org-ml-build-latex-fragment "$1+1$") "$1+1$")
    (org-ml--test-from-string nil
      (org-ml-build-macro "macro") "{{{macro}}}")
    (org-ml--test-from-string '(:value)
      (org-ml-build-radio-target "radio") "<<<radio>>>")
    (org-ml--test-from-string nil
      (org-ml-build-statistics-cookie '(1 2)) "[1/2]"
      (org-ml-build-statistics-cookie '(nil nil)) "[/]"
      (org-ml-build-statistics-cookie '(50)) "[50%]"
      (org-ml-build-statistics-cookie '(nil)) "[%]")
    (org-ml--test-from-string nil
      (org-ml-build-target "target") "<<target>>")
    (org-ml--test-from-string '(:raw-value)
      (->> (org-ml-build-timestamp! '(2020 1 1 0 0)
                                    :end '(2020 1 1 0 10)
                                    :repeater '(cumulate 1 day)
                                    :warning '(all 1 day))
           (org-ml-timestamp-set-collapsed nil))
      "[2020-01-01 Tue 00:00-00:10 -1d +1d]")
    (org-ml--test-from-string nil
      (org-ml-build-verbatim "b") "=b="))

  (describe-many "object branch nodes"
    (org-ml--test-from-string nil
      (org-ml-build-bold "bold") "*bold*")
    (org-ml--test-from-string '(:type)
      (org-ml-build-footnote-reference "ref") "[fn::ref]")
    (org-ml--test-from-string nil
      (org-ml-build-italic "italic") "/italic/")
    (org-ml--test-from-string '(:raw-link :format)
      (org-ml-build-link "//example.com" :type "https") "https://example.com")
    (org-ml--test-from-string nil
      (org-ml-build-subscript "ss") "_ss")
    (org-ml--test-from-string nil
      (org-ml-build-superscript "ss") "^ss")
    (org-ml--test-from-string nil
      (org-ml-build-strike-through "s") "+s+")
    (org-ml--test-from-string nil
      (org-ml-build-table-cell "cell") " cell |")
    (org-ml--test-from-string nil
      (org-ml-build-underline "u") "_u_"))

  (describe-many "element leaf nodes"
    (org-ml--test-from-string '(:value)
      (org-ml-build-babel-call "name") "#+call: name()")
    (org-ml--test-from-string nil
      (org-ml-build-center-block) "#+begin_center\n#+end_center"
      (org-ml-build-center-block (org-ml-build-paragraph! "p")) "#+begin_center\np\n#+end_center")
    ;; TODO maybe I should figure out how to compare values robustly, but at
    ;; least this test demonstrates the string is parsed to a clock
    (org-ml--test-from-string '(:value)
      (org-ml-build-clock! '(2020 1 1 0 0)) "CLOCK: [2020-01-01 Tue 00:00]")
    (org-ml--test-from-string nil
      (org-ml-build-comment "comment") "# comment")
    (org-ml--test-from-string nil
      (org-ml-build-comment-block) "#+begin_comment\n#+end_comment"
      (org-ml-build-comment-block :value "p\n") "#+begin_comment\np\n#+end_comment")
    (org-ml--test-from-string nil
      (org-ml-build-diary-sexp :value '(print 'hi)) "%%(print 'hi)")
    (org-ml--test-from-string '(:value :retain-labels :use-labels)
      (org-ml-build-example-block) "#+begin_example\n#+end_example"
      (org-ml-build-example-block :value "v\n") "#+begin_example\nv\n#+end_example")
    (org-ml--test-from-string nil
      (org-ml-build-export-block "TYPE" "value\n") "#+begin_export TYPE\nvalue\n#+end_export")
    (org-ml--test-from-string nil
      (org-ml-build-fixed-width "val") ": val")
    (org-ml--test-from-string nil
      (org-ml-build-horizontal-rule) "------")
    (org-ml--test-from-string nil
      (org-ml-build-latex-environment '("env" "value")) "\\begin{env}\nvalue\n\\end{env}")
    (org-ml--test-from-string nil
      (org-ml-build-keyword "K" "v") "#+K: v")
    (org-ml--test-from-string nil
      (org-ml-build-special-block "type") "#+begin_type\n#+end_type")
    (org-ml--test-from-string '(:number-lines :retain-labels :use-labels :label-fmt)
      (org-ml-build-src-block :value "(print 'hi)\n") "#+begin_src\n(print 'hi)\n#+end_src")
    (org-ml--test-from-string nil
      (org-ml-build-node-property "KEY" "val") ":KEY: val")
    (org-ml--test-from-string '(:scheduled :deadline :closed)
      (org-ml-build-planning! :scheduled '(2020 1 1)) "SCHEDULED: [2020-01-01 Tue]"))

  (describe-many "element branch nodes"
    (org-ml--test-from-string nil
      (org-ml-build-drawer "DRAW") ":DRAW:\n:END:"
      (org-ml-build-drawer "DRAW" (org-ml-build-paragraph! "p")) ":DRAW:\np\n:END:")
    (org-ml--test-from-string nil
      (org-ml-build-dynamic-block "name") "#+begin: name\n#+end"
      (org-ml-build-dynamic-block "name" (org-ml-build-paragraph! "p")) "#+begin: name\np\n#+end")
    (org-ml--test-from-string nil
      (org-ml-build-footnote-definition "label" (org-ml-build-paragraph! "p")) "[fn:label] p")
    (org-ml--test-from-string '(:raw-value)
      (org-ml-build-headline! :title-text "headline") "* headline")
    (org-ml--test-from-string '(:structure)
      (org-ml-build-item! :paragraph "item") "- item")
    (org-ml--test-from-string nil
      (org-ml-build-paragraph! "para") "para"
      (org-ml-build-paragraph! "*para") "*para"
      (org-ml-build-paragraph) "")
    (org-ml--test-from-string '(:structure :type)
      (org-ml-build-plain-list (org-ml-build-item! :paragraph "item")) "- item")
    (org-ml--test-from-string nil
      (org-ml-build-section (org-ml-build-paragraph! "sec")) "sec"
      (org-ml-build-section (org-ml-build-paragraph! "*sec")) "*sec")
    (org-ml--test-from-string nil
      (org-ml-build-property-drawer) ":PROPERTIES:\n:END:"
      (org-ml-build-property-drawer! '(KEY val)) ":PROPERTIES:\n:KEY: val\n:END:")
    (org-ml--test-from-string nil
      (org-ml-build-quote-block) "#+begin_quote\n#+end_quote"
      (org-ml-build-quote-block (org-ml-build-paragraph! "p")) "#+begin_quote\np\n#+end_quote")
    (org-ml--test-from-string nil
      (org-ml-build-table! '("a")) "| a |"
      (org-ml-build-table) "|")
    (org-ml--test-from-string nil
      (org-ml-build-table-row! '("a")) "| a |"
      (org-ml-build-table-row-hline) "|---|"
      (org-ml-build-table-row) "|")
    (org-ml--test-from-string nil
      (org-ml-build-verse-block) "#+begin_verse\n#+end_verse"
      (org-ml-build-verse-block "hi\n") "#+begin_verse\nhi\n#+end_verse")))

;;; PARSING INVERTABILITY

;; For all org buffer contents, parsing and printing should be
;; perfect inverses.

;; These tests test/use the following:
;; - all the parse functions
;; - `org-ml-to-string'
;; - `org-ml-get-type'

(defun org-ml--test-contents-parse-inversion (type parse-fun contents-list
                                                   &optional prefix suffix)
  "Return form to test the parse/print inversion of CONTENTS-LIST.
Use PARSE-FUN to get the node tree from the contents. All should
be parsed to TYPE."
  (declare (indent 2))
  (let* ((contents-list (--map (if (consp it) (s-join "\n" it) it)
                               contents-list))
         (suffix-char (if (memq type org-ml-elements) "\n" " "))
         ;; TODO this little conditional implies that these nodes are invalid
         ;; if they have a space after them; not sure if this is actually true
         ;; (but if they have spaces after they don't pass this test)
         (contents-list-space (unless (memq type '(node-property plain-text line-break table-cell))
                                (--map (s-append suffix-char it) contents-list)))
         (test-list (append contents-list contents-list-space)))
    (--each test-list
      (let* ((at (if prefix (1+ (length prefix)) 1))
             (parsed (org-ml--with-org-env
                      (when prefix (insert prefix))
                      (insert it)
                      (when suffix (insert suffix))
                      (funcall parse-fun at)))
             (parsed-type (org-ml-get-type parsed)))
        ;; TODO not DRY
        (unless (equal type parsed-type)
          (print (format "%s parsed as %s" it parsed-type)))
        (should (equal type parsed-type))
        (should (equal it (org-ml-to-string parsed)))))))

(describe "parse and print should be perfect inverses"
  (describe "object nodes"
    (describe "leaves"
      (it "code"
        (org-ml--test-contents-parse-inversion 'code #'org-ml-parse-object-at
          (list "~code~")))

      (it "entity"
        (org-ml--test-contents-parse-inversion 'entity #'org-ml-parse-object-at
          (list "\\pi" "\\pi{}")))

      (it "export-snippet"
        (org-ml--test-contents-parse-inversion 'export-snippet #'org-ml-parse-object-at
          (list "@@x:y@@")))

      (it "inline-babel-call"
        (org-ml--test-contents-parse-inversion 'inline-babel-call #'org-ml-parse-object-at
          (list "call_ktulu()"
                "call_ktulu(n=1)"
                "call_ktulu[:x y]()"
                "call_ktulu[:x y](n=1)"
                "call_ktulu()[:a b]"
                "call_ktulu(n=1)[:a b]"
                "call_ktulu[:x y]()[:a b]"
                "call_ktulu[:x y](n=1)[:a b]")))

      (it "inline-src-block"
        (org-ml--test-contents-parse-inversion 'inline-src-block #'org-ml-parse-object-at
          (list "src_python{}"
                "src_python{print \"yo\"}"
                "src_python[:x y]{}"
                "src_python[:x y]{print \"yo\"}")))

      (it "line-break"
        (org-ml--test-contents-parse-inversion 'line-break #'org-ml-parse-object-at
          (list "\\\\\n")))

      (it "latex-fragment"
        (org-ml--test-contents-parse-inversion 'latex-fragment #'org-ml-parse-object-at
          (list "$2+2=5$")))

      (it "macro"
        (org-ml--test-contents-parse-inversion 'macro #'org-ml-parse-object-at
          (list "{{{key}}}"
                "{{{key(x=4)}}}")))

      (it "statistics-cookie"
        (org-ml--test-contents-parse-inversion 'statistics-cookie #'org-ml-parse-object-at
          (list "[/]"
                "[0/0]"
                "[%]"
                "[0%]")))

      (it "timestamp"
        (org-ml--test-contents-parse-inversion 'timestamp #'org-ml-parse-object-at
          (list "[2019-01-01 Tue]"
                "[2019-01-01 Tue 12:00]"
                ;; "[2019-01-01 Tue 12:00-13:00]" TODO this doesn't parse correctly
                "[2019-01-01 Tue]--[2019-01-02 Wed]"
                "<2019-01-01 Tue>"
                "[2019-01-01 Tue +1d]"
                "[2019-01-01 Tue -1y]"
                "[2019-01-01 Tue +1d -1y]")))

      (it "verbatim"
        (org-ml--test-contents-parse-inversion 'verbatim #'org-ml-parse-object-at
          (list "=verbatim=")))

      (it "plain-text"
        (org-ml--test-contents-parse-inversion 'plain-text #'org-ml-parse-object-at
          (list "plain-text"
                ;; all syntax chars by themselves should be plain-text
                "**" "~~" "@@:@@" "//" "[]" "[[]]" "{{{}}}" "<>" "<<>>"
                "<<<>>>" "++" "^" "_" "__" "=="))))

    (describe "branches"
      (it "bold"
        (org-ml--test-contents-parse-inversion 'bold #'org-ml-parse-object-at
          (list "*bold*")))

      (it "footnote-reference"
        (org-ml--test-contents-parse-inversion 'footnote-reference #'org-ml-parse-object-at
          (list "[fn:label]" "[fn:label:nodes]")
          " "))

      (it "italic"
        (org-ml--test-contents-parse-inversion 'italic #'org-ml-parse-object-at
          (list "/italic/")))

      (it "link"
        ;; ignore the value of `org-link-abbrev-alist'
        (let ((org-link-abbrev-alist '(("test" . "fail"))))
          (org-ml--test-contents-parse-inversion 'link #'org-ml-parse-object-at
            ;; this is not exhaustive but hopefully good enough
            (list "https://downloadmoreram.com"
                  "mailto:vladimirputin@pwned.ru"
                  "file:/home/kalilinux/pwneddata"
                  "<https://downloadmoreram.com>"
                  "[[test:foo]]"
                  "[[https://downloadmoreram.com]]"
                  "[[https://downloadmoreram.com][legit advice]]"))))

      (it "radio-target"
        (org-ml--test-contents-parse-inversion 'radio-target #'org-ml-parse-object-at
          (list "<<<radio>>>")))

      (it "strike-through"
        (org-ml--test-contents-parse-inversion 'strike-through #'org-ml-parse-object-at
          (list "+strike+")))

      (it "subscript"
        (org-ml--test-contents-parse-inversion 'subscript #'org-ml-parse-object-at
          (list "_sub" "_{sub}")
          "dummy"))

      (it "superscript"
        (org-ml--test-contents-parse-inversion 'superscript #'org-ml-parse-object-at
          (list "^super" "^{super}")
          "dummy"))

      (it "table-cell"
        (org-ml--test-contents-parse-inversion 'table-cell #'org-ml-parse-object-at
          (list " cell |")
          "|"))))

  (describe "element nodes"
    (describe "leaves"
      (it "babel-call"
        (org-ml--test-contents-parse-inversion 'babel-call #'org-ml-parse-element-at
          (list "#+call: name()\n"
                "#+call: name(x=1)\n"
                "#+call: name[:x y](x=1)\n"
                "#+call: name[:x y]()\n"
                "#+call: name[:x y](x=1) :a b\n"
                "#+call: name[:x y]() :a b\n"
                "#+call: name[]() :a b\n")))

      (it "clock"
        (org-ml--test-contents-parse-inversion 'clock #'org-ml-parse-element-at
          (list "CLOCK: [2019-01-01 Tue]\n"
                "CLOCK: [2019-01-01 Tue]--[2019-01-02 Wed] => 24:00\n"
                ;; TODO this doesn't work
                ;; "CLOCK: [2019-01-01 Tue 00:00-01:00] =>  1:00\n"
                )))

      (it "comment"
        (org-ml--test-contents-parse-inversion 'comment #'org-ml-parse-element-at
          (list "# one\n"
                '("# one"
                  "# two\n")
                ;; TODO this doesn't work
                ;; "#\n"
                )))

      (it "comment-block"
        (org-ml--test-contents-parse-inversion 'comment-block #'org-ml-parse-element-at
          (list '("#+begin_comment"
                  "battle of being"
                  "#+end_comment\n")
                '("#+begin_comment"
                  "#+end_comment\n"))))

      (it "diary-sexp"
        (org-ml--test-contents-parse-inversion 'diary-sexp #'org-ml-parse-element-at
          (list "%%()\n" "%%(whatever)\n")))

      (it "example-block"
        (org-ml--test-contents-parse-inversion 'example-block #'org-ml-parse-element-at
          (list '("#+begin_example"
                  "  example.com"
                  "#+end_example\n")
                '("#+begin_example"
                  "#+end_example\n"))))

      (it "export-block"
        (org-ml--test-contents-parse-inversion 'export-block #'org-ml-parse-element-at
          (list '("#+begin_export PLAIN"
                  "bullet, bombs, bigotry"
                  "#+end_export\n")
                ;; TODO type needs to always be uppercase?
                ;; '("#+BEGIN_EXPORT plain"
                ;;   "#+END_EXPORT\n")
                '("#+begin_export PLAIN"
                  "#+end_export\n"))))

      ;; ;; TODO this will randomly insert a blank after it is parsed
      ;; (it "fixed-width"
      ;;   (org-ml--test-contents-parse-inversion 'fixed-width #'org-ml-parse-element-at
      ;;     (list ": crucifixed"
      ;;           ;; TODO this make a blank
      ;;           ;; ":\n"
      ;;           )))

      (it "horizontal-rule"
        (org-ml--test-contents-parse-inversion 'horizontal-rule #'org-ml-parse-element-at
          (list "-----\n")))

      (it "keyword"
        (org-ml--test-contents-parse-inversion 'keyword #'org-ml-parse-element-at
          (list "#+key: val\n"
                ;; TODO this randomly fails
                ;; "#+KEY:\n"
                "#+key: \n")))

      (it "latex-environment"
        (org-ml--test-contents-parse-inversion 'latex-environment #'org-ml-parse-element-at
          (list '("\\begin{env}"
                  "\\end{env}\n")
                '("\\begin{env}"
                  "latex >>> ms word"
                  "\\end{env}\n"))))

      (it "node-property"
        (org-ml--test-contents-parse-inversion 'node-property #'org-ml-parse-element-at
          (list ":node:     prop\n"
                ;; TODO this seems arbitrary
                ;; ":node\n"
                ":node:     \n")
          "* dummy\n:PROPERTIES:\n"
          ":END:\n"))

      (it "planning"
        (org-ml--test-contents-parse-inversion 'planning #'org-ml-parse-element-at
          (list "CLOSED: <2019-01-01 Tue>\n"
                "CLOSED: <2019-01-01 Tue +1d>\n"
                "CLOSED: <2019-01-01 Tue -1y>\n"
                "CLOSED: <2019-01-01 Tue +1d -1y>\n")
          "* dummy\n"))

      (it "src-block"
        (org-ml--test-contents-parse-inversion 'src-block #'org-ml-parse-element-at
          (list '("#+begin_src"
                  "#+end_src\n")
                ;; TODO this doesn't work if is isn't indented
                '("#+begin_src python -n :x y"
                  "  print \"yo\""
                  "#+end_src\n")))))

    (describe "branches (object node children)"
      (it "paragraph"
        (org-ml--test-contents-parse-inversion 'paragraph #'org-ml-parse-element-at
          ;; TODO there are probably other things I could put here
          (list "paragraph\n")))

      (it "table-row"
        (org-ml--test-contents-parse-inversion 'table-row #'org-ml-parse-table-row-at
          (list "| cell |\n"
                ;; TODO this makes an empty string
                ;; "| |\n"
                )))

      (it "verse-block"
        (org-ml--test-contents-parse-inversion 'verse-block #'org-ml-parse-element-at
          (list '("#+begin_verse"
                  "#+end_verse\n")
                '("#+begin_verse"
                  "Once upon a midnight dreary..."
                  "#+end_verse\n")))))

    (describe "branches (element node children)"
      (it "center-block"
        (org-ml--test-contents-parse-inversion 'center-block #'org-ml-parse-element-at
          (list '("#+begin_center"
                  "#+end_center\n")
                '("#+begin_center"
                  "Of the universe..."
                  "#+end_center\n"))))

      (it "drawer"
        (org-ml--test-contents-parse-inversion 'drawer #'org-ml-parse-element-at
          (list '(":LOGBOOK:"
                  ":END:\n")
                '(":LOGBOOK:"
                  "- logged thingy"
                  ":END:\n"))))

      (it "dynamic-block"
        (org-ml--test-contents-parse-inversion 'dynamic-block #'org-ml-parse-element-at
          (list '("#+begin: name"
                  "#+end:\n")
                '("#+begin: name"
                  "Random contents..."
                  "#+end:\n"))))

      (it "footnote-definition"
        ;; TODO blanks are apparently not allowed and will error
        (org-ml--test-contents-parse-inversion 'footnote-definition #'org-ml-parse-element-at
          (list ;; "[fn:label] \n"
           ;; TODO needs a random space at the end
           ;; "[fn:label]"
           "[fn:label] stuff after\n"
           )))

      (it "headline"
        (org-ml--test-contents-parse-inversion 'headline #'org-ml-parse-element-at
          ;; this is not exhaustive...
          (list "* dummy\n"
                "** dummy\n"
                "* COMMENT dummy\n"
                "* TODO COMMENT dummy\n"
                "* TODO dummy\n"
                "* TODO [#A] dummy\n"
                ;; TODO priority is supposed to be on the other side
                ;; "* TODO [#A] COMMENT dummy\n"
                ;; "* [#A] COMMENT dummy\n"
                )))

      (it "item"
        (org-ml--test-contents-parse-inversion 'item #'org-ml-parse-item-at
          ;; this is not exhaustive...
          ;; TODO not sure why these have two newlines
          (list "- \n\n"
                "1. \n\n"
                ;; TODO these don't work
                ;; "+ \n\n"
                ;; "1) \n\n"
                "- thing\n"
                "- tagged :: thing\n"
                "1. [@20] thing\n"
                )))

      (it "plain-list"
        (org-ml--test-contents-parse-inversion 'plain-list #'org-ml-parse-element-at
          (list "- thing\n"
                "1. thing\n"
                '("- thing"
                  "- more thing\n"))))


      (it "property-drawer"
        (org-ml--test-contents-parse-inversion 'property-drawer #'org-ml-parse-element-at
          (list '(":PROPERTIES:"
                  ":END:\n")
                '(":PROPERTIES:"
                  ":Effort:   0:30"
                  ":END:\n"))
          "* dummy\n"))

      (it "quote-block"
        (org-ml--test-contents-parse-inversion 'quote-block #'org-ml-parse-element-at
          (list '("#+begin_quote"
                  "#+end_quote\n")
                '("#+begin_quote"
                  "Fear is the mind killer..."
                  "#+end_quote\n"))))

      (it "section"
        (org-ml--test-contents-parse-inversion 'section #'org-ml-parse-section-at
          (list "things that could be a paragraph\n"
                "#+key: val\n"
                "# nothing important...\n")))

      (it "special-block"
        (org-ml--test-contents-parse-inversion 'special-block #'org-ml-parse-element-at
          (list '("#+begin_special"
                  "#+end_special\n")
                '("#+begin_special"
                  "You don't belong here"
                  "#+end_special\n"))))

      (it "table"
        (org-ml--test-contents-parse-inversion 'table #'org-ml-parse-element-at
          (list "| simple |\n"
                "| less | simple |\n"
                '("| R | A |"
                  "| G | E |\n")
                ;; TODO this makes a blank string
                ;; "| |\n"
                ))))))

;;; NODE PROPERTY COMPLETENESS

(defun should-have-equal-properties (e1 e2)
  (unless (eq (org-ml-get-type e1) (org-ml-get-type e2))
    (error "Type mismatch: %s\n\n%s" e1 e2))
  (cl-flet
      ((plist-get-keys
        (plist)
        (let ((keys (-slice plist 0 nil 2)))
          (if (org-ml-is-any-type org-ml--element-nodes-with-affiliated e1)
              (-difference keys '(:name :plot :header :results :caption))
            keys))))
    (let ((p1 (plist-get-keys (nth 1 e1)))
          (p2 (plist-get-keys (nth 1 e2))))
      (expect (-difference p1 p2) :not :to-be-truthy)
      (expect (-difference p2 p1) :not :to-be-truthy))))

(defun org-ml--compare-object-props (elem string)
  (should-have-equal-properties
   elem
   (->> (org-ml--from-string (concat " " string))
        (org-ml--get-descendent '(0 1)))))

(defun org-ml--compare-element-props (elem string)
  (should-have-equal-properties
   elem
   (->> (org-ml--from-string string)
        (org-ml--get-descendent '(0)))))

(describe "ensure builders include all properties"
  (describe "object nodes"
    (describe "leaves"
      (it "org-ml--code"
        (org-ml--compare-object-props
         (org-ml-build-code "value") "~code~"))

      (it "org-ml--entity"
        (org-ml--compare-object-props
         (org-ml-build-entity "pi") "\\pi"))

      (it "org-ml--export-snippet"
        (org-ml--compare-object-props
         (org-ml-build-export-snippet "backend" "value") "@@im:padme@@"))

      (it "org-ml--inline-babel-call"
        (org-ml--compare-object-props
         (org-ml-build-inline-babel-call "name") "call_name()"))

      (it "org-ml--inline-src-block"
        (org-ml--compare-object-props
         (org-ml-build-inline-src-block "lang") "src_lang{value}"))

      ;; TODO add latex fragment

      (it "org-ml--line-break"
        (org-ml--compare-object-props
         (org-ml-build-line-break) "\\\\\n"))

      (it "org-ml--macro"
        (org-ml--compare-object-props
         (org-ml-build-macro "value") "{{{value}}}"))

      (it "org-ml--statistics-cookie"
        (org-ml--compare-object-props
         (org-ml-build-statistics-cookie '(1)) "[/]"))

      (it "org-ml--target"
        (org-ml--compare-object-props
         (org-ml-build-target "value") "<<value>>"))

      (it "org-ml--timestamp"
        (org-ml--compare-object-props
         (org-ml-build-timestamp! '(2019 1 1))
         ;; TODO the timestamp parser does not add properties for warnings
         ;; or repeaters if they are not given, this appears to be a bug
         "[2019-01-01 Tue +1d -1d]"))

      (it "org-ml--verbatim"
        (org-ml--compare-object-props
         (org-ml-build-verbatim "value") "=value=")))

    (describe "branches"
      (it "org-ml--bold"
        (org-ml--compare-object-props
         (org-ml-build-bold) "*bold*"))

      (it "org-ml--footnote-reference"
        (org-ml--compare-object-props
         (org-ml-build-footnote-reference) "[fn:1]"))

      (it "org-ml--italic"
        (org-ml--compare-object-props
         (org-ml-build-italic) "/italic/"))

      (it "org-ml--link"
        (org-ml--compare-object-props
         (org-ml-build-link "path") "[[path]]"))

      (it "org-ml--radio-target"
        (org-ml--compare-object-props
         (org-ml-build-radio-target) "<<<target>>>"))

      (it "org-ml--strike-through"
        (org-ml--compare-object-props
         (org-ml-build-strike-through) "+bad+"))

      (it "org-ml--superscript"
        (should-have-equal-properties
         (org-ml-build-superscript)
         (->> (org-ml--from-string "thisis^super")
              (org-ml--get-descendent '(0 1)))))

      (it "org-ml--subscript"
        (should-have-equal-properties
         (org-ml-build-subscript)
         (->> (org-ml--from-string "thisis_subpar")
              (org-ml--get-descendent '(0 1)))))

      (it "org-ml--table-cell"
        (should-have-equal-properties
         (org-ml-build-table-cell "cell")
         (->> (org-ml--from-string "| cell |")
              (org-ml--get-descendent '(0 0 0)))))

      (it "org-ml--underline"
        (org-ml--compare-object-props
         (org-ml-build-underline) "_bad_"))))

  (describe "element nodes"
    (describe "leaves"
      (it "org-ml--babel-call"
        (org-ml--compare-element-props
         (org-ml-build-babel-call "call") "#+call: name()"))

      (it "org-ml--clock"
        (org-ml--compare-element-props
         (org-ml-build-clock (org-ml-build-timestamp! '(2019 1 1)))
         "CLOCK: [2019-01-01 Tue]"))

      (it "org-ml--comment"
        (org-ml--compare-element-props
         (org-ml-build-comment "useless") "# useless"))

      (it "org-ml--comment-block"
        (org-ml--compare-element-props
         (org-ml-build-comment-block)
         "#+begin_comment\nuseless\n#+end_comment"))

      (it "org-ml--diary-sexp"
        (org-ml--compare-element-props
         (org-ml-build-diary-sexp) "%%()"))

      (it "org-ml--example-block"
        (org-ml--compare-element-props
         (org-ml-build-example-block)
         "#+begin_example\nuseless\n#+end_example"))

      (it "org-ml--export-block"
        (org-ml--compare-element-props
         (org-ml-build-export-block "type" "value")
         "#+begin_export type\nuseless\n#+end_export"))

      (it "org-ml--fixed-width"
        (org-ml--compare-element-props
         (org-ml-build-fixed-width "value") ": value"))

      (it "org-ml--horizontal-rule"
        (org-ml--compare-element-props
         (org-ml-build-horizontal-rule) "-----"))

      (it "org-ml--keyword"
        (org-ml--compare-element-props
         (org-ml-build-keyword "key" "val") "#+KEY: val"))

      (it "org-ml--latex-environment"
        (org-ml--compare-element-props
         (org-ml-build-latex-environment '("gloves" "text"))
         "\\begin{env}\nvalue\n\\end{env}"))

      (it "org-ml--node-property"
        (should-have-equal-properties
         (org-ml-build-node-property "key" "value")
         (->> (org-ml--from-string "* dummy\n:PROPERTIES:\n:key: val\n:END:")
              (org-ml--get-descendent '(0 0 0)))))

      (it "org-ml--planning"
        (should-have-equal-properties
         (org-ml-build-planning :closed (org-ml-build-timestamp! '(2019 1 1) :active nil))
         (->> (org-ml--from-string "* dummy\nCLOSED: <2019-01-01 Tue>")
              (org-ml--get-descendent '(0 0)))))

      (it "org-ml--src-block"
        (org-ml--compare-element-props
         (org-ml-build-src-block)
         "#+begin_src\nuseless\n#+end_src")))

    (describe "branches"
      (it "org-ml--paragraph"
        (should-have-equal-properties
         (org-ml-build-paragraph)
         (->> (org-ml--from-string "text")
              (org-ml--get-descendent '(0)))))

      (it "org-ml--table-row"
        (should-have-equal-properties
         (org-ml-build-table-row)
         (->> (org-ml--from-string "| row |")
              (org-ml--get-descendent '(0 0)))))

      (it "org-ml--verse-block"
        (should-have-equal-properties
         (org-ml-build-verse-block)
         (->> (org-ml--from-string "#+begin_verse\nthing\n#+end_verse")
              (org-ml--get-descendent '(0)))))

      (it "org-ml--center-block"
        (org-ml--compare-element-props
         (org-ml-build-center-block)
         "#+begin_center\nuseless\n#+end_center"))

      (it "org-ml--drawer"
        (org-ml--compare-element-props
         (org-ml-build-drawer "name")
         ":LOGBOOK:\nuseless\n:END:"))

      (it "org-ml--dynamic-block"
        (org-ml--compare-element-props
         (org-ml-build-dynamic-block "name" :arguments '(:key val))
         "#+begin: name args\nuseless\n#+end:"))

      (it "org-ml--footnote-definition"
        (org-ml--compare-element-props
         (org-ml-build-footnote-definition "label") "[fn:label]\n"))

      (it "org-ml--headline"
        (should-have-equal-properties
         (org-ml-build-headline)
         (org-ml--from-string "* head")))

      (it "org-ml--item"
        (should-have-equal-properties
         (org-ml-build-item)
         (->> (org-ml--from-string "- head")
              (org-ml--get-descendent '(0 0)))))

      (it "org-ml--plain-list"
        (org-ml--compare-element-props
         (org-ml-build-plain-list) "- item"))

      (it "org-ml--property-drawer"
        (should-have-equal-properties
         (org-ml-build-property-drawer)
         (->> (org-ml--from-string "* dummy\n:PROPERTIES:\n:END:")
              (org-ml--get-descendent '(0 0)))))

      (it "org-ml--quote-block"
        (org-ml--compare-element-props
         (org-ml-build-quote-block)
         "#+begin_quote\n#+end_quote"))

      (it "org-ml--section"
        (org-ml--compare-element-props
         (org-ml-build-section) "* dummy\nstuff"))

      (it "org-ml--special-block"
        (org-ml--compare-element-props
         (org-ml-build-special-block "type")
         "#+begin_type:\n#+end_type:"))

      (it "org-ml--table"
        (org-ml--compare-element-props
         (org-ml-build-table) "| table |")))))

;; SPECIALIZED DEFUN MACRO TESTS

(describe "org-ml--defun-kw internal definition"
  (describe "org-ml--make-header"
    (it "make header"
      (expect (org-ml--make-header '("docstring" (print 'hi)) nil)
              :to-equal
              "docstring\n\n(fn)")
      (expect (org-ml--make-header '("docstring" (print 'hi)) '(one))
              :to-equal
              "docstring\n\n(fn ONE)")
      (expect (org-ml--make-header '("docstring" (print 'hi)) '(one two))
              :to-equal
              "docstring\n\n(fn ONE TWO)")))

  (describe "org-ml--make-kwarg-let/error"
    (it "list too long"
      (should-error (org-ml--make-kwarg-let '(one two three))))
    (it "keyword slot must be a real keyword"
      (should-error (org-ml--make-kwarg-let '((one two))))
      (should-error (org-ml--make-kwarg-let '((one two) three))))
    (it "single arg must be a symbol but not a keyword"
      ;; TODO the keyword guard does not work yet
      ;; (should-error (org-ml--make-kwarg-let :one))
      (should-error (org-ml--make-kwarg-let 1))
      (should-error (org-ml--make-kwarg-let "one"))
      (should-error (org-ml--make-kwarg-let '(1)))))

  (describe "org-ml--make-rest-partition-form"
    (describe "valid restargs"
      (it "single arg"
        (expect '(nil . (one)) :to-equal
                (org-ml--make-rest-partition-form '(one) nil t)))
      (it "multiple args"
        (expect '(nil . (one two)) :to-equal
                (org-ml--make-rest-partition-form '(one two) nil t))))

    (describe "error"
      (it "invalid keywords"
        (should-error (org-ml--make-rest-partition-form '(:one one) '(:two) nil)))
      (it "too many arguments"
        (should-error (org-ml--make-rest-partition-form '(:one one two) (:one) nil)))
      (it "multiple keywords"
        (should-error (org-ml--make-rest-partition-form '(:one one :one three two) (:one) nil))))))

;;; SUPERCONTENTS FRAMEWORK TESTING

(defun org-ml--test-merge-logbook-valid (config output items clocks)
  `(expect (org-ml--merge-logbook ,config ,items ,clocks)
           :to-equal ,output))

(defun org-ml--test-merge-logbook-error (config items clocks)
  `(should-error (org-ml--merge-logbook ,config ,items ,clocks)))

(defmacro org-ml--test-merge-logbook-specs (config &rest specs)
  (declare (indent 1))
  (let ((forms
         (->> (-partition 4 specs)
              (--map
               (-let (((title output items clocks) it))
                 `(it ,title
                    ,(if (eq output 'error)
                         (org-ml--test-merge-logbook-error config items clocks)
                       (org-ml--test-merge-logbook-valid config output items clocks))))))))
    `(progn ,@forms)))

(describe "org-ml--merge-logbook"
  (before-all
    (setq enconf (org-ml--scc-encode nil)
          enconf-notes (org-ml--scc-encode '(:clock-out-notes t))
          c1 (org-ml-build-clock! '(2020 1 1 0 0) :end '(2020 1 1 1 0))
          i1 (org-ml-build-log-note (org-ml-time-to-unixtime '(2020 1 2 0 0)) "1")
          c2 (org-ml-build-clock! '(2020 1 3 0 0) :end '(2020 1 3 1 0))
          i2 (org-ml-build-log-note (org-ml-time-to-unixtime '(2020 1 4 0 0)) "2")
          n1 (org-ml-build-item! :paragraph "clock note")
          p1 (org-ml-build-plain-list i1)
          p2 (org-ml-build-plain-list i2)
          pn1 (org-ml-build-plain-list n1)
          p1n1 (org-ml-build-plain-list n1 i1)
          ;; there should never be a p12 analogue since that's not the right order
          p21 (org-ml-build-plain-list i2 i1)
          x1 (org-ml-build-code "I should cause a fatal error")))
  (describe "without clock notes"
    (org-ml--test-merge-logbook-specs enconf
      "nothing" nil nil nil
      "just clocks" `(,c2 ,c1) nil `(,c1 ,c2)
      "just items" `(,p21) `(,i1 ,i2) nil
      "single clock and item" `(,p1 ,c1) `(,i1) `(,c1)
      "clocks and items" `(,p2 ,c2 ,p1 ,c1) `(,i1 ,i2) `(,c1 ,c2)
      "just clocks (note)" error nil `(,c1 ,n1 ,c2)
      "single clock (note) and item" error `(,i1) `(,c1 ,n1)
      "clocks (note) and items" error `(,i1 ,i2) `(,c1 ,n1 ,c2)
      "just clocks (note in wrong place)" error nil `(,n1 ,c1 ,c2)
      "just garbage (items)" error `(,x1) nil
      "just garbage (clocks)" error nil `(,x1)))
  
  (describe "with clock notes"
    (org-ml--test-merge-logbook-specs enconf-notes
      "nothing" nil nil nil
      "just clocks" `(,c2 ,c1) nil `(,c1 ,c2)
      "just items" `(,p21) `(,i1 ,i2) nil
      "single clock and item" `(,p1 ,c1) `(,i1) `(,c1)
      "clocks and items" `(,p2 ,c2 ,p1 ,c1) `(,i1 ,i2) `(,c1 ,c2)
      "just clocks (note)" `(,c2 ,c1 ,pn1) nil `(,c1 ,n1 ,c2)
      "single clock (note) and item" `(,p1 ,c1 ,pn1) `(,i1) `(,c1 ,n1)
      "clocks (note) and items" `(,p2 ,c2 ,p1 ,c1 ,pn1) `(,i1 ,i2) `(,c1 ,n1 ,c2)
      "clocks (note) and items (different order)" `(,p2 ,c2 ,p1n1 ,c1) `(,i1 ,i2) `(,c1 ,c2 ,n1)
      "just clocks (note in wrong place)" error nil `(,n1 ,c1 ,c2)
      "just garbage (items)" error `(,x1) nil
      "just garbage (clocks)" error nil `(,x1))))

(defmacro expect-separated (c m items clocks unknown in)
  `(-let (((&alist 'items i 'clocks c 'unknown u)
           (-group-by #'car (org-ml--separate-logbook ,c ,m ,in))))
     (expect (list ,items ,clocks ,unknown)
             :to-equal
             (list (-map #'cdr i)
                   (-map #'cdr c)
                   (-map #'cdr u)))))

(defmacro org-ml--test-separate-logbook-specs (config mode &rest specs)
  (declare (indent 2))
  (let ((forms
         (->> (-partition 5 specs)
              (--map
               (-let (((title items clocks unknown input) it))
                 `(it ,title (expect-separated ,config ,mode ,items ,clocks ,unknown ,input)))))))
    `(progn ,@forms)))

(describe "org-ml--separate-logbook"
  (before-all
    (setq enconf (org-ml--scc-encode nil)
          enconf-notes (org-ml--scc-encode '(:clock-out-notes t))
          c1 (org-ml-build-clock! '(2020 1 1 0 0) :end '(2020 1 1 1 0))
          i1 (org-ml-build-log-note (org-ml-time-to-unixtime '(2020 1 2 0 0)) "1")
          c2 (org-ml-build-clock! '(2020 1 3 0 0) :end '(2020 1 3 1 0))
          i2 (org-ml-build-log-note (org-ml-time-to-unixtime '(2020 1 4 0 0)) "2")
          n1 (org-ml-build-item! :paragraph "clock note")
          p1 (org-ml-build-plain-list i1)
          p2 (org-ml-build-plain-list i2)
          pn1 (org-ml-build-plain-list n1)
          pn11 (org-ml-build-plain-list n1 i1)
          p1n1 (org-ml-build-plain-list i1 n1)
          p12 (org-ml-build-plain-list i1 i2)
          x1 (org-ml-build-code "I should cause a fatal error")))

  (describe "mixed mode"
    (describe "without clock notes"
      (org-ml--test-separate-logbook-specs enconf :mixed
        "nothing" nil nil nil nil
        "single item" `(,i1) nil nil `(,p1)
        "single clock" nil `(,c1) nil `(,c1)
        "single garbage entry" nil nil `(,x1) `(,x1)
        "single item and clock" `(,i1) `(,c1) nil `(,p1 ,c1)
        "single item and garbage" `(,i1) nil `(,x1) `(,p1 ,x1)
        "single clock and garbage" nil `(,c1) `(,x1) `(,c1 ,x1)
        "single item, clock, and garbage" `(,i1) `(,c1) `(,x1) `(,p1 ,c1 ,x1)
        "multiple items and clocks" `(,i2 ,i1) `(,c2 ,c1) nil `(,p12 ,c1 ,c2)
        "multiple items and clocks (interlaced)" `(,i2 ,i1) `(,c2 ,c1) nil `(,p1 ,c1 ,p2 ,c2)
        "clock with note" nil `(,c1) `(,n1) `(,c1 ,pn1)
        "clock with note in wrong place" nil `(,c1) `(,n1) `(,pn1 ,c1)
        "clock with note and item" `(,i1) `(,c1) `(,n1) `(,c1 ,pn11)
        "clock with item and note" `(,i1) `(,c1) `(,n1) `(,c1 ,p1n1)))
    (describe "clock notes"
      (org-ml--test-separate-logbook-specs enconf-notes :mixed
        "nothing" nil nil nil nil
        "single item" `(,i1) nil nil `(,p1)
        "single clock" nil `(,c1) nil `(,c1)
        "single garbage entry" nil nil `(,x1) `(,x1)
        "single item and clock" `(,i1) `(,c1) nil `(,p1 ,c1)
        "single item and garbage" `(,i1) nil `(,x1) `(,p1 ,x1)
        "single clock and garbage" nil `(,c1) `(,x1) `(,c1 ,x1)
        "single item, clock, and garbage" `(,i1) `(,c1) `(,x1) `(,p1 ,c1 ,x1)
        "multiple items and clocks" `(,i2 ,i1) `(,c2 ,c1) nil `(,p12 ,c1 ,c2)
        "multiple items and clocks (interlaced)" `(,i2 ,i1) `(,c2 ,c1) nil `(,p1 ,c1 ,p2 ,c2)
        "clock with note" nil `(,n1 ,c1) nil `(,c1 ,pn1)
        "clock with note in wrong place" nil `(,c1) `(,n1) `(,pn1 ,c1)
        "clock with note and item" `(,i1) `(,n1 ,c1) nil `(,c1 ,pn11)
        "clock with item and note" `(,i1) `(,c1) `(,n1) `(,c1 ,p1n1))))

  (describe "items mode"
    (describe "without clock notes"
      (org-ml--test-separate-logbook-specs enconf :items
        "nothing" nil nil nil nil
        "single item" `(,i1) nil nil `(,p1)
        "single clock" nil nil `(,c1) `(,c1)
        "single garbage entry" nil nil `(,x1) `(,x1)
        "single item and clock" `(,i1) nil `(,c1) `(,p1 ,c1)
        "single item and garbage" `(,i1) nil `(,x1) `(,p1 ,x1)
        "single clock and garbage" nil nil `(,x1 ,c1) `(,c1 ,x1)
        "single item, clock, and garbage" `(,i1) nil `(,x1 ,c1) `(,p1 ,c1 ,x1)
        "multiple items and clocks" `(,i2 ,i1) nil `(,c2 ,c1) `(,p12 ,c1 ,c2)
        "multiple items and clocks (interlaced)" `(,i2 ,i1) nil `(,c2 ,c1) `(,p1 ,c1 ,p2 ,c2)
        "clock with note" nil nil `(,n1 ,c1) `(,c1 ,pn1)
        "clock with note in wrong place" nil nil `(,c1 ,n1) `(,pn1 ,c1)
        "clock with note and item" `(,i1) nil `(,n1 ,c1) `(,c1 ,pn11)
        "clock with item and note" `(,i1) nil `(,n1 ,c1) `(,c1 ,p1n1)))
    (describe "with clock notes"
      (org-ml--test-separate-logbook-specs enconf-notes :items
        "nothing" nil nil nil nil
        "single item" `(,i1) nil nil `(,p1)
        "single clock" nil nil `(,c1) `(,c1)
        "single garbage entry" nil nil `(,x1) `(,x1)
        "single item and clock" `(,i1) nil `(,c1) `(,p1 ,c1)
        "single item and garbage" `(,i1) nil `(,x1) `(,p1 ,x1)
        "single clock and garbage" nil nil `(,x1 ,c1) `(,c1 ,x1)
        "single item, clock, and garbage" `(,i1) nil `(,x1 ,c1) `(,p1 ,c1 ,x1)
        "multiple items and clocks" `(,i2 ,i1) nil `(,c2 ,c1) `(,p12 ,c1 ,c2)
        "multiple items and clocks (interlaced)" `(,i2 ,i1) nil `(,c2 ,c1) `(,p1 ,c1 ,p2 ,c2)
        "clock with note" nil nil `(,n1 ,c1) `(,c1 ,pn1)
        "clock with note in wrong place" nil nil `(,c1 ,n1) `(,pn1 ,c1)
        "clock with note and item" `(,i1) nil `(,n1 ,c1) `(,c1 ,pn11)
        "clock with item and note" `(,i1) nil `(,n1 ,c1) `(,c1 ,p1n1))))

  (describe "clocks mode"
    (describe "without clock notes"
      (org-ml--test-separate-logbook-specs enconf :clocks
        "nothing" nil nil nil nil
        "single item" nil nil `(,i1) `(,p1)
        "single clock" nil `(,c1) nil `(,c1)
        "single garbage entry" nil nil `(,x1) `(,x1)
        "single item and clock" nil `(,c1) `(,i1) `(,p1 ,c1)
        "single item and garbage" nil nil `(,x1 ,i1) `(,p1 ,x1)
        "single clock and garbage" nil `(,c1) `(,x1) `(,c1 ,x1)
        "single item, clock, and garbage" nil `(,c1) `(,x1 ,i1) `(,p1 ,c1 ,x1)
        "multiple items and clocks" nil `(,c2 ,c1) `(,i2 ,i1) `(,p12 ,c1 ,c2)
        "multiple items and clocks (interlaced)" nil `(,c2 ,c1) `(,i2 ,i1) `(,p1 ,c1 ,p2 ,c2)
        "clock with note" nil `(,c1) `(,n1) `(,c1 ,pn1)
        "clock with note in wrong place" nil `(,c1) `(,n1) `(,pn1 ,c1)
        "clock with note and item" nil `(,c1) `(,i1 ,n1) `(,c1 ,pn11)
        "clock with item and note" nil `(,c1) `(,n1 ,i1) `(,c1 ,p1n1)))
    (describe "with clock notes"
      (org-ml--test-separate-logbook-specs enconf-notes :clocks
        "nothing" nil nil nil nil
        "single item" nil nil `(,i1) `(,p1)
        "single clock" nil `(,c1) nil `(,c1)
        "single garbage entry" nil nil `(,x1) `(,x1)
        "single item and clock" nil `(,c1) `(,i1) `(,p1 ,c1)
        "single item and garbage" nil nil `(,x1 ,i1) `(,p1 ,x1)
        "single clock and garbage" nil `(,c1) `(,x1) `(,c1 ,x1)
        "single item, clock, and garbage" nil `(,c1) `(,x1 ,i1) `(,p1 ,c1 ,x1)
        "multiple items and clocks" nil `(,c2 ,c1) `(,i2 ,i1) `(,p12 ,c1 ,c2)
        "multiple items and clocks (interlaced)" nil `(,c2 ,c1) `(,i2 ,i1) `(,p1 ,c1 ,p2 ,c2)
        "clock with note" nil `(,n1 ,c1) nil `(,c1 ,pn1)
        "clock with note in wrong place" nil `(,c1) `(,n1) `(,pn1 ,c1)
        "clock with note and item" nil `(,n1 ,c1) `(,i1) `(,c1 ,pn11)
        "clock with item and note" nil `(,c1) `(,n1 ,i1) `(,c1 ,p1n1)))))

(defmacro org-ml--test-logbook-to-nodes (c out items clocks)
  `(->> (org-ml--logbook-init ,items ,clocks nil 0)
        (org-ml--logbook-to-nodes ,c)
        (equal ,out)
        (should)))

(defmacro org-ml--test-logbook-to-nodes-specs (&rest specs)
  (let ((forms
         (->> (-partition 5 specs)
              (--map
               (-let (((title config output items clocks) it))
                 `(it ,title (org-ml--test-logbook-to-nodes ,config ,output ,items ,clocks)))))))
    `(progn ,@forms)))

(describe "logbook to nodes"
  (before-all
    (setq i-name "LOGGING"
          c-name "CLOCKING"
          m-name "LOGBOOK"
          c1 (org-ml-build-clock! '(2020 1 1 0 0) :end '(2020 1 1 1 0))
          i1 (org-ml-build-log-note (org-ml-time-to-unixtime '(2020 1 2 0 0)) "1")
          c2 (org-ml-build-clock! '(2020 1 3 0 0) :end '(2020 1 3 1 0))
          i2 (org-ml-build-log-note (org-ml-time-to-unixtime '(2020 1 4 0 0)) "2")
          p1 (org-ml-build-plain-list i1)
          p2 (org-ml-build-plain-list i2)
          p21 (org-ml-build-plain-list i2 i1)
          di (org-ml-build-drawer i-name p21)
          di* (org-ml-build-drawer m-name p21)
          dc (org-ml-build-drawer c-name c2 c1)
          dc* (org-ml-build-drawer m-name c2 c1)
          dm (org-ml-build-drawer m-name p2 c2 p1 c1)))
  ;; ASSUME the sorting function takes care of clock notes, and since
  ;; everything passes through that, don't test it here
  (org-ml--test-logbook-to-nodes-specs
   "no config" nil
   `(,p2 ,c2 ,p1 ,c1) `(,i1 ,i2) `(,c1 ,c2)

   "item drawer" `(:log-into-drawer ,i-name)
   `(,di ,c2 ,c1) `(,i1 ,i2) `(,c1 ,c2)

   "clock drawer" `(:clock-into-drawer ,c-name)
   `(,dc ,p21) `(,i1 ,i2) `(,c1 ,c2)

   "item and clock drawer (different)" `(:log-into-drawer ,i-name :clock-into-drawer ,c-name)
   `(,di ,dc) `(,i1 ,i2) `(,c1 ,c2)

   "items and clock drawer (same)" '(:log-into-drawer t :clock-into-drawer t)
   `(,dm) `(,i1 ,i2) `(,c1 ,c2)
   "clock limit" '(:log-into-drawer nil :clock-into-drawer 1)
   `(,dc* ,p21) `(,i1 ,i2) `(,c1 ,c2)

   "clock limit (higher)" '(:log-into-drawer nil :clock-into-drawer 2)
   `(,p2 ,c2 ,p1 ,c1) `(,i1 ,i2) `(,c1 ,c2)
   
   "clock limit and item drawer (same)" '(:log-into-drawer t :clock-into-drawer 1)
   `(,dm) `(,i1 ,i2) `(,c1 ,c2)
   
   "clock limit (higher) and item drawer (same)" '(:log-into-drawer t :clock-into-drawer 2)
   `(,di* ,c2 ,c1) `(,i1 ,i2) `(,c1 ,c2)

   "clock limit and item drawer (different)" `(:log-into-drawer ,i-name :clock-into-drawer 1)
   `(,di ,dc*) `(,i1 ,i2) `(,c1 ,c2)

   "clock limit (higher and item drawer (different)" `(:log-into-drawer ,i-name :clock-into-drawer 2)
   `(,di ,c2 ,c1) `(,i1 ,i2) `(,c1 ,c2) nil))

;; eight possible configurations for the logbook based on the values of
;; `org-log-into-drawer' (L) and `org-clock-into-drawer' (C)
;; - L = C = nil: 'mixed'
;; - L = string, C = nil: 'single-items'
;; - L = nil, C = string: 'single-clocks'
;; - L = C = string: 'single-mixed'
;; - L = string1, C = string2: 'dual'
;; - L = nil, C = int: 'single-clocks-or-mixed'
;; - L = string, C = int: 'single-items-or-dual'
;; - L = "LOGBOOK", C = int: 'single-mixed-or-single-items'

(defmacro expect-supercontents (config nodes items clocks unknown
                                               post-blank rest)
  (declare (indent 2))
  `(expect (org-ml--supercontents-from-nodes ,config ,nodes)
           :to-equal
           (org-ml--supercontents-init ,items ,clocks ,unknown ,post-blank ,rest)))

(defmacro org-ml--test-supercontents-specs (config &rest specs)
  (declare (indent 1))
  (let ((forms
         (->> (-partition 7 specs)
              (--map
               (-let (((title input items clocks unknown post-blank contents) it))
                 `(it ,title (expect-supercontents ,config ,input
                               ,items ,clocks ,unknown ,post-blank ,contents)))))))
    `(progn ,@forms)))

(describe "org-ml--supercontents-mixed"
  (before-all
    (setq config nil
          config-notes '(:clock-out-notes t)
          i1 (org-ml-build-log-note 1603767576 "i1")
          i2 (org-ml-build-item! :paragraph "clock note")
          p1 (org-ml-build-plain-list i1)
          p2 (org-ml-build-plain-list i2)
          p12 (org-ml-build-plain-list i1 i2)
          p21 (org-ml-build-plain-list i2 i1)
          c1 (org-ml-build-clock (org-ml-build-timestamp! '(2112 1 1 0 0) :end '(2112 1 2 0 0)))
          r1 (org-ml-build-paragraph! "foo")))
  (describe "with clock notes"
    (org-ml--test-supercontents-specs config-notes
      "nothing" nil nil nil nil nil nil
      "no logbook" (list r1) nil nil nil nil `(,r1)
      "item clock note rest" (list p1 c1 p2 r1) `(,i1) `(,c1 ,i2) nil 0 `(,r1)
      "item note clock rest" (list p1 p2 c1 r1) `(,i1) nil nil 0 `(,p2 ,c1 ,r1)
      "clock item note rest" (list c1 p2 p1 r1) `(,i1) `(,c1 ,i2) nil 0 `(,r1)
      "clock note item rest" (list c1 p1 p2 r1) `(,i1) `(,c1) nil 0 `(,p2 ,r1)
      "note item clock rest" (list p2 p1 c1 r1) nil nil nil nil `(,p21 ,c1 ,r1)
      "note clock item rest" (list p2 c1 p1 r1) nil nil nil nil `(,p2 ,c1 ,p1 ,r1)
      "item clock note" (list p1 c1 p2) `(,i1) `(,c1 ,i2) nil 0 nil
      "item note clock" (list p1 p2 c1) `(,i1) nil nil 0 `(,p2 ,c1)
      "clock item note" (list c1 p2 p1) `(,i1) `(,c1 ,i2) nil 0 nil
      "clock note item" (list c1 p1 p2) `(,i1) `(,c1) nil 0 `(,p2)
      "note item clock" (list p2 p1 c1) nil nil nil nil `(,p21 ,c1)
      "note clock item" (list p2 c1 p1) nil nil nil nil `(,p2 ,c1 ,p1)
      "item clock" (list p1 c1) `(,i1) `(,c1) nil 0 nil
      "item note" (list p1 p2) `(,i1) nil nil 0 `(,p2)
      "clock note" (list c1 p2) nil `(,c1 ,i2) nil 0 nil
      "clock item" (list c1 p1) `(,i1) `(,c1) nil 0 nil
      "note item" (list p2 p1) nil nil nil nil `(,p21)
      "note clock" (list p2 c1) nil nil nil nil `(,p2 ,c1)
      "rest list" (list r1 p1) nil nil nil nil `(,r1 ,p1)))

  (describe "without clock notes"
    (org-ml--test-supercontents-specs config
      "nothing" nil nil nil nil nil nil
      "no logbook" (list r1) nil nil nil nil `(,r1)
      "item clock note rest" (list p1 c1 p2 r1) `(,i1) `(,c1) nil 0 `(,p2 ,r1)
      "item note clock rest" (list p1 p2 c1 r1) `(,i1) nil nil 0 `(,p2 ,c1 ,r1)
      "clock item note rest" (list c1 p2 p1 r1) nil `(,c1) nil 0 `(,p21 ,r1)
      "clock note item rest" (list c1 p1 p2 r1) `(,i1) `(,c1) nil 0 `(,p2 ,r1)
      "note item clock rest" (list p2 p1 c1 r1) nil nil nil nil `(,p21 ,c1 ,r1)
      "note clock item rest" (list p2 c1 p1 r1) nil nil nil nil `(,p2 ,c1 ,p1 ,r1)
      "item clock note" (list p1 c1 p2) `(,i1) `(,c1) nil 0 `(,p2)
      "item note clock" (list p1 p2 c1) `(,i1) nil nil 0 `(,p2 ,c1)
      "clock item note" (list c1 p2 p1) nil `(,c1) nil 0 `(,p21)
      "clock note item" (list c1 p1 p2) `(,i1) `(,c1) nil 0 `(,p2)
      "note item clock" (list p2 p1 c1) nil nil nil nil `(,p21 ,c1)
      "note clock item" (list p2 c1 p1) nil nil nil nil `(,p2 ,c1 ,p1)
      "item note" (list p1 c1) `(,i1) `(,c1) nil 0 nil
      "clock note" (list p1 p2) `(,i1) nil nil 0 `(,p2)
      "clock item" (list c1 p2) nil `(,c1) nil 0 `(,p2)
      "note item" (list c1 p1) `(,i1) `(,c1) nil 0 nil
      "note clock" (list p2 p1) nil nil nil nil `(,p21)
      "rest list" (list p2 c1) nil nil nil nil `(,p2 ,c1))))

(describe "org-ml--supercontents-single-items"
  (before-all
    (setq id-name "LOGGING"
          config `(:log-into-drawer ,id-name)
          config-notes `(:log-into-drawer ,id-name :clock-out-notes t)
          i1 (org-ml-build-item! :paragraph "i1")
          i2 (org-ml-build-log-note 1603767576 "log note")
          i3 (org-ml-build-log-note 1603767576 "log note in drawer")
          p1 (org-ml-build-plain-list i1)
          p2 (org-ml-build-plain-list i2)
          p3 (org-ml-build-plain-list i3)
          p4 (org-ml-build-plain-list i1 i2)
          drwr (org-ml-build-drawer id-name p3)
          ts1 (org-ml-build-timestamp! '(2112 1 1 0 0
                                              :end '(2112 1 2 0 0)))
          c1 (org-ml-build-clock ts1)
          r1 (org-ml-build-paragraph! "foo")))
  (describe "without clock notes"
    (org-ml--test-supercontents-specs config
      "nothing" nil nil nil nil nil nil
      "no logbook" (list r1) nil nil nil nil `(,r1)
      "clock note (no notes)" (list c1 p1 r1) nil `(,c1) nil 0 `(,p1 ,r1)
      "clock note item (no notes)" (list c1 p4 r1) nil `(,c1) nil 0 `(,p4 ,r1)
      ;; ASSUME the code that stops splitting after finding an invalid item is
      ;; fully tested with this example and will therefore do the same in the
      ;; permutations below
      "item clock (store none)" (list p1 c1 r1) nil nil nil nil `(,p1 ,c1 ,r1)
      "drawer clock (store both)" (list drwr c1 r1) `(,i3) `(,c1) nil 0 `(,r1)
      "clock drawer (store both)" (list c1 drwr r1) `(,i3) `(,c1) nil 0 `(,r1)
      "clock item drawer (don't store note)" (list c1 p1 drwr r1) nil `(,c1) nil 0 `(,p1 ,drwr ,r1)))
  (describe "with clock notes"
    (org-ml--test-supercontents-specs config-notes
      "clock item drawer (store all)" (list c1 p1 drwr r1) `(,i3) `(,c1 ,i1) nil 0 `(,r1)
      "clock note item" (list c1 p4 r1) nil `(,c1 ,i1) nil 0 `(,p2 ,r1)
      "clock note" (list c1 p1 r1) nil `(,c1 ,i1) nil 0 `(,r1)
      "clock item" (list c1 p2 r1) nil `(,c1) nil 0 `(,p2 ,r1))))

(describe "org-ml--supercontents-single-clocks"
  (before-all
    (setq cd-name "CLOCKING"
          config `(:clock-into-drawer ,cd-name)
          config-notes `(:clock-into-drawer ,cd-name :clock-out-notes t)
          i1 (org-ml-build-log-note 1603767576 "note 1")
          i2 (org-ml-build-log-note 1603767576 "note 2")
          i3 (org-ml-build-item! :paragraph "clock note")
          p1 (org-ml-build-plain-list i1)
          p2 (org-ml-build-plain-list i2)
          p3 (org-ml-build-plain-list i3)
          ts1 (org-ml-build-timestamp! '(2112 1 1 0 0) :end '(2112 1 2 0 0))
          c1 (org-ml-build-clock ts1)
          drwr1 (org-ml-build-drawer cd-name c1)
          drwr2 (org-ml-build-drawer cd-name c1 p3)
          r1 (org-ml-build-paragraph! "foo")))
  (describe "without clock notes"
    (org-ml--test-supercontents-specs config
      "nothing" nil nil nil nil nil nil
      "no logbook" (list r1) nil nil nil nil `(,r1)
      ;; this only has five valid combinations
      ;;
      "item, drawer, item" (list p1 drwr1 p2 r1) `(,i1 ,i2) `(,c1) nil 0 `(,r1)
      "item, drawer" (list p1 drwr1 r1) `(,i1) `(,c1) nil 0 `(,r1)
      "item" (list p1 r1) `(,i1) nil nil 0 `(,r1)
      "drawer, item" (list drwr1 p1 r1) `(,i1) `(,c1) nil 0 `(,r1)
      "drawer" (list drwr1 r1) nil `(,c1) nil 0 `(,r1)
      ;; invalid
      "loose clock anywhere" (list c1 p1 r1) nil nil nil nil `(,c1 ,p1 ,r1)))
  (describe "with clock notes"
    (org-ml--test-supercontents-specs config-notes
      "drawer with clock notes" (list drwr2 r1) nil `(,c1 ,i3) nil 0 `(,r1))))

(describe "org-ml--supercontents-dual"
  (before-all
    (setq cd-name "CLOCKING"
          id-name "LOGGING"
          config `(:log-into-drawer ,id-name :clock-into-drawer ,cd-name)
          i1 (org-ml-build-log-note 1603767576 "note")
          p1 (org-ml-build-plain-list i1)
          ts1 (org-ml-build-timestamp! '(2112 1 1 0 0) :end '(2112 1 2 0 0))
          c1 (org-ml-build-clock ts1)
          drwr1 (org-ml-build-drawer cd-name c1)
          drwr2 (org-ml-build-drawer id-name p1)
          r1 (org-ml-build-paragraph! "foo")))
  (org-ml--test-supercontents-specs config
    "nothing" nil nil nil nil nil nil
    "no logbook" (list r1) nil nil nil nil `(,r1)
    "one drawer" (list drwr1 r1) nil `(,c1) nil 0 `(,r1)
    "one drawer (other one)" (list drwr2 r1) `(,i1) nil nil 0 `(,r1)
    "two drawers" (list drwr1 drwr2 r1) `(,i1) `(,c1) nil 0 `(,r1)
    "two drawers (other order)" (list drwr2 drwr1 r1) `(,i1) `(,c1) nil 0 `(,r1)
    "clock outside (invalid)" (list c1 drwr2 r1) nil nil nil nil `(,c1 ,drwr2 ,r1)
    "item outside (invalid)" (list p1 drwr1 r1) nil nil nil nil `(,p1 ,drwr1 ,r1)))

(describe "org-ml--supercontents-single-mixed"
  (before-all
    (setq d-name "LOGBOOK"
          config '(:log-into-drawer t :clock-into-drawer t)
          config-notes (list :log-into-drawer t
                             :clock-into-drawer t
                             :clock-out-notes t)
          i1 (org-ml-build-log-note 1603767576 "note 2")
          i2 (org-ml-build-item! :paragraph "clock note")
          p1 (org-ml-build-plain-list i1)
          p2 (org-ml-build-plain-list i2)
          ts1 (org-ml-build-timestamp! '(2112 1 1 0 0) :end '(2112 1 2 0 0))
          c1 (org-ml-build-clock ts1)
          drwr1 (org-ml-build-drawer d-name c1 p1)
          drwr2 (org-ml-build-drawer d-name c1 p2 p1)
          r1 (org-ml-build-paragraph! "foo")))
  (describe "without clock notes"
    (org-ml--test-supercontents-specs config
      "nothing" nil nil nil nil nil nil
      "no logging" (list r1) nil nil nil nil `(,r1)
      "single drawer" (list drwr1 r1) `(,i1) `(,c1) nil 0 `(,r1)
      "clock outside (invalid)" (list c1 drwr1 r1) nil nil nil nil `(,c1 ,drwr1 ,r1)
      "item outside (invalid)" (list p1 drwr1 r1) nil nil nil nil `(,p1 ,drwr1 ,r1)))
  (describe "with clock notes"
    (org-ml--test-supercontents-specs config-notes
      "single drawer with notes" (list drwr2 r1) `(,i1) `(,c1 ,i2) nil 0 `(,r1))))

(describe "org-ml--supercontents-single-clocks-or-mixed"
  ;; ASSUME clock notes are tested using the mixed and single-clocks tests
  (before-all
    (setq clock-limit 1
          config `(:clock-into-drawer ,clock-limit)
          i1 (org-ml-build-log-note 1603767576 "note 1")
          i2 (org-ml-build-log-note 1603767576 "note 2")
          p1 (org-ml-build-plain-list i1)
          p2 (org-ml-build-plain-list i2)
          ts1 (org-ml-build-timestamp! '(2112 1 1 0 0) :end '(2112 1 2 0 0))
          c1 (org-ml-build-clock ts1)
          ts2 (org-ml-build-timestamp! '(2112 1 2 0 0) :end '(2112 1 3 0 0))
          c2 (org-ml-build-clock ts1)
          drwr (org-ml-build-drawer "LOGBOOK" c1)
          r1 (org-ml-build-paragraph! "foo")))
  (org-ml--test-supercontents-specs config
    "nothing" nil nil nil nil nil nil
    "no logbook" (list r1) nil nil nil nil `(,r1)
    ;; same tests as single-clocks when over clock limit
    ;;
    "plain-list, drawer, plain-list" (list p1 drwr p2 r1) `(,i1 ,i2) `(,c1) nil 0 `(,r1)
    "plain-list, drawer" (list p1 drwr r1) `(,i1) `(,c1) nil 0 `(,r1)
    "plain-list" (list p1 r1) `(,i1) nil nil 0 `(,r1)
    "drawer, plain-list" (list drwr p1 r1) `(,i1) `(,c1) nil 0 `(,r1)
    "drawer" (list drwr r1) nil `(,c1) nil 0 `(,r1)
    ;; same as mixed
    ;;
    "loose clock under clock limit" (list c1 p1 r1) `(,i1) `(,c1) nil 0 `(,r1)
    "too many clocks" (list c1 c2 p1 r1) nil `(,c1) nil 0 `(,c2 ,p1 ,r1)))

(describe "org-ml--supercontents-single-items-or-dual"
  (before-all
    (setq id-name "LOGGING"
          clock-limit 1
          config `(:log-into-drawer ,id-name :clock-into-drawer ,clock-limit)
          i1 (org-ml-build-log-note 1603767576 "note 1")
          i2 (org-ml-build-log-note 1603767576 "note 2")
          i3 (org-ml-build-log-note 1603767576 "note 3")
          p1 (org-ml-build-plain-list i1)
          p2 (org-ml-build-plain-list i1 i2)
          p3 (org-ml-build-plain-list i3)
          drwr1 (org-ml-build-drawer id-name p3)
          ts1 (org-ml-build-timestamp! '(2112 1 1 0 0) :end '(2112 1 2 0 0))
          c1 (org-ml-build-clock ts1)
          ts2 (org-ml-build-timestamp! '(2112 1 2 0 0) :end '(2112 1 3 0 0))
          c2 (org-ml-build-clock ts2)
          drwr2 (org-ml-build-drawer "LOGBOOK" c1)
          r1 (org-ml-build-paragraph! "foo")))
  (org-ml--test-supercontents-specs config
    "nothing" nil nil nil nil nil nil
    "no logbook" (list r1) nil nil nil nil `(,r1)
    ;; same as single-items
    ;;
    "clock item (don't store note)" (list c1 p1 r1) nil `(,c1) nil 0 `(,p1 ,r1)
    "clock items (store only clock)" (list c1 p2 r1) nil `(,c1) nil 0 `(,p2 ,r1)
    ;; ASSUME the code that stops splitting
    ;; after finding an invalid item is fully tested with this example and will
    ;; therefore do the same in the permutations below
    "item clock (store none)" (list p1 c1 r1) nil nil nil nil `(,p1 ,c1 ,r1)
    "drawer clock (store both)" (list drwr1 c1 r1) `(,i3) `(,c1) nil 0 `(,r1)
    "clock drawer (store both)" (list c1 drwr1 r1) `(,i3) `(,c1) nil 0 `(,r1)
    "drawer clock item (store only clock)" (list drwr1 c1 p1 r1) `(,i3) `(,c1) nil 0 `(,p1 ,r1)
    "clock item drawer (don't store note)" (list c1 p1 drwr1 r1) nil `(,c1) nil 0 `(,p1 ,drwr1 ,r1)
    "too many clocks" (list c1 c2 p1 drwr1 r1) nil `(,c1) nil 0 `(,c2 ,p1 ,drwr1 ,r1)
    "dual drawer (clock only)" (list drwr2 r1) nil `(,c1) nil 0 `(,r1)
    "dual drawer (item only)" (list drwr1 r1) `(,i3) nil nil 0 `(,r1)
    "dual drawer (both)" (list drwr1 drwr2 r1) `(,i3) `(,c1) nil 0 `(,r1)))

(describe "org-ml--supercontents-single-mixed-or-single-items"
  (before-all
    (setq clock-limit 1
          config `(:log-into-drawer t :clock-into-drawer ,clock-limit)
          i1 (org-ml-build-log-note 1603767576 "note 1")
          p1 (org-ml-build-plain-list i1)
          ts1 (org-ml-build-timestamp! '(2112 1 1 0 0) :end '(2112 1 2 0 0))
          c1 (org-ml-build-clock ts1)
          ts2 (org-ml-build-timestamp! '(2112 1 2 0 0) :end '(2112 1 3 0 0))
          c2 (org-ml-build-clock ts2)
          drwr1 (org-ml-build-drawer "LOGBOOK" c1 p1)
          drwr2 (org-ml-build-drawer "LOGBOOK" p1)
          r1 (org-ml-build-paragraph! "foo")))
  (org-ml--test-supercontents-specs config
    "nothing" nil nil nil nil nil nil
    "no logging" (list r1) nil nil nil nil `(,r1)
    "single drawer" (list drwr1 r1) `(,i1) `(,c1) nil 0 `(,r1)
    "clock outside and inside" (list c1 drwr1 r1) `(,i1) `(,c1) `(,c1) 0 `(,r1)
    "clocks outside and not inside" (list c1 drwr2 r1) `(,i1) `(,c1) nil 0 `(,r1)
    "too many clocks outside" (list c1 c2 drwr2 r1) nil `(,c1) nil 0 `(,c2 ,drwr2 ,r1)
    "item outside (invalid)" (list p1 drwr1 r1) nil nil nil nil `(,p1 ,drwr1 ,r1)))

;; logbook blank line testing
;;
;; assume these tests cover all code paths
;; - any logbook type followed by a blank (item, clock, drawer)
;; - in the case of clocks with notes, clocks followed by plain lists where the
;;   first item has a blank after it

(describe "org-ml--supercontents-mixed-blank-line"
  (before-all
    (setq config nil
          config-notes '(:clock-out-notes t)
          config-drawer '(:log-into-drawer t)
          i1 (->> (org-ml-build-log-note 1603767576 "note 1")
                  (org-ml-set-property :post-blank 1))
          i2 (org-ml-build-log-note 1603767576 "note 2")
          i3 (org-ml-build-item! :post-blank 1 :paragraph "clock note")
          p1 (org-ml-build-plain-list i1 i2)
          p2 (org-ml-build-plain-list i1)
          p3 (org-ml-build-plain-list i2)
          p4 (org-ml-build-plain-list :post-blank 1 i1 i2)
          p5 (org-ml-build-plain-list :post-blank 1 i2)
          p6 (org-ml-build-plain-list i3 i2)
          p66 (org-ml-build-plain-list i2)
          ;; (p6 (org-ml-build-plain-list i1)
          p7 (org-ml-build-plain-list :post-blank 1 i2)
          ts1 (org-ml-build-timestamp! '(2112 1 1 0 0) :end '(2112 1 2 0 0))
          c1 (org-ml-build-clock ts1)
          c2 (org-ml-build-clock ts1 :post-blank 1)
          drwr (org-ml-build-drawer "LOGBOOK" :post-blank 1 p2)
          r1 (org-ml-build-paragraph! "foo")))
  (describe "with clock notes"
    (org-ml--test-supercontents-specs config-notes
      "item space item" (list p1 c1 r1) `(,i1) nil nil 1 `(,p3 ,c1 ,r1)
      "clock item space item" (list c1 p1 r1) `(,i1) `(,c1) nil 1 `(,p3 ,r1)
      "clock note space item" (list c1 p6 r1) nil `(,c1 ,i3) nil 1 `(,p66 ,r1)
      "clock space item" (list c2 p1 r1) nil `(,c2) nil 1 `(,p1 ,r1)
      "item space item space" (list p4 r1) `(,i1) nil nil 1 `(,p7 ,r1)))
  (describe "without clock notes"
    (org-ml--test-supercontents-specs config
      "item space item" (list p1 c1 r1) `(,i1) nil nil 1 `(,p3 ,c1 ,r1)
      "clock item space item" (list c1 p1 r1) `(,i1) `(,c1) nil 1 `(,p3 ,r1)
      "clock note space item" (list c1 p6 r1) nil `(,c1) nil 0 `(,p6 ,r1)
      "clock space item" (list c2 p1 r1) nil `(,c2) nil 1 `(,p1 ,r1)
      "item space item space" (list p4 r1) `(,i1) nil nil 1 `(,p7 ,r1)))
  (describe "with drawer"
    ;; TODO this has side effects :(
    (org-ml--test-supercontents-specs config-drawer
      "single drawer" (list drwr r1) `(,i1) nil nil 1 `(,r1))))

;;; MATCH FRAMEWORK TESTING

;; These are tests for `org-ml-match' and friends. Proceed with caution :)

(defmacro should-error-arg (form)
  "Make an ert error form to test if FORM signals an `arg-type-error'."
  `(should-error ,form :type 'arg-type-error))

(describe "org-ml--match-make-condition-form/error"
  ;; Ensure `org-ml--match-make-condition-form' will error when it
  ;; supposed to do so. All errors (in theory) should be tested here
  ;; so that we don't need to bother testing them anywhere else when
  ;; we test functions higher in the framework
  (unless (fboundp 'org-ml--match-make-condition-form)
    (error "Function not defined"))
  (before-all
    (setq fun #'org-ml--match-make-condition-form))
  (it "quoted"
    (should-error-arg (funcall fun '(quote bold)))
    (should-error-arg (funcall fun '(function bold))))
  (it "invalid type"
    (should-error-arg (funcall fun 'protoss)))
  (it "invalid operator"
    (should-error-arg (funcall fun '(= 1)))
    (should-error-arg (funcall fun '(=/ 1))))
  (it "valid operator with non-integer"
    (should-error-arg (funcall fun '(< "1"))))
  (it "valid operator with too many arguments"
    (should-error-arg (funcall fun '(< 1 2))))
  (it "pred with no arguments"
    (should-error-arg (funcall fun '(:pred))))
  (it "pred with too many arguments"
    (should-error-arg (funcall fun '(:pred stringp integerp))))
  (it "not with no arguments"
    (should-error-arg (funcall fun '(:not))))
  (it "not with too many arguments"
    (should-error-arg (funcall fun '(:not 1 3))))
  (it "and with no arguments"
    (should-error-arg (funcall fun '(:and))))
  (it "and with nonsense"
    (should-error-arg (funcall fun '(:and bold :2))))
  (it "or with no arguments"
    (should-error-arg (funcall fun '(:or))))
  (it "or with nonsense"
    (should-error-arg (funcall fun '(:or bold :2))))
  (it "properties with symbols instead of keywords"
    (should-error-arg (funcall fun '(tags '("hi")))))
  (it "multiple properties"
    (should-error-arg (funcall fun '(:tags '("hi") :todo-keyword "DONE"))))
  (it "just wrong..."
    (should-error-arg (funcall fun nil))
    (should-error-arg (funcall fun :1))))

(describe "org-ml--match-pattern-make-inner-form/error"
  ;; Ensure `org-ml--match-make-inner-form' will error when it supposed to
  ;; do so. All errors (in theory) should be tested here so that
  ;; we don't need to bother testing them anywhere else when we test
  ;; functions higher in the framework
  ;;
  ;; Assume:
  ;; - all invalid patterns at the condition level will be caught by
  ;;   `org-ml--match-make-condition-form/error'.
  ;; - these error paths are independent of `END?' and `LIMIT' so
  ;;   set them both to nil
  (unless (fboundp 'org-ml--match-pattern-make-inner-form)
    (error "Function not defined"))
  (before-all
    (setq fun (-partial #'org-ml--match-pattern-make-inner-form nil nil)))
  (it "slicers present"
    (should-error-arg (funcall fun '(:first bold)))
    (should-error-arg (funcall fun '(:last bold)))
    (should-error-arg (funcall fun '(:nth bold)))
    (should-error-arg (funcall fun '(:sub bold)))
    (should-error-arg (funcall fun '(bold :first)))
    (should-error-arg (funcall fun '(bold :last)))
    (should-error-arg (funcall fun '(bold :nth)))
    (should-error-arg (funcall fun '(bold :sub))))
  (it "just wrong..."
    (should-error-arg (funcall fun '(:swaggart)))))

(defun should-expand-to-alts (pattern alt-patterns)
  (should (equal (org-ml--match-pattern-expand-alternations pattern)
                 alt-patterns)))

(describe "org-ml--match-pattern-expand-alternations"
  ;; ensure that alternations expand properly
  (unless (fboundp 'org-ml--match-pattern-expand-alternations)
    (error "Function not defined"))
  (it "no alternations"
    (should-expand-to-alts '(a) '((a)))
    (should-expand-to-alts '(a b c) '((a b c))))
  (it "1-level alternations"
    (should-expand-to-alts '((x | y)) '((x) (y)))
    (should-expand-to-alts '((x | y) a) '((x a) (y a)))
    (should-expand-to-alts '(a (x | y)) '((a x) (a y)))
    (should-expand-to-alts '(a (x | y) b) '((a x b) (a y b))))
  (it "1-level alternations with nil"
    (should-expand-to-alts '((nil | y)) '(nil (y)))
    (should-expand-to-alts '((nil | y) a) '((a) (y a)))
    (should-expand-to-alts '(a (nil | y)) '((a) (a y)))
    (should-expand-to-alts '(a (nil | y) b) '((a b) (a y b))))
  (it "1-level serial alternations"
    (should-expand-to-alts '((m | n) (x | y)) '((m x) (m y) (n x) (n y)))
    (should-expand-to-alts '(a (m | n) b (x | y) c) '((a m b x c) (a m b y c)
                                                      (a n b x c) (a n b y c))))
  (it "1-level serial alternations with nil"
    (should-expand-to-alts '((nil | n) (x | y)) '((x) (y) (n x) (n y)))
    (should-expand-to-alts '((m | n) (nil | y)) '((m) (m y) (n) (n y)))
    (should-expand-to-alts '(a (nil | n) b (x | y) c) '((a b x c) (a b y c)
                                                        (a n b x c) (a n b y c)))
    (should-expand-to-alts '(a (m | n) b (nil | y) c) '((a m b c) (a m b y c)
                                                        (a n b c) (a n b y c))))
  (it "2-level alternations"
    (should-expand-to-alts '((x | (m | n))) '((x) (m) (n)))
    (should-expand-to-alts '(a (x | (m | n))) '((a x) (a m) (a n)))
    (should-expand-to-alts '((x | y (m | n))) '((x) (y m) (y n)))
    (should-expand-to-alts '(a (x | y (m | n))) '((a x) (a y m) (a y n))))
  (it "2-level alternations with nil"
    (should-expand-to-alts '((nil | (m | n))) '(nil (m) (n)))
    (should-expand-to-alts '(a (nil | (m | n))) '((a) (a m) (a n)))
    (should-expand-to-alts '((nil | y (m | n))) '(nil (y m) (y n)))
    (should-expand-to-alts '(a (nil | y (m | n))) '((a) (a y m) (a y n)))
    (should-expand-to-alts '((x | (nil | n))) '((x) nil (n)))
    (should-expand-to-alts '(a (x | (nil | n))) '((a x) (a) (a n)))
    (should-expand-to-alts '((x | y (nil | n))) '((x) (y) (y n)))
    (should-expand-to-alts '(a (x | y (nil | n))) '((a x) (a y) (a y n)))))

(defun should-expand-to (pattern expanded-pattern)
  (should (equal (org-ml--match-pattern-simplify-wildcards pattern)
                 expanded-pattern)))

(describe "org-ml--match-pattern-simplify-wildcards"
  ;; ensure that bracket and + wildcards expand properly
  (unless (fboundp 'org-ml--match-pattern-simplify-wildcards)
    (error "Function not defined"))
  (it "?"
    (should-expand-to '(x \?) '((nil | x)))
    (should-expand-to '(x \? y) '((nil | x) y)))
  (it "+"
    (should-expand-to '(x +) '(x x *))
    (should-expand-to '(x + y) '(x x * y)))
  (it "brackets"
    (should-expand-to '(x [1]) '(x))
    (should-expand-to '(x [2]) '(x x))
    (should-expand-to '(x [0 1]) '((nil | x)))
    (should-expand-to '(x [2 2]) '(x x))
    (should-expand-to '(x [1 2]) '((x | x x)))
    (should-expand-to '(x [1 nil]) '(x x *))))

(describe "org-ml--match-pattern-simplify-wildcards/error"
  ;; test errors in wildcard expansion
  ;; note, we assume that any malformed patterns are caught later
  ;; so no need to test if we supply two +'s in a row and other garbage
  (unless (fboundp 'org-ml--match-pattern-simplify-wildcards)
    (error "Function not defined"))
  (it "zero not allowed"
    (should-error-arg (org-ml--match-pattern-simplify-wildcards '(x [0]))))
  (it "negative not allowed"
    (should-error-arg (org-ml--match-pattern-simplify-wildcards '(x [-1]))))
  (it "double zeros not allowed"
    (should-error-arg (org-ml--match-pattern-simplify-wildcards '(x [0 0]))))
  (it "negatives not allowed"
    (should-error-arg (org-ml--match-pattern-simplify-wildcards '(x [-1 1])))
    (should-error-arg (org-ml--match-pattern-simplify-wildcards '(x [1 -1])))
    (should-error-arg (org-ml--match-pattern-simplify-wildcards '(x [-1 -1]))))
  (it "must be ascending order"
    (should-error-arg (org-ml--match-pattern-simplify-wildcards '(x [2 1])))))

(describe "org-ml--match-make-slicer-form"
  ;; Ensure `org-ml--match-make-inner-form' will error when it supposed to
  ;; do so. All errors (in theory) should be tested here so that
  ;; we don't need to bother testing them anywhere else when we test
  ;; functions higher in the framework
  ;;
  ;; Assume that all invalid patterns at the predicate and wildcard
  ;; level will be caught by `org-ml--match-make-condition-form/error' and
  ;; `org-ml--match-pattern-make-inner-form/error'
  (unless (fboundp 'org-ml--match-make-slicer-form)
    (error "Function not defined"))
  (before-all
    (setq fun #'org-ml--match-make-slicer-form))
  (it "nth with non-integer"
    (should-error-arg (funcall fun '(:nth "1" bold))))
  (it "sub with non-integers"
    (should-error-arg (funcall fun '(:sub "1" 2 bold)))
    (should-error-arg (funcall fun '(:sub 1 "2" bold))))
  (it "sub with flipped integers"
    (should-error-arg (funcall fun '(:sub 2 1 bold)))
    (should-error-arg (funcall fun '(:sub -1 -2 bold))))
  (it "sub with split integers"
    (should-error-arg (funcall fun '(:sub -1 2 bold)))))

(defmacro match-should-equal (node result &rest patterns)
  "Return form to test if all PATTERNS applied NODE return RESULT."
  (declare (indent 2))
  (let ((tests (--map
                `(expect ,result
                         :to-equal
                         (->> (org-ml-match ',it ,node)
                              (-map #'org-ml-to-trimmed-string)))
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
     (it "first match"
       (match-should-equal node (-take 1 ,expected)
                           (:first ,@pattern) (:nth 0 ,@pattern)
                           (:sub 0 0 ,@pattern)))
     (it "last match"
       (match-should-equal node (-take-last 1 ,expected)
                           (:last ,@pattern) (:nth -1 ,@pattern)
                           (:sub -1 -1 ,@pattern)))
     (it "nth match positive"
       (match-should-equal node (-drop 1 (-take 2 ,expected))
                           (:nth 1 ,@pattern) (:sub 1 1 ,@pattern)))
     (it "nth match negative"
       (match-should-equal node (-drop-last 1 (-take-last 2 ,expected))
                           (:nth -2 ,@pattern) (:sub -2 -2 ,@pattern)))
     (it "out of range positive"
       (match-should-equal node nil
                           (:nth 100 ,@pattern) (:sub 100 100 ,@pattern)))
     (it "out of range negative"
       (match-should-equal node nil
                           (:nth -100 ,@pattern) (:sub -100 -100 ,@pattern)))
     (it "bounded to out of range"
       (match-should-equal node ,expected
                           (:sub 0 100 ,@pattern) (:sub -100 -1 ,@pattern)))
     ;;
     ;; these slicers can only be expressed one way
     ;;
     (it "zero-bounded finite positive"
       (match-should-equal node (-take 2 ,expected)
                           (:sub 0 1 ,@pattern)))
     (it "zero-bounded finite negative"
       (match-should-equal node (-take-last 2 ,expected)
                           (:sub -2 -1 ,@pattern)))
     (it "floating finite positive"
       (match-should-equal node (-drop 1 (-take 3 ,expected))
                           (:sub 1 2 ,@pattern)))
     (it "floating finite negative"
       (match-should-equal node (-drop-last 1 (-take-last 3 ,expected))
                           (:sub -3 -2 ,@pattern)))
     (it "floating out of range positive"
       (match-should-equal node (-drop 1 ,expected)
                           (:sub 1 100 ,@pattern)))
     (it "floating out of range negative"
       (match-should-equal node (-drop-last 1 ,expected)
                           (:sub -100 -2 ,@pattern)))))

;; Here we test the following pattern combinations
;; - multi-level condition
;; - :any + condition
;; - condition + :any
;; - *
;;
;; The reason for choosing these combinations is that all of them
;; combined should hit each of the valid form-building switches in
;; `org-ml--match-pattern-make-inner-form'. Since the behavior of these
;; depends on the value of `LIMIT' and `END?' and these are set
;; depending on the slicer, testing these combinations with all
;; reasonable slicer combination should ensure that every path with
;; every combination of `LIMIT' and `END?' is tested. Note this
;; assumes that `org-ml--match-make-condition-form' is working correctly
;; as the following test only use a few combinations in this function.
;; However, `org-ml--match-make-condition-form' is independent of the
;; chosen slicer so this should not matter

(describe "org-ml-match/slicer-predicate"
  ;; test the single/multiple condition path with all slicers
  (before-all
    (setq node (->> (s-join "\n"
                            '("* one"
                             "** TODO two"
                             "2"
                             "** COMMENT three"
                             "3"
                             "** four"
                             "4"
                             "** DONE five"
                             "5"))
                   (org-ml--from-string))))
  (match-slicer-should-equal node '("2" "3" "4" "5") (headline section)))

(describe "org-ml-match/slicer-any-first"
  ;; test the :any + condition path with all slicers
  (before-all
    (setq node (org-ml-build-paragraph! "*_1_* */2/* _*3*_ _/4/_ /*5*/ /_6_/")))
  (match-slicer-should-equal node '("/2/" "*3*" "/4/" "*5*") (:any (:or bold italic))))

(describe "org-ml-match/slicer-any-last"
  ;; test the condition + :any path with all slicers
  (before-all
    (setq node (org-ml-build-paragraph! "*_1_* */2/* _*3*_ _/4/_ /*5*/ /_6_/")))
  (match-slicer-should-equal node '("_1_" "/2/" "*5*" "_6_") ((:or bold italic) :any)))

(describe "org-ml-match/empty-patterns"
  (before-all
    (setq node (->> (s-join "\n"
                            '("* one"
                              "** two"
                              "** three"))
                    (org-ml--from-string)))
    (defun match-empty (p)
      (expect (org-ml-match p node) :to-equal (list node))))
  (it "empty patterns"
    (match-empty '())
    (match-empty '(:first))
    (match-empty '(:last))
    (match-empty '(:nth 0))
    (match-empty '(:sub 0 0)))
  (it "wildcards with the empty pattern"
    (match-empty '(:first headline \?))
    (match-empty '(:first headline *))
    (match-empty '(:first (nil | headline)))
    (match-empty '(:last (headline | nil)))))

;; (ert-deftest org-ml-match/slicer-many ()
;;   ;; Test the * paths with all slicers. Here the node
;;   ;; is chosen such that some values are nested and thus * will
;;   ;; return them but *! will not
;;   (let ((node (->> (s-join "\n"
;;                            '("* one"
;;                              "- 1"
;;                              "- 2"
;;                              "  - 3"
;;                              "** two"
;;                              "- 4"
;;                              "- 5"
;;                              "  - 6"
;;                              "** three"
;;                              "- 7"
;;                              "- 8"
;;                              "  - 9"))
;;                    (org-ml--from-string)))
;;         (expected '("- 1" "- 2\n  - 3" "- 3" "- 4" "- 5\n  - 6"
;;                     "- 6" "- 7" "- 8\n  - 9" "- 9"))
;;         (expected! '("- 1" "- 2\n  - 3" "- 4" "- 5\n  - 6" "- 7"
;;                      "- 8\n  - 9")))
;;     (match-slicer-should-equal node expected (:any * item))
;;     (match-slicer-should-equal node expected! (:any *! item))))

;; DIFF ALGORITHM

(defmacro org-ml--test-lcs-specs (&rest specs)
  (declare (indent 1))
  (let ((forms
         (->> (-partition 4 specs)
              (--map
               (-let (((title a b ses) it))
                 `(it ,title
                    (expect (car (org-ml--diff-find-ses ,a ,b)) :to-equal ,ses)))))))
    `(progn ,@forms)))

(describe "diff algorithm"
  (describe "find SES"
    (org-ml--test-lcs-specs
        "empty strings" "" "" 0
        "one identical char" "a" "a" 0
        "two identical chars" "aa" "aa" 0

        "zero chars, insert one (1)" "a" "" 1
        "zero chars, insert one (2)" "" "a" 1

        "one char, insert one (1)" "ba" "a" 1
        "one char, insert one (2)" "ab" "a" 1
        "one char, insert one (3)" "a" "ba" 1
        "one char, insert one (4)" "a" "ab" 1
        
        "one char, insert two (1)" "a" "abc" 2
        "one char, insert two (2)" "a" "acb" 2
        "one char, insert two (3)" "a" "cab" 2
        "one char, insert two (4)" "a" "cba" 2
        "one char, insert two (5)" "a" "bca" 2
        "one char, insert two (6)" "a" "bac" 2

        "different chars" "a" "b" 2

        "two chars, one different (1)" "aa" "ab" 2
        "two chars, one different (2)" "aa" "ba" 2

        "three chars, one different (1)" "aaa" "baa" 2
        "three chars, one different (2)" "aaa" "aba" 2
        "three chars, one different (3)" "aaa" "aab" 2

        "three chars, two different (1)" "aaa" "abc" 4
        "three chars, two different (2)" "aaa" "acb" 4
        "three chars, two different (3)" "aaa" "bac" 4
        "three chars, two different (4)" "aaa" "cab" 4
        "three chars, two different (5)" "aaa" "cba" 4
        "three chars, two different (6)" "aaa" "bca" 4
        )))

(provide 'org-ml-dev-test)
;;; org-ml-dev-test.el ends here
