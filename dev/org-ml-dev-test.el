;;; org-ml-dev-test.el --- Tests for org-ml

;;; Commentary:

;;; Code:

(require 'dash)
(require 'org-ml-dev-examples-to-tests)
(require 'org-ml-dev-examples)

;;; LIST OPERATIONS

(ert-deftest org-ml--pad-or-truncate/properties ()
  (let ((finite-list '(1 2 3)))
    ;; zero length list with zero length
    (should (equal nil (org-ml--pad-or-truncate 0 'x nil)))
    ;; zero length list with positive length
    (should (equal '(x) (org-ml--pad-or-truncate 1 'x nil)))
    ;; positive length list; length is less
    (should (equal '(1 2) (org-ml--pad-or-truncate 2 'x finite-list)))
    ;; positive length list; length is equal
    (should (equal '(1 2 3) (org-ml--pad-or-truncate 3 'x finite-list)))
    ;; positive length list; length is greater
    (should (equal '(1 2 3 x) (org-ml--pad-or-truncate 4 'x finite-list)))
    ;; positive length list; length is zero
    (should (equal nil (org-ml--pad-or-truncate 0 'x finite-list)))))

;; TODO add plist-get-keys?
;; TODO add plist-get-vals?
;; TODO add plist-map-values?

(ert-deftest org-ml--is-plist/properties ()
  ;; finite plist
  (should (org-ml--is-plist '(:one one :two 2 :three "3")))
  ;; zero-length plist
  (should (org-ml--is-plist nil))
  ;; symbols instead of keywords
  (should-not (org-ml--is-plist '(one one two 2 three "3")))
  ;; incomplete
  (should-not (org-ml--is-plist '(:one one :two 2 :three)))
  ;; not list
  (should-not (org-ml--is-plist ":one one :two 2 :three")))

;; TODO add plist-remove?

;;; inter-list operations

;; These functions operate using indices that refer to spaces between
;; list members. As such there is no such thing as a nonsensical index.
;; Since there will always be the option to add to the front or the
;; back of the list, even an empty list has a logical index that points
;; to these locations (they just happen to be the same). Therefore,
;; the only errors we need to catch here are those that refer to out
;; of range indices.

(defmacro org-ml--inter-list-ops-test (fun input output-single
                                       output-upper output-lower)
  "Return form to test intra-index list operations using FUN.
INPUT is an input list, OUTPUT-SINGLE is a list made as if FUN were
applied to an empty list, OUTPUT-UPPER is the input list with FUN
applied as if it was given the highest possible index, and OUTPUT-LOWER
is the converse."
  (declare (indent 1))
  `(let ((fun ,fun)
         (input ,input)
         (output-single ,output-single)
         (output-upper ,output-upper)
         (output-lower ,output-lower))
     ;; zero length list at 0
     (should (equal output-single (funcall fun 0 nil)))
     (should (equal output-single (funcall fun -1 nil)))
     ;; zero length list (overrange)
     (should-error (funcall fun 100 nil))
     (should (equal output-single (funcall fun 100 nil t)))
     ;; zero length list (underrange)
     (should-error (funcall fun -100 nil))
     (should (equal output-single (funcall fun -100 nil t)))
     ;; finite list (in range)
     (should (equal output-lower (funcall fun 0 input)))
     (should (equal output-upper (funcall fun -1 input)))
     ;; finite list (overrange)
     (should-error (funcall fun 100 '(1 2)))
     (should (equal output-upper (funcall fun 100 input t)))
     ;; finite list (underrange)
     (should-error (funcall fun -100 '(1 2)))
     (should (equal output-lower (funcall fun -100 input t)))))

(ert-deftest org-ml--insert-at/properties ()
  (org-ml--inter-list-ops-test (lambda (n list &optional p)
                             (org-ml--insert-at n 'x list p))
    '(1 2) '(x) '(1 2 x) '(x 1 2)))

(ert-deftest org-ml--split-at/properties ()
  (org-ml--inter-list-ops-test #'org-ml--split-at
    '(1 2) nil '((1 2) nil) '(nil (1 2))))

(ert-deftest org-ml--splice-at/properties ()
  (org-ml--inter-list-ops-test (lambda (n list &optional p)
                             (org-ml--splice-at n '(x y) list p))
    '(1 2) '(x y) '(1 2 x y) '(x y 1 2)))

;; These functions operate using indices that refer to explicit
;; members of a list. As such there will be no possible integers that
;; will be valid for an empty list. This provides one extra error case
;; to test, which is the possibility that we cannot operate on the
;; list and thus return nil. All else is the same relative to the
;; inter-list operations tests above

(defmacro org-ml--intra-list-ops-test (fun input output-upper output-lower)
  "Return form to test intra-index list operations using FUN.
INPUT is an input list, OUTPUT-UPPER is the input list with FUN
applied as if it was given the highest possible index, and OUTPUT-LOWER
is the converse."
  (declare (indent 1))
  `(let ((fun ,fun)
         (input ,input)
         (output-upper ,output-upper)
         (output-lower ,output-lower))
     ;; index 0 in an empty list
     (should-error (funcall fun 0 nil))
     (should-error (funcall fun 0 nil t))
     (should-not (funcall fun 0 nil 'permit-empty))
     ;; overrange in empty list
     (should-error (funcall fun 100 nil))
     (should-error (funcall fun 100 nil t))
     (should-not (funcall fun 100 nil 'permit-empty))
     ;; underrange in empty list
     (should-error (funcall fun -100 nil))
     (should-error (funcall fun -100 nil t))
     (should-not (funcall fun -100 nil 'permit-empty))
     ;; positive in finite list
     (should (equal output-lower (funcall fun 0 input)))
     ;; negative in finite list
     (should (equal output-upper (funcall fun -1 input)))
     ;; positive overrange in finite list
     (should (equal output-upper (funcall fun 100 input t)))
     (should-error (funcall fun 100 input))
     ;; negative underrange in finite list
     (should (equal output-lower (funcall fun -100 input t)))
     (should-error (funcall fun -100 input))))

(ert-deftest org-ml--remove-at/properties ()
  (org-ml--intra-list-ops-test #'org-ml--remove-at '(1 2 3) '(1 2) '(2 3)))

(ert-deftest org-ml--replace-at/properties ()
  (org-ml--intra-list-ops-test (lambda (n list &optional p)
                             (org-ml--replace-at n 'x list p))
    '(1 2 3) '(1 2 x) '(x 2 3)))

(ert-deftest org-ml--nth/properties ()
  (org-ml--intra-list-ops-test #'org-ml--nth '(1 2 3) 3 1))

;;; list functors

(ert-deftest org-ml--map-first/last/properties ()
  ;; mapping empty list should always return empty list
  (should-not (org-ml--map-first* (s-upcase it) nil))
  (should-not (org-ml--map-last* (s-upcase it) nil))
  ;; mapping list with one member should be same for both
  (should (equal '("X") (org-ml--map-first* (s-upcase it) '("x"))))
  (should (equal '("X") (org-ml--map-last* (s-upcase it) '("x"))))
  ;; mapping list with more than one member should be self-explanatory
  (should (equal '("A" "b" "c") (org-ml--map-first* (s-upcase it) '("a" "b" "c"))))
  (should (equal '("a" "b" "C") (org-ml--map-last* (s-upcase it) '("a" "b" "c"))))
  ;; identity should hold true for any length list (0, 1, and 1+)
  (let ((test-lists '(nil (1) (1 2))))
    (--each test-lists (should (equal it (org-ml--map-first* (identity it) it))))
    (--each test-lists (should (equal it (org-ml--map-last* (identity it) it)))))

  )


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

(defun org-ml--equal (exclude-props node1 node2)
  (cond
   ((and (stringp node1) (stringp node2))
    (equal node1 node2))
   ((and (consp node1) (consp node2))
    (-let (((type1 . (props1 . children1)) node1)
           ((type2 . (props2 . children2)) node2))
      (and (eq type1 type2)
           (org-ml--plist-equal-p exclude-props props1 props2)
           (->> (-zip-fill nil children1 children2)
                (--all? (org-ml--equal exclude-props (car it) (cdr it)))))))))

(defun org-ml--test-from-string (omit-props node string)
  (let ((props (append omit-props '(:begin :contents-begin :end :contents-end :parent :post-affiliated)))
        (type (org-ml-get-type node)))
    (should (org-ml--equal props node (org-ml-from-string type string)))))

(ert-deftest org-ml-from-string/non-nil ()
  (org-ml--test-from-string '(:value) (org-ml-build-babel-call "name") "#+call: name()")
  (org-ml--test-from-string nil (org-ml-build-bold "bold") "*bold*")
  (org-ml--test-from-string nil (org-ml-build-center-block) "#+begin_center\n#+end_center")
  ;; TODO cannot compare the values currently
  ;; (org-ml--test-from-string nil (org-ml-build-clock! '(2020 1 1 0 0)) "CLOCK: [2020-01-01 Tue 00:00]")
  (org-ml--test-from-string nil (org-ml-build-code "code") "~code~")
  (org-ml--test-from-string nil (org-ml-build-comment "comment") "# comment")
  (org-ml--test-from-string nil (org-ml-build-comment-block) "#+begin_comment\n#+end_comment")
  (org-ml--test-from-string nil (org-ml-build-drawer "DRAW") ":DRAW:\n:END:")
  (org-ml--test-from-string nil (org-ml-build-diary-sexp :value '(print 'hi)) "%%(print 'hi)")
  (org-ml--test-from-string nil (org-ml-build-dynamic-block "name") "#+begin: name\n#+end")
  (org-ml--test-from-string '(:latex :latex-math-p :ascii :html :latin1 :utf-8)
                            (org-ml-build-entity "pi") "\\pi")
  (org-ml--test-from-string '(:value :retain-labels :use-labels) (org-ml-build-example-block) "#+begin_example\n#+end_example")
  (org-ml--test-from-string nil (org-ml-build-export-block "TYPE" "value\n") "#+begin_export TYPE\nvalue\n#+end_export")
  (org-ml--test-from-string nil (org-ml-build-export-snippet "be" "val") "@@be:val@@")
  (org-ml--test-from-string nil (org-ml-build-fixed-width "val") ": val")
  ;; TODO add footnote definition
  ;; TODO add footnote ref
  (org-ml--test-from-string '(:raw-value) (org-ml-build-headline! :title-text "headline") "* headline")
  (org-ml--test-from-string nil (org-ml-build-horizontal-rule) "------")
  (org-ml--test-from-string '(:value) (org-ml-build-inline-babel-call "ktulu") "call_ktulu()")
  (org-ml--test-from-string '(:value) (org-ml-build-inline-src-block "python") "src_python{}")
  (org-ml--test-from-string nil (org-ml-build-italic "italic") "/italic/")
  (org-ml--test-from-string '(:structure) (org-ml-build-item! :paragraph "item") "- item")
  (org-ml--test-from-string nil (org-ml-build-keyword "K" "v") "#+K: v")
  ;; TODO add latex env
  ;; TODO add latex frag
  (org-ml--test-from-string '(:raw-link :format) (org-ml-build-link "//example.com" :type "https") "https://example.com")
  ;; TODO add macro
  ;; TODO add node-property
  (org-ml--test-from-string nil (org-ml-build-paragraph! "para") "para")
  (org-ml--test-from-string '(:structure :type) (org-ml-build-plain-list (org-ml-build-item! :paragraph "item")) "- item")
  ;; TODO add planing
  ;; TODO add property drawer
  ;; TODO add radio target
  (org-ml--test-from-string nil (org-ml-build-section (org-ml-build-paragraph! "sec")) "sec")
  ;; TODO add special block
  ;; TODO add src block
  ;; TODO add stats cookie
  (org-ml--test-from-string nil (org-ml-build-strike-through "s") "+s+")
  ;; TODO add subscript
  ;; TODO add superscript
  ;; TODO add table
  ;; TODO add table-cell
  ;; TODO add table-row
  ;; TODO add target
  ;; TODO timestamp does not have all props so can't compare :(
  ;; (org-ml--test-from-string '(:raw-value) (org-ml-build-timestamp! '(2020 1 1)) "[2020-01-01 Tue]")
  (org-ml--test-from-string nil (org-ml-build-underline "u") "_u_")
  (org-ml--test-from-string nil (org-ml-build-verbatim "b") "=b=")
  ;; TODO add verse block
)

;;; PARSING INVERSION

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
  (let ((contents-list (--map (if (consp it) (s-join "\n" it) it)
                              contents-list)))
    (--each contents-list
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

;; leaf object nodes

(ert-deftest org-ml--test-parse-inversion/code ()
  (org-ml--test-contents-parse-inversion 'code #'org-ml-parse-object-at
    (list "~code~")))

(ert-deftest org-ml--test-parse-inversion/entity ()
  (org-ml--test-contents-parse-inversion 'entity #'org-ml-parse-object-at
    (list "\\pi" "\\pi{}")))

(ert-deftest org-ml--test-parse-inversion/export-snippet ()
  (org-ml--test-contents-parse-inversion 'export-snippet #'org-ml-parse-object-at
    (list "@@x:y@@")))

(ert-deftest org-ml--test-parse-inversion/inline-babel-call ()
  (org-ml--test-contents-parse-inversion 'inline-babel-call #'org-ml-parse-object-at
    (list "call_ktulu()"
          "call_ktulu(n=1)"
          "call_ktulu[:x y]()"
          "call_ktulu[:x y](n=1)"
          "call_ktulu()[:a b]"
          "call_ktulu(n=1)[:a b]"
          "call_ktulu[:x y]()[:a b]"
          "call_ktulu[:x y](n=1)[:a b]")))

(ert-deftest org-ml--test-parse-inversion/inline-src-block ()
  (org-ml--test-contents-parse-inversion 'inline-src-block #'org-ml-parse-object-at
    (list "src_python{}"
          "src_python{print \"yo\"}"
          "src_python[:x y]{}"
          "src_python[:x y]{print \"yo\"}")))

(ert-deftest org-ml--test-parse-inversion/line-break ()
  (org-ml--test-contents-parse-inversion 'line-break #'org-ml-parse-object-at
    (list "\\\\\n")))

(ert-deftest org-ml--test-parse-inversion/latex-fragment ()
  (org-ml--test-contents-parse-inversion 'latex-fragment #'org-ml-parse-object-at
    (list "$2+2=5$")))

(ert-deftest org-ml--test-parse-inversion/macro ()
  (org-ml--test-contents-parse-inversion 'macro #'org-ml-parse-object-at
    (list "{{{key}}}"
          "{{{key(x=4)}}}")))

(ert-deftest org-ml--test-parse-inversion/statistics-cookie ()
  (org-ml--test-contents-parse-inversion 'statistics-cookie #'org-ml-parse-object-at
    (list "[/]"
          "[0/0]"
          "[%]"
          "[0%]")))

(ert-deftest org-ml--test-parse-inversion/timestamp ()
  (org-ml--test-contents-parse-inversion 'timestamp #'org-ml-parse-object-at
    (list "[2019-01-01 Tue]"
          "[2019-01-01 Tue 12:00]"
          ;; "[2019-01-01 Tue 12:00-13:00]" TODO this doesn't parse correctly
          "[2019-01-01 Tue]--[2019-01-02 Wed]"
          "<2019-01-01 Tue>"
          "[2019-01-01 Tue +1d]"
          "[2019-01-01 Tue -1y]"
          "[2019-01-01 Tue +1d -1y]")))

(ert-deftest org-ml--test-parse-inversion/verbatim ()
  (org-ml--test-contents-parse-inversion 'verbatim #'org-ml-parse-object-at
    (list "=verbatim=")))

(ert-deftest org-ml--test-parse-inversion/plain-text ()
  (org-ml--test-contents-parse-inversion 'plain-text #'org-ml-parse-object-at
    (list "plain-text"
          ;; all syntax chars by themselves should be plain-text
          "**" "~~" "@@:@@" "//" "[]" "[[]]" "{{{}}}" "<>" "<<>>"
          "<<<>>>" "++" "^" "_" "__" "==")))

;; branch object nodes

(ert-deftest org-ml--test-parse-inversion/bold ()
  (org-ml--test-contents-parse-inversion 'bold #'org-ml-parse-object-at
    (list "*bold*")))

(ert-deftest org-ml--test-parse-inversion/footnote-reference ()
  (org-ml--test-contents-parse-inversion 'footnote-reference #'org-ml-parse-object-at
    (list "[fn:label]" "[fn:label:nodes]")
    " "))

(ert-deftest org-ml--test-parse-inversion/italic ()
  (org-ml--test-contents-parse-inversion 'italic #'org-ml-parse-object-at
    (list "/italic/")))

(ert-deftest org-ml--test-parse-inversion/link ()
  (org-ml--test-contents-parse-inversion 'link #'org-ml-parse-object-at
    ;; this is not exhaustive but hopefully good enough
    (list "https://downloadmoreram.com"
          "mailto:vladimirputin@pwned.ru"
          "file:/home/kalilinux/pwneddata"
          "<https://downloadmoreram.com>"
          "[[https://downloadmoreram.com]]"
          "[[https://downloadmoreram.com][legit advice]]")))

(ert-deftest org-ml--test-parse-inversion/radio-target ()
  (org-ml--test-contents-parse-inversion 'radio-target #'org-ml-parse-object-at
    (list "<<<radio>>>")))

(ert-deftest org-ml--test-parse-inversion/strike-through ()
  (org-ml--test-contents-parse-inversion 'strike-through #'org-ml-parse-object-at
    (list "+strike+")))

(ert-deftest org-ml--test-parse-inversion/subscript ()
  (org-ml--test-contents-parse-inversion 'subscript #'org-ml-parse-object-at
    (list "_sub" "_{sub}")
    "dummy"))

(ert-deftest org-ml--test-parse-inversion/superscript ()
  (org-ml--test-contents-parse-inversion 'superscript #'org-ml-parse-object-at
    (list "^super" "^{super}")
    "dummy"))

(ert-deftest org-ml--test-parse-inversion/table-cell ()
  (org-ml--test-contents-parse-inversion 'table-cell #'org-ml-parse-object-at
    (list " cell |")
    "|"))

;; leaf element nodes

(ert-deftest org-ml--test-parse-inversion/babel-call ()
  (org-ml--test-contents-parse-inversion 'babel-call #'org-ml-parse-element-at
    (list "#+call: name()\n"
          "#+call: name(x=1)\n"
          "#+call: name[:x y](x=1)\n"
          "#+call: name[:x y]()\n"
          "#+call: name[:x y](x=1) :a b\n"
          "#+call: name[:x y]() :a b\n"
          "#+call: name[]() :a b\n")))

(ert-deftest org-ml--test-parse-inversion/clock ()
  (org-ml--test-contents-parse-inversion 'clock #'org-ml-parse-element-at
    (list "CLOCK: [2019-01-01 Tue]\n"
          "CLOCK: [2019-01-01 Tue]--[2019-01-02 Wed] => 24:00\n"
          ;; TODO this doesn't work
          ;; "CLOCK: [2019-01-01 Tue 00:00-01:00] =>  1:00\n"
          )))

(ert-deftest org-ml--test-parse-inversion/comment ()
  (org-ml--test-contents-parse-inversion 'comment #'org-ml-parse-element-at
    (list "# one\n"
          '("# one"
            "# two\n")
          ;; TODO this doesn't work
          ;; "#\n"
          )))

(ert-deftest org-ml--test-parse-inversion/comment-block ()
  (org-ml--test-contents-parse-inversion 'comment-block #'org-ml-parse-element-at
    (list '("#+begin_comment"
            "battle of being"
            "#+end_comment\n")
          '("#+begin_comment"
            "#+end_comment\n"))))

(ert-deftest org-ml--test-parse-inversion/diary-sexp ()
  (org-ml--test-contents-parse-inversion 'diary-sexp #'org-ml-parse-element-at
    (list "%%()\n" "%%(whatever)\n")))

(ert-deftest org-ml--test-parse-inversion/example-block ()
  (org-ml--test-contents-parse-inversion 'example-block #'org-ml-parse-element-at
    (list '("#+begin_example"
            "  example.com"
            "#+end_example\n")
          '("#+begin_example"
            "#+end_example\n"))))

(ert-deftest org-ml--test-parse-inversion/export-block ()
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
;; (ert-deftest org-ml--test-parse-inversion/fixed-width ()
;;   (org-ml--test-contents-parse-inversion 'fixed-width #'org-ml-parse-element-at
;;     (list ": crucifixed"
;;           ;; TODO this make a blank
;;           ;; ":\n"
;;           )))

(ert-deftest org-ml--test-parse-inversion/horizontal-rule ()
  (org-ml--test-contents-parse-inversion 'horizontal-rule #'org-ml-parse-element-at
    (list "-----\n")))

(ert-deftest org-ml--test-parse-inversion/keyword ()
  (org-ml--test-contents-parse-inversion 'keyword #'org-ml-parse-element-at
    (list "#+key: val\n"
          ;; TODO this randomly fails
          ;; "#+KEY:\n"
          "#+key: \n")))

(ert-deftest org-ml--test-parse-inversion/latex-environment ()
  (org-ml--test-contents-parse-inversion 'latex-environment #'org-ml-parse-element-at
    (list '("\\begin{env}"
            "\\end{env}\n")
          '("\\begin{env}"
            "latex >>> ms word"
            "\\end{env}\n"))))

(ert-deftest org-ml--test-parse-inversion/node-property ()
  (org-ml--test-contents-parse-inversion 'node-property #'org-ml-parse-element-at
    (list ":node:     prop\n"
          ;; TODO this seems arbitrary
          ;; ":node\n"
          ":node:     \n")
    "* dummy\n:PROPERTIES:\n"
    ":END:\n"))

(ert-deftest org-ml--test-parse-inversion/planning ()
  (org-ml--test-contents-parse-inversion 'planning #'org-ml-parse-element-at
    (list "CLOSED: <2019-01-01 Tue>\n"
          "CLOSED: <2019-01-01 Tue +1d>\n"
          "CLOSED: <2019-01-01 Tue -1y>\n"
          "CLOSED: <2019-01-01 Tue +1d -1y>\n")
    "* dummy\n"))

(ert-deftest org-ml--test-parse-inversion/src-block ()
  (org-ml--test-contents-parse-inversion 'src-block #'org-ml-parse-element-at
    (list '("#+begin_src"
            "#+end_src\n")
          ;; TODO this doesn't work if is isn't indented
          '("#+begin_src python -n :x y"
            "  print \"yo\""
            "#+end_src\n"))))

;;; branch element nodes with child object nodes

(ert-deftest org-ml--test-parse-inversion/paragraph ()
  (org-ml--test-contents-parse-inversion 'paragraph #'org-ml-parse-element-at
    ;; TODO there are probably other things I could put here
    (list "paragraph\n")))

(ert-deftest org-ml--test-parse-inversion/table-row ()
  (org-ml--test-contents-parse-inversion 'table-row #'org-ml-parse-table-row-at
    (list "| cell |\n"
          ;; TODO this makes an empty string
          ;; "| |\n"
          )))

(ert-deftest org-ml--test-parse-inversion/verse-block ()
  (org-ml--test-contents-parse-inversion 'verse-block #'org-ml-parse-element-at
    (list '("#+begin_verse"
            "#+end_verse\n")
          '("#+begin_verse"
            "Once upon a midnight dreary..."
            "#+end_verse\n"))))

;;; branch element nodes with child element nodes

(ert-deftest org-ml--test-parse-inversion/center-block ()
  (org-ml--test-contents-parse-inversion 'center-block #'org-ml-parse-element-at
    (list '("#+begin_center"
            "#+end_center\n")
          '("#+begin_center"
            "Of the universe..."
            "#+end_center\n"))))

(ert-deftest org-ml--test-parse-inversion/drawer ()
  (org-ml--test-contents-parse-inversion 'drawer #'org-ml-parse-element-at
    (list '(":LOGBOOK:"
            ":END:\n")
          '(":LOGBOOK:"
            "- logged thingy"
            ":END:\n"))))

(ert-deftest org-ml--test-parse-inversion/dynamic-block ()
  (org-ml--test-contents-parse-inversion 'dynamic-block #'org-ml-parse-element-at
    (list '("#+begin: name"
            "#+end:\n")
          '("#+begin: name"
            "Random contents..."
            "#+end:\n"))))

(ert-deftest org-ml--test-parse-inversion/footnote-definition ()
  ;; TODO blanks are apparently not allowed and will error
  (org-ml--test-contents-parse-inversion 'footnote-definition #'org-ml-parse-element-at
    (list ;; "[fn:label] \n"
          ;; TODO needs a random space at the end
          ;; "[fn:label]"
          "[fn:label] stuff after\n"
          )))

(ert-deftest org-ml--test-parse-inversion/headline ()
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

(ert-deftest org-ml--test-parse-inversion/item ()
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

(ert-deftest org-ml--test-parse-inversion/plain-list ()
  (org-ml--test-contents-parse-inversion 'plain-list #'org-ml-parse-element-at
    (list "- thing\n"
          "1. thing\n"
          '("- thing"
            "- more thing\n"))))


(ert-deftest org-ml--test-parse-inversion/property-drawer ()
  (org-ml--test-contents-parse-inversion 'property-drawer #'org-ml-parse-element-at
    (list '(":PROPERTIES:"
            ":END:\n")
          '(":PROPERTIES:"
            ":Effort:   0:30"
            ":END:\n"))
    "* dummy\n"))

(ert-deftest org-ml--test-parse-inversion/quote-block ()
  (org-ml--test-contents-parse-inversion 'quote-block #'org-ml-parse-element-at
    (list '("#+begin_quote"
            "#+end_quote\n")
          '("#+begin_quote"
            "Fear is the mind killer..."
            "#+end_quote\n"))))

(ert-deftest org-ml--test-parse-inversion/section ()
  (org-ml--test-contents-parse-inversion 'section #'org-ml-parse-section-at
    (list "things that could be a paragraph\n"
          "#+key: val\n"
          "# nothing important...\n")))

(ert-deftest org-ml--test-parse-inversion/special-block ()
  (org-ml--test-contents-parse-inversion 'special-block #'org-ml-parse-element-at
    (list '("#+begin_special"
            "#+end_special\n")
          '("#+begin_special"
            "You don't belong here"
            "#+end_special\n"))))

(ert-deftest org-ml--test-parse-inversion/table ()
  (org-ml--test-contents-parse-inversion 'table #'org-ml-parse-element-at
    (list "| simple |\n"
          "| less | simple |\n"
          '("| R | A |"
            "| G | E |\n")
          ;; TODO this makes a blank string
          ;; "| |\n"
          )))

;;; NODE PROPERTY COMPLETENESS

(defun should-have-equal-properties (e1 e2)
  (unless (eq (org-ml-get-type e1) (org-ml-get-type e2))
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

(defun org-ml--compare-object-props (elem string)
  (should-have-equal-properties
   elem
   (->> (org-ml--from-string (concat " " string))
        (org-ml--get-descendent '(0 1)))))

(ert-deftest org-ml--code/valid-props ()
  (org-ml--compare-object-props
   (org-ml-build-code "value") "~code~"))

(ert-deftest org-ml--entity/valid-props ()
  (org-ml--compare-object-props
   (org-ml-build-entity "pi") "\\pi"))

(ert-deftest org-ml--export-snippet/valid-props ()
  (org-ml--compare-object-props
   (org-ml-build-export-snippet "backend" "value") "@@im:padme@@"))

(ert-deftest org-ml--inline-babel-call/valid-props ()
  (org-ml--compare-object-props
   (org-ml-build-inline-babel-call "name") "call_name()"))

(ert-deftest org-ml--inline-src-block/valid-props ()
  (org-ml--compare-object-props
   (org-ml-build-inline-src-block "lang") "src_lang{value}"))

;; TODO add latex fragment

(ert-deftest org-ml--line-break/valid-props ()
  (org-ml--compare-object-props
   (org-ml-build-line-break) "\\\\\n"))

(ert-deftest org-ml--macro/valid-props ()
  (org-ml--compare-object-props
   (org-ml-build-macro "value") "{{{value}}}"))

(ert-deftest org-ml--statistics-cookie/valid-props ()
  (org-ml--compare-object-props
   (org-ml-build-statistics-cookie '(1)) "[/]"))

(ert-deftest org-ml--target/valid-props ()
  (org-ml--compare-object-props
   (org-ml-build-target "value") "<<value>>"))

(ert-deftest org-ml--timestamp/valid-props ()
  (org-ml--compare-object-props
   (org-ml-build-timestamp! '(2019 1 1))
   ;; TODO the timestamp parser does not add properties for warnings
   ;; or repeaters if they are not given, this appears to be a bug
   "[2019-01-01 Tue +1d -1d]"))

(ert-deftest org-ml--verbatim/valid-props ()
  (org-ml--compare-object-props
   (org-ml-build-verbatim "value") "=value="))

;; recursive objects

(ert-deftest org-ml--bold/valid-props ()
  (org-ml--compare-object-props
   (org-ml-build-bold) "*bold*"))

(ert-deftest org-ml--footnote-reference/valid-props ()
  (org-ml--compare-object-props
   (org-ml-build-footnote-reference) "[fn:1]"))

(ert-deftest org-ml--italic/valid-props ()
  (org-ml--compare-object-props
   (org-ml-build-italic) "/italic/"))

(ert-deftest org-ml--link/valid-props ()
  (org-ml--compare-object-props
   (org-ml-build-link "path") "[[path]]"))

(ert-deftest org-ml--radio-target/valid-props ()
  (org-ml--compare-object-props
   (org-ml-build-radio-target) "<<<target>>>"))

(ert-deftest org-ml--strike-through/valid-props ()
  (org-ml--compare-object-props
   (org-ml-build-strike-through) "+bad+"))

(ert-deftest org-ml--superscript/valid-props ()
  (should-have-equal-properties
   (org-ml-build-superscript)
   (->> (org-ml--from-string "thisis^super")
        (org-ml--get-descendent '(0 1)))))

(ert-deftest org-ml--subscript/valid-props ()
  (should-have-equal-properties
   (org-ml-build-subscript)
   (->> (org-ml--from-string "thisis_subpar")
        (org-ml--get-descendent '(0 1)))))

(ert-deftest org-ml--table-cell/valid-props ()
  (should-have-equal-properties
   (org-ml-build-table-cell "cell")
   (->> (org-ml--from-string "| cell |")
        (org-ml--get-descendent '(0 0 0)))))

(ert-deftest org-ml--underline/valid-props ()
  (org-ml--compare-object-props
   (org-ml-build-underline) "_bad_"))

;; elements

(defun org-ml--compare-element-props (elem string)
  (should-have-equal-properties
   elem
   (->> (org-ml--from-string string)
        (org-ml--get-descendent '(0)))))

(ert-deftest org-ml--babel-call/valid-props ()
  (org-ml--compare-element-props
   (org-ml-build-babel-call "call") "#+call: name()"))

(ert-deftest org-ml--clock/valid-props ()
  (org-ml--compare-element-props
   (org-ml-build-clock (org-ml-build-timestamp! '(2019 1 1)))
   "CLOCK: [2019-01-01 Tue]"))

(ert-deftest org-ml--comment/valid-props ()
  (org-ml--compare-element-props
   (org-ml-build-comment "useless") "# useless"))

(ert-deftest org-ml--comment-block/valid-props ()
  (org-ml--compare-element-props
   (org-ml-build-comment-block)
   "#+begin_comment\nuseless\n#+end_comment"))

(ert-deftest org-ml--diary-sexp/valid-props ()
  (org-ml--compare-element-props
   (org-ml-build-diary-sexp) "%%()"))

(ert-deftest org-ml--example-block/valid-props ()
  (org-ml--compare-element-props
   (org-ml-build-example-block)
   "#+begin_example\nuseless\n#+end_example"))

(ert-deftest org-ml--export-block/valid-props ()
  (org-ml--compare-element-props
   (org-ml-build-export-block "type" "value")
   "#+begin_export type\nuseless\n#+end_export"))

(ert-deftest org-ml--fixed-width/valid-props ()
  (org-ml--compare-element-props
   (org-ml-build-fixed-width "value") ": value"))

(ert-deftest org-ml--horizontal-rule/valid-props ()
  (org-ml--compare-element-props
   (org-ml-build-horizontal-rule) "-----"))

(ert-deftest org-ml--keyword/valid-props ()
  (org-ml--compare-element-props
   (org-ml-build-keyword "key" "val") "#+KEY: val"))

(ert-deftest org-ml--latex-environment/valid-props ()
  (org-ml--compare-element-props
   (org-ml-build-latex-environment '("gloves" "text"))
   "\\begin{env}\nvalue\n\\end{env}"))

(ert-deftest org-ml--node-property/valid-props ()
  (should-have-equal-properties
   (org-ml-build-node-property "key" "value")
   (->> (org-ml--from-string "* dummy\n:PROPERTIES:\n:key: val\n:END:")
        (org-ml--get-descendent '(0 0 0)))))

(ert-deftest org-ml--planning/valid-props ()
  (should-have-equal-properties
   (org-ml-build-planning :closed (org-ml-build-timestamp! '(2019 1 1) :active t))
   (->> (org-ml--from-string "* dummy\nCLOSED: <2019-01-01 Tue>")
        (org-ml--get-descendent '(0 0)))))

(ert-deftest org-ml--src-block/valid-props ()
  (org-ml--compare-element-props
   (org-ml-build-src-block)
   "#+begin_src\nuseless\n#+end_src"))

;; containers

(ert-deftest org-ml--paragraph/valid-props ()
  (should-have-equal-properties
   (org-ml-build-paragraph)
   (->> (org-ml--from-string "text")
        (org-ml--get-descendent '(0)))))

(ert-deftest org-ml--table-row/valid-props ()
  (should-have-equal-properties
   (org-ml-build-table-row)
   (->> (org-ml--from-string "| row |")
        (org-ml--get-descendent '(0 0)))))

(ert-deftest org-ml--verse-block/valid-props ()
  (should-have-equal-properties
   (org-ml-build-verse-block)
   (->> (org-ml--from-string "#+begin_verse\nthing\n#+end_verse")
        (org-ml--get-descendent '(0)))))

(ert-deftest org-ml--center-block/valid-props ()
  (org-ml--compare-element-props
   (org-ml-build-center-block)
   "#+begin_center\nuseless\n#+end_center"))

(ert-deftest org-ml--drawer/valid-props ()
  (org-ml--compare-element-props
   (org-ml-build-drawer "name")
   ":LOGBOOK:\nuseless\n:END:"))

(ert-deftest org-ml--dynamic-block/valid-props ()
  (org-ml--compare-element-props
   (org-ml-build-dynamic-block "name" :arguments '(:key val))
   "#+begin: name args\nuseless\n#+end:"))

(ert-deftest org-ml--footnote-definition/valid-props ()
  (org-ml--compare-element-props
   (org-ml-build-footnote-definition "label") "[fn:label]\n"))

(ert-deftest org-ml--headline/valid-props ()
  (should-have-equal-properties
   (org-ml-build-headline)
   (org-ml--from-string "* head")))

(ert-deftest org-ml--item/valid-props ()
  (should-have-equal-properties
   (org-ml-build-item)
   (->> (org-ml--from-string "- head")
        (org-ml--get-descendent '(0 0)))))

(ert-deftest org-ml--plain-list/valid-props ()
  (org-ml--compare-element-props
   (org-ml-build-plain-list) "- item"))

(ert-deftest org-ml--property-drawer/valid-props ()
  (should-have-equal-properties
   (org-ml-build-property-drawer)
   (->> (org-ml--from-string "* dummy\n:PROPERTIES:\n:END:")
        (org-ml--get-descendent '(0 0)))))

(ert-deftest org-ml--quote-block/valid-props ()
  (org-ml--compare-element-props
   (org-ml-build-quote-block)
   "#+begin_quote\n#+end_quote"))

(ert-deftest org-ml--section/valid-props ()
  (org-ml--compare-element-props
   (org-ml-build-section) "* dummy\nstuff"))

(ert-deftest org-ml--special-block/valid-props ()
  (org-ml--compare-element-props
   (org-ml-build-special-block "type")
   "#+begin_type:\n#+end_type:"))

(ert-deftest org-ml--table/valid-props ()
  (org-ml--compare-element-props
   (org-ml-build-table) "| table |"))

(defun should-equal (string elem)
  (should (equal string (->> (org-ml-to-string elem) (s-trim)))))

(defun should-match (regexp elem)
  (should (s-match regexp (->> (org-ml-to-string elem) (s-trim)))))

(ert-deftest org-ml--make-header ()
  (should (equal (org-ml--make-header '("docstring" (print 'hi)) nil)
                 "docstring\n\n(fn)"))
  (should (equal (org-ml--make-header '("docstring" (print 'hi)) '(one))
                 "docstring\n\n(fn ONE)"))
  (should (equal (org-ml--make-header '("docstring" (print 'hi)) '(one two))
                 "docstring\n\n(fn ONE TWO)")))

(ert-deftest org-ml--make-kwarg-let/error ()
  ;; list too long
  (should-error (org-ml--make-kwarg-let '(one two three)))
  ;; keyword slot must be a real keyword
  (should-error (org-ml--make-kwarg-let '((one two))))
  (should-error (org-ml--make-kwarg-let '((one two) three)))
  ;; single arg must be a symbol but not a keyword
  ;; TODO the keyword guard does not work yet
  ;; (should-error (org-ml--make-kwarg-let :one))
  (should-error (org-ml--make-kwarg-let 1))
  (should-error (org-ml--make-kwarg-let "one"))
  (should-error (org-ml--make-kwarg-let '(1))))

(ert-deftest org-ml--make-rest-partition-form/restargs ()
  (should (equal '(nil . (one)) (org-ml--make-rest-partition-form '(one) nil t)))
  (should (equal '(nil . (one two)) (org-ml--make-rest-partition-form '(one two) nil t))))

(ert-deftest org-ml--make-rest-partition-form/error ()
  ;; invalid keywords
  (should-error (org-ml--make-rest-partition-form '(:one one) '(:two) nil))
  ;; too many arguments
  (should-error (org-ml--make-rest-partition-form '(:one one two) (:one) nil))
  ;; multiple keywords
  (should-error (org-ml--make-rest-partition-form '(:one one :one three two) (:one) nil)))

;;; SUPERCONTENTS FRAMEWORK TESTING

(ert-deftest org-ml--merge-logbook ()
  (let* ((enconf (org-ml--scc-encode nil))
         (enconf-notes (org-ml--scc-encode '(:clock-out-notes t)))
         (c1 (org-ml-build-clock! '(2020 1 1 0 0) :end '(2020 1 1 1 0)))
         (i1 (org-ml-build-log-note (org-ml-time-to-unixtime '(2020 1 2 0 0)) "1"))
         (c2 (org-ml-build-clock! '(2020 1 3 0 0) :end '(2020 1 3 1 0)))
         (i2 (org-ml-build-log-note (org-ml-time-to-unixtime '(2020 1 4 0 0)) "2"))
         (n1 (org-ml-build-item! :paragraph "clock note"))
         (p1 (org-ml-build-plain-list i1))
         (p2 (org-ml-build-plain-list i2))
         (pn1 (org-ml-build-plain-list n1))
         (p1n1 (org-ml-build-plain-list n1 i1))
         ;; there should never be a p12 analogue since that's not the right order
         (p21 (org-ml-build-plain-list i2 i1))
         (x1 (org-ml-build-code "I should cause a fatal error")))
    ;; no clock notes
    ;;
    ;; nothing
    (should (equal nil
                   (org-ml--merge-logbook enconf nil nil)))
    ;; just clocks
    (should (equal `(,c2 ,c1)
                   (org-ml--merge-logbook enconf nil `(,c1 ,c2))))
    ;; just items
    (should (equal `(,p21)
                   (org-ml--merge-logbook enconf `(,i1 ,i2) nil)))
    ;; single clock and item
    (should (equal `(,p1 ,c1)
                   (org-ml--merge-logbook enconf `(,i1) `(,c1))))
    ;; clocks and items
    (should (equal `(,p2 ,c2 ,p1 ,c1)
                   (org-ml--merge-logbook enconf `(,i1 ,i2) `(,c1 ,c2))))
    ;; just clocks (note)
    (should-error (org-ml--merge-logbook enconf nil `(,c1 ,n1 ,c2)))
    ;; single clock (note) and item
    (should-error (org-ml--merge-logbook enconf `(,i1) `(,c1 ,n1)))
    ;; clocks (note) and items
    (should-error (org-ml--merge-logbook enconf `(,i1 ,i2) `(,c1 ,n1 ,c2)))
    ;; just clocks (note in wrong place)
    (should-error (org-ml--merge-logbook enconf nil `(,n1 ,c1 ,c2)))
    ;; just garbage (items)
    (should-error (org-ml--merge-logbook enconf `(,x1) nil))
    ;; just garbage (clocks)
    (should-error (org-ml--merge-logbook enconf nil `(,x1)))
    ;; clock notes
    ;;
    ;; nothing
    (should (equal nil
                   (org-ml--merge-logbook enconf-notes nil nil)))
    ;; just clocks
    (should (equal `(,c2 ,c1)
                   (org-ml--merge-logbook enconf-notes nil `(,c1 ,c2))))
    ;; just items
    (should (equal `(,p21)
                   (org-ml--merge-logbook enconf-notes `(,i1 ,i2) nil)))
    ;; single clock and item
    (should (equal `(,p1 ,c1)
                   (org-ml--merge-logbook enconf-notes `(,i1) `(,c1))))
    ;; clocks and items
    (should (equal `(,p2 ,c2 ,p1 ,c1)
                   (org-ml--merge-logbook enconf-notes `(,i1 ,i2) `(,c1 ,c2))))
    ;; just clocks (note)
    (should (equal `(,c2 ,c1 ,pn1)
                   (org-ml--merge-logbook enconf-notes nil `(,c1 ,n1 ,c2))))
    ;; single clock (note) and item
    (should (equal `(,p1 ,c1 ,pn1)
             (org-ml--merge-logbook enconf-notes `(,i1) `(,c1 ,n1))))
    ;; clocks (note) and items
    (should (equal `(,p2 ,c2 ,p1 ,c1 ,pn1)
                   (org-ml--merge-logbook enconf-notes `(,i1 ,i2) `(,c1 ,n1 ,c2))))
    ;; clocks (note) and items (different order)
    (should (equal `(,p2 ,c2 ,p1n1 ,c1)
                   (org-ml--merge-logbook enconf-notes `(,i1 ,i2) `(,c1 ,c2 ,n1))))
    ;; just clocks (note in wrong place)
    (should-error (org-ml--merge-logbook enconf-notes nil `(,n1 ,c1 ,c2)))
    ;; just garbage (items)
    (should-error (org-ml--merge-logbook enconf-notes `(,x1) nil))
    ;; just garbage (clocks)
    (should-error (org-ml--merge-logbook enconf-notes nil `(,x1)))))

(ert-deftest org-ml--separate-logbook ()
  (cl-flet
      ((test
        (c m items clocks unknown in)
        ;; (print (format "%S" (org-ml--separate-logbook c m in)))
        (should (equal (list items clocks unknown)
                       (org-ml--separate-logbook c m in)))))
    (let* ((enconf (org-ml--scc-encode nil))
           (enconf-notes (org-ml--scc-encode '(:clock-out-notes t)))
           (c1 (org-ml-build-clock! '(2020 1 1 0 0) :end '(2020 1 1 1 0)))
           (i1 (org-ml-build-log-note (org-ml-time-to-unixtime '(2020 1 2 0 0)) "1"))
           (c2 (org-ml-build-clock! '(2020 1 3 0 0) :end '(2020 1 3 1 0)))
           (i2 (org-ml-build-log-note (org-ml-time-to-unixtime '(2020 1 4 0 0)) "2"))
           (n1 (org-ml-build-item! :paragraph "clock note"))
           (p1 (org-ml-build-plain-list i1))
           (p2 (org-ml-build-plain-list i2))
           (pn1 (org-ml-build-plain-list n1))
           (pn11 (org-ml-build-plain-list n1 i1))
           (p1n1 (org-ml-build-plain-list i1 n1))
           (p12 (org-ml-build-plain-list i1 i2))
           (x1 (org-ml-build-code "I should cause a fatal error")))

      ;; MODE = MIXED
      ;;
      ;; no clock notes
      ;;
      ;; nothing
      (test enconf :mixed nil nil nil nil)
      ;; single item
      (test enconf :mixed `(,i1) nil nil `(,p1))
      ;; single clock
      (test enconf :mixed nil `(,c1) nil `(,c1))
      ;; single garbage entry
      (test enconf :mixed nil nil `(,x1) `(,x1))
      ;; single item and clock
      (test enconf :mixed `(,i1) `(,c1) nil `(,p1 ,c1))
      ;; single item and garbage
      (test enconf :mixed `(,i1) nil `(,x1) `(,p1 ,x1))
      ;; single clock and garbage
      (test enconf :mixed nil `(,c1) `(,x1) `(,c1 ,x1))
      ;; single item, clock, and garbage
      (test enconf :mixed `(,i1) `(,c1) `(,x1) `(,p1 ,c1 ,x1))
      ;; multiple items and clocks
      (test enconf :mixed `(,i2 ,i1) `(,c2 ,c1) nil `(,p12 ,c1 ,c2))
      ;; multiple items and clocks (interlaced)
      (test enconf :mixed `(,i2 ,i1) `(,c2 ,c1) nil `(,p1 ,c1 ,p2 ,c2))
      ;; clock with note
      (test enconf :mixed nil `(,c1) `(,n1) `(,c1 ,pn1))
      ;; clock with note in wrong place
      (test enconf :mixed nil `(,c1) `(,n1) `(,pn1 ,c1))
      ;; clock with note and item
      (test enconf :mixed `(,i1) `(,c1) `(,n1) `(,c1 ,pn11))
      ;; clock with item and note
      (test enconf :mixed `(,i1) `(,c1) `(,n1) `(,c1 ,p1n1))
      ;; clock notes
      ;;
      ;; nothing
      (test enconf-notes :mixed nil nil nil nil)
      ;; single item
      (test enconf-notes :mixed `(,i1) nil nil `(,p1))
      ;; single clock
      (test enconf-notes :mixed nil `(,c1) nil `(,c1))
      ;; single garbage entry
      (test enconf-notes :mixed nil nil `(,x1) `(,x1))
      ;; single item and clock
      (test enconf-notes :mixed `(,i1) `(,c1) nil `(,p1 ,c1))
      ;; single item and garbage
      (test enconf-notes :mixed `(,i1) nil `(,x1) `(,p1 ,x1))
      ;; single clock and garbage
      (test enconf-notes :mixed nil `(,c1) `(,x1) `(,c1 ,x1))
      ;; single item, clock, and garbage
      (test enconf-notes :mixed `(,i1) `(,c1) `(,x1) `(,p1 ,c1 ,x1))
      ;; multiple items and clocks
      (test enconf-notes :mixed `(,i2 ,i1) `(,c2 ,c1) nil `(,p12 ,c1 ,c2))
      ;; multiple items and clocks (interlaced)
      (test enconf-notes :mixed `(,i2 ,i1) `(,c2 ,c1) nil `(,p1 ,c1 ,p2 ,c2))
      ;; clock with note
      (test enconf-notes :mixed nil `(,n1 ,c1) nil `(,c1 ,pn1))
      ;; clock with note in wrong place
      (test enconf-notes :mixed nil `(,c1) `(,n1) `(,pn1 ,c1))
      ;; clock with note and item
      (test enconf-notes :mixed `(,i1) `(,n1 ,c1) nil `(,c1 ,pn11))
      ;; clock with item and note
      (test enconf-notes :mixed `(,i1) `(,c1) `(,n1) `(,c1 ,p1n1))

      ;; MODE = ITEMS
      ;;
      ;; no clock notes
      ;;
      ;; nothing
      (test enconf :items nil nil nil nil)
      ;; single item
      (test enconf :items `(,i1) nil nil `(,p1))
      ;; single clock
      (test enconf :items nil nil `(,c1) `(,c1))
      ;; single garbage entry
      (test enconf :items nil nil `(,x1) `(,x1))
      ;; single item and clock
      (test enconf :items `(,i1) nil `(,c1) `(,p1 ,c1))
      ;; single item and garbage
      (test enconf :items `(,i1) nil `(,x1) `(,p1 ,x1))
      ;; single clock and garbage
      (test enconf :items nil nil `(,x1 ,c1) `(,c1 ,x1))
      ;; single item, clock, and garbage
      (test enconf :items `(,i1) nil `(,x1 ,c1) `(,p1 ,c1 ,x1))
      ;; multiple items and clocks
      (test enconf :items `(,i2 ,i1) nil `(,c2 ,c1) `(,p12 ,c1 ,c2))
      ;; multiple items and clocks (interlaced)
      (test enconf :items `(,i2 ,i1) nil `(,c2 ,c1) `(,p1 ,c1 ,p2 ,c2))
      ;; clock with note
      (test enconf :items nil nil `(,n1 ,c1) `(,c1 ,pn1))
      ;; clock with note in wrong place
      (test enconf :items nil nil `(,c1 ,n1) `(,pn1 ,c1))
      ;; clock with note and item
      (test enconf :items `(,i1) nil `(,n1 ,c1) `(,c1 ,pn11))
      ;; clock with item and note
      (test enconf :items `(,i1) nil `(,n1 ,c1) `(,c1 ,p1n1))
      ;; clock notes
      ;;
      ;; nothing
      (test enconf-notes :items nil nil nil nil)
      ;; single item
      (test enconf-notes :items `(,i1) nil nil `(,p1))
      ;; single clock
      (test enconf-notes :items nil nil `(,c1) `(,c1))
      ;; single garbage entry
      (test enconf-notes :items nil nil `(,x1) `(,x1))
      ;; single item and clock
      (test enconf-notes :items `(,i1) nil `(,c1) `(,p1 ,c1))
      ;; single item and garbage
      (test enconf-notes :items `(,i1) nil `(,x1) `(,p1 ,x1))
      ;; single clock and garbage
      (test enconf-notes :items nil nil `(,x1 ,c1) `(,c1 ,x1))
      ;; single item, clock, and garbage
      (test enconf-notes :items `(,i1) nil `(,x1 ,c1) `(,p1 ,c1 ,x1))
      ;; multiple items and clocks
      (test enconf-notes :items `(,i2 ,i1) nil `(,c2 ,c1) `(,p12 ,c1 ,c2))
      ;; multiple items and clocks (interlaced)
      (test enconf-notes :items `(,i2 ,i1) nil `(,c2 ,c1) `(,p1 ,c1 ,p2 ,c2))
      ;; clock with note
      (test enconf-notes :items nil nil `(,n1 ,c1) `(,c1 ,pn1))
      ;; clock with note in wrong place
      (test enconf-notes :items nil nil `(,c1 ,n1) `(,pn1 ,c1))
      ;; clock with note and item
      (test enconf-notes :items `(,i1) nil `(,n1 ,c1) `(,c1 ,pn11))
      ;; clock with item and note
      (test enconf-notes :items `(,i1) nil `(,n1 ,c1) `(,c1 ,p1n1))

      ;; MODE = CLOCKS
      ;;
      ;; no clock notes
      ;;
      ;; nothing
      (test enconf :clocks nil nil nil nil)
      ;; single item
      (test enconf :clocks nil nil `(,i1) `(,p1))
      ;; single clock
      (test enconf :clocks nil `(,c1) nil `(,c1))
      ;; single garbage entry
      (test enconf :clocks nil nil `(,x1) `(,x1))
      ;; single item and clock
      (test enconf :clocks nil `(,c1) `(,i1) `(,p1 ,c1))
      ;; single item and garbage
      (test enconf :clocks nil nil `(,x1 ,i1) `(,p1 ,x1))
      ;; single clock and garbage
      (test enconf :clocks nil `(,c1) `(,x1) `(,c1 ,x1))
      ;; single item, clock, and garbage
      (test enconf :clocks nil `(,c1) `(,x1 ,i1) `(,p1 ,c1 ,x1))
      ;; multiple items and clocks
      (test enconf :clocks nil `(,c2 ,c1) `(,i2 ,i1) `(,p12 ,c1 ,c2))
      ;; multiple items and clocks (interlaced)
      (test enconf :clocks nil `(,c2 ,c1) `(,i2 ,i1) `(,p1 ,c1 ,p2 ,c2))
      ;; clock with note
      (test enconf :clocks nil `(,c1) `(,n1) `(,c1 ,pn1))
      ;; clock with note in wrong place
      (test enconf :clocks nil `(,c1) `(,n1) `(,pn1 ,c1))
      ;; clock with note and item
      (test enconf :clocks nil `(,c1) `(,i1 ,n1) `(,c1 ,pn11))
      ;; clock with item and note
      (test enconf :clocks nil `(,c1) `(,n1 ,i1) `(,c1 ,p1n1))
      ;; clock notes
      ;;
      ;; nothing
      (test enconf-notes :clocks nil nil nil nil)
      ;; single item
      (test enconf-notes :clocks nil nil `(,i1) `(,p1))
      ;; single clock
      (test enconf-notes :clocks nil `(,c1) nil `(,c1))
      ;; single garbage entry
      (test enconf-notes :clocks nil nil `(,x1) `(,x1))
      ;; single item and clock
      (test enconf-notes :clocks nil `(,c1) `(,i1) `(,p1 ,c1))
      ;; single item and garbage
      (test enconf-notes :clocks nil nil `(,x1 ,i1) `(,p1 ,x1))
      ;; single clock and garbage
      (test enconf-notes :clocks nil `(,c1) `(,x1) `(,c1 ,x1))
      ;; single item, clock, and garbage
      (test enconf-notes :clocks nil `(,c1) `(,x1 ,i1) `(,p1 ,c1 ,x1))
      ;; multiple items and clocks
      (test enconf-notes :clocks nil `(,c2 ,c1) `(,i2 ,i1) `(,p12 ,c1 ,c2))
      ;; multiple items and clocks (interlaced)
      (test enconf-notes :clocks nil `(,c2 ,c1) `(,i2 ,i1) `(,p1 ,c1 ,p2 ,c2))
      ;; clock with note
      (test enconf-notes :clocks nil `(,n1 ,c1) nil `(,c1 ,pn1))
      ;; clock with note in wrong place
      (test enconf-notes :clocks nil `(,c1) `(,n1) `(,pn1 ,c1))
      ;; clock with note and item
      (test enconf-notes :clocks nil `(,n1 ,c1) `(,i1) `(,c1 ,pn11))
      ;; clock with item and note
      (test enconf-notes :clocks nil `(,c1) `(,n1 ,i1) `(,c1 ,p1n1)))))

(ert-deftest org-ml--logbook-to-nodes ()
  (cl-flet
      ((test
        (c out items clocks unknown)
        (->> (org-ml--logbook-init items clocks unknown)
             (org-ml--logbook-to-nodes c)
             (equal out)
             (should))))
    (let* ((i-name "LOGGING")
           (c-name "CLOCKING")
           (m-name "LOGBOOK")
           (c1 (org-ml-build-clock! '(2020 1 1 0 0) :end '(2020 1 1 1 0)))
           (i1 (org-ml-build-log-note (org-ml-time-to-unixtime '(2020 1 2 0 0)) "1"))
           (c2 (org-ml-build-clock! '(2020 1 3 0 0) :end '(2020 1 3 1 0)))
           (i2 (org-ml-build-log-note (org-ml-time-to-unixtime '(2020 1 4 0 0)) "2"))
           (p1 (org-ml-build-plain-list i1))
           (p2 (org-ml-build-plain-list i2))
           (p21 (org-ml-build-plain-list i2 i1))
           (di (org-ml-build-drawer i-name p21))
           (di* (org-ml-build-drawer m-name p21))
           (dc (org-ml-build-drawer c-name c2 c1))
           (dc* (org-ml-build-drawer m-name c2 c1))
           (dm (org-ml-build-drawer m-name p2 c2 p1 c1)))
      ;; ASSUME the sorting function takes care of clock notes, and since
      ;; everything passes through that, don't test it here
      (test nil `(,p2 ,c2 ,p1 ,c1) `(,i1 ,i2) `(,c1 ,c2) nil)
      (test `(:log-into-drawer ,i-name)
            `(,di ,c2 ,c1) `(,i1 ,i2) `(,c1 ,c2) nil)
      (test `(:clock-into-drawer ,c-name)
            `(,dc ,p21) `(,i1 ,i2) `(,c1 ,c2) nil)
      (test `(:log-into-drawer ,i-name :clock-into-drawer ,c-name)
            `(,di ,dc) `(,i1 ,i2) `(,c1 ,c2) nil)
      (test '(:log-into-drawer t :clock-into-drawer t)
            `(,dm) `(,i1 ,i2) `(,c1 ,c2) nil)
      (test '(:log-into-drawer nil :clock-into-drawer 1)
            `(,dc* ,p21) `(,i1 ,i2) `(,c1 ,c2) nil)
      (test '(:log-into-drawer nil :clock-into-drawer 2)
            `(,p2 ,c2 ,p1 ,c1) `(,i1 ,i2) `(,c1 ,c2) nil)
      (test '(:log-into-drawer t :clock-into-drawer 1)
            `(,dm) `(,i1 ,i2) `(,c1 ,c2) nil)
      (test '(:log-into-drawer t :clock-into-drawer 2)
            `(,di* ,c2 ,c1) `(,i1 ,i2) `(,c1 ,c2) nil)
      (test `(:log-into-drawer ,i-name :clock-into-drawer 1)
            `(,di ,dc*) `(,i1 ,i2) `(,c1 ,c2) nil)
      (test `(:log-into-drawer ,i-name :clock-into-drawer 2)
            `(,di ,c2 ,c1) `(,i1 ,i2) `(,c1 ,c2) nil))))

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

;; TODO be more thorough and do this for all the others (add blanks to the tests)
(ert-deftest org-ml--supercontents-mixed ()
  (let* ((config nil)
         (config-notes '(:clock-out-notes t))
         (i1 (org-ml-build-log-note 1603767576 "i1"))
         (i2 (org-ml-build-item! :paragraph "clock note"))
         (p1 (org-ml-build-plain-list i1))
         (p2 (org-ml-build-plain-list i2))
         (c1 (org-ml-build-clock (org-ml-build-timestamp! '(2112 1 1 0 0) :end '(2112 1 2 0 0))))
         (r1 (org-ml-build-paragraph! "foo")))
    ;; nothing
    (should (equal (org-ml--supercontents-from-nodes config-notes nil)
                   (org-ml--supercontents-init nil nil nil nil)))
    ;; no logbook
    (should (equal (org-ml--supercontents-from-nodes config-notes (list r1))
                   (org-ml--supercontents-init nil nil nil `(,r1))))
    ;; all combinations of item, clock, and clock note, and rest
    (should (equal (org-ml--supercontents-from-nodes config-notes (list p1 c1 p2 r1))
                   (org-ml--supercontents-init `(,i1) `(,c1 ,i2) nil `(,r1))))
    (should (equal (org-ml--supercontents-from-nodes config-notes (list p1 p2 c1 r1))
                   (org-ml--supercontents-init `(,i1) nil nil `(,p2 ,c1 ,r1))))
    (should (equal (org-ml--supercontents-from-nodes config-notes (list c1 p2 p1 r1))
                   (org-ml--supercontents-init `(,i1) `(,c1 ,i2) nil `(,r1))))
    (should (equal (org-ml--supercontents-from-nodes config-notes (list c1 p1 p2 r1))
                   (org-ml--supercontents-init `(,i1) `(,c1) nil `(,p2 ,r1))))
    (should (equal (org-ml--supercontents-from-nodes config-notes (list p2 p1 c1 r1))
                   (org-ml--supercontents-init nil nil nil `(,p2 ,p1 ,c1 ,r1))))
    (should (equal (org-ml--supercontents-from-nodes config-notes (list p2 c1 p1 r1))
                   (org-ml--supercontents-init nil nil nil `(,p2 ,c1 ,p1 ,r1))))
    ;; same things without rest
    (should (equal (org-ml--supercontents-from-nodes config-notes (list p1 c1 p2))
                   (org-ml--supercontents-init `(,i1) `(,c1 ,i2) nil nil)))
    (should (equal (org-ml--supercontents-from-nodes config-notes (list p1 p2 c1))
                   (org-ml--supercontents-init `(,i1) nil nil `(,p2 ,c1))))
    (should (equal (org-ml--supercontents-from-nodes config-notes (list c1 p2 p1))
                   (org-ml--supercontents-init `(,i1) `(,c1 ,i2) nil nil)))
    (should (equal (org-ml--supercontents-from-nodes config-notes (list c1 p1 p2))
                   (org-ml--supercontents-init `(,i1) `(,c1) nil `(,p2))))
    (should (equal (org-ml--supercontents-from-nodes config-notes (list p2 p1 c1))
                   (org-ml--supercontents-init nil nil nil `(,p2 ,p1 ,c1))))
    (should (equal (org-ml--supercontents-from-nodes config-notes (list p2 c1 p1))
                   (org-ml--supercontents-init nil nil nil `(,p2 ,c1 ,p1))))
    ;; same things choose two
    (should (equal (org-ml--supercontents-from-nodes config-notes (list p1 c1))
                   (org-ml--supercontents-init `(,i1) `(,c1) nil nil)))
    (should (equal (org-ml--supercontents-from-nodes config-notes (list p1 p2))
                   (org-ml--supercontents-init `(,i1) nil nil `(,p2))))
    (should (equal (org-ml--supercontents-from-nodes config-notes (list c1 p2))
                   (org-ml--supercontents-init nil `(,c1 ,i2) nil nil)))
    (should (equal (org-ml--supercontents-from-nodes config-notes (list c1 p1))
                   (org-ml--supercontents-init `(,i1) `(,c1) nil nil)))
    (should (equal (org-ml--supercontents-from-nodes config-notes (list p2 p1))
                   (org-ml--supercontents-init nil nil nil `(,p2 ,p1))))
    (should (equal (org-ml--supercontents-from-nodes config-notes (list p2 c1))
                   (org-ml--supercontents-init nil nil nil `(,p2 ,c1))))
    ;; ignore if not in right order (assume what comes after doesn't matter)
    (should (equal (org-ml--supercontents-from-nodes config-notes (list r1 p1))
                   (org-ml--supercontents-init nil nil nil `(,r1 ,p1))))
    ;; same tests but ignore clock notes
    ;;
    ;; nothing
    (should (equal (org-ml--supercontents-from-nodes config nil)
                   (org-ml--supercontents-init nil nil nil nil)))
    ;; no logbook
    (should (equal (org-ml--supercontents-from-nodes config (list r1))
                   (org-ml--supercontents-init nil nil nil `(,r1))))
    ;; all combinations of two items and one clock
    (should (equal (org-ml--supercontents-from-nodes config (list p1 c1 p2 r1))
                   (org-ml--supercontents-init `(,i1) `(,c1) nil `(,p2 ,r1))))
    (should (equal (org-ml--supercontents-from-nodes config (list p1 p2 c1 r1))
                   (org-ml--supercontents-init `(,i1) nil nil `(,p2 ,c1 ,r1))))
    (should (equal (org-ml--supercontents-from-nodes config (list c1 p2 p1 r1))
                   (org-ml--supercontents-init nil `(,c1) nil `(,p2 ,p1 ,r1))))
    (should (equal (org-ml--supercontents-from-nodes config (list c1 p1 p2 r1))
                   (org-ml--supercontents-init `(,i1) `(,c1) nil `(,p2 ,r1))))
    (should (equal (org-ml--supercontents-from-nodes config (list p2 p1 c1 r1))
                   (org-ml--supercontents-init nil nil nil `(,p2 ,p1 ,c1 ,r1))))
    (should (equal (org-ml--supercontents-from-nodes config (list p2 c1 p1 r1))
                   (org-ml--supercontents-init nil nil nil `(,p2 ,c1 ,p1 ,r1))))
    ;; same things without rest
    (should (equal (org-ml--supercontents-from-nodes config (list p1 c1 p2))
                   (org-ml--supercontents-init `(,i1) `(,c1) nil `(,p2))))
    (should (equal (org-ml--supercontents-from-nodes config (list p1 p2 c1))
                   (org-ml--supercontents-init `(,i1) nil nil `(,p2 ,c1))))
    (should (equal (org-ml--supercontents-from-nodes config (list c1 p2 p1))
                   (org-ml--supercontents-init nil `(,c1) nil `(,p2 ,p1))))
    (should (equal (org-ml--supercontents-from-nodes config (list c1 p1 p2))
                   (org-ml--supercontents-init `(,i1) `(,c1) nil `(,p2))))
    (should (equal (org-ml--supercontents-from-nodes config (list p2 p1 c1))
                   (org-ml--supercontents-init nil nil nil `(,p2 ,p1 ,c1))))
    (should (equal (org-ml--supercontents-from-nodes config (list p2 c1 p1))
                   (org-ml--supercontents-init nil nil nil `(,p2 ,c1 ,p1))))
    ;; same things choose two
    (should (equal (org-ml--supercontents-from-nodes config (list p1 c1))
                   (org-ml--supercontents-init `(,i1) `(,c1) nil nil)))
    (should (equal (org-ml--supercontents-from-nodes config (list p1 p2))
                   (org-ml--supercontents-init `(,i1) nil nil `(,p2))))
    (should (equal (org-ml--supercontents-from-nodes config (list c1 p2))
                   (org-ml--supercontents-init nil `(,c1) nil `(,p2))))
    (should (equal (org-ml--supercontents-from-nodes config (list c1 p1))
                   (org-ml--supercontents-init `(,i1) `(,c1) nil nil)))
    (should (equal (org-ml--supercontents-from-nodes config (list p2 p1))
                   (org-ml--supercontents-init nil nil nil `(,p2 ,p1))))
    (should (equal (org-ml--supercontents-from-nodes config (list p2 c1))
                   (org-ml--supercontents-init nil nil nil `(,p2 ,c1))))))

(ert-deftest org-ml--supercontents-single-items ()
  (let* ((id-name "LOGGING")
         (config `(:log-into-drawer ,id-name))
         (config-notes `(:log-into-drawer ,id-name :clock-out-notes t))
         (i1 (org-ml-build-item! :paragraph "i1"))
         (i2 (org-ml-build-log-note 1603767576 "log note"))
         (i3 (org-ml-build-log-note 1603767576 "log note in drawer"))
         (p1 (org-ml-build-plain-list i1))
         (p2 (org-ml-build-plain-list i2))
         (p3 (org-ml-build-plain-list i3))
         (p4 (org-ml-build-plain-list i1 i2))
         (drwr (org-ml-build-drawer id-name p3))
         (ts1 (org-ml-build-timestamp! '(2112 1 1 0 0)
                                       :end '(2112 1 2 0 0)))
         (c1 (org-ml-build-clock ts1))
         (r1 (org-ml-build-paragraph! "foo")))
    ;; nothing
    (should (equal
             (org-ml--supercontents-from-nodes config nil)
             (org-ml--supercontents-init nil nil nil nil)))
    ;; no logbook
    (should (equal
             (org-ml--supercontents-from-nodes config (list r1))
             (org-ml--supercontents-init nil nil nil `(,r1))))
    ;; clock followed by note
    (should (equal
             (org-ml--supercontents-from-nodes config-notes (list c1 p1 r1))
             (org-ml--supercontents-init nil `(,c1 ,i1) nil `(,r1))))
    ;; clock followed by log item
    (should (equal
             (org-ml--supercontents-from-nodes config-notes (list c1 p2 r1))
             (org-ml--supercontents-init nil `(,c1) nil `(,p2 ,r1))))
    ;; clock followed by note (no notes wanted)
    (should (equal
             (org-ml--supercontents-from-nodes config (list c1 p1 r1))
             (org-ml--supercontents-init nil `(,c1) nil `(,p1 ,r1))))
    ;; clock followed by note and item
    (should (equal
             (org-ml--supercontents-from-nodes config-notes (list c1 p4 r1))
             (org-ml--supercontents-init nil `(,c1 ,i1) nil `(,p2 ,r1))))
    ;; clock followed by note and item (no notes wanted)
    (should (equal
             (org-ml--supercontents-from-nodes config (list c1 p4 r1))
             (org-ml--supercontents-init nil `(,c1) nil `(,p4 ,r1))))
    ;; item followed by clock (store none)
    ;; ASSUME the code that stops splitting
    ;; after finding an invalid item is fully tested with this example and will
    ;; therefore do the same in the permutations below
    (should (equal
             (org-ml--supercontents-from-nodes config (list p1 c1 r1))
             (org-ml--supercontents-init nil nil nil `(,p1 ,c1 ,r1))))
    ;; drawer followed by clock (store both)
    (should (equal
             (org-ml--supercontents-from-nodes config (list drwr c1 r1))
             (org-ml--supercontents-init `(,i3) `(,c1) nil `(,r1))))
    ;; clock followed by drawer (store both)
    (should (equal
             (org-ml--supercontents-from-nodes config (list c1 drwr r1))
             (org-ml--supercontents-init `(,i3) `(,c1) nil `(,r1))))
    ;; clock followed by item and drawer (store all)
    (should (equal
             (org-ml--supercontents-from-nodes config-notes (list c1 p1 drwr r1))
             (org-ml--supercontents-init `(,i3) `(,c1 ,i1) nil `(,r1))))
    ;; clock followed by item and drawer (don't store note)
    (should (equal
             (org-ml--supercontents-from-nodes config (list c1 p1 drwr r1))
             (org-ml--supercontents-init nil `(,c1) nil `(,p1 ,drwr ,r1))))))

(ert-deftest org-ml--supercontents-single-clocks ()
  (let* ((cd-name "CLOCKING")
         (config `(:clock-into-drawer ,cd-name))
         (config-notes `(:clock-into-drawer ,cd-name :clock-out-notes t))
         (i1 (org-ml-build-log-note 1603767576 "note 1"))
         (i2 (org-ml-build-log-note 1603767576 "note 2"))
         (i3 (org-ml-build-item! :paragraph "clock note"))
         (p1 (org-ml-build-plain-list i1))
         (p2 (org-ml-build-plain-list i2))
         (p3 (org-ml-build-plain-list i3))
         (ts1 (org-ml-build-timestamp! '(2112 1 1 0 0) :end '(2112 1 2 0 0)))
         (c1 (org-ml-build-clock ts1))
         (drwr1 (org-ml-build-drawer cd-name c1))
         (drwr2 (org-ml-build-drawer cd-name c1 p3))
         (r1 (org-ml-build-paragraph! "foo")))
    ;; nothing
    (should (equal
             (org-ml--supercontents-from-nodes config nil)
             (org-ml--supercontents-init nil nil nil nil)))
    ;; no logbook
    (should (equal
             (org-ml--supercontents-from-nodes config (list r1))
             (org-ml--supercontents-init nil nil nil `(,r1))))
    ;; this only has six valid combinations
    ;;
    ;; plain-list, drawer, plain-list
    (should (equal
             (org-ml--supercontents-from-nodes config (list p1 drwr1 p2 r1))
             (org-ml--supercontents-init `(,i1 ,i2) `(,c1) nil `(,r1))))
    ;; plain-list, drawer
    (should (equal
             (org-ml--supercontents-from-nodes config (list p1 drwr1 r1))
             (org-ml--supercontents-init `(,i1) `(,c1) nil `(,r1))))
    ;; plain-list
    (should (equal
             (org-ml--supercontents-from-nodes config (list p1 r1))
             (org-ml--supercontents-init `(,i1) nil nil `(,r1))))
    ;; drawer, plain-list
    (should (equal
             (org-ml--supercontents-from-nodes config (list drwr1 p1 r1))
             (org-ml--supercontents-init `(,i1) `(,c1) nil `(,r1))))
    ;; drawer
    (should (equal
             (org-ml--supercontents-from-nodes config (list drwr1 r1))
             (org-ml--supercontents-init nil `(,c1) nil `(,r1))))
    ;; drawer with clock notes
    (should (equal
             (org-ml--supercontents-from-nodes config-notes (list drwr2 r1))
             (org-ml--supercontents-init nil `(,c1 ,i3) nil `(,r1))))
    ;; invalid combinations
    ;;
    ;; loose clock anywhere
    (should (equal
             (org-ml--supercontents-from-nodes config (list c1 p1 r1))
             (org-ml--supercontents-init nil nil nil `(,c1 ,p1 ,r1))))))

(ert-deftest org-ml--supercontents-dual ()
  (let* ((cd-name "CLOCKING")
         (id-name "LOGGING")
         (config `(:log-into-drawer ,id-name :clock-into-drawer ,cd-name))
         (i1 (org-ml-build-log-note 1603767576 "note"))
         (p1 (org-ml-build-plain-list i1))
         (ts1 (org-ml-build-timestamp! '(2112 1 1 0 0) :end '(2112 1 2 0 0)))
         (c1 (org-ml-build-clock ts1))
         (drwr1 (org-ml-build-drawer cd-name c1))
         (drwr2 (org-ml-build-drawer id-name p1))
         (r1 (org-ml-build-paragraph! "foo")))
    ;; nothing
    (should (equal
             (org-ml--supercontents-from-nodes config nil)
             (org-ml--supercontents-init nil nil nil nil)))
    ;; no logbook
    (should (equal
             (org-ml--supercontents-from-nodes config (list r1))
             (org-ml--supercontents-init nil nil nil `(,r1))))
    ;; one drawer
    (should (equal
             (org-ml--supercontents-from-nodes config (list drwr1 r1))
             (org-ml--supercontents-init nil `(,c1) nil `(,r1))))
    ;; one drawer (other one)
    (should (equal
             (org-ml--supercontents-from-nodes config (list drwr2 r1))
             (org-ml--supercontents-init `(,i1) nil nil `(,r1))))
    ;; two drawers
    (should (equal
             (org-ml--supercontents-from-nodes config (list drwr1 drwr2 r1))
             (org-ml--supercontents-init `(,i1) `(,c1) nil `(,r1))))
    ;; two drawers (other order)
    (should (equal
             (org-ml--supercontents-from-nodes config (list drwr2 drwr1 r1))
             (org-ml--supercontents-init `(,i1) `(,c1) nil `(,r1))))
    ;; clock outside (invalid)
    (should (equal
             (org-ml--supercontents-from-nodes config (list c1 drwr2 r1))
             (org-ml--supercontents-init nil nil nil `(,c1 ,drwr2 ,r1))))
    ;; item outside (invalid)
    (should (equal
             (org-ml--supercontents-from-nodes config (list p1 drwr1 r1))
             (org-ml--supercontents-init nil nil nil `(,p1 ,drwr1 ,r1))))))

(ert-deftest org-ml--supercontents-single-mixed ()
  (let* ((d-name "LOGBOOK")
         (config '(:log-into-drawer t :clock-into-drawer t))
         (config-notes (list :log-into-drawer t
                             :clock-into-drawer t
                             :clock-out-notes t))
         (i1 (org-ml-build-log-note 1603767576 "note 2"))
         (i2 (org-ml-build-item! :paragraph "clock note"))
         (p1 (org-ml-build-plain-list i1))
         (p2 (org-ml-build-plain-list i2))
         (ts1 (org-ml-build-timestamp! '(2112 1 1 0 0) :end '(2112 1 2 0 0)))
         (c1 (org-ml-build-clock ts1))
         (drwr1 (org-ml-build-drawer d-name c1 p1))
         (drwr2 (org-ml-build-drawer d-name c1 p2 p1))
         (r1 (org-ml-build-paragraph! "foo")))
    ;; nothing
    (should (equal
             (org-ml--supercontents-from-nodes config nil)
             (org-ml--supercontents-init nil nil nil nil)))
    ;; no logging
    (should (equal
             (org-ml--supercontents-from-nodes config (list r1))
             (org-ml--supercontents-init nil nil nil `(,r1))))
    ;; single drawer
    (should (equal
             (org-ml--supercontents-from-nodes config (list drwr1 r1))
             (org-ml--supercontents-init `(,i1) `(,c1) nil `(,r1))))
    ;; single drawer with notes
    (should (equal
             (org-ml--supercontents-from-nodes config-notes (list drwr2 r1))
             (org-ml--supercontents-init `(,i1) `(,c1 ,i2) nil `(,r1))))
    ;; clock outside (invalid)
    (should (equal
             (org-ml--supercontents-from-nodes config (list c1 drwr1 r1))
             (org-ml--supercontents-init nil nil nil `(,c1 ,drwr1 ,r1))))
    ;; item outside (invalid)
    (should (equal
             (org-ml--supercontents-from-nodes config (list p1 drwr1 r1))
             (org-ml--supercontents-init nil nil nil `(,p1 ,drwr1 ,r1))))))

(ert-deftest org-ml--supercontents-single-clocks-or-mixed ()
  ;; ASSUME clock notes are tested using the mixed and single-clocks tests
  (let* ((clock-limit 1)
         (config `(:clock-into-drawer ,clock-limit))
         (i1 (org-ml-build-log-note 1603767576 "note 1"))
         (i2 (org-ml-build-log-note 1603767576 "note 2"))
         (p1 (org-ml-build-plain-list i1))
         (p2 (org-ml-build-plain-list i2))
         (ts1 (org-ml-build-timestamp! '(2112 1 1 0 0) :end '(2112 1 2 0 0)))
         (c1 (org-ml-build-clock ts1))
         (ts2 (org-ml-build-timestamp! '(2112 1 2 0 0) :end '(2112 1 3 0 0)))
         (c2 (org-ml-build-clock ts1))
         (drwr (org-ml-build-drawer "LOGBOOK" c1))
         (r1 (org-ml-build-paragraph! "foo")))
    ;; nothing
    (should (equal
             (org-ml--supercontents-from-nodes config nil)
             (org-ml--supercontents-init nil nil nil nil)))
    ;; no logbook
    (should (equal
             (org-ml--supercontents-from-nodes config (list r1))
             (org-ml--supercontents-init nil nil nil `(,r1))))
    ;; same tests as single-clocks when over clock limit
    ;;
    ;; plain-list, drawer, plain-list
    (should (equal
             (org-ml--supercontents-from-nodes config (list p1 drwr p2 r1))
             (org-ml--supercontents-init `(,i1 ,i2) `(,c1) nil `(,r1))))
    ;; plain-list, drawer
    (should (equal
             (org-ml--supercontents-from-nodes config (list p1 drwr r1))
             (org-ml--supercontents-init `(,i1) `(,c1) nil `(,r1))))
    ;; plain-list
    (should (equal
             (org-ml--supercontents-from-nodes config (list p1 r1))
             (org-ml--supercontents-init `(,i1) nil nil `(,r1))))
    ;; drawer, plain-list
    (should (equal
             (org-ml--supercontents-from-nodes config (list drwr p1 r1))
             (org-ml--supercontents-init `(,i1) `(,c1) nil `(,r1))))
    ;; drawer
    (should (equal
             (org-ml--supercontents-from-nodes config (list drwr r1))
             (org-ml--supercontents-init nil `(,c1) nil `(,r1))))
    ;; same as mixed
    ;;
    ;; loose clock under clock limit
    (should (equal
             (org-ml--supercontents-from-nodes config (list c1 p1 r1))
             (org-ml--supercontents-init `(,i1) `(,c1) nil `(,r1))))
    ;; too many clocks
    (should (equal
             (org-ml--supercontents-from-nodes config (list c1 c2 p1 r1))
             (org-ml--supercontents-init nil `(,c1) nil `(,c2 ,p1 ,r1))))))

(ert-deftest org-ml--supercontents-single-items-or-dual ()
  (let* ((id-name "LOGGING")
         (clock-limit 1)
         (config `(:log-into-drawer ,id-name :clock-into-drawer ,clock-limit))
         (i1 (org-ml-build-log-note 1603767576 "note 1"))
         (i2 (org-ml-build-log-note 1603767576 "note 2"))
         (i3 (org-ml-build-log-note 1603767576 "note 3"))
         (p1 (org-ml-build-plain-list i1))
         (p2 (org-ml-build-plain-list i1 i2))
         (p3 (org-ml-build-plain-list i3))
         (drwr1 (org-ml-build-drawer id-name p3))
         (ts1 (org-ml-build-timestamp! '(2112 1 1 0 0) :end '(2112 1 2 0 0)))
         (c1 (org-ml-build-clock ts1))
         (ts2 (org-ml-build-timestamp! '(2112 1 2 0 0) :end '(2112 1 3 0 0)))
         (c2 (org-ml-build-clock ts2))
         (drwr2 (org-ml-build-drawer "LOGBOOK" c1))
         (r1 (org-ml-build-paragraph! "foo")))
    ;; nothing
    (should (equal
             (org-ml--supercontents-from-nodes config nil)
             (org-ml--supercontents-init nil nil nil nil)))
    ;; no logbook
    (should (equal
             (org-ml--supercontents-from-nodes config (list r1))
             (org-ml--supercontents-init nil nil nil `(,r1))))
    ;; same as single-items
    ;;
    ;; clock followed by one item (don't store note)
    (should (equal
             (org-ml--supercontents-from-nodes config (list c1 p1 r1))
             (org-ml--supercontents-init nil `(,c1) nil `(,p1 ,r1))))
    ;; clock followed by two items (store only clock)
    (should (equal
             (org-ml--supercontents-from-nodes config (list c1 p2 r1))
             (org-ml--supercontents-init nil `(,c1) nil `(,p2 ,r1))))
    ;; item followed by clock (store none)
    ;; ASSUME the code that stops splitting
    ;; after finding an invalid item is fully tested with this example and will
    ;; therefore do the same in the permutations below
    (should (equal
             (org-ml--supercontents-from-nodes config (list p1 c1 r1))
             (org-ml--supercontents-init nil nil nil `(,p1 ,c1 ,r1))))
    ;; drawer followed by clock (store both)
    (should (equal
             (org-ml--supercontents-from-nodes config (list drwr1 c1 r1))
             (org-ml--supercontents-init `(,i3) `(,c1) nil `(,r1))))
    ;; clock followed by drawer (store both)
    (should (equal
             (org-ml--supercontents-from-nodes config (list c1 drwr1 r1))
             (org-ml--supercontents-init `(,i3) `(,c1) nil `(,r1))))
    ;; drawer followed by clock and item (store only clock)
    (should (equal
             (org-ml--supercontents-from-nodes config (list drwr1 c1 p1 r1))
             (org-ml--supercontents-init `(,i3) `(,c1) nil `(,p1 ,r1))))
    ;; clock followed by item and drawer (don't store note)
    (should (equal
             (org-ml--supercontents-from-nodes config (list c1 p1 drwr1 r1))
             (org-ml--supercontents-init nil `(,c1) nil `(,p1 ,drwr1 ,r1))))
    ;; too many clocks
    (should (equal
             (org-ml--supercontents-from-nodes config (list c1 c2 p1 drwr1 r1))
             (org-ml--supercontents-init nil `(,c1) nil `(,c2 ,p1 ,drwr1 ,r1))))
    ;; dual drawer (clock only)
    (should (equal
             (org-ml--supercontents-from-nodes config (list drwr2 r1))
             (org-ml--supercontents-init nil `(,c1) nil `(,r1))))
    ;; dual drawer (item only)
    (should (equal
             (org-ml--supercontents-from-nodes config (list drwr1 r1))
             (org-ml--supercontents-init `(,i3) nil nil `(,r1))))
    ;; dual drawer (both)
    (should (equal
             (org-ml--supercontents-from-nodes config (list drwr1 drwr2 r1))
             (org-ml--supercontents-init `(,i3) `(,c1) nil `(,r1))))))

(ert-deftest org-ml--supercontents-single-mixed-or-single-items ()
  (let* ((clock-limit 1)
         (config `(:log-into-drawer t :clock-into-drawer ,clock-limit))
         (i1 (org-ml-build-log-note 1603767576 "note 1"))
         (p1 (org-ml-build-plain-list i1))
         (ts1 (org-ml-build-timestamp! '(2112 1 1 0 0) :end '(2112 1 2 0 0)))
         (c1 (org-ml-build-clock ts1))
         (ts2 (org-ml-build-timestamp! '(2112 1 2 0 0) :end '(2112 1 3 0 0)))
         (c2 (org-ml-build-clock ts2))
         (drwr1 (org-ml-build-drawer "LOGBOOK" c1 p1))
         (drwr2 (org-ml-build-drawer "LOGBOOK" p1))
         (r1 (org-ml-build-paragraph! "foo")))
    ;; nothing
    (should (equal
             (org-ml--supercontents-from-nodes config nil)
             (org-ml--supercontents-init nil nil nil nil)))
    ;; no logging
    (should (equal
             (org-ml--supercontents-from-nodes config (list r1))
             (org-ml--supercontents-init nil nil nil `(,r1))))
    ;; single drawer
    (should (equal
             (org-ml--supercontents-from-nodes config (list drwr1 r1))
             (org-ml--supercontents-init `(,i1) `(,c1) nil `(,r1))))
    ;; clock outside and inside
    ;; TODO there should not be two clocks in this one
    (should (equal
             (org-ml--supercontents-from-nodes config (list c1 drwr1 r1))
             (org-ml--supercontents-init `(,i1) `(,c1 ,c1) nil `(,r1))))
    ;; clocks outside and not inside
    (should (equal
             (org-ml--supercontents-from-nodes config (list c1 drwr2 r1))
             (org-ml--supercontents-init `(,i1) `(,c1) nil `(,r1))))
    ;; too many clocks outside
    (should (equal
             (org-ml--supercontents-from-nodes config (list c1 c2 drwr2 r1))
             (org-ml--supercontents-init nil `(,c1) nil `(,c2 ,drwr2 ,r1))))
    ;; item outside (invalid)
    (should (equal
             (org-ml--supercontents-from-nodes config (list p1 drwr1 r1))
             (org-ml--supercontents-init nil nil nil `(,p1 ,drwr1 ,r1))))))

;; logbook blank line testing
;;
;; assume these tests cover all code paths
;; - any logbook type followed by a blank (item, clock, drawer)
;; - in the case of clocks with notes, clocks followed by plain lists where the
;;   first item has a blank after it

(ert-deftest org-ml--supercontents-mixed-blank-line ()
  (let* ((config nil)
         (config-notes '(:clock-out-notes t))
         (i1 (->> (org-ml-build-log-note 1603767576 "note 1")
                  (org-ml-set-property :post-blank 1)))
         (i2 (org-ml-build-log-note 1603767576 "note 2"))
         (i3 (org-ml-build-item! :post-blank 1 :paragraph "clock note"))
         (p1 (org-ml-build-plain-list i1 i2))
         (p2 (org-ml-build-plain-list i1))
         (p3 (org-ml-build-plain-list i2))
         (p4 (org-ml-build-plain-list :post-blank 1 i1 i2))
         (p5 (org-ml-build-plain-list :post-blank 1 i2))
         (p6 (org-ml-build-plain-list i3 i2))
         (p66 (org-ml-build-plain-list i2))
         ;; (p6 (org-ml-build-plain-list i1))
         (p7 (org-ml-build-plain-list :post-blank 1 i2))
         (ts1 (org-ml-build-timestamp! '(2112 1 1 0 0) :end '(2112 1 2 0 0)))
         (c1 (org-ml-build-clock ts1))
         (c2 (org-ml-build-clock ts1 :post-blank 1))
         (r1 (org-ml-build-paragraph! "foo")))
    ;; if plain-list has two items with a space between, add only first item
    (should (equal (org-ml--supercontents-from-nodes config-notes (list p1 c1 r1))
                   (org-ml--supercontents-init `(,i1) nil nil `(,p3 ,c1 ,r1))))
    ;; ditto but after a clock
    (should (equal (org-ml--supercontents-from-nodes config-notes (list c1 p1 r1))
                   (org-ml--supercontents-init `(,i1) `(,c1) nil `(,p3 ,r1))))
    ;; ditto but with a clock note after the clock instead
    (should (equal (org-ml--supercontents-from-nodes config-notes (list c1 p6 r1))
                   (org-ml--supercontents-init nil `(,c1 ,i3) nil `(,p66 ,r1))))
    ;; if clock has blank, stop after clock
    (should (equal (org-ml--supercontents-from-nodes config-notes (list c2 p1 r1))
                   (org-ml--supercontents-init nil `(,c2) nil `(,p1 ,r1))))
    ;; if plain-list has two items with blanks after each of them, add only first
    (should (equal (org-ml--supercontents-from-nodes config-notes (list p4 r1))
                   (org-ml--supercontents-init `(,i1) nil nil `(,p7 ,r1))))
    ;; ;; same tests without clock notes
    ;; ;;
    ;; if plain-list has two items with a space between, add only first item
    (should (equal (org-ml--supercontents-from-nodes config (list p1 c1 r1))
                   (org-ml--supercontents-init `(,i1) nil nil `(,p3 ,c1 ,r1))))
    ;; ditto but after a clock
    (should (equal (org-ml--supercontents-from-nodes config (list c1 p1 r1))
                   (org-ml--supercontents-init `(,i1) `(,c1) nil `(,p3 ,r1))))
    ;; ditto but with a clock note after the clock instead
    (should (equal (org-ml--supercontents-from-nodes config (list c1 p6 r1))
                   (org-ml--supercontents-init nil `(,c1) nil `(,p6 ,r1))))
    ;; if clock has blank, stop after clock
    (should (equal (org-ml--supercontents-from-nodes config (list c2 p1 r1))
                   (org-ml--supercontents-init nil `(,c2) nil `(,p1 ,r1))))
    ;; if plain-list has two items with blanks after each of them, add only first
    (should (equal (org-ml--supercontents-from-nodes config (list p4 r1))
                   (org-ml--supercontents-init `(,i1) nil nil `(,p7 ,r1))))))

;; I'm not sure what this test was supposed to do...

;; (ert-deftest org-ml--supercontents-single-items-blank-line ()
;;   (let* ((config '(t nil nil))
;;          (i1 (->> (org-ml-build-log-note 1603767576 "note 1")
;;                   (org-ml-set-property :post-blank 1)))
;;          (i2 (org-ml-build-log-note 1603767576 "note 2"))
;;          (p1 (org-ml-build-plain-list i1 i2))
;;          (p2 (org-ml-build-plain-list i1))
;;          (p3 (org-ml-build-plain-list i2))
;;          (p4 (org-ml-build-plain-list :post-blank 1 i1 i2))
;;          (p5 (org-ml-build-plain-list :post-blank 1 i2))
;;          (ts1 (org-ml-build-timestamp! '(2112 1 1 0 0) :end '(2112 1 2 0 0)))
;;          (drwr (org-ml-build-drawer "LOGBOOK" p3))
;;          (c1 (org-ml-build-clock ts1))
;;          (c2 (org-ml-build-clock ts1 :post-blank 1))
;;          (r1 (org-ml-build-paragraph! "foo")))
;;     ;; clocks with two items with space after first item (don't want notes)
;;     (should (equal
;;              (org-ml--supercontents-from-nodes config (list c1 p1 r1))
;;              (org-ml--supercontents-init nil `(,c1) `(,p1 ,r1))))
;;     ;; clocks with two items with space after first and second item (add clock and first item)
;;     (should (equal
;;              (org-ml--supercontents-from-nodes config (list c1 p4 r1))
;;              (org-ml--supercontents-init nil `(,c1 ,p2) `(,p5 ,r1))))
;;     ;; clocks with two items with space after first and second item (don't add notes)
;;     (should (equal
;;              (org-ml--supercontents-from-nodes nil (list c1 p4 r1))
;;              (org-ml--supercontents-init nil `(,c1) `(,p4 ,r1))))
;;     ;; clocks with space after (only add clock)
;;     (should (equal
;;              (org-ml--supercontents-from-nodes nil (list c2 p1 r1))
;;              (org-ml--supercontents-init nil `(,c2) `(,p1 ,r1))))
;;     ;; drawer with space after (only add drawer)
;;     (should (equal
;;              (org-ml--supercontents-from-nodes nil (list drwr p1 r1))
;;              (org-ml--supercontents-init `(,p3) nil `(,p1 ,r1))))))

;;; MATCH FRAMEWORK TESTING

;; These are tests for `org-ml-match' and friends. Proceed with caution :)

(defmacro should-error-arg (form)
  "Make an ert error form to test if FORM signals an `arg-type-error'."
  `(should-error ,form :type 'arg-type-error))

(ert-deftest org-ml--match-make-condition-form/error ()
  ;; Ensure `org-ml--match-make-condition-form' will error when it
  ;; supposed to do so. All errors (in theory) should be tested here
  ;; so that we don't need to bother testing them anywhere else when
  ;; we test functions higher in the framework
  (unless (fboundp 'org-ml--match-make-condition-form)
    (error "Function not defined"))
  (let ((fun #'org-ml--match-make-condition-form))
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
    (should-error-arg (funcall fun '(:and bold :2)))
    ;; or with no arguments
    (should-error-arg (funcall fun '(:or)))
    ;; or with nonsense
    (should-error-arg (funcall fun '(:or bold :2)))
    ;; properties with symbols instead of keywords
    (should-error-arg (funcall fun '(tags '("hi"))))
    ;; multiple properties
    (should-error-arg (funcall fun '(:tags '("hi") :todo-keyword "DONE")))
    ;; just wrong...
    (should-error-arg (funcall fun nil))
    (should-error-arg (funcall fun :1))))

(ert-deftest org-ml--match-pattern-make-inner-form/error ()
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
  (let ((fun (-partial #'org-ml--match-pattern-make-inner-form nil nil)))
    ;; slicers present
    (should-error-arg (funcall fun '(:first bold)))
    (should-error-arg (funcall fun '(:last bold)))
    (should-error-arg (funcall fun '(:nth bold)))
    (should-error-arg (funcall fun '(:sub bold)))
    (should-error-arg (funcall fun '(bold :first)))
    (should-error-arg (funcall fun '(bold :last)))
    (should-error-arg (funcall fun '(bold :nth)))
    (should-error-arg (funcall fun '(bold :sub)))
    ;; just wrong...
    (should-error-arg (funcall fun '(:swaggart)))))

(defun should-expand-to-alts (pattern alt-patterns)
  (should (equal (org-ml--match-pattern-expand-alternations pattern)
                 alt-patterns)))

(ert-deftest org-ml--match-pattern-expand-alternations ()
  ;; ensure that alternations expand properly
  (unless (fboundp 'org-ml--match-pattern-expand-alternations)
    (error "Function not defined"))
  ;; no alternations
  (should-expand-to-alts '(a) '((a)))
  (should-expand-to-alts '(a b c) '((a b c)))
  ;; 1-level alternations
  (should-expand-to-alts '((x | y)) '((x) (y)))
  (should-expand-to-alts '((x | y) a) '((x a) (y a)))
  (should-expand-to-alts '(a (x | y)) '((a x) (a y)))
  (should-expand-to-alts '(a (x | y) b) '((a x b) (a y b)))
  ;; 1-level alternations with nil
  (should-expand-to-alts '((nil | y)) '(nil (y)))
  (should-expand-to-alts '((nil | y) a) '((a) (y a)))
  (should-expand-to-alts '(a (nil | y)) '((a) (a y)))
  (should-expand-to-alts '(a (nil | y) b) '((a b) (a y b)))
  ;; 1-level serial alternations
  (should-expand-to-alts '((m | n) (x | y)) '((m x) (m y) (n x) (n y)))
  (should-expand-to-alts '(a (m | n) b (x | y) c) '((a m b x c) (a m b y c)
                                                    (a n b x c) (a n b y c)))
  ;; 1-level serial alternations with nil
  (should-expand-to-alts '((nil | n) (x | y)) '((x) (y) (n x) (n y)))
  (should-expand-to-alts '((m | n) (nil | y)) '((m) (m y) (n) (n y)))
  (should-expand-to-alts '(a (nil | n) b (x | y) c) '((a b x c) (a b y c)
                                                      (a n b x c) (a n b y c)))
  (should-expand-to-alts '(a (m | n) b (nil | y) c) '((a m b c) (a m b y c)
                                                      (a n b c) (a n b y c)))
  ;; 2-level alternations
  (should-expand-to-alts '((x | (m | n))) '((x) (m) (n)))
  (should-expand-to-alts '(a (x | (m | n))) '((a x) (a m) (a n)))
  (should-expand-to-alts '((x | y (m | n))) '((x) (y m) (y n)))
  (should-expand-to-alts '(a (x | y (m | n))) '((a x) (a y m) (a y n)))
  ;; 2-level alternations with nil
  (should-expand-to-alts '((nil | (m | n))) '(nil (m) (n)))
  (should-expand-to-alts '(a (nil | (m | n))) '((a) (a m) (a n)))
  (should-expand-to-alts '((nil | y (m | n))) '(nil (y m) (y n)))
  (should-expand-to-alts '(a (nil | y (m | n))) '((a) (a y m) (a y n)))
  (should-expand-to-alts '((x | (nil | n))) '((x) nil (n)))
  (should-expand-to-alts '(a (x | (nil | n))) '((a x) (a) (a n)))
  (should-expand-to-alts '((x | y (nil | n))) '((x) (y) (y n)))
  (should-expand-to-alts '(a (x | y (nil | n))) '((a x) (a y) (a y n))))

(defun should-expand-to (pattern expanded-pattern)
  (should (equal (org-ml--match-pattern-simplify-wildcards pattern)
                 expanded-pattern)))

(ert-deftest org-ml--match-pattern-simplify-wildcards ()
  ;; ensure that bracket and + wildcards expand properly
  (unless (fboundp 'org-ml--match-pattern-simplify-wildcards)
    (error "Function not defined"))
  ;; ?
  (should-expand-to '(x \?) '((nil | x)))
  (should-expand-to '(x \? y) '((nil | x) y))
  ;; +
  (should-expand-to '(x +) '(x x *))
  (should-expand-to '(x + y) '(x x * y))
  ;; brackets
  (should-expand-to '(x [1]) '(x))
  (should-expand-to '(x [2]) '(x x))
  (should-expand-to '(x [0 1]) '((nil | x)))
  (should-expand-to '(x [2 2]) '(x x))
  (should-expand-to '(x [1 2]) '((x | x x)))
  (should-expand-to '(x [1 nil]) '(x x *)))

(ert-deftest org-ml--match-pattern-simplify-wildcards/error ()
  ;; test errors in wildcard expansion
  ;; note, we assume that any malformed patterns are caught later
  ;; so no need to test if we supply two +'s in a row and other garbage
  (unless (fboundp 'org-ml--match-pattern-simplify-wildcards)
    (error "Function not defined"))
  ;; single bracket
  ;; zero not allowed
  (should-error-arg (org-ml--match-pattern-simplify-wildcards '(x [0])))
  ;; negative not allowed
  (should-error-arg (org-ml--match-pattern-simplify-wildcards '(x [-1])))
  ;; double brackets
  ;; double zeros not allowed
  (should-error-arg (org-ml--match-pattern-simplify-wildcards '(x [0 0])))
  ;; negatives not allowed
  (should-error-arg (org-ml--match-pattern-simplify-wildcards '(x [-1 1])))
  (should-error-arg (org-ml--match-pattern-simplify-wildcards '(x [1 -1])))
  (should-error-arg (org-ml--match-pattern-simplify-wildcards '(x [-1 -1])))
  ;; must be ascending order
  (should-error-arg (org-ml--match-pattern-simplify-wildcards '(x [2 1]))))

(ert-deftest org-ml--match-make-slicer-form ()
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
  (let ((fun #'org-ml--match-make-slicer-form))
    ;; nth with non-integer
    (should-error-arg (funcall fun '(:nth "1" bold)))
    ;; sub with non-integers
    (should-error-arg (funcall fun '(:sub "1" 2 bold)))
    (should-error-arg (funcall fun '(:sub 1 "2" bold)))
    ;; sub with flipped integers
    (should-error-arg (funcall fun '(:sub 2 1 bold)))
    (should-error-arg (funcall fun '(:sub -1 -2 bold)))
    ;; sub with split integers
    (should-error-arg (funcall fun '(:sub -1 2 bold)))))

(defmacro match-should-equal (node result &rest patterns)
  "Return form to test if all PATTERNS applied NODE return RESULT."
  (declare (indent 2))
  (let ((tests (--map
                `(should (equal ,result
                                (->> (org-ml-match ',it ,node)
                                     (-map #'org-ml-to-trimmed-string))))
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

(ert-deftest org-ml-match/slicer-predicate ()
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
                   (org-ml--from-string))))
    (match-slicer-should-equal node
      '("2" "3" "4" "5") (headline section))))

(ert-deftest org-ml-match/slicer-any-first ()
  ;; test the :any + condition path with all slicers
  (let ((node (org-ml-build-paragraph!
               "*_1_* */2/* _*3*_ _/4/_ /*5*/ /_6_/")))
    (match-slicer-should-equal node
      '("/2/" "*3*" "/4/" "*5*") (:any (:or bold italic)))))

(ert-deftest org-ml-match/slicer-any-last ()
  ;; test the condition + :any path with all slicers
  (let ((node (org-ml-build-paragraph!
               "*_1_* */2/* _*3*_ _/4/_ /*5*/ /_6_/")))
    (match-slicer-should-equal node
      '("_1_" "/2/" "*5*" "_6_") ((:or bold italic) :any))))

(ert-deftest org-ml-match/empty-patterns ()
  (let ((node (->> (s-join "\n"
                        '("* one"
                          "** two"
                          "** three"))
                (org-ml--from-string))))
    (cl-flet
        ((match-empty
          (n p)
          (should (equal (list n) (org-ml-match p n)))))
      ;; empty patterns
      (match-empty node '())
      (match-empty node '(:first))
      (match-empty node '(:last))
      (match-empty node '(:nth 0))
      (match-empty node '(:sub 0 0))
      ;; wildcards with the empty pattern
      (match-empty node '(:first headline \?))
      (match-empty node '(:first headline *))
      (match-empty node '(:first (nil | headline)))
      (match-empty node '(:last (headline | nil))))))

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

(provide 'org-ml-dev-test)
;;; org-ml-dev-test.el ends here
