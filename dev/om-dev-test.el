;;; om-dev-test.el --- Tests for om

;;; Commentary:

;;; Code:

(require 'dash)
(require 'om-dev-examples-to-tests)
(require 'om-dev-examples)

;;; LIST OPERATIONS

(ert-deftest om--pad-or-truncate/properties ()
  (let ((finite-list '(1 2 3)))
    ;; zero length list with zero length
    (should (equal nil (om--pad-or-truncate 0 'x nil)))
    ;; zero length list with positive length
    (should (equal '(x) (om--pad-or-truncate 1 'x nil)))
    ;; positive length list; length is less
    (should (equal '(1 2) (om--pad-or-truncate 2 'x finite-list)))
    ;; positive length list; length is equal
    (should (equal '(1 2 3) (om--pad-or-truncate 3 'x finite-list)))
    ;; positive length list; length is greater
    (should (equal '(1 2 3 x) (om--pad-or-truncate 4 'x finite-list)))
    ;; positive length list; length is zero
    (should (equal nil (om--pad-or-truncate 0 'x finite-list)))))

;; TODO add plist-get-keys?
;; TODO add plist-get-vals?
;; TODO add plist-map-values?

(ert-deftest om--is-plist/properties ()
  ;; finite plist
  (should (om--is-plist '(:one one :two 2 :three "3")))
  ;; zero-length plist
  (should (om--is-plist nil))
  ;; symbols instead of keywords
  (should-not (om--is-plist '(one one two 2 three "3")))
  ;; incomplete
  (should-not (om--is-plist '(:one one :two 2 :three)))
  ;; not list
  (should-not (om--is-plist ":one one :two 2 :three")))

;; TODO add plist-remove?

;;; inter-list operations

;; These functions operate using indices that refer to spaces between
;; list members. As such there is no such thing as a nonsensical index.
;; Since there will always be the option to add to the front or the
;; back of the list, even an empty list has a logical index that points
;; to these locations (they just happen to be the same). Therefore,
;; the only errors we need to catch here are those that refer to out
;; of range indices.

(defmacro om--inter-list-ops-test (fun input output-single
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

(ert-deftest om--insert-at/properties ()
  (om--inter-list-ops-test (lambda (n list &optional p)
                             (om--insert-at n 'x list p))
    '(1 2) '(x) '(1 2 x) '(x 1 2)))

(ert-deftest om--split-at/properties ()
  (om--inter-list-ops-test #'om--split-at
    '(1 2) nil '((1 2) nil) '(nil (1 2))))

(ert-deftest om--splice-at/properties ()
  (om--inter-list-ops-test (lambda (n list &optional p)
                             (om--splice-at n '(x y) list p))
    '(1 2) '(x y) '(1 2 x y) '(x y 1 2)))

;; These functions operate using indices that refer to explicit
;; members of a list. As such there will be no possible integers that
;; will be valid for an empty list. This provides one extra error case
;; to test, which is the possibility that we cannot operate on the
;; list and thus return nil. All else is the same relative to the
;; inter-list operations tests above

(defmacro om--intra-list-ops-test (fun input output-upper output-lower)
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

(ert-deftest om--remove-at/properties ()
  (om--intra-list-ops-test #'om--remove-at '(1 2 3) '(1 2) '(2 3)))

(ert-deftest om--replace-at/properties ()
  (om--intra-list-ops-test (lambda (n list &optional p)
                             (om--replace-at n 'x list p))
    '(1 2 3) '(1 2 x) '(x 2 3)))

(ert-deftest om--nth/properties ()
  (om--intra-list-ops-test #'om--nth '(1 2 3) 3 1))

;;; list functors

(ert-deftest om--map-first/last/properties ()
  ;; mapping empty list should always return empty list
  (should-not (om--map-first* (s-upcase it) nil))
  (should-not (om--map-last* (s-upcase it) nil))
  ;; mapping list with one member should be same for both
  (should (equal '("X") (om--map-first* (s-upcase it) '("x"))))
  (should (equal '("X") (om--map-last* (s-upcase it) '("x"))))
  ;; mapping list with more than one member should be self-explanatory
  (should (equal '("A" "b" "c") (om--map-first* (s-upcase it) '("a" "b" "c"))))
  (should (equal '("a" "b" "C") (om--map-last* (s-upcase it) '("a" "b" "c"))))
  ;; identity should hold true for any length list (0, 1, and 1+)
  (let ((test-lists '(nil '(1) '(1 2))))
    (--each test-lists (should (equal it (om--map-first #'identity it))))
    (--each test-lists (should (equal it (om--map-last #'identity it))))))

;;; PARSING INVERSION

;; For all org buffer contents, parsing and printing should be
;; perfect inverses.

;; These tests test/use the following:
;; - all the parse functions
;; - `om-to-string'
;; - `om-get-type'

(defun om--test-contents-parse-inversion (type parse-fun contents-list
                                               &optional prefix suffix)
  "Return form to test the parse/print inversion of CONTENTS-LIST.
Use PARSE-FUN to get the node tree from the contents. All should
be parsed to TYPE."
  (declare (indent 2))
  (let ((contents-list (--map (if (consp it) (s-join "\n" it) it)
                              contents-list)))
    (--each contents-list
      (let* ((at (if prefix (1+ (length prefix)) 1))
             (parsed (om--with-org-env
                      (when prefix (insert prefix))
                      (insert it)
                      (when suffix (insert suffix))
                      (funcall parse-fun at)))
             (parsed-type (om-get-type parsed)))
        ;; TODO not DRY
        (unless (equal type parsed-type)
          (print (format "%s parsed as %s" it parsed-type)))
        (should (equal type parsed-type))
        (should (equal it (om-to-string parsed)))))))

;; leaf object nodes

(ert-deftest om--test-parse-inversion/code ()
  (om--test-contents-parse-inversion 'code #'om-parse-object-at
    (list "~code~")))

(ert-deftest om--test-parse-inversion/entity ()
  (om--test-contents-parse-inversion 'entity #'om-parse-object-at
    (list "\\pi" "\\pi{}")))

(ert-deftest om--test-parse-inversion/export-snippet ()
  (om--test-contents-parse-inversion 'export-snippet #'om-parse-object-at
    (list "@@x:y@@")))

(ert-deftest om--test-parse-inversion/inline-babel-call ()
  (om--test-contents-parse-inversion 'inline-babel-call #'om-parse-object-at
    (list "call_ktulu()"
          "call_ktulu(n=1)"
          "call_ktulu[:x y]()"
          "call_ktulu[:x y](n=1)"
          "call_ktulu()[:a b]"
          "call_ktulu(n=1)[:a b]"
          "call_ktulu[:x y]()[:a b]"
          "call_ktulu[:x y](n=1)[:a b]")))

(ert-deftest om--test-parse-inversion/inline-src-block ()
  (om--test-contents-parse-inversion 'inline-src-block #'om-parse-object-at
    (list "src_python{}"
          "src_python{print \"yo\"}"
          "src_python[:x y]{}"
          "src_python[:x y]{print \"yo\"}")))

(ert-deftest om--test-parse-inversion/line-break ()
  (om--test-contents-parse-inversion 'line-break #'om-parse-object-at
    (list "\\\\\n")))

(ert-deftest om--test-parse-inversion/latex-fragment ()
  (om--test-contents-parse-inversion 'latex-fragment #'om-parse-object-at
    (list "$2+2=5$")))

(ert-deftest om--test-parse-inversion/macro ()
  (om--test-contents-parse-inversion 'macro #'om-parse-object-at
    (list "{{{key}}}"
          "{{{key(x=4)}}}")))

(ert-deftest om--test-parse-inversion/statistics-cookie ()
  (om--test-contents-parse-inversion 'statistics-cookie #'om-parse-object-at
    (list "[/]"
          "[0/0]"
          "[%]"
          "[0%]")))

(ert-deftest om--test-parse-inversion/timestamp ()
  (om--test-contents-parse-inversion 'timestamp #'om-parse-object-at
    (list "[2019-01-01 Tue]"
          "[2019-01-01 Tue 12:00]"
          ;; "[2019-01-01 Tue 12:00-13:00]" TODO this doesn't parse correctly
          "[2019-01-01 Tue]--[2019-01-02 Wed]"
          "<2019-01-01 Tue>"
          "[2019-01-01 Tue +1d]"
          "[2019-01-01 Tue -1y]"
          "[2019-01-01 Tue +1d -1y]")))

(ert-deftest om--test-parse-inversion/verbatim ()
  (om--test-contents-parse-inversion 'verbatim #'om-parse-object-at
    (list "=verbatim=")))

(ert-deftest om--test-parse-inversion/plain-text ()
  (om--test-contents-parse-inversion 'plain-text #'om-parse-object-at
    (list "plain-text"
          ;; all syntax chars by themselves should be plain-text
          "**" "~~" "@@:@@" "//" "[]" "[[]]" "{{{}}}" "<>" "<<>>"
          "<<<>>>" "++" "^" "_" "__" "==")))

;; branch object nodes

(ert-deftest om--test-parse-inversion/bold ()
  (om--test-contents-parse-inversion 'bold #'om-parse-object-at
    (list "*bold*")))

(ert-deftest om--test-parse-inversion/footnote-reference ()
  (om--test-contents-parse-inversion 'footnote-reference #'om-parse-object-at
    (list "[fn:label]" "[fn:label:nodes]")
    " "))

(ert-deftest om--test-parse-inversion/italic ()
  (om--test-contents-parse-inversion 'italic #'om-parse-object-at
    (list "/italic/")))

(ert-deftest om--test-parse-inversion/link ()
  (om--test-contents-parse-inversion 'link #'om-parse-object-at
    ;; this is not exhaustive but hopefully good enough
    (list "https://downloadmoreram.com"
          "mailto:vladimirputin@pwned.ru"
          "file:/home/kalilinux/pwneddata"
          "<https://downloadmoreram.com>"
          "[[https://downloadmoreram.com]]"
          "[[https://downloadmoreram.com][legit advice]]")))

(ert-deftest om--test-parse-inversion/radio-target ()
  (om--test-contents-parse-inversion 'radio-target #'om-parse-object-at
    (list "<<<radio>>>")))

(ert-deftest om--test-parse-inversion/strike-through ()
  (om--test-contents-parse-inversion 'strike-through #'om-parse-object-at
    (list "+strike+")))

(ert-deftest om--test-parse-inversion/subscript ()
  (om--test-contents-parse-inversion 'subscript #'om-parse-object-at
    (list "_sub" "_{sub}")
    "dummy"))

(ert-deftest om--test-parse-inversion/superscript ()
  (om--test-contents-parse-inversion 'superscript #'om-parse-object-at
    (list "^super" "^{super}")
    "dummy"))

(ert-deftest om--test-parse-inversion/table-cell ()
  (om--test-contents-parse-inversion 'table-cell #'om-parse-object-at
    (list " cell |")
    "|"))

;; leaf element nodes

(ert-deftest om--test-parse-inversion/babel-call ()
  (om--test-contents-parse-inversion 'babel-call #'om-parse-element-at
    (list "#+CALL: name()\n"
          "#+CALL: name(x=1)\n"
          "#+CALL: name[:x y](x=1)\n"
          "#+CALL: name[:x y]()\n"
          "#+CALL: name[:x y](x=1) :a b\n"
          "#+CALL: name[:x y]() :a b\n"
          "#+CALL: name[]() :a b\n")))

(ert-deftest om--test-parse-inversion/clock ()
  (om--test-contents-parse-inversion 'clock #'om-parse-element-at
    (list "CLOCK: [2019-01-01 Tue]\n"
          "CLOCK: [2019-01-01 Tue]--[2019-01-02 Wed] => 24:00\n"
          ;; TODO this doesn't work
          ;; "CLOCK: [2019-01-01 Tue 00:00-01:00] =>  1:00\n"
          )))

(ert-deftest om--test-parse-inversion/comment ()
  (om--test-contents-parse-inversion 'comment #'om-parse-element-at
    (list "# one\n"
          '("# one"
            "# two\n")
          ;; TODO this doesn't work
          ;; "#\n"
          )))

(ert-deftest om--test-parse-inversion/comment-block ()
  (om--test-contents-parse-inversion 'comment-block #'om-parse-element-at
    (list '("#+BEGIN_COMMENT"
            "battle of being"
            "#+END_COMMENT\n")
          '("#+BEGIN_COMMENT"
            "#+END_COMMENT\n"))))

(ert-deftest om--test-parse-inversion/diary-sexp ()
  (om--test-contents-parse-inversion 'diary-sexp #'om-parse-element-at
    (list "%%()\n" "%%(whatever)\n")))

(ert-deftest om--test-parse-inversion/example-block ()
  (om--test-contents-parse-inversion 'example-block #'om-parse-element-at
    (list '("#+BEGIN_EXAMPLE"
            "example.com"
            "#+END_EXAMPLE\n")
          '("#+BEGIN_EXAMPLE"
            "#+END_EXAMPLE\n"))))

(ert-deftest om--test-parse-inversion/export-block ()
  (om--test-contents-parse-inversion 'export-block #'om-parse-element-at
    (list '("#+BEGIN_EXPORT PLAIN"
            "bullet, bombs, bigotry"
            "#+END_EXPORT\n")
          ;; TODO type needs to always be uppercase?
          ;; '("#+BEGIN_EXPORT plain"
          ;;   "#+END_EXPORT\n")
          '("#+BEGIN_EXPORT PLAIN"
            "#+END_EXPORT\n"))))

(ert-deftest om--test-parse-inversion/fixed-width ()
  (om--test-contents-parse-inversion 'fixed-width #'om-parse-element-at
    (list ": crucifixed\n"
          ;; TODO this make a blank
          ;; ":\n"
          )))

(ert-deftest om--test-parse-inversion/horizontal-rule ()
  (om--test-contents-parse-inversion 'horizontal-rule #'om-parse-element-at
    (list "-----\n")))

(ert-deftest om--test-parse-inversion/keyword ()
  (om--test-contents-parse-inversion 'keyword #'om-parse-element-at
    (list "#+KEY: val\n"
          ;; TODO this randomly fails
          ;; "#+KEY:\n"
          "#+KEY: \n")))

(ert-deftest om--test-parse-inversion/latex-environment ()
  (om--test-contents-parse-inversion 'latex-environment #'om-parse-element-at
    (list '("\\begin{env}"
            "\\end{env}\n")
          '("\\begin{env}"
            "latex >>> ms word"
            "\\end{env}\n"))))

(ert-deftest om--test-parse-inversion/node-property ()
  (om--test-contents-parse-inversion 'node-property #'om-parse-element-at
    (list ":node:     prop\n"
          ;; TODO this seems arbitrary
          ;; ":node\n"
          ":node:     \n")
    "* dummy\n:PROPERTIES:\n"
    ":END:\n"))

(ert-deftest om--test-parse-inversion/planning ()
  (om--test-contents-parse-inversion 'planning #'om-parse-element-at
    (list "CLOSED: <2019-01-01 Tue>\n"
          "CLOSED: <2019-01-01 Tue +1d>\n"
          "CLOSED: <2019-01-01 Tue -1y>\n"
          "CLOSED: <2019-01-01 Tue +1d -1y>\n")
    "* dummy\n"))

(ert-deftest om--test-parse-inversion/src-block ()
  (om--test-contents-parse-inversion 'src-block #'om-parse-element-at
    (list '("#+BEGIN_SRC"
            "#+END_SRC\n")
          ;; TODO this doesn't work if is isn't indented
          '("#+BEGIN_SRC python -n :x y"
            "  print \"yo\""
            "#+END_SRC\n"))))

;;; branch element nodes with child object nodes

(ert-deftest om--test-parse-inversion/paragraph ()
  (om--test-contents-parse-inversion 'paragraph #'om-parse-element-at
    ;; TODO there are probably other things I could put here
    (list "paragraph\n")))

(ert-deftest om--test-parse-inversion/table-row ()
  (om--test-contents-parse-inversion 'table-row #'om-parse-table-row-at
    (list "| cell |\n"
          ;; TODO this makes an empty string
          ;; "| |\n"
          )))

(ert-deftest om--test-parse-inversion/verse-block ()
  (om--test-contents-parse-inversion 'verse-block #'om-parse-element-at
    (list '("#+BEGIN_VERSE"
            "#+END_VERSE\n")
          '("#+BEGIN_VERSE"
            "Once upon a midnight dreary..."
            "#+END_VERSE\n"))))

;;; branch element nodes with child element nodes

(ert-deftest om--test-parse-inversion/center-block ()
  (om--test-contents-parse-inversion 'center-block #'om-parse-element-at
    (list '("#+BEGIN_CENTER"
            "#+END_CENTER\n")
          '("#+BEGIN_CENTER"
            "Of the universe..."
            "#+END_CENTER\n"))))

(ert-deftest om--test-parse-inversion/drawer ()
  (om--test-contents-parse-inversion 'drawer #'om-parse-element-at
    (list '(":LOGBOOK:"
            ":END:\n")
          '(":LOGBOOK:"
            "- logged thingy"
            ":END:\n"))))

(ert-deftest om--test-parse-inversion/dynamic-block ()
  (om--test-contents-parse-inversion 'dynamic-block #'om-parse-element-at
    (list '("#+BEGIN: name"
            "#+END:\n")
          '("#+BEGIN: name"
            "Random contents..."
            "#+END:\n"))))

(ert-deftest om--test-parse-inversion/footnote-definition ()
  (om--test-contents-parse-inversion 'footnote-definition #'om-parse-element-at
    (list "[fn:label] \n"
          ;; TODO needs a random space at the end
          ;; "[fn:label]"
          "[fn:label] stuff after\n"
          )))

(ert-deftest om--test-parse-inversion/headline ()
  (om--test-contents-parse-inversion 'headline #'om-parse-element-at
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

(ert-deftest om--test-parse-inversion/item ()
  (om--test-contents-parse-inversion 'item #'om-parse-item-at
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

(ert-deftest om--test-parse-inversion/plain-list ()
  (om--test-contents-parse-inversion 'plain-list #'om-parse-element-at
    (list "- thing\n"
          "1. thing\n"
          '("- thing"
            "- more thing\n"))))


(ert-deftest om--test-parse-inversion/property-drawer ()
  (om--test-contents-parse-inversion 'property-drawer #'om-parse-element-at
    (list '(":PROPERTIES:"
            ":END:\n")
          '(":PROPERTIES:"
            ":Effort:   0:30"
            ":END:\n"))
    "* dummy\n"))

(ert-deftest om--test-parse-inversion/quote-block ()
  (om--test-contents-parse-inversion 'quote-block #'om-parse-element-at
    (list '("#+BEGIN_QUOTE"
            "#+END_QUOTE\n")
          '("#+BEGIN_QUOTE"
            "Fear is the mind killer..."
            "#+END_QUOTE\n"))))

(ert-deftest om--test-parse-inversion/section ()
  (om--test-contents-parse-inversion 'section #'om-parse-section-at
    (list "things that could be a paragraph\n"
          "#+KEY: val\n"
          "# nothing important...\n")))

(ert-deftest om--test-parse-inversion/special-block ()
  (om--test-contents-parse-inversion 'special-block #'om-parse-element-at
    (list '("#+BEGIN_special"
            "#+END_special\n")
          '("#+BEGIN_special"
            "You don't belong here"
            "#+END_special\n"))))

(ert-deftest om--test-parse-inversion/table ()
  (om--test-contents-parse-inversion 'table #'om-parse-element-at
    (list "| simple |\n"
          "| less | simple |\n"
          '("| R | A |"
            "| G | E |\n")
          ;; TODO this makes a blank string
          ;; "| |\n"
          )))

;;; NODE PROPERTY COMPLETENESS

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
   (om-build-planning :closed (om-build-timestamp! '(2019 1 1) :active t))
   (->> (om--from-string "* dummy\nCLOSED: <2019-01-01 Tue>")
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
    (should-error-arg (funcall fun '(:first) nil))
    (should-error-arg (funcall fun '(:last) nil))
    (should-error-arg (funcall fun '(:nth) nil))
    (should-error-arg (funcall fun '(:sub) nil))
    ;; nth with non-integer
    (should-error-arg (funcall fun '(:nth "1" bold) nil))
    ;; nth with integer but nothing after
    (should-error-arg (funcall fun '(:nth 1) nil))
    ;; sub with non-integers
    (should-error-arg (funcall fun '(:sub "1" 2 bold) nil))
    (should-error-arg (funcall fun '(:sub 1 "2" bold) nil))
    ;; sub with flipped integers
    (should-error-arg (funcall fun '(:sub 2 1 bold) nil))
    (should-error-arg (funcall fun '(:sub -1 -2 bold) nil))
    ;; sub with split integers
    (should-error-arg (funcall fun '(:sub -1 2 bold) nil))
    ;; sub with nothing after it
    (should-error-arg (funcall fun '(:sub 1 2) nil))))

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
