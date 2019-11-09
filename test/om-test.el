;;; om-test.el --- Tests for om

;;; Commentary:

;;; Code:

(require 'dash)

;; length testing
;; TODO this should really be in terms of strings that are parsed

;; (defun should-len (elem)
;;   (when elem
;;     (let* ((pred-len (om-elem--length elem))
;;            (actual-len (length (om-elem-to-string elem)))
;;            (res (= pred-len actual-len)))
;;       (unless res
;;         (print (format "elem: %S" elem))
;;         (print (format "printed: %s" (om-elem-to-string elem)))
;;         (print (format "predicted len: %s" (om-elem--length elem)))
;;         (print (format "actual len: %s" (length (om-elem-to-string elem)))))
;;       ;; pretty sure this is not the way its supposed to work
;;       (should res))))

(ert-deftest om-elem-is-zero-length/single ()
  (should (om-elem-is-zero-length-p (om-elem-build-bold)))
  (should-not (om-elem-is-zero-length-p (om-elem-build-link "path"))))

(ert-deftest om-elem-is-zero-length/nested ()
  (should (om-elem-is-zero-length-p (om-elem-build-bold
                                     (om-elem-build-bold))))
  (should-not (om-elem-is-zero-length-p (om-elem-build-bold
                                         (om-elem-build-bold)
                                         "stuff")))
  (should-not (om-elem-is-zero-length-p (om-elem-build-bold
                                         (om-elem-build-bold "bold"))))
  (should-not (om-elem-is-zero-length-p (om-elem-build-bold
                                         (om-elem-build-bold)
                                         (om-elem-build-link "path")))))

(defun should-len (elem)
  (when elem
    (let* ((norm-elem (om-elem--normalize-boundaries 1 elem))
           (pred-len (om-elem-length norm-elem))
           (actual-len (length (om-elem-to-string elem)))
           (res (= pred-len actual-len)))
      (unless res
        (print (format "elem: %S" norm-elem))
        (print (format "printed: %s" (om-elem-to-string elem)))
        (print (format "predicted len: %s" pred-len))
        (print (format "actual len: %s" actual-len)))
      ;; pretty sure this is not the way its supposed to work
      (should res))))

;; (defmacro om-elem--len-post-blank (type &rest args)
;;   (let ((fun (intern (format "om-elem-build-%s" type))))
;;     `(progn
;;        (should-len (,fun ,@args))
;;        (should-len (,fun ,@args :post-blank 1)))))

;; (defun om-elem--test-recursive-objects (type)
;;   (let ((fun (intern (format "om-elem-build-%s" type)))
;;         (test-name (intern (format "om-elem--length/%s" type)))
;;         (powerset (->> '((:post-blank 1)
;;                          ("text")
;;                          (funcall #',fun "text")
;;                          (funcall #',fun :post-blank 1 "text"))
;;                        (-powerset))))
;;     (print powerset)
;;     (eval
;;      `(ert-deftest ,test-name ()
;;         (funcall #',fun "text")
;;         (funcall #',fun :post-blank 1 "text")
;;         (funcall #',fun "text" (funcall #',fun "text"))
;;         (funcall #',fun :post-blank 1 "text" (funcall #',fun "text"))
;;         (funcall #',fun "text" (funcall #',fun :post-blank 1 "text"))
;;         (funcall #',fun :post-blank 1 "text" (funcall #',fun :post-blank 1 "text"))))))

(cl-defun om-elem--combinations (&rest lists)
  "Make all combinations from LISTS.
Example: the two lists are (1 2) and (a b), this will spit out
\((1 a) (1 b) (2 a) (2 b))."
  (apply #'-table-flat #'list lists))

(cl-defun om-elem--arg-combinations (pos key rest)
  "Make all combinations or POS, KEY, and REST arguments.

POS is a list of all positional arguments (required plus optional)
where the length of the list is equal to the number of required and
optional argument. Each position in the list is another list of all
disired values to be tested. For example ((a b) (1 2)) will test a and
b at position 1 and 1 and 2 at position two.

KEY is a list of all keyword arguments to test, where each member of
the list is a two-membered list like (:key val).

REST is a list of all possible rest arguments and is a list like (one
two three).

The sets of combinations to be tested include all combinations of POS
and all combinations of POS and the powersets of KEY and/or REST"
  (let* ((pos* (apply #'om-elem--combinations pos))
         (rest (--map (list it) rest))
         ;; TODO this requires that each key only have one tested
         ;; value otherwise we will get (:foo 1 :foo 2) (two keys of
         ;; the same name supplied as arguments
         ;; TODO will not test the nil keyword case if nothing else
         ;; is provided
         (key-pwrset (->> (-powerset key)
                          (-non-nil)
                          (--map (-flatten-n 1 it))))
         (rest-pwrset (->> (-powerset rest)
                           (-non-nil)
                           (--map (-flatten-n 1 it)))))
    ;; (print pos*)
    ;; (print key-pwrset)
    ;; (print rest-pwrset)
    (->>
     (append
      (-map #'list pos*)
      (-table-flat #'list pos* key-pwrset)
      (-table-flat #'list pos* rest-pwrset)
      (-table-flat #'list pos* key-pwrset rest-pwrset))
     (--map (-flatten-n 1 it)))))

(defun om-elem--element-combinations (&rest elem-schemas)
  ;; (type pos key rest)
  (->>
   (--map
    (if (stringp it) (list it)
      (let ((fun (intern (format "om-elem-build-%s" (car it))))
            (combinations (apply #'om-elem--arg-combinations (cdr it))))
        (--map (apply fun it) combinations)))
    elem-schemas)
   (apply #'append)))
   ;; (-powerset)
   ;; (-non-nil)))

;; (cl-defun om-elem--arg-combinations (&key req opt key rest)
;;   "Make all combinations or REQ, OPT, KEY, and REST arguments."
;;   (let* ((req (--map (list it) req))
;;          (opt (--map (list it) opt))
;;          (rest (--map (list it) rest))
;;          (key-pwrset (-non-nil (-powerset key)))
;;          (rest-pwrset (-non-nil (-powerset rest))))
;;     (->>
;;      (append
;;       (-map #'list req)
;;       (-table-flat #'list req opt)
;;       (-table-flat #'list req opt key-pwrset)
;;       (-table-flat #'list req opt rest-pwrset)
;;       (-table-flat #'list req opt key-pwrset rest-pwrset))
;;      (--map (-flatten-n 2 it)))))

(cl-defun om-elem--test-arg-combinations (type &key pos key rest)
  "Test every combination of args for TYPE.
REQ is required supplies like ((one) (two)).
OPT is like REQ but for optional, note that all slots must be filled.
KEY is keyword args like ((:one one) (:two two))
REST is rest arguments like (one two three)"
  (let ((fun (intern (format "om-elem-build-%s" type)))
        (test-name (intern (format "om-elem--length/%s" type)))
        (combinations (om-elem--arg-combinations pos key rest)))
    ;; (print (length combinations))
    (eval
     `(ert-deftest ,test-name ()
        (--each ',combinations (should-len (apply #',fun it)))))))

;; test object lengths

(cl-defun om-elem--test-object-length (type &key pos key)
  (om-elem--test-arg-combinations
   type
   :pos pos
   :key (append '((:post-blank 1)) `(,@key))))

(--each '(code target diary-sexp-timestamp verbatim)
  (om-elem--test-object-length it :pos '((":(){ :|:& };:")))) ; fork you

(om-elem--test-object-length 'inline-babel-call
                             :pos '(("whome"))
                             :key '((:arguments "n=666 u=nil")
                                    (:inside-header ":results output")
                                    (:end-header ":results html")))

(om-elem--test-object-length 'inline-src-block
                             :pos '(("trumpscript" "Make America great")
                                    ("nilscript" ""))
                             :key '((:parameters "tangle yes")))

(om-elem--test-object-length 'line-break)

(om-elem--test-object-length 'macro
                             :pos '(("nilbog"))
                             :key '((:args ("n=5")))) ; troll you

(om-elem--test-object-length 'statistics-cookie :pos '((1 nil)
                                                       (2 nil)))

;; TODO add warning/repeater type to timestamp
(om-elem--test-object-length
 'timestamp :pos '((active inactive)
                   ((2112 1 1) (2112 1 1 12 12))
                   (nil (2112 2 2) (2112 2 2 12 12) (2112 1 1 12 14))))

;; test element lengths

(om-elem--test-object-length 'babel-call
                             :pos '(("lol"))
                             :key '((:arguments "n=666 u=nil")
                                    (:inside-header ":results output")
                                    (:end-header ":results html")))

(om-elem--test-object-length
 'clock
 :pos '(((2112 1 1) (2112 1 1 12 12))
        (nil (2112 2 2) (2112 1 1 12 14) (2112 2 2 12 12))))

(om-elem--test-object-length
 'comment :pos '(("champagne cocaine gasoline")))

(om-elem--test-object-length
 'comment-block :pos '(("champagne cocaine gasoline")))

(om-elem--test-object-length 'entity
                             :pos '(("fakepunk"))
                             :key '((:use-brackets-p t)))

(om-elem--test-object-length 'diary-sexp :pos '(("(fork u)")))

(om-elem--test-object-length 'example-block
                             :pos '(("ipsum lorem\nmore stuff"
                                     "ipsum lorem\n  more stuff"))
                             :key '((:switches "n=1")
                                    (:preserve-indent t)))

(om-elem--test-object-length 'export-block
                             :pos '(("latex") ("\begin{insanity}")))

(om-elem--test-object-length 'fixed-width :pos '(("fixed")))

(om-elem--test-object-length 'horizontal-rule)

(om-elem--test-object-length 'keyword :pos '(("key") ("value")))

(om-elem--test-object-length 'latex-environment :pos '(("baby") ("metal")))

(om-elem--test-object-length 'node-property :pos '(("key") ("value")))

(om-elem--test-object-length
 'planning :key '((:closed (2112 1 1))
                  (:scheduled (2112 1 1))
                  (:deadline (2112 1 1))))

(om-elem--test-object-length 'src-block
                             :pos '(("Make America great\nSay it loud"
                                     "Make America great\n  Say it loud"))
                             :key '((:language "trumpscript")
                                    (:switches "-n")
                                    (:parameters "order=66")
                                    (:preserve-indent t)))

;; test recursive object lengths

(cl-defun om-elem--test-recursive-object (type &key pos key)
  (let ((contents (om-elem--element-combinations
                    '(bold nil ((:post-blank 1)) ("more")) "plain")))
    (om-elem--test-arg-combinations
     type
     :pos pos
     :key (append '((:post-blank 1)) `(,@key))
     :rest contents)))

(--each '(bold italic strike-through table-cell radio-target underline)
  (om-elem--test-recursive-object it))

(om-elem--test-recursive-object 'footnote-reference :key '((:label "100")))

(--each '("coderef" "custom-id" "fuzzy" "file")
  (om-elem--test-recursive-object
   'link :pos '(("https://butthole.com")) :key `((:type ,it))))

(--each '(superscript subscript)
  (om-elem--test-recursive-object it :key `((:use-brackets-p t))))

;; container element length

(let ((contents (om-elem--element-combinations
                 '(bold nil ((:post-blank 1)) ("more")) "plain"
                 "whitespace\n\n\n")))
  (om-elem--test-arg-combinations
   'paragraph
   :key '((:post-blank 1))
   :rest contents))

;; ;; TODO test hline? kinda unnecessary...maybe
(let ((contents (om-elem--element-combinations
                 '(table-cell nil ((:post-blank 1)) ("more")))))
  (om-elem--test-arg-combinations
   'table-row
   :key '((:post-blank 1))
   :rest contents))

(let ((contents (om-elem--element-combinations
                 '(bold nil ((:post-blank 1)) ("more")) "plain"
                 "whitespace\n\n\n")))
  (om-elem--test-arg-combinations
   'verse-block
   :key '((:post-blank 1))
   :rest contents))

;; greater element length

(cl-defun om-elem--test-greater-element (type &key pos key rest)
  (om-elem--test-arg-combinations
   type
   :pos pos
   :key (append '((:post-blank 1)) `(,@key))
   :rest rest))

(let ((contents (om-elem--element-combinations
                 '(bold nil ((:post-blank 1)) ("more")) "plain"
                 "whitespace\n\n\n\n")))
  (om-elem--test-greater-element
   'center-block :rest (-map #'om-elem-build-paragraph contents)))

(let ((contents (om-elem--element-combinations
                 '(bold nil ((:post-blank 1)) ("more")) "plain"
                 "whitespace\n\n\n\n")))
  (om-elem--test-greater-element
   'drawer
   :pos '(("drawer"))
   :rest (-map #'om-elem-build-paragraph contents)))

(let ((contents (om-elem--element-combinations
                 '(bold nil ((:post-blank 1)) ("more")) "plain"
                 "whitespace\n\n\n\n")))
  (om-elem--test-greater-element
   'dynamic-block
   :pos '(("name") ("args"))
   :rest (-map #'om-elem-build-paragraph contents)))

(let ((contents (om-elem--element-combinations
                 '(bold nil ((:post-blank 1)) ("more")) "plain"
                 "whitespace\n\n\n\n")))
  (om-elem--test-greater-element
   'footnote-definition
   :pos '(("label"))
   :rest (-map #'om-elem-build-paragraph contents)))

(let ((contents (om-elem--element-combinations
                 '(bold nil ((:post-blank 1)) ("more"))
                 "whitespace\n\n\n\n")))
  (om-elem--test-greater-element
   'headline
   :key '((:title ("title"))
          (:pre-blank 1)
          (:todo-keyword "TODO")
          (:tags ("TAG"))
          (:priority ?A)
          ;; (:footnote-section-p t)
          (:commentedp t)
          (:archivedp t))
   :rest (--map
          (om-elem-build-section (om-elem-build-paragraph it))
          contents)))

(let ((contents (om-elem--element-combinations
                 '(bold nil ((:post-blank 1)) ("more"))
                 "whitespace\n\n\n\n")))
  (om-elem--test-greater-element
   'item
   :key '((:bullet 1)
          (:checkbox on)
          (:tag ("TAG"))
          (:counter 10))
   :rest (--map (om-elem-build-paragraph it) contents)))

(ert-deftest om-elem--item-length-elements()
  ;; TODO not all these make sense...like node prop in an item?
  (let ((contents (list
                   (om-elem-build-babel-call "foo")
                   (om-elem-build-clock '(2019 1 1))
                   (om-elem-build-comment "foo")
                   (om-elem-build-comment-block "foo")
                   (om-elem-build-diary-sexp "foo")
                   (om-elem-build-example-block "foo")
                   (om-elem-build-export-block "foo" "bar")
                   (om-elem-build-fixed-width "foo")
                   (om-elem-build-horizontal-rule)
                   (om-elem-build-keyword "foo" "bar")
                   (om-elem-build-latex-environment "foo" "bar")
                   (om-elem-build-node-property "foo" "bar")
                   (om-elem-build-planning :closed '(2019 1 1))
                   (om-elem-build-src-block "foo"))))
    (--each contents (should-len (om-elem-build-item it)))))

(ert-deftest om-elem--item-length-containers()
  ;; TODO not all these make sense...like property drawer in item?
  (let* ((para (om-elem-build-paragraph "i am a\npuppet"))
         (contents (list
                    ;; one nested item (paragraph first)
                    (->> (om-elem-build-item para)
                         (om-elem-build-plain-list))
                    ;; one nested item (paragraph second)
                    (->> (om-elem-build-item
                          (om-elem-build-horizontal-rule) para)
                         (om-elem-build-plain-list))
                    ;; a bunch of other containers
                    (om-elem-build-center-block para)
                    (om-elem-build-drawer "name" para)
                    (om-elem-build-dynamic-block "name" "args" para)
                    (om-elem-build-footnote-definition "label" para)
                    (->> (om-elem-build-node-property "key" "val")
                         (om-elem-build-property-drawer))
                    (om-elem-build-quote-block para)
                    (om-elem-build-section para)
                    (om-elem-build-special-block "type" para)
                    (om-elem-build-table! :tblfm '("$2" "$4")
                                          '("a" "ab")
                                          '("ccc" "d"))
                    (om-elem-build-verse-block "this\nis\nwar\n"))))
    (--each contents (should-len (om-elem-build-item it)))))

;; contents0 to test blank cells and post blank
;; contents1 to test alignment
(let* ((contents0 (om-elem--element-combinations
                  '(table-cell nil ((:post-blank 1)) ("data"))))
       (contents1 (->> (list (om-elem-build-table-cell "a")
                             (om-elem-build-table-cell
                              (om-elem-build-bold "a")))
                       (-powerset)
                       (-non-nil))))
  (om-elem--test-greater-element
   'table
   :key '((:tblfm ("$1" "$2")))
   :rest (append (--map (om-elem-build-table-row it) contents0)
                 (--map (apply #'om-elem-build-table-row it) contents1))))

(let ((contents (om-elem--element-combinations
                 '(node-property (("key") ("" "value"))
                                 ((:post-blank 1))
                                 nil))))
  (om-elem--test-greater-element 'property-drawer :rest contents))

(let ((contents (om-elem--element-combinations
                 '(bold nil ((:post-blank 1)) ("more")) "plain"
                 "whitespace\n\n\n\n")))
  (om-elem--test-greater-element
   'quote-block
   :rest (-map #'om-elem-build-paragraph contents)))

(let ((contents (om-elem--element-combinations
                 '(bold nil ((:post-blank 1)) ("more")) "plain"
                 "whitespace\n\n\n\n")))
  (om-elem--test-greater-element
   'section
   :rest (-map #'om-elem-build-paragraph contents)))

(let ((contents (om-elem--element-combinations
                 '(bold nil ((:post-blank 1)) ("more")) "plain"
                 "whitespace\n\n\n\n")))
  (om-elem--test-greater-element
   'special-block
   :pos '(("type"))
   :rest (-map #'om-elem-build-paragraph contents)))

;; other...

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
         (list (om-elem-build-item :checkbox 'off :tag "one")
               (om-elem-build-item :checkbox 'on :tag "two")
               (om-elem-build-item :checkbox 'on :tag "three"))))
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
          (om-elem-build-headline :title "headline1" :todo-keyword "TODO")
          (om-elem-build-headline :title "headline2" :todo-keyword "DONE"))))
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
  (let ((dummy (om-elem-build-headline "dummy")))
    ;; TODO test invalid types
    ;; (should-error (om-elem-find dummy 'invalid))
    (should-error (om-elem-find '(:many) dummy))
    (should-error (om-elem-find '(:many section paragraph) dummy))))

;;; om-test.el ends here
