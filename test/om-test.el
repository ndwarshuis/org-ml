;;; om-test.el --- Tests for om

;;; Commentary:

;;; Code:

(require 'dash)

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
