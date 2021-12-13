;;; org-ml-docs.el --- Extract org-ml's docs -*- lexical-binding: t; -*-

;; Copyright (C) 2015 Free Software Foundation, Inc.

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

(require 's)
(require 'lispy)
(require 'dash)
(require 'help-fns)
(require 'package)

(setq text-quoting-style 'grave)

(defvar org-ml-dev-examples-list '())

(defvar org-ml-dev-recipe-list '())

(defconst org-ml-elem--fill-column 80)

(defun org-ml-get-package-version ()
  "Get version of om package."
  (with-current-buffer (find-file-noselect "org-ml.el")
    (mapconcat 'number-to-string (package-desc-version (package-buffer-info)) version-separator)))

(defun format-actual (actual)
  (with-temp-buffer
    (--> (format "%S" actual)
         (replace-regexp-in-string "\n" "\\n" it t t)
         (insert it))
    (goto-char (point-min))
    (lispy-multiline)
    (buffer-string)))

(defun format-expected (sym expected)
  (let* ((s (s-lines (format "%S" expected)))
         (header (format " ;; %S %s" sym (car s)))
         (rest (--map (if (stringp expected) (s-prepend " ;      " it)
                        (s-prepend " ;     " it))
                      (-drop 1 s))))
    (s-join "\n" (cons header rest))))

(defun example-to-string (example)
  (-let* (((actual sym expected) example)
          (expected
           (if (eq (and (listp expected) (car expected)) :result)
               (s-join "\n" (cdr expected))
             expected))
          (actual (format-actual actual))
          (comment
           (cond
            ((eq sym '=>) (format-expected sym expected))
            ((eq sym '~>) (format-expected sym expected))
            ((eq sym '$>) (concat " ;; Output these buffer contents\n"
                                  (format-expected sym expected)))
            ((eq sym '!!>) (format "Error"))
            (t (error "Invalid test case: %s" `(,actual ,sym ,expected))))))
    (--> comment
         (format "%s\n%s\n" actual it)
         (replace-regexp-in-string "\\\\\\?" "?" it)
         ;; (replace-regexp-in-string "\n" "\\n" it t t)
         ;; (replace-regexp-in-string "\t" "\\t" it t t)
         (replace-regexp-in-string "\r" "\\r" it t t))))
         ;; (format "```el\n%s\n```\n" it))))

(defun docs--signature (function)
  "Given FUNCTION (a symbol), return its argument list.
FUNCTION may reference an elisp function, alias, macro or a subr."
  (let* ((function-value (indirect-function function))
         (is-alias (not (eq function-value (symbol-function function))))
         ;; if FUNCTION isn't an alias, function-symbol is simply FUNCTION
         (function-symbol function))

    (when is-alias
      ;; find the last symbol in the alias chain
      (while (symbolp (symbol-function function-symbol))
        (setq function-symbol (symbol-function function-symbol))))

    (or
     (-some->> (help-split-fundoc (documentation function-value)
                                  function-symbol)
               (car)
               (downcase)
               (read)
               (cdr))
     (help-function-arglist function-symbol))))
;; (if (subrp function-value)
;;     ;; read the docstring to find the signature for subrs
;;     (let* ((docstring-args (car (help-split-fundoc
;;                                  (documentation function-value)
;;                                  function-symbol)))
;;            (fun-with-args (read (downcase docstring-args))))
;;       (cdr fun-with-args))
;;   ;; otherwise get the signature directly
;;   (help-function-arglist function-symbol))))

(defun format-doc (cmd)
  ;; remove extra signature for cl-defun org-ml-dev-examples-list
  ;; TODO this is hacky but it works
  (let ((doc (documentation cmd)))
    (unless doc (error "No docstring set for %s" cmd))
    (if (not (s-matches? "(fn .*)" doc)) doc
      (->> (s-lines doc) (-drop-last 2) (s-join "\n")))))

(defun filter-hidden (args)
  (->> (--split-when (eq it :end-hidden) args)
       (--mapcat (--take-while (not (eq it :begin-hidden)) it))))

(defmacro defexamples (cmd &rest examples)
  `(add-to-list 'org-ml-dev-examples-list
                (list
                 ',cmd
                 (docs--signature ',cmd)
                 (format-doc ',cmd)
                 (->> ',examples
                      (filter-hidden)
                      (-partition 3)
                      (-map 'example-to-string)))))

(defun format-buffer-contents (list)
  (->> (--map (format "; %s" it) list)
              (s-join "\n")
              (format ";; Given the following contents:\n%s\n")))

(defmacro defexamples-content (cmd docstring &rest args)
  `(cl-flet
       ((formatted-string?
         (list)
         (memq (and (listp list) (car list)) '(:buffer :comment)))
        (format-content
         (list)
         (->> (car list)
              (-drop 1)
              (format-buffer-contents)))
              ;; (--map (format "; %s" it))
              ;; (s-join "\n")
              ;; (format ";; Given the following contents:\n%s\n")))
        (format-comment
         (list)
         (let ((comment (->> (car list)
                             (-drop 1)
                             (s-join " ")
                             (format ";; %s"))))
           (with-temp-buffer
             (emacs-lisp-mode)
             (insert comment)
             (let ((fill-column org-ml-elem--fill-column))
               (fill-paragraph))
             (buffer-string)))))
     (let* ((doc (or ,docstring (format-doc ',cmd)))
            (example
             (->> (filter-hidden ',args)
                  (-partition-by #'formatted-string?)
                  (--map (cond
                          ((eq :comment (car (car it)))
                           (format-comment it))
                          ((eq :buffer (car (car it)))
                           (format-content it))
                          (t (-some->>
                              (-partition 3 it)
                              (-map #'example-to-string)
                              (s-join "\n"))))))))
       (add-to-list 'org-ml-dev-examples-list (list ',cmd
                                     (docs--signature ',cmd)
                                     doc
                                     (or example '("no examples :(")))))))

(defmacro defrecipe (header description contents form operator result)
  `(let ((example (example-to-string (list ',form ',operator ',result)))
         (contents* (format-buffer-contents ',contents)))
     (add-to-list 'org-ml-dev-recipe-list
                  (format "## %s\n\n%s\n\n```el\n%s\n%s```\n"
                          ,header ,description contents* example))))

(defmacro def-example-subgroup (group desc &rest examples)
  `(progn
     ;; (add-to-list 'org-ml-dev-examples-list ,(concat "### " group))
     (setq org-ml-dev-examples-list (cons ,(concat "### " group) org-ml-dev-examples-list))
     (when ,desc
       ;; (add-to-list 'org-ml-dev-examples-list ,desc))
       (setq org-ml-dev-examples-list (cons ,desc org-ml-dev-examples-list)))
     ,@examples))

(defmacro def-example-group (group desc &rest examples)
  `(progn
     ;; (add-to-list 'org-ml-dev-examples-list ,(concat "## " group))
     (setq org-ml-dev-examples-list (cons ,(concat "## " group) org-ml-dev-examples-list))
     (when ,desc
       ;; (add-to-list 'org-ml-dev-examples-list ,desc))
       (setq org-ml-dev-examples-list (cons ,desc org-ml-dev-examples-list)))
     ,@examples))


(defun format-link (string-name)
  (-let* ((name (intern string-name))
          ((_ signature _ _) (assoc name org-ml-dev-examples-list)))
    (if signature
        (format "[`%s`](#%s)" name (github-id name signature))
      (format "`%s`" name))))

(defun format-docstring-forms (docstring)
  (cl-labels
      ((find-matching-right
        (p)
        ;; return point of matching ")" or nil if not found
        (ignore-errors
          (save-excursion
            (goto-char p)
            (forward-sexp)
            (1- (point)))))
       (has-leading-function?
        (string)
        (->> (s-replace-regexp "(+" "" string)
             (s-split " ")
             (car)
             (intern)
             (fboundp)))
       (has-all-cap-syms?
        (string)
        (->> (s-replace-regexp "[().,]" "" string)
             (s-replace "[" "")
             (s-replace "]" "")
             (s-split " ")
             (--remove (equal it ""))
             (--all? (or (member it '("t" "nil" "|" "*" "?"))
                         (s-matches? "^[A-Z0-9\\-]+$" it)
                         (s-matches? "^\\(:\\|&\\)[a-z0-9-]+$" it)))))
       (is-form?
        (string)
        (or ;; (has-leading-function? string)
         (has-all-cap-syms? string))))
    (let (case-fold-search)
      (with-temp-buffer
        (insert docstring)
        (goto-char (point-min))
        (while (and (< (point) (point-max))
                    (search-forward "(" nil t))
          (-when-let (e (find-matching-right (1- (point))))
            (when (is-form? (buffer-substring (point) e))
              (downcase-region (point) e)
              (goto-char (1- (point)))
              (insert "`")
              (goto-char (+ 2 e))
              (insert "`")))
          (unless (= (point) (point-max))
            (forward-char)))
        (buffer-string)))))

(defun format-docstring-args (signature docstring)
  (let ((sig-args (->> signature
                       (--remove (memq it '(&optional &key &rest)))
                       (--map (if (consp it) (car it) it))
                       (-map #'symbol-name))))
    (cl-flet
        ((quote-and-downcase
          (string)
            ;; hack to work around % not being part of word boundaries
          (let ((s (s-chop-suffix "%" (downcase string))))
             (if (member s sig-args) (format "**`%s`**" s)
               (format "`%s`" s)))))
      (replace-regexp-in-string
       "\\b\\(?3:[A-Z][A-Z-]*[0-9*]*\\)\\(\\*\\|%\\|\\b\\)"
       ;; "[^A-Z0-9-]\\([A-Z0-9-]+\\)[^A-Z0-9-]"
       #'quote-and-downcase docstring t nil 3))))

(defun format-docstring-backquoted (docstring)
  (cl-flet
      ((unquote-and-link
        (string)
        (format-link (substring string 1 -1))))
    (replace-regexp-in-string "`\\([^ \n]+\\)'" #'unquote-and-link docstring t)))

(defun format-docstring-indent (docstring)
  (replace-regexp-in-string "^  " "    " docstring))

(defun format-docstring-strings (docstring)
  (cl-flet
      ((quote-string
        (string)
        (format "`%s`" string)))
  (s-replace-regexp "\"[[:ascii:]]*?\"" #'quote-string docstring)))

(defun format-docstring (signature docstring)
  (let (case-fold-search)
    (->> docstring
         (format-docstring-strings)
         (format-docstring-forms)
         (format-docstring-args signature)
         (format-docstring-backquoted)
         (format-docstring-indent))))

(defun function-to-md (function)
  (if (stringp function)
      (concat "\n" (s-replace "### " "### " function) "\n")
    (-let [(command-name signature docstring examples) function]
      (unless docstring
        (error "No docstring supplied for %s" command-name))
      (format "#### %s `%S`\n\n%s\n\n```el\n%s\n```\n"
      ;; (format "### %s `%s`\n\n%s\n\n%s"
              command-name
              signature
              (format-docstring signature docstring)
              ;; (mapconcat 'identity (-take 3 examples) "\n")))))
              (mapconcat 'identity examples "\n")))))

;; (defun docs--chop-prefix (prefix s)
;;   "Remove PREFIX if it is at the start of S."
;;   (let ((pos (length prefix)))
;;     (if (and (>= (length s) (length prefix))
;;              (string= prefix (substring s 0 pos)))
;;         (substring s pos)
;;       s)))

;; (defun docs--chop-suffix (suffix s)
;;   "Remove SUFFIX if it is at end of S."
;;   (let ((pos (- (length suffix))))
;;     (if (and (>= (length s) (length suffix))
;;              (string= suffix (substring s pos)))
;;         (substring s 0 pos)
;;       s)))

(defun github-id (command-name signature)
  (->> (format "%S-%S" command-name signature)
       (s-downcase)
       (s-replace-regexp "[^a-zA-Z0-9- ]+" "")
       (s-replace " " "-")))

(defun s-replace (old new s)
  "Replace OLD with NEW in S."
  (replace-regexp-in-string (regexp-quote old) new s t t))

(defun function-summary (function)
  (if (stringp function)
      (concat "\n" function "\n")
    (let ((command-name (car function))
          (signature (cadr function)))
      (format "* [%s](#%s) `%S`" command-name (github-id command-name signature) signature))))

(defun simplify-quotes ()
  (goto-char (point-min))
  (while (search-forward "(quote nil)" nil t)
    (replace-match "'()"))
  (goto-char (point-min))
  (while (search-forward "(quote " nil t)
    (forward-char -7)
    (let ((p (point)))
      (forward-sexp 1)
      (delete-char -1)
      (goto-char p)
      (delete-char 7)
      (insert "'"))))

(defun goto-and-remove (s)
  (goto-char (point-min))
  (search-forward s)
  (delete-char (- (length s))))

(defun goto-and-replace-all (s replacement)
  (while (progn (goto-char (point-min)) (search-forward s nil t))
    (delete-char (- (length s)))
    (insert replacement)))

(defun create-cookbook ()
  (with-temp-file "./docs/cookbook.md"
    (insert "# org-ml cookbook\n\n")

    (insert
     (concat "The following are a list of common use cases and formulations"
             "for `org-ml`. If a function is not available straight from the"
             "API it may be here.\n\n"))

    (insert (s-join "\n" org-ml-dev-recipe-list))))

(defun create-api-ref ()
  (let ((org-ml-dev-examples-list (nreverse org-ml-dev-examples-list)))
    (with-temp-file "./docs/api-reference.md"
      (insert "# API Reference\n")

      (insert (mapconcat 'function-summary org-ml-dev-examples-list "\n"))

      (insert (mapconcat 'function-to-md org-ml-dev-examples-list "\n"))

      (insert (format "Version: %s" (org-ml-get-package-version)))

      (simplify-quotes))))

(defun create-docs-files ()
  (create-cookbook)
  (create-api-ref))

;; require the examples

(require 'org-ml-examples)
(require 'org-ml-cookbook)

;; tell user how many functions have no examples

(defconst org-ml-dev-defined-names nil
  "Alist of all functions/macros defined in `org-ml.el'.
The two cells in the alist are 'private' and 'public'.")

(mapatoms
 (lambda (x)
   (when (and (fboundp x) (s-starts-with-p "org-ml-" (symbol-name x)))
     (push x org-ml-dev-defined-names))))

(setq org-ml-dev-defined-names
      (--group-by
       (if (s-starts-with-p "org-ml--" (symbol-name it)) 'private 'public)
       org-ml-dev-defined-names))

(let ((public-syms (alist-get 'public org-ml-dev-defined-names))
      (example-syms (->> (-remove #'stringp org-ml-dev-examples-list)
                         (-map #'car))))
  (-some->> (-difference public-syms example-syms)
            (-map #'symbol-name)
            (--remove (s-ends-with? "*" it))
            (--remove (s-starts-with? "org-ml-update-this-" it))
            (--remove (s-starts-with? "org-ml-parse-this-" it))
            (--map (format "  %s" it))
            (s-join "\n")
            (format "The following functions don't have examples:\n%s")
            (print)))

(provide 'org-ml-docs)
;;; org-ml-docs.el ends here
