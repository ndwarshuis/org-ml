;;; examples-to-docs.el --- Extract om.el's doc from examples.el

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

;; (require 'om)
(require 's)
(require 'lispy)
(require 'om-elem)
(require 'dash)
(require 'help-fns)
(require 'package)

(setq text-quoting-style 'grave)

(defvar functions '())

(defun om-get-package-version ()
  "Get version of om package."
  (with-current-buffer (find-file-noselect "om.el")
    (mapconcat 'number-to-string (package-desc-version (package-buffer-info)) version-separator)))

(defun format-actual (actual)
  (with-temp-buffer
    (insert (format "%S" actual))
    (goto-char (point-min))
    (lispy-multiline)
    (buffer-string)))

(defun format-expected (sym expected)
  (let* ((s (s-lines (format "%S" expected)))
         (header (format " ;; %S %s" sym (car s)))
         (rest (--map (s-prepend " ;;    " it) (-drop 1 s))))
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
  ;; remove extra signature for cl-defun functions
  ;; TODO this is hacky but it works
  (let ((doc (documentation cmd)))
    (unless doc (error "No docstring set for %s" cmd))
    (if (not (s-matches? "(fn .*)" doc)) doc
      (->> (s-lines doc) (-drop-last 2) (s-join "\n")))))

(defmacro defexamples (cmd &rest examples)
  `(add-to-list 'functions
                (list
                 ',cmd
                 (docs--signature ',cmd)
                 (format-doc ',cmd)
                 (-map 'example-to-string (-partition 3 ',examples)))))

(defmacro defexamples-content (cmd docstring &rest args)
  `(cl-flet
       ((formatted-string?
         (list)
         (memq (and (listp list) (car list)) '(:content :comment)))
        (filter-hidden
         (args)
         (->> (--split-when (eq it :end-hidden) args)
              (--mapcat (--take-while (not (eq it :begin-hidden)) it))))
        (format-content
         (list)
         (->> (car list)
              (-drop 1)
              (--map (format "; %s" it))
              (s-join "\n")
              (format ";; Given the following contents:\n%s\n")))
        (format-comment
         (list)
         (->> (car list) (-drop 1) (s-join " ") (format ";; %s"))))
     (let* ((doc (or ,docstring (format-doc ',cmd)))
            (example
             (->> (filter-hidden ',args)
                  (-partition-by #'formatted-string?)
                  (--map (cond
                          ((eq :comment (car (car it)))
                           (format-comment it))
                          ((eq :content (car (car it)))
                           (format-content it))
                          (t (-some->>
                              (-partition 3 it)
                              (-map #'example-to-string)
                              (s-join "\n"))))))))
       (add-to-list 'functions (list ',cmd
                                     (docs--signature ',cmd)
                                     doc
                                     example)))))

(defmacro def-example-subgroup (group desc &rest examples)
  `(progn
     ;; (add-to-list 'functions ,(concat "### " group))
     (setq functions (cons ,(concat "### " group) functions))
     (when ,desc
       ;; (add-to-list 'functions ,desc))
       (setq functions (cons ,desc functions)))
     ,@examples))

(defmacro def-example-group (group desc &rest examples)
  `(progn
     ;; (add-to-list 'functions ,(concat "## " group))
     (setq functions (cons ,(concat "## " group) functions))
     (when ,desc
       ;; (add-to-list 'functions ,desc))
       (setq functions (cons ,desc functions)))
     ,@examples))

(defun quote-and-downcase (string)
  (format "`%s`" (downcase string)))

(defun unquote-and-link (string)
  (format-link (substring string 1 -1)))

(defun format-link (string-name)
  (-let* ((name (intern string-name))
          ((_ signature _ _) (assoc name functions)))
    (if signature
        (format "[`%s`](#%s)" name (github-id name signature))
      (format "`%s`" name))))

(defun format-docstring (docstring)
  (let (case-fold-search)
    (--> docstring
      (replace-regexp-in-string "\\b\\([A-Z][A-Z-]*[0-9]*\\)\\b" 'quote-and-downcase it t)
      (replace-regexp-in-string "`\\([^ ]+\\)'" 'unquote-and-link it t)
      (replace-regexp-in-string "^  " "    " it))))

(defun function-to-md (function)
  (if (stringp function)
      (concat "\n" (s-replace "### " "### " function) "\n")
    (-let [(command-name signature docstring examples) function]
      (unless docstring
        (error "No docstring supplied for %s" command-name))
      (format "#### %s `%s`\n\n%s\n\n```el\n%s\n```\n"
      ;; (format "### %s `%s`\n\n%s\n\n%s"
              command-name
              signature
              (format-docstring docstring)
              ;; (mapconcat 'identity (-take 3 examples) "\n")))))
              (mapconcat 'identity examples "\n")))))

(defun docs--chop-prefix (prefix s)
  "Remove PREFIX if it is at the start of S."
  (let ((pos (length prefix)))
    (if (and (>= (length s) (length prefix))
             (string= prefix (substring s 0 pos)))
        (substring s pos)
      s)))

(defun docs--chop-suffix (suffix s)
  "Remove SUFFIX if it is at end of S."
  (let ((pos (- (length suffix))))
    (if (and (>= (length s) (length suffix))
             (string= suffix (substring s pos)))
        (substring s 0 pos)
      s)))

(defun github-id (command-name signature)
  (docs--chop-suffix
   "-"
   (replace-regexp-in-string "[^a-zA-Z0-9-]+" "-" (docs--chop-prefix
                                                   "!"
                                                   (format "%S %S" command-name signature)))))

(defun s-replace (old new s)
  "Replace OLD with NEW in S."
  (replace-regexp-in-string (regexp-quote old) new s t t))

(defun function-summary (function)
  (if (stringp function)
      (concat "\n" function "\n")
    (let ((command-name (car function))
          (signature (cadr function)))
      (format "* [%s](#%s) `%s`" command-name (github-id command-name signature) signature))))

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

(defun create-docs-file ()
  (let ((functions (nreverse functions)))
    (with-temp-file "./README.md"
      (insert-file-contents-literally "./readme-template.md")

      (goto-and-remove "[[ function-list ]]")
      (insert (mapconcat 'function-summary functions "\n"))

      (goto-and-remove "[[ function-docs ]]")
      (insert (mapconcat 'function-to-md functions "\n"))

      (goto-and-replace-all "[[ version ]]" (om-get-package-version))

      (simplify-quotes))))

;;; examples-to-docs.el ends here
