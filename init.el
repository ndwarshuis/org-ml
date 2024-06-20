(defun fix-null-term (s)
  "Fix string S with extra wonky null terminators.

For whatever reason this affects certain strings in the conda
package for Emacs. These look like `blabla\0\0\0\0\0\0\0`."
  (declare (pure t) (side-effect-free t))
  (save-match-data
    (if (string-match "\0+" s)
        (replace-match "" t t s)
      s)))

;; HACK stuff won't install unless this string is fixed
(if (< (round (string-to-number emacs-version)) 29)
    (setq Info-default-directory-list
          (cons
           (fix-null-term (car Info-default-directory-list))
           (cdr Info-default-directory-list)))
  (setq configure-info-directory (fix-null-term configure-info-directory)))

(setq package-enable-at-startup nil)

(setq user-emacs-directory
      (file-name-concat (expand-file-name ".emacs") emacs-version))

(defvar bootstrap-version)

(let ((bootstrap-file
       (file-name-concat
        user-emacs-directory
        "straight/repos/straight.el/bootstrap.el"))
      (bootstrap-version 7))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
        (url-retrieve-synchronously
         "https://raw.githubusercontent.com/radian-software/straight.el/develop/install.el"
         'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))

(straight-use-package 's)
(straight-use-package 'dash)
(straight-use-package 'buttercup)
(straight-use-package 'lispy)

;; (straight-use-package 'org)

;; (straight-freeze-versions)
;; (straight-thaw-versions)

(defun compile-target ()
  "Compile org-ml."
  (byte-compile-file "org-ml-macs.el")
  (byte-compile-file "org-ml.el"))
