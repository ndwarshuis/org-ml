;;; test-helper.el --- Helpers for om-test.el

;;; test-helper.el ends here

(require 'f)

(defconst test-root (f-dirname (f-this-file)))
(defconst code-root (f-parent test-root))

(require 'om (f-expand "om.el" code-root))

(defmacro om-test-with-sandbox (&rest body)
  "Run BODY in a sandboxed Org directory."
  `(let ((org-directory (f-join test-root "Org")))
     ,@body))

(defmacro om-test-with-file (path &rest body)
  (let ((p (f-join test-root "Org" path)))
    `(with-current-buffer (find-file-noselect ,p t)
       ,@body)))

(defun om-test-parse-all-headlines ()
  "Return a list of all toplevel headlines in the current buffer."
  (org-element-map (org-element-parse-buffer)
      'headline #'identity nil nil 'headline))

(defun file-extract-headlines (path)
  "Open file at PATH and extract all headlines."
  (unless (equal "org" (f-ext path)) (error "Not and org file"))
  (with-current-buffer (find-file-noselect path t)
    (org-element-map (org-element-parse-buffer)
        'headline #'identity nil nil 'headline)))

(defmacro with-org-file-headlines (path &rest body)
  "Open org file at PATH and work on headlines HS."
  `(with-sandbox
    (let ((hs (->> (f-join org-directory ,path)
                   (file-extract-headlines))))
      ,@body)))

(defmacro with-temp-env (vars &rest body)
  "Execute BODY in a temporary environment set by VARS.
VARS is an alist of var:val pairs as strings."
  `(let ((orig-vars (--map (cons (car it) (getenv (car it))) ',vars)))
     (--each ',vars (setenv (car it) (cdr it)))
     ,@body
     (--each orig-vars (setenv (car it) (cdr it)))))

(defun test-org-function (org-file)
  (with-sandbox
   (cl-flet
       ((get-src-block
         (elem)
         (--> (org-element-contents elem)
              (org-element-map it 'src-block #'identity)
              (car it)
              (org-element-property :value it)
              (read it))))
     (with-current-buffer (find-file-noselect
                           (f-join org-directory org-file) t)
       (let* ((tree (org-element-parse-buffer))
              (hs (org-element-map tree 'headline #'identity))
              (fun (get-src-block tree)))
         (--each hs (should
                     (equal
                      (save-excursion
                        (goto-char (org-element-property :begin it))
                        (eval fun))
                      (eval (get-src-block it))))))))))
