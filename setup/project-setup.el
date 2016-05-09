;; Projectile setup

;; (defun projectile-grep-with-default (regexp)
;;   (interactive (let* ((symbol (symbol-at-point))
;;                       (symstr (if symbol (symbol-name symbol) "")))
;;                  (list (read-string
;;                         (format "Regexp grep%s: "
;;                                 (if symbol (format " [%s]" symstr) ""))
;;                         nil nil symstr))))
;;   (projectile-grep regexp))

(require 'subr-x)

;; (global-set-key (kbd "C-c p g") 'projectile-grep-with-default)

(define-key projectile-command-map (kbd "x") #'projectile-todo)
(define-key projectile-command-map (kbd "g") #'projectile-grep)
(define-key projectile-command-map (kbd "f") #'projectile-find-file-dwim)
(define-key projectile-command-map (kbd "s") #'projectile-save-project-buffers)
;; (define-key projectile-command-map (kbd "i") #'projectile-isearch)

;; remove a few bindings I won't need
(define-key projectile-command-map (kbd "A") nil)  ;; ag 
(define-key projectile-command-map (kbd "m") nil)  ;; commander


;; (defun projectile-test ()
;;   "Run test suite."
;;   (interactive)
;;   (unless test-case-mode (test-case-mode 1))
;;   (setq test-cwd (projectile-project-root))
;;   (setq test ".")
;;   (test-case-run))

(defadvice projectile-grep (around better-query activate)
  (let ((orig-read-string (symbol-function #'read-string)))
    (flet ((read-string (prompt &optional initial history default inherit)
                        (funcall orig-read-string
                                 (if (string-empty-p initial) prompt
                                   (concat prompt "[" initial "] "))
                                 nil nil initial inherit)))
      ad-do-it)))

(defun projectile-isearch ()
  "Multi-buffer isearch in project buffers.  BUGGY."
  (interactive)
  (let ((files (projectile-current-project-files))
        (default-directory (projectile-project-root)))
    (multi-isearch-files files)))

(defun projectile-todo ()
  "Find todo tags in project."
  (interactive)
  ;; TODO: display output in a buffer called *<project>-TODO* instead of *grep*.
  (projectile-ack "TODO|XXX|FIXME"))

(defun projectile-find-tag ()
  "Find a tag via tags table (overrides projectile version)."
  (interactive)
  (let* ((root (projectile-project-root))
         (table-file (concat root "TAGS")))
    ;; (message "%S %S" tags-file-name table-file)
    (when (not (equal tags-file-name table-file))
      (shell-command
       (format "ctags -f %s --languages=-HTML,JavaScript --python-kinds=-iv -e -R %s"
               table-file (directory-file-name root)))
      (let ((tags-add-tables nil)
            (revert-without-query '("TAGS")))
        (setq tags-completion-table nil)
        (visit-tags-table table-file)))
    (if current-prefix-arg
        (call-interactively #'find-tag)
      (tags-completion-table)
      (let ((tag-names (mapcar (lambda (x) (prin1-to-string x t)) tags-completion-table)))
        (find-tag (projectile-completing-read "Find tag: " tag-names))))))

(defun projectile-find-next-tag ()
  "Find next tag."
  (interactive)
  (find-tag last-tag t))

(defun projectile-visit-project-tags-table ()
  "Disable this function.")

(defun projectile-recompile-project (arg)
  "Compile project with last compile command."
  (interactive "P")
  (let ((compilation-read-command nil))
    (projectile-compile-project arg)))

;; Overrides projectile-go. We don't have go code, and it detects e.g. the Rust
;; compiler as go due to LLVM having some go files laying around.
;;
;; Also, the function is quite slow for large projects.
(defadvice projectile-go (around no-go activate)
  nil)

;; Add a project type for rustc
(projectile-register-project-type 'rustc '("COMPILER_TESTS.md")
                                  "make -j4 rustc-stage1" "make check-stage1 ")
