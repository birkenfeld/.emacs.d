;; Projectile setup

;; (defun projectile-grep-with-default (regexp)
;;   (interactive (let* ((symbol (symbol-at-point))
;;                       (symstr (if symbol (symbol-name symbol) "")))
;;                  (list (read-string
;;                         (format "Regexp grep%s: "
;;                                 (if symbol (format " [%s]" symstr) ""))
;;                         nil nil symstr))))
;;   (projectile-grep regexp))

;; (global-set-key (kbd "C-c p g") 'projectile-grep-with-default)

(define-key projectile-command-map (kbd "x") 'projectile-todo)

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
    (message "%S %S" tags-file-name table-file)
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
      (let (tag-names)
        (mapatoms (lambda (x)
                    (push (prin1-to-string x t) tag-names))
                  tags-completion-table)
        (find-tag (projectile-completing-read "Find tag: " tag-names))))))

(defun projectile-find-next-tag ()
  "Find next tag."
  (interactive)
  (find-tag last-tag t))

(defun projectile-visit-project-tags-table ()
  "Disable this function.")
