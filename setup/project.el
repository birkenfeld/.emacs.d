;; Eproject setup

(add-to-list 'load-path "~/.emacs.d/ext-lisp/eproject")

(require 'eproject)
(require 'eproject-extras)
(require 'eproject-compile)

(defun eproject-grep-with-default (regexp)
  (interactive (let* ((symbol (symbol-at-point))
                      (symstr (if symbol (symbol-name symbol) "")))
                 (list (read-string
                        (format "Regexp grep%s: "
                                (if symbol (format " [%s]" symstr) ""))
                        nil nil symstr))))
  (eproject-grep regexp))

(global-set-key (kbd "C-c p f") 'eproject-find-file)
(global-set-key (kbd "C-c p a") 'eproject-ack)
(global-set-key (kbd "C-c p g") 'eproject-grep-with-default)
(global-set-key (kbd "C-c p i") 'eproject-ibuffer)
(global-set-key (kbd "C-c p t") 'eproject-todo)
(global-set-key (kbd "C-c p d") 'eproject-revisit-project)
(global-set-key (kbd "C-c p k") 'eproject-kill-project-buffers)
(global-set-key (kbd "C-c p e") 'eproject-test)

(define-project-type python (generic)
  (look-for "setup.py")
  :relevant-files ("\\.py$" "\\.rst$" "\\.js$" "\\.html$" "\\.c$" "\\.h$" "\\.ui$")
  :irrelevant-files ("\\.py[co]$" "_build/.*$"))

(define-project-type haskell (generic)
  (or (look-for "Setup.hs") (look-for "Setup.lhs"))
  :relevant-files ("\\.hs$" "\\.lhs$" "\\.c$"))

(define-project-type elisp (generic)
  (look-for "init.el")
  :relevant-files ("\\.el$")
  :irrelevant-files ("\\.elc$"))

(defun eproject-ack (regexp)
  "Search all files in the current project for REGEXP, using Ack."
  (interactive "sRegexp ack: ")
  (require 'full-ack)
  (let* ((root (eproject-root))
         (default-directory root)
         (files (eproject-list-project-files-relative root)))
    (ack regexp t default-directory)))

(defun eproject-test ()
  "Run test suite."
  (interactive)
  (unless test-case-mode (test-case-mode 1))
  (setq test-cwd (eproject-root))
  (setq test ".")
  (test-case-run))

(defun eproject-find-tag ()
  "Find a tag via tags table."
  (interactive)
  (let* ((root (eproject-root))
         (table-file (concat root ".tags")))
    (when (not (equal tags-file-name table-file))
      (shell-command
       (format "ctags -f %s --languages=-HTML,JavaScript --python-kinds=-iv -e -R %s"
               table-file (directory-file-name root)))
      (let ((revert-without-query '(".tags")))
        (visit-tags-table table-file)))
    (if current-prefix-arg
        (call-interactively #'find-tag)
      (tags-completion-table)
      (let (tag-names)
        (mapatoms (lambda (x)
                    (push (prin1-to-string x t) tag-names))
                  tags-completion-table)
        (find-tag (ido-completing-read "Find tag: " tag-names))))))

(defun eproject-find-next-tag ()
  "Find next tag."
  (interactive)
  (find-tag last-tag t))
