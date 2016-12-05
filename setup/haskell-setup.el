;; Haskell mode setup

;; haskell mode
;(load "haskell-site-file" nil t)

(defun my-haskell-mode-hook ()
  (load-library "inf-haskell")
  (flycheck-mode 1)
  (imenu-add-menubar-index)
  (turn-on-haskell-doc-mode)
  (turn-on-haskell-indentation)
  (inferior-haskell-process)
  (setq comment-start "--")
  ;; (add-to-list 'filladapt-token-table '("-- " haskell-comment))
  ;; (add-to-list 'filladapt-token-match-table '(haskell-comment haskell-comment))
  ;; (add-to-list 'filladapt-token-conversion-table '(haskell-comment . exact))
  (add-hook 'before-save-hook 'delete-trailing-whitespace nil t))

(add-hook 'haskell-mode-hook #'my-haskell-mode-hook)

;; highlight "return" as a builtin
(font-lock-add-keywords 'haskell-mode
 '(("\\<\\(return\\)\\>" 1 font-lock-builtin-face prepend)))
