;; Haskell mode setup

;; Support flymake in Haskell mode

(defun flymake-Haskell-init ()
  (flymake-simple-make-init-impl
   'flymake-create-temp-with-folder-structure nil nil
   (file-name-nondirectory buffer-file-name)
   'flymake-get-Haskell-cmdline))

(defun flymake-get-Haskell-cmdline (source base-dir)
  (list "flycheck_haskell.pl"
        (list source base-dir)))

(require 'flymake)
(push '(".+\\.hs$" flymake-Haskell-init flymake-simple-java-cleanup)
      flymake-allowed-file-name-masks)
(push '(".+\\.lhs$" flymake-Haskell-init flymake-simple-java-cleanup)
      flymake-allowed-file-name-masks)
(push
 '("^\\(\.+\.hs\\|\.lhs\\):\\([0-9]+\\):\\([0-9]+\\):\\(.+\\)"
   1 2 3 4) flymake-err-line-patterns)

;; haskell mode
;(load "haskell-site-file" nil t)

(defun my-haskell-mode-hook ()
  (load-library "inf-haskell")
  (imenu-add-menubar-index)
  (turn-on-haskell-doc-mode)
  (turn-on-haskell-indentation)
  (inferior-haskell-process)
  (setq comment-start "--")
  (add-to-list 'filladapt-token-table '("-- " haskell-comment))
  (add-to-list 'filladapt-token-match-table '(haskell-comment haskell-comment))
  (add-to-list 'filladapt-token-conversion-table '(haskell-comment . exact))
  (add-hook 'before-save-hook 'delete-trailing-whitespace nil t))

(add-hook 'haskell-mode-hook #'my-haskell-mode-hook)

;; highlight "return" as a builtin
(font-lock-add-keywords 'haskell-mode
 '(("\\<\\(return\\)\\>" 1 font-lock-builtin-face prepend)))
