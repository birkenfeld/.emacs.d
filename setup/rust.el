;; Settings and setup for Rust mode

(autoload 'rust-mode "rust-mode")
(setq auto-mode-alist
      (append '(("\\.rs$" . rust-mode))
              auto-mode-alist))

(add-hook 'rust-mode-hook #'flycheck-rust-setup)

(defun compile-now ()
  (interactive)
  (compile compile-command))

(defun my-rust-mode-hook ()
  (push '("::" . ?∷) prettify-symbols-alist)
  (push '("!=" . ?≠) prettify-symbols-alist)
  (push '("<=" . ?≤) prettify-symbols-alist)
  (push '(">=" . ?≥) prettify-symbols-alist)
  (push '("&&" . ?∧) prettify-symbols-alist)
  (push '("||" . ?∨) prettify-symbols-alist)
  (prettify-symbols-mode 1))

(eval-after-load "rust-mode"
  '(progn

     ;; Racer setup: this does add-hook already
     (require 'racer)
     (add-hook 'rust-mode-hook #'racer-activate)
     (add-hook 'rust-mode-hook #'my-rust-mode-hook)

     (define-key rust-mode-map (kbd "TAB") 'racer-complete-or-indent)
     (define-key rust-mode-map (kbd "M-.") 'racer-find-definition)

     ;; some bindings
     (define-key rust-mode-map (kbd "C-c C-c") 'compile-now)
     ))
