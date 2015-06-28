;; Settings and setup for Rust mode

(autoload 'rust-mode "rust-mode")
(setq auto-mode-alist
      (append '(("\\.rs$" . rust-mode))
              auto-mode-alist))

(add-hook 'rust-mode-hook #'flycheck-rust-setup)

(defun compile-now ()
  (interactive)
  (compile compile-command))

(eval-after-load "rust-mode"
  '(progn

     ;; Racer setup: this does add-hook already
     (require 'racer)
     (add-hook 'rust-mode-hook 'racer-activate)

     (define-key rust-mode-map (kbd "TAB") 'racer-complete-or-indent)
     (define-key rust-mode-map (kbd "M-.") 'racer-find-definition)

     ;; some bindings
     (define-key rust-mode-map (kbd "C-c C-c") 'compile-now)
     ))
