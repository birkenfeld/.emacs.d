;; Settings and setup for Rust mode

(autoload 'rust-mode "rust-mode")
(setq auto-mode-alist
      (append '(("\\.rs$" . rust-mode))
              auto-mode-alist))

(defun compile-now ()
  (interactive)
  (compile compile-command))

(defun my-rust-mode-hook ()
  ;; Enable nice electric pairs
  (setq autopair-extra-pairs `(:code ((?< . ?>))))
  (autopair-mode 1)

  ;; Highlight whitespace mistakes
  (setq whitespace-line-column 100)
  (setq whitespace-style '(face trailing tabs lines-tail empty))
  (require 'whitespace)
  (whitespace-mode 1)

  ;; Highlight symbol at point
  (require 'highlight-symbol)
  (highlight-symbol-mode 1)

  ;; Flycheck
  (flycheck-rust-setup)
  (flycheck-mode 1)
  )

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
