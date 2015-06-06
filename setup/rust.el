;; Settings and setup for Rust mode

(autoload 'rust-mode "rust-mode")
(setq auto-mode-alist
      (append '(("\\.rs$" . rust-mode))
              auto-mode-alist))

(add-hook 'rust-mode-hook #'flycheck-rust-setup)

; this does add-hook already
(eval-after-load "rust-mode" '(require 'racer))
