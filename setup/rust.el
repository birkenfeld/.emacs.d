;; Settings and setup for Rust mode

(add-hook 'rust-mode-hook #'flycheck-rust-setup)

; this does add-hook already
(eval-after-load "rust-mode" '(require 'racer))
