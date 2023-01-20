;; Settings and setup for Rust mode

(setq auto-mode-alist
      (append '(("\\.rs$" . rust-mode))
              auto-mode-alist))

(defun my-rust-mode-hook ()
  ;; Enable nice electric pairs
  (electric-pair-mode 1)

  ;; Highlight whitespace mistakes
  (setq-local whitespace-line-column 100)
  (whitespace-mode 1)

  ;; Highlight escapes
  (hes-mode 1)

  ;; Default compile command
  (setq-local compile-command "cargo build ")

  (copilot-mode 1)
  (lsp)
)

(eval-after-load "rust-mode"
  '(progn
     (add-hook 'rust-mode-hook #'my-rust-mode-hook)

     (define-key rust-mode-map (kbd "TAB") #'company-indent-or-complete-common)
     (define-key rust-mode-map (kbd ".") #'company-insert-and-complete)
     (define-key rust-mode-map (kbd ":") #'company-insert-and-complete-colon)
     (define-key rust-mode-map (kbd "C-c C-d") #'lsp-describe-thing-at-point)

     ;; some bindings
     (define-key rust-mode-map (kbd "RET") #'maybe-comment-indent-new-line)

     ;; show doc window in a bottom popup
     (add-to-list 'display-buffer-alist '("*lsp-help" popwin:special-display-popup-window))
     ))

;; highlight-escape-sequences mode
(defconst hes-rust-escape-sequence-re
  (rx (submatch
       (and ?\\ (submatch
                 (or (repeat 1 3 (in "0-7"))
                     (and ?x (repeat 2 xdigit))
                     (and ?u ?{ (repeat 4 xdigit) ?})
                     ;; (any "\"\'\\nrt0")
                     not-newline))))) ;; deprecated
  "Regexp to match Rust escape sequences.")

(eval-after-load 'highlight-escape-sequences
  '(add-to-list 'hes-mode-alist `(rust-mode . ,hes-rust-escape-sequence-re)))
