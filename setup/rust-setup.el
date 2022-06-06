;; Settings and setup for Rust mode

(setq auto-mode-alist
      (append '(("\\.rs$" . rust-mode))
              auto-mode-alist))

(defun my-autopair-rust-action (action pair pos-before)
  (unless (and (eq action 'opening)
               (eq pair ?>)
               (save-excursion (backward-char 2)
                               (or (looking-at " <") (looking-at "<<"))))
    (autopair-default-handle-action action pair pos-before)))

(defun my-rust-mode-hook ()
  ;; Make compilation colored
  ;(setq-local compilation-environment (cons "CARGO_TERM_COLOR=always" compilation-environment))

  ;; Enable nice electric pairs
  (setq autopair-extra-pairs `(:code ((?< . ?>))))
  (setq autopair-handle-action-fns (list #'my-autopair-rust-action))
  (autopair-mode 1)

  ;; Highlight whitespace mistakes
  (setq whitespace-line-column 100)
  (setq whitespace-style '(face trailing tabs lines-tail empty))
  (require 'whitespace)
  (whitespace-mode 1)

  ;; Default compile command
  (setq-local compile-command "cargo build ")

  ;; Language server setup
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
