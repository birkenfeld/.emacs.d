;; Settings and setup for Rust mode

(setq auto-mode-alist
      (append '(("\\.rs$" . rustic-mode))
              auto-mode-alist))

(defun compile-now ()
  (interactive)
  (compile compile-command))

(defun my-autopair-rust-action (action pair pos-before)
  (unless (and (eq action 'opening)
               (eq pair ?>)
               (save-excursion (backward-char 2)
                               (or (looking-at " <") (looking-at "<<"))))
    (autopair-default-handle-action action pair pos-before)))

(defun company-insert-and-complete-colon ()
  ;; Insert and complete, but only on double colon.
  (interactive)
  (if (looking-back ":")
      (company-insert-and-complete)
    (self-insert-command 1)))

(defun my-rust-mode-hook ()
  ;; Enable nice electric pairs
  (setq autopair-extra-pairs `(:code ((?< . ?>))))
  (setq autopair-handle-action-fns (list #'my-autopair-rust-action))
  (autopair-mode 1)

  ;; Highlight whitespace mistakes
  (setq whitespace-line-column 100)
  (setq whitespace-style '(face trailing tabs lines-tail empty))
  (require 'whitespace)
  (whitespace-mode 1)

  ;; Language server setup
  (yas-minor-mode 1)
  (lsp)
  )

(eval-after-load "rustic"
  '(progn
     (add-hook 'rustic-mode-hook #'my-rust-mode-hook)

     (define-key rustic-mode-map (kbd "TAB") #'company-indent-or-complete-common)
     (define-key rustic-mode-map (kbd ".") #'company-insert-and-complete)
     (define-key rustic-mode-map (kbd ":") #'company-insert-and-complete-colon)
     (define-key rustic-mode-map (kbd "C-c C-d") #'lsp-describe-thing-at-point)

     ;; some bindings
     (define-key rustic-mode-map (kbd "C-c C-c") #'compile-now)
     (define-key rustic-mode-map (kbd "RET") #'maybe-comment-indent-new-line)

     ;; show doc window in a bottom popup
     (add-to-list 'display-buffer-alist '("*lsp-help" popwin:special-display-popup-window))
     ))
