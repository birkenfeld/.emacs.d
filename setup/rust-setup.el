;; Settings and setup for Rust mode

(setq auto-mode-alist
      (append '(("\\.rs$" . rust-mode))
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

  ;; Highlight symbol at point
  (require 'highlight-symbol)
  (highlight-symbol-mode 1)

  ;; Racer
  (company-mode 1)
  (racer-mode 1)
  (eldoc-mode 1)

  ;; Flycheck (will interfere with building, unfortunately)
  ;(flycheck-rust-setup)
  ;(flycheck-mode 1)
  )

(eval-after-load "rust-mode"
  '(progn

     ;; Racer setup: this does add-hook already
     (require 'racer)
     (add-hook 'rust-mode-hook #'my-rust-mode-hook)
     (diminish 'racer-mode " R")

     (define-key racer-mode-map (kbd "TAB") #'company-indent-or-complete-common)
     (define-key racer-mode-map (kbd ".") #'company-insert-and-complete)
     (define-key racer-mode-map (kbd ":") #'company-insert-and-complete)
     (define-key rust-mode-map (kbd "C-c C-d") #'racer-describe)

     ;; some bindings
     (define-key rust-mode-map (kbd "C-c C-c") #'compile-now)
     (define-key rust-mode-map (kbd "RET") #'maybe-comment-indent-new-line)

     ;; show Racer doc window in a bottom popup
     (add-to-list 'display-buffer-alist '("*Racer Help" popwin:special-display-popup-window))
     ))
