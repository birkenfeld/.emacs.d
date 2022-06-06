;; Settings and setup for Python mode

;; Ignore Python 3.2+ .pyc directories
(add-to-list 'completion-ignored-extensions "__pycache__/")

(require 'popwin)
(add-to-list 'display-buffer-alist '("*Python Doc*" popwin:special-display-popup-window))

(eval-after-load 'python
  '(define-key python-mode-map (kbd ".") #'company-insert-and-complete))

(put 'font-lock-regexp-grouping-backslash 'face-alias 'font-lock-builtin-face)

;; Related features customization

(defadvice eldoc-message (around no-in-xxx (&rest args) activate)
  "Do not show the \"In: ...\" in echo area."
  (if (and (car args)
           (string-match-p "^In: " (car args))) nil
    ad-do-it))

;; Electric colon: only indent if on a dedenter statement
(defadvice python-indent-post-self-insert-function (around fix-colon activate)
  (unless (and (eq ?: last-command-event)
               (not (python-info-dedenter-statement-p)))
    ad-do-it))

(defun my-python-mode-hook ()
  ;; Remove python-mode's ffap things that slow down find-file
  (setq ffap-alist (remove '(python-mode . python-ffap-module-path) ffap-alist))
  (setq ffap-alist (remove '(inferior-python-mode . python-ffap-module-path) ffap-alist))

  ;; Enable nice electric pairs like in textmate
  (autopair-mode 1)
  (setq autopair-handle-action-fns
        (list #'autopair-default-handle-action
              #'autopair-python-triple-quote-action))

  ;; Highlight whitespace mistakes
  (setq whitespace-style '(face trailing tabs lines-tail empty))
  (require 'whitespace)
  (whitespace-mode 1)

  ;; Highlight symbol at point
  (require 'highlight-symbol)
  (highlight-symbol-mode 1)

  ;; Highlight escape sequences
  (require 'highlight-escape-sequences)
  (setq hes-simple-modes '(python-mode js-mode js2-mode))
  (hes-mode)

  ;; Death to trailing whitespace!
  (set-variable 'show-trailing-whitespace 1)

  ;; Add some local hooks
  ;(add-hook 'before-save-hook 'delete-trailing-whitespace nil t)

  ;; Compile (<f5>) is execute
  (unless (or (file-exists-p "makefile")
              (file-exists-p "Makefile"))
    (set (make-local-variable 'compile-command)
         (concat "python " buffer-file-name)))

  (setq fill-column 79)
  (define-key python-mode-map (kbd "M-q") #'python-fill-paragraph)
  (define-key python-mode-map (kbd "C-c #") #'comment-move-before-line)

  (add-hook 'after-save-hook #'executable-make-buffer-file-executable-if-script-p t t)

  ;; No auto-fill please
  (auto-fill-mode 0)
  (fci-mode 1)

  (lsp)
  )

(add-hook 'python-mode-hook #'my-python-mode-hook)

;; Cython mode
(setq auto-mode-alist
      (append '(("\\.pyx$" . cython-mode)
                ("\\.pxs$" . cython-mode)
                ("\\.pxi$" . cython-mode))
              auto-mode-alist))


;; ReST mode
(autoload 'rst-mode "rst" nil t)
(setq auto-mode-alist
      (append '(("\\.rst$" . rst-mode)
                ("\\.rest$" . rst-mode)
                ("CHANGES$" . rst-mode)
                ("NEWS$" . rst-mode))
              auto-mode-alist))
(eval-after-load 'rst
  '(add-hook 'rst-mode-hook
             (lambda () (set-variable 'show-trailing-whitespace 1))))
(add-hook 'rst-mode-hook 'flycheck-mode)
