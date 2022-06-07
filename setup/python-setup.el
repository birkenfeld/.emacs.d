;; Settings and setup for Python mode

;; Ignore Python 3.2+ .pyc directories
(add-to-list 'completion-ignored-extensions "__pycache__/")

(require 'popwin)
(add-to-list 'display-buffer-alist '("*Python Doc*" popwin:special-display-popup-window))

(eval-after-load 'python
  '(progn
     (define-key python-mode-map (kbd ".") #'company-insert-and-complete)
     (define-key python-mode-map (kbd "M-q") #'python-fill-paragraph)
     (define-key python-mode-map (kbd "C-c #") #'comment-move-before-line)
))

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

  ;; Enable nice electric pairs
  (electric-pair-mode 1)

  ;; Highlight whitespace mistakes
  (whitespace-mode 1)

  ;; Show fill column
  (setq fill-column 79)
  (fci-mode 1)

  ;; Highlight symbol at point
  (highlight-symbol-mode 1)

  ;; Highlight escape sequences
  (hes-mode 1)

  ;; Add some local hooks
  ;(add-hook 'before-save-hook 'delete-trailing-whitespace nil t)

  ;; Compile (<f5>) is execute
  (unless (or (file-exists-p "makefile")
              (file-exists-p "Makefile"))
    (setq-local compile-command (concat "python " buffer-file-name)))

  (add-hook 'after-save-hook #'executable-make-buffer-file-executable-if-script-p t t)

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

;; highlight-escape-sequences mode
(defconst hes-python-escape-sequence-re
  (rx (submatch
       (and ?\\ (submatch
                 (or (repeat 1 3 (in "0-7"))
                     (and ?x (repeat 2 xdigit))
                     (and ?u (repeat 4 xdigit))
                     (and ?U (repeat 8 xdigit))
                     ;; (any "\"\'\\abfnrtv")
                     not-newline))))) ;; deprecated
  "Regexp to match Python escape sequences.")

(eval-after-load 'highlight-escape-sequences
  '(add-to-list 'hes-mode-alist `(python-mode . ,hes-python-escape-sequence-re)))
