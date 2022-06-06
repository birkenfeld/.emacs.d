;; Setup for misc modes

;; SGML ------------------------------------------------------------------------

;; close HTML tags with C-t in sgml mode
(eval-after-load "sgml-mode"
  '(define-key sgml-mode-map (kbd "C-t") #'sgml-close-tag))

;; Elisp -----------------------------------------------------------------------

;; Enable eldoc mode
(add-hook 'emacs-lisp-mode-hook 'turn-on-eldoc-mode)

;; Some nice keybindings
(define-key emacs-lisp-mode-map (kbd "M-.") #'find-function-at-point)
(define-key lisp-mode-shared-map (kbd "C-c v") #'eval-buffer)

;; auto-pair `' in elisp comments and docstrings
(defun my-emacs-lisp-mode-hook ()
  (autopair-mode)
  (push '(?` . ?')
        (getf autopair-extra-pairs :comment))
  (push '(?` . ?')
        (getf autopair-extra-pairs :string)))
(add-hook 'emacs-lisp-mode-hook #'my-emacs-lisp-mode-hook)

;; Remove the .elc file when saving an .el file
(defun remove-elc-on-save ()
  "If you're saving an elisp file, likely the .elc is no longer valid."
  (make-local-variable 'after-save-hook)
  (add-hook 'after-save-hook
            (lambda ()
              (if (file-exists-p (concat buffer-file-name "c"))
                  (delete-file (concat buffer-file-name "c"))))))

(add-hook 'emacs-lisp-mode-hook 'remove-elc-on-save)

;; McStas ----------------------------------------------------------------------

(autoload 'mcstas-mode "mcstas-mode" nil t)
(add-to-list 'auto-mode-alist '("\\.instr$" . mcstas-mode))
(add-to-list 'auto-mode-alist '("\\.comp$" . mcstas-mode))

;; Dired: better navigation of Ctrl-Home/End -----------------------------------

(defun dired-back-to-top ()
  (interactive)
  (beginning-of-buffer)
  (dired-next-line 4))

(defun dired-jump-to-bottom ()
  (interactive)
  (end-of-buffer)
  (dired-next-line -1))

(define-key dired-mode-map [remap beginning-of-buffer] #'dired-back-to-top)
(define-key dired-mode-map [remap end-of-buffer] #'dired-jump-to-bottom)

(define-key dired-mode-map "e" #'wdired-change-to-wdired-mode)

(eval-after-load "wdired"
  '(progn
     (define-key wdired-mode-map (vector 'remap 'beginning-of-buffer) #'dired-back-to-top)
     (define-key wdired-mode-map (vector 'remap 'end-of-buffer) #'dired-jump-to-bottom)))

;; Shell/terminal --------------------------------------------------------------

;; let the shell buffer change the default directory
(defadvice shell-pop-up (before change-to-default-directory activate)
  (let ((dir default-directory)
        (buf (get-buffer shell-pop-internal-mode-buffer)))
    (when buf
      (with-current-buffer buf
        (cond ((eq major-mode 'term-mode)
               (term-send-raw-string (concat "cd " dir "\n")))
              ((eq major-mode 'shell-mode)
               (insert (concat "cd " dir))
               (comint-send-input)))))))

;; Let eterm show colors
(setq term-term-name "eterm-color")

;; This helps with a bug in ansi-term when output lines are longer than
;; the terminal width
(defun turn-off-truncate-lines ()
  (setq truncate-lines nil
        word-wrap t))
(add-hook 'term-mode-hook 'turn-off-truncate-lines)

;; PO mode ---------------------------------------------------------------------

;; Fixes and enhancements for po-mode
(eval-after-load 'po-mode '(require 'gb-po-mode))

;; Web stuff -------------------------------------------------------------------

;; web-mode for HTML/Jinja/CSS/JS
(add-to-list 'auto-mode-alist '("\\.jsp\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.php\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.as[cp]x\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.erb\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.html?\\'" . web-mode))

;; rainbow-mode: color for color names
(add-hook 'css-mode-hook 'rainbow-mode)

;; xdict: lookup dictionary
(autoload 'xdict-query "x-dict" nil t)
(global-set-key (kbd "C-c d") #'xdict-query)
(eval-after-load 'x-dict
  '(add-hook 'xdict-mode-hook (lambda () (setq truncate-lines t))))

;; Markdown --------------------------------------------------------------------

(eval-after-load "markdown-mode"
  '(progn
     (setq markdown-imenu-generic-expression
           '(("title"  "^\\(.*\\)[\n]=+$" 1)
             ("h2-"    "^\\(.*\\)[\n]-+$" 1)
             ("h1"   "^# \\(.*\\)$" 1)
             ("h2"   "^## \\(.*\\)$" 1)
             ("h3"   "^### \\(.*\\)$" 1)
             ("h4"   "^#### \\(.*\\)$" 1)
             ("h5"   "^##### \\(.*\\)$" 1)
             ("h6"   "^###### \\(.*\\)$" 1)
             ("fn"   "^\\[\\^\\(.*\\)\\]" 1)))

     (add-hook 'markdown-mode-hook
               (lambda ()
                 (define-key markdown-mode-map (kbd "<tab>") #'yas-expand)
                 (setq imenu-generic-expression
                       markdown-imenu-generic-expression)))

     (define-key markdown-mode-map (kbd "<M-down>") nil)
     (define-key markdown-mode-map (kbd "<M-up>") nil)
     (define-key markdown-mode-map (kbd "<M-right>") nil)
     (define-key markdown-mode-map (kbd "<M-left>") nil)
     ))

(add-to-list 'auto-mode-alist '("\\.md$" . markdown-mode))
(add-to-list 'auto-mode-alist '("\\.markdown$" . markdown-mode))

;; Treemacs --------------------------------------------------------------------

(eval-after-load "treemacs"
  '(progn
     ;; Open files and expand dirs with single click
     (define-key treemacs-mode-map [mouse-1] #'treemacs-single-click-expand-action)
     ))
