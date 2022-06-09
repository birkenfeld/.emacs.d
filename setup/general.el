;; General setup of sane defaults

;; Make "yes or no" "y or n"
(fset 'yes-or-no-p 'y-or-n-p)

;; Window frame title
(setq frame-title-format "emacs [%b %*%+ %f]")
(setq icon-title-format "emacs [%b]")

;; No bells please
(setq ring-bell-function 'ignore)

;; Prefer UTF-8 coding system
(prefer-coding-system 'utf-8)

;; Put game scores in a different directory
(setq gamegrid-user-score-file-directory
      (locate-user-emacs-file "saved/games/"))

;; Use a nicer mouse scroll behavior
(setq mouse-wheel-scroll-amount '(1 ((shift) . 1) ((control))))

;; Enable otherwise disabled commands
(put 'set-goal-column  'disabled nil)
(put 'downcase-region  'disabled nil)
(put 'upcase-region    'disabled nil)
(put 'narrow-to-region 'disabled nil)
(put 'narrow-to-page   'disabled nil)
(put 'scroll-left      'disabled nil)

;; Update copyright headers before saving
(add-hook 'before-save-hook 'copyright-update)
;; But don't try to update GPL versions
(setq copyright-current-gpl-version nil)

;; Useful hippie-expand functions
(setq hippie-expand-try-functions-list
      '(try-complete-file-name-partially
        try-complete-file-name
        try-expand-all-abbrevs
        try-expand-dabbrev
        try-expand-dabbrev-all-buffers
        try-expand-dabbrev-from-kill
        try-complete-lisp-symbol-partially
        try-complete-lisp-symbol))

;; Auto-fill in text mode
(add-hook 'text-mode-hook 'auto-fill-mode)

;; Nice config file modes
(require 'generic-x)

;; Highlight XXX style code tags in source files
(font-lock-add-keywords 'python-mode
 '(("\\<\\(FIXME\\|HACK\\|XXX\\|TODO\\):?" 1 font-lock-warning-face prepend)))
(font-lock-add-keywords 'haskell-mode
 '(("\\<\\(FIXME\\|HACK\\|XXX\\|TODO\\):?" 1 font-lock-warning-face prepend)))
(font-lock-add-keywords 'c-mode
 '(("\\<\\(FIXME\\|HACK\\|XXX\\|TODO\\):?" 1 font-lock-warning-face prepend)))
(font-lock-add-keywords 'c++-mode
 '(("\\<\\(FIXME\\|HACK\\|XXX\\|TODO\\):?" 1 font-lock-warning-face prepend)))
(font-lock-add-keywords 'latex-mode
 '(("\\<\\(FIXME\\|HACK\\|XXX\\|TODO\\):?" 1 font-lock-warning-face prepend)))
(font-lock-add-keywords 'rust-mode
 '(("\\<\\(FIXME\\|HACK\\|XXX\\|TODO\\):?" 1 font-lock-warning-face prepend)))

;; Nice xterm mouse handling (fails with dragging)
;(xterm-mouse-mode t)

;; Sphinx templated files: find mode after removing _t suffix
(add-to-list 'auto-mode-alist '("_t$" nil t))

;; Kill minibuffer when changing buffer by mouseclick
(defun stop-using-minibuffer ()
  "Abort recursive edit in the minibuffer."
  (when (and (>= (recursion-depth) 1)
             (active-minibuffer-window))
    (abort-recursive-edit)))
(add-hook 'mouse-leave-buffer-hook 'stop-using-minibuffer)

;; Make some mode line displays smaller
(require 'diminish)
(diminish 'abbrev-mode)
(diminish 'eldoc-mode)
(eval-after-load "volatile-highlights" '(diminish 'volatile-highlights-mode))
(eval-after-load "highlight-symbol" '(diminish 'highlight-symbol-mode))
(eval-after-load "which-key" '(diminish 'which-key-mode))
(eval-after-load "company" '(diminish 'company-mode))
(eval-after-load "lsp-mode" '(diminish 'lsp-mode " LSP"))
(eval-after-load "yasnippet" '(diminish 'yas-minor-mode " Y"))

;; M-x enhancement
(require 'smex)
(smex-initialize)
(global-set-key (kbd "M-x") #'smex)

;; Keep region when undoing in region
(defadvice undo-tree-undo (around keep-region activate)
  (if (use-region-p)
      (let ((m (set-marker (make-marker) (mark)))
            (p (set-marker (make-marker) (point))))
        ad-do-it
        (goto-char p)
        (set-mark m)
        (set-marker p nil)
        (set-marker m nil))
    ad-do-it))

;; Coloring in the minibuffer completion buffer
(autoload 'dircolors "dircolors" nil t)
(add-hook 'completion-list-mode-hook #'dircolors)
(add-hook 'buffer-menu-mode-hook #'dircolors)

;; Ibuffer: sort by projects
(add-hook
 'ibuffer-hook
 (lambda ()
   (setq ibuffer-filter-groups (ibuffer-project-generate-filter-groups))
   (unless (eq ibuffer-sorting-mode 'project-file-relative)
     (ibuffer-do-sort-by-project-file-relative))))

;; Save on switching away from Emacs
(defun save-all ()
  (interactive)
  (save-some-buffers t))
(add-hook 'focus-out-hook 'save-all)
