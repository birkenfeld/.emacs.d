;; General setup of sane defaults

;; Scroll one line at a time
(setq scroll-step 1)

;; Don't wait for user input in redisplay
(setq redisplay-dont-pause t)

;; Make "yes or no" "y or n"
(fset 'yes-or-no-p 'y-or-n-p)

;; Window frame title
(setq frame-title-format "emacs [%b %*%+ %f]")
(setq icon-title-format "emacs [%b]")

;; No bells please
(setq ring-bell-function 'ignore)

;; Prefer UTF-8 coding system
(prefer-coding-system 'utf-8)

;; Make all backups in a single directory
(setq backup-directory-alist
      `(("." . ,(expand-file-name "~/.emacs.d/saved/backups"))))

;; Make backups of files, even when they're in version control
(setq vc-make-backup-files t)

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

;; Remove trailing whitespaces before saving (now local)
;(add-hook 'before-save-hook 'delete-trailing-whitespace)
;; Update copyright headers before saving
(add-hook 'before-save-hook 'copyright-update)
;; Update timestamp ("last modified") before saving
(setq time-stamp-pattern "10/[Ll]ast modified: %:y-%02m-%02d %02H:%02M by %u$")
(add-hook 'before-save-hook 'time-stamp)

;; Make file executable if it's a script
(add-hook 'after-save-hook 'executable-make-buffer-file-executable-if-script-p)

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

;; Save point position between sessions
(require 'saveplace)
(setq-default save-place t)
(setq save-place-file (expand-file-name "places" user-emacs-directory))

;; Auto-fill in text mode
(add-hook 'text-mode-hook 'auto-fill-mode)

;; Nice config file modes
(require 'generic-x)

;; Electric bindings for help mode
(require 'ehelp)

;; Abbrev file for abbrev-mode
(setq abbrev-file-name "~/.emacs.d/saved/abbrevs")
(when (file-exists-p abbrev-file-name)
  (read-abbrev-file abbrev-file-name t))

;; Enable wdired, editing filenames in dired renames files
(require 'wdired)
(define-key dired-mode-map (kbd "r") 'wdired-change-to-wdired-mode)

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

;; Nice xterm mouse handling (fails with dragging)
;(xterm-mouse-mode t)

;; Bindings for textmate-like auto pairing of parens/quotes
(require 'autopair)

;; Use it in latex mode for dollar-style inline math
(add-hook 'latex-mode-hook
          #'(lambda ()
              (autopair-mode)
              (modify-syntax-entry ?$ \"\\\"\")))

;; Sphinx templated files: find mode after removing _t suffix
(add-to-list 'auto-mode-alist '("_t$" nil t))

;; Kill minibuffer when changing buffer by mouseclick
(defun stop-using-minibuffer ()
  (when (and (>= (recursion-depth) 1)
             (active-minibuffer-window))
    (abort-recursive-edit)))
(add-hook 'mouse-leave-buffer-hook 'stop-using-minibuffer)

;; Make some mode line displays smaller
(when (require 'diminish nil 'noerror)
  (eval-after-load "reveal" '(diminish 'reveal-mode))
  (eval-after-load "eproject" '(diminish 'eproject-mode))
  (eval-after-load "volatile-highlights" '(diminish 'volatile-highlights-mode))
  (eval-after-load "highlight-symbol" '(diminish 'highlight-symbol-mode))
  (eval-after-load "autopair" '(diminish 'autopair-mode " ()"))
  (eval-after-load "guide-key" '(diminish 'guide-key-mode))
  (eval-after-load "yasnippet" '(diminish 'yas-minor-mode " Y")))

;; M-x enhancement
(require 'smex)
(smex-initialize)
(global-set-key (kbd "M-x") 'smex)
(global-set-key (kbd "M-X") 'smex-major-mode-commands)

;; Smooth scrolling (keep cursor away from screen edges)
(require 'smooth-scrolling)

;; guide-key: pop up a list of keybindings for prefixes with lots of commands
(require 'guide-key)
(setq guide-key/guide-key-sequence '("C-x r" "C-x 4" "C-x v" "C-x 8" "C-x +"
                                     "C-c p" "C-c !" "C-x RET" "C-c ," "C-c ^"))
(guide-key-mode 1)
(setq guide-key/idle-delay 0.75)
(setq guide-key/recursive-key-sequence-flag t)
(setq guide-key/popup-window-position 'bottom)

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
(add-hook 'completion-list-mode-hook 'dircolors)
(add-hook 'buffer-menu-mode-hook 'dircolors)

;; Jump windows with ace-jump
(global-set-key (kbd "C-x o") 'ace-window)

;; Bury buffer with right-click on header line
(global-set-key (kbd "<header-line> <mouse-3>") 'bury-selected-buffer)

(defun bury-selected-buffer (event)
  (interactive "e")
  (with-selected-window (posn-window (event-start event))
    (bury-buffer)))

;; tabbar
;; (require 'tabbar)
;; (global-set-key [C-prior] 'tabbar-backward)
;; (global-set-key [C-next] 'tabbar-forward)
;; ;; kill buffer on left click, switch mode on right click
;; (defun my-tabbar-home-function (event)
;;   (let ((mouse-button (event-basic-type event)))
;;     (cond
;;      ((eq mouse-button 'mouse-3)
;;       (tabbar-buffer-show-groups (not tabbar--buffer-show-groups)))
;;      ((eq mouse-button 'mouse-1)
;;       (kill-buffer nil))
;;      )))
;; (add-hook 'tabbar-init-hook
;;           (lambda () (setq tabbar-home-function 'my-tabbar-home-function))
;;           t)  ; append
;;(tabbar-mode 1)
;;(tabbar-mwheel-mode 1)
