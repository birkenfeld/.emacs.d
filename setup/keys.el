;; Contains all new global / general keybindings

;; Enable "find file at point", but don't try to find URLs
(ffap-bindings)
(setq ffap-url-regexp nil)

;; Support file and lineno finding with "filename:linenum"
(global-set-key (kbd "C-x C-f") 'find-file-with-linenum)

;; Shortcut for reverting a buffer
(global-set-key (kbd "C-x C-r") 'revert-buffer)

;; Display same buffer in other window too
(global-set-key (kbd "C-x C-o") 'clone-indirect-buffer-other-window)

;; Rotate windows
(global-set-key (kbd "C-x M-o") 'rotate-windows)

;; Indent automatically
(global-set-key (kbd "RET") 'newline-and-indent)

;; In the minibuffer, do not kill but delete
(define-key minibuffer-local-map (kbd "M-DEL") 'backward-delete-word)

(global-set-key (kbd "<C-M-backspace>") 'backward-kill-sexp)

;; Useful mouse behavior
(global-set-key (kbd "<left-fringe> <down-mouse-1>") 'mouse-drag-region)
(global-set-key (kbd "<s-mouse-1>") 'mouse-delete-other-windows)
(global-set-key (kbd "<s-mouse-3>") 'mouse-delete-window)

;; Get a buffer menu with the right mouse button.
(global-set-key (kbd "<mouse-3>") 'mouse-buffer-menu)

;; Split window into 3
(global-set-key (kbd "C-x 7") 'split-window-horizontally-into-3)

;; windmove: easily move between windows
(global-set-key (kbd "C-x <left>")  'windmove-left)
(global-set-key (kbd "C-x <right>") 'windmove-right)
(global-set-key (kbd "C-x <up>")    'windmove-up)
(global-set-key (kbd "C-x <down>")  'windmove-down)

;; Better buffer list
(global-set-key (kbd "C-x C-b") 'ibuffer)

;; Custom margin keys (useful for Python indentation)
(global-set-key (kbd "C-M-+") 'increase-left-margin)
(global-set-key (kbd "C-M--") 'decrease-left-margin)

;; Use regex searches by default
(global-set-key (kbd "C-s")   'isearch-forward-regexp)
(global-set-key (kbd "C-r")   'isearch-backward-regexp)
(global-set-key (kbd "C-M-s") 'isearch-forward)
(global-set-key (kbd "C-M-r") 'isearch-backward)

;; compile/make
;(global-set-key (kbd "<f5>")   'recompile)
;(global-set-key (kbd "S-<f5>") 'compile)

;; C-k is kill-whole-line
(global-set-key (kbd "C-k") 'kill-whole-line)

;; shortcuts for switching to "other" file
;; (binding interferes with gud)
;(global-set-key (kbd "C-x C-a") 'ff-find-other-file)

;; Shortcuts for killing buffers
(global-set-key (kbd "C-x k")   'kill-this-buffer)
(global-set-key (kbd "C-x K")   'kill-other-buffer)
(global-set-key (kbd "C-x C-k") 'kill-buffer-and-window)

;; Toggle two most recent buffers
(fset 'quick-switch-buffer [?\C-x ?b return])
(global-set-key (kbd "s-b") 'quick-switch-buffer)
(global-set-key (kbd "s-y") 'bury-buffer)

;; Align code in a pretty way
(global-set-key (kbd "C-x \\") 'align-regexp)

;; Join line with previous
(global-set-key (kbd "C-c ^") 'join-line)

;; F6 stores a position in a file, F7 brings you back to this position
(global-set-key (kbd "<f6>") '(lambda () (interactive) (point-to-register ?1)))
(global-set-key (kbd "<f7>") '(lambda () (interactive) (register-to-point ?1)))

;; Sometimes it's useful to re-highlight the whole buffer
(global-set-key (kbd "<f8>") 'font-lock-fontify-buffer)

;; Speedbar in our frame? sure.
(global-set-key (kbd "<f9>") 'sr-speedbar-toggle)

;; Switch menu-bar on/off
(global-set-key (kbd "<f10>") 'menu-bar-mode)

;; Fullscreen editing
(global-set-key (kbd "<f11>") 'fullscreen)

;; Quickly compare two windows with almost same content
(global-set-key (kbd "<f12>") 'compare-windows)

;; Alt-space expands
(global-set-key (kbd "M-SPC") 'hippie-expand)

;; Make commenting easy; uncommenting with prefix arg
(global-set-key (kbd "C-#") 'comment-region)
(global-set-key (kbd "M-#") 'comment-region)

;; Toggle line numer display
(global-set-key (kbd "C-c n") 'global-linum-mode)

;; Global easy shortcut for occur-mode
(global-set-key (kbd "C-c o") 'occur)

;; Since M-s o isn't needed for occur, use M-s directly for word search
(global-set-key (kbd "M-s") 'isearch-forward-symbol)

;; Scroll without moving cursor
(global-set-key (kbd "M-<up>") '(lambda () (interactive) (scroll-down 5)))
(global-set-key (kbd "M-<down>") '(lambda () (interactive) (scroll-up 5)))

;; Something like vim's '*' binding
(global-set-key (kbd "C-+")   'search-for-this-word)
(global-set-key (kbd "C-*")   'lazy-highlight-cleanup)

;; fixup-whitespace puts the "right" amount of whitespace at the point
(global-set-key (kbd "S-SPC") 'fixup-whitespace)
;; M-del should delete forward
(global-set-key (kbd "M-<delete>") 'kill-word)

;; M-/ runs shell command with region as stdin
(global-set-key (kbd "M-/") 'shell-command-on-region)
;; M-& runs shell command with region as stdin and replaces it with stdout
(global-set-key (kbd "M-&") (lambda () (interactive)
                              (setq current-prefix-arg (list 4))
                              (call-interactively 'shell-command-on-region)))

;; Repeat simple and complex commands
(global-set-key (kbd "C-.") 'repeat)

;; Find everything with apropos
(global-set-key (kbd "C-h a") 'apropos)

;; Copy from above lines
(global-set-key (kbd "C-c <right>") 'copy-above-while-same)

;; Font size management
(global-set-key (kbd "C-x C-+") 'increase-font-size)
(global-set-key (kbd "C-x C--") 'decrease-font-size)

;; Vim-like open line
(global-set-key (kbd "C-o") 'open-line-below)
(global-set-key (kbd "C-S-o") 'open-line-above)

;; Rename buffer and file at the same time
(global-set-key (kbd "C-x C-r") 'rename-current-buffer-file)
(global-set-key (kbd "C-c r") 'revert-buffer)

;; Copy common beginning from the last two lines
(global-set-key (kbd "C-ä") 'copy-above-while-same)

;; Goto line with linum turned on
(global-set-key [remap goto-line] 'goto-line-with-feedback)

;; Killing text
(global-set-key (kbd "C-S-k") 'kill-and-retry-line)

;; Use M-w for copy-line if no active region
(global-set-key (kbd "M-w") 'save-region-or-current-line)

;; more powerful tab-completion in minibuffer
(add-hook 'minibuffer-setup-hook
          '(lambda ()
             (define-key minibuffer-local-map "\t" 'comint-dynamic-complete)))

;; More neat bindings for C-x 8
(global-set-key (kbd "C-x 8 t m") (lambda () (interactive) (insert "™")))
(global-set-key (kbd "C-x 8 ( c )") (lambda () (interactive)  (insert "©")))
(global-set-key (kbd "C-x 8 8") (lambda () (interactive)  (insert "∞")))

;; Evaluate expression and replace anywhere
(global-set-key (kbd "C-c C-e") 'eval-and-replace)
(global-set-key (kbd "C-c C-j") 'eval-print-last-sexp)

;; Outline ---------------------------------------------------------------------

(eval-after-load "outline"
  '(progn
     (global-set-key (kbd "C-c <C-left>")  'hide-body)
     (global-set-key (kbd "C-c <C-right>") 'show-subtree)))

;; Help/info -------------------------------------------------------------------

;; Support back and forward mouse buttons
(eval-after-load "help-mode"
  '(progn
     (define-key help-mode-map (kbd "<mouse-8>") 'help-go-back)
     (define-key help-mode-map (kbd "<mouse-9>") 'help-go-forward)))
(eval-after-load "info"
  '(progn
     (define-key Info-mode-map (kbd "<mouse-8>") 'Info-history-back)
     (define-key Info-mode-map (kbd "<mouse-9>") 'Info-history-forward)))

;; Isearch ---------------------------------------------------------------------

;; C-k is "kill match" in isearch
(define-key isearch-mode-map (kbd "C-k") 'isearch-kill-match)
;; C-o is occur in isearch
(define-key isearch-mode-map (kbd "C-o") 'isearch-occur)
;; C-w yanks the whole symbol
(define-key isearch-mode-map (kbd "C-w") 'isearch-yank-whole-symbol)
;; Backspace deletes one char from the search string...
(define-key isearch-mode-map (kbd "DEL") 'isearch-del-char)
;; ... while C-Backspace goes back
(define-key isearch-mode-map (kbd "<C-backspace>") 'isearch-delete-char)

;; Occur -----------------------------------------------------------------------

;; next/previous match in occur
(define-key occur-mode-map "n" 'next-error-no-select)
(define-key occur-mode-map "p" 'previous-error-no-select)
(define-key occur-mode-map "v" 'occur-mode-display-occurrence)
