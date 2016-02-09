;; Contains all new global / general keybindings

;; Enable "find file at point", but don't try to find URLs
(ffap-bindings)
(setq ffap-url-regexp nil)

;; Support file and lineno finding with "filename:linenum"
(global-set-key (kbd "C-x C-f") 'find-file-with-linenum)

;; Dired
(global-set-key (kbd "C-x C-d") 'dired)

;; Jump windows with ace-jump
(global-set-key (kbd "C-x o") 'ace-window)

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
;; these are usually bound to next-buffer, but this is better
(global-set-key (kbd "C-x <C-left>")  'windmove-left)
(global-set-key (kbd "C-x <C-right>") 'windmove-right)
(global-set-key (kbd "C-x <C-up>")    'windmove-up)
(global-set-key (kbd "C-x <C-down>")  'windmove-down)

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

;; C-k is kill-whole-line, C-l is kill to end of line
(global-set-key (kbd "C-k") 'kill-whole-line)
(global-set-key (kbd "C-l") 'kill-line)

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

;; Search
(global-set-key (kbd "<f2>") 'projectile-grep)
(global-set-key (kbd "S-<f2>") 'projectile-ag)

;; Compile project
(global-set-key (kbd "<f5>")   'projectile-recompile-project)
(global-set-key (kbd "S-<f5>") 'projectile-compile-project)

;; F6 stores a position in a file, S-F6 or C-F6 brings you back to this position
(global-set-key (kbd "<f6>") '(lambda () (interactive) (point-to-register ?1)))
(global-set-key (kbd "<S-f6>") '(lambda () (interactive) (register-to-point ?1)))
(global-set-key (kbd "<C-f6>") '(lambda () (interactive) (register-to-point ?1)))

;; f7 is free

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
(global-set-key (kbd "C-#") 'comment-or-uncomment-region-dwim)
(global-set-key (kbd "M-#") 'comment-dwim)

;; Toggle line numer display
(global-set-key (kbd "C-c n") 'global-linum-mode)

;; Global easy shortcut for occur-mode
(global-set-key (kbd "C-c o") 'occur-dwim)

;; Pop mark
(global-set-key (kbd "C-:") 'pop-to-mark-command)

;; Since M-s o isn't needed for occur, use M-s directly for word search
(global-set-key (kbd "M-s") 'isearch-forward-symbol)

;; Scroll without moving cursor
(global-set-key (kbd "M-<up>") '(lambda () (interactive) (scroll-down 5)))
(global-set-key (kbd "M-<down>") '(lambda () (interactive) (scroll-up 5)))

;; Something like vim's '*' binding
(global-set-key (kbd "C-+")   'search-for-this-word)
(global-set-key (kbd "C-*")   'lazy-highlight-cleanup)

;; better C-a behavior: toggle indentation/start of line
(global-set-key [remap move-beginning-of-line]
                'prelude-move-beginning-of-line)

;; cycle whitespace at the point
(global-set-key (kbd "S-SPC") 'cycle-spacing)
;; M-del should delete forward
(global-set-key (kbd "M-<delete>") 'kill-word)

;; M-/ runs shell command with region as stdin
(global-set-key (kbd "M-/") 'shell-command-on-region)
;; M-& runs shell command with region as stdin and replaces it with stdout
(global-set-key (kbd "M-&") (lambda () (interactive)
                              (setq current-prefix-arg (list 4))
                              (call-interactively 'shell-command-on-region)))

;; Transpose stuff with M-t
(global-unset-key (kbd "M-t")) ;; which used to be transpose-words
(global-set-key (kbd "M-t l") 'transpose-lines)
(global-set-key (kbd "M-t w") 'transpose-words)
(global-set-key (kbd "M-t s") 'transpose-sexps)
(global-set-key (kbd "M-t p") 'transpose-params)

;; Repeat simple and complex commands
(global-set-key (kbd "C-.") 'repeat)

;; Better C-h bindings
(define-key 'help-command (kbd "a") 'apropos)
(define-key 'help-command (kbd "C-f") 'find-function)
(define-key 'help-command (kbd "C-k") 'find-function-on-key)
(define-key 'help-command (kbd "C-v") 'find-variable)
(define-key 'help-command (kbd "C-l") 'find-library)
(define-key 'help-command (kbd "C-i") 'info-display-manual)

;; Copy from above lines
(global-set-key (kbd "C-c <right>") 'copy-above-while-same)

;; Font size management
(global-set-key (kbd "C-x C-+") 'increase-font-size)
(global-set-key (kbd "C-x C--") 'decrease-font-size)

;; Vim-like open line
(global-set-key (kbd "C-o") 'open-line-below)
(global-set-key (kbd "<S-return>") 'open-line-below)
(global-set-key (kbd "C-S-o") 'open-line-above)

;; Rename buffer and file at the same time
(global-set-key (kbd "C-x C-r") 'rename-current-buffer-file)
(global-set-key (kbd "C-c r") 'revert-buffer)

;; Copy common beginning from the last two lines
(global-set-key (kbd "C-ä") 'copy-above-while-same)

;; Align word after point to same position as in previous line
(global-set-key (kbd "C-ü") 'align-around-point-to-above)

;; Goto line with linum turned on
(global-set-key [remap goto-line] 'goto-line-with-feedback)

;; Killing text
(global-set-key (kbd "C-S-k") 'kill-and-retry-line)

;; Use M-w for easy-kill if no active region
(global-set-key (kbd "M-w") 'easy-kill)

;; ido is usually less optimal for write-file
(defalias 'write-file-original 'write-file)  ;; avoid [remap] done by ido
(global-set-key (kbd "C-x C-w") 'write-file-original)

;; more powerful tab-completion in minibuffer
(define-key minibuffer-local-map "\t" 'completion-at-point)

;; More neat bindings for C-x 8
(global-set-key (kbd "C-x 8 t m") (lambda () (interactive) (insert "™")))
(global-set-key (kbd "C-x 8 ( c )") (lambda () (interactive)  (insert "©")))
(global-set-key (kbd "C-x 8 8") (lambda () (interactive)  (insert "∞")))

;; Evaluate expression and replace anywhere
(global-set-key (kbd "C-c C-e") 'eval-and-replace)
(global-set-key (kbd "C-c C-j") 'eval-print-last-sexp)

;; Proced
(global-set-key (kbd "C-x p") 'proced)

;; Hide-show mode --------------------------------------------------------------

(eval-after-load "hideshow"
  '(progn
     (define-key hs-minor-mode-map (kbd "C-c , ,") 'hs-toggle-hiding)
     (define-key hs-minor-mode-map (kbd "C-c , .") 'hs-hide-all)
     (define-key hs-minor-mode-map (kbd "C-c , :") 'hs-show-all)
     (define-key hs-minor-mode-map (kbd "C-c , -") (lambda () (interactive) (hs-hide-level 2)))
     (define-key hs-minor-mode-map (kbd "C-c <C-left>")  'hs-hide-block)
     (define-key hs-minor-mode-map (kbd "C-c <C-right>") 'hs-show-block)))

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
;; Zap to search string
(define-key isearch-mode-map (kbd "M-z") 'zap-to-isearch)

;; Occur -----------------------------------------------------------------------

;; next/previous match in occur
(define-key occur-mode-map "n" 'next-error-no-select)
(define-key occur-mode-map "p" 'previous-error-no-select)
(define-key occur-mode-map "v" 'occur-mode-display-occurrence)

;; Rectangles with multiple-cursors --------------------------------------------

(global-set-key (kbd "<C-return>") 'set-or-deactivate-rectangular-region-anchor)
