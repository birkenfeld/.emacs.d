;; Contains all new global / general keybindings

;; Enable "find file at point", but don't try to find URLs
(ffap-bindings)
(setq ffap-url-regexp nil)

;; ---- Generic global keybinds

;; Useful with Super keybinds
(global-set-key (kbd "s-g") #'keyboard-quit)

;; Support file and lineno finding with "filename:linenum"
(global-set-key (kbd "C-x C-f") #'find-file-with-linenum)

;; Dired
(global-set-key (kbd "C-x C-d") #'dired)

;; Proced
(global-set-key (kbd "C-x C-p") #'proced)

;; ---- Appearance

;; Sometimes it's useful to re-highlight the whole buffer
(global-set-key (kbd "<f8>") #'font-lock-fontify-buffer)

;; Speedbar in our frame? sure.
(global-set-key (kbd "<f9>") #'sr-speedbar-toggle)

;; Switch menu-bar on/off
(global-set-key (kbd "<f10>") #'menu-bar-mode)

;; Fullscreen editing
(global-set-key (kbd "<f11>") #'fullscreen)

;; Toggle line number display
(global-set-key (kbd "C-c n") #'global-linum-mode)

;; Font size management
(global-set-key (kbd "C-x C-+") #'increase-font-size)
(global-set-key (kbd "C-x C--") #'decrease-font-size)

;; ---- Windows

;; Jump windows with ace-jump
(global-set-key (kbd "C-x o") #'ace-window)

;; Display same buffer in other window too
(global-set-key (kbd "C-x C-o") #'clone-indirect-buffer-other-window)

;; Rotate windows
(global-set-key (kbd "C-x M-o") #'rotate-windows)

;; Split window into 3
(global-set-key (kbd "C-x 7") #'split-window-horizontally-into-3)

;; windmove: easily move between windows
(global-set-key (kbd "C-x <left>")  #'windmove-left)
(global-set-key (kbd "C-x <right>") #'windmove-right)
(global-set-key (kbd "C-x <up>")    #'windmove-up)
(global-set-key (kbd "C-x <down>")  #'windmove-down)
;; these are usually bound to next-buffer, but this is better
(global-set-key (kbd "C-x <C-left>")  #'windmove-left)
(global-set-key (kbd "C-x <C-right>") #'windmove-right)
(global-set-key (kbd "C-x <C-up>")    #'windmove-up)
(global-set-key (kbd "C-x <C-down>")  #'windmove-down)

;; ---- Buffers

;; Better buffer list
(global-set-key (kbd "C-x C-b") #'ibuffer)

;; Killing buffers more quickly
(global-set-key (kbd "C-x k")   #'kill-this-buffer)
(global-set-key (kbd "C-x C-k") #'kill-buffer-and-window)

;; ---- Files

;; ido is usually less optimal for write-file
(defalias 'write-file-original #'write-file)  ;; avoid [remap] done by ido
(global-set-key (kbd "C-x C-w") #'write-file-original)

;; Rename buffer and file at the same time
(global-set-key (kbd "C-x C-r") #'rename-current-buffer-file)

;; ---- Navigation

;; Scroll without moving cursor
(global-set-key (kbd "M-<up>") '(lambda () (interactive) (scroll-down 5)))
(global-set-key (kbd "M-<down>") '(lambda () (interactive) (scroll-up 5)))

;; F6 stores a position in a file, S-F6 or C-F6 brings you back to this position
(global-set-key (kbd "<f6>") '(lambda () (interactive) (point-to-register ?1)))
(global-set-key (kbd "<S-f6>") '(lambda () (interactive) (register-to-point ?1)))
(global-set-key (kbd "<C-f6>") '(lambda () (interactive) (register-to-point ?1)))

;; Quickly compare two windows with almost same content
(global-set-key (kbd "<f12>") #'compare-windows)

;; Pop mark (switch between last positions in buffers)
(global-set-key (kbd "C-:") #'pop-to-mark-command)

;; Goto position of last change in a buffer
(global-set-key (kbd "C-;") #'goto-last-change)

;; Something like vim's '*' binding
(global-set-key (kbd "C-+") #'search-for-this-word)
(global-set-key (kbd "C-*") #'lazy-highlight-cleanup)

;; better C-a behavior: toggle indentation/start of line
(global-set-key [remap move-beginning-of-line] #'prelude-move-beginning-of-line)

;; Goto line with linum turned on
(global-set-key [remap goto-line] #'goto-line-with-feedback)

;; Bookmarks
(global-set-key (kbd "C-c b t") #'bm-toggle)
(global-set-key (kbd "C-c b n") #'bm-next)
(global-set-key (kbd "C-c b p") #'bm-previous)

;; Select Imenu entries from Ido
(global-set-key (kbd "C-x m") #'idomenu)

;; Grepping
(global-set-key (kbd "C-x g") #'grep)
(global-set-key (kbd "C-c g") #'grep-find)

;; ---- Editing

;; Indent automatically
(global-set-key (kbd "RET") #'newline-and-indent)

;; Consistent binding for killing sexps
(global-set-key (kbd "<C-M-backspace>") #'backward-kill-sexp)

;; Custom margin keys (useful for Python indentation)
(global-set-key (kbd "C-M-+") #'increase-left-margin)
(global-set-key (kbd "C-M--") #'decrease-left-margin)

;; C-k is kill-whole-line, C-l is kill to end of line
(global-set-key (kbd "C-k") #'kill-whole-line)
(global-set-key (kbd "C-l") #'kill-line)
(global-set-key (kbd "M-k") #'kill-this-symbol)

;; Align code in a pretty way
(global-set-key (kbd "C-x \\") #'align-regexp)

;; Alt-space expands
(global-set-key (kbd "M-SPC") #'hippie-expand)

;; Make commenting easy; uncommenting with prefix arg
(global-set-key (kbd "C-#") #'comment-or-uncomment-region-dwim)
(global-set-key (kbd "M-#") #'comment-dwim)

;; Eval a Calc expression in region or preceding point
(global-set-key (kbd "C-=") #'calc-eval-region)

;; Eval a Lisp expression and replace anywhere
(global-set-key (kbd "C-c C-e") #'eval-and-replace)
(global-set-key (kbd "C-c C-j") #'eval-print-last-sexp)

;; Wrap line in parens
(global-set-key (kbd "C-(") #'wrap-line-in-parens)

;; Better mark-word
(global-set-key (kbd "M-+") #'mark-word)

;; Redo
(global-set-key (kbd "C-x U") #'redo)

;; cycle whitespace at the point
(global-set-key (kbd "S-SPC") #'cycle-spacing)
;; M-del should delete forward
(global-set-key (kbd "M-<delete>") #'kill-word)

;; Transpose stuff with M-t
(global-unset-key (kbd "M-t")) ;; which used to be transpose-words
(global-set-key (kbd "M-t l") #'transpose-lines)
(global-set-key (kbd "M-t w") #'transpose-words)
(global-set-key (kbd "M-t s") #'transpose-sexps)
(global-set-key (kbd "M-t p") #'transpose-params)

;; Repeat simple and complex commands
(global-set-key (kbd "C-.") #'repeat)

;; expand-region + change-inner
(global-set-key (kbd "C-,") #'er/expand-region)
(global-set-key (kbd "M-i") #'change-inner)
(global-set-key (kbd "M-o") #'change-outer)
(global-set-key (kbd "s-i") #'copy-inner)
(global-set-key (kbd "s-o") #'copy-outer)

;; ace-jump
(global-set-key (kbd "C-ö")   #'ace-jump-mode)
(global-set-key (kbd "M-g l") #'ace-jump-line-mode)

;; Vim-like open line
(global-set-key (kbd "C-o")        #'open-line-below)
(global-set-key (kbd "<S-return>") #'open-line-below)
(global-set-key (kbd "C-S-o")      #'open-line-above)

;; Copy common beginning from the last two lines
(global-set-key (kbd "C-ä") #'copy-above-while-same)

;; Align word after point to same position as in previous line
(global-set-key (kbd "C-ü") #'align-around-point-to-above)

;; Killing text
(global-set-key (kbd "C-S-k") #'kill-and-retry-line)

;; Use M-w for easy-kill if no active region
(global-set-key (kbd "M-w") #'easy-kill)

;; More neat bindings for C-x 8
(global-set-key (kbd "C-x 8 t m") (lambda () (interactive) (insert "™")))
(global-set-key (kbd "C-x 8 ( c )") (lambda () (interactive)  (insert "©")))
(global-set-key (kbd "C-x 8 8") (lambda () (interactive)  (insert "∞")))

;; Fast jumping / killing etc. to next occurrence of a character
(global-set-key (kbd "M-j") #'fastnav-jump-to-char-forward)
(global-set-key (kbd "M-J") #'fastnav-jump-to-char-backward)
(global-set-key (kbd "M-m") #'fastnav-mark-to-char-forward)
(global-set-key (kbd "M-M") #'fastnav-mark-to-char-backward)
(global-set-key (kbd "M-z") #'fastnav-zap-up-to-char-forward)
(global-set-key (kbd "M-Z") #'fastnav-zap-up-to-char-backward)

;; ---- External commands

;; Popup shell window
(global-set-key (kbd "<menu>") #'shell-pop)

;; M-/ runs shell command with region as stdin
(global-set-key (kbd "M-/") #'shell-command-on-region)
;; M-& runs shell command with region as stdin and replaces it with stdout
(global-set-key (kbd "M-&") (lambda () (interactive)
                              (setq current-prefix-arg (list 4))
                              (call-interactively #'shell-command-on-region)))

;; ---- Isearch

;; Use regex searches by default
(global-set-key (kbd "C-s")   #'isearch-forward-regexp)
(global-set-key (kbd "C-r")   #'isearch-backward-regexp)
(global-set-key (kbd "C-M-s") #'isearch-forward)
(global-set-key (kbd "C-M-r") #'isearch-backward)

;; Since M-s o isn't needed for occur, use M-s directly for word search
(global-set-key (kbd "M-s") #'isearch-forward-symbol)

;; C-k is "kill match" in isearch
(define-key isearch-mode-map (kbd "C-k") #'isearch-kill-match)
;; C-o is occur in isearch
(define-key isearch-mode-map (kbd "C-o") #'isearch-occur)
;; C-w yanks the whole symbol
(define-key isearch-mode-map (kbd "C-w") #'isearch-yank-whole-symbol)
;; Backspace deletes one char from the search string...
(define-key isearch-mode-map (kbd "DEL") #'isearch-del-char)
;; ... while C-Backspace goes back
(define-key isearch-mode-map (kbd "<C-backspace>") #'isearch-delete-char)
;; Zap to search string
(define-key isearch-mode-map (kbd "M-z") #'zap-to-isearch)
;; Start a query-replace
(define-key isearch-mode-map (kbd "M-q") #'isearch-query-replace-regexp)

;; ---- Occur

;; Global easy shortcut for occur-mode
(global-set-key (kbd "C-c o") #'occur-dwim)

;; Next/previous match in occur
(define-key occur-mode-map "n" #'next-error-no-select)
(define-key occur-mode-map "p" #'previous-error-no-select)
(define-key occur-mode-map "v" #'occur-mode-display-occurrence)

;; ---- Rectangles and stuff with multiple-cursors

(global-set-key (kbd "C-x C-ä") #'mc/edit-ends-of-lines)
(global-set-key (kbd "C-x C-S-ä") #'mc/edit-beginnings-of-lines)
(global-set-key (kbd "C-x C-M-ä") #'mc/edit-lines)
(global-set-key (kbd "C-S-<mouse-1>") #'mc/add-cursor-on-click)

;; Mark additional regions matching current region
(global-set-key (kbd "s-m") #'mc/mark-all-dwim)
;;(global-set-key (kbd "s-p") #'mc/mark-previous-like-this)
;;(global-set-key (kbd "s-n") #'mc/mark-next-like-this)
(global-set-key (kbd "s-r") #'mc/mark-all-in-region)

;; ;; Symbol and word specific mark-more
(global-set-key (kbd "s-w") #'mc/mark-next-word-like-this)
(global-set-key (kbd "s-a") #'mc/mark-all-symbols-like-this)
;;(global-set-key (kbd "s-å") #'mc/mark-previous-word-like-this)
;;(global-set-key (kbd "M-s-æ") #'mc/mark-all-words-like-this)
(global-set-key (kbd "s-n") #'mc/mark-next-symbol-like-this)
(global-set-key (kbd "s-p") #'mc/mark-previous-symbol-like-this)

;; Extra multiple cursors stuff
(global-set-key (kbd "s-t") #'mc/reverse-regions)
(global-set-key (kbd "s-.") #'mc/sort-regions)
(global-set-key (kbd "s-#") #'mc/insert-numbers)

(global-set-key (kbd "<C-return>") #'set-or-deactivate-rectangular-region-anchor)
(define-key rectangular-region-mode-map (kbd "C-w") #'kill-region-deactivate-mc)

;; Disable electric inserting keys when rrm or mc is active
(mapcar (lambda (map)
          (define-key map [remap company-insert-and-complete] #'self-insert-command)
          (define-key map [remap autopair-insert-or-skip-quote] #'self-insert-command)
          (define-key map [remap autopair-insert-opening] #'self-insert-command)
          (define-key map [remap autopair-extra-skip-close-maybe] #'self-insert-command)
          (define-key map [remap autopair-skip-close-maybe] #'self-insert-command))
        (list rectangular-region-mode-map mc/keymap))

;; ---- Projects

;; Search
(global-set-key (kbd "<f2>") #'projectile-grep)
(global-set-key (kbd "S-<f2>") #'projectile-ag)

;; Compile project
(global-set-key (kbd "<f5>")   #'projectile-recompile-project)
(global-set-key (kbd "S-<f5>") #'projectile-compile-project)

;; ---- Mouse

;; Useful mouse behavior
(global-set-key (kbd "<left-fringe> <down-mouse-1>") #'mouse-drag-region)
(global-set-key (kbd "<left-fringe> <drag-mouse-1>") #'mouse-set-region)

;; Get a buffer menu with the right mouse button.
(global-set-key (kbd "<mouse-3>") #'mouse-buffer-menu)

;; Bury buffer with right-click on header line
(global-set-key (kbd "<header-line> <mouse-3>") #'bury-selected-buffer)

;; Support back and forward mouse buttons
(eval-after-load "help-mode"
  '(progn
     (define-key help-mode-map (kbd "<mouse-8>") #'help-go-back)
     (define-key help-mode-map (kbd "<mouse-9>") #'help-go-forward)))
(eval-after-load "info"
  '(progn
     (define-key Info-mode-map (kbd "<mouse-8>") #'Info-history-back)
     (define-key Info-mode-map (kbd "<mouse-9>") #'Info-history-forward)))

;; ---- Global mode-specifics

;; In the minibuffer, do not kill but delete
(define-key minibuffer-local-map (kbd "M-DEL") #'backward-delete-word)

;; more powerful tab-completion in minibuffer
(define-key minibuffer-local-map "\t" #'completion-at-point)

;; Better C-h bindings
(define-key 'help-command (kbd "a") #'apropos)
(define-key 'help-command (kbd "C-f") #'find-function)
(define-key 'help-command (kbd "C-k") #'find-function-on-key)
(define-key 'help-command (kbd "C-v") #'find-variable)
(define-key 'help-command (kbd "C-l") #'find-library)
(define-key 'help-command (kbd "C-i") #'info-display-manual)

;; ---- Hide-show mode

(eval-after-load "hideshow"
  '(progn
     (define-key hs-minor-mode-map (kbd "C-c , ,") #'hs-toggle-hiding)
     (define-key hs-minor-mode-map (kbd "C-c , .") #'hs-hide-all)
     (define-key hs-minor-mode-map (kbd "C-c , :") #'hs-show-all)
     (define-key hs-minor-mode-map (kbd "C-c , -") (lambda () (interactive) (hs-hide-level 2)))
     (define-key hs-minor-mode-map (kbd "C-c <C-left>")  #'hs-hide-block)
     (define-key hs-minor-mode-map (kbd "C-c <C-right>") #'hs-show-block)))
