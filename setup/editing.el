;; Editing / navigating etc. setup

;; winpoint (remember point location by window)
(require 'winpoint)
(window-point-remember-mode 1)

;; Fast jumping / killing etc. to next occurrence of a character
(require 'fastnav)
(global-set-key (kbd "M-j") 'fastnav-jump-to-char-forward)
(global-set-key (kbd "M-J") 'fastnav-jump-to-char-backward)
(global-set-key (kbd "M-m") 'fastnav-mark-to-char-forward)
(global-set-key (kbd "M-M") 'fastnav-mark-to-char-backward)
(global-set-key (kbd "M-z") 'fastnav-zap-up-to-char-forward)
(global-set-key (kbd "M-Z") 'fastnav-zap-up-to-char-backward)

;; expand-region + change-inner
(require 'expand-region)
(require 'change-inner)
(global-set-key (kbd "C-,") 'er/expand-region)
(global-set-key (kbd "M-i") 'change-inner)
(global-set-key (kbd "M-o") 'change-outer)
(global-set-key (kbd "s-i") 'copy-inner)
(global-set-key (kbd "s-o") 'copy-outer)

;; ace-jump
(require 'ace-jump-mode)
(global-set-key (kbd "C-ö") 'ace-jump-mode)  ;; nothing else free...
                                             ;; let's make use of these umlauts

;; Display match count while searching/replacing
(require 'anzu)
(global-anzu-mode 1)
(global-set-key (kbd "M-%") 'anzu-query-replace)
(global-set-key (kbd "C-M-%") 'anzu-query-replace-regexp)

;; highlight newly-inserted text etc
(require 'volatile-highlights)
(volatile-highlights-mode t)

;; Move whole lines
(require 'move-text)
(global-set-key (kbd "<C-S-down>") 'move-text-down)
(global-set-key (kbd "<C-S-up>") 'move-text-up)

;; Martin Blais' dubious paragraphs
(require 'dubious-paragraphs)
(global-set-key [(meta n)] 'dubious-forward-paragraph)
(global-set-key [(meta N)] 'dubious-forward-paragraph-scroll)
(global-set-key [(meta p)] 'dubious-backward-paragraph)
(global-set-key [(meta P)] 'dubious-backward-paragraph-scroll)

;; Martin Blais' repeatable macros
(require 'repeatable)
(repeatable-command-advice kmacro-end-and-call-macro)
;(repeatable-substitute-binding 'search-for-this-word)
(repeatable-command-advice hl-symbol-and-jump)
(repeatable-command-advice next-error)
(repeatable-command-advice previous-error)

;; Go to last change in buffer
(require 'goto-chg)
(global-set-key (kbd "C-;") 'goto-last-change)

;; Adaptive fill is great
(require 'filladapt)
(setq-default filladapt-mode t)

;; Browse the kill ring for things to yank
(require 'browse-kill-ring)
(browse-kill-ring-default-keybindings)
(defadvice yank-pop (around kill-ring-browse-maybe (arg) activate)
  "If last action was not a yank, run `browse-kill-ring' instead."
  (if (not (eq last-command 'yank))
      (browse-kill-ring)
    ad-do-it))

;; Redo command
(autoload 'redo "redo" nil t)
(global-set-key (kbd "C-x U") 'redo)

;; full-ack mode
(autoload 'ack-same "full-ack" nil t)
(autoload 'ack "full-ack" nil t)
(global-set-key (kbd "<f2>") 'ack-same)
(global-set-key (kbd "S-<f2>") 'ack)

;; ido/idomenu: switch to a buffer local tag with ido completion
(autoload 'idomenu "idomenu" nil t)
(global-set-key (kbd "C-x m") 'idomenu)

;; Color dabbrev-expanded phrases
(require 'dabbrev-highlight)

;; Bookmarks
(autoload 'bm-toggle "bm" nil t)
(autoload 'bm-next "bm" nil t)
(autoload 'bm-previous "bm" nil t)
(global-set-key (kbd "C-c b t") 'bm-toggle)
(global-set-key (kbd "C-c b n") 'bm-next)
(global-set-key (kbd "C-c b p") 'bm-previous)

;; Neotree: hide files otherwise hidden in find-file etc.
(eval-after-load "neotree"
  '(setq neo-hidden-files-regexp
         (concat "\\(" (regexp-opt completion-ignored-extensions) "\\|#\\)$")))

;; When popping the mark, continue popping until the cursor actually moves
;; Also, if the last command was a copy - skip past all the expand-region cruft.
(defadvice pop-to-mark-command (around ensure-new-position activate)
  (let ((p (point)))
    (when (eq last-command 'save-region-or-current-line)
      ad-do-it
      ad-do-it
      ad-do-it)
    (dotimes (i 10)
      (when (= p (point)) ad-do-it))))

;; The new register-preview feature in 24.4 is nice, but it destroys my window
;; configuration; use popwin to pop up the preview in the bottom
(require 'popwin)
(add-to-list 'display-buffer-alist '("Register Preview" popwin:special-display-popup-window))

;; Colorize register preview a bit
(defun register-preview-color (r)
  (let* ((c (car r))
         (d (replace-regexp-in-string
             "\n[ \t]*" " "
             (with-output-to-string (describe-register-1 c t))))
         (s (if (string-match "Register.+? contains \\(?:an? \\|the \\)?" d)
                (substring d (match-end 0)) d))
         (k (propertize (single-key-description c) 'face 'font-lock-function-name-face)))
  (concat k ": " s "\n")))

(setq register-preview-function #'register-preview-color)


;; Multiple cursors ------------------------------------------------------------

;; ;; Experimental multiple-cursors
;; (global-set-key (kbd "C-S-c C-S-c") 'mc/edit-lines)
;; (global-set-key (kbd "C-S-c C-e") 'mc/edit-ends-of-lines)
;; (global-set-key (kbd "C-S-c C-a") 'mc/edit-beginnings-of-lines)

;; ;; Mark additional regions matching current region
;; (global-set-key (kbd "M-æ") 'mc/mark-all-dwim)
;; (global-set-key (kbd "C-å") 'mc/mark-previous-like-this)
;; (global-set-key (kbd "C-æ") 'mc/mark-next-like-this)
;; (global-set-key (kbd "C-Æ") 'mc/mark-more-like-this-extended)
;; (global-set-key (kbd "M-å") 'mc/mark-all-in-region)

;; ;; Symbol and word specific mark-more
;; (global-set-key (kbd "s-æ") 'mc/mark-next-word-like-this)
;; (global-set-key (kbd "s-å") 'mc/mark-previous-word-like-this)
;; (global-set-key (kbd "M-s-æ") 'mc/mark-all-words-like-this)
;; (global-set-key (kbd "s-Æ") 'mc/mark-next-symbol-like-this)
;; (global-set-key (kbd "s-Å") 'mc/mark-previous-symbol-like-this)
;; (global-set-key (kbd "M-s-Æ") 'mc/mark-all-symbols-like-this)

;; ;; Extra multiple cursors stuff
;; (global-set-key (kbd "C-~") 'mc/reverse-regions)
;; (global-set-key (kbd "M-~") 'mc/sort-regions)
;; (global-set-key (kbd "H-~") 'mc/insert-numbers)

;; (global-set-key (kbd "C-S-<mouse-1>") 'mc/add-cursor-on-click)

;; ;; Set anchor to start rectangular-region-mode
;; (global-set-key (kbd "H-SPC") 'set-rectangular-region-anchor)

;; ;; Replace rectangle-text with inline-string-rectangle
;; (global-set-key (kbd "C-x r t") 'inline-string-rectangle)
