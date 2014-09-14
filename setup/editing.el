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

;; wgrep (edit in grep results)
(require 'wgrep)

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
