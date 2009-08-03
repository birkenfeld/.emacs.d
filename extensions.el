;; ---------- extensions.el init file: extensions bundled in .emacs.d ----------

;; more load-path
(setq load-path `("~/.emacs.d/emacs-goodies-el"
                  "~/.emacs.d/predictive"
                  "~/.emacs.d/talcum"
                  "~/.emacs.d/haskell-mode"
                  ,@load-path))

;; ELPA package system
(when (load "~/.emacs.d/elpa/package.el")
  (package-initialize))


;; redo
(require 'redo)
(global-set-key (kbd "C-x U") 'redo)

;; better, patched Python mode
(require 'python-mode)

;; auto-completion setup
(require 'auto-complete)
(require 'auto-complete-python)
(setq ac-auto-start nil)
(setq ac-auto-start-chars '("."))
(add-hook 'python-mode-hook 'auto-complete-mode)


;; CVS haskell mode
(autoload 'haskell-mode "haskell-mode")
(autoload 'literate-haskell-mode "haskell-mode")
(add-to-list 'auto-mode-alist '("\\.\\(?:[gh]s\\|hi\\)\\'" . haskell-mode))
(add-to-list 'auto-mode-alist '("\\.l[gh]s\\'" . literate-haskell-mode))

(add-hook 'haskell-mode-hook (lambda ()
  (load-library "inf-haskell")
  (imenu-add-menubar-index)
  (turn-on-haskell-doc-mode)
  (turn-on-haskell-indent)
  (inferior-haskell-process)
  (setq comment-start "--")
  (add-to-list 'filladapt-token-table '("-- " haskell-comment))
  (add-to-list 'filladapt-token-match-table '(haskell-comment haskell-comment))
  (add-to-list 'filladapt-token-conversion-table '(haskell-comment . exact))
  (add-hook 'before-save-hook 'delete-trailing-whitespace nil t)))

;; highlight "return" as a builtin
(font-lock-add-keywords 'haskell-mode
 '(("\\<\\(return\\)\\>" 1 font-lock-builtin-face prepend)))

;; bar cursor
(require 'bar-cursor)
(bar-cursor-mode t)

;; twitter
;(require 'todochiku)
;(defun my-twit-todochiku ()
;  "Helper function for use by the todochiku package."
;  (todochiku-message "Twitter"
;                     (format "From %s:\n%s"
;                             (cadr twit-last-tweet)
;                             (caddr twit-last-tweet))
;                     (expand-file-name "~/.emacs.d/twittericon-48x48.png")))

(require 'twit)
(defun my-twit-knotify ()
  "Display a tweet notification via KNotify."
  (let* ((from (cadr twit-last-tweet))
         (body (caddr twit-last-tweet))
         ;; highlight #hashtags and @usernames
         (body (replace-regexp-in-string "\\(#[a-zA-Z_]*\\)"
                                         "<font color='#88f'>\\1</font>" body))
         (body (replace-regexp-in-string "\\(@[a-zA-Z_]*\\)"
                                         "<font color='#f88'>\\1</font>" body))
         (notif-id (dbus-call-method
                    :session "org.kde.knotify" "/Notify" "org.kde.KNotify"
                    "event" "test" "twit"
                    '(:array (:variant nil))
                    (format "Tweet from <b>%s</b>:<br>%s" from body)
                    '(:array :byte 0 :byte 0 :byte 0 :byte 0)
                    '(:array ) :int64 0)))
    (if (> notif-id 0)
        (run-with-timer 5 nil 'my-twit-knotify-close notif-id)))
  nil)
(defun my-twit-knotify-close (notif-id)
  "Close a tweet notification via KNotify."
  (dbus-call-method
   :session "org.kde.knotify" "/Notify" "org.kde.KNotify"
   "closeNotification" :int32 notif-id))
(add-hook 'twit-new-tweet-hook 'my-twit-knotify)

;; ReST mode
(autoload 'rst-mode "rst")
(setq auto-mode-alist
      (append '(("\\.rst$" . rst-mode)
                ("\\.rest$" . rst-mode)
                ("CHANGES$" . rst-mode)
                ("NEWS$" . rst-mode))
              auto-mode-alist))
(eval-after-load 'rst
  '(add-hook 'rst-mode-hook
             (lambda () (set-variable 'show-trailing-whitespace 1))))

;; tabbar
(require 'tabbar)
(global-set-key [C-prior] 'tabbar-backward)
(global-set-key [C-next] 'tabbar-forward)
;; kill buffer on left click, switch mode on right click
(defun my-tabbar-home-function (event)
  (let ((mouse-button (event-basic-type event)))
    (cond
     ((eq mouse-button 'mouse-3)
      (tabbar-buffer-show-groups (not tabbar--buffer-show-groups)))
     ((eq mouse-button 'mouse-1)
      (kill-buffer nil))
     )))
(add-hook 'tabbar-init-hook
          (lambda () (setq tabbar-home-function 'my-tabbar-home-function))
          t)  ; append
(tabbar-mode 1)
(tabbar-mwheel-mode 1)

;; replace recent character
(require 'rrc)

;; color-grep (automatic syncing between grep and source buffers)
(require 'color-grep)

;; C eldoc mode (automatic function signature tips)
(require 'c-eldoc)
(add-hook 'c-mode-hook 'c-turn-on-eldoc-mode)

;; color dabbrev-expanded phrases
(require 'dabbrev-highlight)

;; winpoint (remember point location by window)
(require 'winpoint)
(window-point-remember-mode 1)

;; bookmarks
(require 'bm)
(global-set-key (kbd "C-c b t") 'bm-toggle)
(global-set-key (kbd "C-c b n") 'bm-next)
(global-set-key (kbd "C-c b p") 'bm-previous)

;; show tabs
(require 'show-wspace)
(add-hook 'python-mode-hook 'highlight-tabs)

;; nxhtml
(load "~/.emacs.d/nxhtml/autostart.el")

;; talcum-mode: replaces LaTeX commands by Unicode symbols
(autoload 'talcum-mode "talcum")
(add-hook 'LaTeX-mode-hook 'talcum-mode)

;; highlight symbol at point
(require 'highlight-symbol)

;; highlight beyond fill column
(require 'highlight-beyond-fill-column)

;; Martin Blais' dubious paragraphs
(require 'dubious-paragraphs)
(global-set-key [(meta n)] 'dubious-forward-paragraph)
(global-set-key [(meta N)] 'dubious-forward-paragraph-scroll)
(global-set-key [(meta p)] 'dubious-backward-paragraph)
(global-set-key [(meta P)] 'dubious-backward-paragraph-scroll)

;; Martin Blais' repeatable macros
(require 'repeatable)
(repeatable-command-advice kmacro-end-and-call-macro)
(repeatable-substitute-binding 'search-for-this-word)
(repeatable-command-advice hl-symbol-and-jump)

;; mk-project: project management
(require 'mk-project)
(global-set-key (kbd "C-c p c") 'project-compile)
(global-set-key (kbd "C-c p g") 'project-grep)
(global-set-key (kbd "C-c p a") 'project-ack)
(global-set-key (kbd "C-c p l") 'project-load)
(global-set-key (kbd "C-c p u") 'project-unload)
(global-set-key (kbd "C-c p f") 'project-find-file)
(global-set-key (kbd "C-c p i") 'project-index)
(global-set-key (kbd "C-c p s") 'project-status)
(global-set-key (kbd "C-c p h") 'project-home)
(global-set-key (kbd "C-c p d") 'project-dired)
(global-set-key (kbd "C-c p t") 'project-tags)
