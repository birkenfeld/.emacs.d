;; ---------- extensions.el init file: extensions bundled in .emacs.d ----------

;; redo
(require 'redo)

;; better, patched Python mode
(require 'python-mode)

;; auto-completion setup
(require 'auto-complete)
(require 'auto-complete-python)
(setq ac-auto-start nil)
(setq ac-auto-start-chars '("."))
(global-auto-complete-mode t)

;; CVS haskell mode
(require 'haskell-mode)
(require 'inf-haskell)

(add-to-list 'auto-mode-alist '("\\.\\(?:[gh]s\\|hi\\)\\'" . haskell-mode))
(add-to-list 'auto-mode-alist '("\\.l[gh]s\\'" . literate-haskell-mode))

(add-hook 'haskell-mode-hook (lambda ()
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
(require 'todochiku)
(autoload 'twit-post "twit" "post to twitter" t)

;; ReST mode
(require 'rst)
(setq auto-mode-alist
      (append '(("\\.rst$" . rst-mode)
                ("\\.rest$" . rst-mode)
                ("CHANGES$" . rst-mode)
                ("NEWS$" . rst-mode))
              auto-mode-alist))
(add-hook 'rst-mode-hook
          (lambda () (set-variable 'show-trailing-whitespace 1)))

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

;; screen lines
(require 'screen-lines)

;; replace recent character
(require 'rrc)

;; color-grep (automatic syncing between grep and source buffers)
(require 'color-grep)

;; C eldoc mode (automatic function signature tips)
(load "c-eldoc")
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
(project-def "Sphinx"
      '((basedir "/home/gbr/devel/sphinx")
        (src-patterns ("*.py" "*.rst"))
        (ignore-patterns ("*.pyc" "*.pyo" "doc/_build/*"))
        (tags-file "/home/gbr/devel/sphinx/TAGS")
        (file-list-cache "/home/gbr/.emacs.d/file-cache-sphinx")
        (vcs hg)
        (compile-cmd "make")
        (startup-hook nil)
        (shutdown-hook nil)))

(global-set-key (kbd "C-c p c") 'project-compile)
(global-set-key (kbd "C-c p g") 'project-grep)
(global-set-key (kbd "C-c p l") 'project-load)
(global-set-key (kbd "C-c p u") 'project-unload)
(global-set-key (kbd "C-c p f") 'project-find-file)
(global-set-key (kbd "C-c p i") 'project-index)
(global-set-key (kbd "C-c p s") 'project-status)
(global-set-key (kbd "C-c p h") 'project-home)
(global-set-key (kbd "C-c p d") 'project-dired)
(global-set-key (kbd "C-c p t") 'project-tags)


;;; This was installed by package-install.el.
;;; This provides support for the package system and
;;; interfacing with ELPA, the package archive.
;;; Move this code earlier if you want to reference
;;; packages in your .emacs.
(when
    (load
     (expand-file-name "~/.emacs.d/elpa/package.el"))
  (package-initialize))
