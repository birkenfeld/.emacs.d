;; ---------- extensions.el init file: extensions bundled in .emacs.d ----------

;; more load-path
(setq load-path `("~/.emacs.d/emacs-goodies-el"
                  "~/.emacs.d/predictive"
                  "~/.emacs.d/talcum"
                  "~/.emacs.d/haskell-mode"
                  ,@load-path))

;; ELPA package system
;(when (load "~/.emacs.d/elpa/package.el")
;  (package-initialize))

;; ---------- always enabled ---------------------------------------------------

;; make some mode line displays smaller
(when (require 'diminish nil 'noerror)
  (eval-after-load "reveal" '(diminish 'reveal-mode))
  (eval-after-load "yasnippet" '(diminish 'yas/minor-mode " Y")))

;; bar cursor
(require 'bar-cursor)
(bar-cursor-mode t)

;; colored moccur
(require 'color-moccur)

;; grep-edit (edit in grep results)
(require 'grep-edit)
;; need to unset ordinary character bindings...
(loop for key in '("n" "p" "{" "}" " ") do
      (define-key grep-mode-map key nil))

;; session (saves histories, variables, ...)
(require 'session)
(session-initialize)

;; adaptive fill
(require 'filladapt)
(setq-default filladapt-mode t)

;; browse-kill-ring
(require 'browse-kill-ring)
(browse-kill-ring-default-keybindings)
(defadvice yank-pop (around kill-ring-browse-maybe (arg) activate)
  "If last action was not a yank, run `browse-kill-ring' instead."
  (if (not (eq last-command 'yank))
      (browse-kill-ring)
    ad-do-it))

;; display flymake errors in modeline
(require 'flymake-cursor)

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
;(tabbar-mode 1)
;(tabbar-mwheel-mode 1)

;; replace recent character
(require 'rrc)

;; winpoint (remember point location by window)
(require 'winpoint)
(window-point-remember-mode 1)

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
(repeatable-command-advice next-error)
(repeatable-command-advice previous-error)

;; mk-project: project management
;(require 'mk-project)
;(global-set-key (kbd "C-c p c") 'project-compile)
;(global-set-key (kbd "C-c p g") 'project-grep)
;(global-set-key (kbd "C-c p a") 'project-ack)
;(global-set-key (kbd "C-c p l") 'project-load)
;(global-set-key (kbd "C-c p u") 'project-unload)
;(global-set-key (kbd "C-c p f") 'project-find-file-ido)
;(global-set-key (kbd "C-c p i") 'project-index)
;(global-set-key (kbd "C-c p s") 'project-status)
;(global-set-key (kbd "C-c p h") 'project-home)
;(global-set-key (kbd "C-c p d") 'project-dired)
;(global-set-key (kbd "C-c p t") 'project-tags)

;; eproject: alternate project management
(require 'eproject)
(require 'eproject-extras)
(require 'eproject-gb)

(global-set-key (kbd "C-c p f") 'eproject-find-file)
(global-set-key (kbd "C-c p a") 'eproject-ack)
(global-set-key (kbd "C-c p g") 'eproject-grep)
(global-set-key (kbd "C-c p i") 'eproject-ibuffer)
(global-set-key (kbd "C-c p t") 'eproject-todo)
(global-set-key (kbd "C-c p d") 'eproject-revisit-project)
(global-set-key (kbd "C-c p k") 'eproject-kill-project-buffers)
(global-set-key (kbd "C-c p e") 'eproject-test)

;; fastnav
(require 'fastnav)
(global-set-key (kbd "M-s") 'jump-to-char-forward)
(global-set-key (kbd "M-S") 'jump-to-char-backward)
(global-set-key (kbd "M-m") 'mark-to-char-forward)
(global-set-key (kbd "M-M") 'mark-to-char-backward)

;; auto-completion setup
(require 'pos-tip)
(require 'auto-complete)
(add-to-list 'ac-dictionary-directories "~/.emacs.d/ac-dict")
(define-key ac-mode-map (kbd "C-c h") 'ac-last-quick-help)
(define-key ac-completing-map "\t" 'ac-complete)
(require 'auto-complete-config)
(ac-config-default)

;; ---------- autoloaded -------------------------------------------------------

;; redo
(autoload 'redo "redo" nil t)
(global-set-key (kbd "C-x U") 'redo)

;; better, patched Python mode
(autoload 'python-mode "python-mode" nil t)

(defun py-backward-kill-nomenclature (arg)
  "Kill characters forward until encountering the end of a nomenclature section."
  (interactive "p")
  (kill-region (point) (progn (py-backward-into-nomenclature arg) (point))))


(eval-after-load 'python-mode
  '(progn
     (require 'auto-complete-python)
     (setup-ropemacs)
     (add-hook 'python-mode-hook 'ac-python-mode-setup)
     (define-key py-mode-map (kbd ".") 'ac-self-insert-and-complete)
     ))

;; show tabs
(eval-after-load 'python-mode
  '(progn
     (require 'show-wspace)
     (add-hook 'python-mode-hook 'highlight-tabs)))


;; haskell mode
(load "haskell-site-file" nil t)

(add-hook 'haskell-mode-hook (lambda ()
  (load-library "inf-haskell")
  (imenu-add-menubar-index)
  (turn-on-haskell-doc-mode)
  (turn-on-haskell-indentation)
  (inferior-haskell-process)
  (setq comment-start "--")
  (add-to-list 'filladapt-token-table '("-- " haskell-comment))
  (add-to-list 'filladapt-token-match-table '(haskell-comment haskell-comment))
  (add-to-list 'filladapt-token-conversion-table '(haskell-comment . exact))
  (add-hook 'before-save-hook 'delete-trailing-whitespace nil t)))

;; highlight "return" as a builtin
(font-lock-add-keywords 'haskell-mode
 '(("\\<\\(return\\)\\>" 1 font-lock-builtin-face prepend)))

;; twitter
;(require 'twit)
;(defun my-twit-knotify ()
;  "Display a tweet notification via KNotify."
;  (let* ((from (cadr twit-last-tweet))
;         (body (caddr twit-last-tweet))
;         ;; highlight #hashtags and @usernames
;         (body (replace-regexp-in-string "\\(#[a-zA-Z_]*\\)"
;                                         "<font color='#88f'>\\1</font>" body))
;         (body (replace-regexp-in-string "\\(@[a-zA-Z_]*\\)"
;                                         "<font color='#f88'>\\1</font>" body))
;         (notif-id (dbus-call-method
;                    :session "org.kde.knotify" "/Notify" "org.kde.KNotify"
;                    "event" "test" "twit"
;                    '(:array (:variant nil))
;                    (format "Tweet from <b>%s</b>:<br>%s" from body)
;                    '(:array :byte 0 :byte 0 :byte 0 :byte 0)
;                    '(:array ) :int64 0)))
;    (if (> notif-id 0)
;        (run-with-timer 5 nil 'my-twit-knotify-close notif-id)))
;  nil)
;(defun my-twit-knotify-close (notif-id)
;  "Close a tweet notification via KNotify."
;  (dbus-call-method
;   :session "org.kde.knotify" "/Notify" "org.kde.KNotify"
;   "closeNotification" :int32 notif-id))
;(add-hook 'twit-new-tweet-hook 'my-twit-knotify)

;; ReST mode
(autoload 'rst-mode "rst" nil t)
(setq auto-mode-alist
      (append '(("\\.rst$" . rst-mode)
                ("\\.rest$" . rst-mode)
                ("CHANGES$" . rst-mode)
                ("NEWS$" . rst-mode))
              auto-mode-alist))
(eval-after-load 'rst
  '(add-hook 'rst-mode-hook
             (lambda () (set-variable 'show-trailing-whitespace 1))))

;; fixes and enhancements for po-mode
(eval-after-load 'po-mode '(require 'gb-po-mode))

;; color-grep (automatic syncing between grep and source buffers)
;(require 'color-grep)

;; C eldoc mode (automatic function signature tips)
(autoload 'c-turn-on-eldoc-mode "c-eldoc" nil t)
(add-hook 'c-mode-hook 'c-turn-on-eldoc-mode)

;; color dabbrev-expanded phrases
(require 'dabbrev-highlight)

;; bookmarks
(autoload 'bm-toggle "bm" nil t)
(autoload 'bm-next "bm" nil t)
(autoload 'bm-previous "bm" nil t)
(global-set-key (kbd "C-c b t") 'bm-toggle)
(global-set-key (kbd "C-c b n") 'bm-next)
(global-set-key (kbd "C-c b p") 'bm-previous)

;; nxhtml
(load "~/.emacs.d/nxhtml/autostart.el")

;; talcum-mode: replaces LaTeX commands by Unicode symbols
(autoload 'talcum-mode "talcum" nil t)
(add-hook 'LaTeX-mode-hook 'talcum-mode)

;; highlight symbol at point
(require 'highlight-symbol)
(add-hook 'python-mode-hook 'highlight-symbol-mode)
(defun highlight-symbol-next-in-function ()
  (interactive)
  (if (eq major-mode 'python-mode)
      (save-restriction
        (save-excursion
          (py-beginning-of-def-or-class)
          (py-narrow-to-defun))
        (highlight-symbol-jump 1))
    (highlight-symbol-next-in-defun)))
(defun highlight-symbol-prev-in-function ()
  (interactive)
  (if (eq major-mode 'python-mode)
      (save-restriction
        (save-excursion
          (py-beginning-of-def-or-class)
          (py-narrow-to-defun))
        (highlight-symbol-jump -1))
    (highlight-symbol-prev-in-defun)))
(global-set-key (kbd "M-g M-m") 'highlight-symbol-next-in-function)
(global-set-key (kbd "M-g M-o") 'highlight-symbol-prev-in-function)
(repeatable-command-advice highlight-symbol-next-in-function)
(repeatable-command-advice highlight-symbol-prev-in-function)

;; highlight beyond fill column
(require 'highlight-beyond-fill-column)

;; full-ack mode
(autoload 'ack-same "full-ack" nil t)
(autoload 'ack "full-ack" nil t)
(global-set-key (kbd "<f2>") 'ack-same)
(global-set-key (kbd "S-<f2>") 'ack)

;; idomenu: switch to a buffer local tag with ido completion
(autoload 'idomenu "idomenu" nil t)
(global-set-key (kbd "C-x m") 'idomenu)

;; xdict: lookup dictionary
(autoload 'xdict-query "x-dict" nil t)
(global-set-key (kbd "C-c d") 'xdict-query)
(eval-after-load 'x-dict
  '(add-hook 'xdict-mode-hook (lambda () (setq truncate-lines t))))

;; shell-pop: easy pop-up of a shell buffer
(require 'shell-pop)
(shell-pop-set-internal-mode "ansi-term")
(shell-pop-set-internal-mode-shell "/bin/zsh")
(global-set-key (kbd "<menu>") 'shell-pop)
(setq term-term-name "eterm-color")
;; this helps with a bug in ansi-term when output lines are longer than
;; the terminal width
(defun turn-off-truncate-lines ()
  (setq truncate-lines nil
        word-wrap t))
(add-hook 'term-mode-hook 'turn-off-truncate-lines)

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

;; iedit: interactively edit all occurrences, useful from isearch
;(autoload 'iedit-mode "iedit" nil t)
;(global-set-key (kbd "C-x C-i") 'iedit-mode)

;; rainbow-mode: color for color names
(require 'rainbow-mode)
(add-hook 'css-mode-hook 'rainbow-mode)

;; test-case-mode: add a nose backend
(require 'test-case-mode)

;; remove global dot that may have been added last session
(setq-default
 mode-line-format
 (mapcan (lambda (x)
           (unless (and (consp x)
                    (stringp (car x))
                    (eq 'test-case-dot-tooltip
                        (get-text-property 0 'help-echo (car x))))
             (list x)))
         (default-value 'mode-line-format)))

(global-set-key (kbd "<f9>") 'test-case-run-without-pdb)
(global-set-key (kbd "S-<f9>") 'test-case-run-with-pdb)

(defun test-case-run-without-pdb ()
  (interactive)
  (unless test-case-mode (test-case-mode 1))
  (set (make-local-variable 'test-case-nose-arguments) "-d")
  (test-case-run))

(defun test-case-run-with-pdb ()
  (interactive)
  (unless test-case-mode (test-case-mode 1))
  (set (make-local-variable 'test-case-nose-arguments) "--pdb --pdb-failures")
  (test-case-run))

(eval-after-load 'python-mode
  '(add-hook 'python-mode-hook 'enable-test-case-mode-if-test))

(defcustom test-case-nose-executable "nosetests"
  "*The nosetests executable."
  :group 'test-case :type 'file)
(defcustom test-case-nose-arguments "-d"
  "*The nosetests arguments."
  :group 'test-case :type 'string)
(defcustom test-cwd "."
  "*The directory from which to run nosetests. Should be set per-buffer."
  :group 'test-case :type 'file :safe 'stringp)
(defcustom test nil
  "*The test file to run instead of this file."
  :group 'test-case :type 'file :safe 'stringp)

(defvar test-case-nose-font-lock-keywords
  (eval-when-compile
    `((,(concat "\\_<\\(?:assert\\|raises\\)\\_>")
       (0 'test-case-assertion append)))))

(defun test-case-nose-failure-pattern ()
  (let ((file (regexp-quote (or test buffer-file-name))))
    (list (concat "  File \"\\(\\(" file "\\)\", line \\([0-9]+\\)\\).*\n"
                  "\\(?:  .*\n\\)*"
                  "\\([^ ].*\\)"
                  )
          2 3 nil nil 4)))

(defun test-case-nose-process-filter (proc string)
  "Filter to switch to comint-mode once Pdb is activated by nose."
  (let ((proc-buffer (process-buffer proc))
        (inhibit-read-only t))
    (with-current-buffer proc-buffer
      (insert string)
      (when (string-match "(Pdb.*) $" string)
        (toggle-read-only 0)
        (comint-mode)
        (set-process-filter proc 'comint-output-filter)
        (goto-char (point-max))
        (set-marker (process-mark proc) (point))
        ;; enable pdbtrack
        (when (fboundp 'py-pdbtrack-track-stack-file)
          (add-hook 'comint-output-filter-functions 'py-pdbtrack-track-stack-file)
          (setq py-pdbtrack-do-tracking-p t))
        ;; show a backtrace
        (insert "bt")
        (ignore-errors (comint-send-input))
        ;; and switch to Pdb buffer
        (pop-to-buffer proc-buffer)))))

(defun test-case-nose-backend (command)
  "Python nose back-end for `test-case-mode'."
  (case command
    ('name "Nose")
    ('supported (derived-mode-p 'python-mode))
    ('command (concat "cd " test-cwd "; " test-case-nose-executable " "
                      test-case-nose-arguments " " (or test buffer-file-name)))
    ('run-hook
     (set-process-filter (get-buffer-process (current-buffer))
                         'test-case-nose-process-filter))
    ('save t)
    ('failure-pattern (test-case-nose-failure-pattern))
    ('font-lock-keywords test-case-nose-font-lock-keywords)))

(add-to-list 'test-case-backends 'test-case-nose-backend t)
