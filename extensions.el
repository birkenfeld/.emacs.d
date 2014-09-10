;; ---------- extensions.el init file: extensions bundled in .emacs.d ----------

;; more load-path
(setq load-path `("~/.emacs.d/emacs-goodies-el"
                  "~/.emacs.d/talcum"
                  ,@load-path))

;; ---------- always enabled ---------------------------------------------------

;; make some mode line displays smaller
(when (require 'diminish nil 'noerror)
  (eval-after-load "reveal" '(diminish 'reveal-mode))
  (eval-after-load "yasnippet" '(diminish 'yas-minor-mode " Y")))

;; M-x enhancement
(require 'smex)
(smex-initialize)
(global-set-key (kbd "M-x") 'smex)
(global-set-key (kbd "M-X") 'smex-major-mode-commands)

;; go to last change in buffer
(require 'goto-chg)
(global-set-key (kbd "C-,") 'goto-last-change)

;; colored moccur
(require 'color-moccur)

;; wgrep (edit in grep results)
(require 'wgrep)

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

;; eproject: alternate project management
(require 'eproject)
(require 'eproject-extras)
(require 'eproject-gb)
(require 'eproject-compile)

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
(global-set-key (kbd "M-j") 'fastnav-jump-to-char-forward)
(global-set-key (kbd "M-J") 'fastnav-jump-to-char-backward)
(global-set-key (kbd "M-m") 'fastnav-mark-to-char-forward)
(global-set-key (kbd "M-M") 'fastnav-mark-to-char-backward)
(global-set-key (kbd "M-z") 'fastnav-zap-to-char-forward)
(global-set-key (kbd "M-Z") 'fastnav-zap-to-char-backward)

;; auto-completion setup
;(require 'pos-tip)
;(require 'auto-complete)
;; (add-to-list 'ac-dictionary-directories "~/.emacs.d/ac-dict")
;; (define-key ac-mode-map (kbd "C-c h") 'ac-last-quick-help)
;; (define-key ac-completing-map "\t" 'ac-complete)
;(require 'auto-complete-config)
;(ac-config-default)

;; highlight newly-inserted text etc
(require 'volatile-highlights)
(volatile-highlights-mode t)

;; ---------- autoloaded -------------------------------------------------------

;; mcstas
(autoload 'mcstas-mode "mcstas-mode" nil t)
(add-to-list 'auto-mode-alist '("\\.instr$" . mcstas-mode))
(add-to-list 'auto-mode-alist '("\\.comp$" . mcstas-mode))

;; redo
(autoload 'redo "redo" nil t)
(global-set-key (kbd "C-x U") 'redo)

;; python-mode stuff
(autoload 'cython-mode "cython-mode" nil t)

(setq auto-mode-alist
      (append '(("\\.pyx$" . cython-mode)
                ("\\.pxs$" . cython-mode)
                ("\\.pxi$" . cython-mode))
              auto-mode-alist))

;; jedi for python completion (done by elpy)
;(autoload 'jedi:setup "jedi" nil t)
;(add-hook 'python-mode-hook 'jedi:setup)

;; haskell mode
;(load "haskell-site-file" nil t)

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

;; C disaster mode (instant disassembly)
(autoload 'disaster "disaster" nil t)
(eval-after-load 'cc-mode
  '(progn
     (require 'disaster)
     (defun my-c-mode-common-hook ()
       (define-key c-mode-base-map (kbd "C-c C-d") 'disaster)
       (define-key c-mode-base-map (kbd "C-c C-c") 'compile))
     (add-hook 'c-mode-common-hook 'my-c-mode-common-hook)))

;; color dabbrev-expanded phrases
(require 'dabbrev-highlight)

;; bookmarks
(autoload 'bm-toggle "bm" nil t)
(autoload 'bm-next "bm" nil t)
(autoload 'bm-previous "bm" nil t)
(global-set-key (kbd "C-c b t") 'bm-toggle)
(global-set-key (kbd "C-c b n") 'bm-next)
(global-set-key (kbd "C-c b p") 'bm-previous)

;; web-mode for HTML/Jinja/CSS/JS
(autoload 'web-mode "web-mode" nil t)
(add-to-list 'auto-mode-alist '("\\.jsp\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.php\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.as[cp]x\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.erb\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.html?\\'" . web-mode))

;; talcum-mode: replaces LaTeX commands by Unicode symbols
(autoload 'talcum-mode "talcum" nil t)
(add-hook 'LaTeX-mode-hook 'talcum-mode)

;; highlight symbol at point
(require 'highlight-symbol)
(add-hook 'python-mode-hook 'highlight-symbol-mode)

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

;; rainbow-mode: color for color names
(require 'rainbow-mode)
(add-hook 'css-mode-hook 'rainbow-mode)

;; test-case-mode: add a nose backend
;(require 'test-case-mode)

;; ;; remove global dot that may have been added last session
;; (setq-default
;;  mode-line-format
;;  (mapcan (lambda (x)
;;            (unless (and (consp x)
;;                     (stringp (car x))
;;                     (eq 'test-case-dot-tooltip
;;                         (get-text-property 0 'help-echo (car x))))
;;              (list x)))
;;          (default-value 'mode-line-format)))

;; (global-set-key (kbd "<f9>") 'test-case-run-without-pdb)
;; (global-set-key (kbd "S-<f9>") 'test-case-run-with-pdb)

;; (defun test-case-run-without-pdb ()
;;   (interactive)
;;   (unless test-case-mode (test-case-mode 1))
;;   (set (make-local-variable 'test-case-nose-arguments) "-d")
;;   (test-case-run))

;; (defun test-case-run-with-pdb ()
;;   (interactive)
;;   (unless test-case-mode (test-case-mode 1))
;;   (set (make-local-variable 'test-case-nose-arguments) "--pdb --pdb-failures")
;;   (test-case-run))

;; (eval-after-load 'python-mode
;;   '(add-hook 'python-mode-hook 'enable-test-case-mode-if-test))

;; (defcustom test-case-nose-executable "nosetests"
;;   "*The nosetests executable."
;;   :group 'test-case :type 'file)
;; (defcustom test-case-nose-arguments "-d"
;;   "*The nosetests arguments."
;;   :group 'test-case :type 'string)
;; (defcustom test-cwd "."
;;   "*The directory from which to run nosetests. Should be set per-buffer."
;;   :group 'test-case :type 'file :safe 'stringp)
;; (defcustom test nil
;;   "*The test file to run instead of this file."
;;   :group 'test-case :type 'file :safe 'stringp)

;; (defvar test-case-nose-font-lock-keywords
;;   (eval-when-compile
;;     `((,(concat "\\_<\\(?:assert\\|raises\\)\\_>")
;;        (0 'test-case-assertion append)))))

;; (defun test-case-nose-failure-pattern ()
;;   (let ((file (regexp-quote (or test buffer-file-name))))
;;     (list (concat "  File \"\\(\\(" file "\\)\", line \\([0-9]+\\)\\).*\n"
;;                   "\\(?:  .*\n\\)*"
;;                   "\\([^ ].*\\)"
;;                   )
;;           2 3 nil nil 4)))

;; (defun test-case-nose-process-filter (proc string)
;;   "Filter to switch to comint-mode once Pdb is activated by nose."
;;   (let ((proc-buffer (process-buffer proc))
;;         (inhibit-read-only t))
;;     (with-current-buffer proc-buffer
;;       (insert string)
;;       (when (string-match "(Pdb.*) $" string)
;;         (toggle-read-only 0)
;;         (comint-mode)
;;         (set-process-filter proc 'comint-output-filter)
;;         (goto-char (point-max))
;;         (set-marker (process-mark proc) (point))
;;         ;; enable pdbtrack
;;         (when (fboundp 'py-pdbtrack-track-stack-file)
;;           (add-hook 'comint-output-filter-functions 'py-pdbtrack-track-stack-file)
;;           (setq py-pdbtrack-do-tracking-p t))
;;         ;; show a backtrace
;;         (insert "bt")
;;         (ignore-errors (comint-send-input))
;;         ;; and switch to Pdb buffer
;;         (pop-to-buffer proc-buffer)))))

;; (defun test-case-nose-backend (command)
;;   "Python nose back-end for `test-case-mode'."
;;   (case command
;;     ('name "Nose")
;;     ('supported (derived-mode-p 'python-mode))
;;     ('command (concat "cd " test-cwd "; " test-case-nose-executable " "
;;                       test-case-nose-arguments " " (or test buffer-file-name)))
;;     ('run-hook
;;      (set-process-filter (get-buffer-process (current-buffer))
;;                          'test-case-nose-process-filter))
;;     ('save t)
;;     ('failure-pattern (test-case-nose-failure-pattern))
;;     ('font-lock-keywords test-case-nose-font-lock-keywords)))

;; (add-to-list 'test-case-backends 'test-case-nose-backend t)

;; grep in version controlled files
(defconst hg-tools-grep-command
  "hg locate --print0 | xargs -0 grep -In %s"
  "The command used for grepping files using hg. See `hg-tools-grep'.")

(defmacro hg-tools-at-branch-root (dirname &rest code)
  `(let ((default-directory (locate-dominating-file (expand-file-name ,dirname) ".hg"))) ,@code))

(defun hg-tools-grep (expression dirname)
  "Search a branch for `expression'. If there's a C-u prefix, prompt for `dirname'."
  (interactive
   (let* ((string (read-string "Search for: "))
          (dir (if (null current-prefix-arg)
                   default-directory
                 (read-directory-name (format "Search for %s in: " string)))))
     (list string dir)))
  (hg-tools-at-branch-root dirname
   (grep-find (format hg-tools-grep-command (shell-quote-argument expression)))))

(eval-after-load "anything"
  '(define-key anything-map (kbd "M-RET") 'anything-execute-persistent-action))

(eval-after-load "ido"
  (setq ido-decorations (quote ("{" "}" ", " ", ..." " [" "]"
                                " [No match]" " [Matched]" " [Not readable]" " [Too big]" " [Confirm]")))

  ;; Display ido results vertically, rather than horizontally
;  (setq ido-decorations (quote ("\n-> " "" "\t" "\n   ..." " [" "]"
;                                " [No match]" " [Matched]" " [Not readable]" " [Too big]" " [Confirm]")))
;  (defun ido-disable-line-truncation () (set (make-local-variable 'truncate-lines) nil))
;  (add-hook 'ido-minibuffer-setup-hook 'ido-disable-line-truncation)
;  (defun ido-define-keys () ;; C-n/p is more intuitive in vertical layout
;    (define-key ido-completion-map (kbd "C-n") 'ido-next-match)
;    (define-key ido-completion-map (kbd "C-p") 'ido-prev-match))
                                        ;  (add-hook 'ido-setup-hook 'ido-define-keys)
  )
