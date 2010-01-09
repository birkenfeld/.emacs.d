;; ---------- .emacs.d/init.el customization file ------------------------------

;; This file should load with every stock Emacs 22 and higher.
;; Extensions only present in my .emacs.d are loaded in extensions.el.
;; Extensions I install via my distribution are loaded in distext.el.

;; set up load path
(setq load-path `("~/.emacs.d" ,@load-path))

;; don't show so many messages on startup
(setq inhibit-startup-message t)
(setq inhibit-startup-echo-area-message t)

;; Enable the Emacs server, allows thin editing sessions via emacsclient
(require 'server)
(unless (server-running-p) (server-start))

;; Load custom variables and fonts
(setq custom-file "~/.emacs.d/custom.el")
(load custom-file 'noerror)


;; ---------- Some influential variables ---------------------------------------

;; set background color
(set-background-color "gray97")

;; scroll one line at a time
(setq scroll-step 1)

;; make "yes or no" "y or n"
(fset 'yes-or-no-p 'y-or-n-p)

;; window frame title
(setq frame-title-format "emacs [%b %*%+ %f]")
(setq icon-title-format "emacs [%b]")

;; no bells please
(setq ring-bell-function 'ignore)

;; prefer UTF-8 coding system
(prefer-coding-system 'utf-8)

;; make all backups in a single directory
(setq backup-directory-alist
      `(("." . ,(expand-file-name "~/.emacs.d/backups"))))

;; use a nicer mouse scroll behavior
(setq mouse-wheel-scroll-amount '(1 ((shift) . 1) ((control))))

;; enable otherwise disabled commands
(put 'set-goal-column  'disabled nil)
(put 'downcase-region  'disabled nil)
(put 'upcase-region    'disabled nil)
(put 'narrow-to-region 'disabled nil)
(put 'narrow-to-page   'disabled nil)

;; remove trailing whitespaces before saving (now local)
;(add-hook 'before-save-hook 'delete-trailing-whitespace)
;; update copyright headers before saving
(add-hook 'before-save-hook 'copyright-update)
;; update timestamp ("last modified") before saving
(setq time-stamp-pattern "10/[Ll]ast modified: %:y-%02m-%02d %02H:%02M by %u$")
(add-hook 'before-save-hook 'time-stamp)

;; fetch semantic tags after saving (now local)
;(add-hook 'after-save-hook 'semantic-fetch-tags)
;; make file executable if it's a script
(add-hook 'after-save-hook 'executable-make-buffer-file-executable-if-script-p)

;; useful hippie-expand functions
(setq hippie-expand-try-functions-list
      '(try-complete-file-name-partially
        try-complete-file-name
        try-expand-all-abbrevs
        ;try-expand-list
        ;try-expand-line
        try-expand-dabbrev
        try-expand-dabbrev-all-buffers
        try-expand-dabbrev-from-kill
        try-complete-lisp-symbol-partially
        try-complete-lisp-symbol))


;; ---------- Keybindings ------------------------------------------------------

;; enable "find file at point", but don't try to find URLs
(ffap-bindings)
(setq ffap-url-regexp nil)

;; support file and lineno finding with "filename:linenum"
(global-set-key (kbd "C-x C-f") 'find-file-with-linenum)

;; shortcut for reverting a buffer
(global-set-key (kbd "C-x C-r") 'revert-buffer)

;; display same buffer in other window too
(global-set-key (kbd "C-x C-o") 'clone-indirect-buffer-other-window)

;; indent automatically
(global-set-key (kbd "RET") 'newline-and-indent)

;; file cache
(define-key minibuffer-local-map (kbd "C-f") 'file-cache-minibuffer-complete)

;; in the minibuffer, do not kill but delete
(define-key minibuffer-local-map (kbd "M-DEL") 'backward-delete-word)

;; useful mouse behavior
(global-set-key (kbd "<left-fringe> <down-mouse-1>") 'mouse-drag-region)
(global-set-key (kbd "<s-mouse-1>") 'mouse-delete-other-windows)
(global-set-key (kbd "<s-mouse-3>") 'mouse-delete-window)

;; get a buffer menu with the right mouse button.
(global-set-key (kbd "<mouse-3>") 'mouse-buffer-menu)

;; split window into 3
(global-set-key (kbd "C-x 7") 'split-window-horizontally-into-3)

;; windmove: easily move between windows
(global-set-key (kbd "C-x <left>")  'windmove-left)
(global-set-key (kbd "C-x <right>") 'windmove-right)
(global-set-key (kbd "C-x <up>")    'windmove-up)
(global-set-key (kbd "C-x <down>")  'windmove-down)

;; custom margin keys (useful for Python indentation)
(global-set-key (kbd "C-M-+") 'increase-left-margin)
(global-set-key (kbd "C-M--") 'decrease-left-margin)

;; use regex searches by default
(global-set-key (kbd "C-s")   'isearch-forward-regexp)
(global-set-key (kbd "C-r")   'isearch-backward-regexp)
(global-set-key (kbd "C-M-s") 'isearch-forward)
(global-set-key (kbd "C-M-r") 'isearch-backward)

;; compile/make
(global-set-key (kbd "<f5>")   'recompile)
(global-set-key (kbd "S-<f5>") 'compile)

;; C-k is kill-whole-line
(global-set-key (kbd "C-k") 'kill-whole-line)

;; shortcuts for killing buffers
(global-set-key (kbd "C-x k")   'kill-this-buffer)
(global-set-key (kbd "C-x K")   'kill-other-buffer)
(global-set-key (kbd "C-x C-k") 'kill-buffer-and-window)

;; align code in a pretty way
(global-set-key (kbd "C-x \\") 'align-regexp)

;; F6 stores a position in a file, F7 brings you back to this position
(global-set-key (kbd "<f6>") '(lambda () (interactive) (point-to-register ?1)))
(global-set-key (kbd "<f7>") '(lambda () (interactive) (register-to-point ?1)))

;; sometimes it's useful to re-highlight the whole buffer
(global-set-key (kbd "<f8>") 'font-lock-fontify-buffer)

;; switch menu-bar on/off
(global-set-key (kbd "<f10>") 'menu-bar-mode)

;; fullscreen editing
(global-set-key (kbd "<f11>") 'fullscreen)

;; quickly compare two windows with almost same content
(global-set-key (kbd "<f12>") 'compare-windows)

;; Alt-space expands
(global-set-key (kbd "M-SPC") 'hippie-expand)

;; make commenting easy; uncommenting with prefix arg
(global-set-key (kbd "C-#") 'comment-region)
(global-set-key (kbd "M-#") 'comment-region)

;; toggle line numer display
(global-set-key (kbd "C-c n") 'global-linum-mode)

;; global easy shortcut for occur-mode
(global-set-key (kbd "C-c o") 'occur)

;; since M-s o isn't needed for occur, use M-s directly for word search
(global-set-key (kbd "M-s") 'isearch-forward-word)

;; scroll without moving cursor
(global-set-key (kbd "C-M-<up>") '(lambda () (interactive) (scroll-down 1)))
(global-set-key (kbd "C-M-<down>") '(lambda () (interactive) (scroll-up 1)))
(global-set-key (kbd "M-<up>") '(lambda () (interactive) (scroll-down 5)))
(global-set-key (kbd "M-<down>") '(lambda () (interactive) (scroll-up 5)))

;; like vim's '*' binding
(global-set-key (kbd "C-+")   'search-for-this-word)
(global-set-key (kbd "C-x *") 'search-for-this-word)
(global-set-key (kbd "C-*")   'isearch-lazy-highlight-cleanup)

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

;; repeat simple and complex commands
(global-set-key (kbd "C-.") 'repeat)

;; find everything with apropos
(global-set-key (kbd "C-h a") 'apropos)

;; flymake error finding, similar to goto-next-error
(global-set-key (kbd "M-g e") 'flymake-goto-next-error)
(global-set-key (kbd "M-g M-e") 'flymake-goto-next-error)

;; support back and forward mouse in Info and help
(eval-after-load 'help-mode
  '(progn
     (define-key help-mode-map (kbd "<mouse-8>") 'help-go-back)
     (define-key help-mode-map (kbd "<mouse-9>") 'help-go-forward)))
(eval-after-load 'info
  '(progn
     (define-key Info-mode-map (kbd "<mouse-8>") 'Info-history-back)
     (define-key Info-mode-map (kbd "<mouse-9>") 'Info-history-forward)))

;; for console mode
;(global-set-key (kbd "C-M-d") 'backward-kill-word)
;(global-set-key (kbd "C-d") 'backward-delete-char-untabify)


;; ---------- Modes ------------------------------------------------------------

;; auto-fill in text mode
(add-hook 'text-mode-hook 'auto-fill-mode)

;; nice config file modes
(require 'generic-x)

;; electric bindings for help mode
(require 'ehelp)

;; terminal mode: display gray color a bit darker
(require 'term)
(setq ansi-term-color-vector [unspecified "black" "red3" "green3" "yellow3"
                                          "blue2" "magenta3" "cyan3" "gray80"])

;; abbrev file for abbrev-mode
(setq abbrev-file-name "~/.emacs.d/abbrevs")
(when (file-exists-p abbrev-file-name)
  (read-abbrev-file abbrev-file-name t))

;; enable wdired, editing filenames in dired renames files
(require 'wdired)
(define-key dired-mode-map (kbd "r") 'wdired-change-to-wdired-mode)

;; highlight XXX style code tags in source files
(font-lock-add-keywords 'python-mode
 '(("\\<\\(FIXME\\|HACK\\|XXX\\|TODO\\):?" 1 font-lock-warning-face prepend)))
(font-lock-add-keywords 'haskell-mode
 '(("\\<\\(FIXME\\|HACK\\|XXX\\|TODO\\):?" 1 font-lock-warning-face prepend)))
(font-lock-add-keywords 'c-mode
 '(("\\<\\(FIXME\\|HACK\\|XXX\\|TODO\\):?" 1 font-lock-warning-face prepend)))
(font-lock-add-keywords 'latex-mode
 '(("\\<\\(FIXME\\|HACK\\|XXX\\|TODO\\):?" 1 font-lock-warning-face prepend)))

;; ignore alltt environments in spell checking
(eval-after-load 'ispell
  '(let ((list (cadr ispell-tex-skip-alists)))
     (add-to-list 'list '("alltt" . "\\\\end[ \t\n]*{[ \t\n]*alltt[ \t\n]*}"))
     (setcdr ispell-tex-skip-alists (list list))))

;; org-mode (organizer, agenda, ...)
;(require 'org)
;(global-set-key (kbd "<f12>") 'org-agenda)

;; nice xterm mouse handling
;(xterm-mouse-mode t)

;; move mouse out of the way, not needed in Emacs 23.2+
;(mouse-avoidance-mode 'exile)


;; ---------- Mode-specific keybindings ----------------------------------------

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

;; next/previous match in occur
(define-key occur-mode-map "n" 'next-error-no-select)
(define-key occur-mode-map "p" 'previous-error-no-select)

;; close HTML tags with C-t in sgml mode
(add-hook 'sgml-mode-hook
	  (lambda () (local-set-key (kbd "C-t") 'sgml-close-tag)))

;; more powerful tab-completion in minibuffer
(add-hook 'minibuffer-setup-hook
          '(lambda ()
             (define-key minibuffer-local-map "\t" 'comint-dynamic-complete)))


;; ---------- C mode specifics -------------------------------------------------

;; C style used in new Python source files
(c-add-style
 "python-new"
 '((indent-tabs-mode . nil)
   (fill-column      . 78)
   (c-basic-offset   . 4)
   (c-offsets-alist  . ((substatement-open . 0)
                        (inextern-lang . 0)
                        (arglist-intro . +)
                        (knr-argdecl-intro . +)))
   (c-hanging-braces-alist . ((brace-list-open)
                              (brace-list-intro)
                              (brace-list-close)
                              (brace-entry-open)
                              (substatement-open after)
                              (block-close . c-snug-do-while)))
   (c-block-comment-prefix . "* ")))

;; Style used mostly for JavaScript
(c-add-style
 "javascript"
 '((indent-tabs-mode . nil)
   (fill-column      . 78)
   (c-basic-offset   . 2)
   (c-offsets-alist  . ((substatement-open . 0)
                        (inextern-lang . 0)
                        (arglist-intro . +)
                        (knr-argdecl-intro . +)))
   (c-hanging-braces-alist . ((brace-list-open)
                              (brace-list-intro)
                              (brace-list-close)
                              (brace-entry-open)
                              (substatement-open after)
                              (block-close . c-snug-do-while)))
   (c-block-comment-prefix . "")))

(add-to-list 'c-default-style '(c-mode . "python-new"))
(add-to-list 'c-default-style '(ecmascript-mode . "javascript"))

(defun c-select-style ()
  "Hack: Select the C style to use from buffer indentation."
  (save-excursion
    (if (re-search-forward "^\t" 3000 t)
        (c-set-style "python")
      (c-set-style "python-new"))))

(add-hook 'c-mode-hook 'c-select-style)
;; c-subword-mode became subword-mode in Emacs 23.2
(add-hook 'c-mode-hook (lambda ()
  (if (fboundp 'subword-mode) (subword-mode) (c-subword-mode))))


;; ---------- Python mode specifics --------------------------------------------

;; support flymake in Python mode
(require 'flymake)

(defun flymake-pyflakes-init ()
  (let* ((temp-file (flymake-init-create-temp-buffer-copy
                     'flymake-create-temp-inplace))
         (local-file (file-relative-name
                      temp-file
                      (file-name-directory buffer-file-name))))
    (list "pyflakes" (list local-file))))

(add-to-list 'flymake-allowed-file-name-masks
             '("\\.py\\'" flymake-pyflakes-init))

;; textmate bindings
(require 'textmate)

(add-hook 'python-mode-hook (lambda ()
  (textmate-mode 1)
  ;; reveal hidden text (folding!) when moving over it
  (reveal-mode 1)
  ;; enable flymake processing by pyflakes
  (if buffer-file-name
      (flymake-mode 1))
  ;; death to trailing whitespace!
  (set-variable 'show-trailing-whitespace 1)

  ;; some custom keybindings
  (local-set-key (kbd "C-c a") 'py-beginning-of-def-or-class)
  (local-set-key (kbd "M-<right>") 'py-forward-into-nomenclature)
  (local-set-key (kbd "M-<left>") 'py-backward-into-nomenclature)
  (local-set-key (kbd "M-DEL") 'py-backward-kill-nomenclature)

  ;; add some local hooks
  ;(add-hook 'before-save-hook 'delete-trailing-whitespace nil t)
  (add-hook 'after-save-hook 'semantic-fetch-tags nil t)

  ;; compile (<f5>) is execute
  (unless (or (file-exists-p "makefile")
              (file-exists-p "Makefile"))
    (set (make-local-variable 'compile-command)
         (concat "python " buffer-file-name)))
))


;; ---------- Haskell mode specifics -------------------------------------------

;; support flymake in Haskell mode

(defun flymake-Haskell-init ()
  (flymake-simple-make-init-impl
   'flymake-create-temp-with-folder-structure nil nil
   (file-name-nondirectory buffer-file-name)
   'flymake-get-Haskell-cmdline))

(defun flymake-get-Haskell-cmdline (source base-dir)
  (list "flycheck_haskell.pl"
        (list source base-dir)))

(push '(".+\\.hs$" flymake-Haskell-init flymake-simple-java-cleanup)
      flymake-allowed-file-name-masks)
(push '(".+\\.lhs$" flymake-Haskell-init flymake-simple-java-cleanup)
      flymake-allowed-file-name-masks)
(push
 '("^\\(\.+\.hs\\|\.lhs\\):\\([0-9]+\\):\\([0-9]+\\):\\(.+\\)"
   1 2 3 4) flymake-err-line-patterns)

;; ---------- Mercurial SMerge mode support ------------------------------------

;; Enters SMerge mode if it finds a file with conflicts in a merge, and
;; calls hg resolve on saving if all conflict markers have been removed.
;; Mostly from vc-hg.el and vc-svn.el.

(defun vc-hg-has-two-parents (file)
  "Hg-specific version of `vc-working-revision'."
  (let*
      ((status nil)
       (out
	(with-output-to-string
	  (with-current-buffer
	      standard-output
	    (setq status
		  (condition-case nil
		      ;; Ignore all errors.
		      (call-process
		       "hg" nil t nil "--cwd" (file-name-directory file)
		       "parents" "--template" "x")
		    ;; Some problem happened.  E.g. We can't find an `hg'
		    ;; executable.
		    (error nil)))))))
    (and (equal 0 status)
         (equal out "xx"))))

(defun vc-hg-resolve-when-done ()
  "Call \"hg resolve -m\" if the conflict markers have been removed."
  (save-excursion
    (goto-char (point-min))
    (unless (re-search-forward "^<<<<<<< local" nil t)
      (vc-hg-command nil 0 buffer-file-name "resolve" "-m")
      ;; Remove the hook so that it is not called multiple times.
      (remove-hook 'after-save-hook 'vc-hg-resolve-when-done t))))

(defun vc-hg-find-file-hook ()
  (when (vc-hg-has-two-parents buffer-file-name))
    ;; If a merge has occurred, then we should try and highlight conflicts.
    (when (save-excursion
            (goto-char (point-min))
            (re-search-forward "^<<<<<<< local" nil t))
      ;; There are conflict markers.
      (smerge-start-session)
      (add-hook 'after-save-hook 'vc-hg-resolve-when-done nil t)
      (message "There are unresolved conflicts in this file.")))


;; ---------- Custom interactive functions -------------------------------------

(defun backward-delete-word (arg)
  "Delete word before point."
  (interactive "p")
  (delete-region (point) (progn (forward-word (- arg)) (point))))

(defun split-window-horizontally-into-3 ()
  "Split current window horizontally into three windows of equal width,
   and mark the rightmost one as dedicated (which means no automatic
   display of buffers in it)."
  (interactive)
  (let* ((edges (window-edges))
         (width (- (caddr edges) (car edges)))
         (rightwin (split-window nil (/ width 3) t))
         (rightrightwin (split-window rightwin (/ width 3) t)))
    (set-window-dedicated-p rightrightwin t)))

(defun fullscreen ()
  "Toggle fullscreen editing."
  (interactive)
  (menu-bar-mode 0)
  (set-frame-parameter nil 'fullscreen
                       (if (frame-parameter nil 'fullscreen) nil 'fullboth)))

(defun kill-other-buffer ()
  "Kill other window's buffer."
  (interactive)
  (let ((window (selected-window)))
    (other-window 1)
    (kill-buffer nil)
    (select-window window)))

(defun find-file-with-linenum ()
  "Find file and go to line number specifed with :num."
  (interactive)
  (let* ((fname (ffap-prompter))
         (cpos (string-match ":" fname))
         (line (if cpos (string-to-number (substring fname (1+ cpos))) 0))
         (fpos (or (and (> line 0) cpos) (length fname))))
    (find-file-at-point (substring fname 0 fpos))
    (when cpos (goto-line (string-to-number (substring fname (1+ cpos)))))))

(defun isearch-kill-match ()
  "Kill the current isearch match string and continue searching."
  (interactive)
  (kill-region isearch-other-end (point)))

(defun isearch-occur ()
  "Invoke `occur' from within isearch."
  (interactive)
  (let ((case-fold-search isearch-case-fold-search))
    (occur (if isearch-regexp isearch-string (regexp-quote isearch-string)))))

(defun isearch-yank-whole-symbol ()
  "Pull symbol at point into search string."
  (interactive)
  (isearch-yank-string (symbol-name (symbol-at-point))))

(defun forward-char-but-not-eol ()
  (interactive)
  (if (not (looking-at "\n")) (forward-char)))

(defvar current-this-regex "")
(defun search-for-this-word ()
  "Emulate Vim's `*' binding."
  (interactive)
  (let ((tag (find-tag-default)))
    (if tag (setq new-this-regex
                  (concat "\\<" (regexp-quote tag) "\\>"))
      (error "point not over tag")))
  (unless (string-equal new-this-regex current-this-regex)
    (font-lock-remove-keywords
     nil (list (list current-this-regex 0 'lazy-highlight-face t)))
    (font-lock-add-keywords
     nil (list (list new-this-regex 0 'lazy-highlight-face t)))
    (setq current-this-regex new-this-regex)
    (font-lock-fontify-buffer)
    (message (concat "Searching for " (substring new-this-regex 2 -2))))
  (unless (search-forward-regexp current-this-regex nil t
                                 (if (looking-at "\\<") 2 1))
    (beginning-of-buffer)
    (message "search hit BOTTOM, continuing at TOP")
    (search-forward-regexp current-this-regex))
  (while (not (looking-at current-this-regex))
    (backward-char 1)))

(defun count-words ()
  "Count words in region or buffer."
  (interactive)
  (if (region-active-p)
      (message "Word count: %s" (how-many "\\w+"
                                          (region-beginning) (region-end)))
    (message "Word count: %s" (how-many "\\w+"
                                        (point-min) (point-max)))))

(require 'grep)
(defun grep (regexp &optional files)
  "Always grep from the current directory."
  (interactive
   (progn
     (grep-compute-defaults)
     (let* ((regexp (grep-read-regexp))
            (files (grep-read-files regexp)))
       (list regexp files))))
  (rgrep regexp files default-directory))

(defun smart-tab ()
  "This smart tab is minibuffer compliant: it acts as usual in
   the minibuffer. Else, if mark is active, indents region. Else if
   point is at the end of a symbol, expands it. Else indents the
   current line."
  (interactive)
  (if (minibufferp)
      (unless (minibuffer-complete)
        (dabbrev-expand nil))
    (if (smart-tab-must-expand prefix)
        (smart-expand-function)
      (smart-indent))))

(defun smart-indent ()
  "Indents region if mark is active, or current line otherwise."
  (interactive)
  (if mark-active
      (indent-region (region-beginning)
                     (region-end))
    (indent-for-tab-command)))

(defun smart-tab-must-expand (&optional prefix)
  "If PREFIX is \\[universal-argument], answers no.
   Otherwise, analyses point position and answers."
  (unless (or (consp prefix)
              mark-active)
    (looking-at "\\_>")))


(defun same-file-in-py3k ()
  "Find this file in py3k instead of trunk."
  (interactive)
  (let* ((fn (buffer-file-name))
         (m (string-match "/python\\(3k\\|30\\|26\\|\\)/" fn))
         (rfn (replace-match "/python3k/" t t fn)))
    (if (not (string= fn rfn))
        (find-file-other-window rfn)
      (message "already in py3k, or not in python at all"))))

(defun same-file-in-trunk ()
  "Find this file in trunk instead of py3k."
  (interactive)
  (let* ((fn (buffer-file-name))
         (m (string-match "/python\\(3k\\|30\\|26\\|\\)/" fn))
         (rfn (replace-match "/python/" t t fn)))
    (if (not (string= fn rfn))
        (find-file-other-window rfn)
      (message "already in trunk, or not in python at all"))))

(defun prompt-face-color (face)
  "Repeatedly prompt for new color for a given face."
  (interactive (list (read-face-name "Face" "default")))
  (if (member face '(nil ""))
      (setq face 'default))
  (while t
    (set-face-attribute face nil :foreground (read-color "Color: "))))


;; ---------- Extension configuration ------------------------------------------

;; load all these files without stopping on errors
(defun load-nonstop (file) (ignore-errors (load file t)))

;; load my extensions if they are present
(load-nonstop "extensions.el")
;; and separately, those provided by the distribution (packed in different
;; files so that errors don't skip the whole file)
(mapc 'load-nonstop (directory-files "~/.emacs.d/distext" t "\\.el"))
;; and finally, local settings that don't go into the repo
(load-nonstop "local.el")
