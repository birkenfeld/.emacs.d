;; Settings and setup for Python mode

;; Ignore Python 3.2+ .pyc directories
(add-to-list 'completion-ignored-extensions "__pycache__/")

;; Enable the elpy "ide" features
(elpy-enable)

;; Correct indentation for elpy test function
(function-put 'elpy-testcase 'lisp-indent-function 1)
(function-put 'mletf* 'lisp-indent-function 1)

;; Use flycheck instead of flymake
(setq elpy-modules (delq 'elpy-module-flymake elpy-modules))
(add-hook 'elpy-mode-hook 'flycheck-mode)

;; Always switch to shell after executing
(defadvice elpy-shell-send-region-or-buffer (around switch activate)
  ad-do-it
  (elpy-shell-switch-to-shell))

;; Find root in site-packages
(defun elpy-project-find-site-packages-root ()
  "Return site-packages root.

Here, we only consider the current "
  (when (locate-dominating-file default-directory "site-packages")
    (let ((root default-directory))
      (while (file-exists-p (expand-file-name "__init__.py" (file-name-directory (directory-file-name root))))
        (setq root (file-name-directory (directory-file-name root))))
      root)))

(add-to-list 'elpy-project-root-finder-functions #'elpy-project-find-site-packages-root)

;; Override some elpy keys
(define-key elpy-mode-map (kbd "<M-down>") nil)
(define-key elpy-mode-map (kbd "<M-up>") nil)
(define-key elpy-mode-map (kbd "<M-left>") nil)
(define-key elpy-mode-map (kbd "<M-right>") nil)
(define-key elpy-mode-map (kbd "M-.") 'projectile-find-tag)
(define-key elpy-mode-map (kbd "C-M-.") 'projectile-find-next-tag)
(define-key elpy-mode-map (kbd "C-c C-d") 'elpy-goto-definition)
(define-key elpy-mode-map (kbd "M-SPC") 'company-complete)

(defun company-insert-and-complete ()
  (interactive)
  ;;  (company-complete-selection)  ; in case it's already completing
  (company-abort)
  (self-insert-command 1)
  (company-complete))

(define-key elpy-mode-map (kbd ".") 'company-insert-and-complete)

(put 'font-lock-regexp-grouping-backslash 'face-alias 'font-lock-builtin-face)

;; Elpy related features customization

(defadvice eldoc-message (around no-in-xxx (&rest args) activate)
  "Do not show the \"In: ...\" in echo area."
  (if (and (car args)
           (string-match-p "^In: " (car args))) nil
    ad-do-it))

(eval-after-load "find-file-in-project"
  '(progn
     (set-variable 'ffip-full-paths t)))

(defun python-hs-end-of-folding-block (arg)
  (interactive)
  (if (looking-at "from\\|import")
      (forward-paragraph)
    (python-nav-end-of-defun)))

;; Fix hs-hide-level to hide all defs correctly (HACK HACK)
(fset 'forward-comment-orig (symbol-function 'forward-comment))
(defadvice hs-hide-level-recursive (around fix-forward-comment activate)
  (flet ((forward-comment (arg)
                          (forward-comment-orig arg)
                          (when (looking-at "def")
                            (beginning-of-line))))
    ad-do-it))

;; Enable nice display of folded regions
(hideshowvis-symbols)
(define-fringe-bitmap 'hs-marker [0 32 48 56 60 56 48 32 0])   ;; "plus"
(define-fringe-bitmap 'hideshowvis-hideable-marker [0 0 0 254 124 56 16 0 0])   ;; "minus"

;; Electric colon: only indent if on a dedenter statement
(defadvice python-indent-post-self-insert-function (around fix-colon activate)
  (unless (and (eq ?: last-command-event)
               (not (python-info-dedenter-statement-p)))
    ad-do-it))

(defun my-python-mode-hook ()
  ;; Remove python-mode's ffap things that slow down find-file
  (setq ffap-alist (remove '(python-mode . python-ffap-module-path) ffap-alist))
  (setq ffap-alist (remove '(inferior-python-mode . python-ffap-module-path) ffap-alist))

  ;; Enable nice electric pairs like in textmate
  (autopair-mode 1)
  (setq autopair-handle-action-fns
        (list #'autopair-default-handle-action
              #'autopair-python-triple-quote-action))

  ;; Highlight whitespace mistakes
  (setq whitespace-style '(face trailing tabs lines-tail empty))
  (require 'whitespace)
  (whitespace-mode 1)

  ;; Highlight symbol at point
  (require 'highlight-symbol)
  (highlight-symbol-mode 1)

  ;; Highlight escape sequences
  (require 'highlight-escape-sequences)
  (setq hes-simple-modes '(python-mode js-mode js2-mode))
  (hes-mode)

  ;; Reveal hidden text (folding!) when moving over it
  ;(reveal-mode 1)

  ;; Death to trailing whitespace!
  (set-variable 'show-trailing-whitespace 1)

  ;; Add some local hooks
  ;(add-hook 'before-save-hook 'delete-trailing-whitespace nil t)

  ;; Compile (<f5>) is execute
  (unless (or (file-exists-p "makefile")
              (file-exists-p "Makefile"))
    (set (make-local-variable 'compile-command)
         (concat "python " buffer-file-name)))

  (define-key python-mode-map (kbd "M-q") 'python-fill-paragraph)

  ;; set up hide-show mode
  ;; remove old python-mode value first
  ;(setq hs-special-modes-alist (assq-delete-all 'python-mode hs-special-modes-alist))
  ;(add-to-list 'hs-special-modes-alist
  ;             `(python-mode "^\\(?:from\\|import\\|\\s-*\\(?:def\\|class\\)\\)\\>" nil "#"
  ;                           python-hs-end-of-folding-block nil))

  (hs-minor-mode)
  (hideshowvis-minor-mode)

  ;; No auto-fill please
  (auto-fill-mode 0)
  )

(add-hook 'python-mode-hook #'my-python-mode-hook)

(defun my-elpy-mode-hook ()
  ;; Elpy resets this to zero, do not want.
  (setq company-idle-delay 1)
  )
(add-hook 'elpy-mode-hook #'my-elpy-mode-hook)

;; Cython mode
(autoload 'cython-mode "cython-mode" nil t)

(setq auto-mode-alist
      (append '(("\\.pyx$" . cython-mode)
                ("\\.pxs$" . cython-mode)
                ("\\.pxi$" . cython-mode))
              auto-mode-alist))


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
(add-hook 'rst-mode-hook 'flycheck-mode)


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
