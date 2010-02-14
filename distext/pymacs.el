;; ---------- pymacs.el init file: Pymacs dependent packages init --------------

;; load path for Python modules, must be set before loading pymacs
(setq pymacs-load-path '("~/.emacs.d/pymacs"))

;; load the pastebin connector
(defun pastebin ()
  (interactive)
  (require 'pymacs)
  (pymacs-load "pastemacs" "paste-")
  (paste-menu))

;; load ropemacs (needs Python package `rope')
(defun load-ropemacs ()
  "Load pymacs and ropemacs"
  (interactive)
  (require 'pymacs)
  (pymacs-load "ropemacs" "rope-")
  ;; Automatically save project python buffers before refactorings
  (setq ropemacs-confirm-saving 'nil)
  ;; Add another keybinding
  (define-key ropemacs-local-keymap (kbd "C-c j") 'rope-jump-to-global)
)

;; load ropemacs automatically for python-mode
(eval-after-load 'python-mode '(ignore-errors (ac-ropemacs-init)))
