;; speedbar customization with sr-speedbar

;; adapted from graphene-speedbar.el

(require 'sr-speedbar)

;; Refresh the speedbar when relevant hooks are run.
(defvar graphene-speedbar-refresh-hooks)
(defvar graphene-speedbar-refresh-hooks-added nil
  "Whether hooks have been added to refresh speedbar.")

(add-hook 'speedbar-mode-hook
          '(lambda ()
             (hl-line-mode 1)
             (visual-line-mode -1)
             (setq automatic-hscrolling nil)
             (setq truncate-lines t)
             (set (make-local-variable 'cua-read-only-cursor-color) "#2e3436")
             (let ((speedbar-display-table (make-display-table)))
               (set-display-table-slot speedbar-display-table 0 8230)
               (setq buffer-display-table speedbar-display-table))
             (when (not graphene-speedbar-refresh-hooks-added)
               (lambda ()
                 (mapc (lambda (hook)
                         (add-hook hook 'speedbar-refresh))
                       graphene-speedbar-refresh-hooks)
                 (setq graphene-speedbar-refresh-hooks-added t)))))

;; More familiar keymap settings.
(add-hook 'speedbar-reconfigure-keymaps-hook
          '(lambda ()
             (define-key speedbar-mode-map [S-up] 'speedbar-up-directory)
             (define-key speedbar-mode-map [right] 'speedbar-flush-expand-line)
             (define-key speedbar-mode-map [left] 'speedbar-contract-line)))

;; Overwrite to take eproject into account
(defun sr-speedbar-refresh ()
  "Refresh the context of speedbar."
  (let ((eproot (condition-case nil (eproject-root) (error default-directory))))
    (when (and (not (equal eproot sr-speedbar-last-refresh-dictionary)) ;if directory is change
               (not (sr-speedbar-window-p))) ;and is not in speedbar buffer
      (setq sr-speedbar-last-refresh-dictionary eproot)
      ;; first open root
      (let ((default-directory eproot)) (speedbar-refresh))
      ;; now let speedbar expand
      (speedbar-refresh))))

;; Always use the last selected window for loading files from speedbar.
(defvar last-selected-window
  (if (not (eq (selected-window) sr-speedbar-window))
      (selected-window)
    (other-window 1)))

(defadvice select-window (after remember-selected-window activate)
  "Remember the last selected window."
  (unless (or (eq (selected-window) sr-speedbar-window)
              (not (window-live-p (selected-window))))
    (setq last-selected-window (selected-window))))

(defun sr-speedbar-before-visiting-file-hook ()
  "Function that hooks `speedbar-before-visiting-file-hook'."
  (select-window last-selected-window))

(defun sr-speedbar-before-visiting-tag-hook ()
  "Function that hooks `speedbar-before-visiting-tag-hook'."
  (select-window last-selected-window))

(defun sr-speedbar-visiting-file-hook ()
  "Function that hooks `speedbar-visiting-file-hook'."
  (select-window last-selected-window))

(defun sr-speedbar-visiting-tag-hook ()
  "Function that hooks `speedbar-visiting-tag-hook'."
  (select-window last-selected-window))
