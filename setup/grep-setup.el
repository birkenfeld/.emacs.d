;; Setup grep library

(require 'grep)

;; Don't wrap lines in grep mode
(add-hook 'grep-mode-hook (lambda () (setq truncate-lines t)))

(defun grep (regexp &optional files)
  "rgrep from the current directory."
  (interactive
   (progn
     (grep-compute-defaults)
     (let* ((regexp (grep-read-regexp))
            (files (grep-read-files regexp)))
       (list regexp files))))
  (rgrep regexp files default-directory))

(global-set-key (kbd "C-x g") #'grep)

(add-to-list 'grep-find-ignored-files ".tags")
(add-to-list 'grep-find-ignored-files ".noseids")

(add-to-list 'grep-find-ignored-directories "_build")
(add-to-list 'grep-find-ignored-directories "__pycache__")

;; wgrep: edit grep results
(require 'wgrep)

(defalias 'wgrep 'wgrep-change-to-wgrep-mode)

;; wgrep expects a regex with 3 capturing groups
(setq wgrep-line-file-regexp "^\\(.*?[^/\n]\\)\\(:[ \t]*\\)\\([1-9][0-9]*\\)[ \t]*:")

;; Use same keybinding as occur
(setq wgrep-enable-key "e")

;; Add custom keybindings
(define-key grep-mode-map (kbd "C-x C-s") #'wgrep-save-all-buffers)
