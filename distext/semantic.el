;; ---------- semantic.el init file: load semantic if present ------------------

;; semantic
(setq semantic-load-turn-useful-things-on t)
(require 'semantic)
(require 'semanticdb)
(semantic-load-enable-code-helpers)

(require 'semantic-tag-folding)
(global-semantic-tag-folding-mode 1)
(global-set-key (kbd "C-c <left>") 'semantic-tag-folding-fold-block)
(global-set-key (kbd "C-c <right>") 'semantic-tag-folding-show-block)

(defun semantic-format-tag-name-short (tag &optional parent color)
  (let ((name (semantic-format-tag-name tag parent color)))
    (if (string-prefix-p "\"\"\"" name) "<doc>" name)))
(setq semantic-imenu-summary-function 'semantic-format-tag-name-short)

;; enable semantic features for Python mode
(add-hook 'python-mode (lambda ()
  (add-hook 'after-save-hook 'semantic-fetch-tags nil t)
))

(add-hook 'semantic-init-hooks (lambda ()
  (when (eq major-mode 'python-mode)
    (semantic-fetch-tags)
    (semantic-stickyfunc-mode 1)
    (semantic-highlight-func-mode 1))) t)
