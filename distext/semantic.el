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

;; enable semantic features for Python mode
(add-hook 'python-mode (lambda ()
  (add-hook 'after-save-hook 'semantic-fetch-tags nil t)
))

(add-hook 'semantic-init-hooks (lambda ()
  (when (eq major-mode 'python-mode)
    (semantic-stickyfunc-mode 1)
    (semantic-highlight-func-mode 1))) t)
