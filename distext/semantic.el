;; ---------- semantic.el init file: load semantic if present ------------------

;; semantic
(setq semantic-load-turn-useful-things-on t)
(require 'semantic)
(require 'semanticdb) 
;(semantic-load-enable-code-helpers)

(require 'semantic-tag-folding)
(global-semantic-tag-folding-mode 1)
(global-set-key (kbd "C-c <left>") 'semantic-tag-folding-fold-block)
(global-set-key (kbd "C-c <right>") 'semantic-tag-folding-show-block)
