(eval-after-load 'dired
  '(progn
     (require 'dired-x)
     (require 'discover)
     ;(require 'dired-sort)
     (add-hook 'dired-mode-hook 'discover-mode)
     ;; we use discover for this
     ;(add-hook 'dired-mode-hook
     ;          (lambda () (guide-key/add-local-guide-key-sequence "%")))
     ))
