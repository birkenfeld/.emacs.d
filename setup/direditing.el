(eval-after-load 'dired
  '(progn
     (require 'dired-x)
     (require 'discover)
     ;(require 'dired-sort)
     (add-hook 'dired-mode-hook 'discover-mode)
     ))
