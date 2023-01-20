;; Settings for GitHub copilot

(eval-after-load 'copilot
  '(progn
     (define-key copilot-mode-map (kbd "s-<return>") #'copilot-accept-completion)
     (define-key copilot-mode-map (kbd "s-<right>") #'copilot-next-completion)
     (define-key copilot-mode-map (kbd "s-<left>") #'copilot-previous-completion)     
))

(autoload 'copilot-mode "copilot")
