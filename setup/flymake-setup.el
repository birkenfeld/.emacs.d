;; Flycheck setup

(defun flycheck-next-or-first-error ()
  "Go to next flycheck error, and wrap around after last error."
  (interactive)
  (condition-case nil
      (flycheck-next-error)
    (error (flycheck-first-error))))

(when (boundp 'next-error-repeat-map)
  (define-key next-error-repeat-map (kbd "M-e") #'flycheck-next-or-first-error)
  (define-key next-error-repeat-map (kbd "e") #'flycheck-next-or-first-error)
  (put 'flycheck-next-or-first-error 'repeat-map 'next-error-repeat-map))

(global-set-key (kbd "M-g M-e") #'flycheck-next-or-first-error)
(global-set-key (kbd "M-g e") #'flycheck-next-or-first-error)
