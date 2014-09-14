;; Flymake / flycheck setup

;; Display flymake errors in modeline
;(require 'flymake-cursor)

(defun flycheck-next-or-first-error ()
  "Go to next flycheck error, and wrap around after last error."
  (interactive)
  (condition-case nil
      (flycheck-next-error)
    (error (flycheck-first-error))))


;; Flymake error finding with the same keys as grep/occur/etc.
(global-set-key (kbd "M-g M-e") 'flycheck-next-or-first-error)
