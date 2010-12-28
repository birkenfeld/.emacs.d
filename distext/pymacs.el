;; ---------- pymacs.el init file: Pymacs dependent packages init --------------

;; load the pastebin connector
(defun pastebin ()
  (interactive)
  (require 'pymacs)
  (pymacs-load "pastemacs" "paste-")
  (paste-menu))

;; load ropemacs automatically for python-mode
;(eval-after-load 'python-mode '(ac-ropemacs-init))
