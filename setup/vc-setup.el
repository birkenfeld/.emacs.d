;; VC, git, magit stuff

(require 'fullframe)

;; git time machine

(global-set-key (kbd "C-x v t") #'git-timemachine)

;; show blame for current line

(global-set-key (kbd "C-x v p") #'git-messenger:popup-message)

;; full screen vc-annotate

(defun vc-annotate-quit-window ()
  "Kills the vc-annotate buffer."
  (interactive)
  (kill-buffer))

(eval-after-load "vc-annotate"
  '(progn
     (fullframe vc-annotate vc-annotate-quit-window)
     (define-key vc-annotate-mode-map (kbd "q") #'vc-annotate-quit-window)))

(global-set-key (kbd "C-x v x") #'magit-status)

;; full screen magit-status
(fullframe magit-status magit-mode-quit-window)
