;; VC, git stuff

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

;; Nicer modeline string
(defun vc-default-mode-line-string (backend file)
  (let* ((backend-name (symbol-name backend))
         (state   (vc-state file backend)))
    (propertize
     (cond ((or (eq state 'up-to-date)
                (eq state 'needs-update))
                                 "   @")
           ((stringp state)      "LL @")
           ((eq state 'added)    "++ @")
           ((eq state 'conflict) "!! @")
           ((eq state 'removed)  "-- @")
           ((eq state 'missing)  "?? @")
           (t                    "MM @"))
     'face 'vc-state-base)))
