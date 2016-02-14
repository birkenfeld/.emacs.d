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

;; ignore whitespace

(defun magit-toggle-whitespace ()
  (interactive)
  (if (member "-w" magit-diff-options)
      (magit-dont-ignore-whitespace)
    (magit-ignore-whitespace)))

(defun magit-ignore-whitespace ()
  (interactive)
  (add-to-list 'magit-diff-options "-w")
  (magit-refresh))

(defun magit-dont-ignore-whitespace ()
  (interactive)
  (setq magit-diff-options (remove "-w" magit-diff-options))
  (magit-refresh))

;; C-c C-a to amend without any prompt

(defun magit-just-amend ()
  (interactive)
  (save-window-excursion
    (magit-with-refresh
      (shell-command "git --no-pager commit --amend --reuse-message=HEAD"))))

;; C-x C-k to kill file on line

(defun magit-kill-file-on-line ()
  "Show file on current magit line and prompt for deletion."
  (interactive)
  (magit-visit-item)
  (delete-current-buffer-file)
  (magit-refresh))

(eval-after-load 'magit
  '(progn
     (define-key magit-status-mode-map (kbd "W") #'magit-toggle-whitespace)
     (define-key magit-status-mode-map (kbd "C-c C-a") #'magit-just-amend)
     (define-key magit-status-mode-map (kbd "C-x C-k") #'magit-kill-file-on-line)
     ))
