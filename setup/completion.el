;; Completion related stuff

;; Ido -------------------------------------------------------------------------

;; Use C-w to go back up a dir to better match normal usage of C-w
;; - insert current file name with C-x C-w instead.
(define-key ido-file-completion-map (kbd "C-w") #'ido-delete-backward-updir)
(define-key ido-file-completion-map (kbd "C-x C-w") #'ido-copy-current-file-name)

(define-key ido-file-dir-completion-map (kbd "C-w") #'ido-delete-backward-updir)
(define-key ido-file-dir-completion-map (kbd "C-x C-w") #'ido-copy-current-file-name)


;; Company ---------------------------------------------------------------------

(defun sane-company-dabbrev (command &optional arg &rest ignored)
  "Just provide dabbrevs to company."
  (interactive (list 'interactive))
  (cl-case command
    (interactive (company-begin-backend 'sane-company-dabbrev))
    (prefix
     (let ((word (company-grab-word)))
       (if (equal word "") nil word)))
    (candidates
     (dabbrev--reset-global-variables)
     (dabbrev--find-all-expansions arg t))
    (ignore-case nil)
    (sorted t)))

(defun company-select-previous-page ()
  (interactive)
  (dotimes (i 10) (company-select-previous)))

(defun company-select-next-page ()
  (interactive)
  (dotimes (i 10) (company-select-next)))

(eval-after-load "company"
  '(progn
     (define-key company-active-map (kbd "<prior>") #'company-select-previous-page)
     (define-key company-active-map (kbd "<next>") #'company-select-next-page)
     (define-key company-active-map (kbd "TAB") #'company-complete-selection)
     (define-key company-active-map [tab] #'company-complete-selection)

     (require 'company-box)
     (add-hook 'company-mode-hook 'company-box-mode)
     ))

(add-hook 'emacs-lisp-mode-hook 'company-mode)
