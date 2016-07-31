;; Completion related stuff

;; Ido -------------------------------------------------------------------------

;; Use C-w to go back up a dir to better match normal usage of C-w
;; - insert current file name with C-x C-w instead.
(define-key ido-file-completion-map (kbd "C-w") #'ido-delete-backward-updir)
(define-key ido-file-completion-map (kbd "C-x C-w") #'ido-copy-current-file-name)

(define-key ido-file-dir-completion-map (kbd "C-w") #'ido-delete-backward-updir)
(define-key ido-file-dir-completion-map (kbd "C-x C-w") #'ido-copy-current-file-name)

;;  ;; Display ido results vertically, rather than horizontally
;;  (setq ido-decorations (quote ("\n-> " "" "\t" "\n   ..." " [" "]"
;;                                " [No match]" " [Matched]" " [Not readable]" " [Too big]" " [Confirm]")))
;;  (defun ido-disable-line-truncation () (set (make-local-variable 'truncate-lines) nil))
;;  (add-hook 'ido-minibuffer-setup-hook 'ido-disable-line-truncation)
;;  (defun ido-define-keys () ;; C-n/p is more intuitive in vertical layout
;;    (define-key ido-completion-map (kbd "C-n") #'ido-next-match)
;;    (define-key ido-completion-map (kbd "C-p") #'ido-prev-match))
;;  (add-hook 'ido-setup-hook 'ido-define-keys)


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

;; don't get confused with fill-column-indicator
(defvar-local company-fci-mode-on-p nil)

(defun company-turn-off-fci (&rest ignore)
  (when (boundp 'fci-mode)
    (setq company-fci-mode-on-p fci-mode)
    (when fci-mode (fci-mode -1))))

(defun company-maybe-turn-on-fci (&rest ignore)
  (when company-fci-mode-on-p (fci-mode 1)))

(eval-after-load "company"
  '(progn
     (define-key company-active-map (kbd "<prior>") #'company-select-previous-page)
     (define-key company-active-map (kbd "<next>") #'company-select-next-page)
     (define-key company-active-map (kbd "TAB") #'company-complete-selection)
     (define-key company-active-map [tab] #'company-complete-selection)

     (add-hook 'company-completion-started-hook 'company-turn-off-fci)
     (add-hook 'company-completion-finished-hook 'company-maybe-turn-on-fci)
     (add-hook 'company-completion-cancelled-hook 'company-maybe-turn-on-fci)))

(add-hook 'emacs-lisp-mode-hook 'company-mode)
