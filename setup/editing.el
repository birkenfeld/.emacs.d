;; Editing / navigating etc. setup

;; winpoint (remember point location by window)
(require 'winpoint)
(window-point-remember-mode 1)

;; Display match count while searching/replacing
(require 'anzu)
(global-anzu-mode 1)
(global-set-key (kbd "M-%") #'anzu-query-replace)
(global-set-key (kbd "C-M-%") #'anzu-query-replace-regexp)

;; highlight newly-inserted text etc
(require 'volatile-highlights)
(volatile-highlights-mode t)

;; Martin Blais' dubious paragraphs
(require 'dubious-paragraphs)
(global-set-key (kbd "M-n") #'dubious-forward-paragraph)
(global-set-key (kbd "M-N") #'dubious-forward-paragraph-scroll)
(global-set-key (kbd "M-p") #'dubious-backward-paragraph)
(global-set-key (kbd "M-P") #'dubious-backward-paragraph-scroll)

;; Easy repeat by last key of key sequence, only needed up to Emacs 27
(unless (fboundp 'repeat-mode)
  (require 'easy-repeat)
  (setq easy-repeat-command-list
        '(flycheck-next-or-first-error
          previous-error
          next-error))
  (easy-repeat-mode 1))

;; Adaptive fill is great
(require 'adaptive-wrap)
(setq-default adaptive-wrap-prefix-mode t)

;; Browse the kill ring for things to yank
(require 'browse-kill-ring)
(browse-kill-ring-default-keybindings)
(defadvice yank-pop (around kill-ring-browse-maybe (arg) activate)
  "If last action was not a yank, run `browse-kill-ring' instead."
  (if (not (eq last-command 'yank))
      (browse-kill-ring)
    ad-do-it))

;; When popping the mark, continue popping until the cursor actually moves
;; Also, if the last command was a copy - skip past all the expand-region cruft.
(defadvice pop-to-mark-command (around ensure-new-position activate)
  (let ((p (point)))
    (when (eq last-command 'save-region-or-current-line)
      ad-do-it
      ad-do-it
      ad-do-it)
    (dotimes (i 10)
      (when (= p (point)) ad-do-it))))

;; The new register-preview feature in 24.4 is nice, but it destroys my window
;; configuration; use popwin to pop up the preview in the bottom
(require 'popwin)
(add-to-list 'display-buffer-alist
             '("Register Preview" popwin:special-display-popup-window))

;; Colorize register preview a bit
(defun register-preview-color (r)
  (let* ((c (car r))
         (d (replace-regexp-in-string
             "\n[ \t]*" " "
             (with-output-to-string (describe-register-1 c t))))
         (s (if (string-match "Register.+? contains \\(?:an? \\|the \\)?" d)
                (substring d (match-end 0)) d))
         (k (propertize (single-key-description c)
                        'face 'font-lock-function-name-face)))
  (concat k ": " s "\n")))

(setq register-preview-function #'register-preview-color)

;; Rectangle mode using multiple-cursors

(require 'rectangular-region-mode)

(defun kill-region-deactivate-mc ()
  (interactive)
  (rrm/switch-to-multiple-cursors)
  (mc/execute-command-for-all-fake-cursors 'kill-region)
  (kill-region (point) (mark))
  (rrm/keyboard-quit))

(defun my-markdown-mode-hook ()
  (set-variable 'show-trailing-whitespace t)
  )

(add-hook 'markdown-mode-hook #'my-markdown-mode-hook)

(add-hook 'compilation-mode-hook 'winnow-mode)

;; Abbrevs

(setq-default abbrev-mode t)

;; Undo macros in a block

(defun block-undo (fn &rest args)
  (let ((marker (prepare-change-group)))
    (unwind-protect (apply fn args)
      (undo-amalgamate-change-group marker))))

(dolist (fn '(kmacro-call-macro
              kmacro-exec-ring-item
              apply-macro-to-region-lines))
  (advice-add fn :around #'block-undo))

;; Fix inserting newlines without indenting in electric-pair mode

(eval-after-load 'elec-pair
  '(defun electric-pair-open-newline-between-pairs-psif ()
     "Honour `electric-pair-open-newline-between-pairs'.
Member of `post-self-insert-hook' if `electric-pair-mode' is on."
     (when (and (if (functionp electric-pair-open-newline-between-pairs)
                    (funcall electric-pair-open-newline-between-pairs)
                  electric-pair-open-newline-between-pairs)
                (eq last-command-event ?\n)
                (< (1+ (point-min)) (point) (point-max))
                (eq (save-excursion
                      (skip-chars-backward "\t\s")
                      (char-before (1- (point))))
                    (matching-paren (char-after))))
       (save-excursion (newline-and-indent)))))


;; Rectangle mode enhancements

;; (require 'rect)

;; (defun move-or-space-to-column (col)
;;   (move-to-column col)
;;   (while (not (eq (current-column) col))
;;     (insert " ")))

;; (defun rectangle-self-insert-on-line (startcol endcol n)
;;     (move-or-space-to-column endcol)
;;     (self-insert-command n))

;; (defun rectangle-self-insert-command (n start end)
;;   (interactive "*p\nr")
;;   (apply-on-rectangle 'rectangle-self-insert-on-line start end n)
;;   (rectangle-forward-char n)
;;   (setq deactivate-mark nil)
;;   (undo-boundary))

;; (defun rectangle-delete-backward-char-on-line (startcol endcol n)
;;   (save-excursion
;;     (move-to-column endcol)
;;     (when (eq (current-column) endcol)
;;       (delete-backward-char n))))

;; (defun rectangle-delete-backward-char (n start end)
;;   (interactive "*p\nr")
;;   (let* ((cols (rectangle--pos-cols start end))
;;          (startcol (car cols))
;;          (endcol (cdr cols)))
;;     (when (> endcol startcol)
;;       (apply-on-rectangle 'rectangle-delete-backward-char-on-line start end n)
;;       (rectangle-backward-char 1)
;;       (setq deactivate-mark nil)
;;       (undo-boundary))))

;;(define-key rectangle-mark-mode-map [remap self-insert-command] #'rectangle-self-insert-command)
;;(define-key rectangle-mark-mode-map (kbd "DEL") #'rectangle-delete-backward-char)
