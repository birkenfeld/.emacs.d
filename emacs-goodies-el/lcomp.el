;;; lcomp.el --- list-completion hacks!

;; Copyright (C) 2002 by Taiki SUGAWARA <taiki.s@cityfujisawa.ne.jp>

;; Author: Taiki SUGAWARA <taiki.s@cityfujisawa.ne.jp>
;; Keywords: tool

;; This file is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.

;; This file is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to
;; the Free Software Foundation, Inc., 59 Temple Place - Suite 330,
;; Boston, MA 02111-1307, USA.

;;; Code:

(defvar lcomp-before-completion-winconf nil)
(defvar lcomp-completion-halfway-p nil)
(defvar lcomp-display-completion-buffer-p nil)
(defvar lcomp-completion-buffer nil)

;;; key bindings.
(define-key completion-list-mode-map "\C-i" 'next-completion)
(define-key completion-list-mode-map "\M-\C-i" 'previous-completion)
(define-key completion-list-mode-map "f" 'next-completion)
(define-key completion-list-mode-map "b" 'previous-completion)
(define-key completion-list-mode-map "n" 'next-line)
(define-key completion-list-mode-map "p" 'previous-line)
(define-key completion-list-mode-map " " 'scroll-up)
(define-key completion-list-mode-map [del] 'scroll-down)
(define-key completion-list-mode-map [backspace] 'scroll-down)
(define-key completion-list-mode-map "q" 'delete-completion-window)
(define-key global-map "\M-v" 'lcomp-select-completion-window-or-scroll-down)

(defadvice try-completion (around lcomp-ad activate)
  (let ((ret ad-do-it))
    (setq lcomp-completion-halfway-p (stringp ret))
    ret))

(defun lcomp-setup-completion ()
  (when (and (not lcomp-before-completion-winconf)
	     (not (window-minibuffer-p)))
    (setq lcomp-display-completion-buffer-p t)
    (setq lcomp-completion-buffer standard-output)
    (setq lcomp-before-completion-winconf (current-window-configuration))))

(add-hook 'completion-setup-hook 'lcomp-setup-completion)

(defun lcomp-resume-before-completion-winconf-1 ()
  (condition-case nil
      (set-window-configuration lcomp-before-completion-winconf)
    (error 
     (message "%s occured. bat ignore." (error-message-string err))))
  (setq lcomp-before-completion-winconf nil)
  (setq lcomp-completion-buffer nil))

(defun lcomp-resume-before-completion-winconf ()
  (when (and lcomp-before-completion-winconf
	     (not (or (and (eq this-command 'self-insert-command)
			   (string-match "\\(\\sw\\|\\s_\\)"
					 (this-command-keys)))
		      (eq (current-buffer) lcomp-completion-buffer)
		      (window-minibuffer-p)
		      lcomp-display-completion-buffer-p
		      lcomp-completion-halfway-p)))
    (let ((buf (current-buffer)))
      (lcomp-resume-before-completion-winconf-1)
      (unless (eq buf (current-buffer))
	(switch-to-buffer buf))))
  (setq lcomp-display-completion-buffer-p nil)
  (setq lcomp-completion-halfway-p nil))

(add-hook 'post-command-hook 'lcomp-resume-before-completion-winconf)

(defadvice choose-completion (after lcomp-ad activate)
  (when lcomp-before-completion-winconf
    (lcomp-resume-before-completion-winconf-1)))

(defadvice delete-completion-window (around lcomp-ad activate)
  (if lcomp-before-completion-winconf
      (lcomp-resume-before-completion-winconf)
    ad-do-it))

(defun lcomp-select-completion-window ()
  (interactive)
  (select-window (get-buffer-window lcomp-completion-buffer)))

(defun lcomp-select-completion-window-or-scroll-down (&optional arg)
  (interactive "P")
  (if lcomp-before-completion-winconf
      (lcomp-select-completion-window)
    (scroll-down arg)))

(provide 'lcomp)

;;; lcomp.el ends here