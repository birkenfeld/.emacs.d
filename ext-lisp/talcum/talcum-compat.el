;;; $Id: talcum-compat.el,v 2.7 2005/09/07 18:04:44 ulmi Exp $
;;;
;;; This file is part of Talcum.
;;;
;;; Talcum is released under the terms and conditions
;;; of the LaTeX Project Public License 1.3a (see file
;;; LPPL-13a for details)
;;;
;;; Talcum has the maintenance status: author-maintained.
;;; Maintainer: Ulrich M. Schwarz, ulmi@users.sarovar.org.
;;; 

(require 'talcum-tools)

(talcum-namespace
 (~~sourcefile-versionstamp "$RCSfile: talcum-compat.el,v $"  "$Revision: 2.7 $"))
	    

(defun talcum-infidel-p ()
"Returns t for XEmacs, nil otherwise."
(string-match "XEmacs\\|Lucid" (emacs-version)))

(defun talcum-auctex-p ()
"Returns t if AucTeX is running, nil otherwise"
(and (boundp 'AUC-TeX-version) (eq major-mode 'latex-mode)))

(defun talcum-latex-p ()
"Returns t if plain old latex mode is running, nil otherwise"
(and (not (boundp 'AUC-TeX-version)) (eq major-mode 'latex-mode)))

(defcustom talcum-buffer-molestors
  '( reftex-view-crossref-when-idle
     reftex-view-cr-ref
     reftex-view-cr-cite
     latex-indent
     ;;show-paren-function ;; won't highlight rendered paren otherwise
     )
  "A list of function symbols of functions that do not set
inhibit-point-motion-hooks even though they should.

This can lead to unwanted unrendering."
  :type '(repeat function))

;; This is done at load-time. This is safe, because advice on an
;; undefined function is stored until the function is defined.
(talcum-namespace
 (dolist (fun talcum-buffer-molestors)
   (~~debug 4 "disabling in: %s %s" fun (symbolp fun))
   (eval
    `(defadvice ,fun (around talcum-point-motion-hook-disabler activate)
       "Prevent point-motion-hooks in this function."
       (let ((inhibit-point-motion-hooks t))
         ad-do-it))))
 ); namespace

(provide 'talcum-compat)

;; talcum-compat.el ends here

