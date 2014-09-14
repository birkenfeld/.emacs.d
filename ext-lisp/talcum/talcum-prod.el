;;; $Id: talcum-prod.el,v 2.5 2005/08/21 19:48:55 ulmi Exp $
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
(require 'talcum-compat)
(require 'talcum-menu)
(require 'advice)

(defgroup talcum-prod nil "Prodding your viewer" :group 'talcum)

(defcustom talcum-use-prod-flag t
  "*Enable the whole prodding subsystem."
  :group 'talcum
  :type 'boolean)

;; current syntax is:
;; (predicate command)
(defcustom talcum-prod-commands 
  '(((lambda () (string-match "xdvi" tex-dvi-view-command)) 
     "pkill -USR1 xdvi.bin")
    (ignore 
     lower-frame))
  "A list of things that might be executed after LaTeXing.
Each element is a list (DOTHIS-P DOWHAT), where:
DOTHIS-P is t, nil, or a function. nil means do not execute this,
anything else means execute this.
DOWHAT is either a string or a function. If a function, it is called,
if a string, the string is fed into the shell that runs LaTeX."
  :group 'talcum-prod
  :type '(repeat
	  (list
	   (choice (const t) (function :value ignore))
	   (choice (string :value "/bin/true") (function :value ignore))
	   )))

(defcustom talcum-prod-cycle 1
  "Talcum checks if it can run the next prod command every n seconds."
  :group 'talcum-prod
  :type '(choice integer (number :value 0.3)))

(defvar talcum-prod-tick 0)

(talcum-namespace

 (defun talcum-prod-generate-menu ()
   (list "--"
         (~~menu-flag-toggle talcum-use-prod-flag 
                             "Auto-refresh Viewer")))

 (add-to-list 'talcum-menu-functions #'talcum-prod-generate-menu)

 (defun ~~work-prod-queue (whatnext &optional docu)
   (when talcum-use-prod-flag
     ;; if delay is high, talcum might not be active anymore 
     ;; when this triggers.
     (~~debug 0 "talcum-prod: advice entered, talcum mode is %s" talcum-mode)
     (when (or talcum-mode docu) ; evil hack: AucTeX moves to some other buffer
       (let ((dothis-p (nth 0 (car whatnext)))
             (dowhat   (nth 1 (car whatnext)))
             (restjobs (cdr whatnext))
             (proc (cond
                    ((talcum-latex-p) (tex-shell-proc))
                    ((talcum-auctex-p) (TeX-process docu)))))
         (when (or 
                (equal dothis-p t)	; always
                (and (functionp dothis-p) (funcall dothis-p))) ; pred says go!
           (~~debug 0 "Yes, I'm doing something: %s" dowhat)
           ;; next, check if we have a shell string or a fun.
           (cond
            ((functionp dowhat) 
             (~~debug 0 "Calling fun: %S" dowhat)
             (funcall dowhat))
            ((stringp dowhat) 
             ;; big ugly hack: assume we can send command if
             ;; no new output is coming for 0.1s. Maybe this 
             ;; will fail for files with lengthy calculations?
             ;; (fp comes to mind, as does beamer)
             ;; OTOH, on all but the first call, the other delay
             ;; is added to it as well.
             (if (or
                  (and (talcum-latex-p) 
                       (/= talcum-prod-tick
                           (setq talcum-prod-tick 
                                 (buffer-modified-tick (tex-shell-buf)))))
                  ;;(accept-process-output proc 0 100))
                  (and (or docu (talcum-auctex-p)) compilation-in-progress))
                 (progn
                   (~~debug 0 "no, not yet.")
                   (setq restjobs whatnext))
               (~~debug 1 "Executing: %s" dowhat)
               ;; note: it's the user's responsibility to add "&"
               ;; if they want a background process here.
               (cond
                ((talcum-latex-p)
                 (tex-send-command dowhat nil nil))
                ((or docu (talcum-auctex-p))
                 (~~debug 0 "TeX-run-command %s %s %s -> %s" docu dowhat ""
                          (TeX-run-command docu dowhat ""))))))
            (t (error "Unknown kind of dowhat: %s" dowhat))))
         (if restjobs
             (run-at-time talcum-prod-cycle nil 
                          (quote ~~work-prod-queue) restjobs docu))))))

 ;; This is called upon activation of the mode.
 (defun talcum-prod-init ()
   ;; This is easy: intercept tex-file.
   (cond
    ((talcum-latex-p)
     (defadvice tex-file (after talcum-prod-viewer) ; disable)
       "Prod viewers to refresh and show the new file."
       (~~work-prod-queue talcum-prod-commands))
     (ad-enable-advice 'tex-file 'after 'talcum-prod-viewer)
     (ad-activate 'tex-file)
     t)
     
    ;; This is tricky. Intercept the plethora of commands AucTeX provides.
    ((talcum-auctex-p)
     (defadvice TeX-run-format (after talcum-prod-viewer-AucTeX) ; disable)
       "Prod viewers to refresh and show the new file."
       (~~work-prod-queue talcum-prod-commands (ad-get-arg 0)))
     (ad-enable-advice 'TeX-run-format 'after 'talcum-prod-viewer-AucTeX)
     (ad-activate 'TeX-run-format)
     t)))

 ;; This is called upon deactivation of the mode.
 (defun talcum-prod-exit ()
   (cond
    ((talcum-latex-p)
     (ad-disable-advice 'tex-file 'after 'talcum-prod-viewer)
     ;;(ad-activate 'tex-file)
     t)
    ((talcum-auctex-p)
     (ad-disable-advice 'TeX-run-format 'after 'talcum-prod-viewer-AucTeX)
     ;;(ad-activate 'TeX-run-format)
     t)))
 
 (~~sourcefile-versionstamp "$RCSfile: talcum-prod.el,v $"  
			    "$Revision: 2.5 $") 
 
 ) ;;namespace

(add-hook 'talcum-enter-mode-hook #'talcum-prod-init)
(add-hook 'talcum-leave-mode-hook #'talcum-prod-exit)


(provide 'talcum-prod)

;;; talcum-prod.el ends here