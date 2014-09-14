;;;!; $Id: talcum-bugreport.el,v 1.4 2005/09/07 18:12:16 ulmi Exp $
;;;!;
;;;!; This file is part of Talcum.
;;;!;
;;;!; Talcum is released under the terms and conditions
;;;!; of the LaTeX Project Public License 1.3a (see file
;;;!; LPPL-13a for details)
;;;!;
;;;!; Talcum has the maintenance status: author-maintained.
;;;!; Maintainer: Ulrich M. Schwarz, ulmi@users.sarovar.org.
;;;!; 

;;; Bug reporting.
;;; 
;;; It's difficult for users to know what's important and what's not.
;;; Also, I tend to give out an erroneous email address.

(require 'talcum-tools)
(require 'talcum-menu)

(talcum-namespace
 (~~sourcefile-versionstamp "$RCSfile: talcum-bugreport.el,v $" "$Revision: 1.4 $"))

(defun talcum-report-bug (subj)
  (interactive "sOne-line description: ")
  (compose-mail "ulmi@users.sarovar.org"
                (concat "[Talcum "
                        talcum-version
                        " Bug] "
                        subj) nil t)
  (insert " 

Please describe your problem here. ulmi speaks English, German and
very little French. In many cases (e.g. display bugs), it is helpful
if a small sample file is included on which the bug is reproducible.

"

;; Please read and acknowledge the following legalese:

;; The bug report includes information about your system (most
;; importantly, operating system and Emacs version, and whether you use
;; AucTeX). This information is included in plain text, at the end of
;; this report.

;; You give permission for this mail to be published and incorporated,
;; whole or in parts, free of charge and obligation to the maintainer, in
;; future releases of Talcum (e.g. if you propose a fix) and/or on the
;; Talcum homepage (probably in the Bugs and Patches sections). The
;; maintainer will give credit where it's due, unless he forgets to (in
;; which case, please submit a bug about forgetting to credit).

"\nThank you for your help!\n     Ulrich"

"\n\nInternal information:\n")
  (mapc
   #'(lambda (sym) 
       (insert
        (concat (symbol-name sym) "'s value: "
                (if (boundp sym) 
                    (format "%s" (symbol-value sym))
                  "(void)")
                "\n")))
   '(
     emacs-version
     system-type
     system-configuration
     AucTeX-version
     talcum-version
     talcum--versions
     talcum-path
     talcum-desired-features
     ))
  t)

(defalias 'talcum-curse-at-programmer 'talcum-report-bug)

(defun talcum-bug-generate-menu ()
  (list "--" 
        '["Report a bug" talcum-curse-at-programmer]))

(add-to-list 'talcum-menu-functions 
             #'talcum-bug-generate-menu 
             :at-end)

(provide 'talcum-bugreport)
;;;!; talcum-bugreport.el ends here.