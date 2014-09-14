;;; $Id: talcum-menu.el,v 1.5 2005/08/26 18:37:41 ulmi Exp $
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

;;; Finally: a clickable menu. 
;;;
;;; The entries are generally runtime-generated. This would not be
;;; necessary for all kinds of menu, but for some (think chapter
;;; selection), and I think it should not pose a big run-time
;;; overhead.

(require 'talcum-tools)
(require 'talcum-custom)

(talcum-namespace
 (~~sourcefile-versionstamp "$RCSfile: talcum-menu.el,v $"  "$Revision: 1.5 $"))

(talcum-namespace

 (defmacro ~~negate (var)
   `(setq ,var (not ,var)))

 ;;; Some common menu helpers
 ;;;

 ;;; A checkbox for a variable.
 (defmacro ~~menu-flag-toggle (var str &optional more)
   `[,str
     (talcum-namespace (~~negate ,var) ,more)
     :style toggle
     :selected ,var
     ])

 ;;; A radio field. Names come from the cars, values come from the
 ;;; cadrs.
  (defmacro ~~menu-choose-one (var names-and-vals)
    `(mapcar 
      (lambda (nav)
        (vector (car nav) 
                (list 'setq ',var (cadr nav))
                :style 'radio
                :selected (list 'equal ',var (cadr nav))))
      ,names-and-vals))

 (defvar talcum-menu-functions nil
   "A list of functions. Each function is called with no arguments.
The result should be a menu item definition. All results are
concatenated and used as the Talcum menu.")

 ;; Not sure if this will be needed. Menus are strange, unclear if
 ;; it's safe to change talcum-menu's value directly.
 (defun talcum-menu-regenerate ()
   (interactive)
   (easy-menu-define talcum-menu talcum-mode-map
     "Talcum's menu."
     (cons "Talcum"
           (apply 'append
            (mapcar 'funcall talcum-menu-functions))))
   t)

 (talcum-menu-regenerate)

) ; namespace
(provide 'talcum-menu)