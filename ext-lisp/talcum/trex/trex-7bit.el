;;; $Id: trex-7bit.el,v 1.2 2005/03/06 21:37:27 ulmi Exp $
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
(require 'talcum-render)

(defvar trex-7bit-mappings
  '(("\"a" . "ä")
    ("\"o" . "ö")
    ("\"u" . "ü")
    ("\"s" . "ß")
    ("\"A" . "Ä")
    ("\"O" . "Ö")
    ("\"U" . "Ü")))

(dolist (ch trex-7bit-mappings)
  (setq talcum-render-list
	(cons `((:active ,(car ch) nil (:7bit :german))
		:display ,(cdr ch))
	 talcum-render-list)))

(talcum-make-render-map-dirty)

(provide 'trex-7bit)