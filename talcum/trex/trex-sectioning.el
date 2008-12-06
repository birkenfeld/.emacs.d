;;; $Id: trex-sectioning.el,v 1.2 2005/03/06 22:23:10 ulmi RELEASE $
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

(defvar trex-sectioning-section-commands
  '("part" "chapter" "section" 
    "subsection" "subsubsection"
    "paragraph" "subparagraph"
    ))

(dolist (sec trex-sectioning-section-commands)
  (setq talcum-render-list
	(cons `((:macro ,sec "*[{" :sectioning)
		:display 2)
	 talcum-render-list)))

(talcum-make-render-map-dirty)

(provide 'trex-sectioning)