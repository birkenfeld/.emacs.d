;;; $Id: trex-markup.el,v 1.4 2005/03/06 21:38:14 ulmi Exp $
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
(require 'talcum-render)

(defgroup trex-markup
  nil
  "Basic markup"
  :group 'trexes)

(defcustom trex-markup-macros
  '("textbf" "textit" "textsc" 
	   "textrm" "textsf" "texttt"
	   "emph")
  "Basic markup macros. These take one argument, and that will be
displayed."
  :group 'trex-markup
  :type '(repeat string))

(dolist (rule trex-markup-macros)
  (add-to-list 'talcum-render-list 
	       `((:macro ,rule "{" (:markup))
		 :display (:rendered 0))))
(talcum-make-render-map-dirty)

(provide 'trex-markup)