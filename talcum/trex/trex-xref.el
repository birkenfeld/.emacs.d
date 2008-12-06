;;; $Id: trex-xref.el,v 1.5 2005/03/06 21:39:38 ulmi Exp $
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
(require 'talcum-custom)

(defgroup trex-xref nil
  "Cross-references: index, label, cite, ref and footnote."
  :group 'trexes)

(defcustom trex-xref-rules
  '(((:macro "label" "{" (:xref))
     :display "^"
     :help 0)
    ((:macro "cite" "[{" (:xref))
     :display "[cit]"
     :help 1)
    ((:macro "ref" "{" (:xref))
     :display "/^"
     :help 0)
    ((:macro "eqref" "{" (:xref))
     :display "(/^)"
     :help 0)
    ((:macro "index" "{" (:xref))
     :display "[ix]"
     :help 0)
    ((:macro "footnote" "{" (:xref))
     :display "´"
     :help 0))
  "Rules defined by this trex."
  :group 'trex-xref
  :type talcum-render-rule-custom-type
  :get 'talcum-render-rule-getter
  :set 'talcum-render-rule-setter
  :initialize 'custom-initialize-default)

(setq talcum-render-list (append talcum-render-list 
				 trex-xref-rules))

(talcum-make-render-map-dirty)

(provide 'trex-xref)