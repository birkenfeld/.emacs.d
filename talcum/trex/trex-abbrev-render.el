;;; $Id: trex-abbrev-render.el,v 2.0 2005/01/25 19:42:01 ulmi Exp $
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

(add-hook 'talcum-rex-init-hook 
	  'trex-abbrev-render-init 'at-end)

(defun trex-abbrev-render-init ()
  (abbrev-mode 1)
  (add-hook 'pre-abbrev-expand-hook 
	    'trex-render-when-abbrev 'at-end 'local))

(defvar trex-render-abbrev-sytab nil
  "Currently unused.")

;; XXX if this is too slow, have a static sytax-table for this.
(defun trex-render-when-abbrev ()
  "Callable by pre-abbrev-expand-hook. Tries to render the word at
point. This does not catch all by far, but it catches commands without
args."
    (let* ((words-include-escapes t)
	   (rng (bounds-of-thing-at-point 'word))
	   (inhibit-point-motion-hooks t))
      (talcum-render-region (car rng) (cdr rng))))

(provide 'trex-abbrev-render)