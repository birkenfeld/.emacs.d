;;; $Id: trex-verbatim.el,v 1.2 2005/03/06 21:39:00 ulmi Exp $
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

(defun trex-verbatim-parse-arg-any-repeated-delimiter (wanted)
  (when (equal wanted ':any-repeated)
    (let ((delim-char (char-after))
	  (b (point)))
      (forward-char 1)
      (if (search-forward (char-to-string delim-char) (point-max) t)
	  (buffer-substring b (point))
	(goto-char b)))))

(add-hook 'talcum-parse-argument-functions 
	  'trex-verbatim-parse-arg-any-repeated-delimiter)

(defvar trex-verbatim-render-rules
  '(((:macro "verb" (:star :any-repeated) :verbatim)
     :face font-lock-constant-face
     :display (lambda (args) (substring (cadr args) 1 -1)))
    ((:environment "verbatim" nil :verbatim)
    :face font-lock-constant-face
    :display body)
    ))


(setq talcum-render-list
      (append trex-verbatim-render-rules
	      talcum-render-list))
(talcum-make-render-map-dirty)
	      

(provide 'trex-verbatim)