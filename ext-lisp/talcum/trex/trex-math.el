;;; $Id: trex-math.el,v 2.6 2005/03/06 21:38:29 ulmi Exp $
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

(defgroup trex-math nil
  "Maths-specific rendering"
  :group 'trexes)

(defface trex-math-operator-face '((t (:weight bold :foreground "dark slate blue")))
  "The face math operators (like sin, cos, etc.) will be displayed in."
  :group 'trex-math)

(setq trex-math-operator-face 'trex-math-operator-face)

(defcustom trex-math-operators
  (list 
   ;; "log-like symbols" from compr. LaTeX symbol list 08OCT2002
   "arccos" "arcsin" "arctan" "arg" "cos" "cosh" "cot" "coth" 
   "csc" "deg" "det" "dim" "exp" "gcd" "hom" "inf"
   "ker" "lg" "lim" "liminf" "limsup" "ln" "log" "max"
   "min" "Pr" "sec" "sin" "sinh" "sup" "tan" "tanh")
  "\"log-like\" math operators"
  :group 'trex-math
  :type '(repeat string))

(defcustom trex-math-render-rules
  '(((:macro "big" "{" (:math))
     :property display (:height 1.2)
     :display 0)
    ((:macro "bigg" "{" (:math))
     :property display (:height 1.3)
     :display 0)
    ((:macro "Big" "{" (:math))
     :property display (:height 1.45)
     :display 0)
    ((:macro "Bigg" "{" (:math))
     :property display (:height 1.6)
     :display 0)
    
    ((:macro "mbox" "{" (:math))
     :property display (:background "#cccccc")
     :display 0)

    ((:macro "left" "{" (:math))
     :property display (:height 2.0 :background "#ccccff")
     :display 0)
    ((:macro "right" "{" (:math))
     :property display (:height 2.0 :background "#ccccff")
     :display 0)
    ((:macro "middle" "{" (:math))
     :property display (:height 2.0 :background "#ccccff")
     :display 0)

    ((:active "\\{" nil (:math))
     :display "{")
    ((:active "\\}" nil (:math))
     :display "}")
    ((:active "*" nil (:math))
     :display "·")

    ((:macro "quad" nil (:math))
     :property display (:background "#ccccff")
     :display "    ")
    ((:macro ";" nil (:math))
     :property display (:background "#ccccff")
     :display "  ")
    )
  "Maths-related rules"
  :group 'trex-math
  :get 'talcum-render-rule-getter
  :set 'talcum-render-rule-setter
  :initialize 'custom-initialize-default
  :type talcum-render-rule-custom-type)

(setq talcum-render-list
      (append talcum-render-list
	      trex-math-render-rules
	      (mapcar
	       (lambda (op)
		 `((:macro ,op nil (:math))
		   :face trex-math-operator-face :display ,op))
	       trex-math-operators)
	      ))
(talcum-make-render-map-dirty)

(provide 'trex-math)
