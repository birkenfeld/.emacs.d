;;; $Id: trex-unicode.el,v 2.7 2005/03/06 21:38:45 ulmi Exp $
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

(defgroup trex-unicode nil
  "Rendering macros as unicode characters"
  :group 'trexes)

(defface trex-unicode-face '((t (:slant normal)))
  "A face known to contain many unicode glyphs. 

This face is applied to characters by :unicode rules to prevent them
from getting lost in italics, boldface, etc."
  :group 'trex-unicode)

(setq trex-unicode-face 'trex-unicode-face)

(defcustom trex-unicode-mappings
  '( ;; format is (macro . utf16code)
    ;; Unicode Math Ops 2200-22FF
    ;; Much still missing.
    ("forall"      . #x2200)
    ("complement"  . #x2201)
    ("partial"     . #x2202)
    ("exists"      . #x2203)
    ("emptyset"    . #x2205)
    ("nabla"       . #x2207)
    ("in"          . #x2208)
    ("notin"       . #x2209)
    ("ni"          . #x220b)
    ("qedhere"     . #x220e)
    ("prod"        . #x220f)
    ("coprod"      . #x2210)
    ("sum"         . #x2211)
    ("mp"          . #x2213)
    ("setminus"    . #x2216)
    ("circ"        . #x2218)
    ("cdot"        . #x2219)
    ("sqrt"        . #x221a)
    ("infty"       . #x221e)
    ("land"        . #x2227)
    ("wedge"       . #x2227)
    ("lor"         . #x2228)
    ("vee"         . #x2228)
    ("cap"         . #x2229)
    ("cup"         . #x222a)
    ("int"         . #x222b)
    ("iint"        . #x222c)
    ("iiiint"      . #x222d)
    ("neq"         . #x2260)
    ("ne"          . #x2260)
    ("leq"         . #x2264)
    ("le"          . #x2264)
    ("geq"         . #x2265)
    ("ge"          . #x2265)
    ("prec"        . #x227a)
    ("succ"        . #x227b)
    ("subset"      . #x2282)
    ("supset"      . #x2283)
    ("subseteq"    . #x2286)
    ("supseteq"    . #x2287)
    ("subsetneq"   . #x228a)
    ("supsetneq"   . #x228b)
    ("unlhd"       . #x22b4)
    ("lhd"         . #x22b2)
    ("unrhd"       . #x22b5)
    ("rhd"         . #x22b3)
    ;; From U27F0-U27FF Supplemental Arrows A
    ("implies"     . #x27f9)
    ("iff"         . #x27fa)
    ("mapsto"      . #x27fc)
    ;; to is uncertain: function-goes-to is long, limit-to short
    ("to"            . #x27f6)
    ("longleftarrow" . #x27f5)
    ("longrightarrow"     . #x27f6)
    ("longleftrightarrow" . #x27f7)
    ("Longleftarrow"      . #x27f8)
    ("Longrightarrow"     . #x27f9)
    ;; From U2190-U21FF Arrows
    ("leftarrow"          . #x2190)
    ("uparrow"            . #x2191)
    ("rightarrow"         . #x2192)
    ("downarrow"          . #x2193)
    ("leftrightarrow"     . #x2194)
    ("updownarrow"        . #x2195)
    ;; many omitted...
    ;; From U2000ff General Punctuation
    ("dots"               . #x2026)
    ("ldots"              . #x2026)
    ("textperthousand"    . #x2030)
    ;; From 27D0ff Misc Math Symb A
    ;; currently all omitted
    ;; From U2A00ff Supp Math Op
    ("bigodot"            . #x2a00)
    ("bigoplus"           . #x2a01)
    ("bigotimes"          . #x2a02)
    ("lneq"               . #x2a87)
    ("gneq"               . #x2a88)
    ;;
    ("wp" . #x2118)
    ("ell" . #x2113)
    ("Im" . #x2111)
    ("Re" . #x211c)
    ("Finv" . #x2132)
    ("Game" . #x2141)
    ("aleph" . #x2135)
    ("beth" . #x2136)
    ("gimel" . #x2137)
    ("daleth" . #x2138)
    ;; Greekery. Note it is not technically correct to use
    ;; the low chars, but who has fonts that have the 1D400
    ;; Math Alnums? (Italic bold sans-serif fraktur
    ;; lowercase greek letter chi my ass!)
    ("alpha" . #x03b1)
    ("beta" . #x03b2)
    ("gamma" . #x03b3)
    ("delta" . #x03b4)
    ("epsilon" . #x03f5)
    ("varepsilon" . #x03b5)
    ("zeta" . #x03b6)
    ("eta" . #x03b7)
    ("theta" . #x03b8)
    ("vartheta" . #x03d1)
    ("iota" . #x03b9)
    ("kappa" . #x03ba)
    ("varkappa" . #x03f0)
    ("lambda" . #x03bb)
    ("mu" . #x03bc)
    ("nu" . #x03bd)
    ("xi" . #x03be)
    ("pi" . #x03c0)
    ("varpi" . #x03d6)
    ("rho" . #x03c1)
    ("varrho" . #x03f1)
    ("sigma" . #x03c3)
    ("varsigma" . #x03c2)
    ("tau" . #x03c4)
    ("upsilon" . #x03c5)
    ("varphi" . #x03c6)
    ("phi" . #x03d5)
    ("chi" . #x03c7)
    ("psi" . #x03c8)
    ("omega" . #x03c9)
    ("digamma" . #x03dd) ;;XXX is \digamma lowercase?
    ("Gamma" . #x0393)
    ("Delta" . #x0394)
    ("Theta" . #x0398)
    ("Lambda" . #x039b)
    ("Xi" . #x039e)
    ("Pi" . #x03a0)
    ("Sigma" . #x03a3)
    ("Upsilon" . #x03a5)
    ("Phi" . #x03a6)
    ("Psi" . #x03a8)
    ("Omega" . #x03a9)
    )
  "An alist of macro-to-utf16-code mappings."
  :group 'trex-unicode
  :type '(repeat
	  (cons 
	   (string)
	   (integer)))
  )

(defun trex-unicode-make-sample ()
  (interactive)
  (dolist (c trex-unicode-mappings)
    (insert "\\" (car c)
	    " ~= " (decode-char 'ucs (cdr c))
	    "\n"))
  (insert "\nand the number sets:\n")
  (dolist (c (list "C" "H" "N" "P" "Q" "R" "Z"))
    (insert c " ~= " (trex-unicode-mathbb (list c)) "\n"))
  t)


(defun trex-unicode-init ()
  ;;   (font-lock-add-keywords 
  ;;    nil
  ;;    '(("\\\\[a-zA-Z@]+" 0 trex-unicode-face prepend)) 
  ;;    'append)
  t)
(add-hook 'talcum-rex-init-hook 'trex-unicode-init 'atend)

(defun trex-unicode-display-function (spec args)
  (when (eq (car-safe spec) ':unicode) 
    (propertize
     (char-to-string (decode-char 'ucs 
				  (cadr spec)))
     ':face 'trex-unicode-face
     'fontified t)))

(add-hook 'talcum-display-spec-functions
	  'trex-unicode-display-function)

(dolist (c trex-unicode-mappings)
  (setq talcum-render-list
	(cons `((:macro ,(car c) nil (:unicode))
		:face trex-unicode-face
		:display (:unicode ,(cdr c)))
	      talcum-render-list)))

(setq talcum-render-list
      (cons 
       '((:macro "mathbb" "{" (:unicode))
	 :display trex-unicode-mathbb)
       talcum-render-list))

(talcum-make-render-map-dirty)


(defun trex-unicode-mathbb (args)
  (talcum-namespace
   (or
    (~~switch (~~maybe-strip-parens (car-safe args))
	      ("C" (char-to-string (decode-char 'ucs #x2102)))
	      ("H" (char-to-string (decode-char 'ucs #x210d)))
	      ("N" (char-to-string (decode-char 'ucs #x2115)))
	      ("P" (char-to-string (decode-char 'ucs #x2119)))
	      ("Q" (char-to-string (decode-char 'ucs #x211a)))
	      ("R" (char-to-string (decode-char 'ucs #x211d)))
	      ("Z" (char-to-string (decode-char 'ucs #x2124))))
    (car-safe args)
    "?")))

(provide 'trex-unicode)