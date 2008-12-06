;;; $Id: trex-lists.el,v 2.5 2005/03/06 21:37:55 ulmi Exp $
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

(defun trex-lists-arabic (i) (number-to-string i))
(defun trex-lists-alph   (i) (char-to-string (+ 96 i)))
(defun trex-lists-Alph   (i) (upcase (trex-lists-alph i)))

;; Let's hope nobody has lists with more than 39 roman items...
(defun trex-lists-roman (i)
  (let* ((result "")
	 (i i))
    (while (>= i 10)
      (setq result (concat result "x")
	    i (- i 10)))
    (concat result
	    (nth i '("" "i" "ii" "iii" "iv" "v" "vi" "vii" "viii" "ix")))))

(defun trex-lists-Roman (i) (upcase (trex-lists-roman i)))

(setq talcum-render-list
      (append
       talcum-render-list
       '(((:environment "enumerate" "" (:lists))
	  :var (trex-lists-nesting . (:enum 0))
	  :recurse)
	 ((:environment "itemize" "" (:lists))
	  :var (trex-lists-nesting . (:item 0))
	  :recurse)
	 ((:environment "description" "" (:lists))
	  :var (trex-lists-nesting . (:desc 0))
	  :recurse)
	 ((:macro "item" "[" (:lists))
	  :display trex-lists-spitzmarke)
	 )))
(talcum-make-render-map-dirty)


(defun trex-lists-spitzmarke (macparams)

  (if (not (boundp 'trex-lists-nesting))
      ;; Whoopsie! We're not in scope. Fallback.
      "** "
    (if (not (string= "" (nth 0 macparams)))
	;; Explicitly given by user, use that
	(talcum-namespace (~~maybe-strip-parens (nth 0 macparams)))

      ;; all else fails: think for ourselves.
      (let* ((list-type (car-safe trex-lists-nesting))
	     (nest-depth 0))
	
	(setq nest-depth 
	      (dolist (n (talcum-var-history 'trex-lists-nesting) nest-depth)
		(if (equal list-type (car-safe n))
		    (setq nest-depth (1+ nest-depth))))
	      trex-lists-nesting (list (car trex-lists-nesting) 
				       (1+ (cadr trex-lists-nesting))))

	(cond
	 ((equal :enum list-type)
	  (funcall (nth nest-depth trex-lists-enum-style) 
		   (cadr trex-lists-nesting)))

	 ((equal :item list-type)
	  (funcall (nth nest-depth trex-lists-item-style)))

	 ((equal :desc list-type)
	  ;; An item of a description has an arg.
	  "[nodesc?]")

	 (t
	  "??"))))))
   

(defgroup trex-lists nil
  "Rendering itemize, enumerate and description."
  :group 'trexes)

(defcustom trex-lists-enum-style
  (list 'ignore ;; dummy element
    (lambda (i) (concat (trex-lists-arabic i) ". "))
    (lambda (i) (concat (trex-lists-alph i) ") "))
    (lambda (i) (concat (trex-lists-roman i) ". "))
    (lambda (i) (concat (trex-lists-Alph i) ". "))
    )
  "Functions that yield a list bullet with a number, one for each
nesting depth." 
  :group 'trex-lists
  :type '(list
	  (const ignore)
	  function
	  function
	  function
	  function
	  ))

(defcustom trex-lists-item-style
  '(ignore ;; dummy element
    (lambda () "o ")
    (lambda () "- ")
    (lambda () "* ")
    (lambda () ". ")
    )
  "Functions that yield a list bullet without a number, one for each
nesting depth."
  :group 'trex-lists
  :type '(list
	  (const ignore)
	  function
	  function
	  function
	  function
	  ))


(provide 'trex-lists)
