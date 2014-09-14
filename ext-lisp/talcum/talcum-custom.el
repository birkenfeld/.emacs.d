;;; $Id: talcum-custom.el,v 1.4 2005/02/19 18:13:59 ulmi Exp $
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

(defconst talcum-render-rule-custom-type
  '(repeat				; many rules
    (list :tag "rule"			; one rule
	  (list :tag "pattern part"	; the pattern part

		(choice :tag "type"	; the type part
			:value :macro
			(const :value :macro)
			(const :value :environment)
			(const :value :active))

		(string :tag "name"
			:value "textbf") ; the name

		(choice :tag "argspec"	; the argspec
			:value "*[{"
			(string :tag "short form"
				:value "*[{") ; short argspec

			(repeat :tag "long form" 
				(symbol :value :mandatory) ; long argspec
				))
		(choice :tag "featspec" ; featspec

			;; FIXME: featspec needs extra attention to disambiguate
			;; feature keywords and function results; likewise for
			;; "never" rule and nested lists? Or drop nil rule altogether? 
			;; OTOH, empty lists aren't really meaningful here

			(const :tag "always" 
			       :value t) ; - success
			(const :tag "never" 
			       :value nil) ; - failure
			(integer :tag "render-level"
				 :value 3) ; - render-thres
			(list :tag "feature"
			      (const :value --feature)
			      (symbol :tag "feature"
				      :value ":trex-my-feature")
			      )		; - feature
			(list :tag "ask predicate"
			      (const :value --function)
			      (function :tag "ask predicate"
					:value ignore)) ; - function
			(repeat :tag "all of the following" ; - recursive featspec
				(choice :tag "all of the following"
				 (const :tag "always" 
					:value t) ; -- success
				 (const :tag "never" ; -- failure 
					:value nil)
				 (integer  :tag "render-level"
					   :value 3) ; -- render-thres
				 (list :tag "feature"
				       (const :value --feature)
				       (symbol :tag "feature"
					       :value ":trex-my-feature")
				       ) ; -- feature
				 (list :tag "ask predicate"
				       (const :value --function)
				       (function :tag "ask predicate"
						 :value ignore)) ; -- function
				 (repeat :tag "any of the following" ; -- recursive featspec ii
					 (choice
					  (const :tag "always" t) ; --- success
					  (const :tag "never" nil) ; --- failure
					  (integer  :tag "render-level"
						    :value 3) ; --- render-thres
					  (list :tag "feature"
						(const :value --feature)
						(symbol :tag "feature"
							:value ":trex-my-feature") 
						) ; --- feature
					  (list :tag "ask predicate"
						(const :value --function)
						(function :tag "ask predicate"
							  :value ignore)) ; --- function
					  (repeat :tag "deeper nesting (freeform)"
						  (sexp))))
				 ))
			))

	  (repeat			;the action part
	   :tag "action part"
	   :value (:display "*")

	   (choice
	    :value (:display "*")

	    (list :tag "display"
		  :value (:display "*")

		  (const :value :display)
		  (choice		; proper display flavour
		   :value "*"

		   (string :tag "fixed text"
			   :value "*")
		   (integer :tag "(n+1)th argument"
			    :value 0)
		   (const :tag "body of environment" 
			  :value body)
		   (list :tag "(rendering recursively)"
			 (const :tag ":rendered" 
				:value :rendered)
			 (choice	; recursive display flavour
			  :value "*"
			  (string :tag "fixed text" 
				  :value "*")
			  (integer :tag "(n+1)th argument" 
				   :value 0)
			  (const :tag "body of environment" 
				 :value body body)
			  (function :tag "result of function" 
				    :value ignore)))
		   (function :tag "result of function"
			     :value ignore)))

	    (list :tag "evaluate anything"
		  :value nil
		  (const :value :eval)
		  (sexp))

	    (list :tag "force face"
		  (const :value :face)	; face action
		  (face))

	    (list :tag "fly-over help"
		  (const :value :help)
		  (choice		; display flavour
		   (string :tag "fixed text"
			   :value "*")
		   (integer :tag "(n+1)th argument"
			    :value 0)
		   (const  :tag "body of environment" 
			   :value body body)
		   (function)))

	    (list :tag "render-opener"
		  (const 
		   :value :opener)
		  (function 
		   :value ignore))

	    (list :tag "property"
		  (const 
		   :value :property)
		  (symbol :tag "name"
			  :value prop)
		  (sexp :tag "value"
			:value nil))

	    (list :tag "local variable"
		  :value my-local-var
		  (const :value :var)
		  (choice
		   (variable :tag "name"
			     :value my-local-var)
		   (cons :tag "with default value"
			 (variable :tag "name"
				   :value my-local-var) 
			 (sexp :tag "value"
			       :value ""))))
	    ))
	  ))
  "A custom-compatible semi-representation of rules. Only works in
conjunction with the special getter talcum-render-rule-getter and
setter talcum-render-rule-setter, which see."
  )


;; Custom vars for rules should look like this:
;;  (defcustom foorules nil "" :get 'talcum-render-rule-getter :set 'talcum-render-rule-setter :type talcum-render-rule-custom-type)

(defconst talcum-render-actions-arities
  '((:display . 1)
    (:eval     . 1)
    (:face     . 1)
    (:help     . 1)
    (:opener   . 1)
    (:property . 2)
    (:var      . 1))
  "Alist that gives the number of arguments for each display flavour.

Not keeping this list up-to-date will prevent customize from working
for unlisted flavours." )

;;;
;;; Circumvent limitations of customization interface.
;;;
;;; Here be dragons!
;;; At least, -getter isn't one sexp with nested mapcar lambdas anymore.

(talcum-namespace

 ;; FIXME: this should set dirty flag?
 (defun talcum-render-rule-setter (nam val)
   "Customize-setter for render rule lists."
   (set nam
	(mapcar (lambda (l) (cons (~~render-rule-pattern-setter (car l))
				  (apply 'append 
					 (apply 'append (cdr l)))))
		val)))

 (defun talcum-render-rule-getter (nam)
   "Customize-getter for render rule lists."
   (mapcar (quote ~~render-rule-getter1)
	   (symbol-value nam)))

 (defun ~~render-rule-getter1 (rule)
   (let ((rule rule)
	 pattern
	 newactions)
     (setq pattern (~~render-rule-pattern-getter (pop rule))) ; pattern part
     ;; now treat the action part
     (while rule
       (let ((action nil)
	     i)
	 (push (pop rule) action) ;; action name
	 (dotimes (i (cdr (assoc (car action) 
				 talcum-render-actions-arities)))
	   (push (pop rule) action))
	 (push (nreverse action) newactions)))
     (list pattern (nreverse newactions))))

 (defun ~~render-rule-pattern-getter (pattern)
   (list (nth 0 pattern)
	 (nth 1 pattern)
	 (nth 2 pattern)
	 (~~render-rule-pattern-getter1 (nth 3 pattern))))

 (defun ~~render-rule-pattern-getter1 (pattern)
   (cond
    ((functionp pattern) (list '--function pattern))
    ((listp pattern)    (mapcar (quote ~~render-rule-pattern-getter1) 
				pattern))
    ((keywordp pattern) (list '--feature pattern))
    (t pattern)))

 (defun ~~render-rule-pattern-setter (pattern)
   (list (nth 0 pattern)
	 (nth 1 pattern)
	 (nth 2 pattern)
	 (~~render-rule-pattern-setter1 (nth 3 pattern))))

 (defun ~~render-rule-pattern-setter1 (pattern)
   (cond
    ((listp pattern)
     (if (member (car pattern) '(--feature --function))
	 (cadr pattern)
       (mapcar (quote ~~render-rule-pattern-setter1) 
	       pattern)))
    (t pattern)))
 )					; namespace

(talcum-namespace
 (~~sourcefile-versionstamp "$RCSfile: talcum-custom.el,v $" "$Revision: 1.4 $"))

(provide 'talcum-custom)