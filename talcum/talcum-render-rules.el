;;; $Id: talcum-render-rules.el,v 2.4 2005/03/06 22:22:43 ulmi RELEASE $
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

;; '((type name args feature) action-part)

;; type is one of :macro :environment :active 

;; :macros get a \ prepended, :env.s get a \begin{}, actives are left
;; unchanged

(require 'talcum-tools)

(talcum-namespace
 (~~sourcefile-versionstamp "$RCSfile: talcum-render-rules.el,v $" "$Revision: 2.4 $"))

(talcum-namespace
 (defconst ~~backslash "\\"
   "Regexp matching a backslash.")

 (defun ~~not-after-backslash (re)
   (concat "\\(?:^\\|[^" (regexp-quote ~~backslash) "]\\)" re))


 (defun ~~pattern-part (rule)
   (car rule))
 (defun ~~action-part  (rule)
   (cdr rule))

 (defun ~~pattern-type (pattern)
   (nth 0 pattern))
 (defun ~~pattern-name (pattern)
   (nth 1 pattern))
 (defun ~~pattern-args (pattern)
   (nth 2 pattern))
 (defun ~~pattern-feat (pattern)
   (nth 3 pattern))

 (defun ~~pattern-matcher (pattern)
   "Make a matchable string from the pattern.

Note that we go through a level of regexp-quote before we search in
the buffer."
   (let ((name (~~pattern-name pattern))
	 (type (~~pattern-type pattern)))
     (cond 
      ((eq type ':macro)
       (concat ~~backslash name))
      ((eq type ':environment)
       (concat ~~backslash "begin{" name "}"))
      ((eq type ':active)
       name)
      (t 
       (~~debug 9 "Huh? What's type %s of %s?" type name) 
       nil))))
      
 ;; check a complex specification rule.
 ;; a sr can be conjunctive or disjunctive.
 ;; a csr is true iff all elements are fulfilled
 ;; a dsr is true iff not all elements are not fulfilled
 ;;
 ;; elements of a sr can be:
 ;; - integers : true iff not larger than talcum::thres
 ;; - functions: true iff they return not-nil
 ;; - lists    : lists within a dsr are csrs and vice versa
 ;; - keywords : true iff that keyword is in talcum::present-features
 (defun ~~rule-desired-p (rule &optional disj)
   ;; we introduce boolean ops that guarantee to return t or nil to
   ;; prevent seeping out of information.
   ;; (also, we cannot do (funcall (if pred and or) a b) since and, or are sforms)
   (let ((&& (lambda (a b) (if a (if b t))))
	 (|| (lambda (a b) (if a t (if b t)))))
     (~~rule-desired-p_ (~~pattern-feat (~~pattern-part rule)) disj)))

 ;; This function does the proper work for ~applicable. Splitting it up
 ;; prevents us from defining && and || over and over again.
 ;;
 ;; disj has a double role: it discriminates csr and dsr, and it must
 ;; also be the respective zero, i.e. 
 ;; - nil for conjunction (since nil && x == nil for all x)
 ;; - t   for disjunction (since   t || x == t   for all x)
 (defun ~~rule-desired-p_ (speclist &optional disj)
   (catch 'shortcut
     (let* ((applies? (not disj))
	    (speclist (if (consp speclist) speclist (list speclist))))
       (dolist (feat speclist applies?)
	 (setq applies?
	       (funcall (if disj || &&)
			(cond
			 ((eq t      feat) t)
			 ((eq nil    feat) nil)
			 ((integerp  feat) (>= talcum-render-threshold feat))
			 ((functionp feat) (funcall feat))
			 ((listp     feat) (~~rule-desired-p_ feat (not disj)))
			 ((keywordp  feat) (talcum-has-render-feature feat))
			 (t                (error "unknown case of speclist: %s" feat)))
			applies?))
	 (if (eq applies? disj)
	     (throw 'shortcut applies?))))))

 ;; XXX to be changed to use a hashtable.
(defun talcum-render-features ()
  (interactive)
  (message "%s" talcum-render-features)
  talcum-render-features)

 (defun talcum-has-render-feature (feat)
   (memq feat talcum-render-features))

 (defun talcum-add-render-feature (feat)
   (interactive "SFeature keyword:")
   (add-to-list 'talcum-render-features feat))

 (defun talcum-remove-render-feature (feat)
   (interactive "SFeature keyword:")
   (setq talcum-render-features 
	 (delq feat talcum-render-features)))

); namespace

(provide 'talcum-render-rules)