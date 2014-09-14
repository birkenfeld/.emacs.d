;;; $Id: talcum-tools.el,v 2.8 2005/09/07 18:04:44 ulmi Exp $
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

(defmacro talcum-namespace_ (nspc &rest what)
  ;;   "Make NSPC the namespace prefix in WHAT. You can use ~~ in
  ;; symbols and it will be replaced by NSPC. For example,
  ;; (talcum-namespace \"mess\" (~~age \"foo!\") becomes (progn
  ;; (message \"foo!\"))." 
  (let (rst name expr (what what))

    (while (equal (car-safe (car-safe what)) 'macro)
      (setq what (macroexpand what)))

    (while what
      (setq expr (pop what))
      (cond

       ;; A symbol. Maybe we need to substitute.
       ((symbolp expr)
	(push (if (and (< 1 (length (setq name (symbol-name expr))))
		       (string= "~~" (substring name 0 2)))
		  (intern (concat nspc (substring name 2)))
		expr)
	      rst))

       ;; A non-empty list. We have excluded the case of a macro by
       ;; the loop at the top. Do the car and cdr recursively and put
       ;; them together again. Be careful to strip the added progn and
       ;; list nesting by cadr. (This took a while to get right.)
       ((and (consp expr) (not (listp (cdr expr))))
	(push
	 (let* ((x (cadr 
		    (macroexpand `(talcum-namespace_ ,nspc ,(car expr)))))
		(y (cadr 
		    (macroexpand `(talcum-namespace_ ,nspc ,(cdr expr))))))
	   (cons x y))
	 rst))

       ((consp expr)
	(push (cdr (macroexpand `(talcum-namespace_ ,nspc ,@expr))) rst))

;;        ((consp expr)
;; 	(push 
;; 	 (cons
;; 	  (cadr (macroexpand `(talcum-namespace_ ,nspc ,(car expr))))
;; 	  (cadr (macroexpand `(talcum-namespace_ ,nspc ,(cdr expr)))))
;; 	 rst))

       ;; It's a literal or nil or something like that.
       ;; Just push it over.
       ((push expr rst))))

    (cons 'progn (nreverse rst))))

;; The default namespace for internal Talcum use is "\ talcum\ \ ".
;; Identifier names with spaces in them should be pretty rare, so
;; we will probably not collide.
(defmacro talcum-namespace (&rest what)
  `(talcum-namespace_ " talcum  " ,@what))

;; We might want for some reason to disable the namespacing.
(talcum-namespace
 (defmacro ~~no-namespace (&rest what)
   `(talcum-namespace_ "~~" ,@what)))


(talcum-namespace
 
;;; write one save, get save-excursion, -match-data, etc. We do not
;;; actually check any semantics, or the presence of the corresponding
;;; save-foos. 
;;;
;;; (save (foo bar baz) bang) expands to (save-foo (save-bar (save-baz bang)))
 (defmacro ~~save (things &rest cmds)
   (let (result
	 (things (nreverse things)))
     (setq result (cons (intern (concat "save-" 
					(symbol-name (pop things))))
			cmds))
     (dolist (thing things result)
       (setq result (list (intern (concat "save-" 
					  (symbol-name thing))) 
			  result)))))



 (defmacro ~~append! (l &rest ls)
   "Append all args and setq the first to the result."
   `(setq ,l (append ,l ,@ls)))

 (defmacro ~~point-after (fu)
   "Execute FU and return the (possibly new) value of point."
   `(progn ,fu (point)))
);namespace
(talcum-namespace
 (defmacro ~~without-touching-buffer (&rest cmds)
   "Do something without touching the buffer, i.e. it is read-only.
Just because we're extra paranoid and this is probably not very expensive."
   `(let ((buffer-read-only t)
	  (inhibit-read-only nil)
	  (inhibit-redisplay t)
	  (inhibit-point-motion-hooks t)
	  (inhibit-modification-hooks t))
      ,@cmds))

 (defmacro ~~with-secretly-touching-buffer (&rest cmds)
   "Modify the current buffer, but do not change read-only or
buffer-modified flags. This is for the rare situation where you want
to override without-touching-buffer."
   `(let ((inhibit-read-only t)
	  (deactivate-mark nil)
	  (was-virgin (buffer-modified-p)))
      (prog1 (progn ,@cmds) (set-buffer-modified-p was-virgin))))
 );namespace
(talcum-namespace

 (defun ~~scantoken (&optional kind)
   "Move point forward by a token, and return the token. Whitespace
after the token is skipped, just as (La)TeX does. Optional parameter
KIND can be 'cmd or 'env; if 'env, we actually skip a whole \\begin{foo}."
   (let ((bot (point))
	 (eot nil)
	 (forward-sexp-function nil) ;;latex-mode thinks it's clever. It's not.
	 )

     (skip-chars-forward " \t")
     (if (eq kind 'env)
	 (~~scantoken 'cmd))		; get the "\begin"
     (setq eot
	   (cond
	    ((looking-at "\\\\")	; a control sequence proper
	     (forward-char 1)
	     (if (not (looking-at "[a-zA-Z]")) ;; self-terminating: \$ etc.
		 (~~point-after (forward-char 1))
	       (skip-chars-forward "a-zA-Z") ;; proper command. skip trailing blanks.
	       (prog1 
		   (point)
		 (skip-chars-forward " \t"))))
	    ((looking-at "{") 
	     (condition-case nil
		 (~~point-after 
		  (forward-sexp))
	       (scan-error (setq eot (1+ bot)))
	       )) ;FIXME
	    (t
	     (~~point-after (forward-char 1)))))

     (buffer-substring bot eot)))

 (defun ~~fwd-arg ()
   "Go forward until a semi-properly nested exp has been moved over.
This means there may be spurious non-matching closing parens."
   (catch 'fail
     (let (stack)
       (skip-syntax-forward "^(")
       (push (char-after) stack)
       (forward-char 1)
       (while stack
	 (skip-syntax-forward "^()")
	 (if (looking-at "\\s(")
	     (push (char-after) stack)
	   (if (equal (char-after) 
		      (cdr (aref (syntax-table) (car stack))))
	       (pop stack)
	     (if (eobp) (throw 'fail nil))))
	 (forward-char 1))
       t)))
);namespace
;;;;
;;;; Multimaps 
;;;;

(talcum-namespace
 ;; create and return a new multimap.
 (defun ~~mmap-new ()
   (make-hash-table :test 'equal))

 ;; put a key-value entry into a multimap. 
 ;; This does not remove current entries in the multimap, 
 ;; even if they have the same key.
 (defun ~~mmap-put (key val mmap)
   (puthash key (cons val (gethash key mmap nil)) mmap))

 ;; look up all entries for a key in a multimap.
 ;; returns nil if none present.
 (defun ~~mmap-get (key mmap)
   (gethash key mmap nil))

 (defun ~~mmap-keys (mmap)
   (let (result)
     (maphash (lambda (key val) (push key result)) mmap)
     result))
); namespace

;;;;
;;;; Scoped Variables.
;;;;

(talcum-namespace
(defun ~~var-scope-start (name)
  (let ((stack (get name 'talcum-varstack)))
    (if (boundp name) 
	(push (symbol-value name) stack))
      ;(make-local-variable name)) nope, render-string opens tempbuffer
    (put name 'talcum-varstack stack)
    (set name nil)))

(defun ~~var-scope-end (name)
  (let ((stack (get name 'talcum-varstack)))
    (put name 'talcum-varstack (cdr-safe stack))
    (if (null stack)
	(makunbound name)
      (set name (car-safe stack)))))

(defun talcum-var-history (name)
  (let ((stack (get name 'talcum-varstack)))
    (and (boundp name)
	 (cons (symbol-value name) stack)))) 

);namespace
;;;;
;;;; Generic tools.
;;;;

(defcustom talcum-verbosity 5
  "*How much going-on Talcum should send to *Messages*. 
0 means everything (good for debugging), 10 is silence of the lambdas."
  :group 'talcum
  :type '(integer))


(talcum-namespace
 (defun ~~filter (p xs)
   (let ((r nil))
     (if (not (listp xs)) 
	 (error "Trying to filter %S which is not a list" xs)
       (if (not (functionp p))
	   (error "Trying to filter by %S which is not a function" p)
	 (while xs
	   (let ((x (car xs))
		 (xss (cdr xs)))
	     (setq r (if (funcall p x) (cons x r)
		       r))
	     (setq xs xss)))
	 (nreverse r)))))

 (defun ~~debug (severity &rest args)
   "Do debugging output, depending on talcum-verbosity."
   (if (>= severity talcum-verbosity)
       (apply 'message args)))


 (defun ~~sourcefile-versionstamp (id rev)
   (string-match "\\([0-9]+\\)\\.\\([0-9]+\\)\\.\\([0-9]+\\)" talcum-version)
   (let* ((major  (string-to-number (match-string 1 talcum-version)))
          (minor  (string-to-number (match-string 2 talcum-version)))
          (commit (string-to-number (match-string 3 talcum-version)))
          (thiscommit (progn
                        (string-match ".Revision: [0-9]+.\\([0-9]+\\) \$$" rev)
                        (string-to-number (match-string 1 rev)))))
     (setq talcum--versions 
           (cons (list id rev)
                 (if (boundp 'talcum--versions) 
                     talcum--versions 
                   nil))
           talcum-version (format "%d.%d.%d" major minor (+ thiscommit commit))
           )))


 (defmacro ~~switch (expr &rest choices)
   "Cond template. Each element of CHOICES is compared against the value of EXPR in a cond.

Silly example:  
(~~switch 9
    (7 :sieben)
    (8 :acht)
    (9 :neun))  ==> :neun "
   (let ((temp (make-symbol "--switchvar--")))
  `(let* ((,temp ,expr))
     ,(cons 'cond
	    (mapcar 
	     (lambda (choice)
	       (if (consp choice)
		   `((equal ,temp ,(car choice))
		     ,(cadr choice))
		 `(t ,choice)))
	     choices)))))

 (defmacro ~~value-safe (symbol)
   (if (boundp symbol) symbol
     nil))
       

 (~~sourcefile-versionstamp "$RCSfile: talcum-tools.el,v $"  "$Revision: 2.8 $")

); namespace

(provide 'talcum-tools)

;;; talcum-tools.el end here