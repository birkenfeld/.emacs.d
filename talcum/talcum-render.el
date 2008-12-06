;;; $Id: talcum-render.el,v 2.20 2005/08/28 09:00:49 ulmi Exp $
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
(require 'talcum-render-ui)
(require 'talcum-render-rules)

(talcum-namespace
 (~~sourcefile-versionstamp "$RCSfile: talcum-render.el,v $" "$Revision: 2.20 $"))

(defvar talcum-default-actions
  '(:property category 
	      talcum-rendered
    :property evaporate t
    :property priority 500 ;; default for show-paren-prio is 1000
;    :opener talcum-make-render-opener
     ))

 (defvar talcum-rex-init-hook nil
   "Hooks called at init time, before the rendering structures are
initialized.")

(talcum-namespace
 (defvar ~~match-any-re)

 (defvar ~~rendermap (~~mmap-new))

 (defun talcum-render-string (str)
   "Add renderings to a string and return the result."
   (if (or (null str) 
	   (string= str ""))
	"`"
     (with-temp-buffer
       (with-syntax-table ~~mode-syntax-table
	 (insert str)
	 (talcum-render-region (point-min) (point-max) 'REPLACE)
	 ;; We're actually carrying font-lock info all along, so don't
	 ;; throw it away now :-D
	 ;; (buffer-substring-no-properties (point-min) (point-max))
	 (buffer-string)))))

     
 ;; Go through the region and call ~~maybe-apply-rule-at-point for
 ;; each match of any item according to the current ~~match-any-re.
 ;; ~~maybe-apply-rule-at-point must check itself whether the rule is
 ;; applicable. We leave point just after the match, i.e. after the
 ;; closing brace of a \begin{} or after the last char of a command.
 ;; 
 ;; We save match data and excursion, so the rules are free to move
 ;; around and do stuff.
 ;;
 ;; The region to render is actually extended to the next following
 ;; non-word character (or end of buffer) for technical reasons.
 (defun talcum-render-region (bor eor &optional replace?)
   (interactive "r")

   ;; Init thingy: if render-list is dirty, rebuild.
   (if (get (quote ~~rendermap) 'talcum-map-is-dirty)
       (talcum-render-reinit))

   (when (and talcum-use-render-flag
	      (< bor eor))
     (~~save (excursion restriction)
       (~~without-touching-buffer

	(unless (~~value-safe ~~user-point)
	  (setq ~~user-point (point)))

	(let* ((bor bor)
	       ;; make sure we can see the character after a macro end, to
	       ;; check if it's to be rendered.
	       (eor (save-excursion
		      (goto-char eor)
		      (skip-syntax-forward "w")
		      (or (eobp)
			  (forward-char 1))
		      (point)))
	       (final-point (copy-marker bor)) ;; always move forward

	       new-end-or-nil
	       matching-rules)
       
	  (goto-char bor)

	  (while (search-forward-regexp 
		  ~~match-any-re 
		  eor 'end-and-nil)

	    (setq matching-rules 
		  (~~mmap-get (concat (match-string 1) 
				      (match-string 2)) 
			      ~~rendermap)
		  new-end-or-nil nil)
	    (move-marker final-point (or (match-end 1) (match-end 2)))

	    (dolist (rule matching-rules)
	      (unless new-end-or-nil
		(~~save (excursion match-data)
			(setq new-end-or-nil
			      (~~maybe-apply-rule-at-point rule eor replace?))
			(if new-end-or-nil
			    (move-marker final-point new-end-or-nil)))))
	    (goto-char final-point))

	  (setq ~~user-point nil)
	  (set-marker final-point nil)
	  t)))))
   
 ;; Check and apply RULE at this point. 
 ;;
 ;; When called, we have point just after the matcher string,
 ;; match-data slot MATCH points at the match. We may move point and trash
 ;; match-data. By setting premature-abort to non-nil, we may prevent
 ;; other rules from looking at this as well.
 ;;
 ;; return what we think is a good point for rendering to continue.
 (defun ~~maybe-apply-rule-at-point (rule scopelimit &optional replace?)
   (let* (args 
	  envlimit
	  (pattern (~~pattern-part rule))
	  (argspec (~~pattern-args pattern))
	  (MATCH (if (equal ':macro (~~pattern-type pattern))
		     1 
		   2))
	  (startofpattern (match-beginning MATCH))
	  (endofpattern (copy-marker (match-end MATCH))))

     (goto-char endofpattern)
     (if 
	 (and 
	  ;; This is cheap. Maybe some context-sensitive rule already prevents us.
	  (~~rule-desired-p rule)

	  ;; parse-arg. returns string if failure, eg. unclosed braces
	  (not (stringp (setq args 
			      (~~parse-arguments argspec scopelimit))))
	  (set-marker endofpattern (point))

	  ;; this would mess with the user's point
	  (not (and (not replace?)
		    (~~value-safe ~~user-point)
		    (>= ~~user-point startofpattern)
		    (<  ~~user-point endofpattern)))

	  ;; if not environment, succeed, otherwise see if we can get the
	  ;; end-of-environment, and then put it into the args.
	  (if (not (equal ':environment (~~pattern-type pattern))) 
	      t
	    (setq envlimit 
		  (~~find-end-of-environment 
		   (~~pattern-name pattern) scopelimit))
	    (unless (stringp envlimit) 
	      ;;end of env in scope! (implicit nil otherwise)
	      (set-marker endofpattern (point))
	      ;;(message "considering `%s´" (buffer-substring startofpattern endofpattern))
	      (~~append! args (list envlimit))))) ;implicit t

	 ;; Yes! Go ahead and do it!
	 ;; now, if somebody matched, always abort.
	 (prog1 endofpattern
	   (save-excursion
	     (~~really-apply-rule-at-point rule args 
					   (cons startofpattern
						 (marker-position endofpattern))
					   replace?)))
       ;; otherwise, hope for some other rule.
       nil)
     ))

 ;; Really apply RULE at point.
 ;;
 ;; We know it's applicable and all the arguments are there.
 ;; PATTERNPOS is a cons with car just before and cdr just after the
 ;; whole thing we will hide. ARGS contains the arguments as strings,
 ;; and buffer positions for the environment body.
 (defun ~~really-apply-rule-at-point (rule args patternpos &optional replace?)
   (let* ((actions (append talcum-default-actions (~~action-part rule)))
	  (localvars nil)
	  (ov (make-overlay (car patternpos) (cdr patternpos)))
	  (talcum-make-render-opener 'talcum-make-render-opener)
	  action arg1 arg2 ;; current action and its args
	  no-display-flag
	  )

     ;; run over the actions and do what they say.
     ;;
     (dolist (subov (overlays-in (car patternpos) (cdr patternpos)))
       (if (overlay-get subov 'display)
	   (delete-overlay subov)))
     
     (while actions

       (setq action (pop actions))
       (cond

	((run-hook-with-args-until-success 
	  'talcum-apply-action-functions action 'actions ov))

	((eq action ':var) ;; Make a local var known.
	 (setq arg1 (pop actions))
	 (if (not (consp arg1))
	     (~~var-scope-start arg1)
	   (~~var-scope-start (car arg1))
	   (set (car arg1) (cdr arg1))
	   (setq arg1 (car arg1)))
	 (push arg1 localvars))

	((eq action ':eval) ;; eval the next element
	 (eval (pop actions)))

	((eq action ':opener)
	 (setq talcum-make-render-opener (pop actions)))

	((eq action ':face)
	 (~~with-secretly-touching-buffer
	  (add-text-properties 
	   (overlay-start ov) (overlay-end ov)
	   (list 'face 
		 (cons (pop actions)
		       (get-text-property (overlay-start ov) 
					  'face))))))

	((eq action ':property)	;; set the (overlay) property
	 (overlay-put ov (pop actions) (pop actions)))

	((eq action ':display) ;; set the display string
	 (if replace?
	     (~~with-secretly-touching-buffer
	      (goto-char (car patternpos))
	      (insert (~~eval-display-spec (pop actions) args))
	      (delete-region (point) (min (+ (point)
					     (cdr patternpos)
					     (- (car patternpos)))
					  (point-max))))
	   (overlay-put ov 'display 
			(format "%s" (~~eval-display-spec (pop actions)
							  args)))))

	((eq action ':recurse) ;; just render the body, without generating overlay
	 (setq no-display-flag t)
	 (let ((body (car (last args))))
	   (talcum-render-region (car body) (cadr body))))

	((eq action ':help) ;; set the flyover text
	 (if replace? ;; doesn't make much sense if we only get the string.
	     (pop actions)
	   (overlay-put ov 'help-echo 
			(format "%s" (~~eval-display-spec (pop actions)
							  args)))))
	(t (message "unknown action! %s" action))
	))

     (if (string= (overlay-get ov 'display) "")
	 (overlay-put ov 'display "<empty>"))

     (unless (or replace? no-display-flag)
       (funcall talcum-make-render-opener ov patternpos))
     (if no-display-flag
	 (delete-overlay ov))

     ;; all done, remove local scope.
     (dolist (lv localvars)
       (~~var-scope-end lv))
     ))

 ;; turn a display spec into a string with text properties.
 (defun ~~eval-display-spec (spec patternargs)
   (let* (result
	  (render? (eq (car-safe spec) ':rendered))
	  (spec (if render? (cadr spec)
		  spec)))
     (setq result
	   (cond
	    ((run-hook-with-args-until-success 
	      'talcum-display-spec-functions spec patternargs))
	    ((stringp spec) spec)
	    ((integerp spec) (~~maybe-strip-parens (nth spec patternargs)))
	    ((eq spec 'body) 
	     (let ((bodypos (car (last patternargs))))
	       (buffer-substring (car bodypos)
				 (cadr bodypos))))
	    ((functionp spec) (funcall spec patternargs))
	    (t (error "Unknown result specifier %s" spec))
	    ))
     (if render?
	 (setq result (talcum-render-string result)))
     result))

;; The cheap, safe variant.
(defun talcum-make-render-opener (ov pos)
  (talcum-namespace
   (~~with-secretly-touching-buffer
    (let ((beg (overlay-start ov))
	 (end (overlay-end ov)))
	(add-text-properties 
	 beg end
       `(point-entered (lambda (_ __) 
			 (let* ((a (overlay-start ,ov))
				(o (overlay-end ,ov)))
			   (when (and a o)
			     ;(message "a&o %s %s, point %s" a o (point))
			     (talcum-unrender-region a o)
			     ;(message "ok.")
			     t)
			   t)))
       )))))

 (defun ~~render-open/close (ov enter-or-leave)
   (~~without-touching-buffer
    (~~with-secretly-touching-buffer
     (let* ((beg (overlay-start ov))
	    (end (overlay-end ov)))
       (cond
	((equal enter-or-leave 'enter)
	 (overlay-put ov 'display nil)
	 ;;       (overlay-put ov 'face nil)
	 (font-lock-fontify-region beg end)
	 (remove-text-properties 
	  beg end
	  (list 'point-entered nil)))

	((and (equal enter-or-leave 'leave)
	      (null (overlay-get ov 'display))
	      (overlay-start ov)
	      (overlay-end ov)
	      (eq (overlay-buffer ov) (current-buffer))
	      (or (< (point) (overlay-start ov))
		  (> (point) (overlay-end ov)))
	      )
	 (delete-overlay ov)
	 (remove-text-properties 
	  beg end
	  (list 'point-entered nil 'point-left nil))
	 (talcum-render-region beg end)))))))
     


 (defun ~~parse-arguments (argspec end)
   "Parse options ARGSPEC in current buffer. Each opening brace matches a
whole braced sexp, all but { are optional. * is optional. Any other
char matches itself. Return a list of what was parsed, or a string
detailing what went wrong. END limits how far in the buffer we will
go."
   (catch 'mismatch
     (let (result found)
       (dolist (wanted (append argspec nil) 
		       (if (> (point) end) 
			   (throw 'mismatch "Exceeded")
			 result))
	 ;; we can always have whitespace between args.
	 (skip-syntax-forward "-" end)
	 (setq found (char-after))
	 (if (null found) (throw 'mismatch "Premature EOB."))
	 (~~append! result 
		    (list 
		     (cond

		      ((run-hook-with-args-until-success 
			'talcum-parse-argument-functions wanted))

		      ((or (equal wanted ':mandatory)
			   (equal wanted ?{)) 
		       (with-syntax-table ~~sanitized-mode-syntax-table
			 (~~scantoken)))

		      ;; [ gets nothing or a [sexp]
		      ((or (equal wanted ':optional)
			   (equal wanted ?\[ ))
		       (if (= found ?\[)
			   (catch 'notclosed
			     (with-syntax-table ~~sanitized-mode-syntax-table
			       (let ((s (point))
				     (forward-sexp-function nil))
				 (while (not (looking-at "[^{]*]"))
				   (if (> (point) end)
				       (throw 'notclosed ""))
				   (forward-sexp 1))
				 (search-forward "]")
				 (if (> (point) end)
				     (throw 'notclosed ""))
				 (buffer-substring 
				  s (point)))))
			 ""))

		      ;; special hardcode hack: * gets itself, optionally
		      ((or (equal wanted ':star)
			   (equal wanted ?*))
		       (if (= found ?*)
			   (buffer-substring
			    (point) (~~point-after (forward-char 1)))
			 ""))


		      ;; better not: more complaints = less bugs :D
		      ;; ;; any other char gets itself
		      ;; ((= wanted found) 
		      ;;  (buffer-substring 
		      ;;  (point) (~~point-after (forward-char 1))))

		      ;; if all else fails, complain and exit
		      (t (throw 'mismatch 
				(format "looking at %c and no rule matches." 
					found)))))
		    nil)))))

 (defun ~~maybe-strip-parens (str)
   "Remove one level of enclosing parentheses from string, if any. We
also strip leading and trailing whitespace in that case. Note: we do
not check the parens match each other!"
   (if (and str (string-match "^[[:space:]]*\\s(\\([^\000]*\\)\\s)[[:space:]]*$" str))
       ;; evil hack there, but not handling NUL is better than not handling \n.
       (match-string 1 str)
     str))

 (defun ~~find-end-of-environment (envname end)
   "Find the end of the environment called envname. 

We assume point after the \\begin, since somebody else might have
parsed arguments already. (Technically, point after the \\ is
sufficient.) We return a list with the position of the first char in
and not in the env. body anymore. We leave point just after the
closing brace of the \\end We only check for nesting of this
environment, not any other. (This means we probably don't DTRT in the
preamble, if the begin-part of a newenvironment does a \\begin{foo}).
Returns a string if we run out of buffer before the env is closed."
   (catch 'endofbuffer
     (let ((depth 1)
	   (re (concat "\\(\\\\begin\\|\\\\end\\){" envname "}"))
	   (a (point))
	    o)
       (while (and (not (zerop depth)) 
		   (or (search-forward-regexp re end t)
		       (throw 'endofbuffer "Exceeded scope")))
	 ;(message "d%s, %s" depth (match-string 1))
	 (if (string= (match-string 1) "\\begin")
	     (setq depth (1+ depth))
	   (setq depth (1- depth))))
       (list a
	     (save-excursion 
	       (search-backward "\\end{")
	       (point)))
       )))

 )					;namespace

(provide 'talcum-render)
