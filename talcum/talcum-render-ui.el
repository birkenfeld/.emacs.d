;;; $Id: talcum-render-ui.el,v 2.18 2005/08/28 09:00:49 ulmi Exp $
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
(require 'talcum-render-rules)
(require 'talcum-custom)
(require 'talcum-menu)

(talcum-namespace
 (~~sourcefile-versionstamp "$RCSfile: talcum-render-ui.el,v $" "$Revision: 2.18 $"))

(defcustom talcum-use-render-flag t
  "*Enable the whole rendering subsystem."
  :group 'talcum
  :type 'boolean)

(defvar talcum-render-list nil
  "The true render list.")

(talcum-namespace
 (defvar ~~mode-syntax-table nil)
 (defvar ~~sanitized-mode-syntax-table nil))

(defgroup talcum-render nil
  "Displaying beautiful letters instead of ugly macros"
  :group 'talcum)

(defcustom talcum-user-render-list nil
  "Local rendering rules"
  :group 'talcum-render
  :get 'talcum-render-rule-getter
  :set 'talcum-render-rule-setter
  :initialize 'custom-initialize-default
  :type talcum-render-rule-custom-type)

(defvar talcum-render-features 'nil)

(defvar talcum-apply-action-functions nil
  "Abnormal hook to introduce new action commands.

Each function is called with the overlay, the current action and the
list of rest-args-and-further-actions. The function should not alter
these unless it has verified it is applicable. The function should
return a non-nil value only if no other rules shall apply to the
action.")

(defvar talcum-display-spec-functions nil
  "Abnormal hook to introduce new display specifiers.

Each function is called with two parameters: the spec and a list
containing the macro's arguments. The function returns a string with
text properties as appropriate, or nil if it is not the right one.
Note that you cannot prevent the :rendered syntax from working with
this -- :rendered is hidden from you beforehand. You may, as a dirty
hack, set the value of render? to nil to override :rendered, though,
and to non-nil to simulate :rendered.")

(defvar talcum-parse-argument-functions nil
  "Abnormal hook used to introduce new kinds of macro parameter style.

Each function is called with one argument: the character that
identifies the parameter style. Functions must return a non-nil value
and leave point at the end of the parameter only if they have handled
that parameter. Otherwise, they may not move point. If you have found
your parameter to be there, but empty (like missing *, return the
empty string \"\".")

(defvar talcum-init-specpdl 6000
  "If you get \"exceeds max-specpdl-size\" errors on startup, set this
to a higher value. (As comparison: GNU Emacs' default is 600.)")

(defvar talcum-semantic-braces
  '(?( ?) ?[ ?] ?« ?» ?< ?> ?\")
  "Not all braces should be made to match each other. For example, a)
does not need an opening brace.")

(defgroup trexes
  nil                                   ;members
  "Customization options for Talcum Rendering EXtensions."
  :group 'talcum)

(defcustom talcum-render-trexes
  '((trex-math :math)
    (trex-unicode :unicode)
    (trex-markup :markup)
    (trex-xref :xref)
    (trex-sectioning :sectioning))
  "Trexes that will be loaded."
  :group 'trexes
  :type 'sexp)

(talcum-namespace
 ;; This used to be in talcum-render. Unfortunately, if font-lock
 ;; kicks in before talcum-render is completely loaded, we would get a
 ;; byte-compile warning: assignment to free var. I wouldn't give a
 ;; flying shit about that if it didn't switch-window away an actual
 ;; useful buffer with actual information in it. Thank you Emacs!
 (defvar ~~user-point nil)
 (make-variable-buffer-local (quote ~~user-point)))

;; XXX HACK XXX should use the proper functions.
;;
;; 2005AUG21: Nevermind, from what I can see, all other font-lock
;; methods disappear in Emacs 22.
(talcum-namespace
 (defadvice jit-lock-fontify-now
   (before talcum-user-point-setter activate)
   "Save user's point."
   
   (unless (~~value-safe ~~user-point)
     (setq ~~user-point (point)))))


(talcum-namespace

 ;; initialize internal stuff.
 ;;
 ;; currently, this mainly recreates the internal hash structures and
 ;; regexpen. This should be called when entering the mode and
 ;; whenever talcum-render-list or talcum-render-features are changed.
 (defun talcum-render-reinit ()
   (interactive)
   (let* ((max-specpdl-size (max max-specpdl-size 
				 talcum-init-specpdl))
	  matcher macro-matchers other-matchers)

     (setq ~~rendermap (~~mmap-new))
     (with-temp-message "Building rendering structures..."
       (dolist (rule (append talcum-render-list 
			     talcum-user-render-list))
	 (when (~~rule-desired-p rule)
	   (setq matcher (~~pattern-matcher (~~pattern-part rule)))
	   (if (equal ':macro (~~pattern-type (~~pattern-part rule)))
	       (push matcher macro-matchers)
	     (push matcher other-matchers))
	   (~~mmap-put matcher rule ~~rendermap)))

       (setq ~~match-any-re 
	     (format "\\(%s\\)\\>\\|\\(%s\\)"
		     (if macro-matchers
			 (regexp-opt macro-matchers)
		       "\\000") ;; XXX hack, because regexp-opt is broken.
		     (if other-matchers
			 (regexp-opt other-matchers)
		       "\\000")))
       t)
     (message "Building rendering structures...done")
     (put (quote ~~rendermap) 'talcum-map-is-dirty nil)
     t))

 (defun talcum-make-render-map-dirty ()
   (put (quote ~~rendermap) 'talcum-map-is-dirty t))

 (defun talcum-render-init ()
   
   (dolist (trex talcum-render-trexes)
     (require (car trex))
     (dolist (feat (cdr trex))
       (talcum-add-render-feature feat)))

   (message "rex-hook")
   (run-hooks 'talcum-rex-init-hook)
   (font-lock-add-keywords nil '(talcum-render-fontlock-function) 'atend)
   (talcum-render-reinit)
   (setq ~~mode-syntax-table (copy-syntax-table))
   (setq ~~sanitized-mode-syntax-table (copy-syntax-table ~~mode-syntax-table))
   ;; it is stupid of forward-sexp et al. to look at ( and ) which are not
   ;; usually syntactical parens: \texttt{)} would take the {) as its argument.
   (dolist (ch talcum-semantic-braces)
     ;; How the fuck does putting a R-val into a function have
     ;; side effects on the var with that R-val?
     (modify-syntax-entry ch "." ~~sanitized-mode-syntax-table))
   t)

 (defun talcum-render-exit ()
   ;; no funs use timers anymore.
   (font-lock-remove-keywords nil '(talcum-render-fontlock-function))
   (talcum-unrender-region (point-min) (point-max))
   t)

 (defun talcum-unrender-region (bor eor)
   "Remove the renderings created by talcum-render."
   (interactive "r")
   (save-excursion
     (~~without-touching-buffer
      (~~with-secretly-touching-buffer
       (remove-text-properties bor eor '(point-entered nil 
                                                       point-left nil 
                                                       face nil 
                                                       fontified nil)))
      (mapc (lambda (ov) 
	      (if (equal (overlay-get ov 'category) 'talcum-rendered)
		  (delete-overlay ov)))
	    (overlays-in bor eor)))))


 (defun talcum-render-find-suitable-start ()
   "Return a buffer position backwards from point where
talcum-render-dwim should start."
   (~~without-touching-buffer
    (save-excursion
      (search-backward "%%%%%" (point-min) 'go-limit)
      (point))))

 (defun talcum-render-find-suitable-end ()
   "Return a buffer position forward from point where
talcum-render-dwim should start."
   (~~without-touching-buffer
    (save-excursion
      (search-forward "%%%%%" (point-max) 'go-limit)
      (point))))

 (defun talcum-render-dwim (pfx) 
   "Refresh the unclutterings in the current suitable region, as found
by t-r-find-suitable-start, -end, which see. With prefix C-u, remove
all unclutterings from that region."
   (interactive "P")
   (setq ~~user-point (point))
   (let ((bos (talcum-render-find-suitable-start))
	 (eos (talcum-render-find-suitable-end)))
     (talcum-unrender-region bos eos)
     (unless (and (listp pfx) pfx) 
       (talcum-render-region bos eos))))
 )					;namespace


(defconst talcum-render-parsep-re "\\(\\\\par[^a-zA-Z]\\)\\|\\(\n\n\\)"
  "A regexp to match the beginning or end of a paragraph.")

(defun talcum-render-paragraph ()
  (interactive)
  "Render the paragraph point is in or after.
May you'll want to advice tex-terminate-paragraph to call this."
  (let* ((inhibit-point-motion-hooks t)
	 (p (point)))
    (talcum-render-region 
     (save-excursion (or (search-backward-regexp talcum-render-parsep-re (point-min) t)
			 (point-min)))
     (save-excursion (or (search-forward-regexp talcum-render-parsep-re (point-max) t)
			 (point-max))))))

(defun talcum-newpar-and-render ()
  (interactive)
  (let* ((inhibit-point-motion-hooks t))
    (save-excursion
      (talcum-render-paragraph))
    (insert "\n\n")))
   
(defun talcum-render-fontlock-function (limit)
  (talcum-render-region (point) limit)
  nil)

(defun talcum-render-fontf-function (start)
  (let ((end (save-excursion 
	       (catch 'end-of-buffer
		 (forward-char 600))
	       (or (search-forward-regexp 
		    talcum-render-parsep-re 
		    (point-max) 
		    'silent-go-limit)
		   (point-max)))))
    (message "rendering from %s to %s" start end)
    (talcum-render-region start end)
    t))
  

;;;
;;; Provide keybindings
;;;
(define-key talcum-keymap "#" 'talcum-render-dwim)
(define-key talcum-keymap "\C-M" 'talcum-newpar-and-render)

;;;
;;; Provide menu
;;;
(talcum-namespace
 (defun talcum-render-make-menu ()
   (list
    (~~menu-flag-toggle talcum-use-render-flag "Rendering"
                        (progn
                          (if talcum-use-render-flag
                              (talcum-render-init)
                            (talcum-render-exit))
                          (talcum-render-dwim 'on)))
    '["Render: DWIM" talcum-render-dwim]
    '["Render buffer" (talcum-render-region (point-min) (point-max))
      :included talcum-use-render-flag]
    '["Render region" (talcum-render-region (region-beginning) (region-end))
      :included talcum-use-render-flag]
    '["Unrender buffer" (talcum-unrender-region (point-min) (point-max))
      :included (not talcum-use-render-flag)]
    '["Unrender region" (talcum-unrender-region (region-beginning) (region-end))
      :included (not talcum-use-render-flag)]
    (let (trexmenu)
      ;; A submenu with a checkbox for every loaded TREX.
      (append '("TREXes" :active talcum-use-render-flag)
              (dolist (trex (talcum-render-features) (nreverse trexmenu))
                (push `[,(symbol-name trex)
                        (progn
                          (if (talcum-has-render-feature ,trex)
                              (talcum-remove-render-feature ,trex)
                            (talcum-add-render-feature ,trex))
                          (talcum-render-reinit))
                        :style toggle
                        :selected (talcum-has-render-feature ,trex)
                        ]
                      trexmenu))))
      ))
 (add-to-list 'talcum-menu-functions #'talcum-render-make-menu)
 )                                      ; namespace

(add-hook 'talcum-enter-mode-hook #'talcum-render-init)
(add-hook 'talcum-leave-mode-hook #'talcum-render-exit)

(provide 'talcum-render-ui)
