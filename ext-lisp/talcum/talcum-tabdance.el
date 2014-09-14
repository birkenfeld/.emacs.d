;;;!; $Id: talcum-tabdance.el,v 1.10 2005/09/03 13:30:38 ulmi Exp $
;;;!;
;;;!; This file is part of Talcum.
;;;!;
;;;!; Talcum is released under the terms and conditions
;;;!; of the LaTeX Project Public License 1.3a (see file
;;;!; LPPL-13a for details)
;;;!;
;;;!; Talcum has the maintenance status: author-maintained.
;;;!; Maintainer: Ulrich M. Schwarz, ulmi@users.sarovar.org.
;;;!; 

;;; Tabdance: Enhanced Minibuffer Functionality.
;;; 
;;; We define an additional key in minibuffer maps to switch between
;;; different completion sets. The idea is that these sets offer
;;; increasing numbers of choices based on user behaviour: the first
;;; level will only offer elements from the history (so now "eq" will
;;; complete to "equation" if you never use eqnarray), the second
;;; level would maybe offer some predefined set as well, a third level
;;; could start parsing included packages for command definitions.
;;;
;;; Currently, we use the first two steps: first, history, then, the
;;; original list, by setting talcum-tabdance-auto-prefer-history to a
;;; non-nil value. LaTeX mode's insert-block thing is redefined to use
;;; the history and thus benefit from this. 

;;; Known deficiency: We are currently limited because of a wart in GNU
;;; Emacs: read-file-name, read-buffer et al. don't honor advice to
;;; completing-read, hence, tabdance won't work with them. (This
;;; includes everything that is called by interactive codes, I think.)

(require 'talcum-tools)
(require 'talcum-compat)
(require 'talcum-custom)
(require 'talcum-menu)

(talcum-namespace
 (~~sourcefile-versionstamp "$RCSfile: talcum-tabdance.el,v $"  "$Revision: 1.10 $"))

(defgroup talcum-tabdance
  nil
  "Improved minibuffer completion"
  :group 'talcum)

(defcustom talcum-use-tabdance-flag nil
  "*Set to non-nil to use tabdance."
  :group 'talcum-tabdance
  :type 'boolean)

(defcustom talcum-tabdance-key nil
  "The key that will switch completion levels. Any value that is
suitable as a second argument to define-key. You may find it
convenient to change this by calling talcum-tabdance-query-key
instead."
  :group 'talcum-tabdance
  :type 'sexp)

;;; Getter here, it is referenced in the docstring above.
(defun talcum-tabdance-query-key (chord)
  (interactive "KKey sequence to get more completions in minibuffer: ")
  (setq talcum-tabdance-key chord))

(defcustom talcum-tabdance-auto-prefer-history t
  "*Make a two-step tabdance out of history and completion list
automagically, in all applications that use completing-read"
  :group 'talcum-tabdance
  :type 'boolean)

(defcustom talcum-tabdance-auto-escalate t
  "*If no completion exists at the current completion level,
automatically try a more choiceful level."
  :group 'talcum-tabdance
  :type 'boolean)

;;; In intermediate CVS versions, this was not documented and checked
;;; with ~~value-safe. This will probably become obsolete if the
;;; default tex-latex-block becomes history-aware itself, unless by
;;; then they've completely gone over the border with asking for
;;; parameters etc.
(defcustom talcum-tabdance-no-hijacking t
  "Do not replace LaTeX-mode's tex-latex-block with a history-enabled
variant."
  :group 'talcum-tabdance
  :type 'boolean)

(defcustom talcum-tabdance-everywhere t
  "*Set to non-nil to use tabdance in all minibuffer queries."
  :group 'talcum-tabdance
  :type 'boolean)


(talcum-namespace

 (defun talcum-tabdance-generate-menu ()
   (list "--"
         (~~menu-flag-toggle talcum-use-tabdance-flag 
                             "Improved Minibuffer")
;;; Oh well, I think they're stable enough. Use the customization.
;;          (~~menu-flag-toggle talcum-tabdance-auto-prefer-history 
;;                              "Automatic History")
;;          (~~menu-flag-toggle talcum-tabdance-auto-escalate
;;                              "Automatic Escalation")
         ))

 (add-to-list 'talcum-menu-functions #'talcum-tabdance-generate-menu)


 (defun talcum-tabdance-escalate ()
   "Go to the next more choicy level of completion, if there is one.
Return a non-nil value if that is a real increase."
   (interactive)
   (when (boundp 'tabdance-level)
     (unless (= tabdance-level (~~pseudolength tabdance-choices))
       (if (interactive-p)
           (message "completion level %s" (1+ tabdance-level)))
       (setq tabdance-level (1+ tabdance-level)))))

 (defun talcum-tabdance-descalate ()
   "Go to the previous, more restricted level of completion if there is one.
Return a non-nil value if that is a real decrease"
   (interactive)
   (when (boundp 'tabdance-level)
     (unless (= 1 tabdance-level)
       (if (interactive-p)
           (message "completion level %s" tabdance-level))
       (setq tabdance-level (1- tabdance-level)))))

;;; This is the one the user will want to call. Question: should the
;;; directions be inverted? With automatic escalation, maybe going
;;; back down will be more useful by default.
 (defun talcum-tabdance-scalate (direction)
   "Without prefix argument, go to a more choiceful completion level,
with prefix argument, go back one level."
   (interactive "P")
   ;;; We call-interactively because we want the message.
   (call-interactively
    (if direction
        #'talcum-tabdance-descalate
      #'talcum-tabdance-escalate)))

;;; Remove doubles from a list. This is likely to be phased out at
;;; some point, because it looks like GNU Emacs 22+ will have an
;;; option to remove doubles on its own. One way or another, this is
;;; not critical, but a completion window that lists equation five
;;; times looks... strange.
 (defun ~~sparsed (xs)
   (let (ys)
     (dolist (x xs ys)
       (add-to-list 'ys x))))

 ;;; Somebody set us up the bomb!
 (defun talcum-tabdance-init ()
   ;;; Be extra paranoid about the key. If auto-escalation is off and
   ;;; there is no escalation key, completion is effectively only
   ;;; selecting from the history.
   (if (not talcum-tabdance-key)
       (message "Tabdance not started: %s" 
                "you have not defined an escalation key.")
     (define-key minibuffer-local-completion-map 
       talcum-tabdance-key (quote talcum-tabdance-scalate))

     (when (and (talcum-latex-p)
                (not talcum-tabdance-no-hijacking))
       (~~tabdance-hijack))
     t))

 (defun ~~tabdance-hijack ()
   ;;; Vaguely similar to the function from classic LaTeX mode as
   ;;; of GNU Emacs 21.3. No "Options" Question -- who uses that
   ;;; anyway? Display of default choice is standard now, offers
   ;;; history, accommodates both latex-mode 21.3 and latex-mode
   ;;; 22.
   (define-skeleton tex-latex-block
     "Create a matching pair of lines \\begin{NAME} and \\end{NAME} at point.
Puts point on a blank line between them. Offers history."
     (let ((blockname 
            (completing-read 
             (format "LaTeX environment: (default %s) " latex-block-default)
             (mapcar 'list 
                     (~~sparsed
                      (append (or
                               (~~value-safe standard-latex-block-names) ;; -21
                               (~~value-safe latex-standard-block-names));; 22-
                              latex-block-names)))
             (not ':filter-by-predicate)
             (not ':require-match) 
             (not ':filled-out-already)
             'latex-block-names ; history
             latex-block-default ; default
             )))
       (setq latex-block-default blockname)
       blockname)
     ;; Do start a new line for environment, otherwise, indentation
     ;; gets confused.
     \n 
     ;; Without extra >, we misindent for document.
     "\\begin{" str "}" >
     \n _ \n
     "\\end{" str "}" > \n)

   ;;; In latex-mode 22--, C-c C-o points to latex-insert-block.
   (defalias 'latex-insert-block 'tex-latex-block))


 (defun talcum-tabdance (str pred allp)
   (let* ((realchoice (~~nth-filtered (1- tabdance-level)
                                     tabdance-choices
                                     'identity)))
     (cond

      ((equal t allp)
       (while (and talcum-tabdance-auto-escalate
                   (null (all-completions str realchoice pred))
                   (talcum-tabdance-escalate)
                   (setq realchoice (~~nth-filtered (1- tabdance-level)
                                                    tabdance-choices
                                                    'identity))))
       (all-completions str realchoice pred))

      ((null allp)
       (while (and talcum-tabdance-auto-escalate
                   (null (try-completion str realchoice pred))
                   (talcum-tabdance-escalate)
                   (setq realchoice (~~nth-filtered (1- tabdance-level)
                                                    tabdance-choices
                                                    'identity))))
       (try-completion str realchoice pred))

      ((equal 'lambda allp)
       (not (null (member str 
                          (all-completions str realchoice pred)))))
      )))

 (defun ~~nth-filtered (n xs pred)
   (nth n (~~filter pred xs)))
 
 (defun ~~pseudolength (xs)
   (length (~~filter 'identity xs)))

 (defadvice completing-read 
   (around tabdance (prompt table 
                            &optional predicate reqmatch initial 
                                      hist def inherit) 
           activate)
   (let ((tabdance-choices '())
         (table table)
         (tabdance-level 1)          ; Ugh. Think of something better.
         )

     (cond
      ;; Safeguard: we're not correctly set up, go back to old behaviour.
      ((or (not talcum-use-tabdance-flag)
           (not talcum-tabdance-key)
           (and (not talcum-mode) (not talcum-tabdance-everywhere)))
       t)

      ;; Explicit :tabdance request. Note that in this case, we are not
      ;; trying to be clever about the history. This is not needed,
      ;; because the caller already knows about tabdance, so it's
      ;; his/her responsibility to include history vars at a suitable
      ;; place.
      ((and (consp table)
            (equal (car table) ':tabdance))
       (setq tabdance-choices (cdr table)
             table 'talcum-tabdance))

      ;; Prefer the history.
      ((and talcum-tabdance-auto-prefer-history hist)
       (setq tabdance-choices (list 
                               ;; We must convert the history from list
                               ;; to alist.
                               (mapcar 'list
                                       ;; The history may be a (symbol .
                                       ;; offset) cons as well, but we
                                       ;; always want the symbol's value,
                                       ;; which is a list.
                                       (symbol-value
                                        (if (consp hist)
                                            (car hist)
                                          hist)))
                               table)
             table 'talcum-tabdance))
     
      ;; Default case: do nothing fancy.
      (t
       t))
     ad-do-it))

 (add-hook 'talcum-mode-hook (lambda () (if talcum-use-tabdance-flag
                                           (talcum-tabdance-init))))
 )                                      ; namespace
(provide 'talcum-tabdance)
;; talcum-tabdance.el ends here.