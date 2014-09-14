;;; $Id: talcum-newcmd.el,v 2.15 2005/08/27 12:41:08 ulmi Exp $
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
(require 'talcum-compat)
(require 'talcum-menu)


(talcum-namespace
 (~~sourcefile-versionstamp "$RCSfile: talcum-newcmd.el,v $"  "$Revision: 2.15 $") 

 (defun talcum-newcmd-generate-menu ()
   (list "--"
    '["Insert \\newcommand in preamble" talcum-insert-newcommand]
    '["Insert \\newenvironment in preamble" talcum-insert-newenvironment]
    '["Insert \\usepackage in preamble" talcum-insert-usepackage]))

 (add-to-list 'talcum-menu-functions #'talcum-newcmd-generate-menu)

 (defvar talcum-newfoo-before-insertion-hooks nil)
 (defvar talcum-newfoo-after-insertion-hooks nil)

 (defun ~~newfoo-find-suitable-place ()
   (let* ((talcum-definitions-file talcum-definitions-file)
	  (goodbuffer 
           ;; 2005AUG25: jumbling the order.
           ;; Doing functionp earlier obliterates the need for
           ;; :latexmode and :auctex flags -- auctex does not
           ;; provide a tex-main-file.
	   (cond
	    ((stringp talcum-definitions-file) 
	     talcum-definitions-file)
	    ((functionp talcum-definitions-file) 
	     (funcall talcum-definitions-file))
	    ((and ;(equal talcum-definitions-file ':latexmode)
              (boundp 'tex-main-file)
              (stringp tex-main-file))
	     tex-main-file)
	    ((and ;(equal talcum-definitions-file ':auctex)
              (talcum-auctex-p)) 
	     (TeX-active-master t))
	    (t (current-buffer))
	    )))
     (when (stringp goodbuffer)
       (switch-to-buffer (find-file-noselect goodbuffer) 'secretly))
     (if buffer-read-only
	 (vc-toggle-read-only))
     ;;(switch-to-buffer goodbuffer 'secretly)
     (~~newfoo-find-suitable-position)))

 (defun ~~newfoo-find-suitable-position (&optional type)
   ;; This is hopeless. What _is_ a good position? 
   ;;
   ;; Optimally, we would have all usepackages first, but their setup
   ;;commands (\hypersetup et al.) just after them, and all commands
   ;;after that.
   (goto-char (point-min))
   (search-forward "\\begin{document}" nil t)
   (forward-line 0)
   ;; Now just before begin{document}.
   ;; Always skip over author, title, date, etc.
   (dolist (textinfo talcum-textinfo-commands)
     (search-backward textinfo nil t))
   (when (eq type :package)
     ;; This misbehaves if no usepackage is present, but other
     ;; commands. Oh c'mon, that's bound to be rare.
     (when (search-backward "\\usepackage" nil t)
       (forward-char (length "\\usepackage"))
       (when (looking-at "[[:space:]]*\\[")
         ;; skip package args
         (skip-syntax-forward " ")
         (~~fwd-arg))
       (~~fwd-arg)
       (when (looking-at "[[:space:]]*\\[")
         ;; skip package release date
         (skip-syntax-forward " ")
         (~~fwd-arg))
       (forward-line 1))
     t)
   t)

 (defmacro ~~at-suitable-place (&rest cmds)
   `(save-excursion
      (save-window-excursion
	(~~newfoo-find-suitable-place)
	,@cmds)))


 (defun talcum-excurse-to-preamble ()
   (interactive)
   (~~at-suitable-place
    (message "Press M-C-c when finished.")
    (recursive-edit)
    (save-buffer)))

 (defun talcum-insert-newcommand (pfx macname) 
   "Jump to a suitable point and generate a skeleton for a LaTeX macro.
Where to jump is determined by ltx-newfoo-find-suitable-place.
With a prefix argument, assume that many parameters (0 otherwise).
With transient-mark-mode and active region, use that region as body."  
   (interactive "P\nsName of macro: \\")

   (let* ((argnumber 
	   (cond
	    ((null pfx) 0)
	    ((integerp pfx) pfx)
	    ((listp pfx) (car pfx))	; empty list caught above
	    ((eq pfx '-) -1)))	       ; whoopsie! Think of something.
	  (beg (and transient-mark-mode mark-active (region-beginning)))
	  (end (and transient-mark-mode mark-active (region-end)))
	  (body (if beg (delete-and-extract-region beg end)
		  "%\n")))
    
     (when beg
       (deactivate-mark)
       (insert (format "\\%s%s"
		       macname
		       (if (and pfx (= 0 argnumber)) 
			   "\\ "
			 (apply 'concat 
				(make-list argnumber "{}")))))
       t)
     (~~at-suitable-place
      (run-hook-with-args 'talcum-newfoo-before-insertion-hooks 
                          ':macro macname argnumber)
      (insert (format 
               "\\newcommand\\%s%s{%%\n\n}\n\n"
               macname
               (if (= 0 argnumber) ""
                 (format "[%s]" argnumber))
               body))
      (forward-line -3)
      (message "Press M-C-c when finished.")
      (recursive-edit)
      (run-hook-with-args 'talcum-newfoo-after-insertion-hooks 
                          ':macro macname argnumber)
      (save-buffer)
      t)
     t))

 (defun talcum-insert-newenvironment (pfx envname) 
   "Jump to a suitable point and generate a skeleton for a LaTeX environment. 
Where to jump is determined by ltx-newfoo-find-suitable-place. The
prefix, if any, specifies the number of arguments."
   (interactive "P\nsName of environment: ")

   (let* ((argnumber 
	   (cond
	    ((null pfx) 0)
	    ((integerp pfx) pfx)
	    ((listp pfx) (car pfx))	; empty list caught above
	    ((eq pfx '-) -1))))	       ; whoopsie! Think of something.

     (~~at-suitable-place
      (run-hook-with-args 'talcum-newfoo-before-insertion-hooks 
                          ':environment envname argnumber)
      (insert (format "\\newenvironment{%s}%s{%%\n%%\n}{%%\n%%\n}\n\n"
                      envname
                      (if (= 0 argnumber) ""
                        (format "[%s]" argnumber))))
      (forward-line -5)
      (message "Press M-C-c when finished.")
      (recursive-edit)
      (run-hook-with-args 'talcum-newfoo-after-insertion-hooks 
                          ':environment envname argnumber)
      (save-buffer))))


 (defun talcum-insert-usepackage (package &optional options)
   (interactive "sPackage name: \n")
   (unless options
     (setq options 
	   (read-from-minibuffer 
	    "Options: " 
	    (cdr-safe (assoc package talcum-package-option-hints)))))
   (~~at-suitable-place
    (~~newfoo-find-suitable-position :package)
    (insert "\\usepackage" 
            (if (< 0 (length options)) 
                (concat "[" options "]")
              "")
            "{" package "}\n")
    (sit-for 1.0)))
 
 ) ;;namespace

;;;
;;; Init and deinit functions, for completeness.
(defun talcum-newcmd-init ()
  t)
(defun talcum-newcmd-exit ()
  t)

;;;
;;; Key bindings
(define-key talcum-keymap "c" 'talcum-insert-newcommand) ; c for command
(define-key talcum-keymap "e" 'talcum-insert-newenvironment) ; e for envir
(define-key talcum-keymap "p" 'talcum-insert-usepackage) ; p for package


;;; 
;;; Customizations

(defgroup talcum-newcmd nil "Inserting new commands" :group 'talcum)

 (defcustom talcum-textinfo-commands
   '(;; LaTeX2e classes and everywhere:
     "\\author" "\\date" "\\title" 
     ;; KOMA-Script:
     "\\publishers" "\\subject" "\\titlehead" 
     "\\dedication" "\\uppertitleback" "\\lowertitleback" "\\extratitle"
     ;; beamer:
     "\\subtitle" "\\institute" "\\subject" "\\keywords" "\\titlegraphic"
     )
   "Commands that are technically in the preamble, but should not be
seperated from begin{document}."
   :group 'talcum-newcmd
   :type '(repeat :tag "Macro name" string))

(defcustom talcum-use-newcmd-flag t
  "*Enable the whole newcommand subsystem."
  :group 'talcum
  :type 'boolean)


(defcustom talcum-definitions-file nil
  "Where Talcum will put new definitions (newcommands, newenvs)."
  :group 'talcum-newcmd
  :tag "Talcum definitions file"
  :type '(choice 
          (const :tag "the current buffer" nil)
          (const :tag "LaTeX-mode's master file (deprecated)" ':latexmode)
          (const :tag "AucTeX' master file (deprecated)" ':auctex)
          (string :tag "filename" :value "preamble.ltx")
          (function :tag "returned by function" :value (lambda () "preamble.tex"))))
			   


(defcustom talcum-package-option-hints
  '(("inputenc" . "latin9")
    ("fontenc" . "T1")
    ("helvet" . "scaled"))
  "Talcum can suggest options for packages."
  :group 'talcum-newcmd
  :tag "Package option suggestions"
  :type '(repeat 
          (cons (string :tag "package name" :value "package")
                (string :tag "option string" :value ""))))


(add-hook 'talcum-enter-mode-hook #'talcum-newcmd-init)
(add-hook 'talcum-leave-mode-hook #'talcum-newcmd-exit)

(provide 'talcum-newcmd)

;;; talcum-newcmd.el ends here.