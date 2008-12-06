;;;!; $Id: talcum.el,v 2.14 2005/09/07 18:04:44 ulmi Exp $
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

;; In spite of the name, this value is changed at load-time. Because
;; ulmi always forgets to set this value, and RCS has major.minor
;; version numbers, the new scheme (post-0.5.0) is as follows:
;;
;; The first two digits are set manually. The last digit is the sum of
;; the minors of all constituent files. This ensures that a local
;; commit will change the overall version.
(defconst talcum-version "0.5.0")

;; Path setup. 
;;
;; Technically, all that we want is all the .el files where we can
;; find them. This used to be done via talcum-path; it happens
;; automatically if Talcum is installed to the magic site-lisp
;; directories; and if at least this file is in the load-path already,
;; we will just assume that
;;  1. main Talcum files are in the same directory
;;  2. TREXes live in the subdir /trex/
;; since they fall out the tar.gz that way.

(let* ((thisfile (locate-library "talcum")))
  (cond
   ((and thisfile (not (boundp 'talcum-path)))
    ;; We are in the load-path already, and we need not honour
    ;; talcum-path. 
    (setq talcum-path (file-name-directory thisfile))
    ;; If we are in the magic trees, we are done.
    (if (not (string-match "/site-lisp/" thisfile))
        ;; Otherwise, add the trex subdir.
        (add-to-list 'load-path (concat talcum-path "/trex/"))))
   (t
    ;; OK, we were called with absolute path the old-fashioned way.
    ;; Unfortunately, there seems to be no way of getting our position
    ;; while we're being loaded.
    (when (not (boundp 'talcum-path))
      ;; Yes well, what am I supposed to do?
      (setq talcum-path "/home/ulmi/Projekte/Emacs/Talcum/talcum/src/")
      (message "Warning: Could not figure out talcum-path, using %s" talcum-path))
    ;; Anyway, now we're ready.
    (add-to-list 'load-path talcum-path)
    (add-to-list 'load-path (concat talcum-path "/trex/")))))

;; Nothing works without these three.
(require 'talcum-tools)
(require 'talcum-compat)
(require 'talcum-custom)

(talcum-namespace
 (~~sourcefile-versionstamp "$RCSfile: talcum.el,v $"  "$Revision: 2.14 $"))

(autoload 'talcum-add-render-feature
  "talcum-render" 'interactive)

;;;
;;; Customize-Setup.
;;;
(defgroup talcum 
  nil ;; members
  "A mixed bag of things that make LaTeX easier." 
  :group 'tex)

(let ((unless-forbidden 
       (lambda (flag sym)
         (if (and (boundp flag) (null (symbol-value flag)))
             nil
           (list sym)))))
  ;; We used to have talcum-use-foo-flags here. That's unfortunate,
  ;; optimally, we don't want to change this file when a new sublibrary
  ;; is added. Now, we have a list of features that will be required.
  (defcustom talcum-desired-features
    ;; append hack here: if those were customized to nil, obey that.
    (append
     (funcall unless-forbidden 'talcum-use-render-flag 'talcum-render)
     (funcall unless-forbidden 'talcum-use-prod-flag   'talcum-prod)
     (funcall unless-forbidden 'talcum-use-newcmd-flag 'talcum-newcmd)
     '(talcum-pikeman talcum-tabdance))
    "Features that should be loaded when Talcum is started"
    :group 'talcum
    :type '(repeat symbol) ;; fix this for something clickable
    ))

;; Two hooks provided for the proper work-doing code. We could do
;; without the -enter hook, but it's here for symmetry. Since we
;; dabble in idle-timers etc. pp., we want to provide a way to disable
;; them.
(defvar talcum-enter-mode-hook nil)
(defvar talcum-leave-mode-hook nil)

;;;
;;; Settings of keys.
;;;
(define-prefix-command 'talcum-keymap)

;;
;; Main entry point for all things talcum
;;
(defconst talcum-lighter " Talcum")

(defcustom talcum-mode-chord (vector ?\C-c ?#)
  "The key combination prefix for all Talcum commands."
  :group 'talcum
  :type 'sexp)

;;;###autoload 
(define-minor-mode talcum-mode
  "A minor mode to enhance both LaTeX- and latex-mode."
  nil					; initially off
  talcum-lighter			; mode indicator
  (list					; keymap
   (cons talcum-mode-chord 'talcum-keymap)) 
  ;; init body

  ;; Hmmm... I think this should work. The docs are very unclear on
  ;; whether the autoload cookie will load the whole file. Let's make
  ;; sure, because without namespaces, we shan't get far.
  (require 'talcum)
  (require 'talcum-menu)

  (when talcum-mode ;; activating!
    (talcum-namespace
     (cond
      ((talcum-latex-p) (~~debug 1 "Classic latex mode, ok."))
      ((talcum-auctex-p) (~~debug 1 "AucTeX LaTeX mode, ok."))
      (t (~~debug 9 (concat "Unsupported major mode \"%S\". "
			    "Things might go wrong.") 
		  major-mode)))

     ;; more activation stuff goes here
     (dolist (feat talcum-desired-features)
       (~~debug 1 "Requiring %s" feat)
       (unless (require feat nil :noerror)
         (~~debug 9 "Requiring %s failed; skipping" feat)))

     (require 'talcum-bugreport)
     (run-hooks 'talcum-enter-mode-hook)
     (talcum-menu-regenerate)
     t))
  (unless talcum-mode ;; deactivating!
    (run-hooks 'talcum-leave-mode-hook)
    t)
  )

(provide 'talcum)

;;; talcum.el ends here.