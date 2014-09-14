;;; $Id: trex-scotty.el,v 2.3 2005/02/21 21:10:44 ulmi Exp $
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

(require 'talcum-custom)
(require 'talcum-render)

(defvar trex-beamer-current-overlay 1)

;;new: moved over from main rendering.
(add-hook 'talcum-parse-argument-functions
	  'trex-beamer-parse-arg-overlay-spec)


(defun trex-beamer-parse-arg-overlay-spec (char-wanted)
  (when (= char-wanted ?<)
    (if (and (= char-wanted (char-after))
	     ;; otherwise, search-forward below will fail.
	     (looking-at "<[^<]*>"))
	(buffer-substring
	 ;; note: <> can't be nested anyway.
	 (point) (search-forward ">" end)) 
      "")))

(defun trex-beamer-display-function (spec args)
  (when (equal (car-safe spec) ':beamer)
    (talcum-namespace
     (~~eval-display-spec 
      (apply 'trex-beamer-overlay-switch args (cdr-safe spec))
      args))))

(add-hook 'talcum-display-spec-functions 
	  'trex-beamer-display-function)

;; new entries to be done:
(setq talcum-render-list
      (append talcum-render-list
	      '(((:environment "frame" "<[" :beamer)
		 :var (trex-beamer-dot . 1)
		 :display (:rendered body))
		((:macro "frame" "<[{" :beamer)
		 :var (trex-beamer-dot . 1)
		 :display (:rendered 2))
		
		((:macro "only" "<{<" :beamer) ;;only one olay may be present. *shrug*
		 :display (:rendered (:beamer "'" 1 "'" "'")))
		((:macro "frametitle" "{" :beamer)
		 :display (:rendered 0))
		((:macro "uncover" "<{" :beamer)
		 :display (:rendered (:beamer "'" 1 "'" "'")))
		((:macro "visible" "<{" :beamer)
		 :display (:rendered (:beamer "'" 1 "'" "'")))
		((:macro "invisible" "<{" :beamer)
		 :display (:rendered (:beamer 1 "'" 1 1)))
		((:macro "temporal" "<{{{" :beamer)
		 :display (:rendered (:beamer 1 2 3 1)))
		((:macro "alt" "<{{{" :beamer)
		 :display (:rendered (:beamer 2 1 2 2)))
		)))

;;;; These were the old defaults.
;;    ;; SECTION V: latex-beamer
;;    (list "\\frametitle"  4 "{" (cons 0 (list 'face '(:height 1.2 ))) "title of frame")

;;    `("\\only" 4 "<{" ,(talcum-beamer-awarino (cons 1 (list 'face '(foreground-color . "gray80")))
;; 					    1
;; 					    (cons 1 (list 'face '(foreground-color . "gray80")))
;; 					    (cons 1 (list 'face '(foreground-color . "gray80"))))
;;     nil)

;;    `("\\alert" 4 "<{" ,(talcum-beamer-awarino 1
;; 					     (cons 1 (list 'face '(foreground-color . "red")))
;; 					     1
;; 					     1) nil)

;;    `("\\uncover" 4 "<{" ,(talcum-beamer-awarino (cons 1 (list 'face '(foreground-color . "gray80")))
;; 						1
;; 						(cons 1 (list 'face '(foreground-color . "gray80")))
;; 						(cons 1 (list 'face '(foreground-color . "gray80"))))
;;     nil)

;;    `("\\visible" 4 "<{" ,(talcum-beamer-awarino (cons 1 (list 'face '(foreground-color . "gray80")))
;; 						1
;; 						(cons 1 (list 'face '(foreground-color . "gray80")))
;; 						(cons 1 (list 'face '(foreground-color . "gray80"))))
;;     nil)

;;    `("\\invisible" 4 "<{" ,(talcum-beamer-awarino 1
;; 						 (cons 1 (list 'face '(foreground-color . "gray80")))
;; 						 1
;; 						 1) nil)

;;    `("\\alt" 4 "<{{" ,(talcum-beamer-awarino 2 1 2 2) nil)

;;    `("\\temporal" 4 "<{{{" ,(talcum-beamer-awarino 1 2 3 1) nil)



(defun trex-beamer-next-overlay ()
  (interactive)
  (trex-beamer-goto-overlay (1+ trex-beamer-current-overlay))
  (message "now at %s" trex-beamer-current-overlay))

(defun trex-beamer-previous-overlay ()
  (interactive)
  (if (= 1 trex-beamer-current-overlay)
      (message "Already at first overlay")
    (trex-beamer-goto-overlay (1- trex-beamer-current-overlay))
    (message "now at %s" trex-beamer-current-overlay)))

(defun trex-beamer-goto-overlay (i)
  (interactive "nNumber of overlay:")
    (let ((reg (trex-beamer-enclose-frame)))
      (setq trex-beamer-current-overlay i)
      (apply 'talcum-unrender-region reg)
      (message "Showing overlay %d" trex-beamer-current-overlay)
      (apply 'talcum-render-region reg)))

(defun trex-beamer-enclose-frame ()
  "Find the frame in which we are and return its start and end in a list."
  (save-excursion
    (list (search-backward-regexp "\\\\frame[^a-zA-Z]\\|\\\\begin{frame}") 
	  (progn (talcum-parseaway "\\frame[<{" (point-max)) (point)) 
	  )))


(defun trex-beamer-overlay-switch (args before within after other)
  "Find the first overlay spec in ARGS, and return BEFORE, WITHIN,
AFTER or OTHER depending on how we are relative to that spec."
  (let ((args args)
	rng)
    (while (and args
		(not (and (car args) 
			  (string-match "^<[^\000]*>$" (car args)))))
      (pop args))
    (setq rng (trex-beamer-split-ranges (or (pop args) "<->")))
					; urx. <>s in fact can be omitted.
    (cond
     ((trex-beamer-before-ranges rng) before)
     ((trex-beamer-within-ranges rng) within)
     ((trex-beamer-after-ranges rng) after)
     (t other))))

(defun trex-beamer-range-start (rng)
  (if (string-match "^\\([0-9]+\\)" rng)
      (string-to-number (match-string 1 rng))
    0))

(defun trex-beamer-range-end (rng)
  (if (string-match "\\(?:[0-9]*-\\)?\\([0-9]+\\)$" rng)
      (string-to-number (match-string 1 rng))
    999))

(defun trex-beamer-within-ranges (rngs)
  (catch 'success
    (dolist (rng rngs)
      (if (and (<= (trex-beamer-range-start rng) trex-beamer-current-overlay)
	       (<= trex-beamer-current-overlay (trex-beamer-range-end rng)))
	  (throw 'success t)))))

(defun trex-beamer-after-ranges (rngs)
  (catch 'failure
    (dolist (rng rngs t)
      (if (<=  trex-beamer-current-overlay (trex-beamer-range-end rng))
	  (throw 'failure nil)))))

(defun trex-beamer-before-ranges (rngs)
  (catch 'failure
    (dolist (rng rngs t)
      (if (<= (trex-beamer-range-start rng) trex-beamer-current-overlay)
	  (throw 'failure nil)))))

;; take a string, and fish out the ranges for mode beamer-mode.
(defun trex-beamer-split-ranges (rngstr)
  (let* ((result "")
	 (hadplus nil))
    (dolist (modes (split-string (substring rngstr 1 -1) "[[:space:]]*| [[:space:]]*") 
		   result)
      
      (when (or (string-match (concat "^" trex-beamer-beamer-mode ":") 
			      modes)
		(and (string-match "^[^:]*$" modes)
		     (equal trex-beamer-beamer-mode "beamer")))
	  (string-match "\\(?:[^:]*:\\)?[[:space:]]*\\([^[:space:]]*\\)[[:space:]]*$" 
			modes)
	  (setq result (concat result (match-string 1 modes) ","))))

    ;; now, go through the single parts of the range and replace . and +.
    ;; XXX: dubious if this works for ranges like "+-, -.". Per interval,
    ;; dot takes precedence over plus.
    (mapcar
     (lambda (rng)
       (while (and (string-match "\\." rng)
		   (boundp 'trex-beamer-dot))
	 (replace-match (number-to-string trex-beamer-dot) 'fixcase 'literal rng))
       (while (and (string-match "\\+" rng)
		   (boundp 'trex-beamer-dot))
	 (unless hadplus
	   (setq hadplus t
		 trex-beamer-dot (1+ trex-beamer-dot)))
	 (replace-match (number-to-string trex-beamer-dot) 'fixcase 'literal rng))
       rng)
     (split-string result "[[:space:]]*,[[:space:]]*"))))

(defcustom trex-beamer-beamer-mode "beamer"
  "The standard mode to assume for beamer's mode specification."
  :group 'talcum-render
  :type 'string)


(provide 'trex-beamer)
;;; File trex-beamer.el ends here.