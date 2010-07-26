;; ---------- auctex.el init file: some auctex customizations ------------------

;; load preview-latex
(require 'preview)

;; scale up the default size a bit
(defun my-preview-scale-from-face ()
  "Calculate preview scale from `preview-reference-face'.
This calculates the scale of EPS images from a document assumed
to have a default font size given by function `preview-document-pt'
so that they match the reference face in height."
  `(lambda nil
     (/ ,(/ (preview-inherited-face-attribute 'preview-reference-face :height
					      'default) 10.0)
	(* (preview-document-pt) 0.85))))

(setq preview-scale-function 'my-preview-scale-from-face)

;; new version from CVS, fixes problems with position in \include files
(defun LaTeX-pdfsync-output-page ()
  "Return page number in output file corresponding to buffer position."
  (let* ((line (TeX-line-number-at-pos))
	 (master (TeX-active-master))
	 (file (file-name-sans-extension
		(file-relative-name (buffer-file-name)
				    (file-name-directory master))))
	 (pdfsync-file (concat master ".pdfsync"))
	 (buf-live-p (get-file-buffer pdfsync-file))
	 (sync-record "0")
	 (sync-line "-1")
	 (sync-page "1")
	 last-match)
    (when (file-exists-p pdfsync-file)
      (with-current-buffer (find-file-noselect pdfsync-file)
	(save-restriction
	  (goto-char (point-min))
	  ;; Narrow region to file in question.
	  (when (not (string= file master))
	    (re-search-forward (concat "^(" file "\\(.tex\\)?$") nil t)
	    (let ((beg (match-beginning 0)))
	      (goto-char beg)
	      (narrow-to-region (line-beginning-position 2)
				(progn (forward-sexp) (point))))
	    (goto-char (point-min)))
	  ;; Look for the record number.
	  (catch 'break
	    (while (re-search-forward "^(\\|^l \\([0-9]+\\) \\([0-9]+\\)" nil t)
	      (cond ((string= (match-string 0) "(")
		     (goto-char (match-beginning 0))
		     (forward-sexp))
		    ((> (string-to-number (match-string 2)) line)
		     (throw 'break nil))
		    (t
		     (setq sync-record (match-string 1)
			   sync-line (match-string 2)
			   last-match (match-beginning 0))))))
	  ;; Look for the page number.
	  (goto-char (or last-match (point-min)))
	  ;; There might not be any p or s lines for the current file,
	  ;; so make it possible to search further.
	  (widen)
	  (catch 'break
	    (while (re-search-forward "^p \\([0-9]+\\)" nil t)
	      (when (>= (string-to-number (match-string 1))
			(string-to-number sync-record))
		(re-search-backward "^s \\([0-9]+\\)" nil t)
		(setq sync-page (match-string 1))
		(throw 'break nil)))))
	;; Kill the buffer if it was loaded by us.
	(unless buf-live-p (kill-buffer (current-buffer)))))
    sync-page))

(defun TeX-build-master ()
  "Run all necessary steps to build the master file, then view it."
  (interactive)
  (save-buffer)
  (let ((master-file  (TeX-master-file))
        (next-command nil)
        (keep-running t)
        (result       nil)
        ;; override some AUCTeX variables
        (TeX-process-asynchronous nil)
        (TeX-command-force        ""))
    (while keep-running
      (setq next-command (or (TeX-command-query master-file)
                             TeX-command-Show))
      (message "running %s" next-command)
      (setq result (save-current-buffer
                     (TeX-command next-command 'TeX-master-file -1)))
      (setq keep-running (not (or   ;; stop if...
                               ;; latex errored out
                               (plist-get TeX-error-report-switches
                                             (intern master-file))
                               ;; last command was Show
                               (eq result nil)))))))

(defadvice reftex-offer-label-menu (around dont-delete-other-windows activate)
  (flet ((delete-other-windows () nil))
    ad-do-it))

(defadvice reftex-offer-bib-menu (around dont-delete-other-windows activate)
  (flet ((delete-other-windows () nil))
    ad-do-it))

(eval-after-load 'tex
  '(define-key TeX-mode-map (kbd "C-c C-x") 'TeX-build-master))
