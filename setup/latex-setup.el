;; Setup LaTeX/AuCTeX related stuff.

;; Load preview-latex
(eval-after-load 'tex
  '(require 'preview))

;; Scale up the default size a bit
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
  '(define-key TeX-mode-map (kbd "C-c C-x") #'TeX-build-master))
