;; Flymake setup

;; Display flymake errors in modeline
(require 'flymake-cursor)


(defun next-error-or-flymake ()
  "Go to next error, or if that is not defined, to next flymake error."
  (interactive)
  (condition-case err
      (next-error)
    (error
     (if (equal (cadr err) "No buffers contain error message locations")
         (flymake-goto-next-error)
       (signal (car err) (cdr err))))))

(defun previous-error-or-flymake ()
  "Go to previous error, or if that is not defined, to previous flymake error."
  (interactive)
  (condition-case err
      (previous-error)
    (error
     (if (equal (cadr err) "No buffers contain error message locations")
         (flymake-goto-prev-error)
       (signal (car err) (cdr err))))))


;; Flymake error finding with the same keys as grep/occur/etc.
(global-set-key (kbd "M-g M-e") 'flymake-goto-next-error)
(global-set-key [remap next-error] 'next-error-or-flymake)
(global-set-key [remap previous-error] 'previous-error-or-flymake)
