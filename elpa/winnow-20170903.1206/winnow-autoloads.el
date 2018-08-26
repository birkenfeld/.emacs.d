;;; winnow-autoloads.el --- automatically extracted autoloads
;;
;;; Code:

(add-to-list 'load-path (directory-file-name
                         (or (file-name-directory #$) (car load-path))))


;;;### (autoloads nil "winnow" "winnow.el" (0 0 0 0))
;;; Generated autoloads from winnow.el

(autoload 'winnow-mode "winnow" "\
Filter compilation results by matching/excluding lines.

If called interactively, enable Winnow mode if ARG is positive, and
disable it if ARG is zero or negative.  If called from Lisp,
also enable the mode if ARG is omitted or nil, and toggle it
if ARG is `toggle'; disable the mode otherwise.

This is invaluable for excluding or limiting to matching `ag-mode' results.

\\{winnow-mode-map}

\(fn &optional ARG)" t nil)

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "winnow" '("winnow-")))

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; coding: utf-8
;; End:
;;; winnow-autoloads.el ends here
