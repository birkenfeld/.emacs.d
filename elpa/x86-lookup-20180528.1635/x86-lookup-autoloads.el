;;; x86-lookup-autoloads.el --- automatically extracted autoloads
;;
;;; Code:

(add-to-list 'load-path (directory-file-name
                         (or (file-name-directory #$) (car load-path))))


;;;### (autoloads nil "x86-lookup" "x86-lookup.el" (0 0 0 0))
;;; Generated autoloads from x86-lookup.el

(autoload 'x86-lookup "x86-lookup" "\
Jump to the PDF documentation for MNEMONIC.
Defaults to the mnemonic under point.

\(fn MNEMONIC)" t nil)

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "x86-lookup" '("x86-lookup-")))

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; coding: utf-8
;; End:
;;; x86-lookup-autoloads.el ends here
