;;; unicode-troll-stopper-autoloads.el --- automatically extracted autoloads
;;
;;; Code:

(add-to-list 'load-path (directory-file-name
                         (or (file-name-directory #$) (car load-path))))


;;;### (autoloads nil "unicode-troll-stopper" "unicode-troll-stopper.el"
;;;;;;  (0 0 0 0))
;;; Generated autoloads from unicode-troll-stopper.el

(autoload 'unicode-troll-stopper-mode "unicode-troll-stopper" "\
Highlight Unicode homoglyphs in the current buffer.

If called interactively, enable Unicode-Troll-Stopper mode if ARG is positive, and
disable it if ARG is zero or negative.  If called from Lisp,
also enable the mode if ARG is omitted or nil, and toggle it
if ARG is `toggle'; disable the mode otherwise.

\(fn &optional ARG)" t nil)

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "unicode-troll-stopper" '("unicode-troll-stopper--keywords")))

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; coding: utf-8
;; End:
;;; unicode-troll-stopper-autoloads.el ends here
