;;; d-mode-autoloads.el --- automatically extracted autoloads  -*- lexical-binding: t -*-
;;
;;; Code:

(add-to-list 'load-path (directory-file-name
                         (or (file-name-directory #$) (car load-path))))


;;;### (autoloads nil "d-mode" "d-mode.el" (0 0 0 0))
;;; Generated autoloads from d-mode.el

(add-to-list 'auto-mode-alist '("\\.d[i]?\\'" . d-mode))

(defvar d-mode-hook nil "\
*Hook called by `d-mode'.")

(custom-autoload 'd-mode-hook "d-mode" t)

(autoload 'd-mode "d-mode" "\
Major mode for editing code written in the D Programming Language.

See http://dlang.org for more information about the D language.

The hook `c-mode-common-hook' is run with no args at mode
initialization, then `d-mode-hook'.

Key bindings:
\\{d-mode-map}

\(fn)" t nil)

(register-definition-prefixes "d-mode" '("d-"))

;;;***

;;;### (autoloads nil "d-mode-test" "d-mode-test.el" (0 0 0 0))
;;; Generated autoloads from d-mode-test.el

(register-definition-prefixes "d-mode-test" '("d-test-" "do-one-test" "kill-test-buffer" "make-test-buffer"))

;;;***

;;;### (autoloads nil nil ("d-mode-pkg.el") (0 0 0 0))

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; coding: utf-8
;; End:
;;; d-mode-autoloads.el ends here
