;;; toml-mode-autoloads.el --- automatically extracted autoloads
;;
;;; Code:
(add-to-list 'load-path (directory-file-name (or (file-name-directory #$) (car load-path))))

;;;### (autoloads nil "toml-mode" "toml-mode.el" (0 0 0 0))
;;; Generated autoloads from toml-mode.el

(autoload 'toml-mode "toml-mode" "\


\(fn)" t nil)

(add-to-list 'auto-mode-alist '("\\.toml$" . toml-mode))

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "toml-mode" '("toml-")))

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; End:
;;; toml-mode-autoloads.el ends here