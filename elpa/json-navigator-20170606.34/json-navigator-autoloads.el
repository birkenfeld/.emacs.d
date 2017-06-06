;;; json-navigator-autoloads.el --- automatically extracted autoloads
;;
;;; Code:
(add-to-list 'load-path (directory-file-name (or (file-name-directory #$) (car load-path))))

;;;### (autoloads nil "json-navigator" "json-navigator.el" (22838
;;;;;;  55351 193231 754000))
;;; Generated autoloads from json-navigator.el

(autoload 'json-navigator-navigate-after-point "json-navigator" "\
Navigate JSON after point.

\(fn)" t nil)

(autoload 'json-navigator-navigate-region "json-navigator" "\
Navigate JSON inside region between START and END.
If START (respectively END) is nil, use `point-min' (respectively `point-max') instead.

\(fn &optional START END)" t nil)

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; End:
;;; json-navigator-autoloads.el ends here
