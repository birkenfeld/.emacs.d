;;; winpoint-autoloads.el --- automatically extracted autoloads
;;
;;; Code:


;;;### (autoloads (winpoint-mode) "winpoint" "winpoint.el" (20730
;;;;;;  20634 0 0))
;;; Generated autoloads from winpoint.el

(defalias 'window-point-remember-mode 'winpoint-mode)

(defvar winpoint-mode nil "\
Non-nil if Winpoint mode is enabled.
See the command `winpoint-mode' for a description of this minor mode.
Setting this variable directly does not take effect;
either customize it (see the info node `Easy Customization')
or call the function `winpoint-mode'.")

(custom-autoload 'winpoint-mode "winpoint" nil)

(autoload 'winpoint-mode "winpoint" "\
Remember positions in a buffer per-window, not per-buffer.
That is, when you have the same buffer open in two different
windows, and you switch the buffer in one window and back again,
the position is the same as it was when you switched away, not
the same as in the other window.

\(fn &optional ARG)" t nil)

;;;***

;;;### (autoloads nil nil ("winpoint-pkg.el") (20730 20634 408121
;;;;;;  71000))

;;;***

(provide 'winpoint-autoloads)
;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; coding: utf-8
;; End:
;;; winpoint-autoloads.el ends here
