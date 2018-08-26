;;; winpoint-autoloads.el --- automatically extracted autoloads
;;
;;; Code:

(add-to-list 'load-path (directory-file-name
                         (or (file-name-directory #$) (car load-path))))


;;;### (autoloads nil "winpoint" "winpoint.el" (0 0 0 0))
;;; Generated autoloads from winpoint.el

(defalias 'window-point-remember-mode 'winpoint-mode)

(defvar winpoint-mode nil "\
Non-nil if Winpoint mode is enabled.
See the `winpoint-mode' command
for a description of this minor mode.
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

If called interactively, enable Winpoint mode if ARG is positive, and
disable it if ARG is zero or negative.  If called from Lisp,
also enable the mode if ARG is omitted or nil, and toggle it
if ARG is `toggle'; disable the mode otherwise.

\(fn &optional ARG)" t nil)

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "winpoint" '("filter" "winpoint-")))

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; coding: utf-8
;; End:
;;; winpoint-autoloads.el ends here
