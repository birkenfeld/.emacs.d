;;; fullframe-autoloads.el --- automatically extracted autoloads
;;
;;; Code:
(add-to-list 'load-path (directory-file-name (or (file-name-directory #$) (car load-path))))

;;;### (autoloads nil "fullframe" "fullframe.el" (22174 29920 156106
;;;;;;  898000))
;;; Generated autoloads from fullframe.el

(autoload 'fullframe/current-buffer-window-config "fullframe" "\
Return the window-configuration stored for the current buffer.

\(fn)" nil nil)

(autoload 'fullframe/erase-current-buffer-window-config "fullframe" "\
Forget the window config associated with the current buffer.

\(fn)" nil nil)

(autoload 'fullframe/set-current-buffer-window-config "fullframe" "\
Associate the current buffer with the window-configuration WCONF.

\(fn WCONF)" nil nil)

(autoload 'fullframe "fullframe" "\
Save window/frame state when executing COMMAND-ON.

Advises COMMAND-ON so that the buffer it displays will appear in
a full-frame window.  The previous window configuration will be
restored when COMMAND-OFF is executed in that buffer.  If
KILL-ON-COFF is non-nil, then the buffer will also be killed
after COMMAND-OFF has completed.

This function uses `defadvice' on versions of emacs < 24.4,
`advice-add' otherwise.

AFTER-COMMAND-ON-FUNC is called after COMMAND-ON was called and
the window it generated is the only one in in the frame.

\(fn COMMAND-ON COMMAND-OFF &optional KILL-ON-COFF AFTER-COMMAND-ON-FUNC)" nil t)

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; End:
;;; fullframe-autoloads.el ends here
