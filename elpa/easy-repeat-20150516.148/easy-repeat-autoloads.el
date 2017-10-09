;;; easy-repeat-autoloads.el --- automatically extracted autoloads
;;
;;; Code:
(add-to-list 'load-path (directory-file-name (or (file-name-directory #$) (car load-path))))

;;;### (autoloads nil "easy-repeat" "easy-repeat.el" (23003 223 478523
;;;;;;  230000))
;;; Generated autoloads from easy-repeat.el

(autoload 'easy-repeat-add-last-command "easy-repeat" "\
Add the last command to `easy-repeat-command-list'.

\(fn)" t nil)

(autoload 'easy-repeat-add-key "easy-repeat" "\
Add the binding of KEY in current keymaps to `easy-repeat-command-list'.

\(fn KEY)" t nil)

(defvar easy-repeat-mode nil "\
Non-nil if Easy-Repeat mode is enabled.
See the `easy-repeat-mode' command
for a description of this minor mode.
Setting this variable directly does not take effect;
either customize it (see the info node `Easy Customization')
or call the function `easy-repeat-mode'.")

(custom-autoload 'easy-repeat-mode "easy-repeat" nil)

(autoload 'easy-repeat-mode "easy-repeat" "\
Repeat easily.
Repeat by last short key, e.g., use 'o' to repeat 'C-x o'.

\(fn &optional ARG)" t nil)

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; End:
;;; easy-repeat-autoloads.el ends here
