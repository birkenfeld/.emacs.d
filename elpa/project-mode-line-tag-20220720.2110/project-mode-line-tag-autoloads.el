;;; project-mode-line-tag-autoloads.el --- automatically extracted autoloads  -*- lexical-binding: t -*-
;;
;;; Code:

(add-to-list 'load-path (directory-file-name
                         (or (file-name-directory #$) (car load-path))))


;;;### (autoloads nil "project-mode-line-tag" "project-mode-line-tag.el"
;;;;;;  (0 0 0 0))
;;; Generated autoloads from project-mode-line-tag.el

(defvar project-mode-line-tag-mode nil "\
Non-nil if Project-Mode-Line-Tag mode is enabled.
See the `project-mode-line-tag-mode' command
for a description of this minor mode.
Setting this variable directly does not take effect;
either customize it (see the info node `Easy Customization')
or call the function `project-mode-line-tag-mode'.")

(custom-autoload 'project-mode-line-tag-mode "project-mode-line-tag" nil)

(autoload 'project-mode-line-tag-mode "project-mode-line-tag" "\
Display the current buffer's project tag in its mode line.

This is a minor mode.  If called interactively, toggle the
`Project-Mode-Line-Tag mode' mode.  If the prefix argument is
positive, enable the mode, and if it is zero or negative, disable
the mode.

If called from Lisp, toggle the mode if ARG is `toggle'.  Enable
the mode if ARG is nil, omitted, or is a positive number.
Disable the mode if ARG is a negative number.

To check whether the minor mode is enabled in the current buffer,
evaluate `(default-value \\='project-mode-line-tag-mode)'.

The mode's hook is called both when the mode is enabled and when
it is disabled.

\(fn &optional ARG)" t nil)

(register-definition-prefixes "project-mode-line-tag" '("project-mode-line-tag"))

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; coding: utf-8
;; End:
;;; project-mode-line-tag-autoloads.el ends here
