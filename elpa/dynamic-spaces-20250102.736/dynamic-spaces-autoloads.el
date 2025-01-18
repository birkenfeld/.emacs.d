;;; dynamic-spaces-autoloads.el --- automatically extracted autoloads (do not edit)   -*- lexical-binding: t -*-
;; Generated by the `loaddefs-generate' function.

;; This file is part of GNU Emacs.

;;; Code:

(add-to-list 'load-path (or (and load-file-name (directory-file-name (file-name-directory load-file-name))) (car load-path)))



;;; Generated autoloads from dynamic-spaces.el

(autoload 'dynamic-spaces-mode "dynamic-spaces" "\
Minor mode that adapts surrounding spaces when editing.

This is a minor mode.  If called interactively, toggle the
`Dynamic-Spaces mode' mode.  If the prefix argument is positive, enable
the mode, and if it is zero or negative, disable the mode.

If called from Lisp, toggle the mode if ARG is `toggle'.  Enable the
mode if ARG is nil, omitted, or is a positive number.  Disable the mode
if ARG is a negative number.

To check whether the minor mode is enabled in the current buffer,
evaluate the variable `dynamic-spaces-mode'.

The mode's hook is called both when the mode is enabled and when it is
disabled.

(fn &optional ARG)" t)
(put 'dynamic-spaces-global-mode 'globalized-minor-mode t)
(defvar dynamic-spaces-global-mode nil "\
Non-nil if Dynamic-Spaces-Global mode is enabled.
See the `dynamic-spaces-global-mode' command
for a description of this minor mode.
Setting this variable directly does not take effect;
either customize it (see the info node `Easy Customization')
or call the function `dynamic-spaces-global-mode'.")
(custom-autoload 'dynamic-spaces-global-mode "dynamic-spaces" nil)
(autoload 'dynamic-spaces-global-mode "dynamic-spaces" "\
Toggle Dynamic-Spaces mode in all buffers.
With prefix ARG, enable Dynamic-Spaces-Global mode if ARG is positive;
otherwise, disable it.

If called from Lisp, toggle the mode if ARG is `toggle'.
Enable the mode if ARG is nil, omitted, or is a positive number.
Disable the mode if ARG is a negative number.

Dynamic-Spaces mode is enabled in all buffers where
`dynamic-spaces-activate-if-applicable' would do it.

See `dynamic-spaces-mode' for more information on Dynamic-Spaces mode.

(fn &optional ARG)" t)
(register-definition-prefixes "dynamic-spaces" '("dynamic-spaces-"))

;;; End of scraped data

(provide 'dynamic-spaces-autoloads)

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; no-native-compile: t
;; coding: utf-8-emacs-unix
;; End:

;;; dynamic-spaces-autoloads.el ends here
