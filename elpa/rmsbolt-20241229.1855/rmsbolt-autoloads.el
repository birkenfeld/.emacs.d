;;; rmsbolt-autoloads.el --- automatically extracted autoloads (do not edit)   -*- lexical-binding: t -*-
;; Generated by the `loaddefs-generate' function.

;; This file is part of GNU Emacs.

;;; Code:

(add-to-list 'load-path (or (and load-file-name (directory-file-name (file-name-directory load-file-name))) (car load-path)))



;;; Generated autoloads from rmsbolt.el

(autoload 'rmsbolt-starter "rmsbolt" "\
Setup new file based on the sample for the language provided.

Uses LANG-NAME to determine the language.

(fn LANG-NAME)" t)
(autoload 'rmsbolt-mode "rmsbolt" "\
Toggle `rmsbolt-mode'.

This mode is enabled in both src and assembly output buffers.

This is a minor mode.  If called interactively, toggle the `Rmsbolt
mode' mode.  If the prefix argument is positive, enable the mode, and if
it is zero or negative, disable the mode.

If called from Lisp, toggle the mode if ARG is `toggle'.  Enable the
mode if ARG is nil, omitted, or is a positive number.  Disable the mode
if ARG is a negative number.

To check whether the minor mode is enabled in the current buffer,
evaluate the variable `rmsbolt-mode'.

The mode's hook is called both when the mode is enabled and when it is
disabled.

(fn &optional ARG)" t)
(autoload 'rmsbolt "rmsbolt" "\
Start a rmsbolt compilation and enable `rmsbolt-mode'.

Provides code region highlighting and automatic recompilation." t)
(register-definition-prefixes "rmsbolt" '("rmsbolt-"))


;;; Generated autoloads from rmsbolt-java.el

(register-definition-prefixes "rmsbolt-java" '("rmsbolt-java-"))


;;; Generated autoloads from rmsbolt-split.el

(register-definition-prefixes "rmsbolt-split" '("rmsbolt-split-"))

;;; End of scraped data

(provide 'rmsbolt-autoloads)

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; no-native-compile: t
;; coding: utf-8-emacs-unix
;; End:

;;; rmsbolt-autoloads.el ends here
