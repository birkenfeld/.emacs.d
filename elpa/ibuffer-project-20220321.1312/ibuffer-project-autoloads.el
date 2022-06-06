;;; ibuffer-project-autoloads.el --- automatically extracted autoloads  -*- lexical-binding: t -*-
;;
;;; Code:

(add-to-list 'load-path (directory-file-name
                         (or (file-name-directory #$) (car load-path))))


;;;### (autoloads nil "ibuffer-project" "ibuffer-project.el" (0 0
;;;;;;  0 0))
;;; Generated autoloads from ibuffer-project.el
 (autoload 'ibuffer-do-sort-by-project-file-relative "ibuffer-project")
 (autoload 'ibuffer-make-column-project-file-relative "ibuffer-project")

(autoload 'ibuffer-project-generate-filter-groups "ibuffer-project" "\
Create ibuffer filters based on project root of buffers." nil nil)

(register-definition-prefixes "ibuffer-project" '("ibuffer-project-" "project-"))

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; coding: utf-8
;; End:
;;; ibuffer-project-autoloads.el ends here
