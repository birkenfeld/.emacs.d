;; Project setup

(define-key project-prefix-map (kbd "x") #'project-grep-todo)

(defun project-grep-todo ()
  "Find todo tags in project."
  (interactive)
  ;; TODO: display output in a buffer called *<project>-TODO* instead of *grep*.
  (project-find-regexp "TODO\\|XXX\\|FIXME"))

(defun project-recompile ()
  "Compile project with last compile command."
  (interactive)
  (let ((compilation-read-command nil))
    (project-compile)))

;; Show project in mode line
(require 'project-mode-line-tag)
(add-to-list 'global-mode-string project-mode-line-tag--mode-line-construct)
