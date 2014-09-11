;;; eproject-gb.el --- more eproject tools

;; Copyright (C) 2010, 2014  Georg Brandl

;; Author: Georg Brandl <georg@python.org>
;; Keywords: eproject

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;;; Code:

(require 'eproject)
(require 'full-ack)

(define-project-type python (generic)
  (look-for "setup.py")
  :relevant-files ("\\.py$" "\\.rst$" "\\.js$" "\\.html$" "\\.c$" "\\.h$")
  :irrelevant-files ("\\.py[co]$"))

(define-project-type haskell (generic)
  (or (look-for "Setup.hs") (look-for "Setup.lhs"))
  :relevant-files ("\\.hs$" "\\.lhs$" "\\.c$"))

(define-project-type elisp (generic)
  (look-for "init.el")
  :relevant-files ("\\.el$")
  :irrelevant-files ("\\.elc$"))

(defun eproject-ack (regexp)
  "Search all files in the current project for REGEXP, using Ack."
  (interactive "sRegexp ack: ")
  (let* ((root (eproject-root))
         (default-directory root)
         (files (eproject-list-project-files-relative root)))
    (ack regexp t default-directory)))

(defun eproject-test ()
  "Run test suite."
  (interactive)
  (unless test-case-mode (test-case-mode 1))
  (setq test-cwd (eproject-root))
  (setq test ".")
  (test-case-run))

(defun eproject-find-tag ()
  "Find a tag via tags table."
  (interactive)
  (let* ((root (eproject-root))
         (table-file (concat root ".tags")))
    (when (not (equal tags-file-name table-file))
      (shell-command
       (format "ctags -f %s --languages=-HTML,JavaScript --python-kinds=-iv -e -R %s"
               table-file (directory-file-name root)))
      (let ((revert-without-query '(".tags")))
        (visit-tags-table table-file)))
    (if current-prefix-arg
        (call-interactively #'find-tag)
      (tags-completion-table)
      (let (tag-names)
        (mapatoms (lambda (x)
                    (push (prin1-to-string x t) tag-names))
                  tags-completion-table)
        (find-tag (ido-completing-read "Find tag: " tag-names))))))

(defun eproject-find-next-tag ()
  "Find next tag."
  (interactive)
  (find-tag last-tag t))

(provide 'eproject-gb)
;;; eproject-gb.el ends here
