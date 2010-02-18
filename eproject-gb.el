;;; eproject-gb.el --- more eproject tools

;; Copyright (C) 2010  Georg Brandl

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
  :relevant-files ("\\.py$" "\\.rst$" "\\.c$" "\\.h$")
  :irrelevant-files ("\\.py[co]$"))

;; remember the last automatically opened rope project, to avoid
;; re-opening it all the time
(defvar eproject-auto-rope-project nil)

;; automatically open rope project when visiting a Python project file
(add-hook 'python-project-file-visit-hook
          (lambda ()
            (when (fboundp 'rope-open-project)
              (let ((root (eproject-root)))
                (unless (equal eproject-auto-rope-project root)
                  (rope-open-project root)
                  (setq eproject-auto-rope-project root))))))

(define-project-type haskell (generic)
  (or (look-for "Setup.hs") (look-for "Setup.lhs"))
  :relevant-files ("\\.hs$" "\\.lhs$" "\\.c$"))

(define-project-type elisp (generic)
  (look-for "recompile")
  :relevant-files ("\\.el$")
  :irrelevant-files ("\\.elc$"))

(defun eproject-ack (regexp)
  "Search all files in the current project for REGEXP, using Ack."
  (interactive "sRegexp ack: ")
  (let* ((root (eproject-root))
         (default-directory root)
         (files (eproject-list-project-files-relative root)))
    (ack regexp t default-directory)))

(provide 'eproject-gb)
;;; eproject-gb.el ends here
