;;; isearch-project.el --- Incremental search through the whole project  -*- lexical-binding: t; -*-

;; Copyright (C) 2019-2025  Shen, Jen-Chieh
;; Created date 2019-03-18 15:16:04

;; Author: Shen, Jen-Chieh <jcs090218@gmail.com>
;; URL: https://github.com/jcs-elpa/isearch-project
;; Package-Version: 20250101.1008
;; Package-Revision: abd8ee560d18
;; Package-Requires: ((emacs "27.1") (f "0.20.0"))
;; Keywords: convenience search

;; This file is NOT part of GNU Emacs.

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
;;
;; Incremental search through the whole project.
;;

;;; Code:

(require 'cl-lib)
(require 'grep)
(require 'isearch)
(require 'project)

(require 'f)

(defgroup isearch-project nil
  "Incremental search through the whole project."
  :prefix "isearch-project-"
  :group 'isearch
  :link '(url-link :tag "Repository" "https://github.com/jcs-elpa/isearch-project"))

(defcustom isearch-project-ignore-paths '(".vs/"
                                          ".vscode/"
                                          "node_modules/")
  "List of path you want to ignore by Incremental searching in the project."
  :type 'list
  :group 'isearch-project)

(defvar isearch-project--search-path ""
  "Record the current search path, so when next time it searhs would not need
to research from the start.")

(defvar isearch-project--project-dir ""
  "Current isearch project directory.")

(defvar isearch-project--files '()
  "List of file path in the project.")

(defvar isearch-project--files-starting-index -1
  "Starting search path index, use with `isearch-project--files' list.")

(defvar isearch-project--files-current-index -1
  "Current search path index, use with `isearch-project--files' list.")

(defvar isearch-project--run-advice t
  "Flag to check if run advice.")

(defvar isearch-project--thing ""
  "Record down the symbol while executing
`isearch-project-forward-symbol-at-point' command.")

(defvar isearch-project-inhibit-yank nil
  "If non-nil inhibit yank.")

;;; Util

(defun isearch-project--is-contain-list-string (in-list in-str)
  "Check if IN-STR contain in any string in the IN-LIST."
  (cl-some #'(lambda (lb-sub-str) (string-match-p (regexp-quote lb-sub-str) in-str)) in-list))

(defun isearch-project--remove-nth-element (nth lst)
  "Remove NTH element from the LST and return the list."
  (if (zerop nth) (cdr lst)
    (let ((last (nthcdr (1- nth) lst)))
      (setcdr last (cddr last))
      lst)))

(defun isearch-project--string-from-file (path)
  "Return PATH file content."
  (with-temp-buffer (insert-file-contents path) (buffer-string)))

;;; Core

(defun isearch-project--f-directories-ignore-directories (path &optional rec)
  "Find all directories in PATH by ignored common directories with FN and REC."
  (let ((dirs (f-directories path))
        (valid-dirs)
        (final-dirs)
        (ignore-lst (append grep-find-ignored-directories
                            isearch-project-ignore-paths
                            project-vc-ignores
                            (when (boundp 'projectile-globally-ignored-directories)
                              projectile-globally-ignored-directories))))
    (dolist (dir dirs)
      (unless (isearch-project--is-contain-list-string ignore-lst (f-slash dir))
        (push dir valid-dirs)))
    (when rec
      (dolist (dir valid-dirs)
        (push (isearch-project--f-directories-ignore-directories dir rec) final-dirs)))
    (setq valid-dirs (reverse valid-dirs)
          final-dirs (reverse final-dirs))
    (flatten-list (append valid-dirs final-dirs))))

(defun isearch-project--f-files-ignore-directories (path &optional fn rec)
  "Find all files in PATH by ignored common directories with FN and REC."
  (let ((dirs (append (list path) (isearch-project--f-directories-ignore-directories path rec)))
        (files))
    (dolist (dir dirs)
      (push (f-files dir fn) files))
    (flatten-list (reverse files))))

(defun isearch-project--prepare ()
  "Incremental search preparation."
  (let (prepare-success)
    (setq isearch-project--project-dir
          (car (cl-remove-if-not #'stringp (project-current))))
    (when isearch-project--project-dir
      ;; Get the current buffer name.
      (setq isearch-project--search-path (buffer-file-name))
      ;; Get all the file from the project, and filter it with ignore path list.
      (setq isearch-project--files (isearch-project--f-files-ignore-directories isearch-project--project-dir nil t))
      ;; Reset to -1.
      (setq isearch-project--files-current-index -1
            prepare-success t))
    prepare-success))

;;;###autoload
(defun isearch-project-forward-symbol-at-point ()
  "Incremental search symbol forward at current point in the project."
  (interactive)
  (setq isearch-project--thing (thing-at-point 'symbol))
  (if (or (use-region-p)
          (char-or-string-p isearch-project--thing))
      (isearch-project-forward)
    (error "[isearch-project]: no region or symbol at point")))

;;;###autoload
(defun isearch-project-forward-thing-at-point ()
  "Incremental search thing forward at current point in the project."
  (interactive)
  (unless isearch-project--thing
    (let ((bounds (seq-some (lambda (thing)
                              (bounds-of-thing-at-point thing))
                            isearch-forward-thing-at-point)))
      (setq isearch-project--thing (buffer-substring-no-properties (car bounds) (cdr bounds)))))
  (if (or (use-region-p)
          (char-or-string-p isearch-project--thing))
      (isearch-project-forward)
    (error "[isearch-project]: no region or symbol at point")))

;;;###autoload
(defun isearch-project-forward ()
  "Incremental search forward in the project."
  (interactive)
  (if (isearch-project--prepare)
      (unwind-protect
          (progn
            (isearch-project--add-advices)
            (isearch-backward nil 1)
            (isearch-forward))
        (isearch-project--remove-advices))
    (error "Cannot isearch project without project directory defined")))

(defun isearch-project--add-advices ()
  "Add all needed advices."
  (advice-add 'isearch-repeat :after #'isearch-project--advice-isearch-repeat-after)
  (setq isearch-message-prefix-add "Project "))

(defun isearch-project--remove-advices ()
  "Remove all needed advices."
  (advice-remove 'isearch-repeat #'isearch-project--advice-isearch-repeat-after)
  (setq isearch-message-prefix-add nil
        isearch-project--thing nil))

(defun isearch-project--find-file-search (fn dt)
  "Open a file and isearch.
If found, leave it.  If not found, try find the next file.
FN : file to search.
DT : search direction."
  (find-file fn)
  (cl-case dt
    (`forward  (goto-char (point-min)))
    (`backward (goto-char (point-max))))
  (isearch-search-string isearch-string nil t)
  (let (isearch-project--run-advice)
    (cl-case dt
      (`forward  (isearch-repeat-forward))
      (`backward (isearch-repeat-backward)))))

(defun isearch-project--advice-isearch-repeat-after (dt &optional cnt)
  "Advice for `isearch-repeat-backward' and `isearch-repeat-forward' command.
DT : search direction.
CNT : search count."
  (when (and (not isearch-success) isearch-project--run-advice)
    (let ((next-file-index isearch-project--files-current-index) (next-fn ""))
      ;; Get the next file index.
      (when (= isearch-project--files-current-index -1)
        (setq isearch-project--files-current-index
              (cl-position isearch-project--search-path
                           isearch-project--files
                           :test 'string=))
        (setq next-file-index isearch-project--files-current-index))

      ;; Record down the starting file index.
      (setq isearch-project--files-starting-index isearch-project--files-current-index)

      (let ((buf-content "")
            (break-it)
            (search-cnt (if cnt cnt 1)))
        (while (not break-it)
          ;; Get the next file index.
          (cl-case dt
            (`backward (cl-decf next-file-index))
            (`forward (cl-incf next-file-index)))

          ;; Cycle it.
          (cl-case dt
            (`backward
             (when (< next-file-index 0)
               (setq next-file-index (- (length isearch-project--files) 1))))
            (`forward
             (when (>= next-file-index (length isearch-project--files))
               (setq next-file-index 0))))

          ;; Target the next file.
          (setq next-fn (nth next-file-index isearch-project--files))

          ;; Update buffer content.
          (setq buf-content (isearch-project--string-from-file next-fn))

          (when (or
                 ;; Found match.
                 (string-match-p isearch-string buf-content)
                 ;; Is the same as the starting file, this prevents infinite loop.
                 (= isearch-project--files-starting-index next-file-index))
            (cl-decf search-cnt)
            (when (<= search-cnt 0) (setq break-it t)))))

      ;; Open the file.
      (isearch-project--find-file-search next-fn dt)

      ;; Update current file index.
      (setq isearch-project--files-current-index next-file-index))))

(defun isearch-project--isearch-yank-string (search-str)
  "Isearch project allow error because we need to search through next file.
SEARCH-STR : Search string."
  (unless isearch-project-inhibit-yank
    (ignore-errors (isearch-yank-string search-str))))

(defun isearch-project--isearch-mode-hook ()
  "Paste the current symbol when `isearch' enabled."
  (cond ((and (use-region-p)
              (memq this-command '( isearch-project-forward
                                    isearch-project-forward-symbol-at-point
                                    isearch-project-forward-thing-at-point)))
         (let ((search-str (buffer-substring-no-properties (region-beginning) (region-end))))
           (deactivate-mark)
           (isearch-project--isearch-yank-string search-str)))
        ((memq this-command '( isearch-project-forward-symbol-at-point
                               isearch-project-forward-thing-at-point))
         (when (char-or-string-p isearch-project--thing)
           (unless (eobp)
             (forward-char 1))
           (forward-symbol -1)
           (isearch-project--isearch-yank-string isearch-project--thing)))))

(add-hook 'isearch-mode-hook #'isearch-project--isearch-mode-hook)

(provide 'isearch-project)
;;; isearch-project.el ends here
