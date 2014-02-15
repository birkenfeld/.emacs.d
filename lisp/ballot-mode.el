;;; ballot-mode.el --- Easy voting with text-format ballots

;; Copyright (C) 2010  Georg Brandl <georg@python.org>

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 2 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Code:

(defun ballot-next-checkbox ()
  "Move to the next checkbox."
  (interactive)
  (let ((where (text-property-any (+ (point) 1) (point-max) 'checkbox t)))
    (if where
        (goto-char where)
      (error "No next checkbox found"))))

(defun ballot-previous-checkbox ()
  "Move to the previous checkbox."
  (interactive)
  (let ((where (previous-single-char-property-change (- (point) 1) 'checkbox)))
    (if where
        (goto-char (- where 1))
      (error "No next checkbox found"))))

(defun ballot-toggle-checkbox ()
  "Toggle current checkbox."
  (interactive)
  (let ((where (text-property-any (- (point) 1) (+ (point) 2) 'checkbox t)))
    (if where
        (let ((inhibit-read-only t))
          (goto-char where)
          (if (eq (char-after) ?X)
              (insert-and-inherit (propertize " " 'checkbox t))
            (insert-and-inherit (propertize "X" 'checkbox t)))
          (delete-char 1)
          (backward-char))
      (error "No checkbox at point"))))

(defun ballot-clear-properties ()
  "Remove properties added by ballot mode."
  (let ((inhibit-read-only t))
    (remove-text-properties (point-min) (point-max)
                            '(checkbox nil read-only nil
                              mouse-face nil keymap nil))))

(defvar ballot-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "TAB") 'ballot-next-checkbox)
    (define-key map (kbd "<backtab>") 'ballot-previous-checkbox)
    (define-key map (kbd "SPC") 'ballot-toggle-checkbox)
    map)
  "Map for use in `ballot-mode'.")

(defvar ballot-checkbox-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "<mouse-1>") 'ballot-toggle-checkbox)
    map)
  "Mouse map for use on checkboxes.")

(define-derived-mode ballot-mode text-mode "Ballot"
  "A mode for easy filling of text-format ballots.

\\{ballot-mode-map}"
  (use-local-map ballot-mode-map)
  (add-hook 'change-major-mode-hook 'ballot-clear-properties)
  (setq font-lock-defaults
          '((("^\\(Ballot .*\\)$" 1 font-lock-function-name-face)
             ("^\\(-+\\)$" 1 font-lock-function-name-face)
             ("\\([a-zA-Z]+\\) \\(( )\\)" (1 font-lock-comment-face)
                                          (2 font-lock-variable-name-face))
             ("\\([a-zA-Z]+\\) \\((X)\\)" (1 font-lock-comment-face)
                                          (2 font-lock-keyword-face)))))
  (let ((modified (buffer-modified-p)))
    (save-restriction
      (save-excursion
        (widen)
        (goto-char (point-min))
        (while (re-search-forward "([ X])" nil t)
          (add-text-properties (- (point) 3) (point) `(pointer hand
                                                       mouse-face highlight
                                                       keymap ,ballot-checkbox-map))
          (add-text-properties (- (point) 2) (- (point) 1) '(checkbox t)))
        (add-text-properties (point-min) (point-max) '(read-only t))))
    (set-buffer-modified-p modified))
  (ballot-next-checkbox)
  (message
   "Use SPC to toggle checkbox, TAB and Shift-TAB to move between checkboxes"))

(add-to-list 'auto-mode-alist '("\\.ballot$" . ballot-mode))

(provide 'ballot-mode)
;;; ballot-mode.el ends here
