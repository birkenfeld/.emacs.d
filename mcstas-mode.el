;;; mcstas-mode.el --- McStas simulation mode <gbr@ndl>

;; Copyright (C) 2011 Georg Brandl.

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

;;; Code:


(defvar mcstas-font-lock-keywords
  `(;; keywords
    (,(regexp-opt '("COMPONENT" "DECLARE" "DEFINE" "DEFINITION" "END"
                    "MCDISPLAY" "FINALLY" "EXTERN" "INITIALIZE" "INSTRUMENT"
                    "OUTPUT" "PARAMETERS" "POLARISATION" "SETTING" "STATE"
                    "TRACE" "SHARE" "EXTEND" "GROUP" "NEXUS" "SAVE" "ITERATE"
                    "%{" "%}"
                    ) 'words)
     1 font-lock-keyword-face)
    ;; keywords, but in single components
    (,(regexp-opt '("ABSOLUTE" "AT" "RELATIVE" "ROTATED" "PREVIOUS" "JUMP"
                    "WHEN" "NEXT" "MYSELF" "COPY" "SPLIT") 'words)
     1 font-lock-builtin-face)
    ;; component names
    ("\\<\\(?:COMPONENT\\|INSTRUMENT\\)[ \t]+\\([a-zA-Z_]+[a-zA-Z0-9_]*\\)"
     1 font-lock-type-face))
  "Additional font lock keywords for McStas mode.")

(defvar mcstas-mode-map
  (let ((map (make-sparse-keymap)))
    ;; Will inherit from `c-mode-map' thanks to define-derived-mode.
    (define-key map ";" 'self-insert-command)
    (define-key map "{" 'self-insert-command)
    (define-key map "}" 'self-insert-command)
    (define-key map "," 'self-insert-command)
    (define-key map "(" 'self-insert-command)
    (define-key map ")" 'self-insert-command)
    (define-key map "\C-c\C-c" 'mcstas-compile)
    map)
  "Keymap used in `mcstas-mode'.")

(define-derived-mode mcstas-mode c-mode "McStas"
  "Major mode for McStas simulations."
  (setcar font-lock-defaults
          (append c-font-lock-keywords mcstas-font-lock-keywords))
  (set (make-local-variable 'compile-command)
       (concat "mcrun " buffer-file-name))
  )


(add-to-list 'auto-mode-alist '("\\.instr$" . mcstas-mode))
(add-to-list 'auto-mode-alist '("\\.comp$" . mcstas-mode))

(provide 'mcstas-mode)
;;; mcstas-mode.el ends here
