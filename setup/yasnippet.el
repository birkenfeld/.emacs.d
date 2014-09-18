;; ---------- yasnippet.el init file: load yasnippet and define snippets -------

;; yet another snippet mode
(require 'yasnippet)

;; Inter-field navigation
(defun yas-goto-end-of-active-field ()
  (interactive)
  (let* ((snippet (car (yas--snippets-at-point)))
        (position (yas--field-end (yas--snippet-active-field snippet))))
    (if (= (point) position)
        (move-end-of-line 1)
      (goto-char position))))

(defun yas-goto-start-of-active-field ()
  (interactive)
  (let* ((snippet (car (yas--snippets-at-point)))
        (position (yas--field-start (yas--snippet-active-field snippet))))
    (if (= (point) position)
        (move-beginning-of-line 1)
      (goto-char position))))

(define-key yas-keymap (kbd "C-e") 'yas-goto-end-of-active-field)
(define-key yas-keymap (kbd "C-a") 'yas-goto-start-of-active-field)

;; No dropdowns please, yas
;(setq yas-prompt-functions '(yas/ido-prompt yas/completing-prompt))

;; No need to be so verbose
(setq yas-verbosity 1)

;; Wrap around region
(setq yas-wrap-around-region t)

(yas-define-snippets
 'haskell-mode
 '(
   ("--"
    "------------------------------------------------------------------------------------------
-- $0"
    "comment segment")))

(yas-define-snippets
 'latex-mode
 '(
   ("frame"
    "\\begin{frame}[<+->]
  \\myframetitle{$1}
  \\begin{itemize*}
  \\item $0
  \\end{itemize*}
\\end{frame}"
    "frame")))


(yas-define-snippets
 'python-mode
 '(
   ("p"
    "print($0)"
    "print stmt (python 3)"
    )
   ("newmod"
    "# -*- coding: utf-8 -*-
\"\"\"
    ${1:name}
    ${1:$(make-string (string-width text) ?\\~)}

    $2

    :copyright: `(format-time-string \"%Y\")` by Georg Brandl.
    :license: ${BSD}.
\"\"\"

$0" "module header" nil nil ((yas-indent-line 'fixed)))
   ("newmod.ex"
    "#!/usr/bin/env python
# -*- coding: utf-8 -*-
\"\"\"
    ${1:name}
    ${1:$(make-string (string-width text) ?\\~)}

    $2

    :copyright: `(format-time-string \"%Y\")` by Georg Brandl.
    :license: ${BSD}.
\"\"\"

$0" "module header, executable")
   ("prexc" "import traceback; traceback.print_exc()" "print tb" nil ("Debug"))
   ("ipdb" "import ipdb; ipdb.set_trace()" "ipdb trace" nil ("Debug"))
   ("stack" "import traceback; traceback.print_stack()" "print stack" nil ("Debug"))
   ("ipprint" "import pprint; pprint.pprint($0)" "pprint" nil ("Debug"))
   ("visit"
    "def visit_${1:nodeclass}(self, node):
    ${2:pass}
def depart_$1(self, node):
    ${3:pass}" "docutils node visit")
   ("roprop" "def _get_${1:foo}(self):\n    return self._$1\n$1 = property(_get_$1)\n\n$0\n" "readonly property")
   ))

;; snippets also provided by elpy

;; ("classdef"
;;  "class ${name}:
;;  \"\"\"
;;  ${docstring}
;;  \"\"\"

;;  def __init__(self, ${args}):
;;      $0" "class definition")
;;    ("modmain"
;;     "def main(args):
;;     $0

;; if __name__ == '__main__':
;;     import sys
;;     sys.exit(main(sys.argv))" "module main stanza")
;;    ("init" "def __init__(self, $1):
;;     \"\"\"$2\"\"\"
;;     ${1:$
;;     (mapconcat
;;      '(lambda (x)
;;         (if (not (string= (nth 0 x) \"\"))
;;             (concat \"self.\" (nth 0 x) \" = \" (nth 0 x))))
;;      (mapcar
;;       '(lambda (x)
;;          (mapcar
;;           '(lambda (x)
;;              (replace-regexp-in-string \"[[:blank:]]*$\" \"\"
;;               (replace-regexp-in-string \"^[[:blank:]]*\" \"\" x)))
;;           x))
;;       (mapcar '(lambda (x) (split-string x \"=\"))
;;               (split-string text \",\")))
;;      (concat \"\\n\" (make-string (current-column) 32)))
;;     }
;;     $0
;; " "init method")


(yas-define-snippets
 'rst-mode
 '(
   ("c" "\\`\\`$1\\`\\` $0" "code insertion")
   ))
