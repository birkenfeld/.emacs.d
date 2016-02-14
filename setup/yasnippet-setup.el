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

(define-key yas-keymap (kbd "C-e") #'yas-goto-end-of-active-field)
(define-key yas-keymap (kbd "C-a") #'yas-goto-start-of-active-field)

;; No dropdowns please, yas
(setq yas-prompt-functions '(yas/ido-prompt yas/completing-prompt))

;; No need to be so verbose
(setq yas-verbosity 1)

;; Wrap around region
(setq yas-wrap-around-region t)

;; Remove trailing whitespaces left after expanding a snippet
(add-hook 'yas-after-exit-snippet-hook
          #'(lambda () (delete-trailing-whitespace yas-snippet-beg yas-snippet-end)))

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
   ("newmod"
    "# -*- coding: utf-8 -*-
\"\"\"
    ${1:name}
    ${1:$(make-string (string-width yas-text) ?\\~)}

    $2

    :copyright: `(format-time-string \"%Y\")` by Georg Brandl.
    :license: ${BSD}.
\"\"\"

$0" "module header" nil nil ((yas-indent-line 'fixed)))
   ("newmodex"
    "#!/usr/bin/env python
# -*- coding: utf-8 -*-
\"\"\"
    ${1:name}
    ${1:$(make-string (string-width yas-text) ?\\~)}

    $2

    :copyright: `(format-time-string \"%Y\")` by Georg Brandl.
    :license: ${BSD}.
\"\"\"

$0" "module header, executable")
   ("prexc" "import traceback; traceback.print_exc()" "print tb" nil ("Debug"))
   ("pdb" "import pdb; pdb.set_trace()" "pdb.set_trace()" nil ("Debug"))
   ("ipdb" "import ipdb; ipdb.set_trace()" "ipdb trace" nil ("Debug"))
   ("stack" "import traceback; traceback.print_stack()" "print stack" nil ("Debug"))
   ("pprint" "import pprint; pprint.pprint($0)" "pprint" nil ("Debug"))
   ("visit"
    "def visit_${1:nodeclass}(self, node):
    ${2:pass}

def depart_$1(self, node):
    ${3:pass}" "docutils node visit")
   ("roprop" "def _get_${1:foo}(self):\n    return self._$1\n$1 = property(_get_$1)\n\n$0\n" "readonly property")
   ("modmain"
    "def main(args):
    $0

if __name__ == '__main__':
    import sys
    sys.exit(main(sys.argv))" "module main stanza")
   ))

(yas-define-snippets
 'rst-mode
 '(
   ("c" "\\`\\`$1\\`\\` $0" "code insertion")
   ))
