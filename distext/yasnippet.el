;; ---------- yasnippet.el init file: load yasnippet and define snippets -------

;; yet another snippet mode
(require 'yasnippet)
(yas/initialize)
(when (file-directory-p "/usr/share/emacs/etc/yasnippet/snippets")
  (yas/load-directory "/usr/share/emacs/etc/yasnippet/snippets"))
(when (file-directory-p "/usr/share/emacs/site-lisp/yasnippet/snippets")
  (yas/load-directory "/usr/share/emacs/site-lisp/yasnippet/snippets"))
(setq yas/window-system-popup-function
      'yas/x-popup-menu-for-template)
;; auto-expand on tab
(add-hook 'python-mode-hook 'yas/minor-mode)

(yas/define-snippets
 'haskell-mode
 '(
   ("--"
    "------------------------------------------------------------------------------------------
-- $0"
    "comment segment"))
 'text-mode)

(yas/define-snippets
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


(yas/define-snippets
 'python-mode
 '(
   ("classdef"
    "class ${name}:
    \"\"\"
    ${docstring}
    \"\"\"

    def __init__(self, ${args}):
        $0" "class definition")
   ("newmod"
    "# -*- coding: utf-8 -*-
\"\"\"
    ${1:name}
    ${1:$(make-string (string-width text) ?\\~)}

    $2

    :copyright: `(format-time-string \"%Y\")` by Georg Brandl.
    :license: ${BSD}.
\"\"\"

$0" "module header")
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
   ("modmain"
    "def main(args):
    $0

if __name__ == '__main__':
    import sys
    sys.exit(main(sys.argv))" "module main stanza")
   ("itrace" "import pdb; pdb.set_trace()" "set trace")
   ("iprexc" "import traceback; traceback.print_exc()" "print tb")
   ("ipprint" "import pprint; pprint.pprint($0)" "pprint")
   ("visit"
    "def visit_${1:nodeclass}(self, node):
    ${2:pass}
def depart_$1(self, node):
    ${3:pass}" "docutils node visit")
   )
 'text-mode)

(yas/define-snippets
 'rst-mode
 '(
   ("c" "\\`\\`$1\\`\\` $0" "code insertion")
   )
 'text-mode)