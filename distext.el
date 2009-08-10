;; ---------- distext.el init file: extensions installed by distribution -------

;; load path for Python modules, must be set before loading pymacs
(setq pymacs-load-path '("~/.emacs.d/pymacs"
                         "~/devel/ext/ropemacs" "~/devel/ext/ropemode"))

;; load the pastebin connector
(defun pastebin ()
  (interactive)
  (require 'pymacs)
  (pymacs-load "pastemacs" "paste-")
  (paste-menu))

;; load ropemacs
(defun load-ropemacs ()
  "Load pymacs and ropemacs"
  (interactive)
  (require 'pymacs)
  (pymacs-load "ropemacs" "rope-")
  ;; Automatically save project python buffers before refactorings
  (setq ropemacs-confirm-saving 'nil)
)

;; load ropemacs automatically for python-mode
(eval-after-load 'python-mode '(ac-ropemacs-init))

;; load preview-latex
(require 'preview)

;; colored moccur
(require 'color-moccur)

;; pylint (call pylint from python-mode)
(eval-after-load 'python-mode '(require 'pylint))

;; grep-edit (edit in grep results)
(require 'grep-edit)
;; need to unset ordinary character bindings...
(loop for key in '("n" "p" "{" "}" " ") do
      (define-key grep-mode-map key nil))

;; session (saves histories, variables, ...)
(require 'session)
(session-initialize)

;; semantic
(setq semantic-load-turn-useful-things-on t)
(require 'semantic)
(require 'semanticdb) 
;(semantic-load-enable-code-helpers)

(require 'semantic-tag-folding)
(global-semantic-tag-folding-mode 1)
(global-set-key (kbd "C-c <left>") 'semantic-tag-folding-fold-block)
(global-set-key (kbd "C-c <right>") 'semantic-tag-folding-show-block)

;; adaptive fill
(require 'filladapt)
(setq-default filladapt-mode t)

;; yet another snippet mode
(require 'yasnippet)
(yas/initialize)
(yas/load-directory "/usr/share/emacs/etc/yasnippet/snippets")
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

;; browse-kill-ring
(require 'browse-kill-ring)
(browse-kill-ring-default-keybindings)
(defadvice yank-pop (around kill-ring-browse-maybe (arg) activate)
  "If last action was not a yank, run `browse-kill-ring' instead."
  (if (not (eq last-command 'yank))
      (browse-kill-ring)
    ad-do-it))
