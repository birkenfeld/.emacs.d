;; ---------- extensions.el init file ------------------------------------------

;; better, patched Python mode
(require 'python-mode)

;; auto-completion setup
(require 'auto-complete)
(require 'auto-complete-python)
(setq ac-auto-start nil)
(setq ac-auto-start-chars '("."))
(global-auto-complete-mode t)
(ac-ropemacs-init)

;; CVS haskell mode
(require 'haskell-mode)
(require 'inf-haskell)

(add-to-list 'auto-mode-alist '("\\.\\(?:[gh]s\\|hi\\)\\'" . haskell-mode))
(add-to-list 'auto-mode-alist '("\\.l[gh]s\\'" . literate-haskell-mode))

(add-hook 'haskell-mode-hook (lambda ()
  (imenu-add-menubar-index)
  (turn-on-haskell-doc-mode)
  (turn-on-haskell-indent)
  (inferior-haskell-process)
  (setq comment-start "--")
  (add-to-list 'filladapt-token-table '("-- " haskell-comment))
  (add-to-list 'filladapt-token-match-table '(haskell-comment haskell-comment))
  (add-to-list 'filladapt-token-conversion-table '(haskell-comment . exact))
  (add-hook 'before-save-hook 'delete-trailing-whitespace nil t)))

;; highlight "return" as a builtin
(font-lock-add-keywords 'haskell-mode
 '(("\\<\\(return\\)\\>" 1 font-lock-builtin-face prepend)))

;; redo
(require 'redo)

;; bar cursor
(require 'bar-cursor)
(bar-cursor-mode t)

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

;; twitter
(autoload 'twit-post "twit" "post to twitter" t)

;; ReST mode
(require 'rst)
(setq auto-mode-alist
      (append '(("\\.rst$" . rst-mode)
                ("\\.rest$" . rst-mode)
                ("CHANGES$" . rst-mode)
                ("NEWS$" . rst-mode))
              auto-mode-alist))
(add-hook 'rst-mode-hook
          (lambda () (set-variable 'show-trailing-whitespace 1)))

;; tabbar
(require 'tabbar)
(global-set-key [C-prior] 'tabbar-backward)
(global-set-key [C-next] 'tabbar-forward)
;; kill buffer on left click, switch mode on right click
(defun my-tabbar-home-function (event)
  (let ((mouse-button (event-basic-type event)))
    (cond
     ((eq mouse-button 'mouse-3)
      (tabbar-buffer-show-groups (not tabbar--buffer-show-groups)))
     ((eq mouse-button 'mouse-1)
      (kill-buffer nil))
     )))
(add-hook 'tabbar-init-hook
          (lambda () (setq tabbar-home-function 'my-tabbar-home-function))
          t)  ; append
(tabbar-mode 1)
(tabbar-mwheel-mode 1)

;; screen lines
(require 'screen-lines)

;; replace recent character
(require 'rrc)

;; colored moccur
(require 'color-moccur)

;; pylint (call pylint from python-mode)
(require 'pylint)

;; color-grep (automatic syncing between grep and source buffers)
(require 'color-grep)

;; grep-edit (edit in grep results)
(require 'grep-edit)

;; session (saves histories, variables, ...)
(require 'session)
(session-initialize)

;; semantic
(require 'semantic-tag-folding)
(global-semantic-tag-folding-mode 1)
(global-set-key (kbd "C-c <left>") 'semantic-tag-folding-fold-block)
(global-set-key (kbd "C-c <right>") 'semantic-tag-folding-show-block)

;; C eldoc mode (automatic function signature tips)
(load "c-eldoc")
(add-hook 'c-mode-hook 'c-turn-on-eldoc-mode)

;; org-mode (organizer, agenda, ...)
(require 'org)
;(global-set-key (kbd "<f12>") 'org-agenda)

;; color dabbrev-expanded phrases
(require 'dabbrev-highlight)

;; adaptive fill
(require 'filladapt)
(setq-default filladapt-mode t)

;; winpoint (remember point location by window)
(require 'winpoint)
(window-point-remember-mode 1)

;; bookmarks
(require 'bm)
(global-set-key (kbd "C-c b t") 'bm-toggle)
(global-set-key (kbd "C-c b n") 'bm-next)
(global-set-key (kbd "C-c b p") 'bm-previous)

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

;; show tabs
(require 'show-wspace)
(add-hook 'python-mode-hook 'highlight-tabs)

;; nxhtml
(load "/home/gbr/.emacs.d/nxhtml/autostart.el")

;; browse-kill-ring
(require 'browse-kill-ring)
(browse-kill-ring-default-keybindings)

;; talcum-mode: replaces LaTeX commands by Unicode symbols
(autoload 'talcum-mode "talcum")
(add-hook 'LaTeX-mode-hook 'talcum-mode)

;; highlight symbol at point
(require 'highlight-symbol)

;; Martin Blais' dubious paragraphs
(require 'dubious-paragraphs)
(global-set-key [(meta n)] 'dubious-forward-paragraph)
(global-set-key [(meta N)] 'dubious-forward-paragraph-scroll)
(global-set-key [(meta p)] 'dubious-backward-paragraph)
(global-set-key [(meta P)] 'dubious-backward-paragraph-scroll)

;; Martin Blais' repeatable macros
(require 'repeatable)
(repeatable-command-advice kmacro-end-and-call-macro)
(repeatable-substitute-binding 'search-for-this-word)
(repeatable-command-advice hl-symbol-and-jump)

;; mk-project: project management
(require 'mk-project)
(project-def "Sphinx"
      '((basedir "/home/gbr/devel/sphinx")
        (src-patterns ("*.py" "*.rst"))
        (ignore-patterns ("*.pyc" "*.pyo" "doc/_build/*"))
        (tags-file "/home/gbr/devel/sphinx/TAGS")
        (file-list-cache "/home/gbr/.emacs.d/file-cache-sphinx")
        (vcs hg)
        (compile-cmd "make")
        (startup-hook nil)
        (shutdown-hook nil)))

(global-set-key (kbd "C-c p c") 'project-compile)
(global-set-key (kbd "C-c p g") 'project-grep)
(global-set-key (kbd "C-c p l") 'project-load)
(global-set-key (kbd "C-c p u") 'project-unload)
(global-set-key (kbd "C-c p f") 'project-find-file)
(global-set-key (kbd "C-c p i") 'project-index)
(global-set-key (kbd "C-c p s") 'project-status)
(global-set-key (kbd "C-c p h") 'project-home)
(global-set-key (kbd "C-c p d") 'project-dired)
(global-set-key (kbd "C-c p t") 'project-tags)



;;; This was installed by package-install.el.
;;; This provides support for the package system and
;;; interfacing with ELPA, the package archive.
;;; Move this code earlier if you want to reference
;;; packages in your .emacs.
(when
    (load
     (expand-file-name "~/.emacs.d/elpa/package.el"))
  (package-initialize))
