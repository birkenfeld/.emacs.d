;; ---------- Custom: managed by Emacs -----------------------------------------

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(LaTeX-babel-hyphen "")
 '(LaTeX-command "latex --enable-write18")
 '(LaTeX-fill-break-at-separators (quote (\\\( \\\) \\\[ \\\])))
 '(LaTeX-menu-max-items 40)
 '(LaTeX-mode-hook
   (quote
    (preview-mode-setup talcum-mode LaTeX-install-toolbar turn-on-reftex LaTeX-math-mode auto-fill-mode)) t)
 '(LaTeX-verbatim-environments
   (quote
    ("verbatim" "verbatim*" "alltt" "listing" "asy" "asydef")))
 '(LaTeX-verbatim-regexp "verbatim\\*?\\|alltt\\|listing")
 '(TeX-PDF-mode t)
 '(TeX-auto-local ".auto/")
 '(TeX-close-quote "\"'")
 '(TeX-command-list
   (quote
    (("LaTeX" "%`%l%(mode)%' %t" TeX-run-TeX nil
      (latex-mode doctex-mode)
      :help "Run LaTeX")
     ("BibTeX" "bibtex %s" TeX-run-BibTeX nil t :help "Run BibTeX")
     ("View" "%V" TeX-run-discard-or-function t t :help "Run Viewer")
     ("Print" "%p" TeX-run-command t t :help "Print the file")
     ("File" "%(o?)dvips %d -o %f " TeX-run-command t t :help "Generate PostScript file")
     ("Index" "makeindex %s" TeX-run-command nil t :help "Create index file")
     ("Spell" "(TeX-ispell-document \"\")" TeX-run-function nil t :help "Spell-check the document")
     ("Clean" "TeX-clean" TeX-run-function nil t :help "Delete generated intermediate files")
     ("Clean All" "(TeX-clean t)" TeX-run-function nil t :help "Delete generated intermediate and output files")
     ("Other" "" TeX-run-command t t :help "Run an arbitrary command")
     ("Latexmk" "latexmk -latexoption=\"--interaction=nonstopmode\" -pdf %t" TeX-run-command nil t))))
 '(TeX-insert-braces nil)
 '(TeX-open-quote "\"`")
 '(TeX-parse-self t)
 '(TeX-source-correlate-method (quote synctex))
 '(TeX-source-correlate-mode t)
 '(TeX-view-program-list
   (quote
    (("Evince"
      ("evince"
       (mode-io-correlate " -p %(outpage)")
       " %o")))))
 '(TeX-view-program-selection
   (quote
    (((output-dvi style-pstricks)
      "dvips and gv")
     (output-dvi "xdvi")
     (output-pdf "xpdf")
     (output-html "xdg-open"))))
 '(ac-auto-start 3)
 '(ac-candidate-limit 20)
 '(ac-comphist-file "~/.emacs.d/saved/ac-comphist")
 '(ac-delay 0.3)
 '(ac-quick-help-delay 0.8)
 '(ac-trigger-key "TAB")
 '(ac-use-fuzzy nil)
 '(ack-and-a-half-use-ido t)
 '(ack-context 0)
 '(ack-heading t)
 '(ag-highlight-search t)
 '(ansi-color-for-comint-mode t)
 '(anzu-mode-lighter "")
 '(anzu-replace-to-string-separator "=>")
 '(apropos-do-all t)
 '(auto-completion-min-chars 0)
 '(auto-completion-syntax-alist (quote (accept . word)))
 '(auto-insert-mode t)
 '(auto-save-list-file-prefix "~/.emacs.d/saved/autosave/saves-")
 '(bib-cite-use-reftex-view-crossref t)
 '(bm-repository-file "~/.emacs.d/saved/bm-repository")
 '(bookmark-default-file "~/.emacs.d/saved/bookmarks")
 '(browse-kill-ring-quit-action (quote save-and-restore))
 '(browse-url-browser-function (quote browse-url-firefox))
 '(browse-url-mozilla-program "firefox")
 '(c-default-style
   (quote
    ((java-mode . "java")
     (awk-mode . "awk")
     (other . "python"))))
 '(c-electric-pound-behavior (quote (alignleft)))
 '(calendar-week-start-day 1)
 '(column-number-mode t)
 '(comint-input-autoexpand (quote history))
 '(comint-move-point-for-output (quote this))
 '(comint-prompt-read-only t)
 '(comint-scroll-to-bottom-on-input (quote this))
 '(comment-line-break-function (quote comment-indent-new-line) t)
 '(comment-style (quote multi-line))
 '(company-backends
   (quote
    (company-nxml company-css company-clang company-capf
                  (company-gtags company-etags company-keywords sane-company-dabbrev)
                  company-oddmuse company-files)))
 '(company-idle-delay 1)
 '(company-quickhelp-delay 0.25)
 '(company-quickhelp-mode t)
 '(company-require-match nil)
 '(company-show-numbers t)
 '(company-tooltip-align-annotations t)
 '(company-tooltip-minimum-width 40)
 '(compilation-ask-about-save nil)
 '(compilation-scroll-output (quote first-error))
 '(completion-auto-show (quote tooltip))
 '(completion-auto-show-delay 0)
 '(completion-auto-show-menu t)
 '(completion-resolve-behaviour (quote reject))
 '(completion-use-echo nil)
 '(copyright-names-regexp "Georg Brandl")
 '(css-electric-brace-behavior t)
 '(css-electric-semi-behavior t)
 '(cua-auto-tabify-rectangles nil)
 '(cua-enable-cua-keys nil)
 '(cua-enable-cursor-indications t)
 '(cua-mode t nil (cua-base))
 '(cursor-type (quote bar))
 '(custom-enabled-themes (quote (tango-dark-adapted)))
 '(custom-safe-themes
   (quote
    ("62ebee71b2fb472aa451e697433472f0a418a4209dbe86fb3527d20d29c3b26c" "51e40cb4ab781b28436f51bb8695ba3443c557b1a7f2ce7f4df10f4426781db2" "de3d74ab9f2b9b8e2cd1d9147aedd328a46d915ae0aad46f7d78e160e885e6c7" "e69004ef51b9e975f7eb8878e8bd0199f6d8025bfaabd6d386fac991fe84c082" default)))
 '(custom-theme-directory "~/.emacs.d/themes")
 '(delete-by-moving-to-trash t)
 '(delete-selection-mode t)
 '(desktop-base-file-name "desktop")
 '(desktop-base-lock-name "desktop.lock")
 '(desktop-file-name-format (quote tilde))
 '(desktop-load-locked-desktop t)
 '(desktop-modes-not-to-save (quote (tags-table-mode grep-mode ack-mode)))
 '(desktop-path (quote ("~/.emacs.d/saved")))
 '(desktop-restore-eager 5)
 '(desktop-save t)
 '(desktop-save-hook (quote (remove-powerline-cache)))
 '(desktop-save-mode t)
 '(diff-hl-draw-borders nil)
 '(diff-hl-fringe-bmp-function (quote diff-hl-fringe-bmp-from-pos))
 '(diff-switches "-u")
 '(dired-dwim-target t)
 '(display-buffer-alist (quote ((".*" display-buffer-reuse-window))))
 '(display-buffer-reuse-frames t)
 '(display-time-24hr-format t)
 '(display-time-day-and-date t)
 '(dnd-open-file-other-window t)
 '(ecb-directories-menu-user-extension-function (quote ignore))
 '(ecb-history-menu-user-extension-function (quote ignore))
 '(ecb-layout-name "left14")
 '(ecb-methods-menu-user-extension-function (quote ignore))
 '(ecb-mode-line-display-window-number nil)
 '(ecb-options-version "2.50")
 '(ecb-primary-secondary-mouse-buttons (quote mouse-1--mouse-2))
 '(ecb-source-file-regexps
   (quote
    ((".*"
      ("\\(^\\(\\.\\|#\\)\\|\\(~$\\|\\.\\(elc\\|obj\\|o\\|class\\|lib\\|dll\\|a\\|so\\|cache\\|pyc\\)$\\)\\)")
      ("^\\.\\(emacs\\|gnus\\)$")))))
 '(ecb-sources-menu-user-extension-function (quote ignore))
 '(ecb-tip-of-the-day nil)
 '(ecb-tree-buffer-style (quote image))
 '(echo-keystrokes 0.2)
 '(ede-project-placeholder-cache-file "~/.emacs.d/saved/projects.ede")
 '(ediff-split-window-function (quote split-window-horizontally))
 '(ediff-window-setup-function (quote ediff-setup-windows-plain))
 '(eldoc-idle-delay 0.1)
 '(eldoc-print-after-edit t)
 '(elpy-project-ignored-directories
   (quote
    (".bzr" "CVS" ".git" ".hg" ".svn" ".tox" "build" "dist" ".cask" "_build" "html")))
 '(elpy-rpc-backend "jedi")
 '(elpy-rpc-timeout 2)
 '(eval-expression-print-level nil)
 '(expand-region-fast-keys-enabled nil)
 '(fancy-narrow-lighter "")
 '(fancy-narrow-mode t)
 '(ffap-machine-p-known (quote reject))
 '(ffap-newfile-prompt t)
 '(fill-column 80)
 '(filladapt-mode-line-string "")
 '(flx-ido-mode nil)
 '(flycheck-check-syntax-automatically (quote (save idle-change mode-enabled)))
 '(flycheck-completion-system (quote ido))
 '(flycheck-disabled-checkers (quote (python-pylint rst-sphinx)))
 '(flycheck-idle-change-delay 2)
 '(flycheck-indication-mode (quote left-fringe))
 '(flycheck-navigation-minimum-level (quote warning))
 '(flymake-no-changes-timeout 1.5)
 '(flyspell-default-dictionary "german")
 '(flyspell-use-meta-tab nil)
 '(folding-allow-overlays t)
 '(font-latex-match-sectioning-0-keywords (quote (("addpart" "") ("addpart*" ""))))
 '(font-latex-match-sectioning-1-keywords (quote (("addchap" "*[{"))))
 '(font-latex-match-sectioning-2-keywords (quote (("addsec" "") ("addsec*" ""))))
 '(font-latex-quotes (quote auto))
 '(fringe-mode (quote (nil . 0)) nil (fringe))
 '(gc-cons-threshold 20000000)
 '(gdb-many-windows t)
 '(global-anzu-mode t)
 '(global-diff-hl-mode t)
 '(global-font-lock-mode t nil (font-core))
 '(global-hl-line-mode t)
 '(global-undo-tree-mode t)
 '(grep-files-aliases
   (quote
    (("asm" . "*.[sS]")
     ("c" . "*.c")
     ("cc" . "*.cc")
     ("ch" . "*.[ch]")
     ("el" . "*.el")
     ("h" . "*.h")
     ("l" . "[Cc]hange[Ll]og*")
     ("m" . "[Mm]akefile*")
     ("tex" . "*.tex")
     ("texi" . "*.texi")
     (rst . "*.rst")
     (py . "*.py"))))
 '(grep-find-command
   "find . \\( -name .hg -o -name .git \\) -prune -o -type f -print0 | xargs -0 -e grep -nHE -e ")
 '(grep-highlight-matches t)
 '(gud-pdb-command-name "python -mpdb")
 '(gud-tooltip-mode t)
 '(haskell-doc-show-global-types t)
 '(haskell-indent-after-keywords
   (quote
    (("where" 2 0)
     ("of" 2)
     ("do" 2)
     ("in" 2 0)
     ("{" 2)
     ("if" 2)
     "then" "else" "let")))
 '(haskell-indent-offset 2)
 '(haskell-program-name "ghci")
 '(hi-lock-auto-select-face t)
 '(highlight-symbol-idle-delay 0.5)
 '(history-delete-duplicates t)
 '(ibuffer-expert t)
 '(icomplete-mode nil)
 '(ido-auto-merge-work-directories-length -1)
 '(ido-create-new-buffer (quote always))
 '(ido-decorations
   (quote
    ("{" "}" ", " ", ..." " [" "]" " [No match]" " [Matched]" " [Not readable]" " [Too big]" " [Confirm]")))
 '(ido-enable-flex-matching t)
 '(ido-ignore-buffers
   (quote
    ("\\` " "Completions\\*" "\\*elpy-rpc" "TAGS\\(\\'\\|\\\\.*\\)")))
 '(ido-mode (quote both) nil (ido))
 '(ido-save-directory-list-file "/home/gbr/.emacs.d/saved/ido.last")
 '(ido-ubiquitous-mode t)
 '(ido-use-filename-at-point (quote guess))
 '(ido-use-virtual-buffers t)
 '(iedit-occurrence-face (quote grep-edit-face))
 '(igrep-options (quote -i) t)
 '(indent-tabs-mode nil)
 '(indicate-empty-lines t)
 '(inferior-haskell-wait-and-jump t)
 '(inhibit-startup-echo-area-message nil)
 '(initial-buffer-choice t)
 '(initial-scratch-message ";; Scratch buffer
")
 '(isearch-allow-scroll t)
 '(ispell-alternate-dictionary "/usr/lib/ispell/german.hash")
 '(ispell-complete-word-dict "/usr/lib/ispell/german.hash")
 '(ispell-extra-args (quote ("-W2")))
 '(ispell-highlight-face (quote flyspell-incorrect))
 '(ispell-local-dictionary-alist nil)
 '(ispell-program-name "aspell")
 '(javascript-indent-level 2)
 '(js-indent-level 2)
 '(js2-allow-rhino-new-expr-initializer nil)
 '(js2-basic-offset 2)
 '(js2-enter-indents-newline nil)
 '(js2-highlight-level 3)
 '(js2-mirror-mode t)
 '(kill-do-not-save-duplicates t)
 '(kill-whole-line t)
 '(lazy-highlight-cleanup nil)
 '(list-directory-brief-switches "-1")
 '(magit-auto-revert-mode-lighter "")
 '(make-backup-file-name-function nil)
 '(make-backup-files nil)
 '(make-cursor-line-fully-visible t)
 '(margin-column 80)
 '(mark-even-if-inactive t)
 '(menu-bar-mode nil)
 '(minibuffer-electric-default-mode t)
 '(minibuffer-prompt-properties
   (quote
    (read-only t point-entered minibuffer-avoid-prompt face minibuffer-prompt)))
 '(mouse-yank-at-point t)
 '(mumamo-chunk-coloring (quote submode-colored))
 '(mumamo-set-major-mode-delay 0.3)
 '(neo-dont-be-alone t)
 '(neo-keymap-style (quote concise))
 '(neo-smart-open t)
 '(next-screen-context-lines 5)
 '(normal-erase-is-backspace (quote maybe))
 '(nxhtml-skip-welcome t)
 '(nxml-sexp-element-flag t)
 '(nxml-slash-auto-complete-flag t)
 '(outline-blank-line t t)
 '(package-selected-packages
   (quote
    (toggle-quotes jedi-direx jinja2-mode guide-key change-inner company-racer racer toml-mode protobuf-mode flycheck-rust rust-mode idomenu dash flycheck highlight-indentation web-mode company pos-tip pyvenv loc-changes irony load-relative projectile realgud paradox discover yaml-mode wgrep volatile-highlights vlf virtualenv undo-tree test-case-mode smooth-scrolling smex smart-operator session redo+ rainbow-mode pretty-mode powerline persp-projectile page-break-lines nose nav-flash monky markdown-mode magit-gh-pulls magit-gerrit lua-mode keywiz js2-mode ido-ubiquitous ibuffer-projectile highlight-symbol highlight-escape-sequences highlight hideshowvis haste haskell-mode guru-mode goto-chg github-browse-file git-timemachine git-messenger fullframe full-ack flycheck-irony flx-ido fastnav fancy-narrow expand-region elpy easy-kill diminish diff-hl d-mode cython-mode company-quickhelp company-irony c-eldoc browse-kill-ring bm autopair auctex anzu ag ack-and-a-half ace-window multiple-cursors winpoint try tagedit popwin ace-jump-mode)))
 '(page-break-lines-char 8213)
 '(paradox-execute-asynchronously nil)
 '(paradox-github-token t)
 '(password-cache-expiry 3600)
 '(paste-kill-url t)
 '(paste-show-in-browser nil)
 '(path-headerline-mode t nil (path-headerline-mode))
 '(po-highlight-face (quote pesche-hardspace))
 '(pop-up-windows nil)
 '(preview-auto-cache-preamble t)
 '(preview-default-document-pt 12)
 '(preview-scale-function (quote preview-scale-from-face) t)
 '(preview-transparent-color t)
 '(projectile-cache-file "/home/gbr/.emacs.d/saved/projectile.cache")
 '(projectile-global-mode t)
 '(projectile-globally-ignored-directories
   (quote
    (".idea" ".eunit" ".git" ".hg" ".fslckout" ".bzr" "_darcs" ".tox" ".svn" "build" "_build")))
 '(projectile-globally-ignored-files (quote ("TAGS" ".tags")))
 '(projectile-idle-timer-hook nil)
 '(projectile-known-projects-file "/home/gbr/.emacs.d/saved/projectile-bookmarks.eld")
 '(projectile-mode-line (quote (:eval (format " P[%s]" (projectile-project-name)))))
 '(projectile-mode-line-lighter "P")
 '(projectile-sort-order (quote recently-active))
 '(projectile-tags-command
   "ctags --languages=-HTML,JavaScript --python-kinds=-iv -Re %s")
 '(projectile-use-git-grep t)
 '(pulse-flag nil)
 '(python-fill-docstring-style (quote pep-257-nn))
 '(python-indent-guess-indent-offset nil)
 '(python-indent-trigger-commands (quote (indent-for-tab-command yas-expand)))
 '(python-skeleton-autoinsert nil)
 '(python-use-skeletons nil)
 '(racer-cmd "/home/gbr/devel/racer/target/release/racer")
 '(racer-rust-src-path "~/devel/ext/rust/src")
 '(recentf-max-saved-items 200)
 '(recentf-menu-open-all-flag t)
 '(recentf-mode t)
 '(recentf-save-file "~/.emacs.d/saved/recentf")
 '(reftex-enable-partial-scans t)
 '(reftex-include-file-commands (quote ("include" "input" "includedoc")))
 '(reftex-plug-into-AUCTeX t)
 '(reftex-save-parse-info t)
 '(reftex-use-multiple-selection-buffers t)
 '(reftex-vref-is-default t)
 '(require-final-newline t)
 '(rng-nxml-auto-validate-flag nil)
 '(rst-definition-face (quote font-lock-function-name-face))
 '(rst-directive-face (quote font-lock-builtin-face))
 '(rst-level-face-base-color "grey")
 '(rst-level-face-base-light 85)
 '(rst-level-face-step-light -7)
 '(rst-mode-lazy nil)
 '(rust-blink-matching-angle-brackets nil)
 '(rust-indent-method-chain t)
 '(save-abbrevs (quote silently))
 '(save-interprogram-paste-before-kill t)
 '(save-place-file "~/.emacs.d/saved/places")
 '(save-place-mode t nil (saveplace))
 '(screen-lines-minor-mode-string " \\/")
 '(scroll-bar-mode (quote right))
 '(scroll-conservatively 0)
 '(search-ring-update t)
 '(search-upper-case t)
 '(select-active-regions t)
 '(select-enable-primary t)
 '(semantic-idle-scheduler-idle-time 200)
 '(semantic-imenu-bucketize-file nil)
 '(semantic-imenu-summary-function (quote semantic-format-tag-name-short))
 '(semantic-tag-folding-highlight-tags-shown-by-reveal-mode t)
 '(semantic-tag-folding-show-tooltips t)
 '(semanticdb-default-file-name ".semantic.cache")
 '(semanticdb-default-save-directory "~/.emacs.d/saved/semantic")
 '(session-save-file "~/.emacs.d/saved/session")
 '(session-use-package t nil (session))
 '(show-paren-mode t)
 '(show-ws-style (quote color))
 '(size-indication-mode t)
 '(smerge-auto-leave nil)
 '(smex-history-length 50)
 '(smex-save-file "~/.emacs.d/saved/smex-items")
 '(snippet-bound-face (quote font-latex-italic-face))
 '(speedbar-directory-button-trim-method (quote trim))
 '(speedbar-hide-button-brackets-flag nil)
 '(speedbar-ignored-modes (quote (fundamental-mode custom-mode)))
 '(speedbar-indentation-width 2)
 '(speedbar-show-unknown-files t)
 '(speedbar-update-flag nil)
 '(speedbar-use-images t)
 '(split-window-preferred-function (quote split-window-preferred-horizontally))
 '(sr-speedbar-auto-refresh t)
 '(sr-speedbar-right-side nil)
 '(sr-speedbar-skip-other-window-p t)
 '(sr-speedbar-width-x 40)
 '(tabbar-background-color "gray90")
 '(tabbar-cycle-scope (quote tabs))
 '(tabbar-separator (quote (1)))
 '(table-time-before-update 0)
 '(talcum-desired-features (quote (talcum-render talcum-newcmd)))
 '(talcum-use-prod-flag nil)
 '(talcum-verbosity 7)
 '(test-case-ask-about-save nil)
 '(tex-close-quote "\"'")
 '(tex-open-quote "\"`")
 '(tool-bar-mode nil)
 '(tooltip-delay 1)
 '(tooltip-short-delay 0.5)
 '(tramp-debug-buffer nil)
 '(tramp-persistency-file-name "/home/gbr/.emacs.d/saved/tramp")
 '(tramp-verbose 5)
 '(transient-mark-mode 1)
 '(trex-unicode-mappings
   (quote
    (("forall" . 8704)
     ("complement" . 8705)
     ("partial" . 8706)
     ("exists" . 8707)
     ("emptyset" . 8709)
     ("nabla" . 8711)
     ("in" . 8712)
     ("notin" . 8713)
     ("ni" . 8715)
     ("qedhere" . 8718)
     ("prod" . 8719)
     ("coprod" . 8720)
     ("sum" . 8721)
     ("mp" . 8723)
     ("setminus" . 8726)
     ("circ" . 8728)
     ("cdot" . 8729)
     ("sqrt" . 8730)
     ("infty" . 8734)
     ("land" . 8743)
     ("wedge" . 8743)
     ("lor" . 8744)
     ("vee" . 8744)
     ("cap" . 8745)
     ("cup" . 8746)
     ("int" . 8747)
     ("iint" . 8748)
     ("iiiint" . 8749)
     ("neq" . 8800)
     ("ne" . 8800)
     ("leq" . 8804)
     ("le" . 8804)
     ("geq" . 8805)
     ("ge" . 8805)
     ("prec" . 8826)
     ("succ" . 8827)
     ("subset" . 8834)
     ("supset" . 8835)
     ("subseteq" . 8838)
     ("supseteq" . 8839)
     ("subsetneq" . 8842)
     ("supsetneq" . 8843)
     ("unlhd" . 8884)
     ("lhd" . 8882)
     ("unrhd" . 8885)
     ("rhd" . 8883)
     ("implies" . 10233)
     ("iff" . 10234)
     ("mapsto" . 10236)
     ("to" . 10230)
     ("longleftarrow" . 10229)
     ("longrightarrow" . 10230)
     ("longleftrightarrow" . 10231)
     ("Longleftarrow" . 10232)
     ("Longrightarrow" . 10233)
     ("leftarrow" . 8592)
     ("uparrow" . 8593)
     ("rightarrow" . 8594)
     ("downarrow" . 8595)
     ("leftrightarrow" . 8596)
     ("updownarrow" . 8597)
     ("dots" . 8230)
     ("ldots" . 8230)
     ("textperthousand" . 8240)
     ("bigodot" . 10752)
     ("bigoplus" . 10753)
     ("bigotimes" . 10754)
     ("lneq" . 10887)
     ("gneq" . 10888)
     ("wp" . 8472)
     ("ell" . 8467)
     ("Im" . 8465)
     ("Re" . 8476)
     ("Finv" . 8498)
     ("Game" . 8513)
     ("aleph" . 8501)
     ("beth" . 8502)
     ("gimel" . 8503)
     ("daleth" . 8504)
     ("alpha" . 945)
     ("beta" . 946)
     ("gamma" . 947)
     ("delta" . 948)
     ("epsilon" . 1013)
     ("varepsilon" . 949)
     ("zeta" . 950)
     ("eta" . 951)
     ("theta" . 952)
     ("vartheta" . 977)
     ("iota" . 953)
     ("kappa" . 954)
     ("varkappa" . 1008)
     ("lambda" . 955)
     ("mu" . 956)
     ("nu" . 957)
     ("xi" . 958)
     ("pi" . 960)
     ("varpi" . 982)
     ("rho" . 961)
     ("varrho" . 1009)
     ("sigma" . 963)
     ("varsigma" . 962)
     ("tau" . 964)
     ("upsilon" . 965)
     ("varphi" . 966)
     ("phi" . 981)
     ("chi" . 967)
     ("psi" . 968)
     ("omega" . 969)
     ("digamma" . 989)
     ("Gamma" . 915)
     ("Delta" . 916)
     ("Theta" . 920)
     ("Lambda" . 923)
     ("Xi" . 926)
     ("Pi" . 928)
     ("Sigma" . 931)
     ("Upsilon" . 933)
     ("Phi" . 934)
     ("Psi" . 936)
     ("Omega" . 937)
     ("N" . 8469)
     ("R" . 8477)
     ("Q" . 8474)
     ("C" . 8450)
     ("Z" . 8484)
     ("pm" . 177)
     ("hbar" . 8463)
     ("AA" . 197)
     ("approx" . 8776)
     ("langle" . 12296)
     ("rangle" . 12297))))
 '(truncate-partial-width-windows nil)
 '(undo-limit 200000)
 '(undo-strong-limit 300000)
 '(undo-tree-mode-lighter " U")
 '(uniquify-buffer-name-style (quote reverse) nil (uniquify))
 '(url-configuration-directory "~/.emacs.d/saved/url/")
 '(url-show-status nil)
 '(vc-delete-logbuf-window nil)
 '(wdired-allow-to-change-permissions t)
 '(wgrep-auto-save-buffer t)
 '(windmove-wrap-around t)
 '(xhtml-multi-mode t))


(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(default ((t (:inherit nil :stipple nil :inverse-video nil :box nil :strike-through nil :overline nil :underline nil :slant normal :weight normal :height 105 :width normal :foundry "microsoft" :family "Consolas"))))
 '(ack-file ((((background light)) (:inherit compilation-info :underline t))))
 '(ack-line ((nil (:inherit compilation-line-number :underline t))))
 '(ack-match ((nil (:inherit match))))
 '(custom-button ((t (:inherit variable-pitch :background "lightgrey" :foreground "black" :box (:line-width 2 :style released-button)))))
 '(diff-file-header ((t (:weight bold))))
 '(diff-hl-change ((t (:background "blue2"))))
 '(diff-hl-delete ((t (:background "red4"))))
 '(diff-hl-insert ((t (:background "green4"))))
 '(ecb-default-general-face ((t (:inherit variable-pitch :height 1.0))))
 '(ecb-default-highlight-face ((t (:inherit highlight))))
 '(ecb-mode-line-data-face ((t (:inherit variable-pitch :background "#555753"))))
 '(ecb-tag-header-face ((t (:inherit highlight))))
 '(eldoc-highlight-function-argument ((t (:inherit ido-first-match))))
 '(flx-highlight-face ((t nil)))
 '(flymake-errline ((t (:underline (:color "red" :style wave)))))
 '(flyspell-duplicate ((t (:foreground "Gold3" :underline t))))
 '(flyspell-incorrect ((t (:foreground "OrangeRed" :underline t))))
 '(font-latex-sectioning-2-face ((t (:inherit font-latex-sectioning-3-face :height 1.2))))
 '(header-line ((t (:inherit variable-pitch :background "#41423f" :weight bold))))
 '(hideshowvis-hidable-face ((t (:foreground "#999"))))
 '(highlight-indentation-face ((t nil)))
 '(hs-face ((t (:box 1 :height 0.8))))
 '(hs-fringe-face ((t (:foreground "#999"))))
 '(keywiz-command-face ((t (:inherit (quote variable-pitch) :foreground "#729fcf" :weight bold :height 1.2))))
 '(keywiz-right-face ((t (:foreground "#8ae234"))))
 '(keywiz-wrong-face ((t (:foreground "#ff4b4b"))))
 '(monky-diff-add ((t (:inherit diff-added))))
 '(monky-diff-del ((t (:inherit diff-removed))))
 '(monky-diff-hunk-header ((t (:inherit diff-hunk-header))))
 '(monky-diff-title ((t (:inherit (diff-file-header vhl/default-face)))))
 '(monky-section-title ((t (:inherit monky-header :underline t :weight bold))))
 '(mumamo-background-chunk-major ((t nil)))
 '(mumamo-border-face-in ((t (:inherit font-lock-preprocessor-face))))
 '(mumamo-border-face-out ((t (:inherit mumamo-border-face-in))))
 '(nxml-comment-content-face ((t (:inherit font-lock-comment-face))) t)
 '(page-break-lines ((t (:inherit font-lock-doc-face :slant normal :weight normal))))
 '(py-XXX-tag-face ((t (:background "yellow" :foreground "#f00"))))
 '(py-builtins-face ((t (:inherit font-lock-keyword-face :weight normal))))
 '(py-class-name-face ((t (:inherit font-lock-type-face :underline t))))
 '(py-decorators-face ((t (:inherit font-lock-keyword-face :weight normal))))
 '(py-pseudo-keyword-face ((t (:inherit font-lock-keyword-face :weight normal))))
 '(realgud-backtrace-number ((t (:inherit font-lock-keyword-face))))
 '(realgud-overlay-arrow1 ((t (:inherit font-lock-function-name-face))))
 '(show-ws-spaces ((((class color)) nil)) t)
 '(show-ws-tabs ((((class color)) (:inherit trailing-whitespace))) t)
 '(show-ws-unbr-spaces ((((class color)) nil)) t)
 '(tabbar-default ((t (:inherit variable-pitch :height 0.9))))
 '(tabbar-selected ((t (:weight bold))))
 '(test-case-assertion ((t (:inherit font-lock-keyword-face))))
 '(test-case-mode-line-success ((t (:inherit mode-line-buffer-id :background "#00cc00" :foreground "black"))))
 '(test-case-result-line ((t (:inherit font-lock-warning-face))))
 '(test-case-result-message ((((background light)) (:inherit font-lock-function-name-face))))
 '(trex-unicode-face ((t nil)))
 '(variable-pitch ((t (:inherit default :family "calibri"))))
 '(web-mode-html-attr-name-face ((t (:inherit font-lock-preprocessor-face))))
 '(web-mode-html-tag-face ((t (:inherit font-lock-function-name-face))))
 '(whitespace-line ((t (:underline (:color foreground-color :style wave))))))
