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
    (preview-mode-setup LaTeX-install-toolbar turn-on-reftex LaTeX-math-mode auto-fill-mode)))
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
 '(TeX-view-program-selection
   (quote
    (((output-dvi style-pstricks)
      "dvips and gv")
     (output-dvi "xdvi")
     (output-pdf "PDF Tools")
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
 '(anzu-search-threshold 500)
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
 '(cscope-display-cscope-buffer nil)
 '(css-electric-brace-behavior t)
 '(css-electric-semi-behavior t)
 '(cua-auto-tabify-rectangles nil)
 '(cua-enable-cua-keys nil)
 '(cua-enable-cursor-indications t)
 '(cua-mode t nil (cua-base))
 '(cua-rectangle-mark-key [ignore])
 '(cursor-type (quote bar))
 '(custom-enabled-themes (quote (tango-dark-adapted)))
 '(custom-safe-themes
   (quote
    ("6869717b2ebe98c6f15a03431b8bf9b52916272cb3862bd448a3ae77032f9837" "07b8a3d6bb985a266458adff96d64644a2672424b4fa307d980790762e916493" default)))
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
 '(elpy-rpc-python-command "python3")
 '(elpy-rpc-timeout 2)
 '(eval-expression-print-level nil)
 '(expand-region-fast-keys-enabled nil)
 '(fancy-narrow-lighter "")
 '(fancy-narrow-mode t)
 '(fci-rule-color "#444444")
 '(ffap-machine-p-known (quote reject))
 '(ffap-newfile-prompt t)
 '(ffap-require-prefix t)
 '(fill-column 80)
 '(flx-ido-mode nil)
 '(flycheck-check-syntax-automatically (quote (save idle-change mode-enabled)))
 '(flycheck-completion-system (quote ido))
 '(flycheck-disabled-checkers (quote (python-pylint rst-sphinx)))
 '(flycheck-idle-change-delay 2)
 '(flycheck-indication-mode (quote left-fringe))
 '(flycheck-navigation-minimum-level (quote warning))
 '(flycheck-python-flake8-executable "python3")
 '(flycheck-rust-cargo-executable "~/.cargo/bin/cargo")
 '(flymake-no-changes-timeout 1.5)
 '(flyspell-default-dictionary "german")
 '(flyspell-use-meta-tab nil)
 '(folding-allow-overlays t)
 '(font-latex-match-sectioning-0-keywords (quote (("addpart" "") ("addpart*" ""))))
 '(font-latex-match-sectioning-1-keywords (quote (("addchap" "*[{"))))
 '(font-latex-match-sectioning-2-keywords (quote (("addsec" "") ("addsec*" ""))))
 '(font-latex-quotes (quote auto))
 '(fringe-mode (quote (nil . 0)) nil (fringe))
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
 '(hl-paren-colors
   (quote
    ("SpringGreen3" "IndianRed1" "IndianRed3" "IndianRed4")))
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
    ("\\` " "Completions\\*" "\\*elpy-rpc" "TAGS\\(\\'\\|\\\\.*\\)" "-preprocessed\\*")))
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
 '(isearch-project-ignore-paths (quote (".vs/" ".vscode/" "node_modules/" "target/")))
 '(ispell-alternate-dictionary "/usr/lib/ispell/german.hash")
 '(ispell-complete-word-dict "/usr/lib/ispell/german.hash")
 '(ispell-extra-args (quote ("-W2")))
 '(ispell-highlight-face (quote flyspell-incorrect))
 '(ispell-local-dictionary-alist nil)
 '(ispell-program-name "aspell")
 '(ivy-action-wrap t)
 '(ivy-use-virtual-buffers t)
 '(javascript-indent-level 2)
 '(js-indent-level 4)
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
 '(mc/edit-lines-empty-lines (quote pad))
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
    (xcscope rust-auto-use flycheck-lilypond verilog-mode csharp-mode docker-compose-mode dockerfile-mode goto-char-preview isearch-project rmsbolt dynamic-spaces ialign winnow auto-minor-mode json-navigator groovy-mode projectile-ripgrep 0xc f digit-groups cargo plur sr-speedbar fullframe kurecolor adaptive-wrap easy-repeat shell-pop autopair disaster fill-column-indicator highlight-parentheses which-key zeal-at-point nasm-mode x86-lookup pdf-tools unicode-troll-stopper toggle-quotes jinja2-mode change-inner racer toml-mode protobuf-mode flycheck-rust rust-mode idomenu highlight-indentation web-mode company pos-tip pyvenv irony projectile realgud paradox discover yaml-mode wgrep volatile-highlights vlf virtualenv undo-tree smooth-scrolling smex session rainbow-mode pretty-mode powerline persp-projectile page-break-lines nose markdown-mode magit-gh-pulls magit-gerrit lua-mode keywiz js2-mode ido-ubiquitous ibuffer-projectile highlight-symbol highlight-escape-sequences highlight hideshowvis haskell-mode guru-mode goto-chg github-browse-file git-timemachine git-messenger full-ack flycheck-irony flx-ido fastnav fancy-narrow expand-region elpy easy-kill diminish diff-hl d-mode cython-mode company-quickhelp company-irony c-eldoc browse-kill-ring bm auctex anzu ag ack-and-a-half ace-window multiple-cursors winpoint try tagedit popwin ace-jump-mode)))
 '(page-break-lines-char 8213)
 '(paradox-execute-asynchronously nil)
 '(paradox-github-token t)
 '(password-cache-expiry 3600)
 '(paste-kill-url t)
 '(paste-show-in-browser nil)
 '(path-headerline-mode t nil (path-headerline-mode))
 '(po-highlight-face (quote pesche-hardspace))
 '(pop-up-windows nil)
 '(powerline-default-separator (quote slant))
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
 '(projectile-mode t nil (projectile))
 '(projectile-mode-line (quote (:eval (format " P[%s]" (projectile-project-name)))))
 '(projectile-mode-line-prefix "P")
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
 '(racer-cmd "/usr/bin/racer")
 '(racer-rust-src-path
   "~/.rustup/toolchains/nightly-x86_64-unknown-linux-gnu/lib/rustlib/src/rust/library/")
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
 '(scroll-margin 4)
 '(search-default-mode (quote character-fold-to-regexp))
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
 '(shell-pop-shell-type
   (quote
    ("shell" "*shell*"
     (lambda nil
       (ansi-term "/bin/zsh")))))
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
 '(test-case-ask-about-save nil)
 '(tex-close-quote "\"'")
 '(tex-open-quote "\"`")
 '(tool-bar-mode nil)
 '(tooltip-delay 1)
 '(tooltip-short-delay 0.5)
 '(tramp-debug-buffer nil)
 '(tramp-persistency-file-name "/home/gbr/.emacs.d/saved/tramp")
 '(tramp-syntax (quote default) nil (tramp))
 '(tramp-verbose 5 nil (tramp))
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
 '(undo-tree-visualizer-diff t)
 '(undo-tree-visualizer-timestamps t)
 '(uniquify-buffer-name-style (quote post-forward) nil (uniquify))
 '(uniquify-ignore-buffers-re "^\\*")
 '(uniquify-trailing-separator-p t)
 '(url-configuration-directory "~/.emacs.d/saved/url/")
 '(url-show-status nil)
 '(use-dialog-box nil)
 '(vc-delete-logbuf-window nil)
 '(vc-git-grep-template
   "git --no-pager grep --recurse-submodules -n <C> -e <R> -- <F>")
 '(wdired-allow-to-change-permissions t)
 '(web-mode-markup-indent-offset 2)
 '(wgrep-auto-save-buffer t)
 '(which-key-idle-delay 0.75)
 '(which-key-mode t)
 '(which-key-separator " > ")
 '(windmove-wrap-around t)
 '(x86-lookup-pdf "/home/gbr/x86.pdf")
 '(xhtml-multi-mode t))


(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(default ((t (:inherit nil :extend nil :stipple nil :background "#2e3436" :foreground "#eeeeec" :inverse-video nil :box nil :strike-through nil :overline nil :underline nil :slant normal :weight normal :height 110 :width normal :family "Input"))))
 '(ack-file ((((background light)) (:inherit compilation-info :underline t))))
 '(ack-line ((nil (:inherit compilation-line-number :underline t))))
 '(ack-match ((nil (:inherit match))))
 '(custom-button ((t (:inherit variable-pitch :background "lightgrey" :foreground "black" :box (:line-width 2 :style released-button)))))
 '(custom-button-pressed ((t (:inherit custom-button :background "lightgrey" :foreground "black" :box (:line-width 2 :style pressed-button)))))
 '(diff-file-header ((t (:weight bold))))
 '(eldoc-highlight-function-argument ((t (:inherit ido-first-match))))
 '(fixed-pitch ((t nil)))
 '(flx-highlight-face ((t nil)))
 '(font-latex-sectioning-2-face ((t (:inherit font-latex-sectioning-3-face :height 1.2))))
 '(hexl-address-region ((t (:inherit font-lock-constant-face))))
 '(hexl-ascii-region ((t (:inherit font-lock-string-face))))
 '(hideshowvis-hidable-face ((t (:foreground "#999"))))
 '(highlight-indentation-face ((t nil)))
 '(hs-face ((t (:box 1 :height 0.8))))
 '(hs-fringe-face ((t (:foreground "#999"))))
 '(nxml-comment-content-face ((t (:inherit font-lock-comment-face))) t)
 '(page-break-lines ((t (:inherit font-lock-doc-face :slant normal :weight normal))))
 '(realgud-backtrace-number ((t (:inherit font-lock-keyword-face))))
 '(realgud-overlay-arrow1 ((t (:inherit font-lock-function-name-face))))
 '(rust-string-interpolation-face ((t (:inherit font-lock-string-face :slant italic :weight normal))))
 '(show-ws-spaces ((((class color)) nil)) t)
 '(show-ws-tabs ((((class color)) (:inherit trailing-whitespace))) t)
 '(show-ws-unbr-spaces ((((class color)) nil)) t)
 '(tabbar-default ((t (:inherit variable-pitch :height 0.9))))
 '(tabbar-selected ((t (:weight bold))))
 '(test-case-assertion ((t (:inherit font-lock-keyword-face))))
 '(test-case-result-line ((t (:inherit font-lock-warning-face))))
 '(variable-pitch ((t (:inherit default :family "Droid Sans"))))
 '(web-mode-html-attr-name-face ((t (:inherit font-lock-preprocessor-face))))
 '(web-mode-html-tag-face ((t (:inherit font-lock-function-name-face))))
 '(which-key-command-description-face ((t (:inherit default))))
 '(which-key-key-face ((t (:inherit font-lock-type-face :weight normal))))
 '(whitespace-empty ((t (:underline (:color foreground-color :style wave)))))
 '(whitespace-line ((t (:underline (:color foreground-color :style wave))))))
