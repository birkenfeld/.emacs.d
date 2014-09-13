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
 '(TeX-parse-self t t)
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
 '(ac-comphist-file "~/.emacs.d/comphist")
 '(ac-delay 0.3)
 '(ac-quick-help-delay 0.8)
 '(ac-trigger-key "TAB")
 '(ac-use-fuzzy nil)
 '(ack-context 0)
 '(ack-heading t)
 '(ansi-color-for-comint-mode t)
 '(apropos-do-all t)
 '(auto-completion-min-chars 0)
 '(auto-completion-syntax-alist (quote (accept . word)))
 '(auto-insert-mode t)
 '(auto-save-list-file-prefix "~/.emacs.d/autosave/saves-")
 '(bib-cite-use-reftex-view-crossref t)
 '(bm-repository-file "~/.emacs.d/bm-repository")
 '(browse-kill-ring-quit-action (quote save-and-restore))
 '(browse-url-browser-function (quote browse-url-firefox))
 '(browse-url-mozilla-program "firefox")
 '(c-default-style
   (quote
    ((java-mode . "java")
     (awk-mode . "awk")
     (other . "python"))))
 '(c-electric-pound-behavior (quote (alignleft)))
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
 '(company-tooltip-minimum-width 40)
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
 '(cua-enable-cua-keys (quote shift))
 '(cua-enable-cursor-indications t)
 '(cua-mode t nil (cua-base))
 '(cua-remap-control-z nil)
 '(cursor-type (quote bar))
 '(custom-enabled-themes (quote (tango-dark-adapted)))
 '(custom-safe-themes
   (quote
    ("251bfca65a18556b68b75d05128acb3b2c04d648cc69163676a8975bcf48f7c3" default)))
 '(delete-selection-mode t)
 '(desktop-base-file-name "desktop")
 '(desktop-base-lock-name "desktop.lock")
 '(desktop-file-name-format (quote tilde))
 '(desktop-load-locked-desktop t)
 '(desktop-modes-not-to-save (quote (tags-table-mode grep-mode ack-mode)))
 '(desktop-path (quote ("~/.emacs.d")))
 '(desktop-restore-eager 5)
 '(desktop-save t)
 '(desktop-save-hook (quote (remove-powerline-cache)))
 '(desktop-save-mode t)
 '(diff-switches "-u")
 '(display-buffer-alist (quote ((".*" display-buffer-reuse-window))))
 '(display-buffer-reuse-frames t)
 '(display-time-24hr-format t)
 '(display-time-day-and-date t)
 '(dnd-open-file-other-window t)
 '(ede-project-placeholder-cache-file "~/.emacs.d/projects.ede")
 '(ediff-split-window-function (quote split-window-horizontally))
 '(eldoc-idle-delay 0.1)
 '(eldoc-print-after-edit t)
 '(elpy-project-ignored-directories
   (quote
    (".bzr" "CVS" ".git" ".hg" ".svn" ".tox" "build" "dist" ".cask" "_build" "html")))
 '(elpy-rpc-backend "jedi")
 '(eproject-completing-read-function (quote eproject--ido-completing-read))
 '(ffap-machine-p-known (quote reject))
 '(ffap-newfile-prompt t)
 '(fill-column 80)
 '(filladapt-mode-line-string "")
 '(flymake-no-changes-timeout 1.5)
 '(flyspell-default-dictionary "german")
 '(flyspell-use-meta-tab nil)
 '(folding-allow-overlays t)
 '(font-latex-match-sectioning-0-keywords (quote (("addpart" "") ("addpart*" ""))))
 '(font-latex-match-sectioning-1-keywords (quote (("addchap" "*[{"))))
 '(font-latex-match-sectioning-2-keywords (quote (("addsec" "") ("addsec*" ""))))
 '(font-latex-quotes (quote auto))
 '(fringe-mode (quote (nil . 0)) nil (fringe))
 '(gc-cons-threshold 4000000)
 '(gdb-many-windows t)
 '(global-font-lock-mode t nil (font-core))
 '(global-hl-line-mode t)
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
   "find . -name .svn -prune -o -type f -print0 | xargs -0 -e grep -nHE -e ")
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
 '(icomplete-mode nil)
 '(ido-decorations
   (quote
    ("{" "}" ", " ", ..." " [" "]" " [No match]" " [Matched]" " [Not readable]" " [Too big]" " [Confirm]")))
 '(ido-enable-flex-matching t)
 '(ido-ignore-buffers (quote ("\\` " "Completions\\*" "\\*elpy-rpc")))
 '(ido-mode (quote both) nil (ido))
 '(ido-save-directory-list-file "/home/gbr/.emacs.d/ido.last")
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
 '(next-screen-context-lines 5)
 '(normal-erase-is-backspace (quote maybe))
 '(nxhtml-skip-welcome t)
 '(nxml-sexp-element-flag t)
 '(nxml-slash-auto-complete-flag t)
 '(outline-blank-line t t)
 '(paradox-github-token t)
 '(password-cache-expiry 3600)
 '(paste-kill-url t)
 '(paste-show-in-browser nil)
 '(po-highlight-face (quote pesche-hardspace))
 '(pop-up-windows nil)
 '(powerline-default-separator (quote arrow))
 '(powerline-height 23)
 '(preview-auto-cache-preamble t)
 '(preview-default-document-pt 12)
 '(preview-scale-function (quote preview-scale-from-face) t)
 '(preview-transparent-color t)
 '(pulse-flag nil)
 '(python-fill-docstring-style (quote pep-257-nn))
 '(python-indent-guess-indent-offset nil)
 '(python-indent-trigger-commands (quote (indent-for-tab-command yas-expand)))
 '(python-skeleton-autoinsert nil)
 '(python-use-skeletons nil)
 '(recentf-max-saved-items 200)
 '(recentf-menu-open-all-flag t)
 '(recentf-mode t)
 '(recentf-save-file "~/.emacs.d/recentf")
 '(reftex-enable-partial-scans t)
 '(reftex-include-file-commands (quote ("include" "input" "includedoc")))
 '(reftex-plug-into-AUCTeX t)
 '(reftex-save-parse-info t)
 '(reftex-use-multiple-selection-buffers t)
 '(reftex-vref-is-default t)
 '(rng-nxml-auto-validate-flag nil)
 '(rst-definition-face (quote font-lock-function-name-face))
 '(rst-directive-face (quote font-lock-builtin-face))
 '(rst-level-face-base-color "grey")
 '(rst-level-face-base-light 85)
 '(rst-level-face-step-light -7)
 '(rst-mode-lazy nil)
 '(save-abbrevs (quote silently))
 '(save-interprogram-paste-before-kill t)
 '(save-place t nil (saveplace))
 '(save-place-file "~/.emacs.d/places")
 '(screen-lines-minor-mode-string " \\/")
 '(scroll-bar-mode (quote right))
 '(scroll-conservatively 0)
 '(search-ring-update t)
 '(search-upper-case t)
 '(select-active-regions t)
 '(semantic-idle-scheduler-idle-time 200)
 '(semantic-imenu-bucketize-file nil)
 '(semantic-imenu-summary-function (quote semantic-format-tag-name-short))
 '(semantic-tag-folding-highlight-tags-shown-by-reveal-mode t)
 '(semantic-tag-folding-show-tooltips t)
 '(semanticdb-default-file-name ".semantic.cache")
 '(semanticdb-default-save-directory "~/.emacs.d/semantic")
 '(session-save-file "~/.emacs.d/session")
 '(show-paren-mode t)
 '(show-ws-style (quote color))
 '(size-indication-mode t)
 '(smex-history-length 50)
 '(smex-save-file "~/.emacs.d/smex-items")
 '(snippet-bound-face (quote font-latex-italic-face))
 '(speedbar-show-unknown-files t)
 '(speedbar-use-images t)
 '(split-window-preferred-function (quote split-window-preferred-horizontally))
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
 '(uniquify-buffer-name-style (quote reverse) nil (uniquify))
 '(url-configuration-directory "~/.emacs.d/url/")
 '(url-show-status nil)
 '(vc-delete-logbuf-window nil)
 '(vline-face (quote vline))
 '(wdired-allow-to-change-permissions t)
 '(wgrep-auto-save-buffer t)
 '(windmove-wrap-around t)
 '(x-select-enable-primary t)
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
 '(diff-file-header ((t (:weight bold))) t)
 '(eldoc-highlight-function-argument ((t (:inherit ido-first-match))))
 '(flymake-errline ((t (:underline (:color "red" :style wave)))))
 '(flyspell-duplicate ((t (:foreground "Gold3" :underline t))) t)
 '(flyspell-incorrect ((t (:foreground "OrangeRed" :underline t))) t)
 '(font-latex-sectioning-2-face ((t (:inherit font-latex-sectioning-3-face :height 1.2))))
 '(mumamo-background-chunk-major ((t nil)))
 '(mumamo-border-face-in ((t (:inherit font-lock-preprocessor-face))))
 '(mumamo-border-face-out ((t (:inherit mumamo-border-face-in))))
 '(nxml-comment-content-face ((t (:inherit font-lock-comment-face))) t)
 '(py-XXX-tag-face ((t (:background "yellow" :foreground "#f00"))))
 '(py-builtins-face ((t (:inherit font-lock-keyword-face :weight normal))))
 '(py-class-name-face ((t (:inherit font-lock-type-face :underline t))))
 '(py-decorators-face ((t (:inherit font-lock-keyword-face :weight normal))))
 '(py-pseudo-keyword-face ((t (:inherit font-lock-keyword-face :weight normal))))
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
 '(vline ((t (:inherit highlight))) t)
 '(web-mode-html-attr-name-face ((t (:inherit font-lock-preprocessor-face))))
 '(web-mode-html-tag-face ((t (:inherit font-lock-function-name-face))))
 '(whitespace-line ((t (:underline (:color foreground-color :style wave))))))
