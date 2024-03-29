(deftheme Georg
  "Created 2011-12-28.")

(custom-theme-set-variables
 'Georg
 '(ido-mode (quote both))
 '(icomplete-mode nil)
 '(global-font-lock-mode t)
 '(fringe-mode (quote (nil . 0)))
 '(display-time-mode t)
 '(delete-selection-mode nil)
 '(cua-mode t)
 '(column-number-mode t)
 '(tool-bar-mode nil))

(custom-theme-set-faces
 'Georg
 '(default ((t (:inherit nil :background "gray97" :foreground "#000000"))))
 '(ac-candidate-face ((t (:background "lightgray" :foreground "black" :underline "gray"))))
 '(ac-selection-face ((t (:inherit ac-candidate-face :background "steelblue" :foreground "white"))))
 '(button ((t (:foreground "#00b" :underline t))))
 '(diff-added ((t (:inherit diff-changed :background "#EEFFEE" :foreground "green4"))))
 '(diff-changed ((t (:background "grey95"))))
 '(diff-context ((t (:inherit shadow :foreground "#333333"))))
 '(diff-file-header ((t (:weight bold))))
 '(diff-header ((t (:foreground "#3333FF"))))
 '(diff-hunk-header ((t (:background "#eeeeee" :weight bold))))
 '(diff-indicator-added ((t (:inherit diff-added :weight bold))))
 '(diff-indicator-removed ((t (:inherit diff-removed :weight bold))))
 '(diff-refine-change ((t (:background "#ffa"))))
 '(diff-removed ((t (:inherit diff-changed :background "#FFEEEE" :foreground "red3"))))
 '(file-name-shadow ((t (:inherit shadow :foreground "grey80"))))
 '(font-latex-verbatim-face ((t (:foreground "SaddleBrown"))))
 '(font-lock-builtin-face ((t (:foreground "#c0c"))))
 '(font-lock-comment-delimiter-face ((t (:inherit font-lock-comment-face :slant normal))))
 '(font-lock-comment-face ((t (:foreground "#bd752f" :slant oblique))))
 '(font-lock-doc-face ((t (:foreground "#c6c" :slant italic))))
 '(font-lock-function-name-face ((t (:foreground "red3" :weight bold))))
 '(font-lock-keyword-face ((t (:foreground "#0000bb" :weight bold))))
 '(font-lock-negation-char-face ((t (:weight bold))))
 '(font-lock-preprocessor-face ((t (:inherit font-lock-builtin-face :foreground "gray50"))))
 '(font-lock-regexp-grouping-backslash ((t (:foreground "firebrick"))))
 '(font-lock-regexp-grouping-construct ((t (:inherit bold))))
 '(font-lock-string-face ((t (:foreground "#070"))))
 '(font-lock-type-face ((t (:foreground "#084"))))
 '(font-lock-warning-face ((((class color) (min-colors 88) (background light)) (:background "yellow" :foreground "Red1" :slant normal :weight extra-bold))))
 '(fringe ((((class color) (background light)) (:background "gray94" :foreground "#999"))))
 '(grep-edit-face ((t (:background "#77ff55"))))
 '(grep-edit-file-face ((t (:background "#77ff55" :weight bold))))
 '(header-line ((default (:inherit default)) (((class color grayscale) (background light)) (:background "grey90" :foreground "grey20" :box nil))))
 '(highlight ((((class color) (min-colors 88) (background light)) (:background "#FAFABF"))))
 '(ido-first-match ((t (:inherit font-lock-function-name-face))))
 '(ido-only-match ((((class color)) (:inherit font-lock-comment-face :weight bold))))
 '(iswitchb-virtual-matches ((t (:inherit font-lock-builtin-face))))
 '(log-view-file ((((class color) (background light)) (:background "#66f" :foreground "#fff" :weight bold))))
 '(margin-face ((t (:background "red"))))
 '(minibuffer-prompt ((t (:foreground "#00a"))))
 '(mode-line ((t (:inherit variable-pitch :background "#FFBB44" :foreground "black" :box (:line-width 3 :color "#FFBB44") :height 0.9))))
 '(mode-line-buffer-id ((t (:foreground "#990000" :weight bold))))
 '(mode-line-highlight ((((class color) (min-colors 88) (background light)) (:inherit mode-line :background "RoyalBlue4" :foreground "white" :box (:line-width 2 :color "RoyalBlue4")))))
 '(mode-line-inactive ((t (:inherit mode-line :background "grey80" :foreground "grey20" :box (:line-width 3 :color "grey80")))))
 '(org-special-keyword ((((class color) (min-colors 16) (background light)) (:foreground "#66aa00"))))
 '(outline-1 ((t (:inherit font-lock-keyword-face))))
 '(outline-2 ((t (:inherit font-lock-function-name-face))))
 '(outline-3 ((t (:inherit font-lock-variable-name-face))))
 '(outline-4 ((t (:inherit font-lock-type-face))))
 '(outline-5 ((t (:inherit font-lock-string-face))))
 '(outline-8 ((t (:inherit font-lock-comment-face))))
 '(pesche-tab ((t (:background "red"))))
 '(py-class-name-face ((t (:inherit font-lock-type-face :foreground "gold3" :underline t :weight bold))))
 '(py-decorators-face ((t (:inherit font-lock-keyword-face :foreground "#f2a" :weight normal))))
 '(py-exception-name-face ((t (:foreground "#f30"))))
 '(region ((((class color) (min-colors 88) (background light) (type gtk)) (:background "lightgoldenrod2"))))
 '(semantic-dirty-token-face ((((class color) (background light)) (:background "gray96"))))
 '(semantic-highlight-edits-face ((((class color) (background light)) (:background "gray95"))))
 '(semantic-highlight-func-current-tag-face ((((class color) (background light)) (:background "#e6e6e6"))))
 '(semantic-unmatched-syntax-face ((((class color) (background light)) (:underline "red"))))
 '(speedbar-directory-face ((((class color) (background light)) (:inherit speedbar-file-face :foreground "blue4"))))
 '(speedbar-file-face ((((class color) (background light)) (:inherit variable-pitch :foreground "cyan4"))))
 '(speedbar-highlight-face ((((class color) (background light)) (:inherit speedbar-file-face :background "green"))))
 '(speedbar-selected-face ((((class color) (background light)) (:inherit speedbar-file-face :foreground "red" :underline t))))
 '(speedbar-tag-face ((((class color) (background light)) (:inherit speedbar-file-face :foreground "brown"))))
 '(trailing-whitespace ((((class color) (background light)) (:background "#ffcccc"))))
 '(variable-pitch ((t (:height 80 :family "droid sans"))))
 '(widget-documentation ((((class color) (background light)) (:inherit custom-documentation :foreground "dark green")))))

(provide-theme 'Georg)
