(deftheme tango-dark-enhancements
  "My own enhancements to the tango-dark theme.")

(custom-theme-set-variables
 'tango-dark-enhancements
 )

(custom-theme-set-faces
 'tango-dark-enhancements
 '(cursor ((t (:background "gold"))))
 '(diff-added ((t (:inherit diff-changed :foreground "#b4fa70"))))
 '(diff-changed ((t (:background "#41423f"))))
 '(diff-context ((t (:inherit shadow :foreground "#e6a8df"))))
 '(diff-file-header ((t (:weight bold))))
 '(diff-header ((t (:foreground "#729fcf" :background "#212526"))))
 '(diff-hunk-header ((t (:weight bold))))
 '(diff-indicator-added ((t (:inherit diff-added :weight bold))))
 '(diff-indicator-removed ((t (:inherit diff-removed :weight bold))))
 '(diff-removed ((t (:inherit diff-changed :foreground "#ff5b5b"))))
 '(flymake-errline ((t (:underline "#ff5b5b"))))
 '(font-lock-comment-face ((t (:slant italic))))
 '(font-lock-function-name-face ((t (:weight bold))))
 '(font-lock-keyword-face ((t (:weight bold))))
 '(font-lock-type-face ((t (:weight bold))))
 '(hl-line ((t (:background "#345"))))
 '(linum ((t (:inherit (shadow fringe)))))
 '(mode-line ((t (:background "#b4fa70" :foreground "#2e3436" :box (:line-width 3 :color "#b4fa70")))))
 '(mode-line-inactive ((t (:background "#41423f" :foreground "#eeeeec" :box (:line-width 3 :color "#41423f")))))
 '(py-class-name-face ((t (:inherit font-lock-type-face :underline t))))
 )

(provide-theme 'tango-dark-enhancements)
