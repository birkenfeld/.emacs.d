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
 '(font-lock-comment-face ((t (:slant italic))))
 '(font-lock-function-name-face ((t (:weight bold))))
 '(font-lock-keyword-face ((t (:weight bold))))
 '(hl-line ((t (:background "#345"))))
 '(linum ((t (:inherit (shadow fringe)))))
 '(mode-line ((t (:background "#d3d7cf" :foreground "#2e3436" :box (:line-width 2 :color "#d3d7cf")))))
 '(mode-line-inactive ((t (:background "#555753" :foreground "#eeeeec" :box (:line-width 2 :color "#555753"))))))

(provide-theme 'tango-dark-enhancements)
