(deftheme tango-dark-enhancements
  "My own enhancements to the tango-dark theme.")

(custom-theme-set-variables
 'tango-dark-enhancements
 )

(let ((class '((class color) (min-colors 89)))
      ;; Tango palette colors.
      (butter-1 "#fce94f") (butter-2 "#edd400") (butter-3 "#c4a000")
      (orange-1 "#fcaf3e") (orange-2 "#f57900") (orange-3 "#ce5c00")
      (choc-1 "#e9b96e") (choc-2 "#c17d11") (choc-3 "#8f5902")
      (cham-1 "#8ae234") (cham-2 "#73d216") (cham-3 "#4e9a06")
      (blue-1 "#729fcf") (blue-2 "#3465a4") (blue-3 "#204a87")
      (plum-1 "#ad7fa8") (plum-2 "#75507b") (plum-3 "#5c3566")
      (red-1 "#ef2929")  (red-2 "#cc0000")  (red-3 "#a40000")
      (alum-1 "#eeeeec") (alum-2 "#d3d7cf") (alum-3 "#babdb6")
      (alum-4 "#888a85") (alum-5 "#555753") (alum-6 "#2e3436")
      ;; Not in Tango palette; used for better contrast.
      (cham-0 "#b4fa70") (blue-0 "#8cc4ff") (plum-0 "#e6a8df")
      (red-0 "#ff4b4b")  (alum-5.5 "#41423f") (alum-7 "#212526"))

  (custom-theme-set-faces
   'tango-dark-enhancements
   `(cursor ((t (:background "gold"))))
   `(diff-added ((t (:inherit diff-changed :foreground ,cham-0))))
   `(diff-changed ((t (:background ,alum-5.5))))
   `(diff-context ((t (:inherit shadow :foreground ,plum-0))))
   `(diff-file-header ((t (:weight bold))))
   `(diff-header ((t (:foreground ,blue-1 :background ,alum-7))))
   `(diff-hunk-header ((t (:weight bold))))
   `(diff-indicator-added ((t (:inherit diff-added :weight bold))))
   `(diff-indicator-removed ((t (:inherit diff-removed :weight bold))))
   `(diff-removed ((t (:inherit diff-changed :foreground "#ff5b5b"))))
   `(flymake-errline ((t (:underline "#ff5b5b"))))
   `(font-lock-comment-face ((t (:slant italic))))
   `(font-lock-function-name-face ((t (:weight bold))))
   `(font-lock-keyword-face ((t (:weight bold))))
   `(font-lock-type-face ((t (:weight bold))))
   `(hl-line ((t (:background "#345"))))
   `(linum ((t (:inherit (shadow fringe)))))
   `(mode-line ((t (:background ,cham-0 :foreground ,alum-6 :box (:line-width 3 :color ,cham-0)))))
   `(mode-line-inactive ((t (:background ,alum-5.5 :foreground ,alum-1 :box (:line-width 3 :color ,alum-5.5)))))
   `(py-class-name-face ((t (:inherit font-lock-type-face :underline t))))
   ))

(provide-theme 'tango-dark-enhancements)
