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
   `(cursor ((,class (:background "gold"))))
   `(diff-added ((,class (:inherit diff-changed :foreground ,cham-0))))
   `(diff-changed ((,class (:background ,alum-5.5))))
   `(diff-context ((,class (:inherit shadow :foreground ,plum-0))))
   `(diff-file-header ((,class (:weight bold))))
   `(diff-header ((,class (:foreground ,blue-1 :background ,alum-7))))
   `(diff-hunk-header ((,class (:weight bold))))
   `(diff-indicator-added ((,class (:inherit diff-added :weight bold))))
   `(diff-indicator-removed ((,class (:inherit diff-removed :weight bold))))
   `(diff-removed ((,class (:inherit diff-changed :foreground "#ff5b5b"))))
   `(flymake-errline ((,class (:underline "#ff5b5b"))))
   `(font-lock-comment-face ((,class (:slant italic))))
   `(font-lock-function-name-face ((,class (:weight bold))))
   `(font-lock-keyword-face ((,class (:weight bold))))
   `(font-lock-type-face ((,class (:weight bold))))
   `(grep-edit-face ((,class (:foreground ,butter-1 :background ,alum-7 :weight bold))))
   `(grep-edit-done-face ((,class (:foreground ,blue-0 :weight bold))))
   `(grep-edit-file-face ((,class (:background ,blue-3 :weight bold))))
   `(header-line ((,class (:inherit nil :background ,alum-5.5))))
   `(hl-line ((,class (:background "#345"))))
   `(linum ((,class (:inherit (shadow fringe)))))
   `(mode-line ((,class (:background ,cham-0 :foreground ,alum-6
                         :box (:line-width 3 :color ,cham-0)))))
   `(mode-line-inactive ((,class (:background ,alum-5.5 :foreground ,alum-1
                                  :box (:line-width 3 :color ,alum-5.5)))))
   `(py-class-name-face ((,class (:inherit font-lock-type-face :underline t))))
   ))

(provide-theme 'tango-dark-enhancements)
