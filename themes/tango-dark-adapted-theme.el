;;; tango-dark-adapted-theme.el --- Tango-based custom theme for faces

;; tango-dark-theme.el copyright follows.

;; Copyright (C) 2010-2014 Free Software Foundation, Inc.

;; Authors: Chong Yidong <cyd@stupidchicken>
;;          Jan Moringen <jan.moringen@uni-bielefeld.de>

;; This file is part of GNU Emacs.

;; GNU Emacs is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; GNU Emacs is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary

;; The colors in this theme come from the Tango palette, which is in
;; the public domain: http://tango.freedesktop.org/

;;; Code:

(deftheme tango-dark-adapted
  "My own adapted tango-dark theme.")

(let ((xwin '((class color) (min-colors 89) (type x)))
      (term '((type ())))
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
      (red-0 "#ff4b4b")  (alum-5.5 "#41423f") (alum-7 "#212526")
      (red-4 "#662020")  (cham-4 "#105a00")
      )

  (custom-theme-set-faces
   'tango-dark-adapted
   `(default ((,xwin (:foreground ,alum-1 :background ,alum-6))))
   ;; Cursor color (unused with CUA, see below in variables section)
   `(cursor ((,xwin (:background ,butter-1))))
   ;; Highlighting faces
   `(fringe ((,xwin (:background ,alum-7))))
   `(highlight ((,xwin (:foreground ,alum-6 :background ,butter-2))
                (,term (:background "#ffffd7"))))
   `(hl-line ((,xwin (:background "#345"))))
   `(region ((,xwin (:background ,alum-5))))
   `(secondary-selection ((,xwin (:background ,blue-3))))
   `(isearch ((,xwin (:foreground ,alum-1 :background ,orange-3))))
   `(lazy-highlight ((,xwin (:background ,choc-3))))
   `(trailing-whitespace ((,xwin (:background ,red-3))))
   `(whitespace-tab ((,xwin (:background "#502525"))))
   `(highlight-indent-face ((,xwin (:inherit fringe :background ,alum-6))
                            (,term (:inherit nil))))
   `(highlight-symbol-face ((,xwin (:background "#1c1c1c"))
                            (,term (:inherit nil))))
   ;; Info faces
   `(Info-quoted ((,xwin (:foreground ,butter-1))))
   ;; Header/Mode line faces
   `(header-line ((,xwin (:inherit nil :background ,alum-5.5))))
   ;; `(mode-line ((,xwin (:inherit variable-pitch :background ,cham-0 :foreground ,alum-6
   ;;                                  :box (:line-width 3 :color ,cham-0)))
   ;;              (,term (:background ,cham-0))))
   ;; `(mode-line-inactive ((,xwin (:background ,alum-5.5 :foreground ,alum-1
   ;;                                           :box (:line-width 3 :color ,alum-5.5)))
   ;;                       (,term (:background ,alum-2))))
   ;; Settings for powerline
   `(mode-line ((,xwin (;:inherit variable-pitch
                        :background ,cham-0 :foreground ,alum-6))
                (,term (:background ,cham-0))))
   `(mode-line-inactive ((,xwin (;:inherit variable-pitch
                                 :background ,alum-5 :foreground ,alum-1))
                         (,term (:background ,alum-2))))
   `(mode-line-buffer-id ((t (:weight bold))))
   `(mode-line-highlight ((,xwin (:foreground ,red-2 :underline nil))))
   `(powerline-active1 ((,xwin (:background ,cham-1 :foreground ,alum-6))
                        (,term (:background ,cham-1))))
   `(powerline-active2 ((,xwin (:background ,cham-3 :foreground ,alum-1))
                        (,term (:background ,cham-3 :foreground "#ffffff"))))
   `(powerline-inactive1 ((,xwin (:background ,alum-4))
                          (,term (:background ,alum-3))))
   `(powerline-inactive2 ((,xwin (:background ,alum-4))
                          (,term (:background ,alum-3))))
   `(compilation-mode-line-fail ((,xwin (:foreground ,red-3))))
   `(compilation-mode-line-run  ((,xwin (:foreground ,butter-1))))
   `(compilation-mode-line-exit ((,xwin (:foreground "#99ff00"))))
   `(linum ((,xwin (:inherit (shadow fringe)))))
   `(vhl/default-face ((,xwin (:background ,alum-7))))
   ;; Escape and prompt faces
   `(minibuffer-prompt ((,xwin (:foreground ,cham-0))))
   `(escape-glyph ((,xwin (:foreground ,butter-3))))
   `(error ((,xwin (:foreground ,red-0))))
   `(warning ((,xwin (:foreground ,orange-1))))
   `(success ((,xwin (:foreground ,cham-1))))
   ;; Font lock faces
   `(font-lock-builtin-face ((,xwin (:foreground ,plum-1))))
   `(font-lock-comment-face ((,xwin (:foreground ,cham-2 :slant italic))
                             (,term (:foreground "blue"))))
   `(font-lock-constant-face ((,xwin (:foreground ,plum-0))))
   `(font-lock-doc-face ((,xwin (:foreground ,plum-0))))
   `(font-lock-function-name-face ((,xwin (:foreground ,butter-1 :weight bold))))
   `(font-lock-keyword-face ((,xwin (:foreground ,cham-0 :weight bold))))
   `(font-lock-string-face ((,xwin (:foreground ,choc-1))
                            (,term (:foreground ,red-3))))
   `(font-lock-type-face ((,xwin (:foreground ,blue-0 :weight bold))))
   `(font-lock-variable-name-face ((,xwin (:foreground ,orange-1))
                                   (,term ())))
   `(font-lock-warning-face ((,xwin (:foreground ,red-3 :background ,butter-2))
                             (,term (:foreground "red" :background "#ffd700"))))
   `(eldoc-highlight-function-argument ((,xwin (:weight bold)) (,term ())))
   ;; Completion faces
   `(ido-only-match ((,xwin (:foreground ,cham-2))))
   `(ido-first-match ((,xwin (:weight bold :foreground ,butter-1))
                      (,term (:foreground "#87005f"))))
   `(company-preview ((,xwin (:foreground ,alum-3))))
   `(company-preview-search ((,xwin (:foreground ,blue-0))))
   `(company-preview-common ((t (:inherit company-preview :underline t))))
   `(company-pseudo-tooltip-selection-face ((t (:inherit company-pseudo-tooltip-face :background "#ff6600"))) t)
   `(company-scrollbar-bg ((t (:inherit company-tooltip :background "gray40"))))
   `(company-scrollbar-fg ((t (:background "gray10"))))
   `(company-tooltip ((t (:background "gray80" :foreground "black"))))
   `(company-tooltip-annotation ((,xwin (:inherit company-tooltip :foreground ,red-3))))
   `(company-tooltip-selection ((,xwin (:inherit company-tooltip :background ,orange-2))))
   `(company-tooltip-common ((t (:inherit company-tooltip))))
   `(company-tooltip-common-selection ((t (:inherit company-tooltip-selection))))
   ;; Button and link faces
   `(link ((,xwin (:underline t :foreground ,blue-1))))
   `(link-visited ((,xwin (:underline t :foreground ,blue-2))))
   ;; Message faces
   `(message-header-name ((,xwin (:foreground ,blue-1))))
   `(message-header-cc ((,xwin (:foreground ,butter-3))))
   `(message-header-other ((,xwin (:foreground ,choc-2))))
   `(message-header-subject ((,xwin (:foreground ,cham-1))))
   `(message-header-to ((,xwin (:foreground ,butter-2))))
   `(message-cited-text ((,xwin (:foreground ,cham-1))))
   `(message-separator ((,xwin (:foreground ,plum-1))))
   ;; SMerge faces
   `(smerge-refined-change ((,xwin (:background ,blue-3))))
   ;; Diff mode faces
   `(diff-added ((,xwin (:inherit diff-changed :foreground ,cham-0))))
   `(diff-changed ((,xwin (:background ,alum-5.5))))
   `(diff-context ((,xwin (:inherit shadow :foreground ,plum-0))))
   `(diff-file-header ((,xwin (:weight bold :foreground ,blue-0))))
   `(diff-header ((,xwin (:foreground ,blue-1 :background ,alum-7))))
   `(diff-hl-change ((,xwin (:background ,blue-3))))
   `(diff-hl-delete ((,xwin (:background ,red-4))))
   `(diff-hl-insert ((,xwin (:background ,cham-4))))
   `(diff-hunk-header ((,xwin (:weight bold))))
   `(diff-indicator-added ((,xwin (:inherit diff-added :weight bold))))
   `(diff-indicator-removed ((,xwin (:inherit diff-removed :weight bold))))
   `(diff-removed ((,xwin (:inherit diff-changed :foreground "#ff5b5b"))))
   ;; Ediff faces
   `(ediff-current-diff-A ((,xwin (:background ,alum-5))))
   `(ediff-fine-diff-A ((,xwin (:background ,blue-3))))
   `(ediff-even-diff-A ((,xwin (:background ,alum-5.5))))
   `(ediff-odd-diff-A ((,xwin (:background ,alum-5.5))))
   `(ediff-current-diff-B ((,xwin (:background ,alum-5))))
   `(ediff-fine-diff-B ((,xwin (:background ,choc-3))))
   `(ediff-even-diff-B ((,xwin (:background ,alum-5.5))))
   `(ediff-odd-diff-B ((,xwin (:background ,alum-5.5))))
   ;; Flyspell/flymake/flycheck faces
   `(flyspell-duplicate ((,xwin (:underline (:color ,orange-1 :style wave)))))
   `(flyspell-incorrect ((,xwin (:underline (:color ,red-1 :style wave)))))
   `(flymake-warnline ((,xwin (:underline (:color ,orange-1 :style wave)))))
   `(flymake-errline ((,xwin (:underline (:color ,red-0 :style wave)))))
   `(flycheck-error ((,xwin (:underline (:color ,red-0 :style wave)))))
   `(flycheck-warning ((,xwin (:underline (:color ,orange-1 :style wave)))))
   `(flycheck-info ((,xwin (:underline (:color ,cham-1 :style wave)))))
   `(flycheck-fringe-error ((,xwin (:foreground ,red-0))))
   `(flycheck-fringe-warning ((,xwin (:foreground ,orange-1))))
   `(flycheck-fringe-info ((,xwin (:foreground ,cham-1))))
   ;; Grep/edit mode
   `(grep-edit-face ((,xwin (:foreground ,butter-1 :background ,alum-7 :weight bold))))
   `(grep-edit-done-face ((,xwin (:foreground ,blue-0 :weight bold))))
   `(grep-edit-file-face ((,xwin (:background ,blue-3 :weight bold))))
   ;; rst mode
   `(rst-level-1-face ((,xwin (:background "grey50"))))
   `(rst-level-2-face ((,xwin (:background "grey35"))))
   `(rst-level-3-face ((,xwin (:background "grey20"))))
   `(rst-level-4-face ((,xwin (:background "grey10"))))
   `(rst-level-5-face ((,xwin (:background "black"))))
   ;; tabbar-mode faces
   `(tabbar-default ((,xwin (:background "gray90" :foreground "gray50"))))
   `(tabbar-selected ((,xwin (:inherit tabbar-default :background ,alum-6 :foreground ,cham-0
                               :box (:line-width 4 :color ,alum-6)))))
   `(tabbar-separator ((,xwin (:inherit tabbar-default))))
   `(tabbar-highlight ((,xwin (:background ,blue-0 :box (:line-width 4 :color ,blue-0)))))
   `(tabbar-unselected ((,xwin (:inherit tabbar-default :box (:line-width 4 :color "grey90")))))
   `(tabbar-button ((,xwin (:inherit tabbar-default :foreground "dark red"))))
   `(tabbar-button-highlight ((,xwin (:inherit tabbar-default :background "white"
                                      :box (:line-width 2 :color "white")))))
   ;; Semantic faces
   `(semantic-decoration-on-includes ((,xwin (:underline ,alum-4))))
   `(semantic-decoration-on-private-members-face
     ((,xwin (:background ,plum-3))))
   `(semantic-decoration-on-protected-members-face
     ((,xwin (:background ,choc-3))))
   `(semantic-decoration-on-unknown-includes
     ((,xwin (:background ,red-3))))
   `(semantic-decoration-on-unparsed-includes
     ((,xwin (:background ,alum-5.5))))
   `(semantic-tag-boundary-face ((,xwin (:overline ,blue-1))))
   `(semantic-unmatched-syntax-face ((,xwin (:underline ,red-1))))
   ;; Speedbar faces
   `(speedbar-button-face ((,xwin (:foreground ,cham-2 :weight bold :height 1.2))))
   `(speedbar-directory-face ((,xwin (:inherit variable-pitch :foreground ,butter-2 :weight bold))))
   `(speedbar-file-face ((,xwin (:inherit variable-pitch :foreground ,blue-0))))
   `(speedbar-highlight-face ((,xwin (:background ,choc-2))))
   `(speedbar-selected-face ((,xwin (:inherit speedbar-file-face :weight bold))))
   `(speedbar-separator-face ((,xwin (:inherit variable-pitch :background ,choc-3 :foreground ,alum-1))))
   `(speedbar-tag-face ((,xwin (:inherit variable-pitch :foreground ,plum-0))))
   ;; Misc faces
   `(paradox-mode-line-face ((,xwin (:foreground ,blue-3))))
   `(path-header-directory-face ((,xwin (:foreground "#8fb28f")) (,term (:foreground "white"))))
   `(path-header-filename-face ((,xwin (:foreground ,alum-1)) (,term (:foreground "white"))))
   `(ace-jump-face-foreground ((,xwin (:foreground ,butter-2))))
   `(show-paren-match ((,xwin (:weight bold :background ,blue-1))))
   `(mc/cursor-face ((,xwin (:underline ,butter-1))))
   ;; Terminal faces
   `(term-color-black ((,xwin (:foreground ,alum-7))))
   `(term-color-red ((,xwin (:foreground ,red-0))))
   `(term-color-green ((,xwin (:foreground ,cham-0))))
   `(term-color-yellow ((,xwin (:foreground ,butter-1))))
   `(term-color-blue ((,xwin (:foreground ,blue-1))))
   `(term-color-magenta ((,xwin (:foreground ,plum-1))))
   `(term-color-cyan ((,xwin (:foreground ,blue-0))))
   `(term-color-white ((,xwin (:foreground ,alum-1))))
   )

  (custom-theme-set-variables
   'tango-dark-adapted
   `(ansi-color-names-vector [,alum-7 ,red-0 ,cham-0 ,butter-1  ;; not sure this is needed 
                              ,blue-1 ,plum-1 ,blue-0 ,alum-1])
   `(cua-normal-cursor-color ,butter-1)
   `(cua-overwrite-cursor-color ,red-1)
   `(cua-read-only-cursor-color ,cham-2)
   )
  )

(provide-theme 'tango-dark-adapted)

;;; tango-dark-adapted-theme.el ends here
