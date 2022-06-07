;; Turn off mouse interface early in startup to avoid momentary display
(if (fboundp 'menu-bar-mode) (menu-bar-mode -1))
(if (fboundp 'tool-bar-mode) (tool-bar-mode -1))
(if (fboundp 'scroll-bar-mode) (scroll-bar-mode -1))

;; Set up load path
(add-to-list 'load-path "~/.emacs.d/lisp")

;; Enable the Emacs server, allows thin editing sessions via emacsclient
(require 'server)
(unless (server-running-p) (server-start))

;; Set up our theme path
(setq custom-theme-directory "~/.emacs.d/themes")

;; Compatibility hack for winpoint.el
(unless (fboundp 'make-variable-frame-local)
  (defun make-variable-frame-local (sym)))

;; Load package manager and add the alternate package repo
(require 'package)
(add-to-list 'package-archives
             '("gnu" . "http://elpa.gnu.org/packages/") t)
(add-to-list 'package-archives
             '("melpa-stable" . "http://stable.melpa.org/packages/") t)
(add-to-list 'package-archives
             '("melpa" . "http://melpa.org/packages/") t)
(package-initialize)

;; Load custom variables and faces
(setq custom-file "~/.emacs.d/custom.el")
(load custom-file)

;; Powerline: start it up right now to get the modeline early
(require 'powerline)
(powerline-default-theme)

;; Fix the annoying "popup-buffer-is-1-line-too-small" glitch with powerline
(setq orig-window-scroll-bar-height (symbol-function 'window-scroll-bar-height))
(defun window-scroll-bar-height (&optional window)
  (let ((res (funcall orig-window-scroll-bar-height window)))
    (+ res 1)))

;; Load all files under setup/.
;; The files are loaded in alphabetically sorted order!
(mapc 'load (directory-files "~/.emacs.d/setup" t "\\.el"))
;; Load everything under local/ (settings that are not in the repo).
(mapc 'load (directory-files "~/.emacs.d/local-setup" t "\\.el"))

;; Session (saves histories, variables, ...)
;; To be called after all other initialization
(require 'session)
(session-initialize)
