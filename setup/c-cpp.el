;; C and C++ mode setup

;; C style used in new Python source files
(c-add-style
 "python-new"
 '((indent-tabs-mode . nil)
   (fill-column      . 78)
   (c-basic-offset   . 4)
   (c-offsets-alist  . ((substatement-open . 0)
                        (inextern-lang . 0)
                        (arglist-intro . +)
                        (knr-argdecl-intro . +)))
   (c-hanging-braces-alist . ((brace-list-open)
                              (brace-list-intro)
                              (brace-list-close)
                              (brace-entry-open)
                              (substatement-open after)
                              (block-close . c-snug-do-while)))
   (c-block-comment-prefix . "* ")))

;; Style used mostly for JavaScript
(c-add-style
 "javascript"
 '((indent-tabs-mode . nil)
   (fill-column      . 78)
   (c-basic-offset   . 4)
   (c-offsets-alist  . ((substatement-open . 0)
                        (inextern-lang . 0)
                        (arglist-intro . +)
                        (knr-argdecl-intro . +)))
   (c-hanging-braces-alist . ((brace-list-open)
                              (brace-list-intro)
                              (brace-list-close)
                              (brace-entry-open)
                              (substatement-open after)
                              (block-close . c-snug-do-while)))
   (c-block-comment-prefix . "")))

(add-to-list 'c-default-style '(c-mode . "python-new"))
(add-to-list 'c-default-style '(c++-mode . "python-new"))
(add-to-list 'c-default-style '(js-mode . "javascript"))
(add-to-list 'c-default-style '(ecmascript-mode . "javascript"))

(defun c-select-style ()
  "Hack: Select the C style to use from buffer indentation."
  (save-excursion
    (if (re-search-forward "^\t" 3000 t)
        (c-set-style "python")
      (c-set-style "python-new"))))

(defun c-or-cpp-header ()
  (when (string-match "\\.h$" buffer-file-name)
    (save-excursion
      (when (re-search-forward "^class" 3000 t)
        (c++-mode)))))

(add-hook 'c-mode-hook 'c-select-style)
(add-hook 'c-mode-hook 'c-or-cpp-header)
(add-hook 'c++-mode-hook 'c-select-style)
;; Navigate by word parts
(add-hook 'c-mode-hook 'subword-mode)
(add-hook 'c++-mode-hook 'subword-mode)
;; Enable nice electric pairs
(add-hook 'c-mode-hook 'electric-pair-mode)
(add-hook 'c++-mode-hook 'electric-pair-mode)

;; Display C++ Doygen doc comments differently
(font-lock-add-keywords 'c++-mode '(("///.*$" 0 font-lock-doc-face prepend)))

;; SIP (Python/C++ wrapper generator) mode: mostly C++ syntax, but with
;; special directives (lines starting with %)

(defun sip-indent-directive ()
  (save-excursion
    (beginning-of-line)
    (when (looking-at-p "[ \t]*%")
      (just-one-space 0))))

(define-derived-mode sip-mode c++-mode "SIP"
  (font-lock-add-keywords
   nil '(("^\\(%[a-zA-Z]*\\)\\(.*\\)$"
          (1 font-lock-preprocessor-face prepend)
          (2 font-lock-string-face prepend))))
  (c-set-stylevar-fallback 'c-special-indent-hook '(sip-indent-directive)))

(add-to-list 'auto-mode-alist '("\\.sip$" . sip-mode))

;; C eldoc mode (automatic function signature tips)
(add-hook 'c-mode-hook 'c-turn-on-eldoc-mode)

;; C disaster mode (instant disassembly)
(eval-after-load 'cc-mode
  '(progn
     (require 'disaster)
     (define-key c-mode-base-map (kbd "C-c C-d") #'disaster)
     (define-key c-mode-base-map (kbd "C-c C-c") #'compile)
     ;(define-key c-mode-base-map (kbd "TAB") 'company-complete)

     (defun my-c-mode-common-hook ()
       (flycheck-mode 1)
       (company-mode 1)
       )
     (add-hook 'c-mode-common-hook 'my-c-mode-common-hook)))
