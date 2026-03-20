;; -*- no-byte-compile: t; lexical-binding: nil -*-
(define-package "copilot" "20260316.2135"
  "An Emacs plugin for GitHub Copilot."
  '((emacs         "27.2")
    (editorconfig  "0.8.2")
    (jsonrpc       "1.0.14")
    (compat        "30")
    (track-changes "1.4"))
  :url "https://github.com/copilot-emacs/copilot.el"
  :commit "c8c06efaa508569e13d7191882ae33435bb14543"
  :revdesc "c8c06efaa508"
  :keywords '("convenience" "copilot")
  :authors '(("zerol" . "z@zerol.me"))
  :maintainers '(("Bozhidar Batsov" . "bozhidar@batsov.dev")))
