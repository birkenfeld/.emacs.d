;; -*- no-byte-compile: t; lexical-binding: nil -*-
(define-package "copilot" "20260325.1151"
  "An Emacs plugin for GitHub Copilot."
  '((emacs         "27.2")
    (editorconfig  "0.8.2")
    (jsonrpc       "1.0.14")
    (compat        "30")
    (track-changes "1.4"))
  :url "https://github.com/copilot-emacs/copilot.el"
  :commit "e8cd58754743ae37732af13340f3d0d0c4f3ff0f"
  :revdesc "e8cd58754743"
  :keywords '("convenience" "copilot")
  :authors '(("zerol" . "z@zerol.me"))
  :maintainers '(("Bozhidar Batsov" . "bozhidar@batsov.dev")))
