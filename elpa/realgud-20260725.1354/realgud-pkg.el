;; -*- no-byte-compile: t; lexical-binding: nil -*-
(define-package "realgud" "20260725.1354"
  "A modular front-end for interacting with external debuggers."
  '((load-relative "1.3.2")
    (loc-changes   "1.2")
    (test-simple   "1.3.0")
    (emacs         "27"))
  :url "https://github.com/realgud/realgud/"
  :commit "0b84e16cc596a0aa0e1093660add56578bdedada"
  :revdesc "0b84e16cc596"
  :keywords '("debugger" "gdb" "python" "perl" "go" "bash" "zsh" "bashdb" "zshdb" "remake" "trepan" "perldb" "pdb")
  :authors '(("Rocky Bernstein" . "rocky@gnu.org"))
  :maintainers '(("Rocky Bernstein" . "rocky@gnu.org")))
