;; -*- no-byte-compile: t; lexical-binding: nil -*-
(define-package "realgud" "20250605.1143"
  "A modular front-end for interacting with external debuggers."
  '((load-relative "1.3.1")
    (loc-changes   "1.2")
    (test-simple   "1.3.0")
    (emacs         "25"))
  :url "https://github.com/realgud/realgud/"
  :commit "bc479d7e1b14721006ec76b47bc070756baec16b"
  :revdesc "bc479d7e1b14"
  :keywords '("debugger" "gdb" "python" "perl" "go" "bash" "zsh" "bashdb" "zshdb" "remake" "trepan" "perldb" "pdb")
  :authors '(("Rocky Bernstein" . "rocky@gnu.org"))
  :maintainers '(("Rocky Bernstein" . "rocky@gnu.org")))
