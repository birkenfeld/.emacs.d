Main Features:
1. Words or selections highlights with different colors set. The highlights
   are still visible even under current line highlight (`hl-line-mode' or
   `global-hl-line-mode' is enabled).
2. Search highlighted things at point in the current buffer.
3. Highlight outward and inward parentheses with different colors set.

Add the following to your .emacs file:
(require 'hl-anything)

Toggle highlighting things at point:
  M-x `hl-highlight-thingatpt-local'

Remove all highlights:
  M-x `hl-unhighlight-all-local'

Search highlights:
  M-x `hl-find-thing-forwardly'
  M-x `hl-find-thing-backwardly'

Enable parenethese highlighting:
  M-x `hl-paren-mode'

Extended Feature:
1. Additional faces set for temporary highlights.
