;; New general editing commands

;; Overwrite: the menu-bar implementation refuses to work without menubar
(defun kill-this-buffer ()	
  "Kill the current buffer.
When called in the minibuffer, get out of the minibuffer
using `abort-recursive-edit'."
  (interactive)
  (cond
   ((menu-bar-non-minibuffer-window-p)
    (kill-buffer (current-buffer)))
   (t
    (abort-recursive-edit))))

;; It's annoying to have minibuffer editing pollute the kill ring
(defun backward-delete-word (arg)
  "Delete (not kill) word before point."
  (interactive "p")
  (delete-region (point) (progn (forward-word (- arg)) (point))))

(defun split-window-horizontally-into-3 ()
  "Split current window horizontally into three windows of equal width,
   and mark the rightmost one as dedicated (which means no automatic
   display of buffers in it)."
  (interactive)
  (let* ((edges (window-edges))
         (width (- (caddr edges) (car edges)))
         (rightwin (split-window nil (/ width 3) t))
         (rightrightwin (split-window rightwin (/ width 3) t)))
    ;; setting dedicated to t would "strongly" dedicate the window with
    ;; the effect that not even switch-to-buffer would work anymore...
    (set-window-dedicated-p rightrightwin 'yes)))

(defun fullscreen ()
  "Toggle fullscreen editing."
  (interactive)
  (menu-bar-mode 0)
  (set-frame-parameter nil 'fullscreen
                       (if (frame-parameter nil 'fullscreen) nil 'fullboth)))

(defun kill-other-buffer ()
  "Kill other window's buffer."
  (interactive)
  (let ((window (selected-window)))
    (other-window 1)
    (kill-buffer nil)
    (select-window window)))

(defun find-file-with-linenum ()
  "Find file and go to line number specifed with :num."
  (interactive)
  (let* ((fname (car (find-file-read-args "Find file: "
                                          (confirm-nonexistent-file-or-buffer))))
         (cpos (string-match ":" fname))
         (line (if cpos (string-to-number (substring fname (1+ cpos))) 0))
         (fpos (or (and (> line 0) cpos) (length fname))))
    (find-file-at-point (substring fname 0 fpos))
    (when cpos (goto-line (string-to-number (substring fname (1+ cpos)))))))

(defun isearch-kill-match ()
  "Kill the current isearch match string and continue searching."
  (interactive)
  (kill-region isearch-other-end (point)))

(defun isearch-occur ()
  "Invoke `occur' from within isearch."
  (interactive)
  (let ((case-fold-search isearch-case-fold-search))
    (occur (if isearch-regexp isearch-string (regexp-quote isearch-string)))))

(defun isearch-yank-whole-symbol ()
  "Pull symbol at point into search string."
  (interactive)
  (isearch-yank-string (symbol-name (symbol-at-point))))

(defun zap-to-isearch (rbeg rend)
  "Kill the region between the mark and the closest portion of
the isearch match string. The behaviour is meant to be analogous
to zap-to-char; let's call it zap-to-isearch. The deleted region
does not include the isearch word. This is meant to be bound only
in isearch mode.  The point of this function is that oftentimes
you want to delete some portion of text, one end of which happens
to be an active isearch word. The observation to make is that if
you use isearch a lot to move the cursor around (as you should,
it is much more efficient than using the arrows), it happens a
lot that you could just delete the active region between the mark
and the point, not include the isearch word."
  (interactive "r")
  (when (not mark-active)
    (error "Mark is not active"))
  (let* ((isearch-bounds (list isearch-other-end (point)))
         (ismin (apply 'min isearch-bounds))
         (ismax (apply 'max isearch-bounds))
         )
    (if (< (mark) ismin)
        (kill-region (mark) ismin)
      (if (> (mark) ismax)
          (kill-region ismax (mark))
        (error "Internal error in isearch kill function.")))
    (isearch-exit)
    ))

(defun occur-dwim ()
  "Call `occur' with a sane default."
  (interactive)
  (let ((default (if (region-active-p)
                     (buffer-substring-no-properties
                      (region-beginning)
                      (region-end))
                   (symbol-name (symbol-at-point)))))
    (when default (push default regexp-history))
    (call-interactively 'occur)))

(defun forward-char-but-not-eol ()
  (interactive)
  (if (not (looking-at "\n")) (forward-char)))

(defvar current-this-regex "")

(defun search-for-this-word ()
  "Emulate Vim's `*' binding."
  (interactive)
  (let ((tag (find-tag-default)))
    (if tag (setq new-this-regex
                  (concat "\\_<" (regexp-quote tag) "\\_>"))
      (error "point not over tag")))
  (unless (string-equal new-this-regex current-this-regex)
    (setq current-this-regex new-this-regex)
    (font-lock-fontify-buffer)
    (message (concat "Searching for " (substring new-this-regex 2 -2))))
  (unless
      (let ((case-fold-search nil))
        (search-forward-regexp current-this-regex nil t
                               (if (looking-at "\\_<") 2 1)))
    (beginning-of-buffer)
    (message "search hit BOTTOM, continuing at TOP")
    (let ((case-fold-search nil))
      (search-forward-regexp current-this-regex)))
  (while (not (looking-at current-this-regex))
    (backward-char 1)))

(defun count-words ()
  "Count words in region or buffer."
  (interactive)
  (if (region-active-p)
      (message "Word count: %s" (how-many "\\w+"
                                          (region-beginning) (region-end)))
    (message "Word count: %s" (how-many "\\w+"
                                        (point-min) (point-max)))))

(defun increase-font-size (&optional amount)
  (interactive)
  (let ((current (face-attribute 'default :height)))
    (set-face-attribute 'default nil :height
                        (+ current (or amount 10)))))

(defun decrease-font-size ()
  (interactive)
  (increase-font-size -10))

(defun smart-tab ()
  "This smart tab is minibuffer compliant: it acts as usual in
   the minibuffer. Else, if mark is active, indents region. Else if
   point is at the end of a symbol, expands it. Else indents the
   current line."
  (interactive)
  (if (minibufferp)
      (unless (minibuffer-complete)
        (dabbrev-expand nil))
    (if (smart-tab-must-expand prefix)
        (smart-expand-function)
      (smart-indent))))

(defun smart-indent ()
  "Indents region if mark is active, or current line otherwise."
  (interactive)
  (if mark-active
      (indent-region (region-beginning)
                     (region-end))
    (indent-for-tab-command)))

(defun smart-tab-must-expand (&optional prefix)
  "If PREFIX is \\[universal-argument], answers no.
   Otherwise, analyses point position and answers."
  (unless (or (consp prefix)
              mark-active)
    (looking-at "\\_>")))

(defun comment-or-uncomment-region-dwim (&optional arg)
  "(Un)Comment active region, or the next ARG lines."
  (interactive "*p")
  (if mark-active
      (call-interactively 'comment-or-uncomment-region)
    (save-excursion
      (let ((bol (line-beginning-position))
            (eol (prog2 (forward-line (1- arg)) (line-end-position))))
        (comment-or-uncomment-region bol eol)))))

(defun prompt-face-color (face)
  "Repeatedly prompt for new color for a given face."
  (interactive (list (read-face-name "Face" "default")))
  (if (member face '(nil ""))
      (setq face 'default))
  (while t
    (set-face-attribute face nil :foreground (read-color "Color: "))))

(defun count-words-wc (&optional filename)
  "Returns the word count of the current buffer.  If `filename' is not nil,
returns the word count of that file."
  (interactive)
  (save-some-buffers) ;; Make sure the current buffer is saved
  (let ((tempfile nil))
    (if (null filename)
        (progn
          (let ((buffer-file (buffer-file-name))
                (lcase-file (downcase (buffer-file-name))))
            (if (and (>= (length lcase-file) 4)
                     (string= (substring lcase-file -4 nil) ".tex"))
                ;; This is a LaTeX document, so DeTeX it!
                (progn
                  (setq filename (make-temp-file "wordcount"))
                  (shell-command-to-string
                   (concat "detex < " buffer-file " > " filename))
                  (setq tempfile t))
              (setq filename buffer-file)))))
    (let ((result (car (split-string
                        (shell-command-to-string
                         (concat "wc -w " filename)) " "))))
      (if tempfile
          (delete-file filename))
      (message (concat "Word Count: " result)))))

(autoload 'copy-from-above-command "misc"
  "Copy characters from previous nonblank line, starting just above point.

  \(fn &optional arg)"
  'interactive)

(defun copy-above-while-same ()
  "Copy from the previous two lines until the first difference."
  (interactive)
  (let* ((col (current-column))
         (n (compare-buffer-substrings
             (current-buffer) ;; This buffer
             (save-excursion
               (forward-line -2)
               (move-to-column col)
               (point)) ;; Start 1
             (line-end-position -1) ;; End 1
             (current-buffer) ;; Still this buffer
             (save-excursion
               (forward-line -1)
               (move-to-column col)
               (point)) ;; Start 2
             (line-end-position 0)))) ;; End 2
    (cond ((not (integerp n))
           (copy-from-above-command 1))
          ((> (abs n) 1)
           (copy-from-above-command (1- (abs n) )))
          (t ;; (zerop n)
           (copy-from-above-command)))))

(defun spellcheck-english ()
  (interactive)
  (ispell-change-dictionary "english")
  (flyspell-mode 1)
  (flyspell-buffer))

(defun open-line-below ()
  (interactive)
  (end-of-line)
  (newline)
  (indent-for-tab-command))

(defun open-line-above ()
  (interactive)
  (beginning-of-line)
  (newline)
  (forward-line -1)
  (indent-for-tab-command))

(defun prelude-move-beginning-of-line (arg)
  "Move point back to indentation or beginning of line."
  (interactive "^p")
  (setq arg (or arg 1))

  ;; Move lines first
  (when (/= arg 1)
    (let ((line-move-visual nil))
      (forward-line (1- arg))))

  (let ((orig-point (point)))
    (back-to-indentation)
    (when (= orig-point (point))
      (move-beginning-of-line 1))))

(defun rename-current-buffer-file ()
  "Renames current buffer and file it is visiting."
  (interactive)
  (let ((name (buffer-name))
        (filename (buffer-file-name)))
    (if (not (and filename (file-exists-p filename)))
        (error "Buffer '%s' is not visiting a file!" name)
      (let ((new-name (read-file-name "New name: " filename)))
        (if (get-buffer new-name)
            (error "A buffer named '%s' already exists!" new-name)
          (ignore-errors (make-directory (file-name-directory new-name) t))
          (ignore-errors (vc-call rename-file filename new-name))
          (when (file-exists-p filename)  ;; if no vc was there
            (rename-file filename new-name 1))
          (rename-buffer new-name)
          (set-visited-file-name new-name)
          (set-buffer-modified-p nil)
          (message "File '%s' successfully renamed to '%s'"
                   name (file-name-nondirectory new-name)))))))

(defun goto-line-with-feedback ()
  "Show line numbers temporarily, while prompting for the line number input"
  (interactive)
  (unwind-protect
      (progn
        (linum-mode 1)
        (goto-line (read-number "Goto line: ")))
    (linum-mode -1)))

(defun sudo-edit (&optional arg)
  "Edit current or specified file as root using sudo."
  (interactive "p")
  (if (or arg (not buffer-file-name))
      (find-file (concat "/sudo:root@localhost:" (ido-read-file-name "File: ")))
    (find-alternate-file (concat "/sudo:root@localhost:" buffer-file-name))))

(defun kill-and-retry-line ()
  "Kill the entire current line and reposition point at indentation"
  (interactive)
  (back-to-indentation)
  (kill-line))

(defun save-region-or-current-line (arg)
  "If no region is active, copy the current line instead of nothing."
  (interactive "P")
  (if (region-active-p)
      (kill-ring-save (region-beginning) (region-end))
    (copy-line arg)))

(defun copy-to-end-of-line ()
  (interactive)
  (kill-ring-save (point)
                  (line-end-position))
  (message "Copied to end of line"))

(defun copy-whole-lines (arg)
  "Copy lines (as many as prefix argument) in the kill ring"
  (interactive "p")
  (kill-ring-save (line-beginning-position)
                  (line-beginning-position (+ 1 arg)))
  (message "%d line%s copied" arg (if (= 1 arg) "" "s")))

(defun copy-line (arg)
  "Copy to end of line, or as many lines as prefix argument"
  (interactive "P")
  (if (null arg)
      (copy-to-end-of-line)
    (copy-whole-lines (prefix-numeric-value arg))))

(defun rotate-windows ()
  "Rotate two windows in a split frame."
  (interactive)
  (cond ((not (> (count-windows)1))
         (message "You can't rotate a single window!"))
        (t
         (setq i 1)
         (setq numWindows (count-windows))
         (while  (< i numWindows)
           (let* (
                  (w1 (elt (window-list) i))
                  (w2 (elt (window-list) (+ (% i numWindows) 1)))

                  (b1 (window-buffer w1))
                  (b2 (window-buffer w2))

                  (s1 (window-start w1))
                  (s2 (window-start w2))
                  )
             (set-window-buffer w1  b2)
             (set-window-buffer w2 b1)
             (set-window-start w1 s2)
             (set-window-start w2 s1)
             (setq i (1+ i)))))))

(defun eval-and-replace ()
  "Replace the preceding sexp with its value."
  (interactive)
  (backward-kill-sexp)
  (condition-case nil
      (prin1 (eval (read (current-kill 0)))
             (current-buffer))
    (error (message "Invalid expression")
           (insert (current-kill 0)))))

(defun duplicate-region (&optional num start end)
  "Duplicates the region bounded by START and END NUM times.
If no START and END is provided, the current region-beginning and
region-end is used."
  (interactive "p")
  (save-excursion
   (let* ((start (or start (region-beginning)))
          (end (or end (region-end)))
          (region (buffer-substring start end)))
     (goto-char end)
     (dotimes (i num)
       (insert region)))))

(defun duplicate-current-line (&optional num)
  "Duplicate the current line NUM times."
  (interactive "p")
  (save-excursion
   (when (eq (point-at-eol) (point-max))
     (goto-char (point-max))
     (newline)
     (forward-char -1))
   (duplicate-region num (point-at-bol) (1+ (point-at-eol)))))

(defun move-forward-out-of-param ()
  (while (not (looking-at ")\\|, \\| ?}\\| ?\\]"))
    (cond
     ((point-is-in-string-p) (move-point-forward-out-of-string))
     ((looking-at "(\\|{\\|\\[") (forward-list))
     (t (forward-char)))))

(defun move-backward-out-of-param ()
  (while (not (looking-back "(\\|, \\|{ ?\\|\\[ ?"))
    (cond
     ((point-is-in-string-p) (move-point-backward-out-of-string))
     ((looking-back ")\\|}\\|\\]") (backward-list))
     (t (backward-char)))))

(defun transpose-params ()
  "Presumes that params are in the form (p, p, p) or {p, p, p} or [p, p, p]"
  (interactive)
  (let* ((end-of-first (cond
                        ((looking-at ", ") (point))
                        ((and (looking-back ",") (looking-at " ")) (- (point) 1))
                        ((looking-back ", ") (- (point) 2))
                        (t (error "Place point between params to transpose."))))
         (start-of-first (save-excursion
                           (goto-char end-of-first)
                           (move-backward-out-of-param)
                           (point)))
         (start-of-last (+ end-of-first 2))
         (end-of-last (save-excursion
                        (goto-char start-of-last)
                        (move-forward-out-of-param)
                        (point))))
    (transpose-regions start-of-first end-of-first start-of-last end-of-last)))

(defun css-expand-statement ()
  (interactive)
  (save-excursion
    (end-of-line)
    (search-backward "{")
    (forward-char 1)
    (let ((beg (point)))
      (newline)
      (er/mark-inside-pairs)
      (replace-regexp ";" ";\n" nil (region-beginning) (region-end))
      (indent-region beg (point)))))

(defun css-contract-statement ()
  (interactive)
  (end-of-line)
  (search-backward "{")
  (while (not (looking-at "}"))
    (join-line -1))
  (back-to-indentation))

(defun comment-move-before-line ()
  "Move an inline comment to a line of its own before the current line."
  (interactive)
  (save-excursion
    (let ((old-kill-ring kill-ring))
      (comment-kill 1)
      (move-end-of-line -1)
      (newline-and-indent)
      (yank)
      (comment-indent)
      (setq kill-ring old-kill-ring))))

(defun align-around-point-to-above ()
  "Adjust whitespace around point so that the following word is aligned
as in the line above."
  (interactive)
  (let ((col (save-excursion
               (previous-line)
               (skip-chars-forward " ")
               (current-column))))
    (save-excursion
      (skip-chars-forward " ")
      (while (> (current-column) col)
        (delete-backward-char 1))
      (while (< (current-column) col)
        (insert " ")))))

(defun set-or-deactivate-rectangular-region-anchor ()
  "Set rectangular region or exit it."
  (interactive)
  (if (or rectangular-region-mode multiple-cursors-mode)
      (rrm/keyboard-quit)
    (set-rectangular-region-anchor)))

(defun maybe-comment-indent-new-line ()
  "Continue comment, if comment is more than two lines long already."
  (interactive)
  (if (and (save-excursion (comment-beginning))
           (save-excursion
             (forward-line -1)
             (end-of-line)
             (comment-beginning)))
      (comment-indent-new-line)
    (newline-and-indent)))

(defun calc-eval-region (arg)
  "Evaluate an expression in calc and communicate the result.

If the region is active evaluate that, otherwise search backwards
to either a matching parenthesis or the first whitespace
character to find the beginning of the expression.

By default, replace the expression with its value. If called with
the universal prefix argument, keep the expression and insert the
result into the buffer after it. If called with a negative prefix
argument, just echo the result in the minibuffer."
  (interactive "p")
  (let (start end)
    (if (use-region-p)
        (setq start (region-beginning) end (region-end))
      (save-excursion
        (setq end (point))
        (if (looking-back ")" 1)
            (backward-sexp)
          (search-backward-regexp "\\s-\\|\n" 0 1)
          (goto-char (1+ (point))))
        (setq start (point))))
    (let ((value (calc-eval (buffer-substring-no-properties start end))))
      (pcase arg
        (1 (delete-region start end))
        (4 (insert " = ")))
      (pcase arg
        ((or 1 4) (insert value))
        (-1 (message value))))))

(defun un-camelcase-word-at-point ()
  "Un-camelcase the word at point, replacing uppercase chars with
the lowercase version preceded by an underscore.

The first char, if capitalized (eg, PascalCase) is just
downcased, no preceding underscore."
  (interactive)
  (save-excursion
    (let ((bounds (bounds-of-thing-at-point 'word)))
      (replace-regexp "\\([A-Z]\\)" "_\\1" nil
                      (1+ (car bounds)) (cdr bounds))
      (downcase-region (car bounds) (cdr bounds)))))

(defun kill-this-symbol ()
  "Kill the whole symbol at point."
  (interactive)
  (let ((bounds (bounds-of-thing-at-point 'symbol)))
    (kill-region (car bounds) (cdr bounds))))

(defun wrap-line-in-parens ()
  "Wrap the rest of the line (minus separators) in parens."
  (interactive)
  (insert "(")
  (let ((beg (point)))
    (save-excursion
      (end-of-line)
      (when (looking-back "[;,]" nil)
        (backward-char))
      (insert ")"))))
