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
  (let* ((fname (ffap-prompter))
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

(defun forward-char-but-not-eol ()
  (interactive)
  (if (not (looking-at "\n")) (forward-char)))

(defvar current-this-regex "")
(defun search-for-this-word ()
  "Emulate Vim's `*' binding."
  (interactive)
  (let ((tag (find-tag-default)))
    (if tag (setq new-this-regex
                  (concat "\\<" (regexp-quote tag) "\\>"))
      (error "point not over tag")))
  (unless (string-equal new-this-regex current-this-regex)
    (font-lock-remove-keywords
     nil (list (list current-this-regex 0 'lazy-highlight-face t)))
    (font-lock-add-keywords
     nil (list (list new-this-regex 0 'lazy-highlight-face t)))
    (setq current-this-regex new-this-regex)
    (font-lock-fontify-buffer)
    (message (concat "Searching for " (substring new-this-regex 2 -2))))
  (unless (search-forward-regexp current-this-regex nil t
                                 (if (looking-at "\\<") 2 1))
    (beginning-of-buffer)
    (message "search hit BOTTOM, continuing at TOP")
    (search-forward-regexp current-this-regex))
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

(require 'grep)
(defun grep (regexp &optional files)
  "Always grep from the current directory."
  (interactive
   (progn
     (grep-compute-defaults)
     (let* ((regexp (grep-read-regexp))
            (files (grep-read-files regexp)))
       (list regexp files))))
  (rgrep regexp files default-directory))

(defalias 'wgrep 'wgrep-change-to-wgrep-mode)

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
          (ignore-errors (make-directory new-name t))
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
