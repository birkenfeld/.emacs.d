;;; haskell-font-lock.el --- Font locking module for Haskell Mode -*- lexical-binding: t -*-

;; Copyright 2003, 2004, 2005, 2006, 2007, 2008  Free Software Foundation, Inc.
;; Copyright 1997-1998  Graeme E Moss, and Tommy Thorn

;; Author: 1997-1998 Graeme E Moss <gem@cs.york.ac.uk>
;;         1997-1998 Tommy Thorn <thorn@irisa.fr>
;;         2003      Dave Love <fx@gnu.org>
;; Keywords: faces files Haskell

;; This file is not part of GNU Emacs.

;; This file is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.

;; This file is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.


;;; Code:

(require 'cl-lib)
(require 'haskell-compat)
(require 'haskell-lexeme)
(require 'font-lock)

;;;###autoload
(defgroup haskell-appearance nil
  "Haskell Appearance."
  :group 'haskell)


(defcustom haskell-font-lock-symbols nil
  "Display \\ and -> and such using symbols in fonts.

This may sound like a neat trick, but be extra careful: it changes the
alignment and can thus lead to nasty surprises with regards to layout."
  :group 'haskell-appearance
  :type 'boolean)

(defcustom haskell-font-lock-symbols-alist
  '(("\\" . "λ")
    ("not" . "¬")
    ("->" . "→")
    ("<-" . "←")
    ("=>" . "⇒")
    ("()" . "∅")
    ("==" . "≡")
    ("/=" . "≢")
    (">=" . "≥")
    ("<=" . "≤")
    ("!!" . "‼")
    ("&&" . "∧")
    ("||" . "∨")
    ("sqrt" . "√")
    ("undefined" . "⊥")
    ("pi" . "π")
    ("~>" . "⇝") ;; Omega language
    ;; ("~>" "↝") ;; less desirable
    ("-<" . "↢") ;; Paterson's arrow syntax
    ;; ("-<" "⤙") ;; nicer but uncommon
    ("::" . "∷")
    ("." "∘" ; "○"
     ;; Need a predicate here to distinguish the . used by
     ;; forall <foo> . <bar>.
     haskell-font-lock-dot-is-not-composition)
    ("forall" . "∀"))
  "Alist mapping Haskell symbols to chars.

Each element has the form (STRING . COMPONENTS) or (STRING
COMPONENTS PREDICATE).

STRING is the Haskell symbol.
COMPONENTS is a representation specification suitable as an argument to
`compose-region'.
PREDICATE if present is a function of one argument (the start position
of the symbol) which should return non-nil if this mapping should
be disabled at that position."
  :type '(alist string string)
  :group 'haskell-appearance)

(defun haskell-font-lock-dot-is-not-composition (start)
  "Return non-nil if the \".\" at START is not a composition operator.
This is the case if the \".\" is part of a \"forall <tvar> . <type>\"."
  (save-excursion
    (goto-char start)
    (or (re-search-backward "\\<forall\\>[^.\"]*\\="
                            (line-beginning-position) t)
        (not (or
              (string= " " (string (char-after start)))
              (string= " " (string (char-before start))))))))

(defcustom haskell-font-lock-quasi-quote-modes
  `(("hsx" . xml-mode)
    ("hamlet" . xml-mode)
    ("shamlet" . xml-mode)
    ("xmlQQ" . xml-mode)
    ("xml" . xml-mode)
    ("cmd" . shell-mode)
    ("sh_" . shell-mode)
    ("jmacro" . javascript-mode)
    ("jmacroE" . javascript-mode)
    ("r" . ess-mode)
    ("rChan" . ess-mode)
    ("sql" . sql-mode))
  "Mapping from quasi quoter token to fontification mode.

If a quasi quote is seen in Haskell code its contents will have
font faces assigned as if respective mode was enabled."
  :group 'haskell-appearance
  :type '(repeat (cons string symbol)))

;;;###autoload
(defface haskell-keyword-face
  '((t :inherit font-lock-keyword-face))
  "Face used to highlight Haskell keywords."
  :group 'haskell-appearance)

;;;###autoload
(defface haskell-constructor-face
  '((t :inherit font-lock-type-face))
  "Face used to highlight Haskell constructors."
  :group 'haskell-appearance)

;; This used to be `font-lock-variable-name-face' but it doesn't result in
;; a highlighting that's consistent with other modes (it's mostly used
;; for function defintions).
(defface haskell-definition-face
  '((t :inherit font-lock-function-name-face))
  "Face used to highlight Haskell definitions."
  :group 'haskell-appearance)

;; This is probably just wrong, but it used to use
;; `font-lock-function-name-face' with a result that was not consistent with
;; other major modes, so I just exchanged with `haskell-definition-face'.
;;;###autoload
(defface haskell-operator-face
  '((t :inherit font-lock-variable-name-face))
  "Face used to highlight Haskell operators."
  :group 'haskell-appearance)

;;;###autoload
(defface haskell-pragma-face
  '((t :inherit font-lock-preprocessor-face))
  "Face used to highlight Haskell pragmas."
  :group 'haskell-appearance)

;;;###autoload
(defface haskell-literate-comment-face
  '((t :inherit font-lock-doc-face))
  "Face with which to fontify literate comments.
Inherit from `default' to avoid fontification of them."
  :group 'haskell-appearance)

(defface haskell-quasi-quote-face
  '((t :inherit font-lock-string-face))
  "Generic face for quasiquotes.

Some quote types are fontified according to other mode defined in
`haskell-font-lock-quasi-quote-modes'."
  :group 'haskell-appearance)

(defun haskell-font-lock-compose-symbol (alist)
  "Compose a sequence of ascii chars into a symbol.
Regexp match data 0 points to the chars."
  ;; Check that the chars should really be composed into a symbol.
  (let* ((start (match-beginning 0))
         (end (match-end 0))
         (syntaxes (cond
                    ((eq (char-syntax (char-after start)) ?w) '(?w))
                    ((eq (char-syntax (char-after start)) ?.) '(?.))
                    ;; Special case for the . used for qualified names.
                    ((and (eq (char-after start) ?\.) (= end (1+ start)))
                     '(?_ ?\\ ?w))
                    (t '(?_ ?\\))))
         sym-data)
    (if (or (memq (char-syntax (or (char-before start) ?\ )) syntaxes)
            (memq (char-syntax (or (char-after end) ?\ )) syntaxes)
            (or (elt (syntax-ppss) 3) (elt (syntax-ppss) 4))
            (and (consp (setq sym-data (cdr (assoc (match-string 0) alist))))
                 (let ((pred (cadr sym-data)))
                   (setq sym-data (car sym-data))
                   (funcall pred start))))
        ;; No composition for you.  Let's actually remove any composition
        ;; we may have added earlier and which is now incorrect.
        (remove-text-properties start end '(composition))
      ;; That's a symbol alright, so add the composition.
      (compose-region start end sym-data)))
  ;; Return nil because we're not adding any face property.
  nil)

(defun haskell-font-lock-symbols-keywords ()
  (when (and haskell-font-lock-symbols
	     haskell-font-lock-symbols-alist)
    `((,(regexp-opt (mapcar 'car haskell-font-lock-symbols-alist) t)
       (0 (haskell-font-lock-compose-symbol ',haskell-font-lock-symbols-alist)
	  ;; In Emacs-21, if the `override' field is nil, the face
	  ;; expressions is only evaluated if the text has currently
	  ;; no face.  So force evaluation by using `keep'.
	  keep)))))

(defun haskell-font-lock-keywords ()
  ;; this has to be a function because it depends on global value of
  ;; `haskell-font-lock-symbols'
  "Generate font lock eywords."
  (let* (;; Bird-style literate scripts start a line of code with
         ;; "^>", otherwise a line of code starts with "^".
         (line-prefix "^\\(?:> ?\\)?")

         (varid "\\b[[:lower:]_][[:alnum:]'_]*\\b")
         ;; We allow ' preceding conids because of DataKinds/PolyKinds
         (conid "\\b'?[[:upper:]][[:alnum:]'_]*\\b")
         (sym "\\s.+")
         (reservedids
          ;; `as', `hiding', and `qualified' are part of the import
          ;; spec syntax, but they are not reserved.
          ;; `_' can go in here since it has temporary word syntax.
          '("case" "class" "data" "default" "deriving" "do"
            "else" "if" "import" "in" "infix" "infixl"
            "infixr" "instance" "let" "module" "mdo" "newtype" "of"
            "rec" "proc" "then" "type" "where" "_"))

         ;; Top-level declarations
         (topdecl-var
          (concat line-prefix "\\(" varid "\\(?:\\s-*,\\s-*" varid "\\)*" "\\)\\s-*"
                  ;; optionally allow for a single newline after identifier
                  "\\([\n]\\s-+\\)?"
                  ;; A toplevel declaration can be followed by a definition
                  ;; (=), a type (::) or (∷), a guard, or a pattern which can
                  ;; either be a variable, a constructor, a parenthesized
                  ;; thingy, or an integer or a string.
                  "\\(" varid "\\|" conid "\\|::\\|∷\\|=\\||\\|\\s(\\|[0-9\"']\\)"))
         (topdecl-var2
          (concat line-prefix "\\(" varid "\\|" conid "\\)\\s-*`\\(" varid "\\)`"))
         (topdecl-bangpat
          (concat line-prefix "\\(" varid "\\)\\s-*!"))
         (topdecl-sym
          (concat line-prefix "\\(" varid "\\|" conid "\\)\\s-*\\(" sym "\\)"))
         (topdecl-sym2 (concat line-prefix "(\\(" sym "\\))"))

         keywords)

    (setq keywords
          `(;; NOTICE the ordering below is significant
            ;;
            ("^#.*$" 0 'font-lock-preprocessor-face t)

            ,@(haskell-font-lock-symbols-keywords)

            ;; Special case for `as', `hiding', `safe' and `qualified', which are
            ;; keywords in import statements but are not otherwise reserved.
            ("\\<import[ \t]+\\(?:\\(safe\\>\\)[ \t]*\\)?\\(?:\\(qualified\\>\\)[ \t]*\\)?\\(?:\"[^\"]*\"[\t ]*\\)?[^ \t\n()]+[ \t]*\\(?:\\(\\<as\\>\\)[ \t]*[^ \t\n()]+[ \t]*\\)?\\(\\<hiding\\>\\)?"
             (1 'haskell-keyword-face nil lax)
             (2 'haskell-keyword-face nil lax)
             (3 'haskell-keyword-face nil lax)
             (4 'haskell-keyword-face nil lax))

            ;; Special case for `foreign import'
            ;; keywords in foreign import statements but are not otherwise reserved.
            ("\\<\\(foreign\\)[ \t]+\\(import\\)[ \t]+\\(?:\\(ccall\\|stdcall\\|cplusplus\\|jvm\\|dotnet\\)[ \t]+\\)?\\(?:\\(safe\\|unsafe\\|interruptible\\)[ \t]+\\)?"
             (1 'haskell-keyword-face nil lax)
             (2 'haskell-keyword-face nil lax)
             (3 'haskell-keyword-face nil lax)
             (4 'haskell-keyword-face nil lax))

            ;; Special case for `foreign export'
            ;; keywords in foreign export statements but are not otherwise reserved.
            ("\\<\\(foreign\\)[ \t]+\\(export\\)[ \t]+\\(?:\\(ccall\\|stdcall\\|cplusplus\\|jvm\\|dotnet\\)[ \t]+\\)?"
             (1 'haskell-keyword-face nil lax)
             (2 'haskell-keyword-face nil lax)
             (3 'haskell-keyword-face nil lax))

            ;; Special case for `type family' and `data family'.
            ;; `family' is only reserved in these contexts.
            ("\\<\\(type\\|data\\)[ \t]+\\(family\\>\\)"
             (1 'haskell-keyword-face nil lax)
             (2 'haskell-keyword-face nil lax))

            ;; Special case for `type role'
            ;; `role' is only reserved in this context.
            ("\\<\\(type\\)[ \t]+\\(role\\>\\)"
             (1 'haskell-keyword-face nil lax)
             (2 'haskell-keyword-face nil lax))

            ;; Toplevel Declarations.
            ;; Place them *before* generic id-and-op highlighting.
            (,topdecl-var  (1 (unless (member (match-string 1) ',reservedids)
                                'haskell-definition-face)))
            (,topdecl-var2 (2 (unless (member (match-string 2) ',reservedids)
                                'haskell-definition-face)))
            (,topdecl-bangpat  (1 (unless (member (match-string 1) ',reservedids)
                                'haskell-definition-face)))
            (,topdecl-sym  (2 (unless (member (match-string 2) '("\\" "=" "->" "→" "<-" "←" "::" "∷" "," ";" "`"))
                                'haskell-definition-face)))
            (,topdecl-sym2 (1 (unless (member (match-string 1) '("\\" "=" "->" "→" "<-" "←" "::" "∷" "," ";" "`"))
                                'haskell-definition-face)))

            ;; These four are debatable...
            ("(\\(,*\\|->\\))" 0 'haskell-constructor-face)
            ("\\[\\]" 0 'haskell-constructor-face)

            (,(concat "`" haskell-lexeme-qid-or-qsym "`") 0 'haskell-operator-face)

            (,haskell-lexeme-qid-or-qsym
             0 (cl-case (haskell-lexeme-classify-by-first-char (char-after (match-beginning 1)))
                 (varid (when (member (match-string 0) ',reservedids)
                          ;; Note: keywords parse as keywords only when not qualified.
                          ;; GHC parses Control.let as a single but illegal lexeme.
                          'haskell-keyword-face))
                 (conid 'haskell-constructor-face)
                 (varsym (when (and (not (member (match-string 0) '("-" "+" ".")))
                                      (not (save-excursion
                                             (goto-char (match-beginning 1))
                                             (looking-at-p "\\sw"))))
                             ;; We need to protect against the case of
                             ;; plus, minus or dot inside a floating
                             ;; point number.
                             'haskell-operator-face))
                 (consym (if (not (member (match-string 1) '("::" "∷")))
                             'haskell-constructor-face
                           'haskell-operator-face))))))
    keywords))


(defun haskell-font-lock-fontify-block (lang-mode start end)
  "Fontify a block as LANG-MODE."
  (let ((string (buffer-substring-no-properties start end))
        (modified (buffer-modified-p))
        (org-buffer (current-buffer)) pos next)
    (remove-text-properties start end '(face nil))
    (with-current-buffer
        (get-buffer-create
         (concat " haskell-font-lock-fontify-block:" (symbol-name lang-mode)))
      (delete-region (point-min) (point-max))
      (insert string " ") ;; so there's a final property change
      (unless (eq major-mode lang-mode) (funcall lang-mode))
      (font-lock-ensure)
      (setq pos (point-min))
      (while (setq next (next-single-property-change pos 'face))
        (put-text-property
         (+ start (1- pos)) (1- (+ start next)) 'face
         (get-text-property pos 'face) org-buffer)
        (setq pos next)))
    (add-text-properties
     start end
     '(font-lock-fontified t fontified t font-lock-multiline t))
    (set-buffer-modified-p modified)))

(defun haskell-syntactic-face-function (state)
  "`font-lock-syntactic-face-function' for Haskell."
  (cond
   ((nth 3 state)
    (if (equal ?| (nth 3 state))
        ;; find out what kind of QuasiQuote is this
        (let* ((qqname (save-excursion
                        (goto-char (nth 8 state))
                        (skip-syntax-backward "w._")
                        (buffer-substring-no-properties (point) (nth 8 state))))
               (lang-mode (cdr (assoc qqname haskell-font-lock-quasi-quote-modes))))

          (if (and lang-mode
                   (fboundp lang-mode))
              (save-excursion
                ;; find the end of the QuasiQuote
                (parse-partial-sexp (point) (point-max) nil nil state
                                    'syntax-table)
                (haskell-font-lock-fontify-block lang-mode (1+ (nth 8 state)) (1- (point)))
                ;; must return nil here so that it is not fontified again as string
                nil)
            ;; fontify normally as string because lang-mode is not present
            'haskell-quasi-quote-face))
      (save-excursion
        (let
            ((state2
              (parse-partial-sexp (point) (point-max) nil nil state
                                  'syntax-table))
             (end-of-string (point)))

          (put-text-property (nth 8 state) (point)
                             'face 'font-lock-string-face)


          (if (or (equal t (nth 3 state)) (nth 3 state2))
              ;; This is an unterminated string constant, use warning
              ;; face for the opening quote.
              (put-text-property (nth 8 state) (1+ (nth 8 state))
                                 'face 'font-lock-warning-face))

          (goto-char (1+ (nth 8 state)))
          (while (re-search-forward "\\\\" end-of-string t)

            (goto-char (1- (point)))

            (if (looking-at haskell-lexeme-string-literal-inside-item)
                (goto-char (match-end 0))

              ;; We are looking at an unacceptable escape
              ;; sequence. Use warning face to highlight that.
              (put-text-property (point) (1+ (point))
                                 'face 'font-lock-warning-face)
              (goto-char (1+ (point)))))))
      ;; must return nil here so that it is not fontified again as string
      nil))
   ;; Detect literate comment lines starting with syntax class '<'
   ((save-excursion
      (goto-char (nth 8 state))
      (equal (string-to-syntax "<") (syntax-after (point))))
    'haskell-literate-comment-face)
   ;; Detect pragmas. A pragma is enclosed in special comment
   ;; delimeters {-# .. #-}.
   ((save-excursion
      (goto-char (nth 8 state))
      (and (looking-at-p "{-#")
           (forward-comment 1)
           (goto-char (- (point) 3))
           (looking-at-p "#-}")))
    'haskell-pragma-face)
   ;; Haddock comment start with either "-- [|^*$]" or "{- ?[|^*$]"
   ;; (note space optional for nested comments and mandatory for
   ;; double dash comments).
   ;;
   ;; Haddock comment will also continue on next line, provided:
   ;; - current line is a double dash haddock comment
   ;; - next line is also double dash comment
   ;; - there is only whitespace between
   ;;
   ;; We recognize double dash haddock comments by property
   ;; 'font-lock-doc-face attached to newline. In case of bounded
   ;; comments newline is outside of comment.
   ((save-excursion
      (goto-char (nth 8 state))
      (or (looking-at-p "\\(?:{- ?\\|-- \\)[|^*$]")
	  (and (looking-at-p "--")              ; are we at double dash comment
	       (forward-line -1)              ; this is nil on first line
	       (eq (get-text-property (line-end-position) 'face)
		   'font-lock-doc-face)	      ; is a doc face
	       (forward-line)
	       (skip-syntax-forward "-")      ; see if there is only whitespace
	       (eq (point) (nth 8 state)))))  ; we are back in position
    'font-lock-doc-face)
   (t 'font-lock-comment-face)))

(defun haskell-font-lock-defaults-create ()
  "Locally set `font-lock-defaults' for Haskell."
  (set (make-local-variable 'font-lock-defaults)
       '((haskell-font-lock-keywords)
         nil nil nil nil
         (font-lock-syntactic-face-function
          . haskell-syntactic-face-function)
         ;; Get help from font-lock-syntactic-keywords.
         (parse-sexp-lookup-properties . t)
         (font-lock-extra-managed-props . (composition)))))

(defun haskell-fontify-as-mode (text mode)
  "Fontify TEXT as MODE, returning the fontified text."
  (with-temp-buffer
    (funcall mode)
    (insert text)
    (if (fboundp 'font-lock-ensure)
        (font-lock-ensure)
      (with-no-warnings (font-lock-fontify-buffer)))
    (buffer-substring (point-min) (point-max))))

;; Provide ourselves:

(provide 'haskell-font-lock)

;; Local Variables:
;; coding: utf-8-unix
;; tab-width: 8
;; End:

;;; haskell-font-lock.el ends here