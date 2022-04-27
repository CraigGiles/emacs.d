;; jai-mode.el ---- Major mode for the Jai programming language
;; Original Author: Kristoffer Grönlund - https://github.com/krig
;; Updated By:      Craig Giles - https://github.com/craiggiles
;; License:         This file is placed in the public domain.

(require 'cl-lib)
(require 'compile)

;; TODO the only reason this is being imported is because of its
;;   indentation rules. Re-write to use custom indent rules when the
;;   language becomes a little more defined
(require 'js)

(defconst jai-mode-syntax-table
  (let ((table (make-syntax-table)))
    (modify-syntax-entry ?\" "\"" table)
    (modify-syntax-entry ?\\ "\\" table)

    ;; additional symbols
    (modify-syntax-entry ?_ "w" table)

    (modify-syntax-entry ?' "." table)
    (modify-syntax-entry ?: "." table)
    (modify-syntax-entry ?+  "." table)
    (modify-syntax-entry ?-  "." table)
    (modify-syntax-entry ?%  "." table)
    (modify-syntax-entry ?&  "." table)
    (modify-syntax-entry ?|  "." table)
    (modify-syntax-entry ?^  "." table)
    (modify-syntax-entry ?!  "." table)
    (modify-syntax-entry ?$  "/" table)
    (modify-syntax-entry ?=  "." table)
    (modify-syntax-entry ?<  "." table)
    (modify-syntax-entry ?>  "." table)
    (modify-syntax-entry ??  "." table)

    ;; Modify some syntax entries to allow nested block comments
    (modify-syntax-entry ?/ ". 124b" table)
    (modify-syntax-entry ?* ". 23n" table)
    (modify-syntax-entry ?\n "> b" table)
    (modify-syntax-entry ?\^m "> b" table)

    table)
  "Syntax table for Jai mode.")

;; TODO should 'it' be a builtin or keyword?
(defconst jai-builtins
  '("cast" "type_info" "size_of")
  "Built-in functions and types for the Jai language. Used for font locking.")

(defconst jai-mode-keywords
  '("if" "ifx" "else" "then" "while" "for" "switch" "case" "struct" "enum"
    "return" "new" "remove" "continue" "break" "defer" "inline" "no_inline"
    "using" "SOA" "it")
  "Built in keywords for the jai language.  Used for font locking.")

(defconst jai-constants
  '("\\-\\-\\-" "null" "true" "false"))

(defconst jai-typenames
  '("int" "u64" "u32" "u16" "u8"
    "s64" "s32" "s16" "s8" "float"
    "float32" "float64" "string"
    "bool"))

(defconst jai-func-regexp "^\\(.*\\)::")
(defconst jai-type-regexp "\\(.*\\):\\ *")
(defconst jai-undef-regexp "\\(---\\)")
(defconst jai-directive-regexp "\\([#@]\\w+\\)")

(defconst jai-number-regexp
  (rx (and
       symbol-start
       (or (and (+ digit) (opt (and (any "eE") (opt (any "-+")) (+ digit))))
           (and "0" (any "xX") (+ hex-digit)))
       (opt (and (any "_" "A-Z" "a-z") (* (any "_" "A-Z" "a-z" "0-9"))))
       symbol-end)))

;; TODO so many bugs... This doesn't match array syntax (either [] or
;;   [..]) and has issues with various pointer types and default
;;   parameters as well.
(defconst jai-parameter-type-regexp ": *\\([\$*]?\\w+\\)") ;; matches T, $T, or *T 

(defconst jai-pointer-type-regexp (rx (group (and "*" (1+ word)))))
(defconst jai-dollar-type-regexp (rx (group "$" (or (1+ word) (opt "$")))))

(defun jai--build-font-lock-keywords ()
  (append
   `(
      (,(concat "\\_<" (regexp-opt jai-mode-keywords t) "\\_>") . font-lock-keyword-face)
      (,(concat "\\(\\_<" (regexp-opt jai-builtins t) "\\_>\\)[[:space:]]*(") 1 font-lock-builtin-face)
      (,(concat "\\_<" (regexp-opt jai-constants t) "\\_>") . font-lock-constant-face)

      (,jai-func-regexp 1 font-lock-function-name-face)
      (,jai-undef-regexp 1 font-lock-constant-face)
      (,jai-directive-regexp 1 font-lock-preprocessor-face)

      ;; Types
      (,(concat "\\_<" (regexp-opt jai-typenames t) "\\_>") . font-lock-type-face)
      (,jai-parameter-type-regexp 1 font-lock-type-face)
      (,jai-pointer-type-regexp 1 font-lock-type-face)
      (,jai-dollar-type-regexp 1 font-lock-type-face)
      (,jai-type-regexp 1 font-lock-type-face)
      (,(concat "\\<" jai-number-regexp "\\>" ) . font-lock-constant-face)
    )
  )
)

(defvar jai-mode-map
  (let ((m (make-sparse-keymap)))
    (unless (boundp 'electric-indent-chars)
        (define-key m "}" #'jai-mode-insert-and-indent)
        (define-key m ")" #'jai-mode-insert-and-indent))
    m)
  "Keymap used by ‘jai-mode’.")

(defun jai-mode-insert-and-indent (key)
  "Invoke the global binding of KEY, then reindent the line."

  (interactive (list (this-command-keys)))
  (call-interactively (lookup-key (current-global-map) key))
  (indent-according-to-mode))

;; imenu hookup
(add-hook 'jai-mode-hook
      (lambda ()
        (setq imenu-generic-expression
          '(
            ("type" "^\\(.*:*.*\\) : " 1)
	    ("function" "^\\(.*\\) :: " 1)
	    ("struct" "^\\(.*\\) *:: *\\(struct\\) *{" 1)
	    )
        )
      )
)

;; NOTE: taken from the scala-indent package and modified for Jai.
;;   Still uses the js-indent-line as a base, which will have to be
;;   replaced when the language is more mature.
(defun jai--indent-on-parentheses ()
  (when (and (= (char-syntax (char-before)) ?\))
             (= (save-excursion (back-to-indentation) (point)) (1- (point))))
    (js-indent-line)))

(defun jai--add-self-insert-hooks ()
  (add-hook 'post-self-insert-hook
            'jai--indent-on-parentheses)
  )

(defconst jai--defun-rx "\(.*\).*\{")

(defmacro jai-paren-level ()
  `(car (syntax-ppss)))

(defun jai-line-is-defun ()
  "return t if current line begins a procedure"
  (interactive)
  (save-excursion
    (beginning-of-line)
    (let (found)
      (while (and (not (eolp)) (not found))
        (if (looking-at jai--defun-rx)
            (setq found t)
          (forward-char 1)))
      found)))

(defun jai-beginning-of-defun (&optional count)
  "Go to line on which current function starts."
  (interactive)
  (let ((orig-level (jai-paren-level)))
    (while (and
            (not (jai-line-is-defun))
            (not (bobp))
            (> orig-level 0))
      (setq orig-level (jai-paren-level))
      (while (>= (jai-paren-level) orig-level)
        (skip-chars-backward "^{")
        (backward-char))))
  (if (jai-line-is-defun)
      (beginning-of-line)))

(defun jai-end-of-defun ()
  "Go to line on which current function ends."
  (interactive)
  (let ((orig-level (jai-paren-level)))
    (when (> orig-level 0)
      (jai-beginning-of-defun)
      (end-of-line)
      (setq orig-level (jai-paren-level))
      (skip-chars-forward "^}")
      (while (>= (jai-paren-level) orig-level)
        (skip-chars-forward "^}")
        (forward-char)))))

;;;###autoload
(define-derived-mode jai-mode prog-mode "Jai"
  "Major mode for editing Jai source text

This mode provides the basics.. VERY basics for editing Jai code."
 :syntax-table jai-mode-syntax-table
 :group 'jai

  ;; Jai style
  (setq indent-tabs-mode nil)
  
  ;; Font lock
  (set (make-local-variable 'font-lock-defaults)
       '(jai--build-font-lock-keywords))

  ;; Comments
  (set (make-local-variable 'comment-start) "// ")
  (set (make-local-variable 'comment-end)   "")
  (set (make-local-variable 'comment-use-syntax) t)
  (set (make-local-variable 'comment-start-skip) "\\(//+\\|/\\*+\\)\\s *")
  (set (make-local-variable 'comment-column) 0)
  (set (make-local-variable 'comment-multi-line) t)

  ;; Indentation
  (set (make-local-variable 'indent-line-function) #'js-indent-line)

  ;; add indent functionality to some characters
  (jai--add-self-insert-hooks)

 (setq bidi-paragraph-direction 'left-to-right)
 (setq-local require-final-newline mode-require-final-newline)
 (setq-local parse-sexp-ignore-comments t)
 (setq-local beginning-of-defun-function 'jai-beginning-of-defun)
 (setq-local end-of-defun-function 'jai-end-of-defun)

  (font-lock-fontify-buffer)
)

;;;###autoload
(add-to-list 'auto-mode-alist (cons "\\.jai\\'" 'jai-mode))

(provide 'jai-mode)
