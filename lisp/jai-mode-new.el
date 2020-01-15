;; jai-mode.el --- Major mode for the Jai programming language
;; Maintainer:	Craig Giles
;; License:	This file is placed in the public domain.

(require 'cl-lib)
(require 'compile)
(require 'scala-mode-indent)
(require 'scala-mode-map)

(defvar jai-mode-syntax-table
  (let ((table (make-syntax-table)))
    (modify-syntax-entry ?\" "\"" table)
    (modify-syntax-entry ?\' "\"" table)
    (modify-syntax-entry ?`  "\"" table)
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
  '("if" "else" "then" "while" "for" "switch" "case" "struct" "enum"
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
(defconst jai-type-regexp "\\(.*\\):\\(.*\\)")
(defconst jai-undef-regexp "\\(---\\)")
(defconst jai-directive-regexp "\\([#@]\\w+\\)")
(defconst go-identifier-regexp "[[:word:][:multibyte:]]+")
(defconst go-label-regexp go-identifier-regexp)
;; (defconst jai-parameter-type-regexp ":\\( *[ *$\\)]\\w+\\)")

;; TODO so many bugs... This doesn't match array syntax (either [] or
;;   [..]) and has issues with various pointer types and default
;;   parameters as well.
(defconst jai-parameter-type-regexp ": *\\([\$*]?\\w+\\)") ;; matches T, $T, or *T 

(defun jai-wrap-word-rx (s)
  (concat "\\<" s "\\>"))

(defun jai-keywords-rx (keywords)
  "build keyword regexp"
  (jai-wrap-word-rx (regexp-opt keywords t)))

(defconst jai-pointer-type-rx (rx (group (and "*" (1+ word)))))
(defconst jai-dollar-type-rx (rx (group "$" (or (1+ word) (opt "$")))))

(defun jai--build-font-lock-keywords ()

  (append
   `(
      (,(concat "\\_<" (regexp-opt jai-mode-keywords t) "\\_>") . font-lock-keyword-face)
      (,(concat "\\(\\_<" (regexp-opt jai-builtins t) "\\_>\\)[[:space:]]*(") 1 font-lock-builtin-face)
      (,(concat "\\_<" (regexp-opt jai-constants t) "\\_>") . font-lock-constant-face)
      (,jai-func-regexp 1 font-lock-function-name-face)
      (,jai-undef-regexp 1 font-lock-constant-face)
      (,jai-directive-regexp 1 font-lock-preprocessor-face)
      (,jai-parameter-type-regexp 1 font-lock-preprocessor-face)

      ;; Types
      (,(jai-keywords-rx jai-typenames) 1 font-lock-type-face)
      (,jai-pointer-type-rx 1 font-lock-type-face)
      (,jai-dollar-type-rx 1 font-lock-type-face)
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
	    ("struct" "^\\(.*\\) :: \\(struct\\)\\(.*\\){" 1)
	    )
        )
      )
)

(defun scala-mode-map:add-self-insert-hooks ()
  (add-hook 'post-self-insert-hook
            'scala-indent:indent-on-parentheses)
  (add-hook 'post-self-insert-hook
            'scala-indent:indent-on-special-words)
  (add-hook 'post-self-insert-hook
            'scala-indent:indent-on-scaladoc-asterisk)
  (add-hook 'post-self-insert-hook
            'scala-indent:fix-scaladoc-close))

;;;###autoload
(define-derived-mode jai-mode prog-mode "Jai"
  "Major mode for editing Jai source text

This mode provides the basics.. VERY basics for editing Jai code."

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

  ;; Indentation
  ;; (set (make-local-variable 'indent-line-function) #'js-indent-line)
  (set (make-local-variable 'indent-line-function) #'scala-indent:indent-line)

  ;; add indent functionality to some characters
  (scala-mode-map:add-remove-indent-hook)
  (scala-mode-map:add-self-insert-hooks)

 ;; :syntax-table jai-mode-syntax-table
 ;; :group 'jai
 ;; (setq bidi-paragraph-direction 'left-to-right)
 ;; (setq-local require-final-newline mode-require-final-newline)
 ;; (setq-local parse-sexp-ignore-comments t)
 ;; (setq-local comment-start-skip "\\(//+\\|/\\*+\\)\\s *")
 ;; (setq-local comment-start "/*")
 ;; (setq-local comment-end "*/")
 ;; (setq-local indent-line-function 'js-indent-line)
 ;; (setq-local font-lock-defaults '(jai-font-lock-defaults))
 ;; (setq-local beginning-of-defun-function 'jai-beginning-of-defun)
 ;; (setq-local end-of-defun-function 'jai-end-of-defun)

  (font-lock-fontify-buffer)
)

;;;###autoload
(add-to-list 'auto-mode-alist (cons "\\.jai\\'" 'jai-mode))

(provide 'jai-mode)
