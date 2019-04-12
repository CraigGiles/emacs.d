;; NOTE: I am not a LISP programmer and I know next to nothing about the EMACS
;; ecosystem and programming environment. Coming from VIM, this could be
;; interesting...

;; ===============================================================
;; Package Management
;; ---------------------------------------------------------------
(require 'package)

;; find packages in melpa, melpa stable, or for org mode, orgmode over elpa (or
;; marmalade for that matter)
(add-to-list 'package-archives '("melpa" . "http://melpa.org/packages/") t)
(add-to-list 'package-archives '("melpa-stable" . "http://stable.melpa.org/packages/") t)
(add-to-list 'package-archives '("org" . "http://orgmode.org/elpa/") t)
(setq package-archive-priorities
      '(("melpa-stable" . 1)
        ("org" . 2)))

(package-initialize)

;; we're going to be using use-package to define and load packages.
;; it's a code based configuration with a bunch of fancy extensions
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

(require 'use-package)
(setq use-package-always-ensure t)

;; ===============================================================
;; Installed Packages
;; ---------------------------------------------------------------
(use-package ido)

(use-package counsel-projectile
  :pin melpa-stable
  :config
  (counsel-mode))

(use-package evil-commentary
  :pin melpa-stable
  :init
  (evil-commentary-mode t))

(use-package evil
  :pin melpa-stable
  :init
  (evil-mode t)

  :config
  (define-key evil-normal-state-map (kbd "j") 'evil-next-visual-line)
  (define-key evil-normal-state-map (kbd "k") 'evil-previous-visual-line)

  (define-key evil-normal-state-map (kbd "-") 'find-file)

  (define-key evil-normal-state-map (kbd "\ep") 'projectile--find-file)
  (define-key evil-normal-state-map (kbd "\eP") 'projectile-find-file-other-window)
 
 
  (define-key evil-normal-state-map (kbd "C-f") 'ag-project-at-point)

  (define-key evil-normal-state-map (kbd "C-u") 'evil-scroll-up)
  (define-key evil-visual-state-map (kbd "C-u") 'evil-scroll-up)
  (define-key evil-insert-state-map (kbd "C-u")
    (lambda ()
      (interactive)
      (evil-delete (point-at-bol) (point))))

  (use-package evil-leader
    :pin melpa-stable
    :config
    (setq evil-leader/in-all-states 1)
    (global-evil-leader-mode)
    (evil-leader/set-leader "SPC")
    (evil-leader/set-key "n" 'evil-search-highlight-persist-remove-all)
    (evil-leader/set-key "SPC" 'other-window))

  (use-package evil-surround
    :pin melpa-stable
    :config
    (global-evil-surround-mode))

  (use-package evil-indent-textobject
    :pin melpa-stable)

  (use-package evil-search-highlight-persist
    :pin melpa-stable
    :config
    (global-evil-search-highlight-persist t))

  (use-package use-package-chords
    :pin melpa-stable
    :config
    (key-chord-mode 1)
    (setq key-chord-two-keys-delay 0.2)
    ;; Exit insert mode with 'jj'
    (key-chord-define evil-insert-state-map "jj" 'evil-normal-state)
    (key-chord-define evil-insert-state-map "Jj" 'evil-normal-state)
    (key-chord-define evil-insert-state-map "JJ" 'evil-normal-state))

  )

(use-package autopair
  :pin melpa-stable
  :config
  (show-paren-mode t))

(use-package smooth-scrolling
  :pin melpa-stable
  :config
  (setq scroll-margin 8
        scroll-conservatively 9999
        scroll-step 1))

(use-package fill-column-indicator
  :pin melpa-stable
  :init
  (setq-default fill-column 80)
  (add-hook 'after-change-major-mode-hook 'fci-mode))

(with-eval-after-load 'evil
  (defalias #'forward-evil-word #'forward-evil-symbol))

(use-package make-mode
  :pin melpa-stable
  :config
  (remove-hook 'before-save-hook 'fix-format-buffer t))

(use-package cc-mode)
(use-package compile)

;; (use-package scala-mode
;;   :pin melpa-stable
;;   :interpreter ("scala" . scala-mode)
;;   :config)

;; (use-package sbt-mode
;;   :pin melpa-stable
;;   :commands sbt-start sbt-command)

(use-package 2048-game)

;; ===============================================================
;; General Editor Settings
;; ---------------------------------------------------------------
(setq emacs-version-osx (string-equal system-type "darwin"))
(setq emacs-version-linux (string-equal system-type "gnu/linux"))
(setq emacs-version-windows (string-equal system-type "windows-nt"))

(setq custom-file "~/.emacs.d/custom.el")
(load custom-file)

;; Put all the backup files in an ~/.emacs.d/backup dir
(setq backup-directory-alist '(("." . "~/.emacs.d/auto-saves")))
(setq create-lockfiles nil)
(setq auto-save-default nil) ;; IMPORTANT(craig) - remove this if it becomes necessary

;; (setq next-line-add-newlines nil)
(setq-default truncate-lines t)
(setq truncate-partial-width-windows nil)

;; Stop Emacs from losing undo information by setting very high limits for undo
;; buffers
(setq undo-limit 20000000)
(setq undo-strong-limit 40000000)

;; Set the OSX's CMD key as the meta key
(setq mac-option-key-is-meta nil)
(setq mac-command-key-is-meta t)
(setq mac-command-modifier 'meta)
(setq mac-option-modifier nil)

; Turn off the bell on Mac OS X
(defun nil-bell ())
(setq ring-bell-function 'nil-bell)

(defun craig-ediff-setup-windows (buffer-A buffer-B buffer-C control-buffer)
  (ediff-setup-windows-plain buffer-A buffer-B buffer-C control-buffer)
)
(setq ediff-window-setup-function 'craig-ediff-setup-windows)
(setq ediff-split-window-function 'split-window-horizontally)

;; Create a big horizontal blue bar so i don't keep loosing my cursor
(global-hl-line-mode 1)

;; ===============================================================
;; Custom Functions
;; ---------------------------------------------------------------
(defun previous-blank-line ()
  "Moves to the previous line containing nothing but whitespace."
  (interactive)
  (search-backward-regexp "^[ \t]*\n")
)

(defun next-blank-line ()
  "Moves to the next line containing nothing but whitespace."
  (interactive)
  (forward-line)
  (search-forward-regexp "^[ \t]*\n")
  (forward-line -1)
)

(defun insert-year ()
  (interactive)
  (insert (format-time-string "%Y")))

;; ===============================================================
;; Keymap
;; ---------------------------------------------------------------
;; (define-key evil-normal-state-map (kbd "C-k") 'previous-blank-line)
;; (define-key evil-normal-state-map (kbd "C-j") 'next-blank-line)
(define-key evil-normal-state-map (kbd "C-k") 'evil-backward-paragraph)
(define-key evil-normal-state-map (kbd "C-j") 'evil-forward-paragraph)

;; (define-key evil-visual-state-map (kbd "C-k") 'previous-blank-line)
;; (define-key evil-visual-state-map (kbd "C-j") 'next-blank-line)
(define-key evil-visual-state-map (kbd "C-k") 'evil-backward-paragraph)
(define-key evil-visual-state-map (kbd "C-j") 'evil-forward-paragraph)

(define-key evil-normal-state-map (kbd "C-e") 'end-of-line)
(define-key evil-normal-state-map (kbd "C-a") 'beginning-of-line)

(define-key evil-normal-state-map (kbd "C-b") 'imenu)

(define-key global-map "\ef" 'find-file)
(define-key global-map "\eF" 'find-file-other-window)

(global-set-key (read-kbd-macro "\eb")  'ido-switch-buffer)
(global-set-key (read-kbd-macro "\eB")  'ido-switch-buffer-other-window)

(define-key global-map "\ej" 'imenu)
(define-key global-map "\ec" 'quick-calc)

;; TODO(craig): Figure out what these need to be
(define-key global-map [f9] 'first-error)
(define-key global-map [f10] 'previous-error)
(define-key global-map [f11] 'next-error)
(define-key global-map "\en" 'next-error)
(define-key global-map "\eN" 'previous-error)

;; Dired: Keymap
;;   - : Move to the parent directory
;;   D : Create directory
;;   % : Create File
(eval-after-load "dired" '(progn
  (define-key dired-mode-map (kbd "M-o") 'other-window) 
  (define-key dired-mode-map "%" 'find-file)
  (define-key dired-mode-map "D" 'dired-create-directory)
  (define-key dired-mode-map "-"
    (lambda ()
      (interactive)
      (find-alternate-file "..")))
  )
)

;; ===============================================================
;; C++ Mode Configuration
;; ---------------------------------------------------------------
(defun save-buffers-without-asking ()
  (interactive)
  (save-some-buffers 'no-confirm (lambda ()
				   (cond
				    ((and buffer-file-name (equal buffer-file-name abbrev-file-name)))
				    ((and buffer-file-name (eq major-mode 'markdown-mode)))
				    ((and buffer-file-name (eq major-mode 'c-mode)))
				    ((and buffer-file-name (eq major-mode 'cc-mode)))
				    ((and buffer-file-name (eq major-mode 'c++-mode)))
				    ((and buffer-file-name (eq major-mode 'scala-mode)))
				    ((and buffer-file-name (eq major-mode 'emacs-lisp-mode)))
				    ((and buffer-file-name (derived-mode-p 'org-mode)))))))

(setq compilation-directory-locked nil)

;; TODO(craig): if osx or linux, build.sh otherwise build.bat
(setq makescript-file "build.sh")

;; Compile Settings
(setq compilation-context-lines 0)
(setq compilation-error-regexp-alist
      (cons '("^\\([0-9]+>\\)?\\(\\(?:[a-zA-Z]:\\)?[^:(\t\n]+\\)(\\([0-9]+\\)) : \\(?:fatal error\\|warnin\\(g\\)\\) C[0-9]+:" 2 3 nil (4))
            compilation-error-regexp-alist))

(defun find-project-directory-recursive ()
  "Recursively search for a makefile."
  (interactive)
  (if (file-exists-p makescript-file) t
    (cd "../")
    (find-project-directory-recursive)))

(defun lock-compilation-directory ()
  "The compilation process should NOT hunt for a makefile"
  (interactive)
  (setq compilation-directory-locked t)
  (message "Compilation directory is locked."))

(defun unlock-compilation-directory ()
  "The compilation process SHOULD hunt for a makefile"
  (interactive)
  (setq compilation-directory-locked nil)
  (message "Compilation directory is roaming."))

(defun find-project-directory ()
  "Find the project directory."
  (interactive)
  (setq find-project-from-directory default-directory)
  (switch-to-buffer-other-window "*compilation*")
  (if compilation-directory-locked (cd last-compilation-directory)
    (cd find-project-from-directory)
    (find-project-directory-recursive)
    (setq last-compilation-directory default-directory)))

(defun make-without-asking ()
  "Make the current build."
  (interactive)
  (save-buffers-without-asking)
  (if (find-project-directory) (compile (concat "./" makescript-file)))
  (other-window 1))

;; Add header files to C++ mode
(add-to-list 'auto-mode-alist '("\\.h\\'" . c++-mode))

;; ---------------------------------------------------------------

; C++ indentation style
(defconst craig-big-fun-c-style
  '((c-electric-pound-behavior   . nil)
    (c-tab-always-indent         . t)
    (c-comment-only-line-offset  . 0)
    (c-hanging-braces-alist      . ((class-open)
                                    (class-close)
                                    (defun-open)
                                    (defun-close)
                                    (inline-open)
                                    (inline-close)
                                    (brace-list-open)
                                    (brace-list-close)
                                    (brace-list-intro)
                                    (brace-list-entry)
                                    (block-open)
                                    (block-close)
                                    (substatement-open)
                                    (statement-case-open)
                                    (class-open)))
    (c-hanging-colons-alist      . ((inher-intro)
                                    (case-label)
                                    (label)
                                    (access-label)
                                    (access-key)
                                    (member-init-intro)))
    (c-cleanup-list              . (scope-operator
                                    list-close-comma
                                    defun-close-semi))
    (c-offsets-alist             . ((arglist-close         .  c-lineup-arglist)
                                    (label                 . -4)
                                    (access-label          . -4)
                                    (substatement-open     .  0)
                                    (statement-case-intro  .  4)
                                    (statement-block-intro .  c-lineup-for)
                                    (case-label            .  4)
                                    (block-open            .  0)
                                    (inline-open           .  0)
                                    (topmost-intro-cont    .  0)
                                    (knr-argdecl-intro     . -4)
                                    (brace-list-open       .  0)
                                    (brace-list-intro      .  4)))
    (c-echo-syntactic-information-p . t))
    "Craig's Big Fun C++ Style")

; CC++ mode handling
(defun craig-replace-string (FromString ToString)
  "Replace a string without moving point."
  (interactive "sReplace: \nsReplace: %s  With: ")
  (save-excursion
    (replace-string FromString ToString)
  ))
(define-key global-map [f8] 'craig-replace-string)


(defun craig-save-buffer ()
  "Save the buffer after untabifying it."
  (interactive)
  (save-excursion
    (save-restriction
      (widen)
      (untabify (point-min) (point-max))))
  (save-buffer))

; TXT mode handling
(defun craig-big-fun-text-hook ()
  ; 4-space tabs
  (setq tab-width 4
        indent-tabs-mode nil)

  ; Newline indents, semi-colon doesn't
  (define-key text-mode-map "\C-m" 'newline-and-indent)

  ; Prevent overriding of alt-s
  (define-key text-mode-map "\es" 'craig-save-buffer)
  )
(add-hook 'text-mode-hook 'craig-big-fun-text-hook)

; Startup windowing
(setq next-line-add-newlines nil)
(setq-default truncate-lines t)
(setq truncate-partial-width-windows nil)
(split-window-horizontally)

(define-key global-map "\t" 'dabbrev-expand)
(define-key global-map [S-tab] 'indent-for-tab-command)
(define-key global-map [backtab] 'indent-for-tab-command)
(define-key global-map "\C-y" 'indent-for-tab-command)
(define-key global-map [C-tab] 'indent-region)
(define-key global-map "	" 'indent-region)

(defun never-split-a-window ()
  "Don't want to attempt to split windows if i dont have to"
  nil)
(setq split-window-preferred-function 'never-split-a-window)

(defun craig-big-fun-c-style ()
  '((c-electric-pound-behavior   . nil)
    (c-tab-always-indent         . t)
    (c-comment-only-line-offset  . 0)
    (c-hanging-braces-alist      . ((class-open)
                                    (class-close)
                                    (defun-open)
                                    (defun-close)
                                    (inline-open)
                                    (inline-close)
                                    (brace-list-open)
                                    (brace-list-close)
                                    (brace-list-intro)
                                    (brace-list-entry)
                                    (block-open)
                                    (block-close)
                                    (substatement-open)
                                    (statement-case-open)
                                    (class-open)))
    (c-hanging-colons-alist      . ((inher-intro)
                                    (case-label)
                                    (label)
                                    (access-label)
                                    (access-key)
                                    (member-init-intro)))
    (c-cleanup-list              . (scope-operator
                                    list-close-comma
                                    defun-close-semi))
    (c-offsets-alist             . ((arglist-close         .  c-lineup-arglist)
                                    (label                 . -4)
                                    (access-label          . -4)
                                    (substatement-open     .  0)
                                    (statement-case-intro  .  4)
                                    (statement-block-intro .  c-lineup-for)
                                    (case-label            .  4)
                                    (block-open            .  0)
                                    (inline-open           .  0)
                                    (topmost-intro-cont    .  0)
                                    (knr-argdecl-intro     . -4)
                                    (brace-list-open       .  0)
                                    (brace-list-intro      .  4)))
    (c-echo-syntactic-information-p . t))
  "Syntax Style for C / C++ code"
)

(defun craig-big-fun-c-hook ()
  ;; Set my style for the current buffer
  (c-add-style "BigFun" craig-big-fun-c-style t)

  ;; 4-space tabs
  (setq tab-width 4
        indent-tabs-mode nil)

  ;; Additional style stuff
  (c-set-offset 'member-init-intro '++)

  ;; No hungry backspace
  (c-toggle-auto-hungry-state -1)

  ;; Newline indents, semi-colon doesn't
  (define-key c++-mode-map "\C-m" 'newline-and-indent)
  (setq c-hanging-semi&comma-criteria '((lambda () 'stop)))

  ;; Handle super-tabbify (TAB completes, shift-TAB actually tabs)
  (setq dabbrev-case-replace t)
  (setq dabbrev-case-fold-search t)
  (setq dabbrev-upcase-means-case-search t)

  ;; Abbrevation expansion
  (abbrev-mode 1)

  (defun craig-header-format ()
     "Format the given file as a header file."
     (interactive)
     (setq BaseFileName (file-name-sans-extension (file-name-nondirectory buffer-file-name)))
     (insert "#if !defined(")
     (push-mark)
     (insert BaseFileName)
     (upcase-region (mark) (point))
     (pop-mark)
     (insert "_H)\n")
     (insert "/* ========================================================================\n")
     (insert "   $File: $\n")
     (insert "   $Date: $\n")
     (insert "   $Revision: $\n")
     (insert "   $Creator: Craig Giles $\n")
     (insert "   $Notice: (C) Copyright 2019 by Craig Giles. All Rights Reserved. $\n")
     (insert "   ======================================================================== */\n")
     (insert "\n")
     (insert "#define ")
     (push-mark)
     (insert BaseFileName)
     (upcase-region (mark) (point))
     (pop-mark)
     (insert "_H\n")
     (insert "\n")
     (insert "\n")
     (insert "\n")
     (insert "#endif")
  )

  (defun craig-source-format ()
     "Format the given file as a source file."
     (interactive)
     (setq BaseFileName (file-name-sans-extension (file-name-nondirectory buffer-file-name)))
     (insert "/* ========================================================================\n")
     (insert "   $File: $\n")
     (insert "   $Date: $\n")
     (insert "   $Revision: $\n")
     (insert "   $Creator: Craig Giles $\n")
     (insert "   $Notice: (C) Copyright 2019 by Craig Giles. All Rights Reserved. $\n")
     (insert "   ======================================================================== */\n")
  )

  (cond ((file-exists-p buffer-file-name) t)
        ((string-match "[.]hin" buffer-file-name) (craig-source-format))
        ((string-match "[.]cin" buffer-file-name) (craig-source-format))
        ((string-match "[.]h" buffer-file-name) (craig-header-format))
        ((string-match "[.]cpp" buffer-file-name) (craig-source-format)))

  (defun craig-find-corresponding-file ()
    "Find the file that corresponds to this one."
    (interactive)
    (setq CorrespondingFileName nil)
    (setq BaseFileName (file-name-sans-extension buffer-file-name))
    (if (string-match "\\.c" buffer-file-name)
       (setq CorrespondingFileName (concat BaseFileName ".h")))
    (if (string-match "\\.h" buffer-file-name)
       (if (file-exists-p (concat BaseFileName ".c")) (setq CorrespondingFileName (concat BaseFileName ".c"))
	   (setq CorrespondingFileName (concat BaseFileName ".cpp"))))
    (if (string-match "\\.hin" buffer-file-name)
       (setq CorrespondingFileName (concat BaseFileName ".cin")))
    (if (string-match "\\.cin" buffer-file-name)
       (setq CorrespondingFileName (concat BaseFileName ".hin")))
    (if (string-match "\\.cpp" buffer-file-name)
       (setq CorrespondingFileName (concat BaseFileName ".h")))
    (if CorrespondingFileName (find-file CorrespondingFileName)
       (error "Unable to find a corresponding file")))
  (defun craig-find-corresponding-file-other-window ()
    "Find the file that corresponds to this one."
    (interactive)
    (find-file-other-window buffer-file-name)
    (craig-find-corresponding-file)
    (other-window -1))

  (define-key c++-mode-map [f12] 'craig-find-corresponding-file)
  (define-key c++-mode-map [M-f12] 'craig-find-corresponding-file-other-window)
  (define-key c++-mode-map [f5] 'make-without-asking)

  ; Alternate bindings for F-keyless setups (ie MacOS X terminal)
  (define-key c++-mode-map "\ec" 'craig-find-corresponding-file)
  (define-key c++-mode-map "\eC" 'craig-find-corresponding-file-other-window)

  (define-key c++-mode-map "\es" 'craig-save-buffer)

  (define-key c++-mode-map "\t" 'dabbrev-expand)
  (define-key c++-mode-map [S-tab] 'indent-for-tab-command)
  (define-key c++-mode-map "\C-y" 'indent-for-tab-command)
  (define-key c++-mode-map [C-tab] 'indent-region)
  (define-key c++-mode-map "	" 'indent-region)

  (define-key c++-mode-map "\ej" 'imenu)

  (define-key c++-mode-map "\e." 'c-fill-paragraph)

  (define-key c++-mode-map "\e/" 'c-mark-function)

  (define-key c++-mode-map "\e " 'set-mark-command)
  (define-key c++-mode-map "\eq" 'append-as-kill)
  (define-key c++-mode-map "\ea" 'yank)
  (define-key c++-mode-map "\ez" 'kill-region)

  ; devenv.com error parsing
  (add-to-list 'compilation-error-regexp-alist 'craig-devenv)
  (add-to-list 'compilation-error-regexp-alist-alist '(craig-devenv
   "*\\([0-9]+>\\)?\\(\\(?:[a-zA-Z]:\\)?[^:(\t\n]+\\)(\\([0-9]+\\)) : \\(?:see declaration\\|\\(?:warnin\\(g\\)\\|[a-z ]+\\) C[0-9]+:\\)"
    2 3 nil (4)))
)

;; ---------------------------------------------------------------

;; All code within an #if 0 block should be set to the comment color
(defun my-c-mode-font-lock-if0 (limit)
  (save-restriction
    (widen)
    (save-excursion
      (goto-char (point-min))
      (let ((depth 0) str start start-depth)
        (while (re-search-forward "^\\s-*#\\s-*\\(if\\|else\\|endif\\)" limit 'move)
          (setq str (match-string 1))
          (if (string= str "if")
              (progn
                (setq depth (1+ depth))
                (when (and (null start) (looking-at "\\s-+0"))
                  (setq start (match-end 0)
                        start-depth depth)))
            (when (and start (= depth start-depth))
              (c-put-font-lock-face start (match-beginning 0) 'font-lock-comment-face)
              (setq start nil))
            (when (string= str "endif")
              (setq depth (1- depth)))))
        (when (and start (> depth 0))
          (c-put-font-lock-face start (point) 'font-lock-comment-face)))))
  nil)

(defun my-c-mode-common-hook ()
  (font-lock-add-keywords
   nil
   '((my-c-mode-font-lock-if0 (0 font-lock-comment-face prepend))) 'add-to-end))

;; (add-hook 'before-save-hook #'my-c++-mode-before-save-hook)
;; (add-hook 'c-mode-common-hook 'craigs-big-fun-c-hook)
(add-hook 'c-mode-common-hook 'my-c-mode-common-hook)
(add-hook 'c-mode-common-hook 'craig-big-fun-c-hook)


;; ===============================================================
;; Theme Settings
;; ---------------------------------------------------------------
(setq fixme-modes '(c++-mode c-mode markdown-mode emacs-lisp-mode scala-mode sbt-mode))
(make-face 'font-lock-todo-face)
(make-face 'font-lock-bug-face)
(make-face 'font-lock-study-face)
(make-face 'font-lock-important-face)
(make-face 'font-lock-note-face)
(mapc (lambda (mode)
 (font-lock-add-keywords
  mode
  '(("\\<\\(TODO\\)" 1 'font-lock-todo-face t)
    ("\\<\\(BUG\\)" 1 'font-lock-bug-face t)
    ("\\<\\(STUDY\\)" 1 'font-lock-study-face t)
    ("\\<\\(IMPORTANT\\)" 1 'font-lock-important-face t)
    ("\\<\\(NOTE\\)" 1 'font-lock-note-face t))))
  fixme-modes)
(modify-face 'font-lock-todo-face "firebrick3" nil nil t nil t nil nil)
(modify-face 'font-lock-bug-face "Red" nil nil t nil t nil nil)
(modify-face 'font-lock-study-face "Yellow" nil nil t nil t nil nil)
(modify-face 'font-lock-important-face "Yellow" nil nil t nil t nil nil)
(modify-face 'font-lock-note-face "CornflowerBlue" nil nil t nil t nil nil)

;; (modify-face 'font-lock-todo-face "Red" nil nil t nil t nil nil)
;; (modify-face 'font-lock-bug-face "Red" nil nil t nil t nil nil)
;; (modify-face 'font-lock-study-face "Yellow" nil nil t nil t nil nil)
;; (modify-face 'font-lock-important-face "Yellow" nil nil t nil t nil nil)
;; (modify-face 'font-lock-note-face "Dark Green" nil nil t nil t nil nil)

; Accepted file extensions and their appropriate modes
(setq auto-mode-alist
      (append
       '(("\\.cpp$"    . c++-mode)
         ("\\.hpp$"    . c++-mode)
         ("\\.hin$"    . c++-mode)
         ("\\.cin$"    . c++-mode)
         ("\\.inl$"    . c++-mode)
         ("\\.rdc$"    . c++-mode)
         ("\\.h$"      . c++-mode)
         ("\\.c$"      . c++-mode)
         ("\\.cc$"     . c++-mode)
         ("\\.c8$"     . c++-mode)
         ("\\.txt$"    . indented-text-mode)
         ("\\.emacs$"  . emacs-lisp-mode)
         ("\\.gen$"    . gen-mode)
         ("\\.ms$"     . fundamental-mode)
         ("\\.m$"      . objc-mode)
         ("\\.mm$"     . objc-mode)
         ("\\.scala$"  . scala-mode)
         ("\\.sc$"     . scala-mode)
         ("\\.sbt$"    . scala-mode)
         ("\\makefile$". make-mode)
         ("\\Makefile$". make-mode)
         ) auto-mode-alist))

(add-to-list 'default-frame-alist '(font . "Liberation Mono-12"))
(set-face-attribute 'default t :font "Liberation Mono-12")

(set-face-attribute 'font-lock-builtin-face nil :foreground "#DAB98F")
(set-face-attribute 'font-lock-comment-face nil :foreground "gray50")
(set-face-attribute 'font-lock-constant-face nil :foreground "olive drab")
(set-face-attribute 'font-lock-doc-face nil :foreground "gray50")
(set-face-attribute 'font-lock-keyword-face nil :foreground "DarkGoldenrod3")
(set-face-attribute 'font-lock-string-face nil :foreground "olive drab")
;; (set-face-attribute 'font-lock-variable-name-face nil :foreground "#dcdcdc")
;; (set-face-attribute 'font-lock-type-face nil :foreground "#dcdcdc")
;; (setq foreground-font-color "#debea1")
;; (setq foreground-font-color "#ecdfd4")
;; (setq foreground-font-color "#dcdcdc")
;; (setq foreground-font-color "#b0b0b0")
;; (setq foreground-font-color "#c6c1ba")
(setq foreground-font-color "#d2cbc0")


;; (setq foreground-font-color "#e4cbb4")

(set-face-attribute 'font-lock-function-name-face nil :foreground foreground-font-color)
(set-face-attribute 'font-lock-variable-name-face nil :foreground foreground-font-color)
(set-face-attribute 'font-lock-type-face nil :foreground foreground-font-color)

(set-face-background 'hl-line "midnight blue");; the -always on- horizontal highlight
(set-background-color "#152426")              ;; something akin to J.Blow's theme
;; (set-foreground-color "#dcdcdc")
(set-foreground-color foreground-font-color)
(set-cursor-color "#40FF40")

(set-face-attribute 'mode-line nil
                    :background "burlywood3"
                    :foreground "black")

(defun post-load-stuff ()
  (interactive)
  "Load all the things that I want loaded, AFTER emacs is up and running"
  (menu-bar-mode -1)
  (scroll-bar-mode -1)
  (tool-bar-mode -1)
  (toggle-frame-maximized)
  (load-file "~/.emacs.d/local-init.el")
)
(add-hook 'window-setup-hook 'post-load-stuff t)

