; NOTE: I am not a LISP programmer and I know next to nothing about the EMACS
; ecosystem and programming environment. Coming from VIM, this could be
; interesting...

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

;; ---------------------------------------------------------------

;; ===============================================================
;; Installed Packages
;; ---------------------------------------------------------------
(use-package counsel-projectile
  :config
  (counsel-mode))

(use-package evil-commentary
  :init
  (evil-commentary-mode t))

;; EVIL mode is VIM keybindings and leader system
(use-package evil
  :init
  (evil-mode t)

  :config
  (define-key evil-normal-state-map (kbd "j") 'evil-next-visual-line)
  (define-key evil-normal-state-map (kbd "k") 'evil-previous-visual-line)
  (define-key evil-normal-state-map (kbd "-") 'dired)
  ;; (define-key evil-normal-state-map (kbd "C-h") (kbd "C-w h"))
  ;; (define-key evil-normal-state-map (kbd "C-j") (kbd "C-w j"))
  ;; (define-key evil-normal-state-map (kbd "C-k") (kbd "C-w k"))
  ;; (define-key evil-normal-state-map (kbd "C-l") (kbd "C-w l"))

  ;; (define-key evil-normal-state-map (kbd "C-w C-h") (kbd "C-w h"))
  ;; (define-key evil-normal-state-map (kbd "C-w C-j") (kbd "C-w j"))
  ;; (define-key evil-normal-state-map (kbd "C-w C-k") (kbd "C-w k"))
  ;; (define-key evil-normal-state-map (kbd "C-w C-l") (kbd "C-w l"))

  (define-key evil-normal-state-map (kbd "C-p") 'counsel-find-file)
  (define-key evil-normal-state-map (kbd "C-f") 'ag-project-at-point)
  (define-key evil-normal-state-map (kbd "C-l") 'other-window)
  (define-key evil-normal-state-map (kbd "C-u") 'evil-scroll-up)
  (define-key evil-visual-state-map (kbd "C-u") 'evil-scroll-up)
  (define-key evil-insert-state-map (kbd "C-u")
    (lambda ()
      (interactive)
      (evil-delete (point-at-bol) (point))))

  (use-package evil-leader
    :config
    (setq evil-leader/in-all-states 1)
    (global-evil-leader-mode)
    (evil-leader/set-leader "SPC")
    (evil-leader/set-key "n" 'evil-search-highlight-persist-remove-all)
    (evil-leader/set-key "f" 'find-file)
    (evil-leader/set-key "o" 'find-file-other-window)
    (evil-leader/set-key "SPC" 'other-window)
    )

  (use-package evil-surround
    :config
    (global-evil-surround-mode))

  (use-package evil-indent-textobject)

  (use-package evil-search-highlight-persist
    :config
    (global-evil-search-highlight-persist t))

  (use-package use-package-chords
    :config
    (key-chord-mode 1)
    (setq key-chord-two-keys-delay 0.2)
    ;; Exit insert mode with 'jj' or 'jk'
    (key-chord-define evil-insert-state-map "jj" 'evil-normal-state)
    (key-chord-define evil-insert-state-map "jk" 'evil-normal-state)
    (key-chord-define evil-insert-state-map "Jj" 'evil-normal-state)
    (key-chord-define evil-insert-state-map "Jk" 'evil-normal-state)
    (key-chord-define evil-insert-state-map "JJ" 'evil-normal-state)
    (key-chord-define evil-insert-state-map "JK" 'evil-normal-state)
    (key-chord-define evil-normal-state-map "ls" 'buffer-menu)

    ;; ;; Use ensime to get the type at the point
    (key-chord-define evil-normal-state-map "et" 'ensime-type-at-point)
    (key-chord-define evil-normal-state-map "ed" 'ensime-edit-definition-other-window)
    )

  )

(with-eval-after-load 'evil
  (defalias #'forward-evil-word #'forward-evil-symbol))

;; (with-eval-after-load 'evil
;;   (defadvice evil-inner-word (around underscore-as-word activate)
;;     (let ((table (copy-syntax-table (syntax-table))))
;;       (modify-syntax-entry ?_ "w" table)
;;       (with-syntax-table table ad-do-it)))
;; )

;; Show matching paren
(use-package autopair
  :config
  (show-paren-mode t)
  (autopair-global-mode))

;; (use-package smooth-scrolling
;;   :config
;;   (setq scroll-margin 8
;;         scroll-conservatively 9999
;;         scroll-step 1))

(use-package fill-column-indicator
  :init
  (setq-default fill-column 80)
  (add-hook 'after-change-major-mode-hook 'fci-mode))

; Ensure that emacs has the shell's PATH variables on osx
(use-package exec-path-from-shell
  :config
  (when (memq window-system '(mac ns x))
    (exec-path-from-shell-initialize)))

(use-package ag
  :config
  (setq ag-reuse-buffers 't))

(use-package scala-mode
  :pin melpa-stable
  :chords ((":." . ":.")
           (".>" . "⇒")
           ("->" . "→")
           ("<-" . "←")
           ("++" . "⧺")
           ("<." . "≤")
           (">." . "≥")
           (".." . "≡"))
  :interpreter ("scala" . scala-mode)
  :config)

(use-package sbt-mode
  :pin melpa-stable
  :commands sbt-start sbt-command)

(use-package ensime
  :ensure t
  :config
  (setq ensime-startup-notification nil)
  (setq ensime-startup-snapshot-notification nil)
  :pin melpa-stable)

(use-package 2048-game)
(use-package cc-mode)
(use-package solarized-theme)

(use-package markdown-mode
  :pin melpa-stable
  :init
  (setq
   auto-mode-alist  (cons '("\\.md$" . markdown-mode) auto-mode-alist)
   auto-mode-alist  (cons '("\\.markdown$" . markdown-mode) auto-mode-alist)))

;; ---------------------------------------------------------------

;; ===============================================================
;; General Editor Settings
;; ---------------------------------------------------------------
(setq custom-file "~/.emacs.d/custom.el")
(load custom-file)

;; Put all the backup files in an ~/.emacs.d/backup dir
(setq backup-directory-alist `(("." . "~/.emacs.d/backup/")))

;; given that I have to work with eclipse users it's the only way to
;; stay sane.
(defun fix-format-buffer ()
  "indent, untabify and remove trailing whitespace for a buffer"
  (interactive)
  (save-excursion
    (delete-trailing-whitespace)
    (indent-region (point-min) (point-max))
    (untabify (point-min) (point-max))))

;; Set the OSX's CMD key as the meta key
(setq mac-option-key-is-meta nil)
(setq mac-command-key-is-meta t)
(setq mac-command-modifier 'meta)
(setq mac-option-modifier nil)

;; use spaces with 4 spaces per tab
(setq-default tab-width 4 indent-tabs-mode nil)

;; Disable scroll bars and toolbars and welcome screen
(scroll-bar-mode -1)
(tool-bar-mode -1)
(setq inhibit-startup-screen t)

;; Put the backup files in a single directory
(setq backup-directory-alist `((".*" . ,temporary-file-directory)))
(setq auto-save-file-name-transforms `((".*" ,temporary-file-directory t)))

;; Center the search (nzz)
(defun my-center-line (&rest _)
  (evil-scroll-line-to-center nil))

(advice-add 'evil-search-next :after #'my-center-line)

(setq evil-motion-state-modes
      (append evil-emacs-state-modes evil-motion-state-modes))
(setq evil-emacs-state-modes nil)

; Stop Emacs from losing undo information by setting very high limits for undo
; buffers
(setq undo-limit 20000000)
(setq undo-strong-limit 40000000)

; Create a big horizontal blue bar so i don't keep loosing my cursor
(global-hl-line-mode 1)

; Turn off the bell on Mac OS X
(defun nil-bell ())
(setq ring-bell-function 'nil-bell)

; Move to the parent directory when in the dired directory listing
(define-key dired-mode-map "%" 'find-file)
(define-key dired-mode-map "d" 'dired-create-directory)
(define-key dired-mode-map "D" 'delete-file-or-directory)
(define-key dired-mode-map "-"
    (lambda ()
      (interactive)
      (find-alternate-file "..")))

; Bright-red TODOs, NOTEs, and other things
(setq fixme-modes '(c++-mode c-mode emacs-lisp-mode scala-mode org-mode))
(make-face 'font-lock-fixme-face)
(make-face 'font-lock-study-face)
(make-face 'font-lock-important-face)
(make-face 'font-lock-note-face)
(mapc (lambda (mode)
        (font-lock-add-keywords
          mode
          '(("\\<\\(TODO\\)" 1 'font-lock-fixme-face t)
            ("\\<\\(STUDY\\)" 1 'font-lock-study-face t)
            ("\\<\\(IMPORTANT\\)" 1 'font-lock-important-face t)
            ("\\<\\(NOTE\\)" 1 'font-lock-note-face t))))
      fixme-modes)
(modify-face 'font-lock-fixme-face "Red" nil nil t nil t nil nil)
(modify-face 'font-lock-study-face "Yellow" nil nil t nil t nil nil)
(modify-face 'font-lock-important-face "Yellow" nil nil t nil t nil nil)
(modify-face 'font-lock-note-face "Dark Green" nil nil t nil t nil nil)

(setq auto-mode-alist
      (append
       '(("\\.cpp$"    . c++-mode)
         ("\\.hin$"    . c++-mode)
         ("\\.cin$"    . c++-mode)
         ("\\.inl$"    . c++-mode)
         ("\\.rdc$"    . c++-mode)
         ("\\.h$"    . c++-mode)
         ("\\.c$"   . c++-mode)
         ("\\.cc$"   . c++-mode)
         ("\\.c8$"   . c++-mode)
         ("\\.txt$" . indented-text-mode)
         ("\\.emacs$" . emacs-lisp-mode)
         ("\\.gen$" . gen-mode)
         ("\\.ms$" . fundamental-mode)
         ("\\.m$" . objc-mode)
         ("\\.mm$" . objc-mode)
         ("\\.scala$" . scala-mode)
         ("\\.sc$" . scala-mode)
         ("\\.sbt$" . sbt-mode)
         ) auto-mode-alist))


; Clock
(display-time)

;; ---------------------------------------------------------------

; Theme based configuration
(load-theme 'zenburn t)
;; (load-theme 'solarized-dark t)

; -----------------------------------------------
; Old Theme Settings
; -----------------------------------------------
;; (add-to-list 'default-frame-alist '(font . "Liberation Mono-12"))
;; (set-face-attribute 'default t :font "Liberation Mono-12")
;; (set-face-attribute 'font-lock-type-face nil :foreground "burlywood3")
;; (set-face-attribute 'font-lock-variable-name-face nil :foreground "burlywood3")

(set-face-attribute 'font-lock-builtin-face nil :foreground "#DAB98F")
;; (set-face-attribute 'font-lock-comment-face nil :foreground "gray50")
;; (set-face-attribute 'font-lock-constant-face nil :foreground "olive drab")
;; (set-face-attribute 'font-lock-doc-face nil :foreground "gray50")
(set-face-attribute 'font-lock-function-name-face nil :foreground "burlywood3")
(set-face-attribute 'font-lock-keyword-face nil :foreground "DarkGoldenrod3")
;; (set-face-attribute 'font-lock-string-face nil :foreground "olive drab")

;; (set-face-attribute 'mode-line-buffer-id nil :foreground "black")
(set-face-attribute 'mode-line nil
                    :background "burlywood3"
                    :foreground "black")

;; (set-face-attribute 'evil-search-highlight-persist-highlight-face nil
;;                     :foreground "base03"
;;                     :background "DarkGoldenrod3")

;; (set-face-attribute 'lazy-highlight nil
;;                     :foreground "black"
;;                     :background "midnight blue")
; -----------------------------------------------
; I like but dont love
; -----------------------------------------------
(set-face-attribute 'font-lock-type-face nil :foreground "#dcdcdc")
(set-face-attribute 'font-lock-variable-name-face nil :foreground "#dcdcdc")
(set-face-attribute 'font-lock-constant-face nil :foreground "burlywood3")
;; (set-face-attribute 'font-lock-string-face nil :foreground "#5b845c")

; -----------------------------------------------
; Set in stone: Dont change
; -----------------------------------------------
(set-face-attribute 'default t :font "Liberation Mono-12")

;; (set-background-color "#152426")               ; something akin to J.Blow's theme
(set-face-background 'hl-line "midnight blue") ; the -always on- horizontal highlight
(set-foreground-color "#dcdcdc")
(set-cursor-color "#40FF40")

;; ---------------------------------------------------------------

(menu-bar-mode -1)
(add-to-list 'initial-frame-alist '(fullscreen . maximized))
(add-to-list 'default-frame-alist '(fullscreen . fullheight))

; Startup windowing
(setq next-line-add-newlines nil)
(setq-default truncate-lines t)
(setq truncate-partial-width-windows nil)
(split-window-horizontally)

(defun never-split-a-window ()
    "Don't want to attempt to split windows if i dont have to"
    nil)
(setq split-window-preferred-function 'never-split-a-window)

;; ===============================================================
;; Scala Mode Configuration
;; ---------------------------------------------------------------
(defun sbt-load-in-other-window ()
  "load sbt in the next window"
  (interactive)
  (switch-to-buffer-other-window (sbt:run-sbt)))

(define-key scala-mode-map [f5] 'sbt-load-in-other-window)

;; ---------------------------------------------------------------

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
(setq makescript-file "build.sh")

; Compile Settings
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

; Add header files to C++ mode
(add-to-list 'auto-mode-alist '("\\.h\\'" . c++-mode))

;; ---------------------------------------------------------------


; C++ indentation style
(defconst craigs-big-fun-c-style
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
    "Craigs's Big Fun C++ Style")

; CC++ mode handling
(defun craigs-big-fun-c-hook ()
  ; Set my style for the current buffer
  (c-add-style "BigFun" craigs-big-fun-c-style t)
  
  ; 4-space tabs
  (setq tab-width 4
        indent-tabs-mode nil)

  ; Additional style stuff
  (c-set-offset 'member-init-intro '++)

  ; No hungry backspace
  (c-toggle-auto-hungry-state -1)

  ; Newline indents, semi-colon doesn't
  (define-key c++-mode-map "\C-m" 'newline-and-indent)
  (setq c-hanging-semi&comma-criteria '((lambda () 'stop)))

  ; Handle super-tabbify (TAB completes, shift-TAB actually tabs)
  (setq dabbrev-case-replace t)
  (setq dabbrev-case-fold-search t)
  (setq dabbrev-upcase-means-case-search t)

  ; Abbrevation expansion
  (abbrev-mode 1)
 
  (defun craigs-find-corresponding-file ()
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
  (defun craigs-find-corresponding-file-other-window ()
    "Find the file that corresponds to this one."
    (interactive)
    (find-file-other-window buffer-file-name)
    (craigs-find-corresponding-file)
    (other-window -1))

  (define-key c++-mode-map [f12] 'craigs-find-corresponding-file)
  (define-key c++-mode-map [M-f12] 'craigs-find-corresponding-file-other-window)
  (define-key c++-mode-map [f5] 'make-without-asking)

                                        ; devenv.com error parsing
  (add-to-list 'compilation-error-regexp-alist 'craigs-devenv)
  (add-to-list 'compilation-error-regexp-alist-alist '(craigs-devenv
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

(add-hook 'c-mode-common-hook 'my-c-mode-common-hook)


;; ---------------------------------------------------------------


;; Find replace string shoudl work without moving locations
(defun replace-string-in-place (FromString ToString)
  "Replace a string without moving point."
  (interactive "sReplace: \nsReplace: %s  With: ")
  (save-excursion
    (replace-string FromString ToString)
  ))
(define-key global-map [f8] 'replace-string-in-place)

(add-hook 'c-mode-common-hook 'craigs-big-fun-c-hook)
