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

  (define-key evil-normal-state-map (kbd "C-p") 'projectile--find-file)
  (define-key evil-normal-state-map (kbd "C-f") 'ag-project-at-point)

  ;; NOTE(craig) -- i want to get in the habit of <C-w><C-w> to goto other window
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
    (key-chord-define evil-insert-state-map "JJ" 'evil-normal-state)
    ;; (key-chord-define evil-normal-state-map "ls" 'buffer-menu)

    ;; TODO(craig) -- move to scala-init.el for scala development
    ;; Use ensime to get the type at the point
    (key-chord-define evil-normal-state-map "et" 'ensime-type-at-point)
    (key-chord-define evil-normal-state-map "ed" 'ensime-edit-definition-other-window)
    )

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
(use-package ido)

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

(define-key evil-normal-state-map (kbd "C-l") 'forward-word)
(define-key evil-normal-state-map (kbd "C-h") 'backward-word)
(define-key evil-normal-state-map (kbd "C-k") 'previous-blank-line)
(define-key evil-normal-state-map (kbd "C-j") 'next-blank-line)
(define-key evil-normal-state-map [home] 'beginning-of-line)
(define-key evil-normal-state-map [end] 'end-of-line)

;; ===============================================================
;; Keymap
;; ---------------------------------------------------------------
(define-key global-map "\ef" 'find-file)
(define-key global-map "\eF" 'find-file-other-window)

(global-set-key (read-kbd-macro "\eb")  'ido-switch-buffer)
(global-set-key (read-kbd-macro "\eB")  'ido-switch-buffer-other-window)

(define-key c++-mode-map "\ej" 'imenu)
(define-key global-map "\ec" 'quick-calc)

;; ===============================================================
;; Theme Settings
;; ---------------------------------------------------------------
(setq fixme-modes '(c++-mode c-mode emacs-lisp-mode))
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

; Accepted file extensions and their appropriate modes
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
         ) auto-mode-alist))
(add-to-list 'default-frame-alist '(font . "Liberation Mono-12"))
(set-face-attribute 'default t :font "Liberation Mono-12")
(set-face-attribute 'font-lock-builtin-face nil :foreground "#DAB98F")
(set-face-attribute 'font-lock-comment-face nil :foreground "gray50")
(set-face-attribute 'font-lock-constant-face nil :foreground "olive drab")
(set-face-attribute 'font-lock-doc-face nil :foreground "gray50")
(set-face-attribute 'font-lock-function-name-face nil :foreground "white")
(set-face-attribute 'font-lock-keyword-face nil :foreground "DarkGoldenrod3")
(set-face-attribute 'font-lock-string-face nil :foreground "olive drab")
(set-face-attribute 'font-lock-type-face nil :foreground "#dcdcdc")
(set-face-attribute 'font-lock-variable-name-face nil :foreground "#dcdcdc")

(set-face-background 'hl-line "midnight blue");; the -always on- horizontal highlight
(set-background-color "#152426")              ;; something akin to J.Blow's theme
(set-foreground-color "#dcdcdc")
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
  ;; (exec-path-from-shell-initialize)
  (load-file "~/.emacs.d/local-init.el")
)
(add-hook 'window-setup-hook 'post-load-stuff t)
