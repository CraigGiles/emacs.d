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

;; ===============================================================
;; Installed Packages
;; ---------------------------------------------------------------
(use-package counsel-projectile
             :config
             (counsel-mode))

(use-package evil-commentary
             :init
             (evil-commentary-mode t))

;; Use Vim / EVIL mode
(use-package evil
             :init
             (evil-mode t)

             :config
             (define-key evil-normal-state-map (kbd "j") 'evil-next-visual-line)
             (define-key evil-normal-state-map (kbd "k") 'evil-previous-visual-line)
             (define-key evil-normal-state-map (kbd "-") 'dired-jump)
             (define-key evil-normal-state-map (kbd "C-h") (kbd "C-w h"))
             (define-key evil-normal-state-map (kbd "C-j") (kbd "C-w j"))
             (define-key evil-normal-state-map (kbd "C-k") (kbd "C-w k"))
             (define-key evil-normal-state-map (kbd "C-l") (kbd "C-w l"))
             (define-key evil-normal-state-map (kbd "C-b") 'desperately-compile)

             (global-set-key (kbd "C-p") 'counsel-projectile-find-file)

             (use-package evil-leader
                          :config
                          (setq evil-leader/in-all-states 1)
                          (global-evil-leader-mode)
                          (evil-leader/set-leader "SPC")
                          ;; Set <leader>n to clear highlights
                          (evil-leader/set-key "n" 'evil-search-highlight-persist-remove-all)
                          ;; <leader>f finds files in project
                          (evil-leader/set-key "f" 'counsel-projectile-find-file)
                          ;; <leader>p finds tags in project
                          (evil-leader/set-key "p" 'projectile-find-tag)
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
                          ;; ;; Use ensime to get the type at the point
                          ;; (key-chord-define evil-normal-state-map "et" 'ensime-type-at-point)
                          )
             )

;; Show matching paren
(use-package autopair
             :config
             (show-paren-mode t)
             (autopair-global-mode))

(use-package smooth-scrolling
             :config
             (setq scroll-margin 8
                   scroll-conservatively 9999
                   scroll-step 1))

(use-package fill-column-indicator
             :init
             (setq-default fill-column 80)
             (add-hook 'after-change-major-mode-hook 'fci-mode))

; Ensure that emacs has the shell's PATH variables on osx
(use-package exec-path-from-shell
             :config
             (when (memq window-system '(mac ns x))
               (exec-path-from-shell-initialize))
)

(use-package ag
             :config
             (setq ag-reuse-buffers 't)
             )

(use-package 2048-game)

;; ===============================================================
;; General Editor Settings
;; ===============================================================
(load-theme 'zenburn t)

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
(global-linum-mode t)

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

; Bright-red TODOs, NOTEs, and other things
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

; Clock
(display-time)

(set-face-attribute 'default t :font "Liberation Mono-11.5")
(set-face-background 'hl-line "midnight blue")
(set-face-attribute 'font-lock-builtin-face nil :foreground "#DAB98F")
(set-face-attribute 'font-lock-comment-face nil :foreground "gray50")
(set-face-attribute 'font-lock-constant-face nil :foreground "olive drab")
(set-face-attribute 'font-lock-doc-face nil :foreground "gray50")
(set-face-attribute 'font-lock-function-name-face nil :foreground "burlywood3")
(set-face-attribute 'font-lock-keyword-face nil :foreground "DarkGoldenrod3")
(set-face-attribute 'font-lock-string-face nil :foreground "olive drab")
(set-face-attribute 'font-lock-type-face nil :foreground "burlywood3")
(set-face-attribute 'font-lock-variable-name-face nil :foreground "burlywood3")

; Compile stuff
; travel up the tree to find a makefile
(defun desperately-compile ()
  "Traveling up the path, find a Makefile and `compile'."
  (interactive)
  (other-window 0)
  (save-some-buffers 1)
  (when (locate-dominating-file default-directory "build.sh")
  (with-temp-buffer
    (cd (locate-dominating-file default-directory "build.sh"))
    (compile "./build.sh"))))


; Show the 'compile' buffer in a vertical split
(defadvice compile (around split-horizontally activate)
  (let ((split-width-threshold 0)
        (split-height-threshold nil))
    ad-do-it))

; Move to the parent directory when in the dired directory listing
(define-key dired-mode-map "-"
    (lambda ()
      (interactive)
      (find-alternate-file "..")))
