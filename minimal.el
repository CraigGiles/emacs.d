; (menu-bar-mode -1)
; (scroll-bar-mode -1)
; ; (tool-bar-mode -1)
; (toggle-frame-maximized)

;; ===============================================================
;;   Package Management
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

;; NOTE: Should look over this config file at some point.
;;       https://github.com/krisajenkins/EvilBegins/blob/master/.emacs
(use-package evil
  :pin melpa-stable
  :defer t
  :init
    (evil-mode t)

  :config
    (setq evil-vsplit-window-right t)
    (setq evil-split-window-below t)

    (define-key evil-normal-state-map (kbd "j") 'evil-next-visual-line)
    (define-key evil-normal-state-map (kbd "k") 'evil-previous-visual-line)

    (define-key evil-normal-state-map (kbd "-") 'find-file)

    (define-key evil-normal-state-map (kbd "M-p") 'find-name-dired)
    ;; (define-key evil-normal-state-map (kbd "M-C-p") 'projectile-find-file-other-window)
    ;; (define-key evil-normal-state-map (kbd "C-c C-c") 'eval-buffer)

    (define-key evil-normal-state-map (kbd "C-f") 'find-grep-dired)
    (define-key evil-normal-state-map (kbd "C-u") 'evil-scroll-up)
    (define-key evil-visual-state-map (kbd "C-u") 'evil-scroll-up)
    (define-key evil-insert-state-map (kbd "C-u") (lambda ()
      (interactive)
      (evil-delete (point-at-bol) (point)))
    )

    (use-package evil-search-highlight-persist
      :pin melpa-stable
      :config
        (global-evil-search-highlight-persist t))

    (use-package use-package-chords
      :pin melpa-stable
      :config
        (key-chord-mode 1)
        (setq key-chord-two-keys-delay 0.2)

        (key-chord-define evil-normal-state-map "gc" 'evil-commentary-line)

        ;; Exit insert mode with 'jj'
        (key-chord-define evil-insert-state-map "jj" 'evil-normal-state)
        (key-chord-define evil-insert-state-map "Jj" 'evil-normal-state)
        (key-chord-define evil-insert-state-map "JJ" 'evil-normal-state)
    ) ;; use-package-chords
  
) ;; evil
;; ===============================================================
;;   General Editor Settings
;; ---------------------------------------------------------------

;; Put all the backup files in an ~/.emacs.d/backup dir
(setq backup-directory-alist '(("." . "~/.emacs.d/auto-saves")))
(setq create-lockfiles nil)
(setq auto-save-default nil) ;; IMPORTANT(craig) - remove this if it becomes necessary

(setq next-line-add-newlines nil)
(setq-default truncate-lines t)
(setq truncate-partial-width-windows nil)

;; Stop Emacs from losing undo information by setting high limits
(setq undo-limit 20000000)
(setq undo-strong-limit 40000000)

;; Set the OSX's CMD key as the meta key
(setq mac-option-key-is-meta nil)
(setq mac-command-key-is-meta t)
(setq mac-command-modifier 'meta)
(setq mac-option-modifier nil)

;; turn on the column numbers in modeline
(setq column-number-mode 1)

; Turn off the bell on Mac OS X
(defun nil-bell ())
(setq ring-bell-function 'nil-bell)

;; Create a big horizontal blue bar so i don't keep loosing my cursor
(global-hl-line-mode 1)
(global-visual-line-mode 1)

;; Auto revert files that change on the hard disk
(global-auto-revert-mode 1)

;; Navigating errors
(define-key global-map [f9] 'first-error)
(define-key global-map (kbd "M-n") 'next-error)
(define-key global-map (kbd "M-C-n") 'previous-error)

;; ===============================================================
;;   Theme Settings
;; ---------------------------------------------------------------

;; Font
(add-to-list 'default-frame-alist '(font . "Liberation Mono-12"))
(set-face-attribute 'default t :font "Liberation Mono-12")

;; Colors
(setq foreground-font-color                                       "#D2CBC0")
(set-face-attribute 'font-lock-builtin-face nil       :foreground "#DAB98F")
(set-face-attribute 'font-lock-comment-face nil       :foreground "#7F7F7F") ;; grey50
(set-face-attribute 'font-lock-doc-face nil           :foreground "#7F7F7F") ;; grey50
(set-face-attribute 'font-lock-string-face nil        :foreground "#65B29E") ;; maybe #458B74
(set-face-attribute 'font-lock-keyword-face nil       :foreground "#DAB98F") ;; DarkGoldenRod3
(set-face-attribute 'font-lock-constant-face nil      :foreground foreground-font-color)
(set-face-attribute 'font-lock-function-name-face nil :foreground foreground-font-color)
(set-face-attribute 'font-lock-variable-name-face nil :foreground foreground-font-color)
(set-face-attribute 'font-lock-type-face nil          :foreground foreground-font-color)
(set-foreground-color                                             foreground-font-color)
(set-background-color                                             "#122A2C") ;; something akin to J.Blow's theme
(set-face-background 'hl-line                                     "#191970") ;; the -always on- horizontal highlight
(set-cursor-color                                                 "#40FF40") ;; Green-ish cursor color
(set-face-attribute 'mode-line nil                    :background "#CDAA7D" ;; "burlywood3"
                                                      :foreground "#000000")
