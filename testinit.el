;; This buffer is for text that is not saved, and for Lisp evaluation.
;; To create a file, visit it with - and enter text in its buffer.

(add-to-list 'load-path "~/.emacs.d/lisp/")
(load "gilesc-theme")

;;
;;      -- Package Management --
;; ---------------------------------------------------------------
(require 'package)

;; find packages in melpa or for org mode, orgmode over elpa
;; (or marmalade for that matter)
(add-to-list 'package-archives '("melpa" . "http://melpa.org/packages/") t)
(add-to-list 'package-archives '("gnu"   . "https://elpa.gnu.org/packages/"))
(add-to-list 'package-archives '("org" . "http://orgmode.org/elpa/") t)

(setq package-archive-priorities
      '(("melpa" . 1)
        ("org" . 2)
        ("gnu" . 3))
)

(package-initialize)

;; we're going to be using use-package to define and load packages.
;; it's a code based configuration with a bunch of fancy extensions
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

(require 'use-package)
(setq use-package-always-ensure t)

;;
;;      -- Packages --
;; ---------------------------------------------------------------
;; NOTE: Should look over this config file at some point.
;;       https://github.com/krisajenkins/EvilBegins/blob/master/.emacs
(use-package evil
  :pin melpa
  :init
    (evil-mode t)

  :config
    (setq evil-vsplit-window-right t)
    (setq evil-split-window-below t)

  :bind (:map evil-normal-state-map
            ("j" . 'evil-next-visual-line)
            ("k" . 'evil-previous-visual-line)
            ("-" . 'find-file)
            ("C-u". 'evil-scroll-up)
            ([tab] . 'evil-toggle-fold)
            ("M-j" . 'counsel-imenu)
            ("M-f" . 'find-file)
            ("M-6" . 'switch-other-window-to-last-buffer)
            ("SPC n" . 'evil-search-highlight-persist-remove-all)
            ("C-w C-h" . 'evil-window-left)
            ("C-w C-l" . 'evil-window-right)
            ("C-f" . 'ag-project-at-point)
            ("M-b" . 'counsel-ibuffer)
            ("M-n" . 'next-error)
            ("C-k" . 'evil-backward-paragraph)
            ("C-j" . 'evil-forward-paragraph))

        (:map evil-visual-state-map
            ("C-u". 'evil-scroll-up)
            ("C-k" . 'evil-backward-paragraph)
            ("C-j" . 'evil-forward-paragraph))
)

(use-package evil-commentary
  :after evil
  :pin melpa
  :init
    (evil-commentary-mode t))

(use-package evil-search-highlight-persist
  :after evil
  :pin melpa
  :init
    (global-evil-search-highlight-persist t))

(use-package evil-escape
  :after evil
  :pin melpa
  :config
    (evil-escape-mode)
)

(use-package use-package-chords
    :pin melpa
    :after evil
    :config
        (key-chord-mode 1)
        (setq key-chord-two-keys-delay 0.1)
        (key-chord-define evil-insert-state-map "Jj" 'evil-escape)
        (key-chord-define evil-insert-state-map "JJ" 'evil-escape)
        (key-chord-define evil-insert-state-map "jj" 'evil-escape)
        (key-chord-define evil-normal-state-map "gc" 'evil-commentary-line)
)

(use-package magit
  :pin melpa
  :config
    (use-package evil-magit)
    (evil-define-key 'normal magit-mode-map [tab] 'magit-section-toggle)
    (evil-define-key 'normal magit-blame-mode-map (kbd "g q") 'magit-blame-quit)
    (evil-define-key 'normal magit-mode-map (kbd "C-r") 'magit-status)
)

(define-key special-mode-map (kbd "C-k") 'scroll-up-command)
(define-key special-mode-map (kbd "C-j") 'scroll-down-command)


;; - Dired Keymap -
;;   -   : Move to the parent directory
;;   S-D : Create directory
;;   %   : Create File
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

;;
;;      -- Modes --
;; ---------------------------------------------------------------
(load "kotlin-config")
(load "jai-config")
(projectile-mode)

;;
;;      -- Functions --
;; ---------------------------------------------------------------

;; Special values:
;;   `gnu'         compiled for a GNU Hurd system.
;;   `gnu/linux'   compiled for a GNU/Linux system.
;;   `darwin'      compiled for Darwin (GNU-Darwin, Mac OS X, ...).
;;   `ms-dos'      compiled as an MS-DOS application.
;;   `windows-nt'  compiled as a native W32 application.
;;   `cygwin'      compiled using the Cygwin library.
;; Anything else indicates some sort of Unix system.
;;
;; Example Usage:
;; (if-system gnu/linux
;;   (message "Free as in Beer")
;;   (message "Free as in Freedom!"))

(defmacro if-system (type &rest body)
  "Evaluate BODY if `system-type' equals TYPE."
  (declare (indent defun))
  `(when (eq system-type ',type)
     ,@body))

(defun save-buffers-without-asking ()
  (interactive)
  (save-some-buffers t))


(load "compile")

;;
;;      -- Settings --
;; ---------------------------------------------------------------
(setq compile-command "make") ;; NOTE: make is the default compile command. Change on a per-language basis

;; Clean up window
(menu-bar-mode -1)
(scroll-bar-mode -1)
(tool-bar-mode -1)

;; Put all the backup files in an ~/.emacs.d/backup dir
(setq backup-directory-alist '(("." . "~/.emacs.d/auto-saves")))
(setq create-lockfiles nil)
(setq auto-save-default nil) ;; IMPORTANT(craig) - remove this if it becomes necessary

(setq next-line-add-newlines nil)
(setq truncate-partial-width-windows nil)
(setq-default truncate-lines t)

;; Stop Emacs from losing undo information by setting high limits
(setq undo-limit 20000000)
(setq undo-strong-limit 40000000)

;; Set the OSX's CMD key as the meta key
(if-system darwin
  (setq mac-option-key-is-meta nil)
  (setq mac-command-key-is-meta t)
  (setq mac-command-modifier 'meta)
  (setq mac-option-modifier nil))

;; turn on the column numbers in modeline
(setq column-number-mode 1)

;; Turn off the bell
(defun nil-bell ())
(setq ring-bell-function 'nil-bell)

;; Auto revert files that change on the hard disk
(global-auto-revert-mode 1)

;; Treat emacs 'symbol' as a word
(with-eval-after-load 'evil
  (defalias #'forward-evil-word #'forward-evil-symbol))

;;
;;      -- Theme --
;; ---------------------------------------------------------------
