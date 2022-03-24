(global-set-key (kbd "M-f") 'find-file)

(add-to-list 'load-path "~/.emacs.d/lisp/")
(require 'gilesc-theme)

(setq custom-file (expand-file-name "custom.el" user-emacs-directory))
(load custom-file)

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
(setq mac-option-key-is-meta nil)
(setq mac-command-key-is-meta t)
(setq mac-command-modifier 'meta)
(setq mac-option-modifier nil)

;; turn on the column numbers in modeline
(setq column-number-mode 1)

;; Turn off the bell on Mac OS X
(defun nil-bell ())
(setq ring-bell-function 'nil-bell)

;; Create a big horizontal blue bar so i don't keep loosing my cursor
(global-hl-line-mode 1)
(global-visual-line-mode 1)

;; Auto revert files that change on the hard disk
(global-auto-revert-mode 1)


(require 'package)
(setq package-archives '(("melpa" . "https://melpa.org/packages/")
                         ("org" . "https://orgmode.org/elpa/")
                         ("elpa" . "https://elpa.gnu.org/packages/")))

(setq package-archive-priorities '(("melpa" . 1)
				   ("elpa" . 2)
				   ("org" . 3)))
(package-initialize)
(unless package-archive-contents
        (package-refresh-contents))

;; Initialize use-package on non-Linux platforms
(unless (package-installed-p 'use-package)
   (package-install 'use-package))

(require 'use-package)
(setq use-package-always-ensure t)

(use-package ivy
  :diminish
  :bind (("C-s" . swiper)
         :map ivy-minibuffer-map
         ("TAB" . ivy-alt-done)
         ("C-l" . ivy-alt-done)
         ("C-j" . ivy-next-line)
         ("C-k" . ivy-previous-line)
         :map ivy-switch-buffer-map
         ("C-k" . ivy-previous-line)
         ("C-l" . ivy-done)
         ("C-d" . ivy-switch-buffer-kill)
         :map ivy-reverse-i-search-map
         ("C-k" . ivy-previous-line)
         ("C-d" . ivy-reverse-i-search-kill))
  :config (ivy-mode 1)
)

(use-package evil
  :ensure t
  :bind (:map evil-normal-state-map
	      ("j" . 'evil-next-visual-line)
	      ("k" . 'evil-previous-visual-line)
	      ("-" . 'find-file)
	      ("C-u" . 'evil-scroll-up)
	      ("C-f" . 'ag-project-at-point)
	      ("C-c C-c" . 'eval-buffer)
	      ([tab] . 'evil-toggle-fold)
	      ("M-6" . 'switch-other-window-to-last-buffer)
	      ("SPC n" . 'evil-search-highlight-persist-remove-all)
	      ("C-w C-h" . 'evil-window-left)
	      ("C-w C-l" . 'evil-window-right)
	      ("C-p" . 'evil-backward-paragraph)
	      ("C-n" . 'evil-forward-paragraph)
	      ("M-j" . 'counsel-imenu)
	      ("C-f" . 'ag-project-at-point)
	      ("C-k" . 'evil-backward-paragraph)
	      ("C-j" . 'evil-forward-paragraph)
	      ("M-b" . 'counsel-ibuffer)
	      ("M-n" . 'next-error)
	      ("M-C-n" . 'previous-error)

	      :map evil-visual-state-map
	      ("C-u" . 'evil-scroll-up)
	      ("C-k" . 'evil-backward-paragraph)
	      ("C-j" . 'evil-forward-paragraph)

	      :map evil-insert-state-map
	      ("C-u" . (lambda ()
			 (interactive)
			 (evil-delete (point-at-bol) (point))))
	      )
  :init
    (setq evil-want-integration t) ;; This is optional since it's already set to t by default.
    (setq evil-want-keybinding nil)

  :config
    (setq evil-vsplit-window-right t)
    (setq evil-split-window-below t)

    (evil-mode t)
)

(use-package evil-commentary
  :after evil
  :init
    (evil-commentary-mode t))

(use-package evil-escape
  :diminish
  :after evil
  :config
  (evil-escape-mode))

(use-package evil-search-highlight-persist
  :diminish
  :after evil
  :config
  (global-evil-search-highlight-persist t))

(use-package use-package-chords
  :diminish
  :after evil
  :config
  (key-chord-mode 1)
  (setq key-chord-two-keys-delay 0.1)
  (key-chord-define evil-insert-state-map "Jj" 'evil-escape)
  (key-chord-define evil-insert-state-map "JJ" 'evil-escape)
  (key-chord-define evil-insert-state-map "jj" 'evil-escape)
  (key-chord-define evil-normal-state-map "gc" 'evil-commentary-line))

(use-package magit
  :config
    (evil-define-key 'normal magit-mode-map [tab] 'magit-section-toggle)
    (evil-define-key 'normal magit-blame-mode-map (kbd "g q") 'magit-blame-quit)
    (evil-define-key 'normal magit-mode-map (kbd "C-r") 'magit-status))

(use-package evil-collection
  :after evil
  :ensure t
  :config
  (evil-collection-init))

(use-package which-key
  :init (which-key-mode)
  :diminish which-key-mode
  :config (setq which-key-idle-delay 0.3))

(use-package helpful
  :ensure t
  :bind
  ([remap describe-function] . helpful-function)
  ([remap describe-command] . helpful-command)
  ([remap describe-variable] . helpful-variable)
  ([remap describe-key] . helpful-key))

(with-eval-after-load 'evil
  (defalias #'forward-evil-word #'forward-evil-symbol))

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

;; Changing some default bindings for special mode
(define-key special-mode-map (kbd "C-p") 'scroll-up-command)
(define-key special-mode-map (kbd "C-n") 'scroll-down-command)
