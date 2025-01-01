(package-initialize)

(add-to-list 'load-path (expand-file-name "local" user-emacs-directory))
(setq custom-file (expand-file-name "custom.el" user-emacs-directory))
(load custom-file)

(load-file (expand-file-name "rc.el" user-emacs-directory))
(load-file (expand-file-name "gilesc-theme.el" user-emacs-directory))
(load-file (expand-file-name "compile-functions.el" user-emacs-directory))

(setq inhibit-splash-screen t)
(setq inhibit-startup-message t)

(menu-bar-mode 0)
(tool-bar-mode 0)
(scroll-bar-mode 0)
(global-display-line-numbers-mode 1)

(ido-mode 0)
;; (ido-everywhere 1)

(rc/require 'ag)
(rc/require 'magit)
(rc/require 'counsel-projectile)
(counsel-projectile-mode 1)

(global-set-key (kbd "M-f") 'find-file)

(defun untabify-except-makefiles ()
  "Replace tabs with spaces except in makefiles."
  (unless (derived-mode-p 'makefile-mode)
    (untabify (point-min) (point-max))))

(add-hook 'before-save-hook 'delete-trailing-whitespace)
(add-hook 'before-save-hook 'untabify-except-makefiles)

;;
;;      -- Evil --
;; -----------------------------------------------------------------
(rc/require 'evil)
(rc/require 'evil-escape)
(rc/require 'evil-commentary)
(rc/require 'evil-search-highlight-persist)
(rc/require 'use-package-chords)

(setq evil-want-keybinding nil) ; NOTE: must be set before evil-collection
(rc/require 'evil-collection)

(evil-mode 1)
(evil-escape-mode 1)
(key-chord-mode 1)
(evil-commentary-mode 1)
(global-evil-search-highlight-persist 1)

(evil-collection-init)

(setq evil-want-integration t) ;; This is optional since it's already set to t by default.
(setq evil-want-keybinding nil)
(setq evil-vsplit-window-right t)
(setq evil-split-window-below t)

(with-eval-after-load 'evil-maps
  ;; Normal Mode
  (define-key evil-normal-state-map (kbd "j") 'evil-next-visual-line)
  (define-key evil-normal-state-map (kbd "k") 'evil-previous-visual-line)
  (define-key evil-normal-state-map (kbd "-") 'find-file)
  (define-key evil-normal-state-map (kbd "C-f") 'ag-project-at-point)
  (define-key evil-normal-state-map (kbd "C-u") 'evil-scroll-up)
  (define-key evil-normal-state-map (kbd "M-p") 'counsel-projectile-find-file)
  (define-key evil-normal-state-map (kbd "C-p") 'counsel-projectile-find-file)
  (define-key evil-normal-state-map (kbd "C-c C-c") 'eval-buffer)
  (define-key evil-normal-state-map [tab]  'evil-toggle-fold)
  (define-key evil-normal-state-map (kbd "M-j") 'counsel-imenu)
  (define-key evil-normal-state-map (kbd "M-6") 'switch-other-window-to-last-buffer)
  (define-key evil-normal-state-map (kbd "SPC n") 'evil-search-highlight-persist-remove-all)
  (define-key evil-normal-state-map (kbd "C-w C-h") 'evil-window-left)
  (define-key evil-normal-state-map (kbd "C-w C-l") 'evil-window-right)
  (define-key evil-normal-state-map (kbd "C-k") 'evil-backward-paragraph)
  (define-key evil-normal-state-map (kbd "C-j") 'evil-forward-paragraph)
  (define-key evil-normal-state-map (kbd "C-k") 'evil-backward-paragraph)
  (define-key evil-normal-state-map (kbd "C-j") 'evil-forward-paragraph)
  (define-key evil-normal-state-map (kbd "C-e") 'end-of-line)
  (define-key evil-normal-state-map (kbd "C-a") 'beginning-of-line)
  (define-key evil-normal-state-map (kbd "M-n") 'next-error)
  (define-key evil-normal-state-map (kbd "M-C-n") 'previous-error)
  (define-key evil-normal-state-map (kbd "M-b") 'counsel-ibuffer)

  ;; Visual Mode
  (define-key evil-visual-state-map (kbd "C-u") 'evil-scroll-up)

  ;; Insert Mode
  (define-key evil-insert-state-map (kbd "C-u") (lambda ()
                                                (interactive)
                                                (evil-delete (point-at-bol) (point))))
)

(setq key-chord-two-keys-delay 0.1)
(key-chord-define evil-insert-state-map "Jj" 'evil-escape)
(key-chord-define evil-insert-state-map "JJ" 'evil-escape)
(key-chord-define evil-insert-state-map "jj" 'evil-escape)
(key-chord-define evil-normal-state-map "gc" 'evil-commentary-line)

(evil-define-key 'normal magit-mode-map [tab] 'magit-section-toggle)
(evil-define-key 'normal magit-blame-mode-map (kbd "g q") 'magit-blame-quit)
(evil-define-key 'normal magit-mode-map (kbd "C-r") 'magit-status)

;;
;;      -- C/CPP --
;; -----------------------------------------------------------------
(require 'simpc-mode)
(add-to-list 'auto-mode-alist '("\\.[hc]\\(pp\\)?\\'" . simpc-mode))

;;
;;      -- jai --
;; -----------------------------------------------------------------
(require 'jai-mode)
(add-to-list 'auto-mode-alist '("\\.jai$" . jai-mode))

(defun my-jai-mode-hook ()
    (add-to-list
     'compilation-error-regexp-alist
     (list "^\\([A-Za-z]:.+?\\):\\([0-9]+\\),\\([0-9]+\\):.*"
           1   ;FILE
           2   ;LINE
           3)) ;COLUMN

    (setq tab-stop 4)
    (setq indent-tabs-mode nil)

    (define-key jai-mode-map "\em" 'make-without-asking)

    (setq build-file-name "build.bat")
    (setq compile-command "call build.bat")

    (message "Jai hook added")
)
(add-hook 'jai-mode-hook 'my-jai-mode-hook)

;;
;;      -- jai --
;; -----------------------------------------------------------------
(rc/require 'markdown-mode)
(add-to-list 'auto-mode-alist '("\\.md$" . markdown-mode))

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

;; Auto revert files that change on the hard disk
(global-auto-revert-mode 1)

;; Treat emacs 'symbol' as a word
(with-eval-after-load 'evil
  (defalias #'forward-evil-word #'forward-evil-symbol))
