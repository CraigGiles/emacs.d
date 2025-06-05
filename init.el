
(package-initialize)

(menu-bar-mode 0)
(tool-bar-mode 0)
(scroll-bar-mode 0)
(global-display-line-numbers-mode 0)

;;
;;      -- Settings --
;; -----------------------------------------------------------------
(setq inhibit-splash-screen t)
(setq inhibit-startup-message t)


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

(global-set-key (kbd "M-f") 'find-file)

;;
;;      -- Font Scaling --
;; -----------------------------------------------------------------
;; Add font scaling with CTRL+= and CTRL+-
(global-set-key (kbd "C-=") 'text-scale-increase)
(global-set-key (kbd "C--") 'text-scale-decrease)
(global-set-key (kbd "C-0") 'text-scale-adjust) ;; Reset font size
;; Set the OSX's CMD key as the meta key

;;
;;      -- Loading --
;; -----------------------------------------------------------------
(add-to-list 'load-path (expand-file-name "local" user-emacs-directory))

;; NOTE: this will create the custom.el file if it does not exist
(setq custom_file_path (expand-file-name "custom.el" user-emacs-directory))
(if (file-exists-p custom_file_path)
    ()
    (make-empty-file custom_file_path)
)

(setq custom-file custom_file_path)
(load custom_file_path)

(load-file (expand-file-name "gilesc-theme.el" user-emacs-directory))
(load-file (expand-file-name "compile.el" user-emacs-directory))
(load-file (expand-file-name "untabify.el" user-emacs-directory))
(load-file (expand-file-name "system.el" user-emacs-directory))
(load-file (expand-file-name "rc.el" user-emacs-directory))

;;
;;      -- Packages --
;; -----------------------------------------------------------------
(rc/require 'ag)
(rc/require 'magit)
(rc/require 'counsel-projectile)
(rc/require 'yaml-mode)
(rc/require 'toml-mode)
(rc/require 'glsl-mode)
(rc/require 'cmake-mode)
(rc/require 'csharp-mode)
(rc/require 'markdown-mode)

(counsel-projectile-mode 1)

;;
;;      -- Evil --
;; -----------------------------------------------------------------
(rc/require 'evil)
(rc/require 'evil-escape)
(rc/require 'evil-commentary)
(rc/require 'evil-search-highlight-persist)
(rc/require 'use-package-chords)
(rc/require 'undo-tree)

(setq evil-want-keybinding nil) ; NOTE: must be set before evil-collection
(rc/require 'evil-collection)

(evil-mode 1)
(evil-escape-mode 1)
(key-chord-mode 1)
(evil-commentary-mode 1)
(global-evil-search-highlight-persist 1)

(setq undo-tree-auto-save-history t)
(setq undo-tree-history-directory-alist '(("." . "~/.emacs.d/undo")))
(global-undo-tree-mode 1)
(evil-set-undo-system 'undo-tree)

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

(define-key special-mode-map (kbd "C-k") 'scroll-up-command)
(define-key special-mode-map (kbd "C-j") 'scroll-down-command)

(evil-define-key 'normal magit-mode-map [tab] 'magit-section-toggle)
(evil-define-key 'normal magit-blame-mode-map (kbd "g q") 'magit-blame-quit)
(evil-define-key 'normal magit-mode-map (kbd "C-r") 'magit-status)

;; Treat emacs 'symbol' as a word
(with-eval-after-load 'evil
  (defalias #'forward-evil-word #'forward-evil-symbol))

;;
;;      -- fixme hilighting --
;; -----------------------------------------------------------------
(require 'fixme-mode)
(setq fixme-modes '(markdown-mode emacs-lisp-mode prog-mode fundamental-mode jai-mode python-mode))
(initialize-fixme-modes)

;;
;;      -- C/CPP --
;; -----------------------------------------------------------------
(rc/require 'cc-mode)

(add-to-list 'auto-mode-alist '("\\.[hc]\\(pp\\)?\\'" . c++-mode))
(add-to-list 'auto-mode-alist '("\\makefile$" . make-mode))
(add-to-list 'auto-mode-alist '("\\Makefile$" . make-mode))

;; (setq split-width-threshold 1600)
(defun never-split-a-window ()
    "Never, ever split a window."
    nil)
(setq split-window-preferred-function 'never-split-a-window)

(defun c-settings ()
  (define-key c++-mode-map "\em" 'compile-project)

  (setq build-file-name "build.bat"
        compile-command "call build.bat"
        c-default-style "ellemtel"
        c-basic-offset 4)

  (add-to-list 'fixme-modes 'make-mode)
  (add-to-list 'fixme-modes 'c++-mode)
  (add-to-list 'fixme-modes 'cc-mode)
  (add-to-list 'fixme-modes 'c-mode)
  (initialize-fixme-modes)

  (message "Applied custom C/CPP settings"))

(add-hook 'c++-mode-hook 'c-settings)

;;
;;      -- jai --
;; -----------------------------------------------------------------
(require 'jai-mode)

(defun jai-settings ()
  (define-key jai-mode-map "\em" 'compile-project)

  (add-to-list 'fixme-modes 'jai-mode)
  (initialize-fixme-modes)

  (setq build-file-name "first.jai"
        compile-command "jai first.jai")

  (message "Applied custom JAI settings"))

(add-hook 'jai-mode-hook 'jai-settings)

;;
;;      -- python --
;; -----------------------------------------------------------------
(setq py-shell-name "py")
(defun python-settings ()
  (define-key python-mode-map "\em" 'compile-project)

  (add-to-list 'fixme-modes 'python-mode)
  (initialize-fixme-modes)

  (setq build-file-name "build.bat"
        compile-command "call build.bat")

  (message "Applied custom PYTHON settings"))

(add-hook 'python-mode-hook 'python-settings)

;;
;;      -- Required Modes --
;; -----------------------------------------------------------------
