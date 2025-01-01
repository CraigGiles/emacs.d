(package-initialize)

(add-to-list 'load-path (expand-file-name "local" user-emacs-directory))
(setq custom-file (expand-file-name "custom.el" user-emacs-directory))
(load custom-file)
(load-file (expand-file-name "rc.el" user-emacs-directory))

(load-file (expand-file-name "gilesc-theme.el" user-emacs-directory))

; (add-to-list 'default-frame-alist '(font . "Liberation Mono-12"))

(setq inhibit-splash-screen t)
(setq inhibit-startup-message t)

(menu-bar-mode 0)
(tool-bar-mode 0)
(scroll-bar-mode 0)
(ido-mode 1)
(ido-everywhere 1)
(global-display-line-numbers-mode 1)


; (rc/require 'smex)
; (global-set-key (kbd "M-x") 'smex)

(rc/require-theme 'gruber-darker)

; (rc/require 'ag)
(rc/require 'magit)

;;
;;      -- Evil --
;; -----------------------------------------------------------------
(rc/require 'evil)
(rc/require 'evil-escape)
(rc/require 'evil-commentary)
(setq evil-want-keybinding nil) ; NOTE: kept getting a warning to set this before evil-collection
(rc/require 'evil-collection)

(rc/require 'use-package-chords)

(evil-mode 1)
(evil-escape-mode 1)
(key-chord-mode 1)
(evil-commentary-mode 1)
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
;;      -- C --
;; -----------------------------------------------------------------
(require 'simpc-mode)
(add-to-list 'auto-mode-alist '("\\.[hc]\\(pp\\)?\\'" . simpc-mode))
