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

    ;; ;; Use ensime to get the type at the point
    (key-chord-define evil-normal-state-map "et" 'ensime-type-at-point)
    (key-chord-define evil-normal-state-map "ed" 'ensime-edit-definition-other-window)
    )

  )

(with-eval-after-load 'evil
  (defalias #'forward-evil-word #'forward-evil-symbol))

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

;; IMPORTANT(craig): This causes a big lag spike
;; Ensure that emacs has the shell's PATH variables on osx
;; (use-package exec-path-from-shell
;;   :pin melpa-stable
;;   :init
;;   (when (memq window-system '(mac ns x))
;;     (exec-path-from-shell-initialize)))

(use-package ag
  :pin melpa-stable
  :config
  (setq ag-reuse-buffers 't))

(use-package scala-mode
  :pin melpa-stable
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

  ;; https://github.com/syl20bnr/spacemacs/issues/4746
  (setq ensime-sem-high-faces
	'(
	  (implicitConversion nil)
	  (var . (:foreground "#ff2222"))
	  (varField . (:foreground "#ff3333"))
	  (functionCall . (:foreground "#dc9157"))
	  (object . (:foreground "#D884E3"))
	  (operator . (:foreground "#cc7832"))
	  (object . (:foreground "#6897bb" :slant italic))
	  (package . (:foreground "yellow"))
	  (deprecated . (:strike-through "#a9b7c6"))
	  (implicitParams nil)
	  )
	;; ensime-completion-style 'company
	;; ensime-sem-high-enabled-p nil ;; disable semantic highlighting
	ensime-tooltip-hints t ;; disable type-inspecting tooltips
	ensime-tooltip-type-hints t ;; disable typeinspecting tooltips
	)

  :pin melpa-stable)

(use-package make-mode
  :pin melpa-stable
  :config
  (remove-hook 'before-save-hook 'fix-format-buffer t))

(use-package cc-mode)
(use-package compile)
(use-package ido)

(use-package 2048-game)
