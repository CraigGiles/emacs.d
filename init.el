(add-to-list 'load-path "~/.emacs.d/lisp/")

(setq custom-file (expand-file-name "custom.el" user-emacs-directory))
(load custom-file)
(require 'gilesc-theme)

(setq fixme-modes '(markdown-mode emacs-lisp-mode prog-mode fundamental-mode jai-mode))
(require 'fixme-mode)
(initialize-fixme-modes)


(global-set-key (kbd "M-f") 'find-file)
(setq compile-command "make") ;; NOTE: make is the default compile command. Change on a per-language basis

;; Clean up window
(menu-bar-mode -1)
(scroll-bar-mode -1)
(tool-bar-mode -1)


;;
;;   --- Functions ---
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
;; ---------------------------------------------------------------
(defmacro if-system (type &rest body)
  "Evaluate BODY if `system-type' equals TYPE."
  (declare (indent defun))
  `(when (eq system-type ',type)
     ,@body))


(defun set-programming-mode ()
  "Set the font size for emacs"
  (interactive)
  (set-face-attribute 'default nil :height 128)
)

(defun set-streaming-mode ()
  "Set the font size for emacs"
  (interactive)
  (set-face-attribute 'default nil :height 200)
)

;; All code within an #if 0 block should be set to the comment color
(defun if0-font-lock (limit)
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

(if-system darwin
    (setq notes-directory "~/Development/notes/")
)

(if-system windows-nt
    (setq notes-directory "w:/notes")
)

(defun save-buffers-without-asking ()
  "Saves all loaded buffers without prompting."
  (interactive)
  (save-some-buffers t))


(defun load-notes-directory ()
    (interactive)
    (find-file notes-directory)
)

(defun untabify-except-makefiles ()
  "Replace tabs with spaces except in makefiles."
  (unless (derived-mode-p 'makefile-mode)
    (untabify (point-min) (point-max))))

(add-hook 'before-save-hook 'delete-trailing-whitespace)
(add-hook 'before-save-hook 'untabify-except-makefiles)


;; ===============================================================
;;   Package Management
;; ---------------------------------------------------------------
(require 'package)

;; find packages in melpa or for org mode, orgmode over elpa
;; (or marmalade for that matter)
(add-to-list 'package-archives '("melpa" . "http://melpa.org/packages/") t)
(add-to-list 'package-archives '("org" . "http://orgmode.org/elpa/") t)
(setq package-archive-priorities
      '(("melpa" . 1)
        ("org" . 2)))

(package-initialize)

;; we're going to be using use-package to define and load packages.
;; it's a code based configuration with a bunch of fancy extensions
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

(require 'use-package)
(setq use-package-always-ensure t)

(defvar bootstrap-version)
(let ((bootstrap-file
       (expand-file-name "straight/repos/straight.el/bootstrap.el" user-emacs-directory))
      (bootstrap-version 5))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
        (url-retrieve-synchronously
         "https://raw.githubusercontent.com/raxod502/straight.el/develop/install.el"
         'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))

;; ===============================================================
;;   Installed Packages
;; ---------------------------------------------------------------
(require 'kotlin-mode)
(require 'jai-mode)


;; Note: Should look over this config file at some point.
;;       https://github.com/krisajenkins/EvilBegins/blob/master/.emacs
(use-package evil
  :after counsel
  :init
    (setq evil-want-integration t) ;; This is optional since it's already set to t by default.
    (setq evil-want-keybinding nil)
    (setq evil-vsplit-window-right t)
    (setq evil-split-window-below t)

    (evil-mode t)

  :bind (:map evil-normal-state-map
              ("j" . 'evil-next-visual-line)
              ("k" . 'evil-previous-visual-line)
              ("-" . 'find-file)
              ("C-f" . 'ag-project-at-point)
              ("C-u" . 'evil-scroll-up)
              ("M-p" . 'counsel-projectile-find-file)
              ("C-p" . 'counsel-projectile-find-file)
              ("C-c C-c" . 'eval-buffer)
              ([tab] . 'evil-toggle-fold)
              ("M-j" . 'counsel-imenu)
              ("M-6" . 'switch-other-window-to-last-buffer)
              ("SPC n" . 'evil-search-highlight-persist-remove-all)
              ("C-w C-h" . 'evil-window-left)
              ("C-w C-l" . 'evil-window-right)
              ("C-k" . 'evil-backward-paragraph)
              ("C-j" . 'evil-forward-paragraph)
              ("C-k" . 'evil-backward-paragraph)
              ("C-j" . 'evil-forward-paragraph)
              ("C-e" . 'end-of-line)
              ("C-a" . 'beginning-of-line)

              ;; Navigating Errors
              ([f9] . 'first-error)
              ("M-n" . 'next-error)
              ("M-C-n" . 'previous-error)

              ;; Switching Buffers
              ("M-b"  . 'counsel-ibuffer)
              ("C-M-b"  . (lambda () ;; Switch buffer other window
                            (interactive)
                            (other-window 1)
                            (counsel-ibuffer)))

              )(:map evil-visual-state-map
              ("C-u" . 'evil-scroll-up)
              )(:map evil-insert-state-map
              ("C-u" . (lambda ()
                         (interactive)
                         (evil-delete (point-at-bol) (point))))
              )
)


(use-package evil-escape
  :after evil
  :config
  (evil-escape-mode))

(use-package evil-search-highlight-persist
  :after evil
  :config
  (global-evil-search-highlight-persist t))

(use-package use-package-chords
  :after evil
  :config (key-chord-mode 1)
          (setq key-chord-two-keys-delay 0.1)
          (key-chord-define evil-insert-state-map "Jj" 'evil-escape)
          (key-chord-define evil-insert-state-map "JJ" 'evil-escape)
          (key-chord-define evil-insert-state-map "jj" 'evil-escape)
          (key-chord-define evil-normal-state-map "gc" 'evil-commentary-line))

(use-package evil-commentary
  :after evil
  :init (evil-commentary-mode t))

(use-package evil-collection
  :after evil
  :ensure t
  :config (evil-collection-init))

(use-package counsel-projectile
  :ensure t
  :defer t
  :init (counsel-projectile-mode t))

(use-package ag
  :ensure t
  :defer t
  :commands (ag ag-regexp ag-project))

(use-package cc-mode
  :defer t
  :config
    (add-hook 'before-save-hook 'fix-format-buffer t)

    ;; File Extensions and which mode they're associated with
    (add-to-list 'auto-mode-alist '("\\.cpp$"     . c++-mode))
    (add-to-list 'auto-mode-alist '("\\.h$"       . c++-mode))
    (add-to-list 'auto-mode-alist '("\\.hpp$"     . c++-mode))
    (add-to-list 'auto-mode-alist '("\\.c$"       . c++-mode))
    (add-to-list 'auto-mode-alist '("\\.cc$"      . c++-mode))
    (add-to-list 'auto-mode-alist '("\\makefile$" . make-mode))
    (add-to-list 'auto-mode-alist '("\\Makefile$" . make-mode))
    (add-to-list 'auto-mode-alist '("\\*"         . markdown-mode))
    (add-to-list 'auto-mode-alist '("\\.m$"       . c++-mode))
    (add-to-list 'auto-mode-alist '("\\.mm$"      . c++-mode))

    (add-to-list 'fixme-modes 'make-mode)
    (add-to-list 'fixme-modes 'c++-mode)
    (add-to-list 'fixme-modes 'cc-mode)
    (add-to-list 'fixme-modes 'c-mode)
    (initialize-fixme-modes)

    (if-system darwin
                 (setq build-file-name "build.sh")
                 (setq compile-command "./build.sh"))

    (if-system windows-nt
                 (setq build-file-name "build.bat")
                 (setq compile-command "build.bat"))

    (setq tab-width 4
          indent-tabs-mode nil)

    (setq gdb-many-windows t
          gdb-use-separate-io-buffer t)

    (setq c-default-style "ellemtel"
          c-basic-offset 4)

    (defun my-c-mode-common-hook ()
      (font-lock-add-keywords
       nil
       '((if0-font-lock (0 font-lock-comment-face prepend))) 'add-to-end))

    (add-hook 'c-mode-common-hook 'my-c-mode-common-hook)

    (defun my-c-mode-common-hook ()
      (font-lock-add-keywords
       nil
       '((if0-font-lock (0 font-lock-comment-face prepend))) 'add-to-end))

    (add-hook 'c-mode-common-hook 'my-c-mode-common-hook)

    (define-key c++-mode-map [f12] 'craig-find-corresponding-file)
    (define-key c++-mode-map [M-f12] 'craig-find-corresponding-file-other-window)
    (define-key c++-mode-map (kbd "M-m") 'make-without-asking)
    (define-key c++-mode-map (kbd "M-M") 'run-without-asking)
    (define-key c++-mode-map (kbd "M-j") 'counsel-imenu)

) ;; use-package cc-mode

(use-package go-mode
  :defer t
  :init
  :config

    (setq tab-width 4
          indent-tabs-mode nil)

    (setq-default indent-tabs-mode nil)
    (setq-default tab-width 4)
    (setq indent-line-function 'insert-tab)

    (define-key go-mode-map "\em" 'make-without-asking)
    (define-key go-mode-map (kbd "C-M-m") 'test-without-asking)

    ;; File Extensions and which mode they're associated with
    (add-to-list 'auto-mode-alist '("\\.go$"      . go-mode))
    (add-to-list 'auto-mode-alist '("\\makefile$" . make-mode))
    (add-to-list 'auto-mode-alist '("\\Makefile$" . make-mode))

    (add-to-list 'fixme-modes 'go-mode)
    (initialize-fixme-modes)

    (setq build-file-name "build.sh")
    (setq compile-command "make")

    (evil-define-key 'normal go-mode-map (kbd "C-b") 'godef-jump)
    (add-hook 'before-save-hook 'gofmt-before-save)

) ;; go-mode

(use-package csharp-mode
  :mode "\\.cs\\'"
  :hook (csharp-mode . my-csharp-mode)
  :config
    (defun my-csharp-mode ()
        (omnisharp-mode 1)
        (flycheck-mode 1)
    )
)

(defun godot-run-main-scene ()
    (interactive)
    (save-buffers-without-asking)
    (gdscript-godot-run-project-debug))

(setq gdscript-godot-executable "W:/godot/Godot_v4.0-alpha14_win64.exe")
(use-package gdscript-mode
    :straight (gdscript-mode
               :type git
               :host github
               :repo "godotengine/emacs-gdscript-mode")
    :config
        (define-key gdscript-mode-map "<f5>" 'godot-run-main-scene)
        (define-key gdscript-mode-map [f5] 'godot-run-main-scene))

;; (use-package omnisharp
;;   :bind
;;   (:map csharp-mode-map
;;         ("<f12>" . omnisharp-go-to-definition)
;;         ("S-<f12>" . omnisharp-find-usages))
;;   :config
;;   ;;(setq omnisharp-server-executable-path "~/.emacs.d/.cache/omnisharp/server/v1.32.18/run")
;;   (setq omnisharp-server-executable-path "~/.vscode/extensions/ms-vscode.csharp-1.20.0/.omnisharp/1.32.20/run")
;;   (add-to-list 'company-backends #'company-omnisharp))

;;   --- Kotlin Mode ---
;; ---------------------------------------------------------------
(defun my-kotlin-mode-hook ()
    (add-to-list 'auto-mode-alist '("\\.kt$" . kotlin-mode))
    (add-to-list 'fixme-modes 'kotlin-mode)
    (initialize-fixme-modes)
    (projectile-mode)

    (setq tab-stop 4)
    (setq indent-tabs-mode nil)

    (define-key kotlin-mode-map "\em" 'make-without-asking)
    (define-key kotlin-mode-map (kbd "C-M-m") 'test-without-asking)

    (setq build-file-name "build.gradle.kts")
    (setq compile-command "../gradlew compileKotlin")
    (message "Kotlin hook added")
)

(add-hook 'kotlin-mode-hook 'my-kotlin-mode-hook)

;;
;;   --- Jai Mode ---
;; ---------------------------------------------------------------
(defun my-jai-mode-hook ()
    (add-to-list 'auto-mode-alist '("\\.jai$" . jai-mode))
    (add-to-list 'fixme-modes 'jai-mode)
    (initialize-fixme-modes)
    (projectile-mode)

    (add-to-list
     'compilation-error-regexp-alist
     (list "^\\([A-Za-z]:.+?\\):\\([0-9]+\\),\\([0-9]+\\):.*"
           1   ;FILE
           2   ;LINE
           3)) ;COLUMN

    (setq tab-stop 4)
    (setq indent-tabs-mode nil)

    (define-key jai-mode-map "\em" 'make-without-asking)
    (define-key jai-mode-map (kbd "C-M-m") 'test-without-asking)

    (setq build-file-name "build.bat")
    (setq compile-command "call build.bat")
    (message "Jai hook added")
)

(add-hook 'jai-mode-hook 'my-jai-mode-hook)



(use-package magit
  :defer t
  :after evil-collection
  :config
    (evil-define-key 'normal magit-mode-map [tab] 'magit-section-toggle)
    (evil-define-key 'normal magit-blame-mode-map (kbd "g q") 'magit-blame-quit)
    (evil-define-key 'normal magit-mode-map (kbd "C-r") 'magit-status)
)

(use-package undo-tree
  :ensure t
  :after evil
  :diminish
  :config
    (setq undo-tree-auto-save-history t)
    (setq undo-tree-history-directory-alist '(("." . "~/.emacs.d/undo")))
    (evil-set-undo-system 'undo-tree)
    (global-undo-tree-mode 1))

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

(use-package markdown-mode
  :init
  :config (flyspell-mode 1))

(dolist (hook '(text-mode-hook))
  (add-hook hook (lambda () (flyspell-mode 1))))

;; ===============================================================
;;   keymap key-bindings keybindings
;; ---------------------------------------------------------------

;; Changing some default bindings for special mode
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

;; TODO(craig): iterate through all modes i'm using and use the iterator to test
;;     (setq loaded-modes '())
;;     (add-to-list 'loaded-modes 'markdown-mode)
;;     (add-to-list 'loaded-modes 'scala-mode)
;; (defun save-buffers-without-asking ()
;;   "Saves all loaded buffers without prompting."
;;   (interactive)
;;   (save-some-buffers 'no-confirm (lambda ()
;;                                    (cond
;;                                     ((and buffer-file-name (equal buffer-file-name abbrev-file-name)))
;;                                     ((and buffer-file-name (eq major-mode 'markdown-mode)))
;;                                     ((and buffer-file-name (eq major-mode 'c-mode)))
;;                                     ((and buffer-file-name (eq major-mode 'cc-mode)))
;;                                     ((and buffer-file-name (eq major-mode 'c++-mode)))
;;                                     ((and buffer-file-name (eq major-mode 'kotlin-mode)))
;;                                     ((and buffer-file-name (eq major-mode 'scala-mode)))
;;                                     ((and buffer-file-name (eq major-mode 'go-mode)))
;;                                     ((and buffer-file-name (eq major-mode 'jai-mode)))
;;                                     ((and buffer-file-name (eq major-mode 'php-mode)))
;;                                     ((and buffer-file-name (eq major-mode 'emacs-lisp-mode)))
;;                                     ((and buffer-file-name (derived-mode-p 'org-mode)))))))

;; TODO create a function that asks if a vertical split exists (or if
;; there is only one window) if one window then split vertically and
;; use that new vertical split
(defun never-split-a-window ()
    "Never, ever split a window."
    nil)
(setq split-window-preferred-function 'never-split-a-window)

;; split right if only one window exists
(defun split-window-right-if-single-window ()
  "Test something"
  (interactive)
  (if (= (length (window-list)) 1) (split-window-right) nil))

;; Compile Settings
;; -------------------------------------------------------
(setq compilation-directory-locked nil)

(defun find-project-directory-recursive ()
  "Recursively search for a makefile."
  (interactive)
  (if (file-exists-p build-file-name) t
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
  (split-window-right-if-single-window)
  (save-buffers-without-asking)
  (if (find-project-directory) (compile compile-command))
  (other-window 1))

(defun craig-find-corresponding-file ()
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
  (if (string-match "\\.go" buffer-file-name)
      (setq CorrespondingFileName (concat BaseFileName "_test.go")))
  (if CorrespondingFileName (find-file CorrespondingFileName)
    (error "Unable to find a corresponding file")))

(defun craig-find-corresponding-file-other-window ()
  "Find the file that corresponds to this one."
  (interactive)
  (find-file-other-window buffer-file-name)
  (craig-find-corresponding-file)
  (other-window -1))

;; ===============================================================
;;   General Editor Settings
;; ---------------------------------------------------------------

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

;; Treat emacs 'symbol' as a word
(with-eval-after-load 'evil
  (defalias #'forward-evil-word #'forward-evil-symbol))
