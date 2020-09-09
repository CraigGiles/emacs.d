;; NOTE: I am not a LISP programmer and I know next to nothing about the EMACS
;; ecosystem and programming environment. Coming from VIM, this could be
;; interesting...

;; NOTE to test this config launch emacs using
;; /Applications/Emacs.app/Contents/MacOS/Emacs -q -l ~/.emacs.d/init.el

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

;; ===============================================================
;;   General functions
;; ---------------------------------------------------------------

(defun switch-other-window-to-last-buffer ()
  "Switch other window to other window's last open buffer."
  (interactive)
  (other-window 1)
  (evil-switch-to-windows-last-buffer)
  (other-window 1))

;; TODO(craig): iterate through all modes i'm using and use the iterator to test
;;     (setq loaded-modes '())
;;     (add-to-list 'loaded-modes 'markdown-mode)
;;     (add-to-list 'loaded-modes 'scala-mode)
(defun save-buffers-without-asking ()
  "Saves all loaded buffers without prompting."
  (interactive)
  (save-some-buffers 'no-confirm (lambda ()
				   (cond
				    ((and buffer-file-name (equal buffer-file-name abbrev-file-name)))
				    ((and buffer-file-name (eq major-mode 'markdown-mode)))
				    ((and buffer-file-name (eq major-mode 'c-mode)))
				    ((and buffer-file-name (eq major-mode 'cc-mode)))
				    ((and buffer-file-name (eq major-mode 'c++-mode)))
				    ((and buffer-file-name (eq major-mode 'scala-mode)))
				    ((and buffer-file-name (eq major-mode 'go-mode)))
				    ((and buffer-file-name (eq major-mode 'jai-mode)))
				    ((and buffer-file-name (eq major-mode 'emacs-lisp-mode)))
				    ((and buffer-file-name (derived-mode-p 'org-mode)))))))

(defun save-buffer-untabify ()
  "Save the buffer after untabifying it."
  (interactive)
  (save-excursion
    (save-restriction
      (widen)
      (untabify (point-min) (point-max))))
  (save-buffer))

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
  (if CorrespondingFileName (find-file CorrespondingFileName)
    (error "Unable to find a corresponding file")))

(defun craig-find-corresponding-file-other-window ()
  "Find the file that corresponds to this one."
  (interactive)
  (find-file-other-window buffer-file-name)
  (craig-find-corresponding-file)
  (other-window -1))

;; All code within an #if 0 block should be set to the comment color
(defun my-c-mode-font-lock-if0 (limit)
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


;; ===============================================================
;;   Installed Packages
;; ---------------------------------------------------------------
(add-to-list 'load-path "~/.emacs.d/lisp/")
(load "jai-mode")

(use-package counsel-projectile
  :pin melpa
  :config
    (counsel-mode))

(use-package ag
  :pin melpa
  :init)

(use-package markdown-mode
  :pin melpa
  :init)

(use-package evil-commentary
  :pin melpa
  :init
    (evil-commentary-mode t))

;; NOTE: Should look over this config file at some point.
;;       https://github.com/krisajenkins/EvilBegins/blob/master/.emacs
(use-package evil
  :pin melpa
  :init
    (evil-mode t)

  :config
    (setq evil-vsplit-window-right t)
    (setq evil-split-window-below t)

    (define-key evil-normal-state-map (kbd "j") 'evil-next-visual-line)
    (define-key evil-normal-state-map (kbd "k") 'evil-previous-visual-line)

    (define-key evil-normal-state-map (kbd "-") 'find-file)

    (define-key evil-normal-state-map (kbd "M-p") 'counsel-fzf)
    (define-key evil-normal-state-map (kbd "C-p") 'counsel-fzf)

    (define-key evil-normal-state-map (kbd "C-c C-c") 'eval-buffer)

    ;; NOTE: This is the way to re-bind an ex command if i ever need it
    ;; (define-key evil-ex-map "e" 'counsel-fzf)

    (define-key evil-normal-state-map (kbd "C-f") 'ag-project-at-point)
    (define-key evil-normal-state-map (kbd "C-u") 'evil-scroll-up)
    (define-key evil-visual-state-map (kbd "C-u") 'evil-scroll-up)
    (define-key evil-insert-state-map (kbd "C-u") (lambda ()
      (interactive)
      (evil-delete (point-at-bol) (point)))
    )

     (use-package evil-escape
       :pin melpa
       :config
         (evil-escape-mode)
     ) 

    (use-package evil-search-highlight-persist
      :pin melpa
      :config
        (global-evil-search-highlight-persist t))

    (use-package use-package-chords
      :pin melpa
      :config
        (key-chord-mode 1)
        (setq key-chord-two-keys-delay 0.1)
        (key-chord-define evil-insert-state-map "Jj" 'evil-escape)
        (key-chord-define evil-insert-state-map "JJ" 'evil-escape)
        (key-chord-define evil-insert-state-map "jj" 'evil-escape)
        (key-chord-define evil-normal-state-map "gc" 'evil-commentary-line)
    ) ;; use-package-chords
) ;; evil

(use-package smooth-scrolling
  :pin melpa
  :config
    (setq scroll-margin 8
          scroll-conservatively 9999
          scroll-step 1))

(use-package magit
  :pin melpa
  :config
    (use-package evil-magit)
    (evil-define-key 'normal magit-mode-map [tab] 'magit-section-toggle)
    (key-chord-define evil-normal-state-map "ga" 'vc-annotate)
    (evil-define-key 'normal magit-blame-mode-map (kbd "g q") 'magit-blame-quit)
    (evil-define-key 'normal magit-mode-map (kbd "C-r") 'magit-status)
)

(use-package sbt-mode
  :pin melpa
  :defer t)

(use-package scala-mode
  :pin melpa
  :defer t
  :config

    ;; NOTE: My first scala-module plugin
    (require 'scala-package)
    (defun scala-insert-package-statement ()
      "Insert the scala package statement at the top of your source file"
        (interactive)
        (save-excursion
            (beginning-of-buffer)
            (insert (scala-create-package-statement (buffer-file-name)))
            (insert "\n")
        )
    )

    (defun sbt-save-and-switch ()
      "Saves the current buffer and switches to the active SBT window."
      (interactive)
      (save-buffers-without-asking)
      (sbt-switch-to-active-sbt-buffer)
      (sbt-clear)
      (other-window 1))

    (define-key scala-mode-map (kbd "M-m") 'sbt-save-and-switch)
    (evil-define-key 'normal scala-mode-map (kbd "C-b") 'sbt-find-definitions)
    (evil-define-key 'normal scala-mode-map (kbd "C-f") 'sbt-find-usages)
    (setq build-file-name "build.sbt")

    ;; File Extensions and which mode they're associated with
    (add-to-list 'auto-mode-alist '("\\.scala$" . scala-mode))
    (add-to-list 'auto-mode-alist '("\\.sc$" . scala-mode))
    (add-to-list 'auto-mode-alist '("\\.sbt$" . scala-mode))

    ;; TODO(craig) -- (add-fixme '('scala-mode 'sbt-mode))
    (add-to-list 'fixme-modes 'scala-mode)
    (add-to-list 'fixme-modes 'sbt-mode)
    (initialize-fixme-modes)

) ;; scala-mode

(use-package make-mode
  :pin melpa
  :defer t
  :config
    (remove-hook 'before-save-hook 'fix-format-buffer t))

(use-package cc-mode
  :pin melpa
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
    (add-to-list 'auto-mode-alist '("\\.m$"       . c++-mode))
    (add-to-list 'auto-mode-alist '("\\.mm$"      . c++-mode))

    (setq build-file-name "build.sh")

    (setq gdb-many-windows t
    	  gdb-use-separate-io-buffer t)

    (key-chord-define evil-normal-state-map "lb" 'gud-break)
    (key-chord-define evil-normal-state-map "lc" 'gud-cont)
    (key-chord-define evil-normal-state-map "ln" 'gud-next)
    (key-chord-define evil-normal-state-map "ls" 'gud-step)

    (load "gud-lldb")

    (setq c-default-style "ellemtel"
          c-basic-offset 4)
    
    (defun my-c-mode-common-hook ()
      (font-lock-add-keywords
       nil
       '((my-c-mode-font-lock-if0 (0 font-lock-comment-face prepend))) 'add-to-end))

    (add-hook 'c-mode-common-hook 'my-c-mode-common-hook)

    (define-key c++-mode-map [f12] 'craig-find-corresponding-file)
    (define-key c++-mode-map [M-f12] 'craig-find-corresponding-file-other-window)
    (define-key c++-mode-map [f7] 'make-without-asking)
    (define-key c++-mode-map (kbd "M-m") 'make-without-asking)
    (define-key c++-mode-map (kbd "M-j") 'imenu)

    (defun file-exists-hooks ()
      
	(defun c-header-format ()
	    "Format the given file as a header file."
	    (interactive)
	    (setq BaseFileName (file-name-sans-extension (file-name-nondirectory buffer-file-name)))
	    (insert "#if !defined(")
	    (push-mark)
	    (insert BaseFileName)
	    (upcase-region (mark) (point))
	    (pop-mark)
	    (insert "_H)\n")
	    (insert "\n")
	    (insert "#define ")
	    (push-mark)
	    (insert BaseFileName)
	    (upcase-region (mark) (point))
	    (pop-mark)
	    (insert "_H\n")
	    (insert "\n")
	    (insert "\n")
	    (insert "\n")
	    (insert "#endif")
	)

	(cond ((file-exists-p buffer-file-name) t)
	    ((string-match "[.]hpp" buffer-file-name) (c-header-format))
	    ((string-match "[.]h" buffer-file-name) (c-header-format))
	)
    )

    (add-hook 'c-mode-common-hook 'file-exists-hooks)

) ;; use-package cc-mode

(use-package go-mode
  :pin melpa
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

    (use-package company-go)

    (evil-define-key 'normal go-mode-map (kbd "C-b") 'godef-jump)
    (add-hook 'before-save-hook 'gofmt-before-save)

) ;; go-mode

(use-package vimrc-mode
  :pin melpa
  :defer t
  :init)

;; ===============================================================
;;   Stupid plugins that i don't need
;; ---------------------------------------------------------------
(use-package 2048-game
  :pin melpa
  :defer t)

;; ===============================================================
;;   General Editor Settings
;; ---------------------------------------------------------------
(with-eval-after-load 'evil
  (defalias #'forward-evil-word #'forward-evil-symbol))

(setq emacs-version-osx (string-equal system-type "darwin"))
(setq emacs-version-linux (string-equal system-type "gnu/linux"))
(setq emacs-version-windows (string-equal system-type "windows-nt"))

(setq custom-file "~/.emacs.d/custom.el")
(load custom-file)

(setq-default indent-tabs-mode nil)
(setq require-final-newline t)

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

;; When i say kill a buffer, i want you to kill 'this' buffer
(global-set-key (kbd "C-x k") 'kill-this-buffer)
(global-set-key (kbd "M-m") 'make-without-asking)

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
  
;; (setq split-width-threshold 80)
;; (setq split-height-threshold 160)
;; (setq split-window-preferred-function 'split-window-horizontally)
;; (setq split-window-preferred-function 'never-split-a-window)


(add-to-list 'auto-mode-alist '("\\.emacs$" . emacs-lisp-mode))
(add-to-list 'auto-mode-alist '("\\.md$"     . markdown-mode))
(add-to-list 'auto-mode-alist '("\\.thrift$". c-mode))
(add-to-list 'auto-mode-alist '("\\.gl$". c-mode))
(add-to-list 'auto-mode-alist '("\\.fs$". c-mode))
(add-to-list 'auto-mode-alist '("\\.vs$". c-mode))

;; ===============================================================
;;   keymap key-bindings keybindings
;; ---------------------------------------------------------------
(defun open-global-todo-file ()
  "Opens the global TODO file on the drive. Usually found in ~/Development/notes/todo.md"
  (interactive)
  (find-file "~/Development/notes/todo.md"))

(defun insert-line-above ()
  "Insert a new line above the current line"
  (interactive)
  (beginning-of-line)
  (newline)
  (previous-line)
)

(defun insert-line-below ()
  "Insert a new line below the current line"
  (interactive)
  (beginning-of-line)
  (next-line)
  (newline)
  (previous-line)
)

;; current buffer operations
;; (define-key global-map [f8] 'replace-string-without-moving)
(define-key global-map [f5] 'open-global-todo-file)
(define-key evil-normal-state-map [tab] 'evil-toggle-fold)
(define-key evil-normal-state-map (kbd "M-j") 'imenu)
(define-key evil-normal-state-map (kbd "M-6") 'switch-other-window-to-last-buffer)
(define-key evil-normal-state-map (kbd "SPC n") 'evil-search-highlight-persist-remove-all)

(define-key evil-normal-state-map (kbd "C-w C-h") 'evil-window-left)
(define-key evil-normal-state-map (kbd "C-w C-l") 'evil-window-right)

(define-key evil-normal-state-map (kbd "C-k") 'evil-backward-paragraph)
(define-key evil-normal-state-map (kbd "C-j") 'evil-forward-paragraph)
(define-key evil-visual-state-map (kbd "C-k") 'evil-backward-paragraph)
(define-key evil-visual-state-map (kbd "C-j") 'evil-forward-paragraph)
(define-key evil-normal-state-map (kbd "C-e") 'end-of-line)
(define-key evil-normal-state-map (kbd "C-a") 'beginning-of-line)
(define-key evil-normal-state-map (kbd "M-<up>") 'insert-line-above)
(define-key evil-normal-state-map (kbd "M-<down>") 'insert-line-below)

;; Navigating files
(define-key global-map (kbd "M-f" ) 'find-file)
(define-key global-map (kbd "M-F" ) 'find-file-other-window)

;; Changing some default bindings for special mode
(define-key special-mode-map (kbd "C-k") 'scroll-up-command)
(define-key special-mode-map (kbd "C-j") 'scroll-down-command)

;; Navigating windows
(define-key global-map (kbd "C-q") 'delete-other-windows)

;; Navigating buffers
(define-key global-map (kbd "M-b" ) 'counsel-ibuffer)
(define-key global-map (kbd "C-M-b" ) (lambda () ;; Switch buffer other window
  (interactive)
  (other-window 1)
  (counsel-ibuffer))
)

;; Navigating errors
(define-key global-map [f9] 'first-error)
(define-key global-map (kbd "M-n") 'next-error)
(define-key global-map (kbd "M-C-n") 'previous-error)

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

;; ===============================================================
;;   Fixme highlights
;; ---------------------------------------------------------------
(setq fixme-modes '(jai-mode c++-mode c-mode cc-mode markdown-mode emacs-lisp-mode prog-mode fundamental-mode))
(defun initialize-fixme-modes ()
  "Sets the highlighted words like TODO and NOTE and colorschemes for these words"
  (interactive)
  (make-face 'font-lock-todo-face)
  (make-face 'font-lock-done-face)
  (make-face 'font-lock-next-face)
  (make-face 'font-lock-progress-face)
  (make-face 'font-lock-bug-face)
  (make-face 'font-lock-cleanup-face)
  (make-face 'font-lock-speed-face)
  (make-face 'font-lock-important-face)
  (make-face 'font-lock-note-face)

  (mapc (lambda (mode)
	  (font-lock-add-keywords
	   mode
	   '(
	     ("\\<\\(BUG\\)" 1 'font-lock-bug-face t)
	     ("\\<\\(NOTE\\)" 1 'font-lock-note-face t)
	     ("\\<\\(IMPORTANT\\)" 1 'font-lock-important-face t)
	     ("\\<\\(CLEANUP\\)" 1 'font-lock-cleanup-face t)
	     ("\\<\\(SPEED\\)" 1 'font-lock-speed-face t)

	     ("\\<\\(TODO\\)" 1 'font-lock-todo-face t)
	     ("\\<\\(NEXT\\)" 1 'font-lock-next-face t)
	     ("\\<\\(HOLD\\)" 1 'font-lock-next-face t)
	     ("\\<\\(PROGRESS\\)" 1 'font-lock-progress-face t)
	     ("\\<\\(PROG\\)" 1 'font-lock-progress-face t)
	     ("\\<\\(DONE\\)" 1 'font-lock-done-face t)
	     )))
	fixme-modes)

  (modify-face 'font-lock-todo-face "firebrick3" nil nil t nil t nil nil)
  (modify-face 'font-lock-bug-face "Red" nil nil t nil t nil nil)
  (modify-face 'font-lock-cleanup-face "Yellow" nil nil t nil t nil nil)
  (modify-face 'font-lock-speed-face "Yellow" nil nil t nil t nil nil)
  (modify-face 'font-lock-next-face "CornflowerBlue" nil nil t nil t nil nil)
  (modify-face 'font-lock-progress-face "Yellow" nil nil t nil t nil nil)
  (modify-face 'font-lock-important-face "Yellow" nil nil t nil t nil nil)
  (modify-face 'font-lock-done-face "Green" nil nil t nil t nil nil)
  (modify-face 'font-lock-note-face "CornflowerBlue" nil nil t nil t nil nil))

;; ===============================================================
;;   Theme Settings
;; ---------------------------------------------------------------
;; Font
;; (add-to-list 'default-frame-alist '(font . "Liberation Mono-12"))
;; (add-to-list 'default-frame-alist '(font . "Liberation Mono-24"))
;; (add-to-list 'default-frame-alist '(width . 165))
;; (add-to-list 'default-frame-alist '(height . 60))
;; (set-face-attribute 'default t :font "Liberation Mono-24")
;; (set-frame-font "Liberation Mono-16" nil t)
;; (set-frame-font "Liberation Mono-12" nil t)
(set-frame-font "Liberation Mono-10" nil t)

(defun set-font-at-size (s)
  "Set the font size for emacs"
  (interactive)
  (message (concat "Setting font size to " (number-to-string s)))
  (set-frame-font (concat "Liberation Mono-" (number-to-string s)) nil t)
)

(defun set-font-size-huge ()
  "Set the font size for emacs"
  (interactive)
  (set-font-at-size 24)
)

(defun set-font-size-big ()
  "Set the font size for emacs"
  (interactive)
  (set-font-at-size 16)
)

(defun set-font-size-small ()
  "Set the font size for emacs"
  (interactive)
  (set-font-at-size 10)
)
;; (set-frame-font (concat "Liberation Mono-" (number-to-string fs)) nil t)

;; Colors
(setq foreground-font-color                                       "#D2CBC0")
(set-face-attribute 'font-lock-builtin-face nil       :foreground "#DAB98F")
;; (set-face-attribute 'font-lock-comment-face nil       :foreground "#7F7F7F") ;; grey50
(set-face-attribute 'font-lock-comment-face nil       :foreground "#53A347") ;; green-ish
(set-face-attribute 'font-lock-doc-face nil           :foreground "#7F7F7F") ;; grey50
(set-face-attribute 'font-lock-string-face nil        :foreground "#65B29E") ;; maybe #458B74
(set-face-attribute 'font-lock-keyword-face nil       :foreground "#DAB98F") ;; DarkGoldenRod3
(set-face-attribute 'font-lock-constant-face nil      :foreground foreground-font-color)
(set-face-attribute 'font-lock-function-name-face nil :foreground foreground-font-color)
(set-face-attribute 'font-lock-variable-name-face nil :foreground foreground-font-color)
(set-face-attribute 'font-lock-type-face nil          :foreground foreground-font-color)
(set-foreground-color                                             foreground-font-color)
;; (set-background-color                                             "#122A2C") ;; something akin to J.Blow's theme
(set-background-color                                             "#072626") ;; Actually J.Blow's theme
(set-face-background 'hl-line                                     "#191970") ;; the -always on- horizontal highlight
(set-cursor-color                                                 "#40FF40") ;; Green-ish cursor color
(set-face-attribute 'mode-line nil                    :background "#CDAA7D" ;; "burlywood3"
                                                      :foreground "#000000")

;; ===============================================================
;;   Post Load Hook
;; ---------------------------------------------------------------
;; initial window
(setq default-frame-alist '(
    ;; (width . 120) ; character
    ;; (height . 65) ; lines

  )
)

(scroll-bar-mode -1)
(tool-bar-mode -1)
(initialize-fixme-modes)
(toggle-frame-maximized)
(split-window-horizontally)


;; ===============================================================
;;   C++ Mode Configuration
;;   TODO(craig): This is just kind of a dumping ground at this
;;     point. I should really take the time to clean this up but it
;;     works and I don't really want to touch it.
;;   NOTE(craig): Most of this was taken from Casey Muratori, game
;;     developer at molly rocket and the handmade hero series.
;; ---------------------------------------------------------------
(defun save-buffers-without-asking ()
  "Saves all loaded buffers without prompting.

   As all the buffers can be hidden, this is used when you want to
   save every buffer listed prior to doing something like building 
   the project."
  (interactive)
  (save-some-buffers 'no-confirm (lambda ()
				   (cond
				    ((and buffer-file-name (equal buffer-file-name abbrev-file-name)))
				    ((and buffer-file-name (eq major-mode 'markdown-mode)))
				    ((and buffer-file-name (eq major-mode 'c-mode)))
				    ((and buffer-file-name (eq major-mode 'cc-mode)))
				    ((and buffer-file-name (eq major-mode 'c++-mode)))
				    ((and buffer-file-name (eq major-mode 'scala-mode)))
				    ((and buffer-file-name (eq major-mode 'go-mode)))
				    ((and buffer-file-name (eq major-mode 'emacs-lisp-mode)))
				    ((and buffer-file-name (derived-mode-p 'org-mode)))))))

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
  (if (find-project-directory) (compile (concat "./" build-file-name)))
  (other-window 1))

;; TODO: convert this to be '(make-without-asking "test")
(defun test-without-asking ()
  "Make the current build."
  (interactive)
  (save-buffers-without-asking)
  (if (find-project-directory) (compile (concat "./" build-file-name " test")))
  (other-window 1))

