(global-set-key (kbd "M-f") 'find-file)

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

;; Font
(set-face-attribute 'default t       :font "Liberation Mono-12")
(add-to-list 'default-frame-alist '(font . "Liberation Mono-12"))
(add-to-list 'default-frame-alist '(width . 165))
(add-to-list 'default-frame-alist '(height . 60))

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

