;; Create a big horizontal blue bar so i don't keep loosing my cursor
(global-hl-line-mode 1)
(global-visual-line-mode 1)

;; Font
(set-face-attribute 'default t       :font "Liberation Mono-12")
(add-to-list 'default-frame-alist '(font . "Liberation Mono-12"))

;; Colors
(setq foreground-font-color                                       "#D2CBC0")
(set-face-attribute 'font-lock-builtin-face nil       :foreground "#DAB98F")
(set-face-attribute 'font-lock-comment-face nil       :foreground "#53A347") ;; green-ish
(set-face-attribute 'font-lock-doc-face nil           :foreground "#7F7F7F") ;; grey50
(set-face-attribute 'font-lock-string-face nil        :foreground "#65B29E") ;; maybe #458B74
(set-face-attribute 'font-lock-keyword-face nil       :foreground "#DAB98F") ;; DarkGoldenRod3
(set-face-attribute 'font-lock-constant-face nil      :foreground foreground-font-color)
(set-face-attribute 'font-lock-function-name-face nil :foreground foreground-font-color)
(set-face-attribute 'font-lock-variable-name-face nil :foreground foreground-font-color)
(set-face-attribute 'font-lock-type-face nil          :foreground foreground-font-color)
(set-foreground-color                                             foreground-font-color)
(set-background-color                                             "#072626") ;; Actually J.Blow's theme
(set-face-background 'hl-line                                     "#191970") ;; The -always on- horizontal highlight
(set-cursor-color                                                 "#40FF40") ;; Green-ish cursor color
(set-face-attribute 'mode-line nil                    :background "#CDAA7D" ;; "burlywood3"
                                                      :foreground "#000000")

(setq fixme-modes '(markdown-mode emacs-lisp-mode prog-mode fundamental-mode))
(defun initialize-fixme-modes ()
  "Sets the highlighted words like TODO and NOTE and colorschemes for these words"
  (interactive)
  (make-face 'font-lock-todo-face)
  (make-face 'font-lock-done-face)
  (make-face 'font-lock-next-face)
  (make-face 'font-lock-important-face)
  (make-face 'font-lock-note-face)

  (mapc (lambda (mode)
          (font-lock-add-keywords
           mode
           '(
             ("\\<\\(TODO\\)" 1 'font-lock-todo-face t)
             ("\\<\\(@TODO\\)" 1 'font-lock-todo-face t)

             ("\\<\\(NOTE\\)" 1 'font-lock-note-face t)
             ("\\<\\(@NOTE\\)" 1 'font-lock-note-face t)

             ("\\<\\(IMPORTANT\\)" 1 'font-lock-important-face t)
             ("\\<\\(@IMPORTANT\\)" 1 'font-lock-important-face t)

             ("\\<\\(DONE\\)" 1 'font-lock-done-face t)
             ("\\<\\(@DONE\\)" 1 'font-lock-done-face t)

             )))
        fixme-modes)

  (modify-face 'font-lock-todo-face "firebrick3" nil nil t nil t nil nil)
  (modify-face 'font-lock-next-face "CornflowerBlue" nil nil t nil t nil nil)
  (modify-face 'font-lock-important-face "Yellow" nil nil t nil t nil nil)
  (modify-face 'font-lock-done-face "Green" nil nil t nil t nil nil)
  (modify-face 'font-lock-note-face "CornflowerBlue" nil nil t nil t nil nil))

(provide 'gilesc-theme)
