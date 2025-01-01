(setq fixme-modes '(markdown-mode emacs-lisp-mode prog-mode fundamental-mode))

(defun initialize-fixme-modes ()
  "Sets the highlighted words like TODO and NOTE and colorschemes for these words"
  (interactive)
  (make-face 'font-lock-todo-face)
  (make-face 'font-lock-done-face)
  (make-face 'font-lock-important-face)
  (make-face 'font-lock-note-face)

  (mapc (lambda (mode)
          (font-lock-add-keywords
           mode
           '(
             ("\\<\\(TODO\\)" 1 'font-lock-todo-face t)
             ("\\<\\(DONE\\)" 1 'font-lock-done-face t)
             ("\\<\\(IMPORTANT\\)" 1 'font-lock-important-face t)
             ("\\<\\(NOTE\\)" 1 'font-lock-note-face t)
             )))
        fixme-modes)

  (modify-face 'font-lock-todo-face "firebrick3" nil nil t nil t nil nil)
  (modify-face 'font-lock-done-face "Green" nil nil t nil t nil nil)
  (modify-face 'font-lock-important-face "Yellow" nil nil t nil t nil nil)
  (modify-face 'font-lock-note-face "CornflowerBlue" nil nil t nil t nil nil))

(provide 'fixme-mode)
