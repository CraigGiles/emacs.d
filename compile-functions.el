(defun split-window-right-if-single-window ()
  "Test something"
  (interactive)
  (if (= (length (window-list)) 1) (split-window-right) nil))

(defun save-buffers-without-asking ()
  "Saves all loaded buffers without prompting."
  (interactive)
  (save-some-buffers t))

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

