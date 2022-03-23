(load "kotlin-mode")

;;   --- Kotlin Mode ---
;; ---------------------------------------------------------------
(defun kotlin-mode-config ()
    (add-to-list 'auto-mode-alist '("\\.kt$" . kotlin-mode))
    (add-to-list 'auto-mode-alist '("\\.kts$" . kotlin-mode))
    (add-to-list 'fixme-modes 'kotlin-mode)
    (initialize-fixme-modes)

    (setq tab-stop 4)
    (setq indent-tabs-mode nil)

    (define-key kotlin-mode-map "\em" 'compile)

    (setq build-file-name "build.gradle.kts")
    (setq compile-command "./gradlew compileKotlin")

    (message "Kotlin hook added")
)

(add-hook 'kotlin-mode-hook 'kotlin-mode-config)
