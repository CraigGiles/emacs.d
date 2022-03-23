
(load "jai-mode")

;;   --- Jai Mode ---
;; ---------------------------------------------------------------
(defun jai-mode-config ()
    (add-to-list 'auto-mode-alist '("\\.jai$" . jai-mode))
    (add-to-list 'fixme-modes 'jai-mode)
    (initialize-fixme-modes)

    (setq tab-stop 4)
    (setq indent-tabs-mode nil)

    (define-key jai-mode-map "\em" 'compile)

    (setq build-file-name "build.bat")
    (setq compile-command "call build.bat")

    (message "Jai hook added")
)

(add-hook 'jai-mode-hook 'jai-mode-config)
