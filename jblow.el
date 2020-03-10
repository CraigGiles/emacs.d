(setq debug-on-error t)
(load-library "view")
(setq truncate-partial-width-windows nil)
(setq make-backup-files nil)

(menu-bar-mode -1)
(tool-bar-mode -1)
(tooltip-mode -1)

(setq dabbrev-case-replace nil)
(setq dabbrev-case-fold-search t)
(setq dabbrev-upcase-means-case-search t)
(setq visible-bell t)

(global-set-key [?\C-u] 'upcase-word)
(define-key global-map [C-backspace] 'kill-word)
(setq-default indent-tabs-mode nil)
(setq require-final-newline t)
(global-auto-revert-mode 1)
