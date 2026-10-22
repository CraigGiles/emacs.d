;; Windows Emacs' GnuTLS often dies mid-download over TLS 1.3 ("No Data" errors)
(when (eq system-type 'windows-nt)
  (setq gnutls-algorithm-priority "NORMAL:-VERS-TLS1.3"))

(add-to-list 'package-archives
             '("melpa" . "https://melpa.org/packages/") t)
;; (add-to-list 'package-archives
;;              '("melpa-stable" . "https://stable.melpa.org/packages/") t)

(defvar rc/package-contents-refreshed nil)

(defun rc/package-refresh-contents-once ()
  (when (not rc/package-contents-refreshed)
    (setq rc/package-contents-refreshed t)
    (package-refresh-contents)))

(defun rc/require-one-package (package)
  (when (not (package-installed-p package))
    (rc/package-refresh-contents-once)
    (package-install package)))

(defun rc/require (&rest packages)
  (dolist (package packages)
    (rc/require-one-package package)))

;; (defun rc/require-theme (theme)
;;   (let ((theme-package (->> theme
;;                             (symbol-name)
;;                             (funcall (-flip #'concat) "-theme")
;;                             (intern))))
;;     (rc/require theme-package)
;;     (load-theme theme t)))

(rc/require 'dash)
(require 'dash)

;; dash-functional was merged into dash (2.18+) and removed from the archives
;; (rc/require 'dash-functional)
;; (require 'dash-functional)
