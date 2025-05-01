;; Initialize package system
(require 'package)
(add-to-list 'package-archives
             '("melpa" . "https://melpa.org/packages/") t)

;; Initialize use-package
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

(require 'use-package)
(setq use-package-always-ensure t)
(setq use-package-compute-statistics t)

;; Load dash as it's required by other packages
(use-package dash
  :demand t)

(use-package dash-functional
  :demand t)

;; Helper function for theme loading
(defun rc/require-theme (theme)
  (let ((theme-package (->> theme
                           (symbol-name)
                           (funcall (-flip #'concat) "-theme")
                           (intern))))
    (use-package theme-package
      :config
      (load-theme theme t))))
