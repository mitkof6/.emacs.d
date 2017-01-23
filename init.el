;; Added by Package.el.  This must come before configurations of
;; installed packages.  Don't delete this line.  If you don't want it,
;; just comment it out by adding a semicolon to the start of the line.
;; You may delete these explanatory comments.
(package-initialize)

(add-to-list 'load-path (expand-file-name "lisp" user-emacs-directory))

;;(setq warning-minimum-level :emergency) ;; disable warning message

;;----------------------------------------------------------------------------
;; Bootstrap config
;;----------------------------------------------------------------------------
(setq custom-file (expand-file-name "custom.el" user-emacs-directory))
(require 'init-utils)

;; Machinery for installing required packages.
;; explicitly call 'package-initialize to set up all packages installed via
;; ELPA. Should come before all package-related config files
(require 'init-elpa)
(require 'init-exec-path) ;; Set up $PATH

;;----------------------------------------------------------------------------
;; Load configs for specific features and modes
;;----------------------------------------------------------------------------

(require 'init-dired)
(require 'init-flycheck)
(require 'init-c-style)
(require 'init-irony)
(require 'init-spelling)
(require 'init-rtags)
;; (require 'init-helm)
(require 'init-recentf)
(require 'init-ido)
(require 'init-yasnippet)
(require 'init-hippie-expand)
(require 'init-auto-complete)
(require 'init-fonts)
(require 'init-tabbar)
(require 'init-editing-utils)
(require 'init-evil)
(require 'init-matlab)
(require 'init-git)
(require 'init-markdown)
(require 'init-auctex)
(require 'init-org)
;; (require 'init-python-mode)

(require 'init-themes)

;;----------------------------------------------------------------------------
;; Variables configured via the interactive 'customize' interface
;;----------------------------------------------------------------------------
(when (file-exists-p custom-file)
  (load custom-file))

(provide 'init)
