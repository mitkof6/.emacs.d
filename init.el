;;----------------------------------------------------------------------------
;; Temporarily reduce garbage collection during startup
;;----------------------------------------------------------------------------

;; Added by Package.el.  This must come before configurations of
;; installed packages.  Don't delete this line.  If you don't want it,
;; just comment it out by adding a semicolon to the start of the line.
;; You may delete these explanatory comments.
(package-initialize)

(defconst sanityinc/initial-gc-cons-threshold gc-cons-threshold
  "Initial value of `gc-cons-threshold' at start-up time.")
(setq gc-cons-threshold (* 128 1024 1024))
(add-hook 'after-init-hook
          (lambda () (setq gc-cons-threshold sanityinc/initial-gc-cons-threshold)))

;;----------------------------------------------------------------------------
;; Bootstrap configure
;;----------------------------------------------------------------------------
;; add lisp directory
(add-to-list 'load-path (expand-file-name "lisp" user-emacs-directory))
(require 'init-benchmarking) ;; Measure startup time

(setq custom-file (expand-file-name "custom.el" user-emacs-directory))
(require 'init-utils)
(require 'init-elpa)      ;; Machinery for installing required packages
(require 'init-exec-path) ;; Set up $PATH

;;----------------------------------------------------------------------------
;; Load configs for specific features and modes
;;----------------------------------------------------------------------------

(require 'init-dired)
(require 'init-flycheck)
(require 'init-c-style)
(require 'init-irony)
(require 'init-rtags)
(require 'init-cmake-ide)
;; (require 'init-helm)
;; (require 'init-ivy)
(require 'init-csv)
(require 'init-windows)
(require 'init-spelling)
(require 'init-isearch)
(require 'init-whitespace)
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
;; (require 'init-org)
(require 'init-python-mode)

;;----------------------------------------------------------------------------
;; Theme
;;----------------------------------------------------------------------------
(require-package 'afternoon-theme)
(require 'afternoon-theme)
(setq inhibit-startup-screen t)

;;----------------------------------------------------------------------------
;; Variables configured via the interactive 'customize' interface
;;----------------------------------------------------------------------------
(when (file-exists-p custom-file)
  (load custom-file))

(provide 'init)
