;;------------------------------------------------------------------------------
;; Temporarily reduce garbage collection during startup
;;------------------------------------------------------------------------------

;; Added by Package.el.  This must come before configurations of
;; installed packages.  Don't delete this line.  If you don't want it,
;; just comment it out by adding a semicolon to the start of the line.
;; You may delete these explanatory comments.
(package-initialize)

(defconst sanityinc/initial-gc-cons-threshold gc-cons-threshold
  "Initial value of `gc-cons-threshold' at start-up time.")
(setq gc-cons-threshold (* 128 1024 1024))
(add-hook 'after-init-hook
          (lambda ()
            (setq gc-cons-threshold sanityinc/initial-gc-cons-threshold)))

;;------------------------------------------------------------------------------
;; Bootstrap configure
;;------------------------------------------------------------------------------
;; add lisp directory
(add-to-list 'load-path (expand-file-name "lisp" user-emacs-directory))

(setq custom-file (expand-file-name "custom.el" user-emacs-directory))
(require 'ds-utils)
(require 'ds-elpa)      ;; Machinery for installing required packages
(require 'ds-exec-path) ;; Set up $PATH

;;------------------------------------------------------------------------------
;; Load configs for specific features and modes
;;------------------------------------------------------------------------------

(require 'ds-dired)
(require 'ds-flycheck)
(require 'ds-c-style)
(require 'ds-irony)
(require 'ds-rtags)
(require 'ds-cmake-ide)
;; (require 'ds-helm-everywhere)
(require 'ds-ivy)
(require 'ds-windows)
(require 'ds-spelling)
(require 'ds-isearch)
(require 'ds-whitespace)
(require 'ds-recentf)
(require 'ds-ido)
(require 'ds-yasnippet)
(require 'ds-auto-complete)
(require 'ds-fonts)
(require 'ds-tabbar)
(require 'ds-editing-utils)
(require 'ds-git)
(require 'ds-markdown)
(require 'ds-auctex)
(require 'ds-python)
(require 'ds-lisp)

;;------------------------------------------------------------------------------
;; Customization
;;------------------------------------------------------------------------------
(require-package 'afternoon-theme)
(require 'afternoon-theme)

;; change yes-or-no to y-or-n
(fset 'yes-or-no-p 'y-or-n-p)

;; display time
(display-time-mode t)
(setq display-time-24hr-format t)

;; don't show the startup screen
(setq inhibit-startup-screen t)
;; don't show the menu bar
;; (menu-bar-mode nil)
;; don't show the tool bar
;; (require 'tool-bar)
;; (tool-bar-mode nil)
;; don't show the scroll bar
;; (scroll-bar-mode nil)

;;------------------------------------------------------------------------------
;; Variables configured via the interactive 'customize' interface
;;------------------------------------------------------------------------------
(when (file-exists-p custom-file)
  (load custom-file))

(provide 'init)
