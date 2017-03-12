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
;; add lisp directory that contains custom configurations
(add-to-list 'load-path (expand-file-name "lisp" user-emacs-directory))

;; set custom.el file
(setq custom-file (expand-file-name "custom.el" user-emacs-directory))

(require 'ds-utils)
(require 'ds-elpa)      ;; Machinery for installing required packages
(require 'ds-exec-path) ;; Set up $PATH

;;------------------------------------------------------------------------------
;; Load configs for specific features and modes
;;------------------------------------------------------------------------------

(require 'ds-ivy)
(require 'ds-dired)
(require 'ds-flycheck)
(require 'ds-flyspell)
(require 'ds-cpp)
(require 'ds-yasnippet)
(require 'ds-auto-complete) ;; always after yasnippet
;; (require 'ds-windows)
(require 'ds-whitespace)
(require 'ds-editing-utils)
(require 'ds-git)
(require 'ds-auctex)
(require 'ds-markdown)
(require 'ds-org)
(require 'ds-pdf-tools)
(require 'ds-python)
(require 'ds-lisp)
(require 'ds-ui)
(require 'ds-w3m)
(require 'ds-gnus)
(require 'ds-global-key-bindings)

;;------------------------------------------------------------------------------
;; Variables configured via the interactive 'customize' interface
;;------------------------------------------------------------------------------
(when (file-exists-p custom-file)
  (load custom-file))

(provide 'init)
