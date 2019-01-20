;;------------------------------------------------------------------------------
;; init.el
;;------------------------------------------------------------------------------

(package-initialize)

(defconst ds/initial-gc-cons-threshold gc-cons-threshold
  "Initial value of `gc-cons-threshold' at start-up time.")
(setq gc-cons-threshold (* 128 1024 1024))
(add-hook 'after-init-hook
          (lambda ()
            (setq gc-cons-threshold ds/initial-gc-cons-threshold)))

;;------------------------------------------------------------------------------
;; general
;;------------------------------------------------------------------------------

;; add lisp directory that contains custom configurations
(add-to-list 'load-path (expand-file-name "lisp" user-emacs-directory))

;; set custom.el file
(setq custom-file (expand-file-name "custom.el" user-emacs-directory))

(require 'ds-utils)
(require 'ds-elpa)      ;; Machinery for installing required packages
(require 'ds-exec-path) ;; Set up $PATH

;; use-packag
(eval-when-compile
  (require-package 'use-package))
(require-package 'diminish) 
(require-package 'bind-key) 

;; emacs client
(server-start)

;;------------------------------------------------------------------------------
;; Load configs for specific features and modes
;;------------------------------------------------------------------------------

(require 'ds-ivy)
(require 'ds-flycheck)
(require 'ds-flyspell)
(require 'ds-yasnippet)
(require 'ds-editing-utils)
(require 'ds-w3m)
(require 'ds-gnus)
(require 'ds-lsp)
(require 'ds-python)
(require 'ds-octave)
(require 'ds-julia)
(require 'ds-lisp)
(require 'ds-java)
(require 'ds-auctex)
(require 'ds-org)
(require 'ds-pdf-tools)
(require 'ds-ui)
(require 'ds-maxima)
(require 'ds-global-key-bindings)

;;------------------------------------------------------------------------------
;; Variables configured via the interactive 'customize' interface
;;------------------------------------------------------------------------------

(when (file-exists-p custom-file)
  (load custom-file))

(provide 'init)
