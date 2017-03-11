;;----------------------------------------------------------------------------
;; setup rtags
;;----------------------------------------------------------------------------
;; load custom files
(require-package 'company)
(add-to-list 'load-path "~/.emacs.d/lisp/rtags")
(load "rtags.el")
(load "company-rtags.el")
(load "flycheck-rtags.el")
(require 'rtags)
(require 'company-rtags)

(setq rtags-completions-enabled t)
(eval-after-load 'company
                 '(add-to-list
                   'company-backends 'company-rtags))
(setq rtags-autostart-diagnostics t)
(rtags-enable-standard-keybindings)

(setq rtags-use-helm t)

;; flycheck rtags
;; (require 'flycheck-rtags)
;; (defun ds/flycheck-rtags-setup ()
;;   (flycheck-select-checker 'rtags)
;;   ;; RTags creates more accurate overlays.
;;   (setq-local flycheck-highlighting-mode nil)
;;   (setq-local flycheck-check-syntax-automatically nil))
;; ;; c-mode-common-hook is also called by c++-mode
;; (add-hook 'c-mode-common-hook #'ds/flycheck-rtags-setup)

(provide 'ds-rtags)
