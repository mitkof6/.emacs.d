;;----------------------------------------------------------------------------
;; Setup rtags
;;----------------------------------------------------------------------------

;; load custom files
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

;; (setq rtags-use-helm t)

(require 'flycheck-rtags)
(defun my-flycheck-rtags-setup ()
  (flycheck-select-checker 'rtags)
  (setq-local flycheck-highlighting-mode nil) ;; RTags creates more accurate overlays.
  (setq-local flycheck-check-syntax-automatically nil))
;; c-mode-common-hook is also called by c++-mode
(add-hook 'c-mode-common-hook 'my-flycheck-rtags-setup)


;;----------------------------------------------------------------------------
;; Setup cmake-ide
;;----------------------------------------------------------------------------

(require-package 'cmake-ide)
(require 'cmake-ide)
(cmake-ide-setup)
;; Set cmake-ide-flags-c++ to use C++11
(setq cmake-ide-flags-c++ (append '("-std=c++11")))
;; We want to be able to compile with a keyboard shortcut
(global-set-key (kbd "C-c m") 'cmake-ide-compile)

;; Set rtags to enable completions and use the standard keybindings.
;; A list of the keybindings can be found at:
;; http://syamajala.github.io/c-ide.html
(setq rtags-autostart-diagnostics t)
(rtags-diagnostics)
(setq rtags-completions-enabled t)
(rtags-enable-standard-keybindings)

(provide 'init-rtags)
