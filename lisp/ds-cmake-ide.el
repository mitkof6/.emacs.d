;;---------------------------------------------------------------------
;; Setup cmake-ide
;;---------------------------------------------------------------------
(require-package 'cmake-ide)
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

;; Compiling:
(define-key c++-mode-map (kbd "C-c C-c") 'compile)
;; Change compilation command:
(setq compile-command "make ")

(global-set-key (kbd "<f5>")
                (lambda ()
                  (interactive)
                  (setq-local compilation-read-command nil)
                  (call-interactively 'compile)))

(provide 'ds-cmake-ide)
