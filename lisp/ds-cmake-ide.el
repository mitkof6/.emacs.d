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

;;---------------------------------------------------------------------
;; Compiling:
;;---------------------------------------------------------------------
(require-package 'compile)

;; Change compilation command:
(setq compile-command "make ")

;; if there are no errors make the compilation window vanish
(setq compilation-finish-function
      (lambda (buf str)
        (if (null (string-match ".*exited abnormally.*" str))
            ;;no errors, make the compilation window go away in a few seconds
            (progn
              (run-at-time
               "2 sec" nil 'delete-windows-on
               (get-buffer-create "*compilation*"))
              (message "No Compilation Errors!")))))

(defun ds/compile ()
  "Run compile and resize the compile window"
  (interactive)
  (progn
    (call-interactively 'compile)
    (setq w (get-buffer-window "*compilation*"))
    (setq cur (selected-window))
    (select-window w)
    (setq h (window-height w))
    (shrink-window (- h 15))
    (select-window cur)))
(global-set-key [f9] 'ds/compile)
(define-key c++-mode-map (kbd "C-c C-c") #'ds/compile)

(defadvice compile (around split-horizontally activate)
  (let ((split-width-threshold nil)
        (split-height-threshold 0))
    ad-do-it))

(setq compilation-ask-about-save nil  ; Just save before compiling
      compilation-always-kill t       ; Just kill old compile processes before
                                        ; starting the new one
      compilation-scroll-output 'first-error ; Automatically scroll to first
                                        ; error
      )

(provide 'ds-cmake-ide)
