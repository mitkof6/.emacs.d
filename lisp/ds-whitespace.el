;; Whitespace
;; 80 char mark and utility for whitespace
(require-package 'whitespace)
(global-whitespace-mode t)

(require-package 'whitespace-cleanup-mode)
(global-whitespace-cleanup-mode t)
(global-set-key [remap just-one-space] 'cycle-spacing)
(setq-default show-trailing-whitespace t)

;; But don't show trailing whitespace in SQLi, inf-ruby etc.
(dolist (hook '(special-mode-hook
                Info-mode-hook
                eww-mode-hook
                term-mode-hook
                comint-mode-hook
                compilation-mode-hook
                twittering-mode-hook
                minibuffer-setup-hook))
  (add-hook hook #'ds/no-trailing-whitespace))

;; clear and auto-indent, hook before save
(defun ds/clear-and-indent()
  "Indents an entire buffer using the default intending scheme."
  (interactive)
  (save-excursion
   (delete-trailing-whitespace)
   (indent-region (point-min) (point-max) nil)
   (untabify (point-min) (point-max))))
(add-hook 'before-save-hook #'ds/clear-and-indent)

(defun ds/no-trailing-whitespace ()
  "Turn off display of trailing whitespace in this buffer."
  (setq show-trailing-whitespace nil))

(provide 'ds-whitespace)
