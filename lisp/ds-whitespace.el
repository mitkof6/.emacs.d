(setq-default show-trailing-whitespace t)

;; Whitespace

(defun ds/no-trailing-whitespace ()
  "Turn off display of trailing whitespace in this buffer."
  (setq show-trailing-whitespace nil))

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

(require-package 'whitespace-cleanup-mode)
(global-whitespace-cleanup-mode t)

(global-set-key [remap just-one-space] 'cycle-spacing)

;; 80 char mark and utility for whitespace
;; (require-package 'whitespace)
;; (global-whitespace-mode t)

;; fill column indicator
(require-package 'fill-column-indicator)
;; number of characters until the fill column
(define-globalized-minor-mode global-fci-mode fci-mode
  (lambda ()
    ;; (set-fill-column 80)
    ;; (setq fci-rule-width 1)
    (setq fci-rule-color "darkgrey")
    (fci-mode 1)))
(global-fci-mode t)
(setq-default fill-column 80)

;; clear and auto-indent, hook before save
(defun ds/clear-and-indent()
  "Indents an entire buffer using the default intending scheme."
  (interactive)
  (save-excursion
    (delete-trailing-whitespace)
    (indent-region (point-min) (point-max) nil)
    (untabify (point-min) (point-max))))
(add-hook 'before-save-hook #'ds/clear-and-indent)

(provide 'ds-whitespace)
