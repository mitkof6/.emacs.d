;; Require flycheck to be present
(require-package 'flycheck)

;; Force flycheck to always use c++11 support. We use
;; the clang language backend so this is set to clang
(add-hook 'c++-mode-hook
          (lambda () (setq flycheck-clang-language-standard "c++11")))

;; Turn flycheck on everywhere
(global-flycheck-mode)

;; Use flycheck-pyflakes for python. Seems to work a little better.
;; (require 'flycheck-pyflakes)

;; C-c ! l displays flyckeck error list
(add-to-list 'display-buffer-alist
             `(,(rx bos "*Flycheck errors*" eos)
                (display-buffer-reuse-window
                 display-buffer-in-side-window)
                (side            . bottom)
                (reusable-frames . visible)
                (window-height   . 0.2)))

;; this function is used to close the buffer (e.g. flyckeck error list)
(defun lunaryorn-quit-bottom-side-windows ()
  "Quit side windows of the current frame."
  (interactive)
  (dolist (window (window-at-side-list))
    (quit-window nil window)))
(global-set-key (kbd "C-c q") #'lunaryorn-quit-bottom-side-windows)

;; flycheck-mode
(add-hook 'c++-mode-hook 'flycheck-mode)
(add-hook 'c-mode-hook 'flycheck-mode)
(require-package 'flycheck-irony)
(eval-after-load 'flycheck
                 '(add-hook 'flycheck-mode-hook #'flycheck-irony-setup))

(provide 'ds-flycheck)
