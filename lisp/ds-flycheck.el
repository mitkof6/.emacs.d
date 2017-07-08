(use-package flycheck
  :ensure t
  ;;:defer t
  :config
  ;; Force flycheck to always use c++11 support. We use
  ;; the clang language backend so this is set to clang
  (add-hook 'c++-mode-hook
            (lambda ()
              (setq flycheck-clang-language-standard "c++11")))
  ;; Turn flycheck on everywhere
  ;; (global-flycheck-mode)

  ;; flycheck-mode
  ;; (add-hook 'c++-mode-hook 'flycheck-mode)
  ;; (add-hook 'c-mode-hook 'flycheck-mode)

  ;; C-c ! l displays flyckeck error list (customize window)
  (add-to-list 'display-buffer-alist
               `(,(rx bos "*Flycheck errors*" eos)
                 (display-buffer-reuse-window
                  display-buffer-in-side-window)
                 (side            . bottom)
                 (reusable-frames . visible)
                 (window-height   . 0.2)))
  )

;; Use flycheck-pyflakes for python. Seems to work a little better.
;; (require 'flycheck-pyflakes)

(provide 'ds-flycheck)
