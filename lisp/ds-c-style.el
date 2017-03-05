;; style preferences
(setq c++-tab-always-indent t)
(setq c-basic-offset 4)
(setq c-indent-level 4)

(setq tab-stop-list '(4 8 12 16 20 24 28 32 36 40 44 48 52 56 60))
(setq tab-width 4)
(setq indent-tabs-mode nil)  ; use spaces only if nil

(add-hook 'c-mode-common-hook
          (lambda()
            ;; my customizations for all of c-mode, c++-mode, objc-mode, java-mode
            (c-set-offset 'substatement-open 0)
            ;; show-hide region
            (local-set-key (kbd "C-c <right>") 'hs-show-block)
            (local-set-key (kbd "C-c <left>")  'hs-hide-block)
            (local-set-key (kbd "C-c <up>")    'hs-hide-all)
            (local-set-key (kbd "C-c <down>")  'hs-show-all)
            (hs-minor-mode t)))

(provide 'ds-c-style)
