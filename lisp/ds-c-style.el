;; style preferences
(require-package 'cc-mode)
(setq c++-tab-always-indent t)
(setq c-basic-offset 4)
(setq c-indent-level 4)

(setq tab-stop-list '(4 8 12 16 20 24 28 32 36 40 44 48 52 56 60))
(setq tab-width 4)
(setq indent-tabs-mode nil)  ; use spaces only if nil

(add-hook 'c-mode-common-hook
          (lambda()
            ;;
            (c-set-offset 'substatement-open 0)
            ;; long argument layout
            (c-set-offset 'arglist-intro '+)
            ;; show-hide region
            ;; (local-set-key (kbd "C-c h [") 'hs-show-block)
            ;; (local-set-key (kbd "C-c h ]")  'hs-hide-block)
            ;; (local-set-key (kbd "C-c h {")    'hs-hide-all)
            ;; (local-set-key (kbd "C-c h }")  'hs-show-all)
            ;; (hs-minor-mode t)
            ))

(provide 'ds-c-style)
