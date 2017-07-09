;; disable toolbar
(when (fboundp 'tool-bar-mode)
  (tool-bar-mode -1))

;; disable menu bar
(menu-bar-mode -1)

;; the blinking cursor is nothing, but an annoyance
(blink-cursor-mode -1)

(setq
 ;; disable the annoying bell ring
 ring-bell-function 'ignore
 ;; disable startup screen
 inhibit-startup-screen t
 ;; nice scrolling
 scroll-margin 0
 scroll-conservatively 100000
 scroll-preserve-screen-position 1)

;; mode line settings
(use-package hlinum
  :ensure t
  :config
  ;; (hlinum-activate)
  ;; (global-linum-mode t)
  (column-number-mode t)
  (size-indication-mode t))

;; enable y/n answers
(fset 'yes-or-no-p 'y-or-n-p)

;; more useful frame title, that show either a file or a
;; buffer name (if the buffer isn't visiting a file)
(setq frame-title-format
      '("" invocation-name " DS - "
        (:eval (if (buffer-file-name)
                   (abbreviate-file-name (buffer-file-name))
                 "%b"))))

;; recent
(use-package recentf
  :ensure t
  :config
  (recentf-mode 1)
  (setq recentf-max-saved-items 100
        recentf-exclude '("/tmp/" "/ssh:")
        recentf-max-menu-item 100))

;; smart mode line
(use-package smart-mode-line
  :ensure t
  :config
  (setq sml/no-confirm-load-theme t
        ;; delegate theming to the currently active theme
        ;;  sml/theme nil
        sml/theme 'dark
        ;;  sml/theme 'light
        ;;  sml/theme 'respectful
        )
  (add-hook 'after-init-hook #'sml/setup))

;; show the cursor when moving after big movements in the window
(require-package 'beacon)
(beacon-mode +1)

;; show available keybindings after you start typing
(require-package 'which-key)
(which-key-mode +1)

;; enhanced help mechanism
(require-package 'help-fns+)

;;------------------------------------------------------------------------------
;; themes
;; -----------------------------------------------------------------------------

;; zenburn-theme
(require-package 'zenburn-theme)
(require 'zenburn-theme)

;; moe-theme
(require-package 'moe-theme)
(require 'moe-theme)
(load-theme 'moe-dark t)

;; powerline
(require-package 'powerline-evil)
(require-package 'powerline)
(require-package 'smart-mode-line-powerline-theme)
;; (powerline-default-theme)
(powerline-moe-theme)
;; (powerline-center-evil-theme)
;; (powerline-center-theme)
;; (powerline-vim-theme)
;; (powerline-nano-theme)

;; afternoon-theme
;; (require-package 'afternoon-theme)
;; (require 'afternoon-theme)
;; (load-theme 'afternoon-theme)

;; solarized
;; (require-package 'color-theme-solarized)
;; (load-theme 'solarized t)
;; (set-frame-parameter nil 'background-mode 'light)
;; (set-frame-parameter nil 'background-mode 'dark)

;; function that changes to next theme
(require 'cl)
(lexical-let ((themes '(moe-light moe-dark zenburn)))
  (rplacd (last themes) themes)
  (defun ds/change-to-next-theme ()
    (interactive)
    (load-theme (pop themes) t)))

(provide 'ds-ui)
