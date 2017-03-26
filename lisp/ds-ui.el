;; disable toolbar
(when (fboundp 'tool-bar-mode)
  (tool-bar-mode -1))

;; disable menu bar
(menu-bar-mode -1)

;; the blinking cursor is nothing, but an annoyance
(blink-cursor-mode -1)

;; disable the annoying bell ring
(setq ring-bell-function 'ignore)

;; disable startup screen
(setq inhibit-startup-screen t)

;; nice scrolling
(setq scroll-margin 0
      scroll-conservatively 100000
      scroll-preserve-screen-position 1)

;; mode line settings
(require-package 'hlinum)
;; (hlinum-activate)
;; (global-linum-mode t)
(column-number-mode t)
(size-indication-mode t)

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
(require 'recentf)
(recentf-mode 1)
(setq recentf-max-saved-items 100
      recentf-exclude '("/tmp/" "/ssh:"))
(setq recentf-max-menu-item 100)

;; smart mode line
(require-package 'smart-mode-line)
(setq sml/no-confirm-load-theme t)
;; delegate theming to the currently active theme
;; (setq sml/theme nil)
(setq sml/theme 'dark)
;; (setq sml/theme 'light)
;; (setq sml/theme 'respectful)
(add-hook 'after-init-hook #'sml/setup)

;; show the cursor when moving after big movements in the window
(require-package 'beacon)
(beacon-mode +1)

;; show available keybindings after you start typing
(require-package 'which-key)
(which-key-mode +1)

;; enhanced help mechanism
(require-package 'help-fns+)

;;------------------------------------------------------------------------------
;; theme
;; -----------------------------------------------------------------------------

;; moe-theme
(require-package 'moe-theme)
(require 'moe-theme)
(load-theme 'moe-dark t)

;; afternoon-theme
;; (require-package 'afternoon-theme)
;; (require 'afternoon-theme)
;; (load-theme 'afternoon-theme)

;; solarized
;; (require-package 'color-theme-solarized)
;; (load-theme 'solarized t)
;; (set-frame-parameter nil 'background-mode 'light)
;; (set-frame-parameter nil 'background-mode 'dark)

(require-package 'powerline)
;; (powerline-default-theme)
(powerline-moe-theme)
;; (powerline-center-evil-theme)
;; (powerline-center-theme)
;; (powerline-vim-theme)
;; (powerline-nano-theme)


;; function that changes to next theme
(require 'cl)
(lexical-let ((themes '(moe-light moe-dark)))
             (rplacd (last themes) themes)
             (defun ds/change-to-next-theme ()
               (interactive)
               (load-theme (pop themes) t)))

(provide 'ds-ui)
