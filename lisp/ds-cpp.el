;; setup company
(use-package company
             :ensure t
             :config
             (add-hook 'after-init-hook 'global-company-mode)
             ;; (add-hook 'c++-mode-hook 'company-mode)
             ;; (add-hook 'c-mode-hook 'company-mode)
             (setq company-idle-delay nil
                   company-minimum-prefix-length 2
                   company-show-numbers t
                   company-tooltip-limit 20
                   company-dabbrev-downcase nil))

;; company statistics for sorting of completion candidates by frequency
(use-package company-statistics
             :ensure t
             :config
             (company-statistics-mode))

(use-package irony
             :ensure t
             :config
             (progn
               (use-package company-irony
                            :ensure t
                            :config
                            (add-to-list 'company-backends 'company-irony)
                            :bind ("M-RET" . company-complete))
               (add-hook 'c++-mode-hook 'irony-mode)
               (add-hook 'c-mode-hook 'irony-mode)
               (add-hook 'irony-mode-hook 'company-irony-setup-begin-commands)
               (add-hook 'irony-mode-hook 'irony-cdb-autosetup-compile-options)))

;; setup rtags
(use-package rtags
             :ensure t
             :config
             ;; setup rtags
             (setq rtags-completions-enabled t
                   rtags-autostart-diagnostics t
                   rtags-display-result-backend 'ivy)
             (rtags-diagnostics)
             (rtags-enable-standard-keybindings))

(use-package flycheck-irony
             :ensure t
             :after (irony, flycheck, rtags)
             :config
             (with-eval-after-load 'flycheck
               (add-hook 'flycheck-mode-hook 'flycheck-irony-setup))
             ;; setup flycheck-rtags
             (with-eval-after-load 'rtags
               (require-package 'flycheck-rtags)
               (defun ds/flycheck-rtags-setup ()
                 (flycheck-select-checker 'rtags)
                 ;; RTags creates more accurate overlays.
                 (setq-local flycheck-highlighting-mode nil)
                 (setq-local flycheck-check-syntax-automatically nil))
               (add-hook 'c-mode-hook 'ds/flycheck-rtags-setup)
               (add-hook 'c++-mode-hook 'ds/flycheck-rtags-setup)))

(use-package irony-eldoc
             :ensure t
             :config
             (with-eval-after-load 'irony
               (add-hook 'irony-mode-hook 'irony-eldoc)))

(use-package function-args
             :ensure t
             :config
             (fa-config-default))

;; setup cmake-ide (use-package is problematic must find why)
;; (use-package cmake-ide
;;              :ensure t
;;              :config
;;              (with-eval-after-load 'rtags
;;                (cmake-ide-setup)
;;                ;; set cmake-ide-flags-c++ to use C++11
;;                (setq cmake-ide-flags-c++ (append '("-std=c++11"))))
;;              :bind ("C-c m" . cmake-ide-compile))
(require-package 'cmake-ide)
(cmake-ide-setup)
;; set cmake-ide-flags-c++ to use C++11
(setq cmake-ide-flags-c++ (append '("-std=c++11")))
(global-set-key (kbd "C-c m") 'cmake-ide-compile)

;; make sure cmake-mode is installed for viewing CMake files
(use-package cmake-mode
             :ensure t)

;;------------------------------------------------------------------------------
;; https://github.com/tuhdo/semantic-refactor
;;------------------------------------------------------------------------------

;; not working properly yet

(require-package 'srefactor)

(semantic-mode 1)

(provide 'ds-cpp)
