;;------------------------------------------------------------------------------
;; set up code completion with company and irony + rtags for finding symbols
;;------------------------------------------------------------------------------

(require-package 'company)
(require-package 'rtags)
(require-package 'irony)
(require-package 'company-irony)
(require-package 'company-irony-c-headers)
(require-package 'company-rtags)
(require 'rtags)
(require 'cl)

;; setup company
(global-company-mode)
(setq company-idle-delay nil
      company-minimum-prefix-length 2
      company-show-numbers t
      company-tooltip-limit 20
      company-dabbrev-downcase nil
      )

;; setup rtags
(setq rtags-completions-enabled t
      rtags-autostart-diagnostics t
      rtags-display-result-backend 'ivy)
(rtags-diagnostics)
(rtags-enable-standard-keybindings)

;; setup irony
(add-hook 'c++-mode-hook 'irony-mode)
(add-hook 'c-mode-hook 'irony-mode)
(add-hook 'objc-mode-hook 'irony-mode)

(defun ds/irony-mode-hook ()
  "Custom irony mode hook to remap keys."
  (define-key irony-mode-map [remap completion-at-point]
    'irony-completion-at-point-async)
  (define-key irony-mode-map [remap complete-symbol]
    'irony-completion-at-point-async))
(add-hook 'irony-mode-hook 'ds/irony-mode-hook)
(add-hook 'irony-mode-hook 'irony-cdb-autosetup-compile-options)

(add-hook 'irony-mode-hook 'company-irony-setup-begin-commands)
(eval-after-load 'company
                 '(add-to-list
                   'company-backends '(company-irony
                                       company-rtags
                                       company-yasnippet
                                       company-irony-c-headers
                                       company-clang
                                       )))

;;------------------------------------------------------------------------------
;; company statistics for sorting of completion candidates by frequency
;;------------------------------------------------------------------------------

(require-package 'company-statistics)
(company-statistics-mode)

;;------------------------------------------------------------------------------
;; bind TAB for indent-or-complete
;;------------------------------------------------------------------------------

(defun ds/irony-check-expansion ()
  (save-excursion
   (if (looking-at "\\_>") t
       (backward-char 1)
       (if (looking-at "\\.") t
           (backward-char 1)
           (if (looking-at "->") t nil)))))

(defun ds/irony-indent-or-complete ()
  "Indent or Complete."
  (interactive)
  (cond ((and (not (use-region-p))
              (ds/irony-check-expansion))
         ;; (message "complete")
         (company-irony 'interactive)        ; better completion
         ;; (company-complete)
         )
        (t
         ;; (message "indent")
         (call-interactively 'c-indent-line-or-region))))

(defun ds/irony-mode-keys ()
  "Modify keymaps used by `irony-mode'."
  (local-set-key (kbd "TAB") 'ds/irony-indent-or-complete))
(add-hook 'c-mode-common-hook 'ds/irony-mode-keys)

;;------------------------------------------------------------------------------
;; flycheck-irony
;;------------------------------------------------------------------------------

(require-package 'flycheck-irony)
(eval-after-load 'flycheck
                 '(add-hook 'flycheck-mode-hook #'flycheck-irony-setup))


;; setup flycheck-rtags
(require-package 'flycheck-rtags)
(defun ds/flycheck-rtags-setup ()
  (flycheck-select-checker 'rtags)
  ;; RTags creates more accurate overlays.
  (setq-local flycheck-highlighting-mode nil)
  (setq-local flycheck-check-syntax-automatically nil))
(add-hook 'c-mode-hook 'ds/flycheck-rtags-setup)
(add-hook 'c++-mode-hook 'ds/flycheck-rtags-setup)
(add-hook 'objc-mode-hook 'ds/flycheck-rtags-setup)

;;------------------------------------------------------------------------------
;; eldoc-mode
;;------------------------------------------------------------------------------

(require-package 'irony-eldoc)
(add-hook 'irony-mode-hook 'irony-eldoc)

;;------------------------------------------------------------------------------
;; function-args
;;------------------------------------------------------------------------------

(require-package 'function-args)
(fa-config-default)

;;------------------------------------------------------------------------------
;; setup rtags-helm
;;------------------------------------------------------------------------------

;; (require-package 'rtags-helm)
;; (setq rtags-use-helm t)

;;------------------------------------------------------------------------------
;; setup cmake-ide
;;------------------------------------------------------------------------------

(require-package 'cmake-ide)
(cmake-ide-setup)

;; Set cmake-ide-flags-c++ to use C++11
(setq cmake-ide-flags-c++ (append '("-std=c++11")))

;; We want to be able to compile with a keyboard shortcut
(global-set-key (kbd "C-c m") 'cmake-ide-compile)

;; make sure cmake-mode is installed for viewing CMake files
(require-package 'cmake-mode)

;;------------------------------------------------------------------------------
;; https://github.com/tuhdo/semantic-refactor
;;------------------------------------------------------------------------------

;; not working properly yet

(require-package 'srefactor)

(semantic-mode 1)

(define-key c-mode-map (kbd "M-RET") 'srefactor-refactor-at-point)
(define-key c++-mode-map (kbd "M-RET") 'srefactor-refactor-at-point)


(provide 'ds-cpp)
