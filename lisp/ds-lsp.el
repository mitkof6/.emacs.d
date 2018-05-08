(use-package lsp-ui
             :ensure t
             :config
             (add-hook 'lsp-after-open-hook 'lsp-enable-imenu)
             ;; (define-key evil-normal-state-map (kbd "C-p") 'lsp-ui-peek-jump-forward)
             ;; (define-key evil-normal-state-map (kbd "C-t") 'lsp-ui-peek-jump-backward)
             )

(use-package lsp-mode
             :ensure t
             ;; :after (cquery, lsp-ui)
             :config
             (add-hook 'lsp-mode-hook 'lsp-ui-mode)
             (add-hook 'lsp-mode-hook 'flycheck-mode)
             (add-hook 'c-mode-common-hook 'lsp-cquery-enable))

(use-package company-lsp
             :ensure t
             :config
             (setq company-transformers nil company-lsp-async t
                   company-lsp-cache-candidates nil)
             (add-hook 'c-mode-common-hook
                       (lambda ()
                         (local-set-key (kbd "M-RET") 'company-lsp))))

;; setup company
(use-package company
             :ensure t
             :config
             (add-hook 'after-init-hook 'global-company-mode)
             (setq company-idle-delay nil
                   company-minimum-prefix-length 2
                   company-show-numbers t
                   company-tooltip-limit 20
                   company-dabbrev-downcase nil)
             ;; :bind ("M-RET" . company-complete)
             )

;; company statistics for sorting of completion candidates by frequency
(use-package company-statistics
             :ensure t
             :config
             (company-statistics-mode))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; C++

(defun ds/cquery-enable ()
  (condition-case nil
                  (lsp-cquery-enable)
                  (user-error nil)))

(use-package cquery
             :ensure t
             :commands lsp-cquery-enable
             :init (add-hook 'c-mode-common-hook 'ds/cquery-enable)
             :config
             (setq cquery-extra-init-params '(:completion (:detailedLabel t))
                   lsp-ui-doc-include-signature t  ; include type signature in the child frame
                   lsp-ui-sideline-show-symbol t  ; show symbol on the right of info
                   ;; cquery-sem-highlight-method 'font-lock
                   cquery-sem-highlight-method 'overlay
                   )
             ;; for rainbow semantic highlighting
             ;; (cquery-use-default-rainbow-sem-highlight)
             ;; enable line numbers
             (defun ds/c-hook ()
               (linum-mode))
             (add-hook 'c-mode-common-hook 'ds/c-hook))

(use-package cmake-ide
             :ensure t
             :config
             (cmake-ide-setup)
             (setq cmake-ide-flags-c++ (append '("-std=c++11"))
                   cmake-ide-make-command "make --no-print-directory -j4"
                   compilation-skip-threshold 2 ;; show only errors
                   compilation-auto-jump-to-first-error t ;; go to first error
                   )
             :bind ("C-c m" . cmake-ide-compile))


;; make sure cmake-mode is installed for viewing CMake files
(use-package cmake-mode
             :ensure t)

;; emacs Lisp defun to bury the compilation buffer if everything compiles
;; smoothly
(defun ds/bury-compile-buffer-if-successful (buffer string)
  (when (and
         (string-match "compilation" (buffer-name buffer))
         (string-match "finished" string)
         (not (search-forward "warning" nil t)))
    (bury-buffer buffer)
    (switch-to-prev-buffer (get-buffer-window buffer) 'kill)))
(add-hook 'compilation-finish-functions 'ds/bury-compile-buffer-if-successful)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; python

(use-package lsp-python
             :ensure t
             :config
             (add-hook 'python-mode-hook #'lsp-python-enable)
             (add-hook 'python-mode-hook
                       (lambda ()
                         (local-set-key (kbd "M-RET") 'company-complete))))

(provide 'ds-lsp)
