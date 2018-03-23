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
             :bind ("M-RET" . company-lsp))

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
             (cquery-use-default-rainbow-sem-highlight)
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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; python

(use-package lsp-python
             :ensure t
             ;; :after (lsp-mode)
             :config
             (add-hook 'python-mode-hook #'lsp-python-enable)
             ;; this should be set in due to ipython v5
             ;;           ;; https://github.com/jorgenschaefer/elpy/issues/949
             (setq python-shell-interpreter "ipython2"
                   python-shell-interpreter-args "--simple-prompt --pprint --matplotlib"
                   elpy-rpc-python-command "python2")

             (defun ds/python-hook ()
               (linum-mode))
             (add-hook 'python-mode-hook 'ds/python-hook))

(provide 'ds-lsp)
