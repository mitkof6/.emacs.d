(use-package elpy
  :ensure t
  :defer t
  :config
  (require-package 'py-autopep8)
  (package-initialize)
  (elpy-enable)
  (elpy-use-ipython)

  ;; this should be set in due to ipython v5
  ;; https://github.com/jorgenschaefer/elpy/issues/949
  (setq python-shell-interpreter "ipython2"
	python-shell-interpreter-args "--simple-prompt --pprint"
	elpy-rpc-python-command "python2")

  (add-hook 'python-mode-hook 'elpy-mode)
  (with-eval-after-load 'elpy
    (remove-hook 'elpy-modules 'elpy-module-flymake)
    (add-hook 'elpy-mode-hook 'flycheck-mode)
    (add-hook 'elpy-mode-hook 'py-autopep8-enable-on-save)))


(use-package ein
  :ensure t
  :defer t
  :config
  (require-package 'smartrep)
  (setq
   ein:use-smartrep t
   ein:use-auto-complete-superpack t
   ein:output-type-preference '(emacs-lisp svg png jpeg html text latex javascript))
  )


;; ;; jedi
;; (require-package 'jedi)
;; (require-package 'company-jedi)
;; (add-hook 'python-mode-hook 'jedi:setup)
;; (setq jedi:complete-on-dot t)                 ; optional

(provide 'ds-python)
