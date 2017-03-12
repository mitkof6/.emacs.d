;;------------------------------------------------------------------------------
;; elpy
;; -----------------------------------------------------------------------------
(require-package 'elpy)
(require-package 'py-autopep8)
(package-initialize)
(elpy-enable)
(elpy-use-ipython)

;; this should be set in due to ipython v5
;; https://github.com/jorgenschaefer/elpy/issues/949
(setq python-shell-interpreter "ipython2"
      python-shell-interpreter-args "--simple-prompt --pprint")
(setq elpy-rpc-python-command "python2")

(add-hook 'python-mode-hook 'elpy-mode)
(with-eval-after-load 'elpy
  (remove-hook 'elpy-modules 'elpy-module-flymake)
  (add-hook 'elpy-mode-hook 'flycheck-mode)
  (add-hook 'elpy-mode-hook 'py-autopep8-enable-on-save))

;;------------------------------------------------------------------------------
;; ein
;; -----------------------------------------------------------------------------
(require-package 'ein)
;; (setq ein:use-auto-complete t)
;; Or, to enable "superpack" (a little bit hacky improvements):
(setq ein:use-auto-complete-superpack t)

(require-package 'smartrep)
(setq ein:use-smartrep t)
(setq elpy-rpc-backend "jedi")

;;------------------------------------------------------------------------------
;; jedi
;; -----------------------------------------------------------------------------
;; (require-package 'jedi)
;; (require-package 'company-jedi)
;; (add-hook 'python-mode-hook 'jedi:setup)
;; (setq jedi:complete-on-dot t)                 ; optional

(provide 'ds-python)
