(setq auto-mode-alist
      (append '(("SConstruct\\'" . python-mode)
                ("SConscript\\'" . python-mode))
              auto-mode-alist))

(require-package 'pip-requirements)

(when (maybe-require-package 'anaconda-mode)
  (after-load 'python
    (add-hook 'python-mode-hook 'anaconda-mode)
    (add-hook 'python-mode-hook 'anaconda-eldoc-mode))
  (when (maybe-require-package 'company-anaconda)
    (after-load 'company
      (after-load 'company
        (add-hook 'python-mode-hook
                  (lambda ()
                    (sanityinc/local-push-company-backend #'company-anaconda)))))))

;;use IPython (currently not working due to bug)
(setq python-shell-interpreter "python2")'

(require-package 'jedi)
(require-package 'company-jedi) ;; part of jedi

(defun ds/python-mode-hook ()
  (add-to-list 'company-backends 'company-jedi))
(add-hook 'python-mode-hook 'ds/python-mode-hook)
(add-hook 'python-mode-hook 'jedi:setup)
(setq jedi:complete-on-dot t)

;;--------------------------------------
;; elpy
;; --------------------------------------
(package-initialize)
(require-package 'elpy)
(require-package 'ein)
(require-package 'py-autopep8)
(elpy-enable)
;; (elpy-use-ipython)

;; use flycheck not flymake with elpy
(when (require 'flycheck nil t)
  (setq elpy-modules (delq 'elpy-module-flymake elpy-modules))
  (add-hook 'elpy-mode-hook 'flycheck-mode))

;; enable autopep8 formatting on save
(require 'py-autopep8)
(add-hook 'elpy-mode-hook 'py-autopep8-enable-on-save)

(provide 'ds-python)
