(use-package dired
	     ;;:ensure t
	     :defer t
	     :config
	     (toggle-diredp-find-file-reuse-dir 1)
	     (setq dired-recursive-deletes 'top))

(use-package dired+
	     :ensure t
	     :defer t
	     :after dired
	     )

(provide 'ds-dired)
