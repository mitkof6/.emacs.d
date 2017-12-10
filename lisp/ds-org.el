(use-package org
	     :ensure t
	     :mode "\\.org\\'"
	     :after flyspell
	     :config
	     ;; turn flyspell on
	     (add-hook 'org-mode-hook 'turn-on-flyspell)
	     ;; defines todo sequence (global)
	     (setq org-todo-keywords
		   '((sequence "TODO" "IN-PROGRESS" "WAITING" "DONE"))
		   ;; add done time stamp
		   org-log-done t
		   ;; add note when done
		   org-log-done 'note)
	     :bind (("\C-cl" . org-store-link)
		    ("\C-ca" . org-agenda)
		    ("\C-cc" . org-capture)
		    ("\C-ci" . org-iswitchb)))

(provide 'ds-org)
