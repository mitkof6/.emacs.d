(use-package org
  :mode "\\.org$"
  :ensure t
  ;; :defer t
  :config
  ;; turn flyspell on
  (add-hook 'org-mode-hook 'turn-on-flyspell)

  ;; the alist may not work
  (add-to-list 'auto-mode-alist '("\\.org$" . org-mode))

  ;; defines todo sequence (global)
  (setq org-todo-keywords
	'((sequence "TODO" "IN-PROGRESS" "WAITING" "DONE"))
	;; add done time stamp
	org-log-done t
	;; add note when done
	org-log-done 'note)

  ;; basic global key combinations are defined
  (define-key global-map "\C-cl" 'org-store-link)
  (define-key global-map "\C-ca" 'org-agenda)
  (define-key global-map "\C-cc" 'org-capture)
  (define-key global-map "\C-ci" 'org-iswitchb))

(provide 'ds-org)
