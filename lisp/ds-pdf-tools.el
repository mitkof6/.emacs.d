(use-package pdf-tools
	     :ensure t
	     :init (pdf-tools-install)
	     )

;; (add-to-list 'pdf-tools-enabled-modes 'pdf-view-midnight-minor-mode)
;; (setq pdf-view-midnight-colors '("#d6d6d6" . "#000000"))

(provide 'ds-pdf-tools)
