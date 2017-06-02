;;------------------------------------------------------------------------------
;; Package: yasnippet
;;------------------------------------------------------------------------------
(use-package yasnippet
	     ;; :defer t
	     :ensure t
	     :config
	     (yas-reload-all)
	     (yas-global-mode))

;; make Ctrl-c k the only trigger key for yas
;; (define-key yas-minor-mode-map (kbd "<tab>") nil)
;; (define-key yas-minor-mode-map (kbd "TAB") nil)
;; (define-key yas-minor-mode-map (kbd "C-c k") 'yas-expand)

(provide 'ds-yasnippet)
