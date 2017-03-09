;; Font size
(global-set-key (kbd "C-+") 'text-scale-increase)
(global-set-key (kbd "C--") 'text-scale-decrease)

;; Start eshell or switch to it if it's active.
(global-set-key (kbd "C-x m") 'eshell)

;; kill all buffers
(global-set-key (kbd "C-c C-k") 'ds/close-all-buffers)

(provide 'ds-global-key-bindings)
