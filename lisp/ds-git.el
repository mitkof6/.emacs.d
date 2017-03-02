(require-package 'magit)

(global-set-key (kbd "M-g M-s") 'magit-status)
(global-set-key (kbd "M-g M-c") 'magit-checkout)

(provide 'ds-git)
