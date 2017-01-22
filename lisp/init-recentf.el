(require 'recentf)
(recentf-mode 1)
(setq recentf-max-saved-items 25
      recentf-exclude '("/tmp/" "/ssh:"))
(setq recentf-max-menu-item 25)

(provide 'init-recentf)
