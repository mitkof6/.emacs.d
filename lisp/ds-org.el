;; Enable org-mode
(require-package 'org)
;; Make org-mode work with files ending in .org
(add-to-list 'auto-mode-alist '("\\.org$" . org-mode))
;; The above is the default in recent emacs

;; basic global key combinations are defined
(define-key global-map "\C-cl" 'org-store-link)
(define-key global-map "\C-ca" 'org-agenda)
(setq org-log-done t)

;; turn flyspell on
(add-hook 'org-mode-hook 'turn-on-flyspell)

;; add workflow
(setq org-todo-keywords
      '((sequence "TODO" "IN-PROGRESS" "WAITING" "DONE")))

(provide 'ds-org)
