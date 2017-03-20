;; Enable org-mode
(require-package 'org)

;; Make org-mode work with files ending in .org
(add-to-list 'auto-mode-alist '("\\.org$" . org-mode))

;; basic global key combinations are defined
(define-key global-map "\C-cl" 'org-store-link)
(define-key global-map "\C-ca" 'org-agenda)
(define-key global-map "\C-cc" 'org-capture)
(define-key global-map "\C-ci" 'org-iswitchb)

;; turn flyspell on
(add-hook 'org-mode-hook 'turn-on-flyspell)

;; defines todo sequence (global)
(setq org-todo-keywords
      '((sequence "TODO" "IN-PROGRESS" "WAITING" "DONE")))

;; add done time stamp
(setq org-log-done t)
;; add note when done
(setq org-log-done 'note)

(provide 'ds-org)
