;; Enable org-mode
(require-package 'org)
;; Make org-mode work with files ending in .org
(add-to-list 'auto-mode-alist '("\\.org$" . org-mode))
;; The above is the default in recent emacs

(provide 'ds-org)
