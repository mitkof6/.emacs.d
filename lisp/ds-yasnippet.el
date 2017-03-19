;;------------------------------------------------------------------------------
;; Package: yasnippet
;;------------------------------------------------------------------------------
(require-package 'yasnippet)
;; To get a bunch of extra snippets that come in super handy see:
;; https://github.com/AndreaCrotti/yasnippet-snippets
;; or use:
;; git clone https://github.com/AndreaCrotti/yasnippet-snippets.git
;; ~/.emacs.d/snippets/
(yas-global-mode 1)
(yas-reload-all)

;; make Ctrl-c k the only trigger key for yas
;; (define-key yas-minor-mode-map (kbd "<tab>") nil)
;; (define-key yas-minor-mode-map (kbd "TAB") nil)
;; (define-key yas-minor-mode-map (kbd "C-c k") 'yas-expand)

(provide 'ds-yasnippet)
