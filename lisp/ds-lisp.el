;; setup sbcl slime
(require-package 'slime)
(add-hook 'lisp-mode-hook (lambda () (slime-mode t)))
(add-hook 'inferior-lisp-mode-hook (lambda () (inferior-slime-mode t)))
(setq inferior-lisp-program "/usr/bin/sbcl")

;; setup racket
(require-package 'racket-mode)

(provide 'ds-lisp)
