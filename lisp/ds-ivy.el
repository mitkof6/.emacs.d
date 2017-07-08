(use-package ivy
  :ensure t
  ;;:defer t
  :config
  (setq-default ivy-use-virtual-buffers t
                ivy-count-format ""
                projectile-completion-system 'ivy
                ivy-initial-inputs-alist
                '((counsel-M-x . "^")
                  (man . "^")
                  (woman . "^")))
  ;; IDO-style directory navigation
  (define-key ivy-minibuffer-map (kbd "C-j") #'ivy-immediate-done)
  (define-key ivy-minibuffer-map (kbd "RET") #'ivy-alt-done)
  (defun ds/enable-ivy-flx-matching ()
    "Make `ivy' matching work more like IDO."
    (interactive)
    (require-package 'flx)
    (setq-default ivy-re-builders-alist
                  '((t . ivy--regex-fuzzy))))
  (add-hook 'after-init-hook
            (lambda ()
              (when (bound-and-true-p ido-ubiquitous-mode)
                (ido-ubiquitous-mode -1))
              (when (bound-and-true-p ido-mode)
                (ido-mode -1))
              (ivy-mode 1)))
  :diminish ivy-mode)

(use-package counsel
  :ensure t
  ;;:defer t
  :config
  (setq-default counsel-mode-override-describe-bindings t)
  (add-hook 'after-init-hook 'counsel-mode)
  :diminish counsel-mode)

(use-package swiper
  :ensure t
  ;; :defer t
  :after ivy
  :config
  (define-key ivy-mode-map (kbd "C-s") 'swiper))

(provide 'ds-ivy)
