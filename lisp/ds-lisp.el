(use-package slime
  :ensure t
  ;;:defer t
  :config
  (add-hook 'lisp-mode-hook (lambda () (slime-mode t)))
  (add-hook 'inferior-lisp-mode-hook (lambda () (inferior-slime-mode t)))
  (setq inferior-lisp-program "/usr/bin/sbcl")

  (defalias 'equalp 'cl-equalp)
  (autoload 'slime "slime" "Superior Lisp Interaction Mode for Emacs" t)
  (slime-setup '(slime-asdf slime-banner slime-clipboard slime-compiler-notes-tree
                            slime-fancy slime-fontifying-fu slime-hyperdoc slime-indentation
                            slime-media slime-mrepl slime-parse slime-presentation-streams
                            slime-sbcl-exts slime-sprof slime-xref-browser))
  (setq slime-header-line-p nil
        common-lisp-style 'modern
        slime-startup-animation nil
        slime-enable-evaluate-in-emacs t
        slime-net-coding-system 'utf-8-unix
        lisp-indent-function 'common-lisp-indent-function
        inferior-lisp-program "sbcl --dynamic-space-size 4096"
        ;; "ccl -K utf-8" "ecl" "alisp" "ccl" "clisp" "abcl"
        slime-complete-symbol-function 'slime-fuzzy-complete-symbol
        common-lisp-hyperspec-root (concat "file://"
                                           (expand-file-name
                                            "~/dev/archlinux-config/lisp/HyperSpec/")))
             )

(use-package racket-mode
  :ensure t
  ;;:defer t
  :config
  (setq tab-always-indent 'complete)
  (set (make-local-variable 'eldoc-documentation-function) 'racket-eldoc-function)
  ;; scheme
  ;; (add-hook 'geiser-repl-mode-hook 'lisp-mode-paredit-hook)
  ;; (add-hook 'slime-repl-mode-hook 'lisp-mode-paredit-hook)
  ;; (add-hook 'scheme-mode-hook 'lisp-mode-paredit-hook)
  ;; (setq scheme-program-name "scheme" ;; "racket"
  ;;       geiser-scheme-implementation 'chicken
  ;;       geiser-debug-show-debug-p nil
  ;;       geiser-debug-jump-to-debug-p nil)
  )

(use-package cider
  :ensure t
             ;; :pin melpa-stable
             )

(provide 'ds-lisp)
