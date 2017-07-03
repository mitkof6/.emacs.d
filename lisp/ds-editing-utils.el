;; global default indentation
(setq tab-stop-list '(4 8 12 16 20 24 28 32 36 40 44 48 52 56 60)
      tab-width 4
      indent-tabs-mode nil)  ; use spaces only if nil

;; the number of characters for fill command
(setq-default fill-column 80)

(defun ds/code-indentation ()
  "Preferences for indentation."
  ;; close statement
  (c-set-offset 'substatement-open 0)
  ;; long argument names
  (c-set-offset 'arglist-intro '+)
  ;; indentation
  (setq c++-tab-always-indent t
        c-basic-offset 4
        c-indent-level 4))
(add-hook 'c-mode-common-hook 'ds/code-indentation)
(add-hook 'java-mode-hook 'ds/code-indentation)

;; auto-revert (changes to files are seen by emacs)
(global-auto-revert-mode)
(setq global-auto-revert-non-file-buffers t
      auto-revert-verbose t)

;; hippie expand is dabbrev expand on steroids
(setq hippie-expand-try-functions-list
      '(try-expand-dabbrev
        try-expand-dabbrev-all-buffers
        try-expand-dabbrev-from-kill
        try-complete-file-name-partially
        try-complete-file-name
        try-expand-all-abbrevs
        try-expand-list
        try-expand-line
        try-complete-lisp-symbol-partially
        try-complete-lisp-symbol))

(use-package smartparens
  :ensure t
  :config
  (require 'smartparens-config)
  (setq sp-base-key-bindings 'paredit
        sp-autoskip-closing-pair 'always
        sp-hybrid-kill-entire-symbol nil)
  ;; (smartparens-global-strict-mode)
  (sp-use-paredit-bindings)
  (show-smartparens-global-mode +1)
  :diminish smartparens-mode)

;; enable autopairing (disabled use smartparens)
(use-package autopair
  :ensure t
  :disabled
  :config
  (autopair-global-mode t)
  (show-paren-mode t)
  :diminish autopair-mode)

;; visual line (for good world wrapping when lines are long)
;; (global-visual-line-mode t)
;; (setq line-move-visual nil)
;; (diminish 'visual-line-mode)

;; auto-fill mode
(add-hook 'text-mode-hook 'turn-on-auto-fill)

;; ;; enable uppercase and lowercase transform for region
;; (put 'upcase-region 'disabled nil)
;; (put 'downcase-region 'disabled nil)

;; ;; use page-break-line to handle the ^l page-breaking symbol
;; (require-package 'page-break-lines)
;; (global-page-break-lines-mode)
;; (diminish 'page-break-lines-mode)

;; enable subword-mode (move between camel case words)
(global-subword-mode t)

;; undo-tree
(use-package undo-tree
  :ensure t
  :config
  (global-undo-tree-mode t)
  :diminish undo-tree-mode)

;; paredit
(use-package paredit
  :ensure t
  :disabled
  :config
  (defun paredit-space-for-delimiter-p-lisp (endp delimiter)
    "Don't add space after #/."
    nil)
  (defun lisp-mode-paredit-hook ()
    (enable-paredit-mode)
    (add-to-list (make-local-variable 'paredit-space-for-delimiter-predicates)
                 'paredit-space-for-delimiter-p-lisp))
  (add-hook 'lisp-mode-hook 'lisp-mode-paredit-hook)
  (add-hook 'lisp-interaction-mode-hook 'lisp-mode-paredit-hook)
  (add-hook 'emacs-lisp-mode-hook 'lisp-mode-paredit-hook)
  (add-hook 'clojure-mode-hook 'lisp-mode-paredit-hook))

;; hide and show
(add-hook 'c-mode-common-hook   'hs-minor-mode)
(add-hook 'emacs-lisp-mode-hook 'hs-minor-mode)
(add-hook 'java-mode-hook       'hs-minor-mode)
(add-hook 'lisp-mode-hook       'hs-minor-mode)
(add-hook 'python-mode-hook     'hs-minor-mode)
(add-hook 'sh-mode-hook         'hs-minor-mode)
(setq hs-hide-comments nil)

;; whitespace
(use-package whitespace-cleanup-mode
  :ensure t
  :config
  (global-whitespace-cleanup-mode t)
  (global-set-key [remap just-one-space] 'cycle-spacing)
  ;; (setq-default show-trailing-whitespace t)
  ;; but don't show trailing whitespace in SQLi, inf-ruby etc.
  (defun ds/no-trailing-whitespace ()
    "Turn off display of trailing whitespace in this buffer."
    (setq show-trailing-whitespace nil))
  (dolist (hook '(special-mode-hook
                  Info-mode-hook
                  eww-mode-hook
                  term-mode-hook
                  comint-mode-hook
                  compilation-mode-hook
                  twittebring-mode-hook
                  minibuffer-setup-hook))
    (add-hook hook 'ds/no-trailing-whitespace))
  :diminish whitespace-cleanup-mode)

(provide 'ds-editing-utils)
