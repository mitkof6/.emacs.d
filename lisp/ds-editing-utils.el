;; diminsh minor mode name to save mode line space
(require-package 'diminish)

;; some default value
;; (setq-default
;;  blink-cursor-delay 0.5
;;  blink-cursor-interval 0.4
;;  buffers-menu-max-size 20
;;  case-fold-search t
;;  column-number-mode t
;;  compilation-scroll-output t
;;  delete-selection-mode t
;;  grep-scroll-output t
;;  indent-tabs-mode nil
;;  line-spacing 0.2
;;  make-backup-files nil
;;  mouse-yank-at-point t
;;  save-interprogram-paste-before-kill t
;;  scroll-preserve-screen-position 'always
;;  scroll-step 1
;;  scroll-margin 3
;;  scroll-conservatively 10000
;;  set-mark-command-repeat-pop t
;;  show-trailing-whitespace t
;;  tooltip-delay 1.5
;;  truncate-lines nil
;;  truncate-partial-width-windows nil
;;  visible-bell t)

;; global default indentation
(setq tab-stop-list '(4 8 12 16 20 24 28 32 36 40 44 48 52 56 60)
      tab-width 4
      indent-tabs-mode nil)  ; use spaces only if nil

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

;; (require 'cc-mode)
(add-hook 'c-mode-common-hook 'ds/code-indentation)
(add-hook 'java-mode-hook 'ds/code-indentation)
;; (add-hook 'python-mode-hook 'ds/code-indentation)

;; auto-revert (changes to files are seen by emacs)
(global-auto-revert-mode)
(setq global-auto-revert-non-file-buffers t
      auto-revert-verbose t)


;; hippie expand is dabbrev expand on steroids
;;(require 'hippie-expand)
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

;; ;; smart pairing for all
;; (require-package 'smartparens)
;; (require 'smartparens-config)
;; (setq sp-base-key-bindings 'paredit)
;; (setq sp-autoskip-closing-pair 'always)
;; (setq sp-hybrid-kill-entire-symbol nil)
;; (sp-use-paredit-bindings)
;; (show-smartparens-global-mode +1)

;; enable auto-pairing
(require-package 'autopair)
(autopair-global-mode t)
(show-paren-mode t)
(diminish 'autopair-mode)

;; visual line (for good world wrapping when lines are long)
(global-visual-line-mode t)
;; (setq line-move-visual nil)
(diminish 'visual-line-mode)

;; ;; enable uppercase and lowercase transform for region
;; (put 'upcase-region 'disabled nil)
;; (put 'downcase-region 'disabled nil)

;; ;; use page-break-line to handle the ^l page-breaking symbol
;; (require-package 'page-break-lines)
;; (global-page-break-lines-mode)
;; (diminish 'page-break-lines-mode)

;; enable subword-mode (move between camel case words)
(global-subword-mode t)

;; multiple-cursors-mode
;; (require-package 'multiple-cursors)
;; multiple-cursors
;; (global-set-key (kbd "c-<") 'mc/mark-previous-like-this)
;; (global-set-key (kbd "c->") 'mc/mark-next-like-this)
;; (global-set-key (kbd "c-+") 'mc/mark-next-like-this)
;; (global-set-key (kbd "c-c c-<") 'mc/mark-all-like-this)
;; from active region to multiple cursors:
;; (global-set-key (kbd "c-c c r") 'set-rectangular-region-anchor)
;; (global-set-key (kbd "c-c c c") 'mc/edit-lines)
;; (global-set-key (kbd "c-c c e") 'mc/edit-ends-of-lines)
;; (global-set-key (kbd "c-c c a") 'mc/edit-beginnings-of-lines)

;; undo-tree
(require-package 'undo-tree)
(global-undo-tree-mode t)
(diminish 'undo-tree-mode)

;; paredit
(require-package 'paredit)
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
(add-hook 'clojure-mode-hook 'lisp-mode-paredit-hook)

;; hide and show
;; (load-library "hideshow")
(add-hook 'c-mode-common-hook   'hs-minor-mode)
(add-hook 'emacs-lisp-mode-hook 'hs-minor-mode)
(add-hook 'java-mode-hook       'hs-minor-mode)
(add-hook 'lisp-mode-hook       'hs-minor-mode)
(add-hook 'python-mode-hook     'hs-minor-mode)
(add-hook 'sh-mode-hook         'hs-minor-mode)
(setq hs-hide-comments nil)

;; whitespace
(require-package 'whitespace-cleanup-mode)
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

;; predictive mode
;; (add-to-list 'load-path "~/.emacs.d/lisp/predictive")
;; (add-to-list 'load-path "~/.emacs.d/lisp/predictive/latex")
;; (add-to-list 'load-path "~/.emacs.d/lisp/predictive/texinfo"XS)
;; (require 'predictive)
;; (autoload 'predictive-mode "~/.emacs.d/lisp/predictive/predictive"
;;                "Turn on Predictive Completion Mode." t)
;; (set-default 'predictive-auto-add-to-dict t)
;; (setq predictive-main-dict 'rpg-dictionary
;;       predictive-auto-learn t
;;       predictive-add-to-dict-ask nil
;;       predictive-use-auto-learn-cache nil
;;       predictive-which-dict t)

(provide 'ds-editing-utils)
