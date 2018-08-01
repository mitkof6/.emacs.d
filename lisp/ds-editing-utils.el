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

;; must be disabled so tabs are not inserted
(setq-default indent-tabs-mode nil)

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
             (smartparens-global-mode)
             (sp-use-paredit-bindings)
             (show-smartparens-global-mode +1)
             :diminish smartparens-mode)


;; visual line (for good world wrapping when lines are long)
;; (global-visual-line-mode t)
;; (setq line-move-visual nil)
;; (diminish 'visual-line-mode)

;; auto-fill mode
(add-hook 'text-mode-hook 'turn-on-auto-fill)

;; enable subword-mode (move between camel case words)
(global-subword-mode t)

;; undo-tree
(use-package undo-tree
             :ensure t
             :config
             (global-undo-tree-mode t)
             :diminish undo-tree-mode)

;; hide and show
(add-hook 'c-mode-common-hook   'hs-minor-mode)
(add-hook 'emacs-lisp-mode-hook 'hs-minor-mode)
(add-hook 'java-mode-hook       'hs-minor-mode)
(add-hook 'lisp-mode-hook       'hs-minor-mode)
(add-hook 'python-mode-hook     'hs-minor-mode)
(add-hook 'sh-mode-hook         'hs-minor-mode)
(setq hs-hide-comments nil)

;; recent opened files
(use-package recentf
             :ensure t
             :config
             (recentf-mode 1)
             (setq recentf-max-saved-items 100
                   recentf-exclude '("/tmp/" "/ssh:")
                   recentf-max-menu-item 100))

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

;; GLSL shader mode
(use-package shader-mode
             :ensure t
             :mode ("\\.fragmentshader\\'" "\\.vertexshader\\'"))

;; Stefan Monnier <foo at acm.org>. It is the opposite of fill-paragraph
(defun ds/unfill-paragraph (&optional region)
  "Takes a multi-line paragraph and makes it into a single line of text."
  (interactive (progn (barf-if-buffer-read-only) '(t)))
  (let ((fill-column (point-max))
        ;; This would override `fill-column' if it's an integer.
        (emacs-lisp-docstring-fill-column t))
    (fill-paragraph nil region)))

;; folding regions automatically
(use-package folding
             :ensure t
             :config
             (folding-add-to-marks-list 'tex-mode "%{{{" "%}}}" nil t)
             (folding-mode-add-find-file-hook)
             :diminish folding-mode)

;; markdown
(use-package markdown-mode
             :mode "\\.\\(md\\|markdown\\)\\'"
             :ensure t)

;; writegood-mode (duplicates, passive voice, )
(add-to-list 'load-path "~/.emacs.d/lisp/third/writegood-mode")
(require 'writegood-mode)
(global-set-key (kbd "C-c b s") 'writegood-mode)

;; languagetool
(use-package langtool
             :config
             (setq langtool-java-classpath
                   "/usr/share/languagetool:/usr/share/java/languagetool/*"))

;; define word
(use-package define-word)

;; synonyms
(use-package synosaurus)

(provide 'ds-editing-utils)
