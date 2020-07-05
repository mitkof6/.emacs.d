;;; -*- mode: emacs-lisp -*-
;;
;; Emacs configuration file
;;
;; Author: Dimitar Stanev

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; general
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(package-initialize)

(require 'cl)
(require 'cl-lib)
(require 'package)

;; configure packages
(setq package-archives '(("org" . "http://orgmode.org/elpa/")
                         ("gnu" . "https://elpa.gnu.org/packages/")
                         ("melpa" . "https://melpa.org/packages/")))


;; add directory to load-path
(setq custom-file (expand-file-name "custom.el" user-emacs-directory))

;;On-demand installation of packages
(defun require-package (package &optional min-version no-refresh)
  "Install given PACKAGE, optionally requiring MIN-VERSION.
If NO-REFRESH is non-nil, the available package lists will not be
re-downloaded in order to locate PACKAGE."
  (if (package-installed-p package min-version)
      t
      (if (or (assoc package package-archive-contents) no-refresh)
          (if (boundp 'package-selected-packages)
              ;; Record this as a package the user installed explicitly
              (package-install package nil)
              (package-install package))
          (progn
            (package-refresh-contents)
            (require-package package min-version t)))))


;; use-package
;; (unless (package-installed-p 'use-package)
;;   (package-install 'use-package))
(require-package 'use-package)

;; start emacs server
(require 'server)
(or (server-running-p)
    (server-start))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; ui customization
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; revert open bufers
(global-auto-revert-mode t)

;; enable y/n answers
(fset 'yes-or-no-p 'y-or-n-p)

;; disable toolbar
(when (fboundp 'tool-bar-mode)
  (tool-bar-mode -1))

;; disable menu bar
(menu-bar-mode -1)

;; disable blinking cursor
(blink-cursor-mode -1)

;; record windows configurations
(winner-mode t)

;; enable subword-mode (move between camel case words)
(global-subword-mode t)

;; show trailing white space
(setq-default show-trailing-whitespace t)

;; use space instead of tabs with width = 4
(setq tab-width 4
      indent-tabs-mode t
      tab-stop-list (number-sequence 4 200 4))

(use-package clang-format
             :ensure t
             :config
             ;; (global-set-key (kbd "C-c i") 'clang-format-region)
             ;; (global-set-key (kbd "C-c u") 'clang-format-buffer)
             (setq clang-format-style-option "file"))

;; show available keybindings after you start typing
(use-package which-key
             :ensure t
             :config
             (which-key-mode +1))

(setq
 ;; more useful frame title
 frame-title-format '((:eval (if (buffer-file-name)
                                 (abbreviate-file-name (buffer-file-name))
                                 "%b")))
 ;; disable startup screen
 inhibit-startup-screen t)

;; theme
(use-package moe-theme
             :ensure t
             :config
             (load-theme 'moe-dark t)
             (use-package powerline
                          :ensure t)
             (use-package smart-mode-line-powerline-theme
                          :ensure t
                          :config
                          (powerline-moe-theme)))

;; (use-package spacemacs-theme
;;              :ensure t
;;              :defer t
;;              :init (load-theme 'spacemacs-dark t))

(use-package ivy
             :ensure t
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
                         (ivy-mode 1))))

(use-package counsel
             :ensure t
             :config
             (setq-default counsel-mode-override-describe-bindings t)
             (add-hook 'after-init-hook 'counsel-mode))

(use-package swiper
             :ensure t
             :config
             (define-key ivy-mode-map (kbd "C-s") 'swiper))

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
             (show-smartparens-global-mode +1))

;; ;; undo-tree
;; (use-package undo-tree
;;              :ensure t
;;              :config
;;              (global-undo-tree-mode t)
;;              ;; :diminish undo-tree-mode
;;           )

;; move cursor to other buffers
(global-set-key (kbd "C-c b p") 'windmove-up)
(global-set-key (kbd "C-c b n") 'windmove-down)
(global-set-key (kbd "C-c b b") 'windmove-left)
(global-set-key (kbd "C-c b f") 'windmove-right)

;; move buffers
(use-package buffer-move
             :ensure t
             :bind (("C-c b P" . buf-move-up)
                    ("C-c b N" . buf-move-down)
                    ("C-c b B" . buf-move-left)
                    ("C-c b F" . buf-move-right)))

;; transpose buffers horizontally
(use-package transpose-frame
             :ensure t
             :bind ("C-c b t" . flop-frame))

;; show files on tab
(use-package tabbar
             :ensure t
             :config
             (tabbar-mode 't)
             :bind (("C-c n" . tabbar-forward)
                    ("C-c p" . tabbar-backward)))

;; Stefan Monnier <foo at acm.org>. It is the opposite of fill-paragraph
(defun ds/unfill-paragraph (&optional region)
  "Takes a multi-line paragraph and makes it into a single line of text."
  (interactive (progn (barf-if-buffer-read-only) '(t)))
  (let ((fill-column (point-max))
        ;; This would override `fill-column' if it's an integer.
        (emacs-lisp-docstring-fill-column t))
    (fill-paragraph nil region)))
(global-set-key (kbd "C-c b u") 'ds/unfill-paragraph)

;; font size
(global-set-key (kbd "C-+") 'text-scale-increase)
(global-set-key (kbd "C--") 'text-scale-decrease)

;; start eshell or switch to it if it's active.
(global-set-key (kbd "C-c u e") 'eshell)

;; find and open file
(global-set-key (kbd "C-c b o") 'ffap)

;; expand region to select region by semantics
(use-package expand-region
             :ensure t
             :bind ("C-=" . er/expand-region))

;; copy/cut whole line without selecting (better than
;; whole-line-or-region-mode)
(defun ds/string-chomp (str)
  "Chomp leading and tailing whitespace from str."
  (while (string-match "\\`\n+\\|^\\s-+\\|\\s-+$\\|\n+\\'"
                       str)
         (setq str (replace-match "" t t str)))
  str)

(defun ds/chomp-kill-ring-car ()
  "Clear whitespaces from first element of kill ring."
  (let ((str (pop kill-ring)))
    (push (ds/string-chomp str) kill-ring)))

(defun ds/copy-whole-line ()
  "Place current line in kill ring."
  (interactive)
  (kill-whole-line)
  (yank)
  (ds/chomp-kill-ring-car))
(global-set-key (kbd "C-c u c") 'ds/copy-whole-line)

(defun ds/cut-whole-line ()
  "Place current line in kill ring and kill."
  (interactive)
  (kill-whole-line)
  (ds/chomp-kill-ring-car))
(global-set-key (kbd "C-c u x") 'ds/cut-whole-line)

;; delete
(defun ds/delete-line-no-kill ()
  (interactive)
  (delete-region
   (point)
   (save-excursion (move-end-of-line 1) (point)))
  (delete-char 1))
(global-set-key (kbd "C-c u d") 'ds/delete-line-no-kill)

;; kill all buffers
(defun ds/kill-all-buffers ()
  (interactive)
  (mapc 'kill-buffer (buffer-list)))
(global-set-key (kbd "C-c b a") 'ds/kill-all-buffers)

;; kill current buffer without confirmation
(global-set-key (kbd "C-c b k") 'kill-this-buffer)

;; eval-buffer
(global-set-key (kbd "C-c b e") 'eval-buffer)

;; use hippie-expand instead of dabbrev
(global-set-key (kbd "M-/") 'hippie-expand)

;; save word to dictionary
(defun ds/save-to-dictionary ()
  (interactive)
  (let ((current-location (point))
        (word (flyspell-get-word)))
    (when (consp word)
      (flyspell-do-correct 'save nil (car word)
                           current-location (cadr word) (caddr word)
                           current-location))))
(global-set-key [f1] 'ds/save-to-dictionary)
(global-set-key (kbd "C-c b s") 'flyspell-buffer)
(add-hook 'text-mode-hook 'flyspell-mode)
(add-hook 'prog-mode-hook 'flyspell-prog-mode)

;; change dictionary toggle
(lexical-let ((dictionaries '("en" "el")))
             (rplacd (last dictionaries) dictionaries)
             (defun ds/ispell-change-to-next-dictionary ()
               (interactive)
               (ispell-change-dictionary (pop dictionaries))))
(global-set-key [f2] 'ds/ispell-change-to-next-dictionary)

;; toggle flyspell mode
(global-set-key [f3] 'flyspell-mode)

;; toggle flycheck mode
(global-set-key [f4] 'flycheck-mode)

;; clear and auto-indent, hook before save
(defun ds/clear-and-indent()
  "Indents an entire buffer using the default intending scheme."
  (interactive)
  (save-excursion
   (delete-trailing-whitespace)
   (indent-region (point-min) (point-max) nil)
   (untabify (point-min) (point-max))))
(global-set-key [f5] 'ds/clear-and-indent)
;; too slow for big files
;; (add-hook 'before-save-hook #'ds/clear-and-indent)

;; line number
(use-package hlinum
             :ensure t
             :config
             (column-number-mode nil)
             (size-indication-mode nil)
             :bind ("<f9>" . global-linum-mode))

;; fill column indicator
(use-package fill-column-indicator
             :ensure t
             :config
             (define-globalized-minor-mode global-fci-mode fci-mode
               (lambda ()
                 (when (and (not (string-match "^\*.*\*$" (buffer-name)))
                            (not (eq major-mode 'dired-mode)))
                   (setq fci-rule-color "darkgrey")
                   (setq fill-column 80)
                   (fci-mode 1))))
             :bind ("<f10>" . global-fci-mode))

;; 80 char mark and utility for whitespace
(use-package whitespace
             :ensure t
             :bind ("<f11>" . global-whitespace-mode))

;; toggle save emacs sessions
;; (global-set-key [f12] 'desktop-save-mode)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; packages
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; recent opened files
(use-package recentf
             :ensure t
             :config
             (recentf-mode 1)
             (setq recentf-max-saved-items 100
                   recentf-exclude '("/tmp/" "/ssh:")
                   recentf-max-menu-item 100))

(use-package pdf-tools
             :ensure t
             :config
             (pdf-tools-install)
             ;; (add-to-list 'pdf-tools-enabled-modes 'pdf-view-midnight-minor-mode)
             ;; (setq pdf-view-midnight-colors '("#d6d6d6" . "#000000"))
             (defun ds/disable-cursor-blink () (blink-cursor-mode 0))
             (add-hook 'pdf-view-mode-hook 'ds/disable-cursor-blink))

;; (use-package auto-complete
;;              :ensure t
;;              :config
;;              (ac-config-default))

(use-package yasnippet-snippets
             :ensure t)

(use-package yasnippet
             :ensure t
             :config
             (yas-global-mode 1))

(use-package company
             :ensure t
             :bind ("M-RET" . company-complete)
	     :init (add-hook 'after-init-hook 'global-company-mode)
             :config
             (setq company-idle-delay 0
                   company-minimum-prefix-length 2
                   company-show-numbers t
                   company-tooltip-limit 20
                   company-dabbrev-downcase nil))

;; GLSL shader mode
(use-package shader-mode
             :ensure t
             :mode ("\\.fragmentshader\\'" "\\.vertexshader\\'"))

;; languagetool
(use-package langtool
             :ensure t
             :config
             (setq langtool-java-classpath
                   "/usr/share/languagetool:/usr/share/java/languagetool/*")
             :bind ("C-c b l" . langtool-check-buffer))

;; define word
(use-package define-word
             :ensure t)

;; synonyms
(use-package synosaurus
             :ensure t)

;; hide and show code snippets
(use-package hideshow
             :ensure t
             :config
             (add-hook 'prog-mode-hook #'hs-minor-mode))

;; flycheck is better than flymake
(use-package flycheck
             :ensure t
             :config
             (global-flycheck-mode))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; lsp
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(use-package lsp-mode
             :ensure t
             :commands lsp
             :config
             ;; prefer using lsp-ui (flycheck)
             (setq lsp-prefer-flymake nil)
             ;; setup clangd
             (setq lsp-clients-clangd-args '("-j=2" "-background-index" "-log=error"))
             ;; hook languages
             ;; (add-hook 'python-mode-hook 'lsp)
             (add-hook 'c-mode-common-hook 'lsp))

;; lsp-treemacs
(use-package lsp-treemacs
             :ensure t
             :config
             (lsp-treemacs-sync-mode 1))

;; lsp extras
(use-package lsp-ui
             :ensure t
             :requires lsp-mode flycheck
             :config
             ;; setup lsp-ui
             (setq lsp-ui-doc-enable t
                   lsp-ui-doc-use-childframe t
                   lsp-ui-doc-position 'top
                   lsp-ui-doc-include-signature t
                   lsp-ui-sideline-enable nil
                   lsp-ui-flycheck-enable t
                   lsp-ui-flycheck-list-position 'right
                   lsp-ui-flycheck-live-reporting t
                   lsp-ui-peek-enable t
                   lsp-ui-peek-list-width 60
                   lsp-ui-peek-peek-height 25)
             (add-hook 'lsp-mode-hook 'lsp-ui-mode))

(use-package company-lsp
             :ensure t
             :commands company-lsp
             :config
             (push 'company-lsp company-backends)
             ;; disable client-side cache because the LSP server does a better job.
             (setq company-transformers nil
                   company-lsp-async t
                   company-lsp-cache-candidates nil))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; python
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(setq python-indent-offset 4
      python-shell-interpreter "ipython3"
      python-shell-interpreter-args "--simple-prompt --pprint --matplotlib"
      elpy-rpc-python-command "python3")

(defun ds/python-shell-send-snippet ()
  (interactive)
  (save-excursion
   (search-backward "# %%")
   (end-of-line)
   (set-mark-command nil)
   (search-forward "# %%")
   (call-interactively 'python-shell-send-region)
   (deactivate-mark)))

(defun ds/python-hook ()
  ;; (linum-mode)
  (flyspell-prog-mode)
  (local-set-key (kbd "C-c C-g") 'ds/python-shell-send-snippet))
(add-hook 'python-mode-hook 'ds/python-hook)

(use-package elpy
	     :ensure t
	     :init
	     (elpy-enable))

(use-package py-autopep8
             :ensure t)

(use-package blacken
			 :ensure t)

(use-package ein
             :ensure t
             :config
             (setq ein:use-auto-complete-superpack t
                   ein:output-type-preference '(emacs-lisp svg png jpeg html
                                                text latex javascript)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; C++
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun ds/c++-hook ()
  ;; (linum-mode)
  (c-set-offset 'substatement-open 0) ;; close statement
  (c-set-offset 'arglist-intro '+)    ;; long argument names
  (setq c++-tab-always-indent t
        c-basic-offset 4
        c-indent-level 4
        tab-width 4
        indent-tabs-mode t)
  (flyspell-prog-mode))
(add-hook 'c-mode-common-hook 'ds/c++-hook)

(use-package cmake-ide
             :ensure t
             :config
             ;; (cmake-ide-setup)
             (setq ; cmake-ide-flags-c++ (append '("-std=c++11"))
                   cmake-ide-make-command "make --no-print-directory -j4"
                   compilation-skip-threshold 2 ;; show only errors
                   compilation-auto-jump-to-first-error t) ;; go to first error
             :bind ("C-c m" . cmake-ide-compile))

;; make sure cmake-mode is installed for viewing CMake files
(use-package cmake-mode
             :ensure t)

;; emacs Lisp defun to bury the compilation buffer if everything
;; compiles smoothly
(defun ds/bury-compile-buffer-if-successful (buffer string)
  (when (and
         (string-match "compilation" (buffer-name buffer))
         (string-match "finished" string)
         (not (search-forward "warning" nil t)))
    (bury-buffer buffer)
    (switch-to-prev-buffer (get-buffer-window buffer) 'kill)))
(add-hook 'compilation-finish-functions 'ds/bury-compile-buffer-if-successful)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; octave
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(setq auto-mode-alist
      (cons '("\\.m$" . octave-mode) auto-mode-alist))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; lisp
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; common lisp
(use-package slime
             :ensure t
             :config
             (add-hook 'lisp-mode-hook (lambda () (slime-mode t)))
             (add-hook 'inferior-lisp-mode-hook (lambda () (inferior-slime-mode t)))
             (setq inferior-lisp-program "/usr/bin/sbcl")
             (defalias 'equalp 'cl-equalp)
             (autoload 'slime "slime" "Superior Lisp Interaction Mode for Emacs" t)
             (slime-setup '(slime-asdf slime-banner slime-clipboard
                            slime-compiler-notes-tree slime-fancy
                            slime-fontifying-fu slime-hyperdoc
                            slime-indentation slime-media
                            slime-mrepl slime-parse
                            slime-sbcl-exts slime-sprof
                            slime-xref-browser))
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
                                                       "~/dev/archlinux-config/lisp/HyperSpec/"))))

;; scheme, racket
(use-package racket-mode
             :ensure t
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

;; ;; clojure
;; (use-package cider
;;   :ensure t)

;; hy
(use-package hy-mode
             :ensure t
             :config
             ;; (defun ds/hy-hook ()
             ;;   (company-mode 1)
             ;;   (run-jedhy))
             ;; (add-hook 'hy-mode-hook 'ds/hy-hook)
	     )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; latex
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(require-package 'auctex)

;; make AUCTeX aware of style files and multi-file documents
(setq TeX-auto-save t)
(setq TeX-parse-self t)
(setq-default TeX-master nil)

;; configure reftex
(require 'reftex)
(add-hook 'LaTeX-mode-hook 'turn-on-reftex)   ; with AUCTeX LaTeX mode
(add-hook 'latex-mode-hook 'turn-on-reftex)   ; with Emacs latex mode

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; magit binding
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(use-package magit
             :ensure t
             :config
             ;; show word by word difference
             (setq magit-diff-refine-hunk 'all)
             :bind (("C-c g s" . magit-status)
                    ("C-c g i" . magit-init)
                    ("C-c g t" . magit-stash)
                    ("C-c g l" . magit-log)
                    ("C-c g c" . magit-commit)
                    ("C-c g p" . magit-push-other)
                    ("C-c g u" . magit-pull)
                    ("C-c g d" . magit-diff)
                    ("C-c g o" . magit-checkout)
                    ("C-c g m" . magit-merge)
                    ("C-c g a" . magit-remote-add)
                    ("C-c g r" . magit-remote-remove)
                    ("C-c g n" . magit-clone)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; custom
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(when (file-exists-p custom-file)
  (load custom-file))

(provide 'init)
