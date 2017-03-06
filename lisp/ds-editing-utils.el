;; enhanced help mechanism
(require-package 'help-fns+)

;; dimish minor mode name to save mode line space
(require-package 'diminish)

;; some default value
(setq-default
 blink-cursor-delay 0.5
 blink-cursor-interval 0.4
 buffers-menu-max-size 20
 case-fold-search t
 column-number-mode t
 compilation-scroll-output t
 delete-selection-mode t
 grep-scroll-output t
 indent-tabs-mode nil
 line-spacing 0.2
 make-backup-files nil
 mouse-yank-at-point t
 save-interprogram-paste-before-kill t
 scroll-preserve-screen-position 'always
 scroll-step 1
 scroll-margin 3
 scroll-conservatively 10000
 set-mark-command-repeat-pop t
 show-trailing-whitespace t
 tooltip-delay 1.5
 truncate-lines nil
 truncate-partial-width-windows nil
 visible-bell t)

;; auto-revert
(global-auto-revert-mode)
(setq global-auto-revert-non-file-buffers t
      auto-revert-verbose t)

;; enable auto-pairing
(require-package 'autopair)
(autopair-global-mode t)
(show-paren-mode t)
(diminish 'autopair-mode)

;; newline behavior
(global-set-key (kbd "RET") 'newline-and-indent)
(defun ds/newline-at-end-of-line ()
  "Move to end of line, enter a newline, and reindent."
  (interactive)
  (move-end-of-line 1)
  (newline-and-indent))

(global-set-key (kbd "<S-return>") 'ds/newline-at-end-of-line)

;; display line number
;; (require 'linum)
(require-package 'hlinum)
(hlinum-activate)
(global-linum-mode t)

;; visual line
(global-visual-line-mode t)
;; (diminish 'global-visual-line-mode)
(diminish 'visual-line-mode)

;; expand-region
(require-package 'expand-region)
(global-set-key (kbd "C-=") 'er/expand-region)

;; enable uppercase and lowercase transform for region
(put 'upcase-region 'disabled nil)
(put 'downcase-region 'disabled nil)

;; whole-line-or-region-mode
(require-package 'whole-line-or-region)
(whole-line-or-region-mode t)
(diminish 'whole-line-or-region-mode)
(make-variable-buffer-local 'whole-line-or-region-mode)

;; enable cua mode without prefix key
;; (cua-selection-mode t)

;; use page-break-line to handle the ^l page-breaking symbol
(require-package 'page-break-lines)
(global-page-break-lines-mode)
(diminish 'page-break-lines-mode)

;; enable subword-mode
;; (global-subword-mode t)

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
;; (global-set-key "\c-xu" 'undo-tree-visualize)
(global-undo-tree-mode t)
(diminish 'undo-tree-mode)

(provide 'ds-editing-utils)
