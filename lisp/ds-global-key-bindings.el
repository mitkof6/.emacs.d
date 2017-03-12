;; Font size
(global-set-key (kbd "C-+") 'text-scale-increase)
(global-set-key (kbd "C--") 'text-scale-decrease)

;; Start eshell or switch to it if it's active.
(global-set-key (kbd "C-x m") 'eshell)

;; kill all buffers
(defun ds/close-all-buffers ()
  (interactive)
  (mapc 'kill-buffer (buffer-list)))
(global-set-key (kbd "C-x C-k") 'ds/close-all-buffers)

;; eval-buffer
(global-set-key (kbd "C-c C-b") 'eval-buffer)

;; use hippie-expand instead of dabbrev
(global-set-key (kbd "M-/") 'hippie-expand)

;; change dictionary toggle
(lexical-let ((dictionaries '("en_US" "greek")))
             (rplacd (last dictionaries) dictionaries)
             (defun ds/ispell-change-to-next-dictionary ()
               (interactive)
               (ispell-change-dictionary (pop dictionaries))))
(global-set-key [f2] 'ds/ispell-change-to-next-dictionary)

;; toggle flyspell mode
(global-set-key [f3] 'flyspell-mode)

;; set ispell complete word
(global-set-key (kbd "C-c s") 'ispell-word)

;; this function is used to close the buffer (e.g. flyckeck error list)
(defun ds/quit-bottom-side-windows ()
  "Quit side windows of the current frame."
  (interactive)
  (dolist (window (window-at-side-list))
    (quit-window nil window)))
(global-set-key (kbd "C-c q") 'ds/quit-bottom-side-windows)

;; toggle flycheck mode
(global-set-key [f4] 'flycheck-mode)

;; newline behavior
(global-set-key (kbd "RET") 'newline-and-indent)
(defun ds/newline-at-end-of-line ()
  "Move to end of line, enter a newline, and reindent."
  (interactive)
  (move-end-of-line 1)
  (newline-and-indent))
(global-set-key (kbd "<S-return>") 'ds/newline-at-end-of-line)

;; fill column indicator
(require-package 'fill-column-indicator)
(define-globalized-minor-mode global-fci-mode fci-mode
  (lambda ()
    (when (and (not (string-match "^\*.*\*$" (buffer-name)))
               (not (eq major-mode 'dired-mode)))
      (setq fci-rule-color "darkgrey")
      (setq fill-column 80)
      (fci-mode 1))))
(global-set-key [f5] 'global-fci-mode)

;; toggle line numbers
(global-set-key [f6] 'global-linum-mode)


(provide 'ds-global-key-bindings)
