;; Font size
(global-set-key (kbd "C-+") 'text-scale-increase)
(global-set-key (kbd "C--") 'text-scale-decrease)

;; Start eshell or switch to it if it's active.
(global-set-key (kbd "C-x m") 'eshell)

;; kill all buffers
(defun ds/kill-all-buffers ()
  (interactive)
  (mapc 'kill-buffer (buffer-list)))
(global-set-key (kbd "C-x C-k") 'ds/kill-all-buffers)

;; kill current buffer without confirmation
(global-set-key (kbd "C-x k") 'kill-this-buffer)

;; eval-buffer
(global-set-key (kbd "C-c C-b") 'eval-buffer)

;; use hippie-expand instead of dabbrev
(global-set-key (kbd "M-/") 'hippie-expand)

;; newline behavior
(defun ds/newline-at-end-of-line ()
  "Move to end of line, enter a newline, and reindent."
  (interactive)
  (move-end-of-line 1)
  (newline-and-indent))
(global-set-key (kbd "<S-return>") 'ds/newline-at-end-of-line)
(global-set-key (kbd "RET") 'newline-and-indent)

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

;; fill column indicator
(require-package 'fill-column-indicator)
(define-globalized-minor-mode global-fci-mode fci-mode
  (lambda ()
    (when (and (not (string-match "^\*.*\*$" (buffer-name)))
               (not (eq major-mode 'dired-mode)))
      (setq fci-rule-color "darkgrey")
      (setq fill-column 80)
      (fci-mode 1))))
(global-set-key [f6] 'global-fci-mode)

;; 80 char mark and utility for whitespace
(require-package 'whitespace)
(global-set-key [f7] 'global-whitespace-mode)

;; toggle line numbers
(global-set-key [f8] 'global-linum-mode)

;; toggle save emacs sessions
(desktop-save-mode 1)
(global-set-key [f9] 'desktop-save-mode)

;;------------------------------------------------------------------------------
;; windows binding (see ds-windows.el)
;;------------------------------------------------------------------------------

;; navigate window layouts with "C-c n" and "C-c p"
(global-set-key (kbd "C-c n") 'tabbar-forward)
(global-set-key (kbd "C-c p") 'tabbar-backward)

;; make "C-x o" prompt for a target window when there are more than 2
(global-set-key (kbd "C-x o") 'switch-window)

;; override C-x 1
(global-set-key (kbd "C-x 1") 'ds/toggle-delete-other-windows)

;; when splitting window, show (other-buffer) in the new window
(global-set-key (kbd "C-x 2")
                (ds/split-window-with-other-buffer 'split-window-vertically))
(global-set-key (kbd "C-x 3")
                (ds/split-window-with-other-buffer 'split-window-horizontally))

;; horizontal splitting
(global-set-key (kbd "C-x |") 'ds/split-window-horizontally-instead)

;; vertical splitting
(global-set-key (kbd "C-x _") 'ds/split-window-vertically-instead)

;; Split the window to see the most recent buffer in the other window.
;; Call a second time to restore the original window configuration.
(global-set-key [f12] 'ds/split-window)

;; Toggle whether the current window is dedicated to its current buffer.
(global-set-key (kbd "C-c <down>") 'ds/toggle-current-window-dedication)

;;------------------------------------------------------------------------------
;; w3m binding (see ds-w3m.el)
;;------------------------------------------------------------------------------

;; (add-hook 'w3m-mode-hook 'scroll-lock-mode)
(global-set-key (kbd "C-c w") 'w3m)

(defun w3m-apply-custom-map ()
  (define-key w3m-mode-map (kbd "M-n") 'w3m-next-buffer)
  (define-key w3m-mode-map (kbd "M-p") 'w3m-previous-buffer)
  (define-key w3m-mode-map (kbd "}") 'w3m-goto-next-url)
  (define-key w3m-mode-map (kbd "{") 'w3m-goto-previous-url)
  (define-key w3m-mode-map (kbd "g") 'w3m-goto-url-no-initial)
  (define-key w3m-mode-map (kbd "G") 'w3m-goto-url-no-initial-new-session)
  (define-key w3m-mode-map (kbd "f") 'w3m-lnum-follow)
  (define-key w3m-mode-map (kbd "j") 'w3m-lnum-follow-new-session)
  (define-key w3m-mode-map (kbd "q") 'w3m-delete-buffer)
  (define-key w3m-mode-map (kbd "Q") 'w3m-close-window)
  (define-key w3m-mode-map (kbd "C-c C-w") 'w3m-quit))
(add-hook 'w3m-mode-hook 'w3m-apply-custom-map)

;;------------------------------------------------------------------------------
;; magit binding
;;------------------------------------------------------------------------------

(require-package 'magit)
(global-set-key (kbd "C-c g s") 'magit-status)
(global-set-key (kbd "C-c g l") 'magit-log)
(global-set-key (kbd "C-c g c") 'magit-commit)
(global-set-key (kbd "C-c g p") 'magit-push)
(global-set-key (kbd "C-c g u") 'magit-pull)
(global-set-key (kbd "C-c g d") 'magit-diff)
(global-set-key (kbd "C-c g o") 'magit-checkout)
(global-set-key (kbd "C-c g m") 'magit-merge)
(global-set-key (kbd "C-c g a") 'magit-remote-add)
(global-set-key (kbd "C-c g r") 'magit-remote-remove)
(global-set-key (kbd "C-c g n") 'magit-clone)

(provide 'ds-global-key-bindings)
