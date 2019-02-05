;; font size
(global-set-key (kbd "C-+") 'text-scale-increase)
(global-set-key (kbd "C--") 'text-scale-decrease)

;; start eshell or switch to it if it's active.
(global-set-key (kbd "C-c u e") 'eshell)

;; find and open file
(global-set-key (kbd "C-c b o") 'ffap)

;; expand-region
(require-package 'expand-region)
(global-set-key (kbd "C-c u r") 'er/expand-region)

;; company complete
;; (global-set-key (kbd "M-<return>") 'company-complete)

;; copy/cut whole line without selecting (better than whole-line-or-region-mode)
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

;; newline behavior
(defun ds/newline-at-end-of-line ()
  "Move to end of line, enter a newline, and re-indent."
  (interactive)
  (move-end-of-line 1)
  (newline-and-indent))
(global-set-key (kbd "<S-return>") 'ds/newline-at-end-of-line)
(global-set-key (kbd "RET") 'newline-and-indent)

;; change dictionary toggle
(lexical-let ((dictionaries '("en" "el")))
             (rplacd (last dictionaries) dictionaries)
             (defun ds/ispell-change-to-next-dictionary ()
               (interactive)
               (ispell-change-dictionary (pop dictionaries))))
(global-set-key [f2] 'ds/ispell-change-to-next-dictionary)

;; toggle flyspell mode
(global-set-key [f3] 'flyspell-mode)

;; ;; set ispell complete word (M-$ default)
;; (global-set-key (kbd "C-c u s") 'ispell-word)

;; ;; save word to dictionary
;; (defun ds/save-word-to-dict ()
;;   "Saves a word to a dictionary"
;;   (interactive)
;;   (let ((current-location (point))
;;          (word (flyspell-get-word)))
;;     (when (consp word)
;;       (flyspell-do-correct 'save nil (car word)
;;                         current-location (cadr word)
;;                         (caddr word) current-location))))
;; (global-set-key (kbd "C-c u d") 'ds/save-word-to-dict)
;; ;; (global-set-key (kbd "C-c u d") 'ispell-pdict-save)

;; this function is used to close the buffer (e.g. flyckeck error list)
(defun ds/quit-bottom-side-windows ()
  "Quit side windows of the current frame."
  (interactive)
  (dolist (window (window-at-side-list))
    (quit-window nil window)))
(global-set-key (kbd "C-c u q") 'ds/quit-bottom-side-windows)

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

;; 80 char mark and utility for whitespace
(require-package 'whitespace)
(global-set-key [f6] 'global-whitespace-mode)

;; toggle line numbers
(global-set-key [f9] 'global-linum-mode)

;; fill column indicator
(require-package 'fill-column-indicator)
(define-globalized-minor-mode global-fci-mode fci-mode
  (lambda ()
    (when (and (not (string-match "^\*.*\*$" (buffer-name)))
               (not (eq major-mode 'dired-mode)))
      (setq fci-rule-color "darkgrey")
      (setq fill-column 80)
      (fci-mode 1))))
(global-set-key [f10] 'global-fci-mode)

;; toggle save emacs sessions
;; (desktop-save-mode 1)
(global-set-key [f11] 'desktop-save-mode)

;; toggle next theme
(global-set-key [f12] 'ds/change-to-next-theme)

;;------------------------------------------------------------------------------
;; windows binding (see ds-windows.el)
;;------------------------------------------------------------------------------

;; navigate window layouts with "C-c n" and "C-c p"
(require-package 'tabbar)
(tabbar-mode 't)
(global-set-key (kbd "C-c n") 'tabbar-forward)
(global-set-key (kbd "C-c p") 'tabbar-backward)


;; transpose frames (mainly for function flop-frame)
(require-package 'transpose-frame)
(global-set-key (kbd "C-c b t") 'flop-frame)

;; frames can be recorded by 'window-configuration-to-register (C-x r w)
;; mouse point can be registered by C-x r C-space
;; frames that are registered can be recovered by (C-x r j 'char')

;; move windows to buffers
(global-set-key (kbd "C-c b u") 'windmove-up)
(global-set-key (kbd "C-c b d") 'windmove-down)
(global-set-key (kbd "C-c b l") 'windmove-left)
(global-set-key (kbd "C-c b r") 'windmove-right)

;; move windows
(require-package 'buffer-move)
(global-set-key (kbd "C-c b U") 'buf-move-up)
(global-set-key (kbd "C-c b D") 'buf-move-down)
(global-set-key (kbd "C-c b L") 'buf-move-left)
(global-set-key (kbd "C-c b R") 'buf-move-right)

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

;;------------------------------------------------------------------------------
;; hide-show
;;------------------------------------------------------------------------------

(global-set-key (kbd "C-c u H") 'hs-hide-all)
(global-set-key (kbd "C-c u h") 'hs-hide-block)
(global-set-key (kbd "C-c u S") 'hs-show-all)
(global-set-key (kbd "C-c u s") 'hs-show-block)
(global-set-key (kbd "C-c u t") 'hs-toggle-hiding)

(provide 'ds-global-key-bindings)