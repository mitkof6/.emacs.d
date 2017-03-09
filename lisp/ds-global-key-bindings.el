;; Font size
(global-set-key (kbd "C-+") 'text-scale-increase)
(global-set-key (kbd "C--") 'text-scale-decrease)

;; Start eshell or switch to it if it's active.
(global-set-key (kbd "C-x m") 'eshell)

;; kill all buffers
(global-set-key (kbd "C-c C-k") 'ds/close-all-buffers)

;; eval-buffer
(global-set-key (kbd "C-c C-b") 'eval-buffer)

;; use hippie-expand instead of dabbrev
(global-set-key (kbd "M-/") 'hippie-expand)

;; toggle flycheck mode
(global-set-key (kbd "C-c f") 'flycheck-mode)

;; set ispell complete word
(global-set-key (kbd "C-c s") 'ispell-word)

;; change dictionary toggle
(lexical-let ((dictionaries '("en_US" "greek")))
             (rplacd (last dictionaries) dictionaries)
             (defun ispell-change-to-next-dictionary ()
               (interactive)
               (ispell-change-dictionary (pop dictionaries))))
(global-set-key [f2] 'ispell-change-to-next-dictionary)


(provide 'ds-global-key-bindings)
