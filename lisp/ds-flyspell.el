;;----------------------------------------------------------------------------
;; Add spell-checking in comments for all programming language modes
;;----------------------------------------------------------------------------

(if (fboundp 'prog-mode)
    (add-hook 'prog-mode-hook 'flyspell-prog-mode)
  (dolist (hook '(lisp-mode-hook
                  emacs-lisp-mode-hook
                  scheme-mode-hook
                  clojure-mode-hook
                  ruby-mode-hook
                  yaml-mode
                  python-mode-hook
                  shell-mode-hook
                  php-mode-hook
                  css-mode-hook
                  haskell-mode-hook
                  caml-mode-hook
                  nxml-mode-hook
                  crontab-mode-hook
                  perl-mode-hook
                  tcl-mode-hook
                  javascript-mode-hook))
    (add-hook hook 'flyspell-prog-mode)))

(after-load 'flyspell
  (add-to-list 'flyspell-prog-text-faces 'nxml-text-face))

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

(provide 'ds-flyspell)
