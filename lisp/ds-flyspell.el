;;----------------------------------------------------------------------------
;; Add spell-checking in comments for all programming language modes
;;----------------------------------------------------------------------------

;; spell checking
;; (when (executable-find "hunspell")
;;   (setq-default ispell-program-name "hunspell")
;;   (setq ispell-really-hunspell t)
;;   (require 'init-flyspell))
(when (executable-find "aspell")
  (setq-default ispell-program-name "aspell")
  (setq ispell-really-aspell t)
  (setq ispell-really-aspell t)
  (setq ispell-extra-args '("--sug-mode=fast"))
  (setq ispell-extra-args '("--sug-mode=ultra"))
  (setq ispell-really-aspell t)
  (setq ispell-personal-dictionary "~/.emacs.d/personal-dict")
  (setq flyspell-issue-message-flag nil)
  (setq flyspell-issue-welcome-flag nil)
  (setq ispell-parser 'tex)
;;  (flyspell-mode 0)
  )

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
                    javascript-mode-hook
                    ))
      (add-hook hook 'flyspell-prog-mode)))

(after-load 'flyspell
            (add-to-list 'flyspell-prog-text-faces 'nxml-text-face))


;; ;; org-mode
;; (add-to-list 'ispell-skip-region-alist
;;              '(":\\(PROPERTIES\\|LOGBOOK\\):" . ":END:"))
;; (add-to-list 'ispell-skip-region-alist
;;              '("#\\+BEGIN_SRC" . "#\\+END_SRC"))

(provide 'ds-flyspell)
