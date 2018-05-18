(when (executable-find "aspell")
  (setq-default ispell-program-name "aspell")
  (setq ispell-really-aspell t
	ispell-extra-args '("--sug-mode=fast")
	ispell-personal-dictionary "~/.emacs.d/personal-dict"
	flyspell-issue-message-flag nil
	flyspell-issue-welcome-flag nil
	ispell-parser 'tex))

;; add flyspell-prog-mode
(dolist (mode '(emacs-lisp-mode-hook
		inferior-lisp-mode-hook
		clojure-mode-hook
		python-mode-hook
		js-mode-hook
		R-mode-hook
		c++-mode-hook
		c-mode-hook
		java-mode-hook))
  (add-hook mode
	    '(lambda ()
	       (flyspell-prog-mode))))

;; mouse-3 (middle) will be used for correcting words
(eval-after-load "flyspell"
  '(progn
     (define-key flyspell-mouse-map [down-mouse-3] #'flyspell-correct-word)
     (define-key flyspell-mouse-map [mouse-3] #'undefined)))

(provide 'ds-flyspell)
