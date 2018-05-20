(when (executable-find "aspell")
  (setq-default ispell-program-name "aspell")
  (setq ispell-really-aspell t
	ispell-extra-args '("--sug-mode=fast")
	ispell-personal-dictionary "~/.emacs.d/personal_dictionary"
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

;; enable flyspell-mode
(dolist (hook '(text-mode-hook))
  (add-hook hook (lambda () (flyspell-mode 1))))

;; save word to dictionary
(defun ds/save-to-dictionary ()
  (interactive)
  (let ((current-location (point))
         (word (flyspell-get-word)))
    (when (consp word)    
      (flyspell-do-correct 'save nil (car word)
      current-location (cadr word) (caddr word)
      current-location))))

(provide 'ds-flyspell)
