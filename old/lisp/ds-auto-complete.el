;;------------------------------------------------------------------------------
;; setup auto-complete
;;------------------------------------------------------------------------------
;; should be loaded after yasnippet

(use-package auto-complete
             :ensure t
             ;;:defer t
	     :after yasnippet
             :config
             (require 'auto-complete-config)
             (global-auto-complete-mode t)
             (add-to-list 'ac-dictionary-directories
                          (expand-file-name "lisp/custom-dicts" user-emacs-directory))
             ;; use TAB to explicitily trigger the auto-complete func
             (ac-set-trigger-key "TAB")
             ;; (setq-default ac-dwim nil)
             ;; if TAB is pressed, first indent current line, then try to complete
             (setq tab-always-indent 'complete
                   ;; use quick-help to show the documents
                   ac-use-quick-help t
                   ac-quick-help-delay 0.5)
             (setq ac-trigger-commands
                   (cons 'backward-delete-char-untabify ac-trigger-commands))
             ;; prevent auto-complete from automatically expanding
             (setq-default ac-expand-on-auto-complete nil
                           ac-auto-start nil)
             (after-load 'init-yasnippet
                         (set-default 'ac-sources
                                      '(ac-source-dictionary
                                        ac-source-words-in-buffer
                                        ac-source-words-in-same-mode-buffers
                                        ac-source-words-in-all-buffer
                                        ac-source-functions
                                        ac-source-yasnippet))))

(use-package popup
	     :ensure t
	     ;;:defer t
	     :after auto-complete)

(use-package fuzzy
             :ensure t
             ;;:defer t
             :after auto-complete
             :config
             ;; use fuzzy matching. needs manually triggering.
             (setq ac-fuzzy-enable t))

;; use pos-tip instead of popup
(use-package pos-tip
             :ensure t
             ;;:defer t
             :after auto-complete
             :config
             (setq ac-quick-help-prefer-pos-tip t))


;;------------------------------------------------------------------------------
;; add custom sources
;;------------------------------------------------------------------------------

;; add ac-sources for latex mode
(use-package ac-math
             :ensure t
             ;;:defer t
             :after auto-complete
             :config
             (add-to-list 'ac-modes 'latex-mode)
             (defun ac-latex-mode-setup ()
               (setq ac-sources
                     (append '(ac-source-math-unicode
                               ac-source-math-latex
                               ac-source-latex-commands)
                             ac-sources)))
             (add-hook 'latex-mode-hook 'ac-latex-mode-setup)
             (add-hook 'LaTeX-mode-hook 'ac-latex-mode-setup)
             ;; org mode
             (add-to-list 'ac-modes 'org-mode))

(provide 'ds-auto-complete)
