;; should be loaded after yasnippet
(require-package 'fuzzy)
(require-package 'popup)
(require-package 'auto-complete)
(require 'auto-complete-config)

(global-auto-complete-mode t)
(add-to-list 'ac-dictionary-directories
             (expand-file-name "lisp/custom-dicts" user-emacs-directory))

;; if TAB is pressed, first indent current line, then try to complete
(setq tab-always-indent 'complete)

;; prevent auto-complete from automatically expanding
(setq-default ac-expand-on-auto-complete nil)
(setq-default ac-auto-start nil)
;; use TAB to explicitily trigger the auto-complete func
(ac-set-trigger-key "TAB")
;; (setq-default ac-dwim nil)


;; use pos-tip instead of popup
(require-package 'pos-tip)
(setq ac-quick-help-prefer-pos-tip t)
;; use quick-help to show the documents
(setq ac-use-quick-help t)
(setq ac-quick-help-delay 0.5)

;; use fuzzy matching. needs manually triggering.
(setq ac-fuzzy-enable t)

(setq ac-trigger-commands
      (cons 'backward-delete-char-untabify ac-trigger-commands))
(after-load 'init-yasnippet
            (set-default 'ac-sources
                         '(ac-source-dictionary
                           ac-source-words-in-buffer
                           ac-source-words-in-same-mode-buffers
                           ac-source-words-in-all-buffer
                           ac-source-functions
                           ac-source-yasnippet)))

;;------------------------------------------------------------------------------
;; add custom sources
;;------------------------------------------------------------------------------

;; add ac-sources for latex mode
(require-package 'ac-math)
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
(add-to-list 'ac-modes 'org-mode)

;; add ac-source for clang
(require-package 'auto-complete-clang)
(setq ac-clang-flags
      (append '("-std=c++11")
              (mapcar (lambda (item) (concat "-I" item))
                      (split-string
                       "
/usr/include/c++/5
/usr/include/x86_64-linux-gnu/c++/5
/usr/include/c++/5/backward
/usr/lib/gcc/x86_64-linux-gnu/5/include
/usr/local/include
/usr/lib/gcc/x86_64-linux-gnu/5/include-fixed
/usr/include/x86_64-linux-gnu
/usr/include
"))))

(defun ds/ac-cc-mode-setup ()
  (setq ac-sources (append '(ac-source-clang) ac-sources)))
(add-hook 'c-mode-common-hook 'ds/ac-cc-mode-setup)

(provide 'ds-auto-complete)
