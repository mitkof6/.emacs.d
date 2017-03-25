;;------------------------------------------------------------------------------
;; jdee
;;------------------------------------------------------------------------------
;; (require-package 'jdee)

;;------------------------------------------------------------------------------
;; eclim
;;------------------------------------------------------------------------------

;; (require-package 'eclim)
;; (global-eclim-mode)

;; ;; control eclimd from emacs
;; (require 'eclimd)
;; (setq eclimd-executable "/usr/lib/eclipse/eclimd")
;; (setq eclimd-default-workspace "~/dev/eclipse")
;; (setq eclimd-wait-for-process nil)

;; ;; displaying compilation error messages in the echo area
;; (setq help-at-pt-display-when-idle t)
;; (setq help-at-pt-timer-delay 0.1)
;; (help-at-pt-set-timer)

;; ;; configuring auto-complete-mode
;; ;; regular auto-complete initialization
;; (require 'auto-complete-config)
;; (ac-config-default)

;; ;; add the emacs-eclim source
;; (require-package 'ac-emacs-eclim)
;; ;; (require 'ac-emacs-eclim-source)
;; (ac-emacs-eclim-config)

;; ;; configuring company-mode
;; (require-package 'company)
;; (require-package 'company-emacs-eclim)
;; (company-emacs-eclim-setup)
;; (global-company-mode t)

;;------------------------------------------------------------------------------
;; unused
;;------------------------------------------------------------------------------

;; ;; use gradle for build
;; (require-package 'gradle-mode)
;; (add-hook 'java-mode-hook '(lambda() (gradle-mode 1)))

;; (defun ds/java-build-and-run ()
;;	(interactive)
;;	(gradle-run "build run"))


;; ;; Add development sources of emacs-eclim to the load path
;; ;; (add-to-list 'load-path "~/src/emacs-eclim")

;; (defun ds/insert-serial-version-uuid()
;;   (interactive)
;;   (insert "private static final long serialVersionUID = 1L;"))

;; (defun ds/eclim-maven-run-quick-package()
;;   (interactive)
;;   (eclim-maven-run "-o -q -DskipTests package"))

;; (defun ds/java-mode-hook ()
;;   (auto-fill-mode)
;;   (gtags-mode)
;;   ;;  (flyspell-prog-mode) ;; grabs C-; :(
;;   ;;  (flymake-mode)
;;   (eclim-mode)
;;   (subword-mode)
;;   (yas-minor-mode)
;;   (idle-highlight-mode)
;;   (git-gutter+-mode)

;;   ;; Only include eclim candidates in the completion menu, must be run
;;   ;; after eclim-mode above.
;;   (setq ac-sources '(ac-source-emacs-eclim))

;;   (define-key c-mode-base-map (kbd "<f2>") 'eclim-problems)
;;   (define-key c-mode-base-map (kbd "M-m") 'eclim-java-find-declaration)
;;   (define-key c-mode-base-map (kbd "C-M-j") 'ds/insert-serial-version-uuid)
;;   (define-key c-mode-base-map (kbd "C-M-o") 'eclim-java-import-organize)
;;   (define-key c-mode-base-map (kbd "C-S-e") 'eclim-problems-show-errors)
;;   (define-key c-mode-base-map (kbd "C-S-w") 'eclim-problems-show-warnings)
;;   (define-key c-mode-base-map (kbd "C-<f9>") 'ds/eclim-maven-run-quick-package)
;;   (define-key c-mode-base-map (kbd "C-m") 'c-context-line-break)
;;   (define-key c-mode-base-map (kbd "C-q") 'eclim-java-show-documentation-for-current-element)
;;   (define-key c-mode-base-map (kbd "M-RET") 'eclim-problems-correct)
;;   (define-key c-mode-base-map (kbd "M-<f7>") 'eclim-java-find-references)
;;   (define-key c-mode-base-map (kbd "M-i") 'eclim-java-implement) ;; IDEA is C-i
;;   (define-key c-mode-base-map (kbd "S-<f6>") 'eclim-java-refactor-rename-symbol-at-point)
;;   (define-key c-mode-base-map (kbd "S-<f7>") 'gtags-find-tag-from-here)

;;   ;; Fix indentation for anonymous classes
;;   (c-set-offset 'substatement-open 0)
;;   (if (assoc 'inexpr-class c-offsets-alist)
;;       (c-set-offset 'inexpr-class 0))

;;   ;; Indent arguments on the next line as indented body.
;;   (c-set-offset 'arglist-intro '+))
;; (add-hook 'java-mode-hook 'ds/java-mode-hook)

;; (defun ds/java-before-save-hook()
;;   (when (eq major-mode 'java-mode)
;;     (eclim-problems-show-errors)))
;; (add-hook 'before-save-hook 'ds/java-before-save-hook)

;; (defun ds/default-code-style-hook()
;;   (setq c-basic-offset 2
;;	c-label-offset 0
;;	indent-tabs-mode nil
;;	compile-command "cd ~/src/drifting/jms && mvn -q -o -DskipTests package"
;;	require-final-newline nil))
;; (add-hook 'java-mode-hook 'ds/default-code-style-hook)

;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; ;; Flymake settings
;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; (require 'flymake)
;; (setq flymake-log-level -1) ;; 3 is debug

;; ;; On the fly checkstyle & pmd checking
;; (defun my-flymake-init ()
;;   (list "my-java-flymake-checks"
;;	(list (flymake-init-create-temp-buffer-copy
;;	       'flymake-create-temp-with-folder-structure))))
;; ;; (add-to-list 'flymake-allowed-file-name-masks
;; ;;             '("\\.java$" my-flymake-init flymake-simple-cleanup))

;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; ;; Highlighting gud
;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; (defun ds/java-debug-highlighting()
;;   (interactive)
;;   (insert "monitor locals\n")
;;   (highlight-regexp "^[a-zA-Z].* =" "hi-green")
;;   (highlight-phrase "com.escenic" "hi-red-b")
;;   (highlight-phrase "neo." "hi-red-b")
;;   )
;; (add-hook 'gud-mode-hook 'ds/java-debug-highlighting)

;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; ;; Interface to eclipse via eclim
;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; (require-package 'eclim)
;; ;; (global-eclim-mode)

;; ;; Variables
;; (setq eclim-auto-save t
;;       eclim-executable "/usr/lib/eclipse/eclim"
;;       eclimd-wait-for-process nil
;;       eclimd-default-workspace "~/dev/workspace"
;;       eclim-use-yasnippet nil
;;       eclim-autoupdate-problems nil
;;       eclim-print-debug-messages nil
;;       help-at-pt-display-when-idle t
;;       help-at-pt-timer-delay 1.0
;;       )

;; ;; Call the help framework with the settings above & activate
;; ;; eclim-mode
;; (help-at-pt-set-timer)

;; ;; Use company mode for auto completion
;; (require-package 'company)
;; (require 'company-emacs-eclim)
;; (company-emacs-eclim-setup)
;; (setq company-emacs-eclim-ignore-case t)

;; ;; restore the window configuration after running certain eclim commands
;; ;; (add-hook 'eclim-problems-mode-hook 'winner-undo)

;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; ;; 2015-08-18 eclim fails, added this from
;; ;; https://gist.github.com/mads379/3402786
;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; (defun toggle-maximize-buffer ()
;;   (interactive)
;;   (if (= 1 (length (window-list)))
;;       (jump-to-register '_)
;;       (progn
;;	(window-configuration-to-register '_)
;;	(delete-other-windows))))

;; ;; TODO: get this into emacs-eclim
;; (add-to-list 'eclim--file-coding-system-mapping '("iso-latin-1-unix" . "iso-8859-1"))


(provide 'ds-java)
