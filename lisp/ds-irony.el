;;------------------------------------------------------------------------------
;; Semantics
;;------------------------------------------------------------------------------
;; enable semantics mode for auto-completion
(require-package 'cc-mode)
(require 'semantic) ;; no need for require-package
(global-semanticdb-minor-mode 1)
(global-semantic-idle-scheduler-mode 1)
(global-semantic-idle-summary-mode 1) ;; for c code only
(semantic-mode 1)

(require-package 'stickyfunc-enhance)
(add-to-list 'semantic-default-submodes 'global-semantic-stickyfunc-mode)

;; Prohibit semantic from searching through system headers. We want
;; company-clang to do that for us.
(setq-mode-local c-mode semanticdb-find-default-throttle
                 '(local project unloaded recursive))
(setq-mode-local c++-mode semanticdb-find-default-throttle
                 '(local project unloaded recursive))

(semantic-remove-system-include "/usr/include/" 'c++-mode)
(semantic-remove-system-include "/usr/local/include/" 'c++-mode)
(add-hook 'semantic-init-hooks
          'semantic-reset-system-include)

;;------------------------------------------------------------------------------
;; set up code completion with company and irony
;;------------------------------------------------------------------------------
(require-package 'company)
;; (require-package 'company-rtags) customly installed in ds-rtags
(require-package 'rtags)
(require 'company-rtags) ;; no need for require-package
(global-company-mode)

;; setup irony-mode to load in c-modes
(require-package 'irony)
(require-package 'company-irony)
(require-package 'company-irony-c-headers)
(require-package 'cl)
(add-hook 'c++-mode-hook 'irony-mode)
(add-hook 'c-mode-hook 'irony-mode)
;; (add-hook 'objc-mode-hook 'irony-mode)

;; irony-mode hook that is called when irony is triggered
(defun ds/irony-mode-hook ()
  "Custom irony mode hook to remap keys."
  (define-key irony-mode-map [remap completion-at-point]
    'irony-completion-at-point-async)
  (define-key irony-mode-map [remap complete-symbol]
    'irony-completion-at-point-async))

(add-hook 'irony-mode-hook 'ds/irony-mode-hook)
(add-hook 'irony-mode-hook 'irony-cdb-autosetup-compile-options)

;; company-irony setup, c-header completions
(add-hook 'irony-mode-hook 'company-irony-setup-begin-commands)
;; Remove company-semantic because it has higher precedance than company-clang
;; Using RTags completion is also faster than semantic, it seems. Semantic
;; also provides a bunch of technically irrelevant completions sometimes.
;; All in all, RTags just seems to do a better job.
(setq company-backends (delete 'company-semantic company-backends))
;; Enable company-irony and several other useful auto-completion modes
;; We don't use rtags since we've found that for large projects this can cause
;; async timeouts. company-semantic (after company-clang!) works quite well
;; but some knowledge some knowledge of when best to trigger is still necessary.
(eval-after-load 'company
                 '(add-to-list
                   'company-backends '(company-irony-c-headers
                                       company-irony company-yasnippet
                                       company-clang company-rtags)))

(defun ds/disable-semantic ()
  "Disable the company-semantic backend."
  (interactive)
  (setq company-backends (delete '(company-irony-c-headers
                                   company-irony company-yasnippet
                                   company-clang company-rtags
                                   company-semantic)
                                 company-backends))
  (add-to-list
   'company-backends '(company-irony-c-headers
                       company-irony company-yasnippet
                       company-clang company-rtags)))

(defun ds/enable-semantic ()
  "Enable the company-semantic backend."
  (interactive)
  (setq company-backends (delete '(company-irony-c-headers
                                   company-irony company-yasnippet
                                   company-clang)
                                 company-backends))
  (add-to-list
   'company-backends '(company-irony-c-headers
                       company-irony company-yasnippet company-clang)))

;; Zero delay when pressing tab
(setq company-idle-delay 0.5)

;;------------------------------------------------------------------------------
;; bind TAB for indent-or-complete
;;------------------------------------------------------------------------------
(defun ds/irony-check-expansion ()
  (save-excursion
   (if (looking-at "\\_>") t
       (backward-char 1)
       (if (looking-at "\\.") t
           (backward-char 1)
           (if (looking-at "->") t nil)))))

(defun ds/irony-indent-or-complete ()
  "Indent or Complete"
  (interactive)
  (cond ((and (not (use-region-p))
              (ds/irony-check-expansion))
         (message "complete")
         (company-complete-common))
        (t
         (message "indent")
         (call-interactively 'c-indent-line-or-region))))

(defun ds/irony-mode-keys ()
  "Modify keymaps used by `irony-mode'."
  (local-set-key (kbd "TAB") 'ds/irony-indent-or-complete)
  (local-set-key [tab] 'ds/irony-indent-or-complete))

(add-hook 'c-mode-common-hook 'ds/irony-mode-keys)

;;------------------------------------------------------------------------------
;; eldoc-mode
;;------------------------------------------------------------------------------
(require-package 'irony-eldoc)
(add-hook 'irony-mode-hook 'irony-eldoc)

;;------------------------------------------------------------------------------
;; function-args
;;------------------------------------------------------------------------------
(require-package 'function-args)
(fa-config-default)

(provide 'ds-irony)
