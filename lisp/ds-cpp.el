;;------------------------------------------------------------------------------
;; set up code completion with company and irony + rtags for finding symbols
;;------------------------------------------------------------------------------

(require-package 'company)
(require-package 'rtags)
(require-package 'irony)
(require-package 'company-irony)
(require-package 'company-irony-c-headers)
(require-package 'company-rtags)
(require 'rtags)
(require 'cl)

(global-company-mode)

;; setup rtags
(setq rtags-completions-enabled t
      rtags-autostart-diagnostics t
      rtags-display-result-backend 'ivy)
(rtags-diagnostics)
(rtags-enable-standard-keybindings)


;; irony-mode hook that is called when irony is triggered
(add-hook 'c++-mode-hook 'irony-mode)
(add-hook 'c-mode-hook 'irony-mode)
(add-hook 'objc-mode-hook 'irony-mode)

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
;; (setq company-backends (delete 'company-semantic company-backends))
;; Enable company-irony and several other useful auto-completion modes
;; We don't use rtags since we've found that for large projects this can cause
;; async timeouts. company-semantic (after company-clang!) works quite well
;; but some knowledge some knowledge of when best to trigger is still necessary.
(eval-after-load 'company
                 '(add-to-list
                   'company-backends '(;; company-irony-c-headers
                                       company-irony
                                       company-yasnippet
                                       ;; company-clang
                                       company-rtags
                                       )))


(setq company-idle-delay nil
      company-minimum-prefix-length 2
      company-show-numbers t
      company-tooltip-limit 20
      company-dabbrev-downcase nil

)

;; Windows performance tweaks
(when (boundp 'w32-pipe-read-delay)
  (setq w32-pipe-read-delay 0))
;; Set the buffer size to 64K on Windows (from the original 4K)
(when (boundp 'w32-pipe-buffer-size)
  (setq irony-server-w32-pipe-buffer-size (* 64 1024)))

;;------------------------------------------------------------------------------
;; company statistics (sorting of completion candidates by frequency)
;;------------------------------------------------------------------------------

(require-package 'company-statistics)
(company-statistics-mode)

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
  "Indent or Complete."
  (interactive)
  (cond ((and (not (use-region-p))
              (ds/irony-check-expansion))
         ;; (message "complete")
         ;; (company-irony 'interactive)	; better completion
         (company-complete-common)
         )
        (t
         ;; (message "indent")
         (call-interactively 'c-indent-line-or-region))))

(defun ds/irony-mode-keys ()
  "Modify keymaps used by `irony-mode'."
  (local-set-key (kbd "TAB") 'ds/irony-indent-or-complete))
(add-hook 'c-mode-common-hook 'ds/irony-mode-keys)

;;------------------------------------------------------------------------------
;; flycheck-irony
;;------------------------------------------------------------------------------

;; for some reason flycheck-irony have problems with .dir-locals.el and
;; ignores them

(require-package 'flycheck-irony)
(eval-after-load 'flycheck
                 '(add-hook 'flycheck-mode-hook #'flycheck-irony-setup))


;; setup flycheck-rtags
(require-package 'flycheck-rtags)
(defun ds/flycheck-rtags-setup ()
  (flycheck-select-checker 'rtags)
  ;; RTags creates more accurate overlays.
  (setq-local flycheck-highlighting-mode nil)
  (setq-local flycheck-check-syntax-automatically nil))
(add-hook 'c-mode-hook 'ds/flycheck-rtags-setup)
(add-hook 'c++-mode-hook 'ds/flycheck-rtags-setup)
(add-hook 'objc-mode-hook 'ds/flycheck-rtags-setup)

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

;;------------------------------------------------------------------------------
;; setup rtags-helm
;;------------------------------------------------------------------------------

;; (require-package 'rtags-helm)
;; (setq rtags-use-helm t)

;;------------------------------------------------------------------------------
;; setup cmake-ide
;;------------------------------------------------------------------------------

(require-package 'cmake-ide)
(cmake-ide-setup)

;; Set cmake-ide-flags-c++ to use C++11
(setq cmake-ide-flags-c++ (append '("-std=c++11")))

;; We want to be able to compile with a keyboard shortcut
(global-set-key (kbd "C-c m") 'cmake-ide-compile)

;; make sure cmake-mode is installed for viewing CMake files
(require-package 'cmake-mode)

;;---------------------------------------------------------------------
;; compiling
;;---------------------------------------------------------------------

(require-package 'compile)

;; Change compilation command:
(setq compile-command "make ")

;; if there are no errors make the compilation window vanish
(setq compilation-finish-function
      (lambda (buf str)
        (if (null (string-match ".*exited abnormally.*" str))
            ;;no errors, make the compilation window go away in a few seconds
            (progn
              (run-at-time
               "2 sec" nil 'delete-windows-on
               (get-buffer-create "*compilation*"))
              (message "No Compilation Errors!")))))

(defun ds/compile ()
  "Run compile and resize the compile window"
  (interactive)
  (progn
    (call-interactively 'compile)
    (setq w (get-buffer-window "*compilation*"))
    (setq cur (selected-window))
    (select-window w)
    (setq h (window-height w))
    (shrink-window (- h 15))
    (select-window cur)))
;; (global-set-key [f9] 'ds/compile)
(define-key c++-mode-map (kbd "C-c C-c") #'ds/compile)

;; useful but when you jump to error it splits vertically,
;; thus I disable this feature
;; when the compile window shows always split vertically
;; (defadvice compile (around split-horizontally activate)
;;   (let ((split-width-threshold nil)
;;         (split-height-threshold 0))
;;     ad-do-it))

(setq compilation-ask-about-save nil  ; Just save before compiling
      compilation-always-kill t       ; Just kill old compile processes before
      compilation-scroll-output 'first-error ; Automatically scroll to first
      )


(provide 'ds-cpp)
