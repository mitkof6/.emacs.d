;;------------------------------------------------------------------------------
;; auctex
;;------------------------------------------------------------------------------

(require-package 'auctex)
;; (require 'auctex-autoloads)
(mapc (lambda (mode)
        (add-hook 'LaTeX-mode-hook mode))
      (list ;; 'auto-fill-mode
            'LaTeX-math-mode
            'turn-on-reftex
            'TeX-fold-mode
            'auto-complete-mode
            'autopair-mode
            'outline-minor-mode
            'visual-line-mode
            'flyspell-mode
            ))

(defun ds/latex-mode-hook ()
  "Latex custom preferences"
  (setq
   ;;TeX-auto-untabify t        ; remove all tabs before saving
   TeX-engine 'xetex            ; use xelatex default
   TeX-show-compilation nil     ; display compilation windows
   ;; LaTeX-indent-level 4		; indents with 4 tabs
   )
  (TeX-global-PDF-mode t)       ; PDF mode enable, not plain
  (setq TeX-save-query nil)
  (setq-default TeX-master nil)         ; set master file
  ;; (local-set-key ["TAB"] 'TeX-complete-symbol)
  )
(add-hook 'LaTeX-mode-hook 'ds/latex-mode-hook)

;; ensures that the pdf is reverted after compilation
;; this is the case when pdf-tools are used
(add-hook 'TeX-after-compilation-finished-functions
	  #'TeX-revert-document-buffer)

;; configuration for TeX-fold-mode
;; add entries you want to be fold, or comment that needn't to be fold.
(setq TeX-fold-env-spec-list
      (quote (("[figure]" ("figure"))
              ("[table]" ("table"))
              ("[itemize]" ("itemize"))
              ("[description]" ("description"))
              ("[tabular]" ("tabular"))
              ("[frame]" ("frame"))
              ("[array]" ("array"))
              ("[code]" ("lstlisting"))
              ;;              ("[eqnarray]" ("eqnarray"))
              )))

;; configuration for reftex
;; make the toc displayed on the left
(setq reftex-toc-split-windows-horizontally t)
;; adjust the fraction
(setq reftex-toc-split-windows-fraction 0.3)

;; configure Abbrev mode
;; (define-abbrev-table 'TeX-mode-abbrev-table (make-abbrev-table))
;; (add-hook 'TeX-mode-hook
;;           (lambda ()
;;             (setq abbrev-mode t)
;;             (setq local-abbrev-table TeX-mode-abbrev-table)))

;;------------------------------------------------------------------------------
;; preview-pane
;;------------------------------------------------------------------------------

(require-package 'latex-preview-pane)
(diminish 'latex-preview-pane-mode "Preview-Pane")
(defun ds/latex-preview-pane-hook ()
  "Sets latex-preview-pane variables."
  (latex-preview-pane-mode 1)
  (setq latex-preview-pane-multifile-mode 'auctex)
  )
(add-hook 'LaTeX-mode-hook 'ds/latex-preview-pane-hook)
;; add this hook so that when the file is updated the pane is updated
(add-hook 'latex-preview-pane-hook 'auto-revert-mode)


(provide 'ds-auctex)
