(use-package auctex
  :mode "\\.tex$"
  :ensure t
  ;;:defer t
  :config
  (require 'tex)

  (mapc (lambda (mode)
          (add-hook 'LaTeX-mode-hook mode))
        (list 'auto-fill-mode
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
    (TeX-global-PDF-mode t)               ; PDF mode enable, not plain
    (TeX-fold-mode 1)                     ; TeX fold mode
    (reftex-mode 1)
    (TeX-fold-mode 1)
    (flycheck-mode 0)
    (flyspell-mode 1)
    ;;(local-set-key ["TAB"] 'TeX-complete-symbol)
    ;; (setq-default TeX-master nil)         ; set master file
    (setq
     ;;TeX-auto-untabify t                ; remove all tabs before saving
     ;;TeX-engine 'xetex                  ; use xelatex default
     ;; LaTeX-indent-level 4              ; indents with 4 tabs
     LaTeX-command "pdflatex"
     TeX-show-compilation nil             ; display compilation windows
     TeX-save-query nil
     TeX-auto-save t
     TeX-parse-self t
     reftex-plug-into-AUCTeX
     TeX-PDF-mode t
     ))
  (add-hook 'LaTeX-mode-hook 'ds/latex-mode-hook)

  ;; automatically use fold buffer C-c C-o C-b
  ;; (add-hook 'find-file-hook 'TeX-fold-buffer t)

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
  (setq reftex-toc-split-windows-horizontally t
        ;; adjust the fraction
        reftex-toc-split-windows-fraction 0.3)
  )

;;------------------------------------------------------------------------------
;; preview-pane
;;------------------------------------------------------------------------------

;; (require-package 'latex-preview-pane)
;; (diminish 'latex-preview-pane-mode "Preview-Pane")
;; (defun ds/latex-preview-pane-hook ()
;;   "Sets latex-preview-pane variables."
;;   (latex-preview-pane-mode 1)
;;   (setq latex-preview-pane-multifile-mode 'auctex)
;;   )
;; (add-hook 'LaTeX-mode-hook 'ds/latex-preview-pane-hook)
;; ;; add this hook so that when the file is updated the pane is updated
;; (add-hook 'latex-preview-pane-hook 'auto-revert-mode)


(provide 'ds-auctex)
