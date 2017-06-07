(require-package 'auctex)
(require-package 'cdlatex)
(require-package 'smartparens)
(require 'cl)
(require 'tex)

(eval-after-load "company"
  '(progn
     (require-package 'company-auctex)
     (company-auctex-init)))

(defcustom ds/latex-fast-math-entry 'LaTeX-math-mode
  "Method used for fast math symbol entry in LaTeX."
  :link '(function-link :tag "AUCTeX Math Mode" LaTeX-math-mode)
  :link '(emacs-commentary-link :tag "CDLaTeX" "cdlatex.el")
  :group 'ds
  :type '(choice (const :tag "None" nil)
                 (const :tag "AUCTeX Math Mode" LaTeX-math-mode)
                 (const :tag "CDLaTeX" cdlatex)))

;; AUCTeX configuration
(setq TeX-close-quote ""
      TeX-open-quote ""
      ;; TeX-auto-untabify t                ; remove all tabs before saving
      ;; TeX-engine 'xetex                  ; use xelatex default
      ;; LaTeX-indent-level 4              ; indents with 4 tabs
      ;; LaTeX-command "pdflatex"
      TeX-show-compilation nil             ; display compilation windows
      TeX-save-query nil
      TeX-auto-save t
      TeX-parse-self t
      ;; use pdflatex
      TeX-PDF-mode t
      )

;; (setq-default TeX-master nil)

;; sensible defaults for OS X, other OSes should be covered out-of-the-box
(when (eq system-type 'darwin)
  (setq TeX-view-program-selection
        '((output-dvi "DVI Viewer")
          (output-pdf "PDF Viewer")
          (output-html "HTML Viewer")))

  (setq TeX-view-program-list
        '(("DVI Viewer" "open %o")
          ("PDF Viewer" "open %o")
          ("HTML Viewer" "open %o"))))

(defun ds/latex-mode-defaults ()
  "Default hook for `LaTeX-mode'."
  (turn-on-auto-fill)
  (abbrev-mode +1)
  (smartparens-mode +1)
  (auto-fill-mode +1)
  (turn-on-reftex)
  (TeX-fold-mode +1)
  (auto-complete-mode +1)
  (autopair-mode t)
  (outline-minor-mode +1)
  (visual-line-mode +1)
  (flyspell-mode +1)
  (folding-mode +1)
  (case ds/latex-fast-math-entry
    (LaTeX-math-mode (LaTeX-math-mode 1))
    (cdlatex (turn-on-cdlatex))))

(setq ds/latex-mode-hook 'ds/latex-mode-defaults)
(add-hook 'LaTeX-mode-hook (lambda ()
                             (run-hooks 'ds/latex-mode-hook)))


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

(provide 'ds-auctex)
