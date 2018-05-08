(setq org-confirm-babel-evaluate nil
      org-src-window-setup 'current-window
      org-src-preserve-indentation t
      org-latex-pdf-process
      '("pdflatex -interaction nonstopmode -output-directory %o %f"
        "bibtex %b"
        "pdflatex -interaction nonstopmode -output-directory %o %f"
        "pdflatex -interaction nonstopmode -output-directory %o %f"))

(require 'ob-python)
(org-babel-do-load-languages
  'org-babel-load-languages
  '((python . t)))

(provide 'ds-org)
