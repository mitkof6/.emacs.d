(use-package folding
  :ensure t
  :config
  (folding-add-to-marks-list 'tex-mode "%{{{" "%}}}" nil t)
  (folding-mode-add-find-file-hook)
  )

(provide 'ds-folding)
