;; no primary server:
(setq gnus-select-method '(nnnil ""))

;; get email, and store in nnml:
(setq gnus-secondary-select-methods '((nnml "")))

(setq user-full-name "Dimitar Stanev"
      user-mail-address "jimstanev@gmail.com")

;; get local email, and store it in nnml; connect via IMAP to imap.mcom.com,
;; and also via IMAP to imap.gmail.com:
(setq gnus-secondary-select-methods '((nnml "")
                                         (nnimap "imap.gmail.com")))

(provide 'ds-gnus)
