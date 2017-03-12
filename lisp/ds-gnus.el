(require 'gnus)

;; no primary server:
(setq gnus-select-method '(nnnil ""))

(setq user-full-name "Dimitar Stanev"
      user-mail-address "jimstanev@gmail.com")

;; where your mail will be saved locally default value
(setq nnml-directory "~/.emacs.d/gmail")
(setq message-directory "~/.emacs.d/gmail")

;; all Gmail groups will be ignored by the default value of
;; gnus-ignored-newsgroups, so let's change that default value
(setq gnus-ignored-newsgroups
      "^to\\.\\|^[0-9. ]+\\( \\|$\\)\\|^[\”]\”[#’()]")

;; primary source of incomming mails
(setq gnus-select-method
      '(nnimap "gmail"
               (nnimap-address "imap.gmail.com")
               (nnimap-server-port 993)
               (nnimap-stream ssl)))

;; configure gmail SMTP
(setq message-send-mail-function 'smtpmail-send-it
      smtpmail-starttls-credentials '(("smtp.gmail.com" 587 nil nil))
      smtpmail-auth-credentials '(("smtp.gmail.com" 587 "jimstanev@gmail.com"
                                   nil))
      smtpmail-default-smtp-server "smtp.gmail.com"
      smtpmail-smtp-server "smtp.gmail.com"
      smtpmail-smtp-service 587
      starttls-use-gnutls t)

(provide 'ds-gnus)
