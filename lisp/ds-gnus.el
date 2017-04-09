;;------------------------------------------------------------------------------
;; gnus general setup
;;------------------------------------------------------------------------------

(require 'nnir)

;; use as mail only
(setq gnus-select-method '(nnml ""))

;; ask encryption password once
(setq epa-file-cache-passphrase-for-symmetric-encryption t)

(setq gnus-thread-sort-functions
      '(gnus-thread-sort-by-most-recent-date
        (not gnus-thread-sort-by-number)))

;; use cache and store into concrete folders
(setq gnus-use-cache t)
(setq gnus-directory "~/.emacs.d/gnus/")
(setq nnfolder-directory "~/.emacs.d/gnus/mail/")
(setq gnus-article-save-directory "~/.emacs.d/gnus/article/")
(setq gnus-cache-directory "~/.emacs.d/gnus/cache/")
(setq gnus-kill-files-directory "~/.emacs.d/gnus/kill-files/")
(setq gnus-default-directory "~/.emacs.d/gnus/")
(setq nnml-directory "~/.emacs.d/gnus/nnml/")
(setq smtpmail-queue-dir "~/.emacs.d/gnus/mail/")

;; BBDB: Address list
(require-package 'bbdb)
(bbdb-initialize 'message 'gnus 'sendmail)
(add-hook 'gnus-startup-hook 'bbdb-insinuate-gnus)
(setq bbdb/mail-auto-create-p t
      bbdb/news-auto-create-p t)

;; auto-complete emacs address using bbdb UI
(add-hook 'message-mode-hook
          '(lambda ()
            (flyspell-mode t)
            (local-set-key "<TAB>" 'bbdb-complete-name)))

;; fetch only part of the article if we can
(setq gnus-read-active-file 'some)

;; open attachment
(eval-after-load
 'mailcap
 '(progn
   (cond
     ;; on OSX, maybe change mailcap-mime-data?
     ((eq system-type 'darwin))
     ;; on Windows, maybe change mailcap-mime-data?
     ((eq system-type 'windows-nt))
     (t
      ;; Linux, read ~/.mailcap
      (mailcap-parse-mailcaps)))))

;; enable gnus topics
(add-hook 'gnus-group-mode-hook 'gnus-topic-mode)

;; threads!  I hate reading un-threaded email -- especially mailing
;; lists.  This helps a ton!
(setq gnus-summary-thread-gathering-function 'gnus-gather-threads-by-subject)

;; I prefer to see only the top level message.  If a message has
;; several replies or is part of a thread, only show the first message.
;; `gnus-thread-ignore-subject' will ignore the subject and
;; look at 'In-Reply-To:' and 'References:' headers.
(setq gnus-thread-hide-subtree t)
(setq gnus-thread-ignore-subject t)

;; read HTML mail
;; you need install the command line web browser 'w3m' and Emacs plugin 'w3m'
(setq mm-text-html-renderer 'w3m)

;; don't delete mails on server (POP3)
;; (setq pop3-leave-mail-on-server t)

;; message fill column
(defun ds/message-mode-setup ()
       (setq fill-column 80)
       (turn-on-auto-fill))
     (add-hook 'message-mode-hook 'ds/message-mode-setup)

;;------------------------------------------------------------------------------
;; RSS
;;------------------------------------------------------------------------------

;; test rss (needs configuration)
;; (add-to-list 'gnus-secondary-select-methods
;;              '(nnrss "site"))

;; configure atom RSS
(require 'mm-url)
(defadvice mm-url-insert (after DE-convert-atom-to-rss () )
  "Converts atom to RSS by calling xsltproc."
  (when (re-search-forward "xmlns=\"http://www.w3.org/.*/Atom\""
                           nil t)
    (goto-char (point-min))
    (message "Converting Atom to RSS... ")
    (call-process-region (point-min) (point-max)
                         "xsltproc"
                         t t nil
                         (expand-file-name "~/.emacs.d/atom2rss.xsl") "-")
    (goto-char (point-min))
    (message "Converting Atom to RSS... done")))
(ad-activate 'mm-url-insert)

;;------------------------------------------------------------------------------
;; setup for multiple mails
;;------------------------------------------------------------------------------

;; gmail

(add-to-list 'gnus-secondary-select-methods
             '(nnimap "gmail"
               (nnimap-address "imap.gmail.com")
               (nnimap-server-port 993)
               (nnimap-stream ssl)
               (nnir-search-engine imap)
               ;; @see http://www.gnu.org/software/emacs/manual/html_node/gnus/Expiring-Mail.html
               ;; press 'E' to expire email
               ;; (nnmail-expiry-target "nnimap+gmail:[Gmail]/Trash")
               (nnmail-expiry-wait 90)))

;; upatras
(add-to-list 'gnus-secondary-select-methods
             '(nnimap "upatras"
               (nnimap-address "mail.upatras.gr")
               (nnimap-server-port 993)
               (nnimap-stream ssl)
               (nnir-search-engine imap)
               (nnmail-expiry-wait 90)))

;; ece.upatras
(add-to-list 'mail-sources
             `(pop :server "mailgate.ece.upatras.gr"
                   :port 110
                   :user "stanev@ece"
                   :password nil
                   :leave 365))

;; default
(setq user-full-name "Dimitar Stanev"
      user-mail-address "jimstanev@gmail.com"
      ;; smtpmail-smtp-server ""
      ;; smtpmail-smtp-service 0
      ;; auth-sources "~/.authinfo.gpg"
      )

;; let gnus change the "From:" line by looking at current group we are in
(setq gnus-posting-styles
      '(
        ("gmail" (address "jimstanev@gmail.com"))
        ("ece.upatras" (address "stanev@ece.upatras.gr"))
        ("upatras" (address "stanev@upatras.gr"))
        ))

;; (require-package 'smtpmail-multi)

;; available SMTP accounts
(defvar smtp-accounts
  '(
    (plain "stanev@ece.upatras.gr" "mailgate.ece.upatras.gr" 25 "stanev@ece" nil)
    (ssl   "stanev@upatras.gr" "mail.upatras.gr" 465 "stanev" nil "key" "cert")
    (ssl   "jimstanev@gmail.com" "smtp.gmail.com" 587 "jimstanev" nil "key" "cert")
    ))

;; message signature
(setq message-signature nil)
(setq message-signature-directory "~/.emacs.d/signature/")
(setq message-signature-file "upatras")

;;------------------------------------------------------------------------------
;; utility function for choosing the send server from 'FROM' field
;;------------------------------------------------------------------------------

(require 'cl)
(require 'smtpmail)

(setq send-mail-function 'smtpmail-send-it
      message-send-mail-function 'smtpmail-send-it
      mail-from-style nil
      user-full-name "Dimitar Stanev"
      smtpmail-debug-info t
      smtpmail-debug-verb t)

(defun ds/set-smtp (mech server port user password)
  "Set related SMTP variables for supplied parameters."
  (setq smtpmail-smtp-server server
        smtpmail-smtp-service port
        smtpmail-auth-credentials (list (list server port user password))
        smtpmail-auth-supported (list mech)
        smtpmail-starttls-credentials nil)
  (message "Setting SMTP server to `%s:%s' for user `%s'."
           server port user))

(defun ds/set-smtp-ssl (server port user password  &optional key cert)
  "Set related SMTP and SSL variables for supplied parameters."
  (setq starttls-use-gnutls t
        starttls-gnutls-program "gnutls-cli"
        starttls-extra-arguments nil
        smtpmail-smtp-server server
        smtpmail-smtp-service port
        smtpmail-auth-credentials (list (list server port user password))
        smtpmail-starttls-credentials (list (list server port key cert)))
  (message
   "Setting SMTP server to `%s:%s' for user `%s'. (SSL enabled.)"
   server port user))

(defun ds/change-smtp ()
  "Change the SMTP server according to the current from line."
  (save-excursion
   (loop with from = (save-restriction
                      (message-narrow-to-headers)
                      (message-fetch-field "from"))
         for (auth-mech address . auth-spec) in smtp-accounts
         when (string-match address from)
           do (cond
                ((memq auth-mech '(cram-md5 plain login))
                 (return (apply 'ds/set-smtp (cons auth-mech auth-spec))))
                ((eql auth-mech 'ssl)
                 (return (apply 'ds/set-smtp-ssl auth-spec)))
                (t (error "Unrecognized SMTP auth. mechanism: `%s'." auth-mech)))
         finally (error "Cannot infer SMTP information."))))

(defadvice ds/smtpmail-via-smtp
    (before smtpmail-via-smtp-ad-change-smtp (recipient smtpmail-text-buffer))
  "Call `ds/change-smtp' before every `smtpmail-via-smtp'."
  (with-current-buffer smtpmail-text-buffer (ds/change-smtp)))
(ad-activate 'ds/smtpmail-via-smtp)

;;------------------------------------------------------------------------------
;; define topics organization
;;------------------------------------------------------------------------------

(eval-after-load
 'gnus-topic
 '(progn
   (setq gnus-message-archive-group '((format-time-string "sent.%Y")))
   (setq gnus-server-alist
    '(("archive" nnfolder "archive"
       (nnfolder-directory "~/.emacs.d/gnus/mail/archive")
       (nnfolder-active-file "~/.emacs.d/gnus/mail/archive/active")
       (nnfolder-get-new-mail nil)
       (nnfolder-inhibit-expiry t))))

   (setq gnus-topic-topology '(("Gnus" visible)
                               (("RSS" visible))
                               (("Email" visible)
                                (("gmail" visible nil nil))
                                (("ece.upatras" visible nil nil))
                                (("upatras" visible nil nil)))
                               ))

   (setq gnus-topic-alist '(
                            ("gmail" ; the key of topic
                             "nnimap+gmail:INBOX"
                             "nnimap+gmail:Sent Items"
                             "nnimap+gmail:Junk E-mail"
                             "nnimap+gmail:[Gmail]"
                             "nnimap+gmail:[Gmail]/Drafts"
                             "nnimap+gmail:[Gmail]/Spam"
                             "nnimap+gmail:[Gmail]/Trash"
                             "nnimap+gmail:[Gmail]/Sent Mail")
                            ("ece.upatras" ; the key of topic
                             "mail.misc")
                            ("upatras" ; the key of topic
                             "nnimap+upatras:INBOX"
                             "nnimap+upatras:INBOX.Sent"
                             "nnimap+upatras:INBOX.Drafts"
                             "nnimap+upatras:INBOX.Junk"
                             "nnimap+upatras:INBOX.Trash")
                            ("Email" ; the key of topic
                             "nnfolder+archive:sent.2017"
                             "nndraft:drafts")
                            ("RSS"      ; the key of topic
                             "nnrss:RT - Daily news")
                            ("Gnus")))))

;;------------------------------------------------------------------------------
;; gnus hydra
;;------------------------------------------------------------------------------

;; after some time will remove hydra
(require-package 'hydra)

(eval-after-load 'gnus-group
                 '(progn
                   (defhydra hydra-gnus-group (:color blue)
                    "Do?"
                    ("a" gnus-group-list-active "REMOTE groups A A")
                    ("l" gnus-group-list-all-groups "LOCAL groups L")
                    ("c" gnus-topic-catchup-articles "Read all c")
                    ("G" gnus-group-make-nnir-group "Search server G G")
                    ("g" gnus-group-get-new-news "Refresh g")
                    ("s" gnus-group-enter-server-mode "Servers")
                    ("m" gnus-group-new-mail "Compose m OR C-x m")
                    ("#" gnus-topic-mark-topic "mark #")
                    ("q" nil "cancel"))
                   ;; y is not used by default
                   (define-key gnus-group-mode-map "y" 'hydra-gnus-group/body)))

;; gnus-summary-mode
(eval-after-load 'gnus-sum
                 '(progn
                   (defhydra hydra-gnus-summary (:color blue)
                    "Do?"
                    ("s" gnus-summary-show-thread "Show thread")
                    ("h" gnus-summary-hide-thread "Hide thread")
                    ("n" gnus-summary-insert-new-articles "Refresh / N")
                    ("f" gnus-summary-mail-forward "Forward C-c C-f")
                    ("!" gnus-summary-tick-article-forward "Mail -> disk !")
                    ("p" gnus-summary-put-mark-as-read "Mail <- disk")
                    ("c" gnus-summary-catchup-and-exit "Read all c")
                    ("e" gnus-summary-resend-message-edit "Resend S D e")
                    ("R" gnus-summary-reply-with-original "Reply with original R")
                    ("r" gnus-summary-reply "Reply r")
                    ("W" gnus-summary-wide-reply-with-original "Reply all with original S W")
                    ("w" gnus-summary-wide-reply "Reply all S w")
                    ("#" gnus-topic-mark-topic "mark #")
                    ("q" nil "cancel"))
                   ;; y is not used by default
                   (define-key gnus-summary-mode-map "y" 'hydra-gnus-summary/body)))

;; gnus-article-mode
(eval-after-load 'gnus-art
                 '(progn
                   (defhydra hydra-gnus-article (:color blue)
                    "Do?"
                    ("f" gnus-summary-mail-forward "Forward")
                    ("R" gnus-article-reply-with-original "Reply with original R")
                    ("r" gnus-article-reply "Reply r")
                    ("W" gnus-article-wide-reply-with-original "Reply all with original S W")
                    ("o" gnus-mime-save-part "Save attachment at point o")
                    ("w" gnus-article-wide-reply "Reply all S w")
                    ("q" nil "cancel"))
                   ;; y is not used by default
                   (define-key gnus-article-mode-map "y" 'hydra-gnus-article/body)))

(eval-after-load 'message
                 '(progn
                   (defhydra hydra-message (:color blue)
                    "Do?"
                    ("ca" mml-attach-file "Attach C-c C-a")
                    ("cc" message-send-and-exit "Send C-c C-c")
                    ("q" nil "cancel"))
                   (global-set-key (kbd "C-c u h") 'hydra-message/body)))

(provide 'ds-gnus)
