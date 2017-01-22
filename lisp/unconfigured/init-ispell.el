(require 'ispell)
;; (require 'rw-hunspell)

(add-to-list 'ispell-local-dictionary-alist '("greek"
                                              "[[:alpha:]]"
                                              "[^[:alpha:]]"
                                              "[']"
                                              t
                                              ("-d" "el_GR"); Dictionary file name
                                              nil
                                              utf-8))

(add-to-list 'ispell-local-dictionary-alist '("english"
                                              "[[:alpha:]]"
                                              "[^[:alpha:]]"
                                              "[']"
                                              t
                                              ("-d" "en_US")
                                              nil
                                              utf-8))

(setq ispell-program-name "hunspell"          ; Use hunspell to correct mistakes
      ispell-dictionary   "english") ; Default dictionary to use

(defun switch-dictionary-en-gr ()
  "Switch german and english dictionaries."
  (interactive)
  (let* ((dict ispell-current-dictionary)
         (new (if (string= dict "englush") "english"
                   "greek")))
    (ispell-change-dictionary new)
    (message "Switched dictionary from %s to %s" dict new)))

(global-set-key (kbd "C-c d") 'switch-dictionary-en-gr)

(provide 'init-ispell)
