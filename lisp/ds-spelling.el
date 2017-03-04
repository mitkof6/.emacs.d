(require 'ispell)

;; spell checking
;; (when (executable-find "hunspell")
;;   (setq-default ispell-program-name "hunspell")
;;   (setq ispell-really-hunspell t)
;;   (require 'init-flyspell))
(when (executable-find "aspell")
  (setq-default ispell-program-name "aspell")
  (setq ispell-really-aspell t)
  (require 'ds-flyspell))

(when (executable-find ispell-program-name)
  (require 'ds-flyspell))

(provide 'ds-spelling)
