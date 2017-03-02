(require 'ispell)

(when (executable-find ispell-program-name)
  (require 'ds-flyspell))

(provide 'ds-spelling)
