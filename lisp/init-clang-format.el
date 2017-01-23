;; clang-format can be triggered using C-M-tab
(require 'clang-format)
(global-set-key [C-M-tab] 'clang-format-region)
;; Create clang-format file using google style
;; clang-format -style=google -dump-config > .clang-format

(provide 'init-clang-format)
