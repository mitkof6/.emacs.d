(require-package 'exec-path-from-shell)

(when (memq window-system '(mac ns x))
  (exec-path-from-shell-initialize))

(provide 'ds-exec-path)
