;;----------------------------------------------------------------------------
;; Define windows handling
;;----------------------------------------------------------------------------

;; navigate window layouts with "C-c n" and "C-c p"
(require-package 'tabbar)
(tabbar-mode 't)

;; navigate window layouts with "C-c <left>" and "C-c <right>"
;;(add-hook 'after-init-hook 'winner-mode)

;; make "C-x o" prompt for a target window when there are more than 2
(require-package 'switch-window)
(setq-default switch-window-shortcut-style 'alphabet)
(setq-default switch-window-timeout nil)

;; override C-x 1
(defun ds/toggle-delete-other-windows ()
  "Delete other windows in frame if any, or restore previous window config."
  (interactive)
  (if (and winner-mode
           (equal (selected-window) (next-window)))
      (winner-undo)
      (delete-other-windows)))

;; when splitting window, show (other-buffer) in the new window
(defun ds/split-window-with-other-buffer (split-function)
  (lexical-let ((s-f split-function))
               (lambda (&optional arg)
                 "Split this window and switch to the new window unless ARG is provided."
                 (interactive "P")
                 (funcall s-f)
                 (let ((target-window (next-window)))
                   (set-window-buffer target-window (other-buffer))
                   (unless arg
                     (select-window target-window))))))

;; rearrange split windows
(defun ds/split-window-horizontally-instead ()
  (interactive)
  (save-excursion
   (delete-other-windows)
   (funcall
    (ds/split-window-func-with-other-buffer 'split-window-horizontally))))

(defun ds/split-window-vertically-instead ()
  (interactive)
  (save-excursion
   (delete-other-windows)
   (funcall (ds/split-window-func-with-other-buffer 'split-window-vertically))))

;; borrowed from http://postmomentum.ch/blog/201304/blog-on-emacs
(defun ds/split-window()
  "Split the window to see the most recent buffer in the other window.
Call a second time to restore the original window configuration."
  (interactive)
  (if (eq last-command 'ds/split-window)
      (progn
        (jump-to-register :ds/split-window)
        (setq this-command 'ds/unsplit-window))
      (window-configuration-to-register :ds/split-window)
      (switch-to-buffer-other-window nil)))

(defun ds/toggle-current-window-dedication ()
  "Toggle whether the current window is dedicated to its current buffer."
  (interactive)
  (let* ((window (selected-window))
         (was-dedicated (window-dedicated-p window)))
    (set-window-dedicated-p window (not was-dedicated))
    (message "Window %sdedicated to %s"
             (if was-dedicated "no longer " "")
             (buffer-name))))

(unless (memq window-system '(nt w32))
  (windmove-default-keybindings 'control))

;; view tags other window
(defun ds/view-tag-other-window (tagname &optional next-p regexp-p)
  "Same as `find-tag-other-window' but doesn't move the point"
  (interactive (find-tag-interactive "View tag other window: "))
  (let ((window (get-buffer-window)))
    (find-tag-other-window tagname next-p regexp-p)
    (recenter 0)
    (select-window window)))

;; transpose frames (mainly for function flop-frame)
(require-package 'transpose-frame)

(provide 'ds-windows)
