;;;; -*- mode: emacs-lisp -*-
(require-package 'w3m)

;; (add-hook 'w3m-mode-hook 'scroll-lock-mode)
(global-set-key (kbd "C-c w") 'w3m)
(setq browse-url-mozilla-program "firefox"
      browse-url-browser-function 'w3m-browse-url-new-session)

(defun w3m-browse-url-new-session (url &optional new-session)
  (w3m-browse-url url t))

(setq w3m-language 'utf-8
      w3m-coding-system 'utf-8
      w3m-file-coding-system 'utf-8
      w3m-input-coding-system 'utf-8
      w3m-output-coding-system 'utf-8
      w3m-current-coding-system 'utf-8
      w3m-default-coding-system 'utf-8
      w3m-terminal-coding-system 'utf-8
      w3m-file-name-coding-system 'utf-8
      w3m-bookmark-file-coding-system 'utf-8
      w3m-file-coding-system-for-read 'utf-8
      w3m-form-textarea-file-coding-system 'utf-8
      w3m-form-input-textarea-coding-system 'utf-8
      w3m-home-page "https://www.google.com/"
      w3m-default-display-inline-images t
      w3m-confirm-leaving-secure-page nil
      w3m-session-crash-recovery nil
      w3m-use-cookies nil)

;;; Keybindings and respective functions
(defun w3m-goto-previous-url ()
  (interactive)
  (if w3m-previous-url
      (let ((w3m-prefer-cache t))
        (w3m-goto-url w3m-previous-url))
      (signal 'no-next-url nil)))

(defun w3m-goto-next-url ()
  (interactive)
  (if w3m-next-url
      (let ((w3m-prefer-cache t))
        (w3m-goto-url w3m-next-url))
      (signal 'no-next-url nil)))

(defun w3m-lnum-follow-new-session ()
  (interactive)
  (w3m-lnum-follow 4))

(defun w3m-goto-url-no-initial (url &optional reload charset post-data
                                      referer handler element no-popup)
  (interactive
   (list (w3m-input-url nil "" nil nil 'feeling-lucky)
         current-prefix-arg
         (w3m-static-if (fboundp 'universal-coding-system-argument)
                        coding-system-for-read)))
  (w3m-goto-url url reload charset post-data referer handler element no-popup))

(defun w3m-goto-url-no-initial-new-session (url &optional reload charset post-data referer)
  (interactive
   (list (w3m-input-url nil "" nil nil 'feeling-lucky)
         current-prefix-arg
         (w3m-static-if (fboundp 'universal-coding-system-argument)
                        coding-system-for-read)))
  (w3m-goto-url-new-session url reload charset post-data referer))

(defun w3m-apply-custom-map ()
  (define-key w3m-mode-map (kbd "M-n") 'w3m-next-buffer)
  (define-key w3m-mode-map (kbd "M-p") 'w3m-previous-buffer)
  (define-key w3m-mode-map (kbd "}") 'w3m-goto-next-url)
  (define-key w3m-mode-map (kbd "{") 'w3m-goto-previous-url)
  (define-key w3m-mode-map (kbd "g") 'w3m-goto-url-no-initial)
  (define-key w3m-mode-map (kbd "G") 'w3m-goto-url-no-initial-new-session)
  (define-key w3m-mode-map (kbd "f") 'w3m-lnum-follow)
  (define-key w3m-mode-map (kbd "j") 'w3m-lnum-follow-new-session)
  (define-key w3m-mode-map (kbd "q") 'w3m-delete-buffer)
  (define-key w3m-mode-map (kbd "Q") 'w3m-close-window)
  (define-key w3m-mode-map (kbd "C-c C-w") 'w3m-quit))

(add-hook 'w3m-mode-hook 'w3m-apply-custom-map)

(require 'w3m-search)
(eval-after-load
 'w3m-search
 '(progn
   (setq w3m-uri-replace-alist
    (append (list
             '("\\`arch:" w3m-search-uri-replace "arch")
             '("\\`aur:" w3m-search-uri-replace "aur")
             '("\\`d:" w3m-search-uri-replace "duckduckgo")
             '("\\`i:" w3m-search-uri-replace "imdb")
             '("\\`ld:" w3m-search-uri-replace "luckyd")
             '("\\`lg:" w3m-search-uri-replace "luckyg")
             '("\\`lisp:" w3m-search-uri-replace "lispdoc")
             '("\\`mw:" w3m-search-uri-replace "mw")
             '("\\`ocl:" w3m-search-uri-replace "opencl")
             '("\\`p:" w3m-search-uri-replace "piratebay")
             '("\\`s:" w3m-search-uri-replace "scholar")
             '("\\`t:" w3m-search-uri-replace "torrent")
             '("\\`w:" w3m-search-uri-replace "wikipedia"))
     w3m-uri-replace-alist))
   (setq w3m-search-engine-alist
    (append (list
             '("duckduckgo" "https://duckduckgo.com/?q=%s")
             '("lispdoc" "http://lispdoc.com/?q=%s&search=Full+text+search")
             w3m-search-engine-alist)))))

(provide 'ds-w3m)
