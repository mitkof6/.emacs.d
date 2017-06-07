;; (require 'octave-mode)

(setq auto-mode-alist
      (cons '("\\.m$" . octave-mode) auto-mode-alist))

(setq-default octave-auto-indent t
	      octave-auto-newline t
	      octave-blink-matching-block t
	      octave-block-offset 4
	      octave-continuation-offset 4
	      octave-continuation-string "\\"
	      octave-mode-startup-message t
	      octave-send-echo-input t
	      octave-send-line-auto-forward t
	      octave-send-show-buffer t)

(defun ds/octave-mode-hook ()
  (abbrev-mode 1)
  (auto-fill-mode 1)
  (if (eq window-system 'x)
      (font-lock-mode 1)))
(add-hook 'octave-mode-hook 'ds/octave-mode-hook)

;; configure up/down keys for octave inferior mode
(add-hook 'inferior-octave-mode-hook
	  (lambda ()
	    (turn-on-font-lock)
	    (define-key inferior-octave-mode-map [up]
	      'comint-previous-input)
	    (define-key inferior-octave-mode-map [down]
	      'comint-next-input)))

;; add completion
(add-hook 'octave-mode-hook
	  '(lambda nil
	    (define-key octave-mode-map [(tab)]
	     'octave-complete-symbol)))

;; run an inferior Octave process in a special Emacs buffer
(autoload 'run-octave "octave-inf" nil t)

;; if you have the win32 version of octave
(add-hook 'inferior-octave-mode-hook
	  '(lambda ()
	    (setq inferior-octave-program
	     "/usr/bin/octave")))

;; If `gnuserv' is installed, add the lines
(autoload 'octave-help "octave-hlp" nil t)
;; (require-package 'gnuserv)
;; (gnuserv-start)

(provide 'ds-octave)
