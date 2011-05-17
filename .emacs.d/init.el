(setq inhibit-startup-message t ; show scratch buffer on start
      inhibit-startup-echo-area-message t)

(global-linum-mode 1) ; line numbers in all buffers
(define-key global-map (kbd "RET") 'newline-and-indent)
(add-to-list 'load-path "~/.emacs.d/")
(setq next-line-add-newlines t) ; C-n makes new line
(setq x-menu 'meta)
(setq-default indent-tabs-mode nil)

(dolist (dir '(
	       "/usr/local/share/emacs/site-lisp"
	       ))
  (add-to-list 'load-path dir))

(require 'magit)
(load "util")
(load "haskell")
(load "org-mode")

(ido-mode t)
(show-paren-mode t)
