(setq inhibit-startup-message t ; show scratch buffer on start
      inhibit-startup-echo-area-message t)

(global-linum-mode 1) ; line numbers in all buffers
(define-key global-map (kbd "RET") 'newline-and-indent)
(add-to-list 'load-path "~/.emacs.d/")
(setq next-line-add-newlines t) ; C-n makes new line
(setq x-menu 'meta)
(setq-default indent-tabs-mode nil)
(menu-bar-mode -1)
(tool-bar-mode -1)
(scroll-bar-mode -1)

(ido-mode t)
(show-paren-mode t)
  
(dolist (dir '(
	       "/usr/local/share/emacs/site-lisp"
	       ))
  (add-to-list 'load-path dir))

(require 'magit)
(require 'vimpulse)
(load "my-auctex")
(load "my-util")
(load "my-haskell")
(load "my-org-mode")
(load "my-java")
(load "my-php")
(load "my-wl")
(load "my-vcs")
(load "my-prolog")
(load "my-nxhtml")
(sml-modeline-mode t)
