(setq inhibit-startup-message t ; show scratch buffer on start
      inhibit-startup-echo-area-message t)

(global-linum-mode 1) ; line numbers in all buffers
(define-key global-map (kbd "RET") 'newline-and-indent)
(add-to-list 'load-path "~/.emacs.d/")
(setq next-line-add-newlines t) ; C-n makes new line
(setq x-menu 'meta)

(load "/usr/share/emacs/site-lisp/haskell-mode/haskell-site-file")
(add-hook 'haskell-mode-hook 'turn-on-haskell-doc-mode)
(add-hook 'haskell-mode-hook 'turn-on-haskell-indention)

(load "auctex.el" nil t t)
(load "preview-latex.el" nil t t)
