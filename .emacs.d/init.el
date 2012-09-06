(setq inhibit-startup-message t ; show scratch buffer on start
      inhibit-startup-echo-area-message t)

(global-linum-mode 1) ; line numbers in all buffers
(define-key global-map (kbd "RET") 'newline-and-indent)
(setq next-line-add-newlines t) ; C-n makes new line
(setq x-menu 'meta)
(setq-default indent-tabs-mode nil)
(setq-default show-trailing-whitespace t)
(menu-bar-mode -1)
(tool-bar-mode -1)
(scroll-bar-mode -1)

; http://savannah.nongnu.org/projects/dtrt-indent/
;(require 'dtrt-indent)
;(dtrt-indent-mode 1)

; Save all backupfiles in ~/.saves
(setq
   backup-by-copying t      ; don't clobber symlinks
   backup-directory-alist
    '(("." . "~/.saves"))    ; don't litter my fs tree
   auto-save-list-file-name "~/.auto-save-list"
   delete-old-versions t
   kept-new-versions 6
   kept-old-versions 2
   version-control t)       ; use versioned backups

; Save auto saved files in /tmp
(setq auto-save-file-name-transforms
   `((".*" ,temporary-file-directory t)))

(setq browse-url-generic-program "conkeror"
      browse-url-browser-function 'browse-url-generic)

(ido-mode t)
(show-paren-mode t)

(add-to-list 'load-path (expand-file-name "~/.emacs.d/"))
(dolist (dir '(
  "/usr/local/share/emacs/site-lisp"
  "/home/pascal/.elisp"
  ))
  (add-to-list 'load-path dir))

(load "color-theme-molokai/color-theme-molokai")
(color-theme-molokai)

(load "undo-tree/undo-tree")
(global-undo-tree-mode)

(load "my-evil")
(load "my-helm")
(load "my-auctex")
(load "my-util")
(load "my-haskell")
(load "my-org-mode")
(load "my-java")
(load "my-wl")
(load "my-vcs")
(load "my-prolog")
(load "my-agda")
