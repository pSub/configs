;; Use the emacs package manager
(require 'package)
(dolist (source '(("marmalade" . "http://marmalade-repo.org/packages/")))
  (add-to-list 'package-archives source))

(package-initialize)
(unless package-archive-contents
  (package-refresh-contents))

(defun require-package (p)
  (unless (package-installed-p p)
    (package-install p)))

(require-package 'yasnippet)
(require-package 'undo-tree)
(require-package 'flycheck)
(require-package 'ghc)
(require-package 'ghci-completion)
(require-package 'auctex)

(add-hook 'after-init-hook #'global-flycheck-mode)

(yas-global-mode 1)

;; auto-complete
(require 'auto-complete-config)
(add-to-list 'ac-dictionary-directories "~/.emacs.d/elpa/auto-complete/dict")

(set-default 'ac-sources
             '(ac-source-abbrev
               ac-source-dictionary
               ac-source-yasnippet
               ac-source-words-in-buffer
               ac-source-words-in-same-mode-buffers
               ac-source-semantic))
 
(ac-config-default)
 
(global-auto-complete-mode t)

(setq inhibit-startup-message t ; show scratch buffer on start
      inhibit-startup-echo-area-message t)

(global-linum-mode 1) ; line numbers in all buffers
(define-key global-map (kbd "RET") 'newline-and-indent)
(setq next-line-add-newlines t) ; C-n makes new line
(setq x-menu 'meta)
(setq-default indent-tabs-mode nil)
(setq-default show-trailing-whitespace nil)
(menu-bar-mode -1)
(tool-bar-mode -1)
(scroll-bar-mode -1)


;; Save all backupfiles in ~/.saves
(setq
   backup-by-copying t      ; don't clobber symlinks
   backup-directory-alist
    '(("." . "~/.saves"))    ; don't litter my fs tree
   auto-save-list-file-name "~/.auto-save-list"
   delete-old-versions t
   kept-new-versions 6
   kept-old-versions 2
   version-control t)       ; use versioned backups

;; Save auto saved files in /tmp
(setq auto-save-file-name-transforms
   `((".*" ,temporary-file-directory t)))


(setq url-cache-directory temporary-file-directory)

(setq browse-url-generic-program "dwb"
     browse-url-browser-function 'browse-url-generic)

(ido-mode t)
(show-paren-mode t)

(dolist (dir '(
  "~/.emacs.d/"
; Paths for NixOS
  "~/.nix-profile/share/org/contrib/lisp/"
  "~/.nix-profile/share/emacs/site-lisp/"
  "~/.nix-profile/emacs/site-lisp/org/"
; Path for Arch Linux
  "/usr/local/share/emacs/site-lisp"
  "~/.elisp"
  ))
  (add-to-list 'load-path (expand-file-name dir)))

; http://savannah.nongnu.org/projects/dtrt-indent/
;(load "dtrt-indent/dtrt-indent")
;(dtrt-indent-mode 1)

;(load "color-theme-molokai/color-theme-molokai")
;(color-theme-molokai)

(global-undo-tree-mode)

;(load "my-evil")
;(load "my-helm")
(load "my-auctex")
(load "my-util")
(load "my-haskell")
(load "my-org-mode")
(load "my-vcs")
;(load "my-notmuch")
(load "my-scala")
(load "my-agda")
(load "my-elfeed")
