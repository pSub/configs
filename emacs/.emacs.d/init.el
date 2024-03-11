;; Use the emacs package manager
(require 'package)
(add-to-list 'package-archives
             '("melpa-stable" . "https://stable.melpa.org/packages/"))

(package-initialize)
(unless package-archive-contents
  (package-refresh-contents))

(defun require-package (p)
  (unless (package-installed-p p)
    (package-install p)))

(defalias 'yes-or-no-p 'y-or-n-p)

(require-package 'yasnippet)
(require-package 'undo-tree)
(require-package 'flycheck)
(require-package 'ghc)
;(require-package 'ghci-completion)
(require-package 'auctex)

(add-hook 'after-init-hook #'global-flycheck-mode)

(yas-global-mode 1)

(setq inhibit-startup-message t ; show scratch buffer on start
      inhibit-startup-echo-area-message t)

;(global-linum-mode 1) ; line numbers in all buffers
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

(setq browse-url-generic-program "brave"
     browse-url-browser-function 'browse-url-generic)

(ido-mode t)
(show-paren-mode t)

(setq tramp-ssh-controlmaster-options "")

(dolist (dir '(
  "~/.emacs.d/configs/"
; Paths for NixOS
  "~/.nix-profile/share/org/contrib/lisp/"
  "~/.nix-profile/share/emacs/site-lisp/org/"
  ))
  (add-to-list 'load-path (expand-file-name dir)))

(add-to-list 'package-directory-list "~/.nix-profile/share/emacs/site-lisp/elpa")

(package-initialize)

; http://savannah.nongnu.org/projects/dtrt-indent/
;(load "dtrt-indent/dtrt-indent")
;(dtrt-indent-mode 1)

(global-undo-tree-mode)



;(load "my-evil")
;(load "my-helm")
(load "my-nix")
(load "my-writegood")
(load "my-auctex")
(load "my-util")
(load "my-haskell")
(load "my-org-mode")
(load "my-vcs")
;(load "my-notmuch")
(load "my-scala")
;(load "my-agda")
;(load "my-elfeed")
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(haskell-notify-p t)
 '(haskell-process-wrapper-function
   (lambda
     (argv)
     (append
      (list \"nix-shell\" \"-I\" \"\.\" \"--command\")
      (list
       (mapconcat
        (quote identity)
        argv \" \")))))
 '(haskell-stylish-on-save t)
 '(haskell-tags-on-save t)
 '(org-entities-user
   (quote
    (("ballot" "\\ballot" nil "✗" "" "" "✗")
     ("" "" nil "" "" "" ""))))
 '(package-selected-packages
   (quote
    (grip-mode markdown-mode writegood-mode use-package shm scala-mode nix-mode magit yasnippet undo-tree ghci-completion ghc flycheck auctex))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )


(use-package markdown-mode
  :ensure t
  :commands (markdown-mode gfm-mode)
  :mode (("README\\.md\\'" . gfm-mode)
         ("\\.md\\'" . markdown-mode)
         ("\\.markdown\\'" . markdown-mode))
  :init (setq markdown-command "pandoc"))
