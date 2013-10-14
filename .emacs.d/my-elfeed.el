(require-package 'elfeed)
(require 'elfeed)

(defun elfeed-my-update ()
  (interactive)
  (load-file "~/.emacs.d/my-elfeed.el")
  (elfeed-update))

(define-key elfeed-search-mode-map "G" 'elfeed-my-update)

(global-set-key (kbd "C-x w") 'elfeed)

(setq elfeed-feeds (read-lines "~/.feeds"))

(dolist (regex '("github.*udisks-glue"
                 "github.*bgs"
                 "bitbucket.*dwb"))
  (add-hook 'elfeed-new-entry-hook
            (elfeed-make-tagger :feed-url regex
                                :add '(packaging))))

(dolist (regex '("tu-darmstadt.de"
                 "cased.de"))
  (add-hook 'elfeed-new-entry-hook
            (elfeed-make-tagger :feed-url regex
                                :add '(tu-darmstadt))))

(provide ' my-elfeed)
