(require-package 'elfeed)
(require 'elfeed)

(global-set-key (kbd "C-x w") 'elfeed)

(setq elfeed-feeds (read-lines "~/.feeds"))

(dolist (regex '("github.*udisks-glue"
                 "github.*bgs"
                 "bitbucket.*dwb"))
  (add-hook 'elfeed-new-entry-hook
            (elfeed-make-tagger :feed-url regex
                                :add '(packaging))))

(provide ' my-elfeed)
