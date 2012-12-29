(load-file (let ((coding-system-for-read 'utf-8))
                (shell-command-to-string "agda-mode locate")))
(custom-set-variables
 '(agda2-include-dirs (quote ("." "/home/pascal/.agda/lib-0.6/src")))
 )

(provide 'my-agda)
