(load-file (let ((coding-system-for-read 'utf-8))
                (shell-command-to-string "agda-mode locate")))
(custom-set-variables
 '(agda2-include-dirs (quote ("." "~/.agda/src")))
 '(haskell-notify-p t)
 '(haskell-process-type (quote ghci)))

(provide 'my-agda)
