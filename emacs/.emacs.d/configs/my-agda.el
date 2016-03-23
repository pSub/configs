(load-file (let ((coding-system-for-read 'utf-8))
                (shell-command-to-string "agda-mode locate")))
(custom-set-variables
 '(agda2-include-dirs (quote ("." "/home/pascal/.nix-profile/share/agda")))
 )

(provide 'my-agda)
