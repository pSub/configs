(add-to-list 'load-path "~/.elisp/helm")
(require 'helm-config)
(global-set-key (kbd "C-c h") 'helm-mini)

(provide 'my-helm)
