(require 'notmuch)
(require 'gnus-art)

;; Sign messages by default.
(add-hook 'message-setup-hook 'mml-secure-sign-pgpmime)

(provide 'my-notmuch)
