(autoload 'wl "wl" "Wanderlust" t)
(autoload 'wl-other-frame "wl" "Wanderlust on new frame." t)
(autoload 'wl-draft "wl-draft" "Write draft with Wanderlust." t)

;; s14.pixelx.de as main IMAP server
(setq elmo-imap4-default-server "s14.pixelx.de")

;; s14.pixelx.de as main SMTP server
(setq wl-smtp-posting-server "s14.pixelx.de")

(setq wl-message-id-domain "s14.pixelx.de")

;; web433p1 as main user
(setq elmo-imap4-default-user "web433p1")

;; Use SSL connection
(setq elmo-imap4-default-stream-type 'ssl)

(setq ssl-certificate-verification-policy 1)

;; Set IMP Port to 933
(setq elmo-imap4-default-port '993)




(autoload 'wl-user-agent-compose "wl-draft" nil t)
(if (boundp 'mail-user-agent)
    (setq mail-user-agent 'wl-user-agent))
(if (fboundp 'define-mail-user-agent)
    (define-mail-user-agent
      'wl-user-agent
      'wl-user-agent-compose
      'wl-draft-send
      'wl-draft-kill
      'mail-send-hook))

(provide 'my-wl)
