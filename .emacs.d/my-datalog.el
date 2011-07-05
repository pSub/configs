(load "~/.elisp/des")
; adapt the following path as necessary:
(setq des-prolog-file "~/.bin/des.pl")
(setq-default des-system 'swi)
(add-to-list 'auto-mode-alist '("\\.dl$" . des-mode))

(provide 'my-datalog)
