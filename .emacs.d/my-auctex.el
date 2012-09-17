(load "auctex.el" nil t t)
(load "preview-latex.el" nil t t)

;; Set PDF mode as default
(setq TeX-PDF-mode t)

(setq-default TeX-master nil) ; Query for master file.

;; Set zathura as prefered pdf reader
(setq TeX-view-program-list '(("zathura" "zathura %o")))
(setq TeX-view-program-selection '((output-pdf "zathura")))

;; Load RefTex
(add-hook 'LaTeX-mode-hook 'turn-on-reftex)

;; Load flyspell-mode
(add-hook 'LaTeX-mode-hook 'flyspell-mode)

;; Spellcheck the buffer
(add-hook 'LaTeX-mode-hook 'flyspell-buffer)

;; Load auto-fill-mode
(add-hook 'LaTeX-mode-hook 'auto-fill-mode)

;; Load orgtbl-mode
(add-hook 'LaTeX-mode-hook 'orgtbl-mode)

(provide 'my-auctex)
