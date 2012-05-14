(load "auctex.el" nil t t)
(load "preview-latex.el" nil t t)

;; Set PDF mode as default
(setq TeX-PDF-mode t)

;; Set zathura as prefered pdf reader
(setq TeX-view-program-list '(("Zathura" "zathura %o")))
(setq TeX-view-program-selection '((output-pdf "Zathura")))

;; Load RefTex
(add-hook 'LaTeX-mode-hook 'turn-on-reftex)

;; Load flyspell-mode
(add-hook 'LaTeX-mode-hook 'flyspell-mode)

;; Spellcheck the buffer
(add-hook 'LaTeX-mode-hook 'flyspell-buffer)

;; Load auto-fill-mode
(add-hook 'LaTeX-mode-hook 'auto-fill-mode)

(provide 'my-auctex)
