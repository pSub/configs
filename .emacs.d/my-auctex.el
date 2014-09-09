;; http://pleasefindattached.blogspot.de/2011/12/emacsauctex-sentence-fill-greatly.html
(defadvice LaTeX-fill-region-as-paragraph (around LaTeX-sentence-filling)
  "Start each sentence on a new line."
  (let ((from (ad-get-arg 0))
        (to-marker (set-marker (make-marker) (ad-get-arg 1)))
        tmp-end)
    (while (< from (marker-position to-marker))
      (forward-sentence)
      ;; might have gone beyond to-marker --- use whichever is smaller:
      (ad-set-arg 1 (setq tmp-end (min (point) (marker-position to-marker))))
      ad-do-it
      (ad-set-arg 0 (setq from (point)))
      (unless (or
               (bolp)
               (looking-at "\\s *$"))
        (LaTeX-newline)))
    (set-marker to-marker nil)))
(ad-activate 'LaTeX-fill-region-as-paragraph)

(setq reftex-bibliography-commands '("bibliography" "nobibliography" "addbibresource"))

(setq LaTeX-verbatim-environments-local '("Verbatim" "lstlisting"))

;; Set PDF mode as default
(setq TeX-PDF-mode t)

(setq-default TeX-master nil) ; Query for master file.

;; Set zathura as prefered pdf reader
(setq TeX-view-program-list '(("pdfviewer" "pdfviewer %o")))
(setq TeX-view-program-selection '((output-pdf "pdfviewer")))

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
