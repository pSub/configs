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

(setq TeX-command "pdftex")

(setq-default TeX-master nil) ; Query for master file.

(setq TeX-view-program-list '(("llpp.inotify" "llpp.inotify %o")))
(setq TeX-view-program-selection '((output-pdf "llpp.inotify")))

;; Load RefTex
(add-hook 'LaTeX-mode-hook 'turn-on-reftex)

;; Load flyspell-mode
(add-hook 'LaTeX-mode-hook 'flyspell-mode)

;; Spellcheck the buffer
(add-hook 'LaTeX-mode-hook 'flyspell-buffer)

;; Write Good mode
(add-hook 'LaTeX-mode-hook 'writegood-mode)

;; Load auto-fill-mode
(add-hook 'LaTeX-mode-hook 'auto-fill-mode)

;; Load orgtbl-mode
(add-hook 'LaTeX-mode-hook 'orgtbl-mode)

;; (defun my-fill-latex-paragraph ()
;;   "Fill the current paragraph, separating sentences w/ a newline.

;; AUCTeX's latex.el reimplements the fill functions and is *very*
;; convoluted.  We use part of it --- skip comment par we are in."
;;   (interactive)
;;   (if (save-excursion
;;         (beginning-of-line) (looking-at TeX-comment-start-regexp))
;;       (TeX-comment-forward)
;;   (let ((to (progn
;;               (LaTeX-forward-paragraph)
;;               (point)))
;;         (from (progn
;;                 (LaTeX-backward-paragraph)
;;                 (point)))
;;         (to-marker (make-marker)))
;;     (set-marker to-marker to)
;;     (while (< from (marker-position to-marker))
;;       (forward-sentence)
;;       (setq tmp-end (point))
;;       (LaTeX-fill-region-as-paragraph from tmp-end)
;;       (setq from (point))
;;       (unless (bolp)
;;         (LaTeX-newline))))))

;; (eval-after-load "latex"
;;   '(define-key LaTeX-mode-map (kbd "C-M-q") 'my-fill-latex-paragraph))

(provide 'my-auctex)
