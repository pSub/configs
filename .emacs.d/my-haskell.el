;; haskell-emacs from Chris Done

(message "Loading dependencies...")
(add-to-list 'load-path (expand-file-name "~/.elisp/haskell-emacs/lib/"))
(add-to-list 'load-path (expand-file-name "~/.elisp/haskell-emacs/lib/auto-complete-1.3.1"))
(require 'auto-complete)
(require 'auto-complete-etags)
(require 'paredit)
(message "Loaded dependencies.")

(message "Loading hs library...")
(add-to-list 'load-path "~/.elisp/haskell-emacs/src/")
(require 'hs)
(message "Load hs library.")

;; Instructions:
(message "Run M-x hs-project-start (that's Alt-x) to start a project.")

;; Setup associations with file types.
(add-to-list 'auto-mode-alist (cons "\\.hs\\'" 'hs-mode))
(add-to-list 'auto-mode-alist (cons "\\.cabal\\'" 'hs-cabal-mode))
(add-to-list 'auto-mode-alist '("\\.hcr\\'" . hs-core-mode))

;; Setup key bindings
(add-hook
 'hs-mode-hook
 (lambda ()
   (interactive)
   ;; Bring up the interactive mode for this project.
   (define-key hs-mode-map (kbd "C-`") 'hs-mode-bring-interactive-mode)

   ;; Space after a symbol shows its info.
   (define-key hs-mode-map (kbd "SPC") 'hs-mode-space-info)
   (define-key hs-interactive-mode-map (kbd "SPC") 'hs-mode-space-info)

   ;; Insert language extensions.
   (define-key hs-mode-map (kbd "C-c e") 'hs-mode-insert-language-extension)

   ;; Build the current Cabal project.
   (define-key hs-mode-map (kbd "C-c C-c") 'hs-cabal-build-interactive)

   ;; Run a cabal command (prompting for which command).
   (define-key hs-mode-map (kbd "C-c c") 'hs-cabal-ido-interactive)

   ;; Run a script within the project directory.
   ;; E.g., define: (setq hs-config-scripts '("scripts/dothis" "scripts/dothat"))
   (define-key hs-mode-map (kbd "C-c t") 'hs-cabal-script-interactive)

   ;; Get the :type of the current symbol at point.
   (define-key hs-mode-map (kbd "C-c C-t") 'hs-process-type-of-interactive)

   ;; Display the :info of the current symbol at point.
   (define-key hs-mode-map (kbd "C-c C-i") 'hs-process-info-of-interactive)

   ;; Load the current file.
   (define-key hs-mode-map (kbd "<f5>")
     (lambda ()
       (interactive)
       (when (buffer-modified-p) (save-buffer))
       (hs-process-load-interactive)))

   ;; Save the current file, updating the etags file.
   (define-key hs-mode-map (kbd "\C-x\C-s")
     (lambda ()
       (interactive)
       (when (buffer-modified-p)
         (save-buffer) (hs-tags-generate-interactive))))

   ;; Go to the same column when hitting ret.
   (define-key hs-mode-map (kbd "<return>") 'hs-mode-newline-same-col)

   ;; Indent one level.
   (define-key hs-mode-map (kbd "C-<return>") 'hs-mode-newline-indent)

   ;; Move the code below the current nesting left one.
   (define-key hs-mode-map (kbd "C-<left>")
     (lambda () (interactive) (hs-move-nested -1)))

   ;; Move the code below the current nesting right one.
   (define-key hs-mode-map (kbd "C-<right>")
     (lambda () (interactive) (hs-move-nested 1)))

   ;; Useful editing features of paredit.
   (define-key hs-mode-map (kbd "\"") 'paredit-doublequote)
   (define-key hs-mode-map (kbd "[") 'paredit-open-square)
   (define-key hs-mode-map (kbd "(") 'paredit-open-round)
   (define-key hs-mode-map (kbd "]") 'paredit-close-square)
   (define-key hs-mode-map (kbd ")") 'paredit-close-round)
   (define-key hs-mode-map (kbd "{") 'paredit-open-curly)
   (define-key hs-mode-map (kbd "}") 'paredit-close-curly)
   (define-key hs-mode-map (kbd "M-(") 'paredit-wrap-round)
   (define-key hs-mode-map (kbd "DEL") 'paredit-backward-delete)
   (define-key hs-mode-map (kbd "C-k") 'paredit-kill)

   ;; Jump to the imports.
   (define-key hs-mode-map [f8] 'hs-navigate-imports)

   ;; Sort and re-align the import list.
   (define-key hs-mode-map (kbd "C-c C-.")
     (lambda ()
       (interactive)
       (let ((col (current-column)))
         (hs-sort-imports)
         (hs-align-imports)
         (goto-char (+ (line-beginning-position)
                       col)))))))

;; It's nice to have these globally defined so that you can
;; build/re-build/run scripts related to your project from anywhere.
(global-set-key (kbd "C-c t") 'hs-cabal-script-interactive)
(global-set-key (kbd "C-c C-c") 'hs-cabal-build-interactive)
(global-set-key (kbd "C-c c") 'hs-cabal-ido-interactive)
