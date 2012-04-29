(require 'org-install)
(require 'org-contacts)

(add-to-list 'auto-mode-alist '("\\.org\\'" . org-mode))
(global-set-key "\C-cl" 'org-store-link)
(global-set-key "\C-ca" 'org-agenda)
(global-set-key "\C-cc" 'org-capture)
(global-font-lock-mode 1)

(setq org-directory "~/org/")

(setq org-agenda-files (list (concat org-directory "agenda/"))
      org-default-notes-file (concat org-directory "notes.org")
      org-contacts-files (concat org-directory "contacts.org"))

(setq org-refile-targets '((buffer-file-name :maxlevel . 3)
                           (org-agenda-files :maxlevel . 2)
                           )
      org-refile-use-outline-path 'file)

(setq org-capture-templates
      '(("t" "Todo" entry (file (format "%s/agenda/todo.org" org-directory))
         "* TODO %?\n  %i\n  %a")
        ("m" "Meeting" entry (file (format "%s/agenda/meetings.org" org-directory))
         "* %?\n %i\n  %a")
        ("x" "Note with Clipboard" entry (file (format "%s/notes.org" org-directory))
         "* %?\n  %i\n  %x")
        ("n" "Note" entry (file (format "%s/notes.org" org-directory))
         "* %?\n  %i\n  %a")
        ;; dedicated templates
        ("s" "Save link for reading" entry (file+headline
                                            (format "%s/links.org" org-directory)
                                            "Unsorted")
         "* %:description\n  %:link\n  %U"
         )
        ("c" "Contacts" entry (file (concat org-directory "contacts.org"))
         "* %(org-contacts-template-name)
:PROPERTIES:
:EMAIL: %(org-contacts-template-email)
:END:")
        ))

(defvar org-agenda-mode-map (make-sparse-keymap)
   "Keymap for `org-agenda-mode'.")

(defun my/agenda-frame ()
  (modify-frame-parameters nil '( (name . "Agenda Frame")
                                  (instance . "agenda-frame")))
  (if (fboundp 'x-focus-frame)
      (x-focus-frame nil))
  (let ((org-agenda-window-setup 'current-window))
    (make-variable-frame-local 'org-agenda-mode-map)
    (org-agenda-list)))

(provide 'my-org-mode)
