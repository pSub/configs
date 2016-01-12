(require 'org-install)
(require 'org-contacts)
(require 'org-habit)
;(require 'org-journal)
(require 'diary-lib)

(setq solar-n-hemi-seasons
      '("Frühlingsanfang" "Sommeranfang" "Herbstanfang" "Winteranfang"))

(setq holiday-general-holidays
      '((holiday-fixed 1 1 "Neujahr")
        (holiday-fixed 5 1 "1. Mai")
        (holiday-fixed 10 3 "Tag der Deutschen Einheit")))

;; Feiertage für Bayern, weitere auskommentiert
(setq holiday-christian-holidays
      '((holiday-float 12 0 -4 "1. Advent" 24)
        (holiday-float 12 0 -3 "2. Advent" 24)
        (holiday-float 12 0 -2 "3. Advent" 24)
        (holiday-float 12 0 -1 "4. Advent" 24)
        (holiday-fixed 12 25 "1. Weihnachtstag")
        (holiday-fixed 12 26 "2. Weihnachtstag")
        (holiday-fixed 1 6 "Heilige Drei Könige")
        (holiday-easter-etc -48 "Rosenmontag")
        ;; (holiday-easter-etc -3 "Gründonnerstag")
        (holiday-easter-etc  -2 "Karfreitag")
        (holiday-easter-etc   0 "Ostersonntag")
        (holiday-easter-etc  +1 "Ostermontag")
        (holiday-easter-etc +39 "Christi Himmelfahrt")
        (holiday-easter-etc +49 "Pfingstsonntag")
        (holiday-easter-etc +50 "Pfingstmontag")
        (holiday-easter-etc +60 "Fronleichnam")
        (holiday-fixed 8 15 "Mariae Himmelfahrt")
        (holiday-fixed 11 1 "Allerheiligen")
        ;; (holiday-float 11 3 1 "Buss- und Bettag" 16)
        (holiday-float 11 0 1 "Totensonntag" 20)))

(add-to-list 'auto-mode-alist '("\\.org\\'" . org-mode))
(global-set-key "\C-cl" 'org-store-link)
(global-set-key "\C-ca" 'org-agenda)
(global-set-key "\C-cc" 'org-capture)
(global-set-key "\C-cb" 'org-iswitchb)
(global-font-lock-mode 1)

;; Resume clocking tasks when emacs is restarted
(org-clock-persistence-insinuate)

(setq org-directory "~/org/")

(setq org-hide-leading-stars 'hidestars
      org-completion-use-ido t
      org-log-done 'time

      ;; Save clock data and notes in a separate drawer
      org-clock-into-drawer "CLOCK"
      org-clock-history-length 35
      org-clock-in-resume t
      org-clock-persist t
      org-agenda-start-with-clockreport-mode t
      org-agenda-window-setup 'current-window
      org-agenda-clockreport-parameter-plist (quote (:link t :maxlevel 5 :fileskip0 t :compact t))
      org-agenda-sticky t
      )

(setq org-agenda-files (list (concat org-directory "agenda/meetings.org")
                             (concat org-directory "agenda/todo.org"))
;;                             (concat org-directory "agenda/habits.org"))
      org-default-notes-file (concat org-directory "notes.org")
      org-contacts-files (list (concat org-directory "contacts.org"))
      org-journal-dir (concat org-directory "journal/"))

(setq org-refile-targets '((buffer-file-name :maxlevel . 3)
                           (org-agenda-files :maxlevel . 2)
                           )
      org-refile-use-outline-path 'file)

(add-hook 'org-agenda-mode-hook '(lambda () (hl-line-mode 1 )))

; Erase all reminders and rebuilt reminders for today from the agenda
(defun my/org-agenda-to-appt ()
  (interactive)
  (setq appt-time-msg-list nil)
  (org-agenda-to-appt))

; Rebuild the reminders everytime the agenda is displayed
(add-hook 'org-finalize-agenda-hook 'my/org-agenda-to-appt 'append)

(my/org-agenda-to-appt)

; Activate appointments so we get notifications
(appt-activate t)

; If we leave Emacs running overnight - reset the appointments one minute after midnight
(run-at-time "24:01" nil 'my/org-agenda-to-appt)

(setq org-capture-templates
      '(("t" "Todo" entry (file (format "%s/agenda/todo.org" org-directory))
         "* TODO %?\n  %i\n  %a")
        ("T" "Thesis" entry (file (format "%s/agenda/thesis.org" org-directory))
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
         "*
:PROPERTIES:
:EMAIL:
:URL:
:WORK:
:HOME:
:MOBILE:
:LOCATION:
:BIRTHDAY:
:NOTE:
:END:")
        ))

(defvar org-agenda-mode-map (make-sparse-keymap)
   "Keymap for `org-agenda-mode'.")

(defun my/agenda-frame ()
  (if (fboundp 'x-focus-frame)
      (x-focus-frame nil))
  (let ((org-agenda-window-setup 'current-window))
    (make-variable-frame-local 'org-agenda-mode-map)
    (org-agenda-list)))

(provide 'my-org-mode)
