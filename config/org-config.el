;; Org mode
(use-package org-mode
  :mode (("\\.\\(org\\|org_archive\\|txt\\)$" . org-mode))
  :bind (
         ("C-c l" . org-store-link)
         ("C-c a" . org-agenda)
         ("C-c b" . org-iswitchb)
         )
  :init
  (progn
    (use-package org-tempo)
    (use-package org-bullets)
    (use-package org-tree-slide)
    (add-hook 'org-mode-hook (λ () (org-bullets-mode 1)))
    (setq org-log-done 'time)
    (setq org-log-into-drawer t)
    (setq org-use-sub-superscripts "{}")
    (setq org-log-state-notes-insert-after-drawers nil)
    (setq org-agenda-files '("~/Dropbox/org"))
    (setq org-todo-keywords
          '((sequence "TODO(t)" "NEXT(n)" "|" "DONE(d)")
            (sequence "WAITING(w@/!)" "HOLD(h@/!)" "|" "CANCELLED(c@/!)" "PHONE" "MEETING")))
    (setq org-todo-keyword-faces
          '(;("TODO" :foreground "red" :weight bold)
            ("NEXT" :foreground "#8CD0D3" :weight bold)
            ;("DONE" :foreground "forest green" :weight bold)
            ("WAITING" :foreground "orange" :weight bold)
            ("HOLD" :foreground "magenta" :weight bold)
            ("CANCELLED" :foreground "forest green" :weight bold)
            ("MEETING" :foreground "forest green" :weight bold)
            ("PHONE" :foreground "forest green" :weight bold)))
    (setq org-treat-S-cursor-todo-selection-as-state-change nil)
    (setq org-todo-state-tags-triggers
          '(("CANCELLED" ("CANCELLED" . t))
            ("WAITING" ("WAITING" . t))
            ("HOLD" ("WAITING") ("HOLD" . t))
            (done ("WAITING") ("HOLD"))
            ("TODO" ("WAITING") ("CANCELLED") ("HOLD"))
            ("NEXT" ("WAITING") ("CANCELLED") ("HOLD"))
            ("DONE" ("WAITING") ("CANCELLED") ("HOLD"))
            ))
    (setq org-directory "~/Dropbox/org/")
    (setq org-default-notes-file (concat org-directory "refile.org"))
    (global-set-key (kbd "<f8>") 'org-capture)

    ;; Capture templates for: TODO tasks, Notes, appointments, phone calls, meetings and org-protocol
    (setq org-capture-templates
          '(("t" "todo" entry (file org-default-notes-file)
             "* TODO %?\n%U\n%a\n" :clock-in t :clock-resume t)
            ("r" "respond" entry (file org-default-notes-file)
             "* NEXT Respond to %:from on %:subject\nSCHEDULED: %t\n%U\n%a\n" :clock-in t :clock-resume t :immediate-finish t)
            ("n" "note" entry (file org-default-notes-file)
             "* %? :NOTE:\n%U\n%a\n" :clock-in t :clock-resume t)
            ("w" "org-protocol" entry (file org-default-notes-file)
             "* TODO Review %c\n%U\n" :immediate-finish t)
            ("m" "Meeting" entry (file org-default-notes-file)
             "* MEETING with %? :MEETING:\n%U" :clock-in t :clock-resume t)
            ("p" "Phone call" entry (file org-default-notes-file)
             "* PHONE %? :PHONE:\n%U" :clock-in t :clock-resume t)
            ("i" "Interruption" entry (file org-default-notes-file)
             "* Interrupted by %? :INTERRUPTION:\n%U" :clock-in t :clock-resume t)
            ("h" "Habit" entry (file org-default-notes-file)
             ("* NEXT %?\n%U\n%a\nSCHEDULED: %(format-time-string \"<%Y-%m-%d %a .+1d/3d>\")\n:PROPERTIES:\n:STYLE: habit\n:REPEAT_TO_STATE: NEXT\n:END:\n"))
            ))
    ; Targets include this file and any file contributing to the agenda - up to 9 levels deep
    (setq org-refile-targets '((nil :maxlevel . 9)
                               (org-agenda-files :maxlevel . 9)))

    ; Use full outline paths for refile targets - we file directly with IDO
    (setq org-refile-use-outline-path t)

    ; Allow refile to create parent tasks with confirmation
    (setq org-refile-allow-creating-parent-nodes 'confirm)

    (setq org-completion-use-ido t)

    ; Exclude DONE state tasks from refile targets
    (setq org-refile-target-verify-function (λ ()
                                              (not (member (nth 2 (org-heading-components)) org-done-keywords))
                                              ))

    ; Clock settings
    ;; Resume clocking task when emacs is restarted
    (org-clock-persistence-insinuate)

    ;; Resume clocking task on clock-in if the clock is open
    (setq org-clock-in-resume t)
    (setq org-drawers '("PROPERTIES" "LOGBOOK"))
    (setq org-clock-into-drawer t)
    (setq org-clock-out-when-done t)
    ;; Save the running clock and all clock history when exiting Emacs, load it on startup
    (setq org-clock-persist t)
    (setq org-clock-persist-query-resume nil)
    (setq org-clock-auto-clock-resolution 'when-no-clock-is-running)
    (setq org-clock-report-include-clocking-task t)

    ;; Babel settings
    (setq org-src-preserve-indentation t)
    ))

(org-babel-do-load-languages
 'org-babel-load-languages
 '(
   (shell . t)
   (python . t)
   (ruby . t)
   (js . t)
   ))

(provide 'org-config)
