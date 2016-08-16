(defconst emacs-start-time (current-time))

;; Setup load path
(setq config-dir (expand-file-name "lisp" user-emacs-directory))
(setq defuns-dir (expand-file-name "defuns" user-emacs-directory))
(setq quicklisp-dir "~/quicklisp")
(add-to-list 'load-path config-dir)
(add-to-list 'load-path defuns-dir)
(add-to-list 'load-path quicklisp-dir)

;; Settings for currently logged in user
(setq user-settings-dir
      (concat user-emacs-directory "users/" user-login-name))
(add-to-list 'load-path user-settings-dir)


;; ssh for tramp
(setq tramp-default-method "ssh")

;; Readable file sizes in dired
(setq dired-listing-switches "-alh")

;; Neotree find current file and jump to node
(setq neo-smart-open t)

;; Setup packages
(require 'setup-packages)

;; Setup appearance
(require 'appearance)

;; Use λ for lambda
(require 'real-lambda)

;; Use fn key as Hyper
(setq ns-function-modifier 'hyper)
(global-set-key (kbd "H-l") (Λ (insert "\u03bb")))
(global-set-key (kbd "H-L") (Λ (insert "\u039B")))

;; Toggle EVIL mode
(global-set-key (kbd "C-c e") (Λ (evil-mode 'toggle)))

;; Are we on a mac?
(setq is-mac (equal system-type 'darwin))

;; Sane defaults
(require 'sane-defaults)

;; Set default directory to home
(setq default-directory (f-full (getenv "HOME")))

(when is-mac
  ;; Setup environment variables from the user's shell
  (use-package exec-path-from-shell
    :init
    (exec-path-from-shell-initialize)))

(autoload 'ansi-color-for-comint-mode-on "ansi-color" nil t)
(add-hook 'shell-mode-hook 'ansi-color-for-comint-mode-on)

(setq words-of-wisdom '(
                        ";; Let the hacking commence!"
                        ";; Hacks and glory await!"
                        ";; Hack and be merry!"
                        ";; Your hacking starts... NOW!"
                        ";; May the source be with you!"
                        (format ";; %s, this could be the start of a beautiful program." (capitalize (user-login-name)))
                        ))

(add-hook 'emacs-startup-hook
          (λ ()
            (when (string= (buffer-name) "*scratch*")
              (animate-string
               (eval (nth (% (random 10) (length words-of-wisdom)) words-of-wisdom))
               (/ (frame-height) 2))
              )))

;; personal defuns
(use-package editing-defuns)
(use-package file-defuns)
(use-package folding)
(use-package lisp-defuns)
(use-package config-options)
(use-package personal-keybindings)

;; Packages
(use-package evil
  :config
  (evil-mode))

(use-package zoom-frm
  :config
  (when window-system (maximize-frame)))

(use-package neotree
  :config
  (add-hook 'neotree-mode-hook
  (λ ()
     (define-key evil-normal-state-local-map (kbd "TAB") 'neotree-enter)
     (define-key evil-normal-state-local-map (kbd "SPC") 'neotree-enter)
     (define-key evil-normal-state-local-map (kbd "q") 'neotree-hide)
     (define-key evil-normal-state-local-map (kbd "RET") 'neotree-enter))
  ))

;; Represent undo-history as an actual tree (visualize with C-x u)
(setq undo-tree-mode-lighter "")
(use-package undo-tree
  :config (global-undo-tree-mode))

(use-package ace-jump-mode
  :commands ace-jump-mode
  :init
  (progn
    (bind-key "SPC" 'ace-jump-mode evil-normal-state-map))
  (bind-key "C-c SPC" 'ace-jump-mode))

;; Setup inf-ruby
(use-package inf-ruby
  :config
  (add-hook 'after-init-hook 'inf-ruby-switch-setup))

;; Projectile everywhere
(use-package projectile
  :bind(
        ("s-T" . projectile-toggle-between-implementation-and-test)
        ("s-O" . projectile-find-file)
        ))

(use-package org-mode
  :mode (("\\.\\(org\\|org_archive\\|txt\\)$" . org-mode))
  :bind (
         ("C-c l" . org-store-link)
         ("C-c a" . org-agenda)
         ("C-c b" . org-iswitchb)
         )
  :init
  (progn
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
    ))

(org-babel-do-load-languages
 'org-babel-load-languages
 '(
   (sh . t)
   (python . t)
   (ruby . t)
   ))

;; Setup Ruby
(use-package enh-ruby-mode
  :config
  (progn
    (add-hook 'enh-ruby-mode-hook 'rvm-activate-corresponding-ruby)
    (add-to-list 'interpreter-mode-alist '("ruby" . enh-ruby-mode))
    (setq rinari-tags-file-name "TAGS")
    (setq ruby-deep-indent-paren nil)
    (use-package rvm
      :commands rvm-use-default)
    (use-package ruby-tools)
    (use-package rhtml-mode)
    (use-package web-mode
      :mode (("\\.rhtml$" . web-mode)
             ("\\.html\\.erb$" . web-mode)))
    (use-package rspec-mode
      :config
      (progn
        (setq rspec-use-rvm t)
        (defadvice rspec-compile (around rspec-compile-around activate)
          "Use BASH shel for running the specs"
          (let ((shell-file-name "/bin/bash"))
            ad-do-it))))
    )
  :mode (("\\.rake$" . enh-ruby-mode)
         ("\\.gemspec$" . enh-ruby-mode)
         ("\\.ru$" . enh-ruby-mode)
         ("Rakefile$" . enh-ruby-mode)
         ("Gemfile$" . enh-ruby-mode)
         ("Capfile$" . enh-ruby-mode)
         ("Guardfile$" . enh-ruby-mode)
         ("\\.rb$" . enh-ruby-mode))
  :interpreter "ruby")

(use-package web-mode
  :config (progn
            (use-package my-html-defuns)
            (add-to-list 'auto-mode-alist '("\\.erb\\'" . web-mode))
            (add-to-list 'auto-mode-alist '("\\.html?\\'" . web-mode))
            (add-to-list 'auto-mode-alist '("\\.jsx?\\'" . web-mode))
            (add-hook 'web-mode-hook
                      (lambda ()
                        (setq web-mode-style-padding 2)
                        (setq web-mode-script-padding 2)))))

(use-package smartparens
  :config
  (progn
    (use-package smartparens-config)
    (use-package smartparens-ruby)
    (use-package smartparens-html)
    (smartparens-global-mode 1)
    (show-smartparens-global-mode 1)
    (setq smartparens-strict-mode t)
    (setq sp-autoescape-string-quote nil)
    (setq sp-autoinsert-if-followed-by-word t)
    (sp-local-pair 'emacs-lisp-mode "`" nil :when '(sp-in-string-p))
    ))

(use-package css-mode
  :config (setq css-indent-offset 2))

(use-package jsx-mode
  :mode (("\\.jsx$" . jsx-mode)
         ("\\.jsx$" . web-mode))
  :config
  (progn
    (add-hook 'jsx-mode-hook (λ () setq jsx-indent-level 2))
    )
  )

(use-package js2-mode
  :mode (("\\.js$" . js2-mode)
         ("Jakefile$" . js2-mode))
  :interpreter ("node" . js2-mode)
  :config
  (progn
    (add-hook 'js2-mode-hook (λ () (setq js2-basic-offset 2)))))

(use-package coffee-mode
  :mode (("\\.coffee$" . coffee-mode)
         ("\\.coffee.erb$" . coffee-mode)
         ("Cakefile" . coffee-mode))
  :config
  (progn
    (add-hook 'coffee-mode-hook
              (λ ()
                (setq coffee-tab-width 2)))))

(use-package ert-async
  :config (add-to-list 'emacs-lisp-mode-hook 'ert-async-activate-font-lock-keywords))

(use-package emacs-lisp-mode
  :config
  (progn
    (use-package eldoc
      :init (add-hook 'emacs-lisp-mode-hook 'turn-on-eldoc-mode))
    (use-package macrostep
      :bind ("C-c e" . macrostep-expand))
    (use-package ert
      :config (add-to-list 'emacs-lisp-mode-hook 'ert--activate-font-lock-keywords)))
  :interpreter (("emacs" . emacs-lisp-mode))
  :mode ("Cask" . emacs-lisp-mode))

(use-package html-script-src)
(use-package haml-mode)
(use-package sass-mode)

(use-package httprepl)
(use-package ag)
(use-package git-gutter)
(use-package git-gutter-fringe)

(use-package eshell
  :config
  (progn
    (add-hook 'eshell-first-time-mode-hook
              (lambda ()
                (add-to-list 'eshell-visual-commands "htop")))
    (setq eshell-history-size 5000)
    (setq eshell-save-history-on-exit t)))

(use-package ido
  :init (ido-mode)
  :config
  (progn
    (setq ido-enable-prefix nil
          ido-enable-flex-matching t
          ido-case-fold nil
          ido-auto-merge-work-directories-length -1
          ido-create-new-buffer 'always
          ido-use-filename-at-point nil
          ido-max-prospects 10
          ido-use-faces nil)
    (add-to-list 'ido-ignore-files "\\.DS_Store")

    (defun my/setup-ido ()
      ;; Go straight home
      (define-key ido-file-completion-map
        (kbd "~")
        (Λ
         (cond
          ((looking-back "~/") (insert "projects/"))
          ((looking-back "/") (insert "~/"))
          (:else (call-interactively 'self-insert-command)))))

      ;; Use C-w to go back up a dir to better match normal usage of C-w
      ;; - insert current file name with C-x C-w instead.
      (define-key ido-file-completion-map (kbd "M-\d") 'ido-delete-backward-updir)
      (define-key ido-file-completion-map (kbd "C-x C-w") 'ido-copy-current-file-name)

      (define-key ido-file-dir-completion-map (kbd "M-\d") 'ido-delete-backward-updir)
      (define-key ido-file-dir-completion-map (kbd "C-x C-w") 'ido-copy-current-file-name))

    (add-hook 'ido-setup-hook 'my/setup-ido)
    ))

(use-package flx-ido
  :config (flx-ido-mode 1))

(use-package ido-vertical-mode
  :config
  (progn
    (setq ido-vertical-define-keys 'C-n-C-p-up-and-down)
    (ido-vertical-mode)
    ))

;; ido everywhere
(use-package ido-ubiquitous
  :config (ido-ubiquitous-mode 1))

(use-package ido-at-point
  :init (ido-at-point-mode)
  :bind ("C-," . completion-at-point))

(use-package smex
  :commands smex-initialize
  :bind ("C-c x" . smex))

(use-package magit
  :config
  (progn
    ;; Subtler highlight
    (set-face-background 'diff-file-header "#121212")
    (set-face-foreground 'diff-context "#666666")
    (set-face-foreground 'diff-added "#00cc33")
    (set-face-foreground 'diff-removed "#ff0000")

    (set-default 'magit-stage-all-confirm nil)
    (set-default 'magit-unstage-all-confirm nil)

    (eval-after-load 'ediff
      '(progn
         (set-face-foreground 'ediff-odd-diff-B "#ffffff")
         (set-face-background 'ediff-odd-diff-B "#292521")
         (set-face-foreground 'ediff-even-diff-B "#ffffff")
         (set-face-background 'ediff-even-diff-B "#292527")

         (set-face-foreground 'ediff-odd-diff-A "#ffffff")
         (set-face-background 'ediff-odd-diff-A "#292521")
         (set-face-foreground 'ediff-even-diff-A "#ffffff")
         (set-face-background 'ediff-even-diff-A "#292527")))
    (add-hook 'magit-mode-hook 'magit-load-config-extensions)
    (defun magit-save-and-exit-commit-mode ()
      (interactive)
      (save-buffer)
      (server-edit)
      (delete-window))

    (defun magit-exit-commit-mode ()
      (interactive)
      (kill-buffer)
      (delete-window))

    (eval-after-load "git-commit-mode"
      '(define-key git-commit-mode-map (kbd "C-c C-k") 'magit-exit-commit-mode))

    ;; C-c C-a to amend without any prompt

    (defun magit-just-amend ()
      (interactive)
      (save-window-excursion
        (magit-with-refresh
         (shell-command "git --no-pager commit --amend --reuse-message=HEAD"))))

    (eval-after-load "magit"
      '(define-key magit-status-mode-map (kbd "C-c C-a") 'magit-just-amend))

    ;; C-x C-k to kill file on line

    (defun magit-kill-file-on-line ()
      "Show file on current magit line and prompt for deletion."
      (interactive)
      (magit-visit-item)
      (delete-current-buffer-file)
      (magit-refresh))

    (define-key magit-status-mode-map (kbd "C-x C-k") 'magit-kill-file-on-line)

    ;; full screen magit-status

    (defadvice magit-status (around magit-fullscreen activate)
      (window-configuration-to-register :magit-fullscreen)
      ad-do-it
      (delete-other-windows))

    (defun magit-quit-session ()
      "Restores the previous window configuration and kills the magit buffer"
      (interactive)
      (kill-buffer)
      (jump-to-register :magit-fullscreen))

    (define-key magit-status-mode-map (kbd "q") 'magit-quit-session)

    ;; full screen vc-annotate

    (defun vc-annotate-quit ()
      "Restores the previous window configuration and kills the vc-annotate buffer"
      (interactive)
      (kill-buffer)
      (jump-to-register :vc-annotate-fullscreen))

    (eval-after-load "vc-annotate"
      '(progn
         (defadvice vc-annotate (around fullscreen activate)
           (window-configuration-to-register :vc-annotate-fullscreen)
           ad-do-it
           (delete-other-windows))

         (define-key vc-annotate-mode-map (kbd "q") 'vc-annotate-quit)))

    ;; ignore whitespace

    (defun magit-toggle-whitespace ()
      (interactive)
      (if (member "-w" magit-diff-options)
          (magit-dont-ignore-whitespace)
        (magit-ignore-whitespace)))

    (defun magit-ignore-whitespace ()
      (interactive)
      (add-to-list 'magit-diff-options "-w")
      (magit-refresh))

    (defun magit-dont-ignore-whitespace ()
      (interactive)
      (setq magit-diff-options (remove "-w" magit-diff-options))
      (magit-refresh))

    (define-key magit-status-mode-map (kbd "W") 'magit-toggle-whitespace)

    ;; Show blame for current line

    (use-package git-messenger
      :bind ("C-x v p" . git-messenger:popup-message))

    ;; Don't bother me with flyspell keybindings

    (eval-after-load "flyspell"
      '(define-key flyspell-mode-map (kbd "C-.") nil))

    )
  )

(use-package emms-setup
  :config
  (progn
    (emms-all)
    (emms-default-players)))

;; quicklisp
(if (file-exists-p
     (expand-file-name "slime-helper.el" quicklisp-dir))
    (progn
      (use-package slime-helper
        :commands slime
        :init
        (setq inferior-lisp-program "/usr/local/bin/sbcl"))))


(let ((elapsed (float-time (time-subtract (current-time)
					  emacs-start-time))))
  (message "Loading %s...done (%.3fs)" load-file-name elapsed))

(add-hook 'after-init-hook
	  `(lambda ()
	     (let ((elapsed (float-time (time-subtract (current-time)
						       emacs-start-time))))
	       (message "Loading %s...done (%.3fs) [after-init]"
			,load-file-name elapsed)))
	  t)
