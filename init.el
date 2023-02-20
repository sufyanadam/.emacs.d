;; Measure startup time
(defconst emacs-start-time (current-time))
(add-hook 'after-init-hook
	  `(lambda ()
	     (let ((elapsed (float-time (time-subtract (current-time)
						       emacs-start-time))))
	       (message "Loading %s...done (%.3fs) [after-init]"
			,load-file-name elapsed)))
	  t)

;; Setup load path
(setq quicklisp-dir "~/quicklisp")
(setq config-dir (expand-file-name (symbol-name 'config) user-emacs-directory))

(add-to-list 'load-path quicklisp-dir)
(add-to-list 'load-path (expand-file-name config-dir user-emacs-directory))
(add-to-list 'load-path (expand-file-name "emacs-wiki-packages/zoom-frm" user-emacs-directory))

;; Setup packages
(require 'setup-packages)

;; Use λ for lambda
(require 'real-lambda)

;; Setup appearance
(require 'appearance)

;; Are we on a mac?
(setq is-mac (equal system-type 'darwin))

;; Set default directory to home
(setq default-directory (f-full (getenv "HOME")))

;; Setup keyboard
(use-package keyboard-config)

;; Sane defaults
(use-package sane-defaults)

;; personal defuns
(use-package editing-defuns)
(use-package file-defuns)
(use-package folding)
(use-package lisp-defuns)
(use-package config-options)
(use-package personal-keybindings)
(use-package fun-defuns)

;;Setup shell
(use-package shell-settings)

(use-package zoom-frm
  :config
  (when window-system (maximize-frame)))

(use-package zoom-in-out)

;; Org mode config
(use-package org-config)

;; Setup tramp
(use-package tramp)

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

;; Space Jump!
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
            (add-to-list 'auto-mode-alist '("\\.js[x]?\\'" . web-mode))
            (add-to-list 'auto-mode-alist '("\\.ts[x]?\\'" . typescript-mode))
            (add-to-list 'auto-mode-alist '("\\.mjs?\\'" . js2-mode))
            (add-to-list 'auto-mode-alist '("\\.vue?\\'" . web-mode))
            (add-to-list 'auto-mode-alist '("\\.ts?\\'" . web-mode))
            (setq web-mode-content-types-alist
                  '(("jsx"  . "\\.js[x]?\\'"))
                  )
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

;; Dump jump mode
(use-package dumb-jump)

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
(use-package ido-completing-read+ 
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
    (require 'play-sound)
    (define-emms-simple-player afplay '(file)
      (regexp-opt '(".mp3" ".m4a" ".aac"))
      "afplay")
    (setq emms-player-list `(,emms-player-afplay))
    (emms-default-players)
    ))

(setq inferior-lisp-program "sbcl")

;; Packages
(use-package evil
  :config
  (evil-mode))

(use-package slime
  :config
  (slime-setup '(slime-fancy)))

(use-package emojify
  :config
  (global-emojify-mode)
  )

;; Settings for currently logged in user
(setq user-settings-dir
      (concat user-emacs-directory "users/" user-login-name))
(add-to-list 'load-path user-settings-dir)

(use-package welcome)

;; Show how long it took to load config
(let ((elapsed (float-time (time-subtract (current-time)
					  emacs-start-time))))
  (message "Loading %s...done (%.3fs)" load-file-name elapsed))

