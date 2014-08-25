;; Setup load path
(add-to-list 'load-path user-emacs-directory)
(setq defuns-dir (expand-file-name "defuns" user-emacs-directory))
(add-to-list 'load-path defuns-dir)

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

;; Settings for currently logged in user
(setq user-settings-dir
      (concat user-emacs-directory "users/" user-login-name))
(add-to-list 'load-path user-settings-dir)

;; Are we on a mac?
(setq is-mac (equal system-type 'darwin))

;; Sane defaults 
(require 'sane-defaults)

;; Setup display time
(setq display-time-24hr-format t)
(display-time)

;; Set default directory to home
(setq default-directory (f-full (getenv "HOME")))

;; Represent undo-history as an actual tree (visualize with C-x u)
(setq undo-tree-mode-lighter "")
(require 'undo-tree)
(global-undo-tree-mode)

;; Keep cursor away from edges when scrolling up/down
(require 'smooth-scrolling)

(when is-mac
  ;; Setup environment variables from the user's shell
  (require-package 'exec-path-from-shell)
  (exec-path-from-shell-initialize))
  
(autoload 'ansi-color-for-comint-mode-on "ansi-color" nil t)
(add-hook 'shell-mode-hook 'ansi-color-for-comint-mode-on)


;; Fill column indicator
(require 'fill-column-indicator)
(setq fci-rule-color "#111122")

(add-hook 'emacs-startup-hook
	  (λ ()
	    (when (string= (buffer-name) "*scratch*")
	      (animate-string (format ";; I am ready for you, master %s" (capitalize (user-login-name))) (/ (frame-height) 2)))))

;; Packages
(require 'use-package)

(use-package nyan-mode
             :init (nyan-mode 1))

(use-package nyan-prompt
             :config (add-hook 'eshell-load-hook 'nyan-prompt-enable))


;; Setup inf-ruby
(use-package inf-ruby
  :config (add-hook 'after-init-hook 'inf-ruby-switch-setup))

;; Projectile everywhere
(use-package projectile
  :commands projectile-global-mode
  :bind( 
        ("s-T" . projectile-toggle-between-implementation-and-test)
        ("s-O" . projectile-find-file)
        )
  )

;; Setup Ruby
(use-package enh-ruby-mode
  :config
  (progn
    (add-hook 'enh-ruby-mode-hook 'rvm-activate-corresponding-ruby)
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
         ("\\.rb\\$" . enh-ruby-mode))
  :interpreter "ruby")


(use-package web-mode
  :config (progn
            (use-package my-html-defuns)
            (add-to-list 'auto-mode-alist '("\\.erb\\'" . web-mode))
            (add-to-list 'auto-mode-alist '("\\.html?\\'" . web-mode))
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

(use-package js-mode
  :mode ("\\.json$" . js-mode)
  :config
  (progn
    (add-hook 'js-mode-hook (λ () (setq js-indent-level 2)))))

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

(use-package eshell
  :config
  (progn
    (add-hook 'eshell-first-time-mode-hook
              (lambda ()
                (add-to-list 'eshell-visual-commands "htop")))
    (setq eshell-history-size 5000)
    (setq eshell-save-history-on-exit t)))

(use-package httprepl)
(use-package ag)
(use-package git-gutter)
(use-package git-gutter-fringe)

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
             :init (flx-ido-mode 1))

(use-package ido-vertical-mode
             :init (ido-vertical-mode))

;; ido everywhere
(use-package ido-ubiquitous
  :init (ido-ubiquitous-mode 1))

(use-package ido-at-point
  :init (ido-at-point-mode)
  :bind ("C-," . completion-at-point))

(use-package smex
  :init (smex-initialize)
  :bind ("C-c x" . smex))

(use-package magit
  :config
  (progn
    ;; Subtler highlight
    (set-face-background 'magit-item-highlight "#121212")
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

    (require-package 'git-messenger)
    (global-set-key (kbd "C-x v p") #'git-messenger:popup-message)

    ;; Don't bother me with flyspell keybindings

    (eval-after-load "flyspell"
      '(define-key flyspell-mode-map (kbd "C-.") nil))

    )
  )

(use-package slime
  :config
  (progn
    (use-package slime-autoloads
      :commands slime-setup
      :config
      (progn
        (setq inferior-lisp-program "/usr/local/bin/sbcl")
        (setq slime-contribs '(slime-fancy))))))

;; personal defuns
(use-package editing-defuns)
(use-package file-defuns)
(use-package folding)
(use-package lisp-defuns)

(put 'erase-buffer 'disabled nil)
