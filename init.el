;; Setup load path
(add-to-list 'load-path user-emacs-directory)

;; Setup packages
(require 'setup-packages)

;; Setup appearance
(require 'appearance)

(defalias 'λ 'lambda)

;; shorthand for interactive lambdas
(defmacro Λ (&rest body)
  `(λ ()
     (interactive)
     ,@body))

;; Use fn key as Hyper
(setq ns-function-modifier 'hyper)
(global-set-key (kbd "H-l") (Λ (insert "\u03bb")))
(global-set-key (kbd "H-L") (Λ (insert "\u039B")))

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

;; Functions (load all files in defuns-dir)
(setq defuns-dir (expand-file-name "defuns" user-emacs-directory))
(dolist (file (directory-files defuns-dir t "\\w+"))
  (when (file-regular-p file)
    (load file)))

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


;; ;; Setup inf-ruby
;; ;(autoload 'inf-ruby "inf-ruby" "Run an inferior Ruby process" t)
;; ;(add-hook 'ruby-mode-hook 'inf-ruby-minor-mode)
;; ;(add-hook 'after-init-hook 'inf-ruby-switch-setup)
(use-package inf-ruby
  :config (add-hook 'after-init-hook 'inf-ruby-switch-setup))

;; ;; Projectile everywhere
;; (projectile-global-mode)

(use-package projectile
  :init
  (progn
    (projectile-global-mode)))

;; Setup Ruby
(use-package ruby-mode
  :defer t
  :init
 (progn
    (use-package rvm
      :init (rvm-use-default))
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
            ad-do-it)))))
           
 :config
 (progn
   (add-hook 'ruby-mode-hook 'rvm-activate-corresponding-ruby)
   (setq ruby-deep-indent-paren nil))

 :mode (("\\.rake$" . ruby-mode)
        ("\\.gemspec$" . ruby-mode)
        ("\\.ru$" . ruby-mode)
        ("Rakefile$" . ruby-mode)
        ("Gemfile$" . ruby-mode)
        ("Capfile$" . ruby-mode)
        ("Guardfile$" . ruby-mode)
        ("\\.rb\\$" . ruby-mode))
 :interpreter "ruby")


(use-package web-mode
  :init (progn
          (add-to-list 'auto-mode-alist '("\\.erb\\'" . web-mode))
          (add-to-list 'auto-mode-alist '("\\.html?\\'" . web-mode)))
  :config (progn
            (add-hook 'web-mode-hook
                      (lambda ()
                        (setq web-mode-style-padding 2)
                        (setq web-mode-script-padding 2)))))

(use-package smarparens
  :init
  (progn
    (use-package smartparens-config)
    (use-package smartparens-ruby)
    (use-package smartparens-html)
    (smartparens-global-mode 1)
    (show-smartparens-global-mode 1))
  :config
  (progn
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
  :defer t
  :init
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
  :bind ("M-e" . eshell)
  :init
  (add-hook 'eshell-first-time-mode-hook
            (lambda ()
              (add-to-list 'eshell-visual-commands "htop")))
  :config
  (progn
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
               ))

(use-package flx-ido
             :init (flx-ido-mode 1))

(use-package ido-vertical-mode
             :init (ido-vertical-mode))

(use-package smex
  :init (smex-initialize)
  :bind ("M-x" . smex))










;; Setup coffee-mode
;(load "setup-coffee-mode.el")


;; Setup edbi
;(setenv "PERL5LIB" (concat "/Users/" user-login-name "/perl5/lib/perl5"))

;; Setup haml-mode
;(add-to-list 'auto-mode-alist '("\\.haml$" . haml-mode))

;; Setup php-mode
;(add-hook 'php-mode-hook
          ;; (lambda ()
          ;;   (setq indent-tabs-mode nil
          ;;         tab-width 2)))

;; Setup popwin
;(require 'popwin)
;(custom-set-variables
; '(display-buffer-function 'popwin:display-buffer))

;; Setup rcodetools
;(require 'rcodetools)

;; Setup rinari
;(require 'rinari)

;(add-hook 'ruby-mode-hook
          ;; (lambda ()
          ;;   (rvm-activate-corresponding-ruby)
          ;;   (ruby-electric-mode)
          ;;   (setq rinari-tags-file-name "TAGS")))

;(add-hook 'ruby-mode-hook 'robe-mode)
;(add-hook 'robe-mode-hook 'robe-start)

;; Setup skewer mode
;; (add-hook 'js2-mode-hook 'skewer-mode)
;; (add-hook 'css-mode-hook 'skewer-css-mode)
;; (add-hook 'html-mode-hook 'skewer-html-mode)

;; Setup slime
;;(load (expand-file-name "~/quicklisp/slime-helper.el"))
;;(setq inferior-lisp-program "sbcl")

;; (require 'ac-slime)
;; (add-hook 'slime-mode-hook 'set-up-slime-ac)
;; (add-hook 'slime-repl-mode-hook 'set-up-slime-ac)
;; (eval-after-load "auto-complete"
;;   '(add-to-list 'ac-modes 'slime-repl-mode))



;; guide-key
;(require 'guide-key)
;; (setq guide-key/guide-key-sequence '("C-x r" "C-x 4" "C-x v" "C-x 8" "C-x +"))
;; (guide-key-mode 1)
;; (setq guide-key/recursive-key-sequence-flag t)
;; (setq guide-key/popup-window-position 'bottom)

;; Setup extensions
;(eval-after-load 'ido '(require 'setup-ido))
;(require 'setup-org)
;;(eval-after-load 'dired '(require 'setup-dired))
;(eval-after-load 'magit '(require 'setup-magit))
;(eval-after-load 'grep '(require 'setup-rgrep))
;(eval-after-load 'shell '(require 'setup-shell))
;(require 'setup-hippie)
;(require 'setup-yasnippet)
;(require 'setup-perspective)
;(require 'setup-ffip)
;(require 'setup-html-mode)
;(require 'setup-paredit)

;; Font lock dash.el
;(eval-after-load "dash" '(dash-enable-font-lock))

;; Default setup of smartparens
;; (require 'smartparens-config)
;; (setq sp-autoescape-string-quote nil)
;; (--each '(css-mode-hook
;;           restclient-mode-hook
;;           js-mode-hook
;;           java-mode
;;           ruby-mode
;;           markdown-mode
;;           groovy-mode)
;;   (add-hook it 'turn-on-smartparens-mode))

;; Language specific setup files
;; (eval-after-load 'js2-mode '(require 'setup-js2-mode))
;; (eval-after-load 'ruby-mode '(require 'setup-ruby-mode))
;; (eval-after-load 'clojure-mode '(require 'setup-clojure-mode))
;; (eval-after-load 'markdown-mode '(require 'setup-markdown-mode))

;; Load stuff on demand
;; (autoload 'skewer-start "setup-skewer" nil t)
;; (autoload 'skewer-demo "setup-skewer" nil t)
;; (autoload 'flycheck-mode "setup-flycheck" nil t)
;; (autoload 'auto-complete-mode "auto-complete" nil t)

;; Map files to modes
;;(require 'mode-mappings)

;; Highlight escape sequences
;; (require 'highlight-escape-sequences)
;; (hes-mode)
;; (put 'font-lock-regexp-grouping-backslash 'face-alias 'font-lock-builtin-face)

;; Visual regexp
;; (require 'visual-regexp)
;; (define-key global-map (kbd "M-&") 'vr/query-replace)
;; (define-key global-map (kbd "M-/") 'vr/replace)

;; Functions (load all files in defuns-dir)
;; (setq defuns-dir (expand-file-name "defuns" user-emacs-directory))
;; (dolist (file (directory-files defuns-dir t "\\w+"))
;;   (when (file-regular-p file)
;;     (load file)))

;; Load reasonably default keybindings
;(load "key-bindings.el")

;; (require 'expand-region)
;; (require 'multiple-cursors)
;; (require 'delsel)
;; (require 'jump-char)
;; (require 'wgrep)
;; (require 'smart-forward)
;; (require 'change-inner)
;; (require 'multifiles)

;; Fill column indicator
;; (require 'fill-column-indicator)
;; (setq fci-rule-color "#111122")

;; Browse kill ring
;; (require 'browse-kill-ring)
;; (setq browse-kill-ring-quit-action 'save-and-restore)

;; ;; Smart M-x is smart
;; (require 'smex)
;; (smex-initialize)


;; Elisp go-to-definition with M-. and back again with M-,
;; (autoload 'elisp-slime-nav-mode "elisp-slime-nav")
;; (add-hook 'emacs-lisp-mode-hook (lambda () (elisp-slime-nav-mode t) (eldoc-mode 1)))

;(when is-mac (require 'mac))

;; Fix whitespace on save, but only if the file was clean
;(global-whitespace-cleanup-mode)

;(require 'multiple-cursors)

;; Show line numbers for all files
;(add-hook 'find-file-hook (lambda () (linum-mode 1)))

;; Emacs server
;(require 'server)
;(unless (server-running-p)
;  (server-start))

;; Conclude init by setting up specifics for the current user
;(when (file-exists-p user-settings-dir)
;  (mapc 'load (directory-files user-settings-dir nil "^[^#].*el$")))

