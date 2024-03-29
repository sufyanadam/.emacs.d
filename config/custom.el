(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(connection-local-criteria-alist
   '(((:application tramp :machine "sufyans-MBP.lan")
      tramp-connection-local-darwin-ps-profile)
     ((:application tramp :protocol "flatpak")
      tramp-flatpak-connection-local-default-profile)
     ((:application tramp :protocol "kubernetes")
      tramp-kubernetes-connection-local-default-profile)
     ((:application tramp :machine "localhost")
      tramp-connection-local-darwin-ps-profile)
     ((:application tramp :machine "sufyans-MacBook-Pro.local")
      tramp-connection-local-darwin-ps-profile)
     ((:application tramp)
      tramp-connection-local-default-system-profile
      tramp-connection-local-default-shell-profile)
     ((:application eshell) eshell-connection-default-profile)))
 '(connection-local-profile-alist
   '((tramp-flatpak-connection-local-default-profile
      (tramp-remote-path "/app/bin" tramp-default-remote-path "/bin"
                         "/usr/bin" "/sbin" "/usr/sbin"
                         "/usr/local/bin" "/usr/local/sbin"
                         "/local/bin" "/local/freeware/bin"
                         "/local/gnu/bin" "/usr/freeware/bin"
                         "/usr/pkg/bin" "/usr/contrib/bin" "/opt/bin"
                         "/opt/sbin" "/opt/local/bin"))
     (tramp-kubernetes-connection-local-default-profile
      (tramp-config-check . tramp-kubernetes--current-context-data)
      (tramp-extra-expand-args 97
                               (tramp-kubernetes--container
                                (car tramp-current-connection))
                               104
                               (tramp-kubernetes--pod
                                (car tramp-current-connection))
                               120
                               (tramp-kubernetes--context-namespace
                                (car tramp-current-connection))))
     (tramp-connection-local-darwin-ps-profile
      (tramp-process-attributes-ps-args "-acxww" "-o"
                                        "pid,uid,user,gid,comm=abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ"
                                        "-o" "state=abcde" "-o"
                                        "ppid,pgid,sess,tty,tpgid,minflt,majflt,time,pri,nice,vsz,rss,etime,pcpu,pmem,args")
      (tramp-process-attributes-ps-format (pid . number)
                                          (euid . number)
                                          (user . string)
                                          (egid . number) (comm . 52)
                                          (state . 5) (ppid . number)
                                          (pgrp . number)
                                          (sess . number)
                                          (ttname . string)
                                          (tpgid . number)
                                          (minflt . number)
                                          (majflt . number)
                                          (time . tramp-ps-time)
                                          (pri . number)
                                          (nice . number)
                                          (vsize . number)
                                          (rss . number)
                                          (etime . tramp-ps-time)
                                          (pcpu . number)
                                          (pmem . number) (args)))
     (tramp-connection-local-busybox-ps-profile
      (tramp-process-attributes-ps-args "-o"
                                        "pid,user,group,comm=abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ"
                                        "-o" "stat=abcde" "-o"
                                        "ppid,pgid,tty,time,nice,etime,args")
      (tramp-process-attributes-ps-format (pid . number)
                                          (user . string)
                                          (group . string) (comm . 52)
                                          (state . 5) (ppid . number)
                                          (pgrp . number)
                                          (ttname . string)
                                          (time . tramp-ps-time)
                                          (nice . number)
                                          (etime . tramp-ps-time)
                                          (args)))
     (tramp-connection-local-bsd-ps-profile
      (tramp-process-attributes-ps-args "-acxww" "-o"
                                        "pid,euid,user,egid,egroup,comm=abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ"
                                        "-o"
                                        "state,ppid,pgid,sid,tty,tpgid,minflt,majflt,time,pri,nice,vsz,rss,etimes,pcpu,pmem,args")
      (tramp-process-attributes-ps-format (pid . number)
                                          (euid . number)
                                          (user . string)
                                          (egid . number)
                                          (group . string) (comm . 52)
                                          (state . string)
                                          (ppid . number)
                                          (pgrp . number)
                                          (sess . number)
                                          (ttname . string)
                                          (tpgid . number)
                                          (minflt . number)
                                          (majflt . number)
                                          (time . tramp-ps-time)
                                          (pri . number)
                                          (nice . number)
                                          (vsize . number)
                                          (rss . number)
                                          (etime . number)
                                          (pcpu . number)
                                          (pmem . number) (args)))
     (tramp-connection-local-default-shell-profile
      (shell-file-name . "/bin/sh") (shell-command-switch . "-c"))
     (tramp-connection-local-default-system-profile
      (path-separator . ":") (null-device . "/dev/null"))
     (eshell-connection-default-profile (eshell-path-env-list))))
 '(custom-safe-themes
   '("97fd71cc2c7fb988234877248967b60a92424335f41b3a2469df782d38216f54"
     "9558f71c706fba7b136e75d9c5e73ddd2c9d91e76e2b18f733d4ab2f388f3b72"
     "f0ea6118d1414b24c2e4babdc8e252707727e7b4ff2e791129f240a2b3093e32"
     "3b819bba57a676edf6e4881bd38c777f96d1aa3b3b5bc21d8266fa5b0d0f1ebf"
     "2b5aa66b7d5be41b18cc67f3286ae664134b95ccc4a86c9339c886dfd736132d"
     "7fbb8d064286706fb1e319c9d3c0a8eafc2efe6b19380aae9734c228b05350ae"
     default))
 '(enh-ruby-bounce-deep-indent nil)
 '(enh-ruby-deep-arglist nil)
 '(enh-ruby-deep-indent-paren nil)
 '(enh-ruby-hanging-brace-deep-indent-level 0)
 '(enh-ruby-hanging-brace-indent-level 2)
 '(indent-tabs-mode nil)
 '(javascript-indent-level 2)
 '(js-curly-indent-offset 0)
 '(js-expr-indent-offset 0)
 '(js-indent-level 2)
 '(js-paren-indent-offset 0)
 '(js-square-indent-offset 0)
 '(js-switch-indent-offset 0)
 '(js2-basic-offset 2)
 '(js2-highlight-level 3)
 '(js2-missing-semi-one-line-override t)
 '(js2-strict-missing-semi-warning nil)
 '(lua-indent-level 2)
 '(lua-indent-string-contents t)
 '(org-agenda-files '("~/stessa-notes.org" "~/Dropbox/org"))
 '(org-export-backends '(ascii html icalendar latex md odt))
 '(org-fontify-done-headline t)
 '(org-fontify-whole-heading-line t)
 '(org-hide-emphasis-markers t)
 '(org-hide-leading-stars t)
 '(org-pretty-entities t)
 '(org-src-fontify-natively t)
 '(package-selected-packages
   '(ac-inf-ruby ac-js2 ac-slime ace-jump-mode afternoon-theme ag
                 ample-theme ample-zen-theme babel babel-repl
                 bash-completion blamer browse-kill-ring change-inner
                 cider coffee-mode color-theme-sanityinc-tomorrow
                 company-tern css-eldoc csv-mode dash-at-point demo-it
                 dir-treeview direx dockerfile-mode dumb-jump edbi
                 edit-indirect elisp-slime-nav emms
                 emms-player-simple-mpv emojify enh-ruby-mode eproject
                 ert-async evil-mc evil-smartparens
                 exec-path-from-shell exwm exwm-edit exwm-firefox-core
                 exwm-firefox-evil exwm-mff exwm-surf exwm-x
                 fancy-narrow feature-mode fill-column-indicator
                 find-file-in-project flx-ido flycheck fold-this
                 geiser gist git-gutter-fringe git-messenger
                 gitconfig-mode gitignore-mode god-mode gptel
                 guide-key highlight-escape-sequences
                 highlight-indentation html-script-src
                 html-to-markdown httprepl ido-at-point
                 ido-completing-read+ ido-ubiquitous ido-vertical-mode
                 idomenu impatient-mode import-js jabber js2-refactor
                 jsx-mode jump-char lsp-vue lua-mode magit
                 markdown-mode markdown-preview-mode monokai-theme
                 move-text multifiles naquadah-theme neotree
                 nodejs-repl org-beautify-theme org-bullets
                 org-plus-contrib org-present org-tree-slide pallet
                 pandoc paredit parse-csv pdf-tools prettier-js
                 pretty-symbols project-explorer projectile
                 rainbow-mode rbenv restclient rhtml-mode rinari robe
                 rsense rspec-mode ruby-tools rvm sass-mode scss-mode
                 simplezen skewer-less slim-mode slime-repl-ansi-color
                 smart-forward smex smooth-scrolling spacegray-theme
                 sublime-themes swift-mode tagedit tern-auto-complete
                 terraform-mode tide tree-sitter treemacs
                 treemacs-evil treemacs-icons-dired treemacs-magit
                 treemacs-persp treemacs-projectile treemacs-tab-bar
                 ts typescript-mode use-package visual-regexp-steroids
                 vue-mode wallpaper web-mode wgrep
                 whitespace-cleanup-mode window-numbering yaml-mode
                 zenburn-theme zoom zoom-frm zoom-window))
 '(python-indent-offset 2)
 '(ruby-deep-indent-paren nil)
 '(scss-compile-at-save nil)
 '(sh-basic-offset 2)
 '(sh-indentation 2)
 '(swift-mode:basic-offset 2)
 '(swift3-mode:basic-offset 2)
 '(tab-width 2)
 '(web-mode-attr-indent-offset 2)
 '(web-mode-attr-value-indent-offset 2)
 '(web-mode-code-indent-offset 2)
 '(web-mode-css-indent-offset 2)
 '(web-mode-enable-auto-closing t)
 '(web-mode-enable-auto-indentation t)
 '(web-mode-markup-indent-offset 2)
 '(web-mode-script-padding 2)
 '(web-mode-sql-indent-offset 2)
 '(web-mode-style-padding 2))
 '(custom-safe-themes (quote ("c7359bd375132044fe993562dfa736ae79efc620f68bab36bd686430c980df1c" "53e29ea3d0251198924328fd943d6ead860e9f47af8d22f0b764d11168455a8e" "0ebe0307942b6e159ab794f90a074935a18c3c688b526a2035d14db1214cf69c" "33c5a452a4095f7e4f6746b66f322ef6da0e770b76c0ed98a438e76c497040bb" "ce79400f46bd76bebeba655465f9eadf60c477bd671cbcd091fe871d58002a88" "3b819bba57a676edf6e4881bd38c777f96d1aa3b3b5bc21d8266fa5b0d0f1ebf" "dd43c9f997208c61ce0f4855932cc20a57ae2f37fe2ced218dace5c8c321d1e8" "628278136f88aa1a151bb2d6c8a86bf2b7631fbea5f0f76cba2a0079cd910f7d" default)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(default ((t (:background nil))))
 '(js2-private-function-call ((t (:foreground "goldenrod")))))
