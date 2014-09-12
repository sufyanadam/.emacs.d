(bind-key "C-c C-c" 'comment-or-uncomment-current-line-or-region)
(bind-key "C-c C-g" 'magit-status)
(bind-key "C-c C-l" 'magit-file-log)

(bind-key "C-c C-f" 'projectile-find-file)
(bind-key "C-c C-t" 'projectile-toggle-between-implementation-and-test)

(provide 'personal-keybindings)
