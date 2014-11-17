(bind-key "C-c C-c" 'comment-or-uncomment-current-line-or-region)
(bind-key "C-c C-g" 'magit-status)
(bind-key "C-c C-l" 'magit-file-log)

(bind-key "M-[" 'er/contract-region)
(bind-key "M-]" 'er/expand-region)

(bind-key "C-c f" 'projectile-find-file)
(bind-key "C-c C-t" 'projectile-toggle-between-implementation-and-test)
(bind-key "C-c r" 'eval-and-replace)
(provide 'personal-keybindings)
