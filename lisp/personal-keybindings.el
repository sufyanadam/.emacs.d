(bind-key "C-c C-c" 'comment-or-uncomment-current-line-or-region)
(bind-key "C-c C-g" 'magit-status)
(bind-key "C-c C-l" 'magit-file-log)

(bind-key "M-[" 'er/expand-region)
(bind-key "M-]" 'er/contract-region)

(bind-key "C-c f" 'projectile-find-file)
(bind-key "C-c C-t" 'projectile-toggle-between-implementation-and-test)
(bind-key "C-c r" 'eval-and-replace)
(bind-key "C-c c" 'inf-ruby-console-auto)
(bind-key "C-c ag" 'ag)
(bind-key "C-c d" 'duplicate-current-line-or-region)
(bind-key "C-c '" 'toggle-quotes)
(bind-key "C-c o" (Λ (insert "save_and_open_page")))
(bind-key "C-c g" (Λ (insert "require 'factory_girl'; g = FactoryGirl; g.find_definitions")))
(bind-key "C-c l" (Λ (shell-command (concat
                                     "open dict://"
                                     (if (region-active-p)
                                         (buffer-substring (region-beginning) (region-end))
                                       (read-string "Lookup word: " (current-word)))))))


;; different jumps for different visual modes
(defadvice evil-visual-line (before spc-for-line-jump activate)
  (bind-key "SPC" 'evil-ace-jump-line-mode evil-motion-state-map))

(defadvice evil-visual-char (before spc-for-char-jump activate)
  (bind-key "SPC" 'evil-ace-jump-char-mode evil-motion-state-map))

(defadvice evil-visual-block (before spc-for-char-jump activate)
  (bind-key "SPC" 'evil-ace-jump-char-mode evil-motion-state-map))

(provide 'personal-keybindings)
