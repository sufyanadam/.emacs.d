;; Toggle EVIL mode
(global-set-key (kbd "C-c e") (Λ (evil-mode 'toggle)))

;; Use fn key as Hyper
(setq ns-function-modifier 'hyper)
(global-set-key (kbd "H-l") (Λ (insert "\u03bb")))
(global-set-key (kbd "H-L") (Λ (insert "\u039B")))

(provide 'keyboard-config)
