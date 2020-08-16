;; zoom in/out like we do everywhere else.

(global-set-key (kbd "s-=") 'text-scale-increase)

(global-set-key (kbd "s--") 'text-scale-decrease)

(global-set-key (kbd "<s-wheel-down>") 'text-scale-decrease)

(global-set-key (kbd "<s-wheel-up>") 'text-scale-increase)

(provide 'zoom-in-out)
