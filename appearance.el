(load-theme 'zenburn :no-confirm)

;; Coding font
(set-default-font "Consolas-16")
(add-to-list 'default-frame-alist '(font . "Consolas-16"))

;;(set-default-font "Anonymice Powerline-18")
;;(add-to-list 'default-frame-alist '(font . "Anonymice Powerline-18"))


(setq
 font-lock-maximum-decoration t
 color-theme-is-global t
 truncate-partial-width-windows nil)

;; Highlight current line
(global-hl-line-mode 1)

;; Don't defer screen updates when performing operations
(setq redisplay-dont-pause t)

;; Highlight matching parentheses when the point is on them.
(show-paren-mode 1)

(when window-system
  (setq frame-title-format '(buffer-file-name "%f" ("%b")))
  (tooltip-mode -1)
  (blink-cursor-mode -1))

(provide 'appearance)
