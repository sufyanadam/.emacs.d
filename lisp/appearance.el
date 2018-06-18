(load-theme 'zenburn :no-confirm)

;; Coding font
(setq current-font-size 18)
(defun current-font ()
  (format "Hack-%d" current-font-size)
  )
(set-default-font (current-font))
(add-to-list 'default-frame-alist `(font . ,(current-font)))

;(set-default-font "Anonymice Powerline-18")
;(add-to-list 'default-frame-alist '(font . "Anonymice Powerline-18"))

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

;; Fix compilation buffer
(require 'ansi-color)
(defun colorize-compilation-buffer ()
  (toggle-read-only)
  (ansi-color-apply-on-region (point-min) (point-max))
  (toggle-read-only))
(add-hook 'compilation-filter-hook 'colorize-compilation-buffer)
(setq compilation-scroll-output t)

(when window-system
  (setq frame-title-format '(buffer-file-name "%f" ("%b")))
  (tooltip-mode -1)
  (blink-cursor-mode -1))

(provide 'appearance)
