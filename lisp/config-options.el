;;Disable dialogs
(defadvice yes-or-no-p (around prevent-dialog activate)
  "Prevent yes-or-no-p from activating a dialog"
  (let ((use-dialog-box nil))
    ad-do-it))
(defadvice y-or-n-p (around prevent-dialog-yorn activate)
  "Prevent y-or-n-p from activating a dialog"
  (let ((use-dialog-box nil))
    ad-do-it))

;; Setup display time
(setq display-time-24hr-format t)
(display-time)

;; Keep cursor away from edges when scrolling up/down
(use-package smooth-scrolling)

;; Fill column indicator
(require 'fill-column-indicator)
(setq fci-rule-color "#111122")

;; Disable undo buffer warning
;;(add-to-list 'warning-suppress-types '(undo discard-info))

;; Enable erasing of buffer
(put 'erase-buffer 'disabled nil)

(projectile-global-mode 1)

;; TODO
;; bind smartparens surround mode with a more convenient keybinding
(provide 'config-options)
