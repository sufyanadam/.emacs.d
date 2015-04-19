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

(defadvice evil-mode (before say-cheater activate)
  (if (not (bound-and-true-p evil-mode))
      (emms-play-file (concat user-emacs-directory "sounds/cheater.wav"))))

;; Prevent magit release notes make a choice msg
;; from showing up
(setq magit-last-seen-setup-instructions "1.4.0")


;; TODO
;; bind smartparens surround mode with a more convenient keybinding
(provide 'config-options)
