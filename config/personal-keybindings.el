(bind-key "C-c C-c" 'comment-or-uncomment-current-line-or-region)
(bind-key "<f12>" 'magit-status)
(bind-key "C-c h" 'magit-log-buffer-file)

(bind-key "M-j" 'er/expand-region)
(bind-key "M-n" 'er/contract-region)

(bind-key "<f5>" 'compile)
(bind-key "C-c f" 'projectile-find-file)
(bind-key "C-c C-t" 'projectile-toggle-between-implementation-and-test)
(bind-key "C-c r" 'eval-and-replace)
(bind-key "C-c c" 'inf-ruby-console-auto)
(bind-key "C-c ag" 'ag)
(bind-key "C-c d" 'duplicate-current-line-or-region)
(bind-key "C-c '" 'toggle-quotes)
(bind-key "C-c o" (Λ (insert "save_and_open_page")))
(bind-key "C-c g" (Λ (insert "require 'factory_girl'; g = FactoryGirl; g.find_definitions")))
(bind-key "C-c t" (Λ (shell-command (concat
                                     "open dict://"
                                     (if (region-active-p)
                                         (buffer-substring (region-beginning) (region-end))
                                       (read-string "Lookup word: " (current-word)))))))

(bind-key "C-c z" 'zoom-window-zoom)

(bind-key "s-1" 'neotree-toggle)

(bind-key "s-b" 'robe-jump)

;; Start live html prototyping
(defun live-prototype (live-prototype-buffer-name)
  "Create a new html buffer to prototype with"
  (generate-new-buffer live-prototype-buffer-name)
  (switch-to-buffer live-prototype-buffer-name)
  (web-mode)
  (impatient-mode)
  (httpd-start-impatient live-prototype-buffer-name))

(defun increase-font-size ()
  (setq current-font-size (+ current-font-size 1))
  (set-default-font (format "Hack-%d" current-font-size))
  )

(bind-key "s-=" (Λ (increase-font-size)))

(defun decrease-font-size ()
  (setq current-font-size (- current-font-size 1))
  (set-default-font (format "Hack-%d" current-font-size))
  )

(bind-key "s--" (Λ (decrease-font-size)))

;(bind-key "s-<return>" (Λ (live-prototype (concat (read-string "Prototype buffer name: " "prototype")))))
(bind-key "s-<return>" (Λ (browse-url-at-point)))

(bind-key "s-\\" 'gptel)
;; No Problemo
(bind-key "C-c n p" (Λ (sa-play-sound (file-name-concat sounds-dir "no-problemo.mp3"))))

;; Affirmative
(bind-key "C-c y" (Λ (sa-play-sound (file-name-concat sounds-dir "t3_affirmative.wav"))))

;; Negative
(bind-key "C-c -" (Λ (sa-play-sound (file-name-concat sounds-dir "negative.mp3"))))

;; Let's do it
(bind-key "C-c !" (Λ (sa-play-sound (file-name-concat sounds-dir "lets-do-it.mp3"))))

;; Nice to meet u
(bind-key "C-c n m" (Λ (sa-play-sound (file-name-concat sounds-dir "nice-to-meet-u.mp3"))))

;; I've been waiting for you
(bind-key "C-c w" (Λ (sa-play-sound (file-name-concat sounds-dir "ive-been-waiting-for-you.ogg"))))

;; those files have been erased
(bind-key "C-c <backspace>" (Λ (sa-play-sound (file-name-concat sounds-dir "those-files-have-been-erased.ogg"))))

(require 'evil)
(bind-key "S-<up>" 'move-text-up evil-motion-state-map)
(bind-key "S-<down>" 'move-text-down evil-motion-state-map)

;; different jumps for different visual modes
(defadvice evil-visual-line (before spc-for-line-jump activate)
  (bind-key "SPC" 'evil-ace-jump-line-mode evil-motion-state-map))

(defadvice evil-visual-char (before spc-for-char-jump activate)
  (bind-key "SPC" 'evil-ace-jump-char-mode evil-motion-state-map))

(defadvice evil-visual-block (before spc-for-char-jump activate)
  (bind-key "SPC" 'evil-ace-jump-char-mode evil-motion-state-map))

(provide 'personal-keybindings)
