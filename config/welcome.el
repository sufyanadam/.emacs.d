; Welcome message
(setq words-of-wisdom '(
                        ";; Let the hacking commence!"
                        ";; Hacks and glory await!"
                        ";; Hack and be merry!"
                        ";; Your hacking starts... NOW!"
                        ";; May the source be with you!"
                        (format ";; %s, this could be the start of a beautiful program." (capitalize (user-login-name)))
                        ))

(add-hook 'emacs-startup-hook
          (λ ()
            (when (string= (buffer-name) "*scratch*")
              (animate-string
               (eval (nth (% (random 10) (length words-of-wisdom)) words-of-wisdom))
               (/ (frame-height) 2))
              )))

(provide 'welcome)
