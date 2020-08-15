(when is-mac
  ;; Setup environment variables from the user's shell
  (use-package exec-path-from-shell
    :init
    (exec-path-from-shell-initialize)))

(autoload 'ansi-color-for-comint-mode-on "ansi-color" nil t)
(add-hook 'shell-mode-hook 'ansi-color-for-comint-mode-on)

(provide 'shell)
