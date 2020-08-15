(defun sa-play-sound (filename)
  (emms-play-file (concat user-emacs-directory (concat "sounds/" filename))))

(provide 'fun-defuns)
