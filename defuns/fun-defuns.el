(defun play-sound (filename)
  (emms-play-file (concat user-emacs-directory (concat "sounds/" filename))))
