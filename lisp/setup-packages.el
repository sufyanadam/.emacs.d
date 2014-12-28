(require 'auto-cask)
(auto-cask/setup)
(require 'use-package)
(require 'pallet)
(require 'package)

;; Add melpa to package repos
;;(add-to-list 'package-archives 
;;              '("melpa" . "http://melpa.milkbox.net/packages/") t)
;; (add-to-list 'package-archives
;;              '("org" . "http://orgmode.org/elpa/") t)

;; (package-initialize)

;; (unless (file-exists-p "~/.emacs.d/.cask/24.3.1/elpa/archives/melpa")
;;   (package-refresh-contents))

;; (defun packages-install (packages)
;;   (dolist (package packages)
;;     (when (not (package-installed-p package))
;;       (package-install package)))
;;   (delete-other-windows))

;; ;;; On-demand installation of packages

;; (defun require-package (package &optional min-version no-refresh)
;;   "Install given PACKAGE, optionally requiring MIN-VERSION.
;; If NO-REFRESH is non-nil, the available package lists will not be
;; re-downloaded in order to locate PACKAGE."
;;   (if (package-installed-p package min-version)
;;       t
;;     (if (or (assoc package package-archive-contents) no-refresh)
;;         (package-install package)
;;       (progn
;;         (package-refresh-contents)
;;         (require-package package min-version t)))))

(provide 'setup-packages)
