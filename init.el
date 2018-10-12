;;; init.el --- Part of Iwan's Emacs configuration

;;; Commentary:
;;
;; This file is part of my default Emacs configuration.

;;; Code:

;; Set load-path for libraries
(add-to-list
 'load-path
 (expand-file-name "lisp" user-emacs-directory))

(load-library "tweak-startup-time")

;; Initialize use-package
(load-library "init-use-package")

(org-babel-load-file (expand-file-name "settings.org" user-emacs-directory))

;;; init.el ends here
