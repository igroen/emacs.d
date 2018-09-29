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

(load-library "defaults")

;; Initialize use-package
(load-library "init-use-package")

;; Snippets
;; * Comment/Uncomment line or region
;; * Delete current file and buffer
(load-library "snippets")

;; Setup python
(load-library "python-setup")

;; Setup c++
(load-library "c++-setup")

;; Setup org-mode
(load-library "org-mode-setup")

;; Setup miscellaneous libraries
(load-library "miscellaneous")

;;; init.el ends here
