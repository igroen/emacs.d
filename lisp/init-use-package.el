;;; init-use-package.el --- Part of Iwan's Emacs configuration

;;; Commentary:
;;
;; This file is part of my default Emacs configuration.

;;; Code:

(require 'package)
(setq package-enable-at-startup nil)

;; Packages to install
(defvar package-list
  '(bind-key
    diminish
    use-package))

;; Set the repository locations
(add-to-list 'package-archives
            '("melpa" . "https://melpa.org/packages/"))

;; Activate all the packages (in particular autoloads)
(package-initialize)

;; Fetch the list of packages available
(unless package-archive-contents
  (package-refresh-contents))

;; Install the missing packages
(dolist (package package-list)
  (unless (package-installed-p package)
    (package-install package)))

;; Enable use-package and friends
(eval-when-compile
  (require 'use-package))
(require 'diminish)
(require 'bind-key)

;;; init-use-package.el ends here
