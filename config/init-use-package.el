(require 'package)
(setq package-enable-at-startup nil)

;; Packages to install
(setq package-list
      '(bind-key
        diminish
        use-package))

;; Set the repository locations
(setq package-archives
      (append package-archives
              '(("gnu" . "https://elpa.gnu.org/packages/")
                ("marmalade" . "https://marmalade-repo.org/packages/")
                ("melpa" . "https://melpa.org/packages/"))))

;; Activate all the packages (in particular autoloads)
(package-initialize)

;; Fetch the list of packages available
(unless package-archive-contents
  (package-refresh-contents))

;; Install the missing packages
(dolist (package package-list)
  (unless (package-installed-p package)
    (package-install package)))

;; Enable use-package
(eval-when-compile
  (require 'use-package))
(require 'diminish)  ;; if you use :diminish
(require 'bind-key)  ;; if you use any :bind variant
