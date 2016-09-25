;; list the packages you want
(setq package-list '(
    better-defaults
    magit
    projectile
    neotree
    helm
    elpy
    multiple-cursors
    expand-region
    virtualenvwrapper
    drag-stuff
    flymake-jslint
    flycheck
    py-autopep8
    markdown-mode
    yaml-mode
    go-mode
    )
)

;; list the repositories containing them
(setq package-archives
  '(("gnu" . "http://elpa.gnu.org/packages/")
    ("marmalade" . "https://marmalade-repo.org/packages/")
    ("melpa" . "http://melpa.org/packages/")))

;; activate all the packages (in particular autoloads)
(package-initialize)

;; fetch the list of packages available
(unless package-archive-contents
  (package-refresh-contents))

;; install the missing packages
(dolist (package package-list)
  (unless (package-installed-p package)
    (package-install package)))
