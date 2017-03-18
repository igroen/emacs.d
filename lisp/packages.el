;; list the packages you want
(setq package-list '(
    better-defaults
    magit
    gist
    projectile
    neotree
    helm
    flx-ido
    elpy
    multiple-cursors
    expand-region
    virtualenvwrapper
    drag-stuff
    flycheck
    py-autopep8
    py-isort
    markdown-mode
    yaml-mode
    )
)

;; list the repositories containing them
(setq package-archives
  '(("gnu" . "https://elpa.gnu.org/packages/")
    ("marmalade" . "https://marmalade-repo.org/packages/")
    ("melpa" . "https://melpa.org/packages/")))

;; activate all the packages (in particular autoloads)
(package-initialize)

;; fetch the list of packages available
(unless package-archive-contents
  (package-refresh-contents))

;; install the missing packages
(dolist (package package-list)
  (unless (package-installed-p package)
    (package-install package)))
