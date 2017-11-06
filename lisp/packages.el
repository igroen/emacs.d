;; list the packages you want
(setq package-list '(
    better-defaults
    cmake-mode
    company-irony
    company-irony-c-headers
    drag-stuff
    elpy
    emmet-mode
    expand-region
    flx-ido
    flycheck
    flycheck-irony
    flycheck-yamllint
    gist
    git-gutter
    helm
    helm-projectile
    irony
    irony-eldoc
    json-mode
    magit
    markdown-mode
    multiple-cursors
    neotree
    ng2-mode
    projectile
    py-autopep8
    py-isort
    tide
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
