;; Packages to install
(setq package-list '(
    better-defaults
    cmake-ide
    cmake-mode
    company-irony
    company-irony-c-headers
    company-rtags
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
    helm-ag
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
    rtags
    tide
    yaml-mode
    yasnippet-snippets
    )
)

;; Set the repository locations
(setq package-archives
  '(("gnu" . "https://elpa.gnu.org/packages/")
    ("marmalade" . "https://marmalade-repo.org/packages/")
    ("melpa" . "https://melpa.org/packages/")))

;; Activate all the packages (in particular autoloads)
(package-initialize)

;; Fetch the list of packages available
(unless package-archive-contents
  (package-refresh-contents))

;; Install the missing packages
(dolist (package package-list)
  (unless (package-installed-p package)
    (package-install package)))
