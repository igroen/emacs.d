;;; miscellaneous.el --- Part of Iwan's Emacs configuration

;;; Commentary:
;;
;; This file is part of my default Emacs configuration.

;;; Code:

(use-package abbrev
  :diminish abbrev-mode)

(use-package eldoc
  :diminish eldoc-mode)

(use-package ido
  :init (setq ido-everywhere t)

  :config
  (ido-mode t)

  (use-package flx-ido
    :ensure t

    :init
    (setq ido-enable-flex-matching t)
    (setq ido-use-faces nil)

    :config (flx-ido-mode t)))

(use-package dired-x
  :init (setq-default dired-omit-files-p t)

  ;; Hide dot files in dired omit-mode (C-x M-o)
  :config (setq dired-omit-files (concat dired-omit-files "\\|^\\..+$")))

(use-package counsel
  :ensure t

  :bind
  ("M-x" . counsel-M-x)
  ("C-x C-f" . counsel-find-file))

(use-package ivy :demand
  :ensure t

  :diminish ivy-mode

  :bind ("<f6>" . ivy-resume)

  :init
  (setq ivy-use-virtual-buffers t
        ivy-count-format "%d/%d "
	ivy-initial-inputs-alist nil)

  :config
  (ivy-mode 1))

(use-package swiper
  :ensure t

  :bind
  ([remap isearch-forward]  . swiper)
  ([remap isearch-backward] . swiper))

(use-package projectile
  :ensure t

  :diminish projectile-mode

  :bind-keymap ("C-c p" . projectile-command-map)

  :init
  (setq projectile-enable-caching t)

  :config
  ;; Enable projectile globally
  (projectile-mode))

(use-package counsel-projectile
  :ensure t

  :hook (after-init . counsel-projectile-mode))

(use-package epa
  :init
  ;; GnuPG: To use GnuPG gpg-agent must be running.
  ;; Prefer armored ASCII
  (setq epa-armor t)
  ;; Prompt for the password in the minibuffer
  (setq epa-pinentry-mode 'loopback))

(use-package xclip
  :ensure t

  ;; Enable xclip-mode to use the system clipboard when killing/yanking
  ;; Install xclip on Linux for this to work. On OSX pbcopy/pbpaste will be used
  :config (xclip-mode t))

(use-package neotree
  :ensure t

  :bind ([f8] . neotree-toggle)

  :init
  ;; List of ignored files/directories
  (setq neo-hidden-regexp-list
        '("^\\."
          "^__pycache__$"
          "\\.pyc$"
          "\\.egg-info$"
          "~$"
          "^#.*#$"
          "\\.elc$")))

(use-package multiple-cursors
  :ensure t

  :bind (("C-x C-m C-e" . mc/edit-lines)
         ("C-x C-m C-n" . mc/mark-next-like-this)
         ("C-x C-m C-p" . mc/mark-previous-like-this)
         ("C-x C-m C-a" . mc/mark-all-like-this)))

(use-package magit
  :ensure t

  :bind ("C-x g" . magit-status))

(use-package git-timemachine
  :ensure t)

(use-package git-gutter
  :ensure t

  :diminish git-gutter-mode

  :config (global-git-gutter-mode t))

(use-package gist
  :ensure t

  :defer t

  ;; Ask for gist description when creating gist
  :init (setq gist-ask-for-description t))

(use-package expand-region
  :ensure t

  :bind ("C-x w" . er/expand-region))

(use-package drag-stuff
  :ensure t

  :bind (("M-p" . drag-stuff-up)
         ("M-n" . drag-stuff-down))

  :config (drag-stuff-global-mode 1))

(use-package yaml-mode
  :ensure t

  :defer t)

(use-package flycheck
  :ensure t

  :diminish flycheck-mode

  :hook (after-init . global-flycheck-mode)

  :config
  (use-package flycheck-yamllint
    :ensure t

    :defer t

    :hook (flycheck-mode . flycheck-yamllint-setup)))


(use-package company
  :diminish company-mode

  :hook (after-init . global-company-mode))

(use-package flymake
  :diminish flymake-mode)

(use-package yasnippet-snippets
  :ensure t

  :diminish yas-minor-mode

  :config (yas-global-mode))

(use-package try
  :ensure t)

(use-package which-key
  :ensure t

  :diminish which-key-mode

  :config (which-key-mode))

(use-package ace-window
  :ensure t

  :bind ([remap other-window] . ace-window))

;;; miscellaneous.el ends here
