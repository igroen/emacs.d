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

(use-package helm
  :ensure t

  :bind (("M-x" . helm-M-x)
         ("C-x b" . helm-mini)
         ("C-x C-b" . helm-buffers-list)
         ("C-x C-f" . helm-find-files))

  ;; Fuzzy matching
  :init
  (defvar helm-M-x-fuzzy-match                  t)
  (defvar helm-bookmark-show-location           t)
  (defvar helm-buffers-fuzzy-matching           t)
  (defvar helm-completion-in-region-fuzzy-match t)
  (defvar helm-file-cache-fuzzy-match           t)
  (defvar helm-imenu-fuzzy-match                t)
  (defvar helm-mode-fuzzy-match                 t)
  (defvar helm-locate-fuzzy-match               t)
  (defvar helm-recentf-fuzzy-match              t)
  (defvar helm-semantic-fuzzy-match             t)
  (defvar helm-quick-update                     t)
  (defvar helm-split-window-inside-p            t)

  :config
  (use-package helm-ag
    :ensure t))

(use-package projectile
  :ensure t

  :diminish projectile-mode

  :bind-keymap ("C-c p" . projectile-command-map)

  :init
  (setq projectile-enable-caching t)
  (setq projectile-completion-system 'helm)
  ;; When running projectile-switch-project (C-c p p),
  ;; neotree will change root automatically.
  (setq projectile-switch-project-action 'neotree-projectile-action)

  :config
  ;; Enable projectile globally
  (projectile-mode)

  (use-package helm-projectile
    :ensure t

    :config (helm-projectile-on)))


(use-package epa
  :init
  ;; GnuPG: To use GnuPG gpg-agent must be running.
  ;; Prefer armored ASCII
  (setq epa-armor t)
  ;; Prompt for the password in the minibuffer
  (setq epa-pinentry-mode 'loopback))

(use-package better-defaults
  :ensure t

  ;; Dont't create auto-save-list folder in `user-emacs-directory`
  ;; Backups are placed in backups folder by better-defaults plugin
  :init (setq auto-save-list-file-prefix nil)

  ;; Fix for better-defaults `save-place' which was removed in emacs 25.1
  ;; https://github.com/technomancy/better-defaults/issues/15
  :config (save-place-mode t))

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

(use-package deft
  :ensure t

  :bind ("<f9>" . deft)

  :commands (deft))

;;; miscellaneous.el ends here
