;; Set load-path for libraries
(add-to-list 'load-path "~/.emacs.d/lisp/")

;; Start with empty buffer
(setq inhibit-splash-screen t)
(switch-to-buffer "**")

;; Disable bell completely
(setq ring-bell-function 'ignore)

;; Install and initialize packages
;; (package-initialize)
(load-library "packages")

;; Snippets
;; * Comment/Uncomment line or region
;; * Delete current file and buffer
(load-library "snippets")

;; Setup python
(load-library "python-setup")

;; Dont't create auto-save-list folder in .emacs.d
;; Backups are placed in backups folder by better-defaults plugin
(setq auto-save-list-file-prefix nil)

;; Enable ido mode and flx-ido
(ido-mode 1)
(setq ido-everywhere 1)
(flx-ido-mode 1)
(setq ido-enable-flex-matching 1)
(setq ido-use-faces nil)

;; Hide dot files in dired omit-mode (C-x M-o)
(setq dired-omit-files "^\\...+$")

;; Enable helm for M-x
(global-set-key (kbd "M-x")     'helm-M-x)
(global-set-key (kbd "C-x b")   'helm-mini)
(global-set-key (kbd "C-x C-b") 'helm-buffers-list)

;; Helm fuzzy matching
(setq helm-M-x-fuzzy-match                  t
      helm-bookmark-show-location           t
      helm-buffers-fuzzy-matching           t
      helm-completion-in-region-fuzzy-match t
      helm-file-cache-fuzzy-match           t
      helm-imenu-fuzzy-match                t
      helm-mode-fuzzy-match                 t
      helm-locate-fuzzy-match               t
      helm-recentf-fuzzy-match              t
      helm-semantic-fuzzy-match             t
      helm-quick-update                     t
      helm-split-window-in-side-p           t)

;; Fix for better-defaults `save-place' which was removed in emacs 25.1
;; https://github.com/technomancy/better-defaults/issues/15
(save-place-mode t)

;; Org-mode
(setq org-log-done 'time)  ;; Show time when done

;; Syntax highlighting for org-mode
(setq org-src-fontify-natively 0)

;; Autorefresh buffers on file change
(global-auto-revert-mode t)

;; Remove trailing whitespace on save
(add-hook 'before-save-hook 'delete-trailing-whitespace)

;; Enable projectile globally
(projectile-global-mode)
(setq projectile-enable-caching t)

;; Configure Neo Tree
(global-set-key [f8] 'neotree-toggle)

;; When running ‘projectile-switch-project’ (C-c p p),
;; ‘neotree’ will change root automatically.
(setq projectile-switch-project-action 'neotree-projectile-action)

;; Auto close bracket insertion. New in emacs 24
(electric-pair-mode 1)

;; Multiple cursors
(global-set-key (kbd "C-x C-m C-e") 'mc/edit-lines)
(global-set-key (kbd "C-x C-m C-n") 'mc/mark-next-like-this)
(global-set-key (kbd "C-x C-m C-p") 'mc/mark-previous-like-this)
(global-set-key (kbd "C-x C-m C-a") 'mc/mark-all-like-this)

;; Erc
(setq erc-server "irc.freenode.net")
(setq erc-autojoin-channels-alist
      '(("freenode.net" "#python" "#django" "#emacs")))
(setq erc-port 6697)
(setq erc-nick "gumol")
(setq erc-hide-list '("JOIN" "PART" "QUIT"))

;; Magit rules!
(global-set-key (kbd "C-x g") 'magit-status)

;; Ask for gist description when creating gist
(setq gist-ask-for-description t)

;; Expand region
(global-set-key (kbd "C-x w") 'er/expand-region)

;; drag-stuff
(drag-stuff-global-mode 1)
;; Fix for M-up/M-down not working on os x (Meta key doesn't work with symbols)
(define-key input-decode-map "\e\eOA" [(meta up)])
(define-key input-decode-map "\e\eOB" [(meta down)])
(global-set-key (kbd "M-n") 'drag-stuff-down)
(global-set-key (kbd "M-p") 'drag-stuff-up)

;; jslint must be installed: sudo npm install -g jslint
(add-hook 'js-mode-hook 'flymake-jslint-load)

;; Major mode to use for file extension
(add-to-list 'auto-mode-alist '("\\.zcml\\'" . nxml-mode))
(add-to-list 'auto-mode-alist '("\\.pt\\'" . html-mode))

;; GnuPG: To use GnuPG gpg-agent must be running.
;; Open .asc files in the same way of .gpg
(setq epa-file-name-regexp "\\.\\(gpg\\|asc\\)$")
(epa-file-name-regexp-update)
(setq epa-armor t)  ;; Prefer armored ASCII

;; Move lines added by the customize system to seperate file
(setq custom-file "~/.emacs.d/custom.el")
(load custom-file)
