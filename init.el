;; Start with empty buffer
(setq inhibit-splash-screen t)
(switch-to-buffer "**")

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

;; Dont't create auto-save-list folder in .emacs.d
;; Backups are placed in backups folder by better-defaults plugin
(setq auto-save-list-file-prefix nil)

;; Enable ido mode
(setq ido-enable-flex-matching t)
(setq ido-everywhere t)
(ido-mode 1)

;; Enable helm for M-x
(global-set-key (kbd "M-x") 'helm-M-x)

;; Fix for better-defaults `save-place' which was removed in emacs 25.1
;; https://github.com/technomancy/better-defaults/issues/15
(save-place-mode t)

;; Enable elpy (Run M-x elpy-config to configure)
(elpy-enable)

;; Disable flymake and enable flycheck for realtime syntax checking
(when (require 'flycheck nil t)
  (setq elpy-modules (delq 'elpy-module-flymake elpy-modules))
  (add-hook 'elpy-mode-hook 'flycheck-mode))

;; Enable autopep8 on save
(add-hook 'elpy-mode-hook 'py-autopep8-enable-on-save)

;; Org-mode
(setq org-log-done 'time)  ;; Show time when done

;; Syntax highlighting for org-mode
(setq org-src-fontify-natively 0)

;; Autorefresh buffers on file change
(global-auto-revert-mode t)

;; Remove trailing whitespace on save
(add-hook 'before-save-hook 'delete-trailing-whitespace)

;; Delete current file and buffer
(defun xah-delete-current-file-copy-to-kill-ring ()
  "Delete current buffer/file and close the buffer, push content to `kill-ring'.
URL `http://ergoemacs.org/emacs/elisp_delete-current-file.html'
Version 2016-07-20"
  (interactive)
  (progn
    (kill-new (buffer-string))
    (message "Buffer content copied to kill-ring.")
    (when (buffer-file-name)
      (when (file-exists-p (buffer-file-name))
        (progn
          (delete-file (buffer-file-name))
          (message "Deleted file: 「%s」." (buffer-file-name)))))
    (let ((buffer-offer-save nil))
      (set-buffer-modified-p nil)
      (kill-buffer (current-buffer)))))
(global-set-key (kbd "C-c k")  'xah-delete-current-file-copy-to-kill-ring)

;; Comment/Uncomment line or region
(defun comment-or-uncomment-region-or-line ()
  "Comments or uncomments the region or the current line if there's no active region."
  (interactive)
  (let (beg end)
    (if (region-active-p)
      (setq beg (region-beginning) end (region-end))
      (setq beg (line-beginning-position) end (line-end-position)))
    (comment-or-uncomment-region beg end)))
(global-set-key (kbd "C-x C-\\") 'comment-or-uncomment-region-or-line)

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

;; Disable bell completely
(setq ring-bell-function 'ignore)

;; Erc
(setq erc-server "irc.freenode.org")
(setq erc-port 6667)
(setq erc-nick "gumol")
(setq erc-password "***")
(setq erc-hide-list '("JOIN" "PART" "QUIT"))

;; Magit rules!
(global-set-key (kbd "C-x g") 'magit-status)

;; Expand region
(global-set-key (kbd "C-x w") 'er/expand-region)

;; virtualenvwrapper
(venv-initialize-interactive-shells)  ;; if you want interactive shell support
(venv-initialize-eshell)  ;; if you want eshell support
(setq venv-location "~/.virtualenvs/")

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

;; gofmt
(add-hook 'before-save-hook #'gofmt-before-save)

;; GnuPG: Open .asc files in the same way of .gpg
(setq epa-file-name-regexp "\\.\\(gpg\\|asc\\)$")
(epa-file-name-regexp-update)
(setq epa-armor t)  ;; Prefer armored ASCII

;; Move lines added by the customize system to seperate config file
(load "~/.emacs.d/custom.el")
