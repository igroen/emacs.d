;; Start with empty buffer
(setq inhibit-splash-screen t)
(switch-to-buffer "**")

;; Disable bell completely
(setq ring-bell-function 'ignore)

;; Use leuven-theme
(load-theme 'leuven)

;; Show the current column in  the mode line
(setq column-number-mode t)

;; Nice scrolling
(setq scroll-margin 0
      scroll-conservatively 100000
      scroll-preserve-screen-position 1)

;; Enable y/n answers
(fset 'yes-or-no-p 'y-or-n-p)

;; Autorefresh buffers on file change
(global-auto-revert-mode t)

;; Highlight the current line
(global-hl-line-mode t)

;; Remove trailing whitespace on save
(add-hook 'before-save-hook 'delete-trailing-whitespace)

;; Use version contral and keep multiple backup files
(setq delete-old-versions t
  kept-new-versions 6
  kept-old-versions 2
  version-control t)

;; Auto close bracket insertion.
(electric-pair-mode 1)

;; GnuPG: To use GnuPG gpg-agent must be running.
;; Prefer armored ASCII
(setq epa-armor t)
;; Prompt for the password in the minibuffer
(setq epa-pinentry-mode 'loopback)

;; Move lines added by the customize system to seperate file
;; Config changes made through the customize UI will be stored here
(setq custom-file (expand-file-name "custom.el" user-emacs-directory))

(when (file-exists-p custom-file)
  (load custom-file))
