;; Start with empty buffer
(setq inhibit-splash-screen t)
(switch-to-buffer "**")

;; Disable bell completely
(setq ring-bell-function 'ignore)

;; Use leuven-theme
(load-theme 'leuven t)

;; Show the current column in  the mode line
(setq column-number-mode t)

;; Scroll one line at a time
(setq scroll-step 1
      scroll-conservatively 10000)

;; Autorefresh buffers on file change
(global-auto-revert-mode t)

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
(setq custom-file "~/.emacs.d/custom.el")
(load custom-file 'noerror)
