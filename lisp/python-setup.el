;; Enable elpy (Run M-x elpy-config to configure)
(elpy-enable)
(global-set-key (kbd "M-*") 'pop-tag-mark)

;; Elpy seems partially incompatible with Emacs 25's 'native completion' feature
;; https://github.com/jorgenschaefer/elpy/issues/887
(setq python-shell-completion-native-enable nil)

;; Autocomplet doesn't work with Rope as backend. Use Jedi instead
(setq elpy-rpc-backend "jedi")

;; Disable flymake and enable flycheck for realtime syntax checking
(when (require 'flycheck nil t)
  (setq elpy-modules (delq 'elpy-module-flymake elpy-modules))
  (add-hook 'elpy-mode-hook 'flycheck-mode))

;; Enable autopep8 on save
(add-hook 'elpy-mode-hook 'py-autopep8-enable-on-save)

;; Enable py-isort on save
(add-hook 'before-save-hook 'py-isort-before-save)

;; Example use of python
(defun example ()
  (interactive)
  (let ((cmd (format
              "/usr/local/bin/python3 ~/.emacs.d/python/example.py %s"
              (buffer-file-name))))
    (shell-command-on-region (region-beginning) (region-end) cmd nil "REPLACE" nil t)))
