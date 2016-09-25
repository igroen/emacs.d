;; Enable elpy (Run M-x elpy-config to configure)
(elpy-enable)

;; Disable flymake and enable flycheck for realtime syntax checking
(when (require 'flycheck nil t)
  (setq elpy-modules (delq 'elpy-module-flymake elpy-modules))
  (add-hook 'elpy-mode-hook 'flycheck-mode))

;; Enable autopep8 on save
(add-hook 'elpy-mode-hook 'py-autopep8-enable-on-save)

;; virtualenvwrapper
(venv-initialize-interactive-shells)  ;; if you want interactive shell support
(venv-initialize-eshell)  ;; if you want eshell support
(setq venv-location "~/.virtualenvs/")
