;;; c++-setup.el --- Part of Iwan's Emacs configuration

;;; Commentary:
;;
;; This file is part of my default Emacs configuration.

;;; Code:

(use-package irony
  :ensure t

  :diminish irony-mode

  ;; Use irony-mode for c-hooks
  :hook ((c++-mode . irony-mode)
         (c-mode . irony-mode)
         (objc-mode . irony-mode))

  :init
  (setq-default c-basic-offset 4)
  ;; Run rtags-install within emacs
  (setq rtags-completions-enabled t)
  (setq rtags-autostart-diagnostics t)
  (setq cmake-ide-build-dir "cmake_build")

  :config
  (add-hook 'irony-mode-hook 'irony-cdb-autosetup-compile-options)
  (add-hook 'irony-mode-hook 'irony-eldoc)
  (add-hook 'flycheck-mode-hook 'flycheck-irony-setup)

  (use-package cmake-mode
    :ensure t

    :defer t)

  (use-package cmake-ide
    :ensure t

    :defer t)

  (use-package company-irony-c-headers
    :ensure t

    :defer t

    :diminish company-mode)

  (use-package company-irony
    :ensure t

    :disabled

    :defer t

    :diminish company-mode)

  (use-package company-rtags
    :ensure t

    :defer 1)

  (use-package irony-eldoc
    :ensure t

    :defer t

    :diminish eldoc-mode)

  (use-package flycheck-irony
    :ensure t

    :defer t

    :diminish flycheck-mode)

  (use-package rtags
    :ensure t

    :defer t

    :diminish rtags-mode

    :config
    (with-no-warnings
      (rtags-enable-standard-keybindings)))

  ;; company-irony and company-rtags are both completion backends.
  ;; When using both of them together duplicate completions are shown.
  ;; So use one of these completions backends but not both of them.
  (add-to-list 'company-backends '(;;company-irony
                                   company-irony-c-headers
                                   company-rtags))
  (cmake-ide-setup))

;;; c++-setup.el ends here
