;;; python-setup.el --- Part of Iwan's Emacs configuration

;;; Commentary:
;;
;; This file is part of my default Emacs configuration.

;;; Code:

(use-package elpy
  :ensure t

  :diminish elpy-mode

  :hook (python-mode . elpy-mode)

  :init
  ;; Elpy seems partially incompatible with Emacs 25's 'native completion' feature
  ;; https://github.com/jorgenschaefer/elpy/issues/887
  (setq python-shell-completion-native-enable nil)
  ;; Don't print evaluated code fragments in the python shell
  ;; https://elpy.readthedocs.io/en/latest/ide.html#option-elpy-shell-echo-input
  (setq elpy-shell-echo-input nil)

  :config
  (elpy-enable)

  ;; (add-hook 'before-save-hook 'elpy-black-fix-code)

  (use-package py-autopep8
    :ensure t

    :after elpy

    ;; :hook (elpy-mode . py-autopep8-enable-on-save)
    )

  (use-package py-isort
    :ensure t

    :after elpy

    ;; :hook (before-save . py-isort-before-save)
    ))


;; Example use of python
(defun example ()
  "This is an example of using python on an Emacs buffer."
  (interactive)
  (let ((cmd (format
              "/usr/local/bin/python3 ~/.emacs.d/python/example.py %s"
              (buffer-file-name))))
    (shell-command-on-region (region-beginning) (region-end) cmd nil "REPLACE" nil t)))

;;; python-setup.el ends here
