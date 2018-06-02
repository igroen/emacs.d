;; Enable elpy (Run M-x elpy-config to configure)
(elpy-enable)
(global-set-key (kbd "M-*") 'pop-tag-mark)

;; Don't print evaluated code fragments in the python shell
;; https://elpy.readthedocs.io/en/latest/ide.html#option-elpy-shell-echo-input
(setq elpy-shell-echo-input nil)

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
