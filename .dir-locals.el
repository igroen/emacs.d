;;; Directory Local Variables            -*- no-byte-compile: t -*-
;;; For more information see (info "(emacs) Directory Variables")

((nil . ((indent-tabs-mode  . nil)))
 (python-base-mode
  . ((python-indent-offset . 4)
     (tab-width . 4)
     (eglot-workspace-configuration
      . (:pylsp (:plugins (:mccabe (:enabled :json-false)
                           :pycodestyle (:enabled :json-false)
                           :pyflakes (:enabled :json-false)
                           :flake8 (:enabled t))
                 :configurationSources ["flake8"])))
     (eval . (pyvenv-activate (expand-file-name "venv")))))
 (c-mode
  . ((c-basic-offset . 4)))
 (c++-mode
  . ((c-basic-offset . 4))))
