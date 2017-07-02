;; Use indentatino of 4 spaces
(setq-default c-basic-offset 4)

;; Use irony-mode for c-hooks
(add-hook 'c++-mode-hook 'irony-mode)
(add-hook 'c-mode-hook 'irony-mode)
(add-hook 'objc-mode-hook 'irony-mode)

;; (add-hook 'irony-mode-hook 'my-irony-mode-hook)
(add-hook 'irony-mode-hook 'irony-cdb-autosetup-compile-options)

;; Enable company completion for irony
(eval-after-load 'company
  '(add-to-list
    'company-backends '(company-irony-c-headers company-irony)))

;; Enable flycheck for irony
(eval-after-load 'flycheck
  '(add-hook 'flycheck-mode-hook #'flycheck-irony-setup))

;; Set gcc and clang c++ version used
(add-hook 'c++-mode-hook (lambda () (setq flycheck-gcc-language-standard "c++14")))
(add-hook 'c++-mode-hook (lambda () (setq flycheck-clang-language-standard "c++14")))
