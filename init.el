;; Set load-path for libraries
(add-to-list 'load-path "~/.emacs.d/config/")

(load-library "tweak-startup-time")

(load-library "defaults")

;; Initialize use-package
(load-library "init-use-package")

;; Snippets
;; * Comment/Uncomment line or region
;; * Delete current file and buffer
(load-library "snippets")

;; Setup python
(load-library "python-setup")

;; Setup c++
(load-library "c++-setup")

;; Setup org-mode
(load-library "org-mode-setup")

;; Setup miscellaneous libraries
(load-library "miscellaneous")
