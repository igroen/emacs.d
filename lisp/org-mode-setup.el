;; Keyboard shortcuts
(define-key global-map "\C-ca" 'org-agenda)
(define-key global-map "\C-cl" 'org-store-link)

;; Show time when done
(setq org-log-done t)

;; Folder to look for agenda files
(setq org-agenda-files '("~/org"))

;; Syntax highlighting for org-mode
(setq org-src-fontify-natively t)

;; Add languages for the ‘src’ code blocks in org-mode
(org-babel-do-load-languages
 'org-babel-load-languages
 '((emacs-lisp . t)
   (sh . t)
   (python . t)))
