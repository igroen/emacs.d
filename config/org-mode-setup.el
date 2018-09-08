(use-package org
  :bind (("C-c a" . org-agenda)
         ("C-c c" . org-capture)
         ("C-c l" . org-store-link))

  :init
  ;; Show time when done
  (setq org-log-done t)

  ;; Folder to look for agenda files
  (setq org-agenda-files '("~/Dropbox/org"))

  ;; Syntax highlighting for org-mode
  (setq org-src-fontify-natively t)

  :config
  ;; Add languages for the ‘src’ code blocks in org-mode
  (org-babel-do-load-languages
   'org-babel-load-languages
   '((emacs-lisp . t)
     (shell . t)
     (python . t))))
