;;; org-mode-setup.el --- Part of Iwan's Emacs configuration

;;; Commentary:
;;
;; This file is part of my default Emacs configuration.

;;; Code:

(use-package org
  :bind (("C-c a" . org-agenda)
         ("C-c c" . org-capture)
         ("C-c l" . org-store-link))

  :init
  ;; Show time when done
  (setq org-log-done t)

  ;; Folder to look for agenda files
  (setq org-agenda-files '("~/Sync/org"))

  ;; Log quick notes (C-c C-z) into LOGBOOK drawer
  (setq org-log-into-drawer t)

  ;; Syntax highlighting for org-mode
  (setq org-src-fontify-natively t)

  :config
  ;; Add languages for the ‘src’ code blocks in org-mode
  (org-babel-do-load-languages
   'org-babel-load-languages
   '((emacs-lisp . t)
     (shell . t)
     (python . t))))

;;; org-mode-setup.el ends here
