;;; Improve Emacs startup time

;;
;; PLEASE DO NOT ADD ANY CODE `BEFORE' THIS SECTION
;;
;; Avoid garbage collection during startup. The GC eats up quite a bit
;; of time, easily doubling the startup time. The trick is to turn up
;; the memory threshold (500 MB should be sufficient) in order to
;; prevent it from running during startup.

(setq gc-cons-threshold (* 500 1024 1024)
      gc-cons-percentage 0.6)

;; After Emacs startup has been completed, set `gc-cons-threshold' to
;; 16 MB and reset `gc-cons-percentage' to its original value.
(add-hook 'emacs-startup-hook
          '(lambda ()
             (setq gc-cons-threshold (* 16 1024 1024)
                   gc-cons-percentage 0.1)))

;; Log Emacs startup time in *Messages*
(add-hook 'emacs-startup-hook
          '(lambda ()
             (message (format "Emacs startup time: %s" (emacs-init-time)))))
