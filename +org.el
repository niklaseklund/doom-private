;;;  -*- lexical-binding: t; -*-

;;
;; Org-mode
(setq org-directory "~/org")
(after! org
  (setq outline-blank-line nil
        org-cycle-separator-lines 2
        org-log-done 'time)
  ;; Org-links to emails
  (require 'ol-notmuch)
  ;; Popups
  (set-popup-rule! "^CAPTURE.*\\.org$" :size 0.4 :side 'bottom :select t :autosave t))

;; Hooks
(add-hook 'org-mode-hook (lambda ()
                           (hl-line-mode -1)))
(add-hook 'org-mode-hook (lambda ()
                           (flycheck-mode -1))
          (add-hook 'org-mode-hook #'doom-disable-line-numbers-h))
