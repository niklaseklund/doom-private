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

;;
;; Hugo
(after! ox-hugo
  (defun org-hugo-publish ()
    "Publish changes to the blog."
    (interactive)
    (let ((default-directory "~/src/emacs-blog/"))
      ;; Build the project
      (shell-command "hugo")
      (let ((default-directory (concat default-directory "public/"))
            (commit-message (format "rebuilding site %s" (shell-command-to-string "echo -n $(date +%Y%m%d)"))))
        ;; Add modified files
        (apply #'call-process `("git" nil "*add-debug*" nil "add" "."))
        ;; Create commit
        (apply #'call-process `("git" nil "*commit-debug*" nil "commit" "-m" ,commit-message))
        ;; Publish
        (apply #'call-process `("git" nil "*push-debug*" nil "push" "origin" "master"))))))
