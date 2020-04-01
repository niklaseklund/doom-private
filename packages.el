;;; packages.el -*- lexical-binding: t; -*-

;; apps
(package! ivy-youtube)
(package! transmission)

;; org
(package! org-super-agenda)
(package! org-noter)
(package! org-mime)

;; shell
(package! esh-autosuggest)
(package! fish-completion)
(package! bash-completion)

;; system
(package! guix)
(package! dired-recent)
(package! dired-subtree)
(package! dired-narrow)
(package! disk-usage)
(package! emacs-conflicts :recipe (:host github :repo "ibizaman/emacs-conflicts" :branch "master"))
(package! bluetooth)
(package! command-log-mode)
(package! alert)
(package! yequake)
(package! gif-screencast)

;; chat
(package! erc-hl-nicks)

;; rss
(package! elfeed)
(package! elfeed-org)

;; mail
(package! notmuch)
(package! counsel-notmuch)

;; package development
(package! navigel)
(package! emacsql)
(package! emacsql-sqlite)
(package! lorem-ipsum)
