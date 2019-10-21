;;; packages.el -*- lexical-binding: t; -*-

;; writing
(package! org-noter)
(package! org-jira)
(package! writeroom-mode)
(package! org-brain)

;; shell
(package! esh-autosuggest)
(package! fish-completion)
(package! eshell-detach :recipe (:host gitlab :repo "niklascarlsson/eshell-detach"))

;; debugging
(package! dap-mode)

;; media
(package! gif-screencast)
(package! ivy-youtube)

;; gtd
(package! org-super-agenda)

;; system
(package! disk-usage)
(package! transmission)
(package! pulseaudio-control)
(package! enwc)
(package! gerrit-ci :recipe (:local-repo "~/opensource/gerrit-ci"))
(package! magit-gerrit :recipe (:host github :repo "niklascarlsson/magit-gerrit"))
(package! emacs-conflicts :recipe (:host github :repo "ibizaman/emacs-conflicts" :branch "master"))

;; chat
(package! erc-hl-nicks)

;; rss
(package! elfeed)
(package! elfeed-org)

;; mail
(package! notmuch)
(package! org-mime)
(package! counsel-notmuch)
