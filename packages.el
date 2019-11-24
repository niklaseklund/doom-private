;;; packages.el -*- lexical-binding: t; -*-

;; writing
(package! org-noter)
(package! writeroom-mode)
(package! org-brain)
(package! org-ref)
(package! jupyter)

;; shell
(package! fish-completion)
(package! esh-autosuggest)
(package! eshell-detach :recipe (:host gitlab :repo "niklascarlsson/eshell-detach"))

;; debugging
(package! dap-mode)

;; media
(package! gif-screencast)
(package! ivy-youtube)

;; gtd
(package! org-super-agenda)
(package! ejira :recipe (:host github :repo "nyyManni/ejira"))

;; system
(package! disk-usage)
(package! transmission)
(package! pulseaudio-control)
(package! enwc)
(package! magit-gerrit :recipe (:host github :repo "niklascarlsson/magit-gerrit"))
(package! gerrit-ci :recipe (:host gitlab :repo "niklascarlsson/gerrit-ci" :branch "master"))
(package! emacs-conflicts :recipe (:host github :repo "ibizaman/emacs-conflicts" :branch "master"))
(package! bluetooth)
(package! dired-recent)
(package! dired-subtree)
(package! dired-narrow)

;; chat
(package! erc-hl-nicks)

;; rss
(package! elfeed)
(package! elfeed-org)

;; mail
(package! notmuch)
(package! org-mime)
(package! counsel-notmuch)
