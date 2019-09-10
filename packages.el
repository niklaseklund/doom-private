;;; packages.el -*- lexical-binding: t; -*-

;; writing
(package! org-noter)
(package! org-jira)
(package! ox-hugo)
(package! writeroom-mode)
(package! org-brain)

;; shell
(package! esh-autosuggest)
(package! fish-completion)
(package! eshell-detach :recipe (:host gitlab :repo "niklascarlsson/eshell-detach"))

;; debugging
(package! dap-mode)

;; recording
(package! gif-screencast)

;; gtd
(package! org-super-agenda)

;; system
(package! disk-usage)

;; chat
(package! erc-hl-nicks)

;; Feeder
(package! elfeed)
(package! elfeed-org)
