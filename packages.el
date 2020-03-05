;;; packages.el -*- lexical-binding: t; -*-

;; apps
(package! ebuku :recipe (:host github :repo "flexibeast/ebuku"))
(package! counsel-spotify)
(package! ivy-youtube)
(package! transmission)

;; writing
(package! org-noter)
(package! org-ref)

;; shell
(package! fish-completion)
(package! bash-completion)
(package! esh-autosuggest)
(package! eshell-detach :recipe (:host gitlab :repo "niklascarlsson/eshell-detach"))
(package! yequake)

;; media
(package! gif-screencast)
(package! emms)

;; gtd
(package! org-super-agenda)

;; code
(package! slime)
(package! slime-repl-ansi-color)
(package! slime-company)

;; system
(package! disk-usage)
(package! enwc)
(package! magit-gerrit :recipe (:host github :repo "niklascarlsson/magit-gerrit"))
(package! emacs-conflicts :recipe (:host github :repo "ibizaman/emacs-conflicts" :branch "master"))
(package! bluetooth)
(package! dired-recent)
(package! dired-subtree)
(package! dired-narrow)
(package! command-log-mode)
(package! alert)
(package! guix)

;; chat
(package! erc-hl-nicks)

;; rss
(package! elfeed)
(package! elfeed-org)

;; mail
(package! notmuch)
(package! org-mime)
(package! counsel-notmuch)

;; development
(package! navigel)
(package! emacsql)
(package! emacsql-sqlite)

;; wip
(package! which-key-posframe)
(package! hydra-posframe :recipe (:host github :repo "Ladicle/hydra-posframe"))
