;;; +chat.el -*- lexical-binding: t; -*-

(use-package! erc
  :defer t
  :custom
  (erc-autojoin-channels-alist '(("freenode.net" "#archlinux" "##c++"
                                  "#emacs" "#python")))
  (erc-autojoin-timing 'ident)
  (erc-autojoin-delay 60)
  (erc-fill-function 'erc-fill-static)
  (erc-fill-static-center 22)
  (erc-header-line-format nil)
  (erc-hide-list '("JOIN" "PART" "QUIT"))
  (erc-join-buffer 'buffer)
  (erc-lurker-hide-list '("JOIN" "PART" "QUIT"))
  (erc-prompt-for-nickserv-password nil)
  (erc-server-reconnect-attempts 5)
  (erc-server-reconnect-timeout 3)
  (erc-track-exclude-types '("JOIN" "MODE" "NICK" "PART" "QUIT"
                             "324" "329" "332" "333" "353" "477"))
  :config
  (add-to-list 'erc-modules 'notifications)
  (add-to-list 'erc-modules 'spelling)
  (erc-services-mode 1)
  (erc-update-modules)
  (setq erc-nick "niklascarlsson")
  (add-hook 'erc-mode-hook (lambda () (flycheck-mode -1)))
  )

(use-package! erc-hl-nicks
  :after erc)
