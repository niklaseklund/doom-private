;;; +chat.el -*- lexical-binding: t; -*-

(use-package! erc
  :defer t
  :custom
  (erc-autojoin-timing 'ident)
  (erc-fill-function 'erc-fill-static)
  (erc-fill-static-center 22)
  (erc-header-line-format nil)
  (erc-hide-list '("JOIN" "PART" "QUIT"))
  (erc-join-buffer 'bury)
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
  (setq erc-nick '("niklascarlsson" "downfall" "niklas"))
  (add-hook 'erc-mode-hook (lambda () (flycheck-mode -1)))
  (add-hook 'erc-mode-hook #'hide-mode-line-mode)

  ;; autojoin channels
  (defmacro erc-autojoin (&rest args)
    `(add-hook 'erc-after-connect
               '(lambda (server nick)
                  (cond
                   ,@(mapcar (lambda (servers+channels)
                               (let ((servers (car servers+channels))
                                     (channels (cdr servers+channels)))
                                 `((member erc-session-server ',servers)
                                   (mapc 'erc-join-channel ',channels))))
                             args)))))
  (erc-autojoin
   (("irc.freenode.net") "#emacs" "#emacsconf" "#python" "#next-browser" "#lisp" "#stumpwm")))

;; TODO: Create a workspace layout that I can call which creates a layout of the
;; channels that I use the most?

(use-package! erc-hl-nicks
  :after erc)
