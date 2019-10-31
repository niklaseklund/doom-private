;;; +chat.el -*- lexical-binding: t; -*-

(use-package! erc
  :defer t
  :custom
  (erc-autojoin-timing 'ident)
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
  (setq erc-nick '("niklas" "downfall" "niklascarlsson"))
  (add-hook 'erc-mode-hook (lambda () (flycheck-mode -1)))

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
   (("irc.freenode.net") "#emacs"))

  ;; join channels
  (defun nc/erc-join-channel ()
    "Select a channel to join."
    (interactive)
    (let* ((channels '("#archlinux"
                       "#python"
                       "#emacsconf"
                       "#next-browser"
                       "#lisp"
                       "#stumpwm"))
           (join-channel (completing-read "Join channel: "
                                          (cl-sort channels 'string-lessp :key 'downcase) nil t)))
      (erc-join-channel join-channel))))


(use-package! erc-hl-nicks
  :after erc)
