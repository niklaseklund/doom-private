;;; +mail.el -*- lexical-binding: t; -*-

(use-package! org-mime
  :after (org notmuch)
  :config (setq org-mime-library 'mml))


(use-package! counsel-notmuch
  :commands counsel-notmuch
  :after notmuch)


(after! notmuch
  ;; treat notmuch buffers as reall buffer (going from show->search will work)
  ;; (add-hook 'doom-real-buffer-functions #'notmuch-interesting-buffer)
  (add-hook! '(notmuch-search-mode-hook
               notmuch-show-mode-hook
               notmuch-message-mode-hook) #'hide-mode-line-mode)
  (add-hook! '(notmuch-show-mode-hook
               notmuch-message-mode-hook) #'+notmuch-setup-mail-mode)
  (defun +notmuch-setup-mail-mode ()
    (writeroom-mode 1)
    (doom-disable-line-numbers-h)
    (visual-line-mode))

  ;; TODO: How to deal with closing of windows?
  (map!
   (:map notmuch-search-mode-map
     :nv "gr" #'notmuch-refresh-this-buffer))


  ;; Other mail related settings
  (setq send-mail-function 'sendmail-send-it
        notmuch-fcc-dirs
        '(("niklas.carlsson@posteo.net" . "posteo/Sent -inbox +sent -unread +private")
          ("niklas.carlsson@zenuity.com" . "zenuity/Sent -inbox +sent -unread +work"))
        message-send-mail-function 'message-send-mail-with-sendmail
        sendmail-program "msmtp"
        message-kill-buffer-on-exit t
        notmuch-message-headers-visible nil))
