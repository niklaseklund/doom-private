;;; +mail.el -*- lexical-binding: t; -*-

(use-package! org-mime
  :after (org notmuch)
  :config (setq org-mime-library 'mml))


(use-package! counsel-notmuch
  :commands counsel-notmuch
  :after notmuch)


(after! notmuch
  ;; Configure ui
  (add-hook 'doom-real-buffer-functions #'notmuch-interesting-buffer)
  (advice-add #'notmuch-start-notmuch-sentinel :around #'+notmuch-dont-confirm-on-kill-process-a)

  ;; keybindings
  (map! :map (notmuch-search-mode-map notmuch-tree-mode-map notmuch-show-mode-map)
        :desc "Search notmuch"    "C-s" #'counsel-notmuch)

  (set-face-attribute 'notmuch-search-date nil :weight 'bold :slant 'italic)

  ;; Configure mail settings
  (setq notmuch-fcc-dirs
        '(("niklas.carlsson@posteo.net" . "posteo/Sent -inbox +sent -unread +private"))
        send-mail-function 'sendmail-send-it
        ;; notmuch-message-headers-visible nil
        notmuch-search-oldest-first nil
        message-kill-buffer-on-exit t
        message-send-mail-function 'message-send-mail-with-sendmail
        sendmail-program "msmtp"
        notmuch-saved-searches
        '((:name "inbox" :query "tag:inbox not tag:trash" :key "i")
          (:name "flagged" :query "tag:flagged" :key "f")
          (:name "sent" :query "tag:sent" :key "s")
          (:name "drafts" :query "tag:draft" :key "d"))))
