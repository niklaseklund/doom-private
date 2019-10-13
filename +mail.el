;;; +mail.el -*- lexical-binding: t; -*-

(use-package! org-mime
  :after (org notmuch)
  :config (setq org-mime-library 'mml))


(use-package! counsel-notmuch
  :when (featurep! :completion ivy)
  :commands counsel-notmuch
  :after notmuch)

(after! notmuch
  ;; treat notmuch buffers as reall buffer (going from show->search will work)
  (add-hook 'doom-real-buffer-functions #'notmuch-interesting-buffer)
  (add-hook 'notmuch-search-mode-hook #'hide-mode-line-mode)

  ;;
  ;; bindings
  ;; avoid killing notmuch search window
  (map!
   :map notmuch-search-mode-map
   :nv "q" nil)
  (map!
   :map notmuch-tree-mode-map
   :nv "d" nil))

;; Other mail related settings
(setq send-mail-function 'sendmail-send-it
      notmuch-fcc-dirs nil              ; creates a copy of the mail, disable
      message-send-mail-function 'message-send-mail-with-sendmail
      sendmail-program "msmtp"
      message-kill-buffer-on-exit t
      notmuch-message-headers-visible nil)

;; (defun +notmuch-dont-confirm-on-kill-process-a (orig-fn &rest args)
;;   "Don't prompt for confirmation when killing notmuch sentinel."
;;   (let (confirm-kill-processes)
;;     (apply orig-fn args)))
;; (advice-add #'notmuch-start-notmuch-sentinel :around #'+notmuch-dont-confirm-on-kill-process-a)
