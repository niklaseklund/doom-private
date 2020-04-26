;;; +system.el -*- lexical-binding: t; -*-

;; A collection of system utilty functionality

;;
;; Package manager
(use-package! guix
  :defer t
  :config
  (set-popup-rule! "\\*Guix Packages*" :ignore t)
  (set-popup-rule! "\\*Guix Package Info*" :side 'bottom :size 0.8 :vslot 10)
  (set-popup-rule! "\\*Guix REPL\\*" :side 'bottom :size 0.3 :vslot 5))


;;
;; Disk usage
(use-package! disk-usage
  :defer t)


;;
;; Bluetooth
(use-package! bluetooth
  :defer t
  :config
  (advice-add 'bluetooth-list-devices :before '+bluetooth-pop-to-buffer-a)
  (set-popup-rule! "*Bluetooth*" :size 0.3 :side 'bottom :select t :autosave t)
  (map!
   :map bluetooth-mode-map
   :desc "Connect" :n "c" #'bluetooth-connect
   :desc "Disconnect" :n "x" #'bluetooth-disconnect))


;;
;; Transmission
(use-package! transmission
  :defer t
  :config
  (setq transmission-refresh-modes '(transmission-mode
                                      transmission-files-mode
                                      transmission-info-mode
                                      transmission-peers-mode))
  (set-popup-rule! "*transmission*" :size 0.3 :side 'bottom :select t :autosave t)
  (with-eval-after-load 'transmission
    (advice-add 'transmission :before '+transmission-start-daemon-a)
    (setq transmission-refresh-modes '(transmission-mode
                                       transmission-files-mode
                                       transmission-info-mode
                                       transmission-peers-mode)
          transmission-refresh-interval 1)))


;;; Video
(use-package! ivy-youtube
  :defer t
  :config
  (setq ivy-youtube-key (+pass-get-secret "web/youtube/api-key")
        browse-url-browser-function 'browse-url-generic
        browse-url-generic-program "firefox"
        ivy-youtube-play-at (executable-find "vlc")
        ivy-youtube-history-file (concat doom-local-dir "ivy-youtube-history")))


;;
;; Conflicts
(use-package! emacs-conflict
  :after dired)

;;
;; Notifications
(use-package! alert
  :config
  (setq alert-default-style 'libnotify)
  (add-hook 'compilation-start-hook '+compile-start-time-h)
  (add-hook 'compilation-finish-functions '+compile-notify-finish-h))
