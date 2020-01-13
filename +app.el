;;; +app.el -*- lexical-binding: t; -*-

;;; Description
;; This file contains application packages that aims to extend the powers of
;; Emacs to control or interact with external applications to bring workflows
;; into Emacs.


;;; Music
(use-package! counsel-spotify
  :defer t
  :config
  (setq counsel-spotify-client-id (+pass-get-user "web/spotify/spotify.el")
        counsel-spotify-client-secret (+pass-get-secret "web/spotify/spotify.el")))


;;; Video
(use-package! ivy-youtube
  :defer t
  :config
  (setq ivy-youtube-key (+pass-get-secret "web/youtube/api-key")
        browse-url-browser-function 'browse-url-generic
        browse-url-generic-program "firefox"
        ivy-youtube-play-at "/usr/bin/vlc"
        ivy-youtube-history-file (concat doom-local-dir "ivy-youtube-history")))


;;; Bookmarks
(use-package! ebuku
  :defer t
  :config
  (setq ebuku-buku-path "/usr/bin/buku"))


;;; Torrents
(use-package! transmission
  :defer t
  :config
  (setq  transmission-refresh-modes '(transmission-mode
                                      transmission-files-mode
                                      transmission-info-mode
                                      transmission-peers-mode))
  (set-popup-rule! "*transmission*" :size 0.3 :side 'bottom :select t :autosave t)
  (with-eval-after-load 'transmission
    (advice-add 'transmission :before 'nc/transmission-start-daemon-a)
    (setq transmission-refresh-modes '(transmission-mode
                                       transmission-files-mode
                                       transmission-info-mode
                                       transmission-peers-mode)
          transmission-refresh-interval 1)))
