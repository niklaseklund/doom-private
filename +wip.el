;;; +wip.el -*- lexical-binding: t; -*-

(use-package! which-key-posframe
  :config
  (which-key-posframe-mode))

(use-package! hydra-posframe
  :hook (after-init . hydra-posframe-enable)
  :config
  ;; Temporary fix want something that matches the ivy-posframe better so they
  ;; can be used at the same time. Could this be some inspiration?
  ;; https://github.com/sandinmyjoints/.emacs.d/blob/1070cfae96d6a197fc6422d27f509d626e01ba07/setup-lisp/services.el
  (setq hydra-posframe-poshandler 'posframe-poshandler-frame-top-center))

(use-package! ebuku
  :config
  (setq ebuku-buku-path "/usr/bin/buku"))

(use-package! counsel-spotify
  :defer t
  :config
  (setq counsel-spotify-client-id (auth-source-pass-get "user" "web/spotify/spotify.el")
        counsel-spotify-client-secret (auth-source-pass-get 'secret "web/spotify/spotify.el")))
