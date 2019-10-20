;;;  -*- lexical-binding: t; -*-


;;
;; Emacs daemon
;; Make sure theme loads properly
;; https://github.com/arcticicestudio/nord-emacs/issues/59
(if (daemonp)
    (cl-labels ((load-dracula (frame)
                              (with-selected-frame frame
                                (load-theme 'doom-dracula t))
                              (remove-hook 'after-make-frame-functions #'load-dracula)))
      (add-hook 'after-make-frame-functions #'load-dracula))
  (load-theme 'doom-dracula t))


;;
;; Force load packages

;; Eshell
(require 'eshell)
(require 'em-term)
(require 'em-tramp)
(require 'em-alias)
(require 'em-banner)
(require 'em-basic)
(require 'em-glob)
(require 'em-hist)
(require 'em-ls)
(require 'em-script)
(require 'em-unix)

;; Mail
(require 'notmuch)

;; Org
(require 'org)
(require 'org-agenda)

;; Git
(require 'magit)

;; Irc
(require 'erc)
(require 'flyspell)

;; Misc
(require 'writeroom-mode)
(require 'ws-butler)
(require 'so-long)
(require 'recentf)
(require 'company)
(require 'ivy)
(require 'yasnippet)
(require 'which-key)
(require 'avy)
(require 'semantic)
(require 'whitespace)
(require 'evil-mc)
