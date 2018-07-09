;; -*- no-byte-compile: t; -*-
;;; config/niklascarlsson/packages.el

;; (package! writeroom-mode)
(package! lsp-mode)
(package! company-lsp)
(package! lsp-ui)
(package! emacs-ccls :recipe (:fetcher github :repo "MaskRay/emacs-ccls" :files ("*")))
(package! org-noter)
(package! esh-autosuggest)
(package! fish-completion)
