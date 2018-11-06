;;; config/niklascarlsson/init.el -*- lexical-binding: t; -*-

;; Override the doom leader keys
(setq doom-leader-key ","
      doom-localleader-key ", m")

;; Set the font
(when IS-MAC
  (setq doom-font (font-spec :family "Roboto Mono" :size 13))
  )
(when IS-LINUX
  (setq doom-font (font-spec :family "Roboto Mono" :size 15))
  )

;; Set the theme
(setq doom-theme 'doom-one)
;; (setq doom-theme 'doom-dracula)
;; (setq doom-theme 'doom-nord-light)
