;;; +work.el -*- lexical-binding: t; -*-

;; A collection of work utilty functionality


(use-package! gerrit
 :load-path "~/opensource/gerrit"
 :ensure nil
 :config
 (setq gerrit-url "gerrit.zenuity.com"
       gerrit-email "niklas.carlsson@zenuity.com"
       gerrit-job-regexp "^-\\s-*src--check-src-\\(.*\\)-src\\b\\s-*\\(\\bhttps.*/\\)\\s-*:\\s-*\\(\\b.*\\b\\)\\s-*in\\s-*\\(\\b.*\\b\\)")

 ;; Make DOOM not treat gerrit windows as popups
 (set-popup-rule! "\\*gerrit-*" :ignore t)

 ;; Keymap
 (map!
  (:map gerrit-mode-map
    :n "?" #'gerrit-dispatch
    :n "b" #'gerrit-browse
    :n "f" #'counsel-imenu
    :n "l" #'gerrit-changes-transient
    :n "m" #'gerrit-message-transient
    :n "o" #'gerrit-imenu-open
    :n "p" #'gerrit-list-patch-sets
    :n "r" #'gerrit-refresh-changes
    :n "RET" #'gerrit-list-jobs)
  (:map gerrit-change-mode-map
    :n "^" #'navigel-open-parent
    :n "f" #'counsel-imenu
    :n "o" #'gerrit-imenu-open
    :n "b" #'gerrit-browse)))
