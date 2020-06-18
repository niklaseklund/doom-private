;;;  -*- lexical-binding: t; -*-

;;
;; UI
(setq user-full-name    "Niklas Carlsson"
      user-mail-address "niklas.carlsson@posteo.net")

;; customize
(setq doom-modeline-height 40
      +doom-modeline-buffer-file-name-style 'relative-from-project
      doom-theme 'doom-nord
      x-super-keysym 'super
      x-meta-keysym 'meta
      +workspaces-switch-project-function #'ignore
      +pretty-code-enabled-modes '(emacs-lisp-mode org-mode))

;; host os configuration
(when (string= (system-name) "archbook")
  (setq doom-font (font-spec :family "Fira Code" :size 16)
        doom-big-font-increment 8
        ;; doom-variable-pitch-font (font-spec :family "EtBembo" :size 22)
        )
  (font-put doom-font :weight 'semi-light))
(when (string= (system-name) "u445bfa80-2ca8")
  (setq doom-font (font-spec :family "Iosevka Term SS04" :size 16)
        doom-big-font-increment 8
        doom-variable-pitch-font (font-spec :family "EtBembo" :size 18))
  (font-put doom-font :weight 'semi-light))


;;
;; Lookup
(add-to-list '+lookup-provider-url-alist '("ArchWiki" "https://wiki.archlinux.org/index.php?search=%s"))
(setq +lookup-provider-url-alist (assoc-delete-all "Google images" +lookup-provider-url-alist))
(setq +lookup-provider-url-alist (assoc-delete-all "Google maps" +lookup-provider-url-alist))


;;
;; Mode file association
(add-to-list 'auto-mode-alist '("\\.MD\\'" . markdown-mode))

;;
;; Bindings
(map!
 ;; create custom leader bindings
 (:leader
  :desc "Resume Avy" :n "\"" #'avy-resume
  (:prefix "o"
   :desc "Open mail" :n "m" (cmd! (notmuch-search "tag:inbox"))
   :desc "Open (pass-)store" :n "S" #'pass
   :desc "Open (ivy-pass-)store" :n "s" #'ivy-pass
   :desc "Open Guix" :n "g" #'guix-popup)
  (:prefix "s"
   :desc "Search Youtube" :n "y" #'ivy-youtube)
  (:prefix "p"
   :desc "Open Magit in project:" :n "p" (cmd! (counsel-projectile-switch-project "v")))
  (:prefix "w"
   :desc "Select window" :n "w" #'ace-window)))


;;
;; Chat
(after! circe
  (set-irc-server! "chat.freenode.net"
                   `(:tls t
                     :port 6697
                     :nick "niklascarlsson"
                     :sasl-username ,(+pass-get-user "web/irc.freenode.net")
                     :sasl-password (lambda (&rest _) (+pass-get-secret "web/irc.freenode.net"))
                     :channels ("#emacs" "#guix"))))

;;
;; Load other config files
(load! "+code")
(load! "+eshell")
(load! "+exwm")
(load! "+org")
(load! "+mail")
(load! "+system")
