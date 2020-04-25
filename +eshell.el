;;; +eshell.el -*- lexical-binding: t; -*-

;;
;;; Eshell core
(after! eshell
  (setq eshell-hist-ignoredups t
        eshell-buffer-maximum-lines 1024
        eshell-history-size 10000)

  ;; Enable TRAMP to use sudo.
  (require 'em-tramp)

  ;; Handle visual commands
  (after! term
    (add-hook 'term-mode-hook #'hide-mode-line-mode))
  (after! em-term
    (pushnew! eshell-visual-commands "bluetoothctl"))

  ;; Aliases
  (set-eshell-alias!
   "d" "dired $1"
   "gl" "(call-interactively 'magit-log-current)"
   "gs" "magit-status"
   "gc" "magit-commit"
   "gb" "(call-interactively #'magit-branch-checkout)"
   "gbc" "(call-interactively #'magit-branch-create)"
   "bat" "+eshell/bat $1"
   "sudo" "eshell/sudo $*"
   "nm" "nc/enwc"
   "locate" "counsel-locate $1"
   "man" "(+default/man-or-woman)"
   "info" "+eshell/info-manual"
   "tm" "transmission"
   "cal" "calendar"
   "pass" "(pass)"
   "fd" "+eshell/fd $1"
   "fo" "find-file-other-window $1"
   "rgi" "+default/search-cwd"
   "mountdrives" "nc/mount-drives")
  (setenv "PAGERQ" "cat")

  (add-hook! 'eshell-first-time-mode-hook
    (lambda () (add-hook 'eshell-pre-command-hook 'eshell-save-some-history))))


;;
;; Detach
(use-package! detach
  :load-path "~/src/detach"
  :ensure nil
  :after eshell
  :config
  (setq detach-db-dir doom-etc-dir)
  (require 'counsel-detach)
  ;; Keybindings
  (map! :map eshell-mode-map
        :ni [C-return] #'counsel-detach
        :ni [S-return] #'detach-create-session)
  ;; Popup rules
  (set-popup-rule! "\\*detach-*" :size 0.5 :side 'right :quit t :modeline nil)
  ;; Enable ivy-rich
  (ivy-rich-mode 0)
  (ivy-rich-mode +1))

;;
;; Shell
;; Customize the shell to use per host
;; https://www.gnu.org/software/tramp/
(connection-local-set-profile-variables
 'remote-bash
 '((explicit-shell-file-name . "/bin/bash")
   (explicit-bash-args . ("-i"))))
(connection-local-set-profiles
  '(:application tramp :protocol "ssh" :machine "pi")
  'remote-bash)
