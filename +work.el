;;; +work.el -*- lexical-binding: t; -*-

;; A collection of work utilty functionality


;;
;; Gerrit
(use-package! gerrit
  :load-path "~/opensource/gerrit"
  :ensure nil
  :defer t
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


;;
;; Tramp remote editing
(with-eval-after-load 'tramp-sh
  ;; Create persistent connections
  (customize-set-variable
   'tramp-ssh-controlmaster-options
   (concat
    "-o ControlPath=/tmp/ssh-ControlPath-%%r@%%h:%%p "
    "-o ControlMaster=auto -o ControlPersist=yes"))
  (customize-set-variable 'tramp-use-ssh-controlmaster-options nil)
  ;; Add the remote host path
  (add-to-list 'tramp-remote-path 'tramp-own-remote-path)

  ;; solution for getting around a server with warning message about a not fully
  ;; functional terminal. This is due to the fact that tramp is set to "dumb"
  ;; Found a similar problem to mine here:
  ;; http://emacs.1067599.n8.nabble.com/problem-getting-files-under-SunOS-from-cygwin-td277890.html
  ;; there is also an example there about an interactive user input version if I ever need that :)
  (defconst my-tramp-press-return-prompt-regexp
    "\\(-  (press RETURN)\\)\\s-*"
    "Regular expression matching my login prompt request.")

  (defun my-tramp-press-return-action (proc vec)
    "Enter \"?\^M\" to send a carriage return."
    (save-window-excursion
      (message "%s" vec)
      (with-current-buffer (tramp-get-connection-buffer vec)
        (tramp-message vec 6 "\n%s" (buffer-string))
        ;; The control character for Enter is ^M
        ;; https://www.gnu.org/software/emacs/manual/html_node/elisp/Ctl_002dChar-Syntax.html#Ctl_002dChar-Syntax
        (tramp-send-string vec "?\^M"))))

  (add-to-list 'tramp-actions-before-shell
               '(my-tramp-press-return-prompt-regexp my-tramp-press-return-action)))
