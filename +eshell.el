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

  ;; Keybindings
  (map! :map eshell-mode-map
        :i "C-p" 'eshell-previous-input
        :i "C-n" 'eshell-next-input
        :i "M-c" 'counsel-projectile-find-dir
        :ni "C-k" #'evil-window-up
        :ni "C-j" #'evil-window-down
        :ni "C-h" #'evil-window-left
        :ni "C-l" #'evil-window-right
        :i "TAB" #'completion-at-point
        :i [tab] #'completion-at-point)

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
    (lambda () (add-hook 'eshell-pre-command-hook 'eshell-save-some-history)))

  (remove-hook 'eshell-mode-hook '+eshell-init-company-h))


;;
;;; Completion

(use-package! esh-autosuggest
  :after eshell
  :config
  (setq ivy-do-completion-in-region t)

  (defun setup-eshell-ivy-completion-h ()
    "Use ivy mini-buffer for completion.
This works pretty nice with childframes I think."
    (map! :map eshell-mode-map
          [remap eshell-pcomplete] 'completion-at-point)
    (setq-local ivy-display-functions-alist
                (remq (assoc 'ivy-completion-in-region ivy-display-functions-alist)
                      ivy-display-functions-alist)))

  (add-hook 'eshell-mode-hook #'esh-autosuggest-mode)
  (add-hook 'eshell-mode-hook #'setup-eshell-ivy-completion-h))

(use-package! fish-completion
  :config
  (setq fish-completion-fallback-on-bash-p t)
  (global-fish-completion-mode))

(use-package! bash-completion
  :config
  (setq bash-completion-prog (executable-find "bash")))



;; Vterm
(after! vterm

  (defadvice! +default/yank-pop-a (orig-fn &rest args)
    :around #'+default/yank-pop
    (if (eq major-mode 'vterm-mode )
        (+vterm/yank-pop)
      (apply orig-fn args)))

  (map!
   (:map vterm-mode-map
     :desc "Search history" :i "C-s" (lambda! () (vterm-send-key "r" nil nil t))
     :desc "Paste from evil register" :i "C-r" #'+vterm/paste-from-register)))


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
