;;; +eshell.el -*- lexical-binding: t; -*-

(after! eshell
  ;;
  ;; Keybindings
  ;; Keys must be bound in a hook because eshell resets its keymap every
  ;; time `eshell-mode' is enabled.
  (add-hook! 'eshell-first-time-mode-hook
    (defun my/+eshell-init-keymap-h ()
      (map! :map eshell-mode-map
            :i "C-p" 'eshell-previous-input
            :i "C-n" 'eshell-next-input
            :i "M-c" 'counsel-projectile-find-dir)))


  ;;
  ;; Aliases
  (set-eshell-alias!
   "ff" "+ivy/projectile-find-file"
   "/p" "+ivy/project-search"
   "/d" "+ivy/project-search-from-cwd"
   "d" "deer $1"
   "l" "ls -l"
   "la" "ls -la"
   "gl" "(call-interactively 'magit-log-current)"
   "gs" "magit-status"
   "gc" "magit-commit"
   "gbD" "my/git-branch-delete-regexp $1"
   "gbS" "my/git-branch-match $1"
   "rg" "rg --color=always $*"
   "bat" "my/eshell-bat $1")
  (setenv "PAGER" "cat")

  ;;
  ;; Detach
  ;; (load! "eshell-detach")

  )


;;
;; Detach
(use-package! eshell-detach
  :after eshell
  :config
  (setq eshell-detach-max-file-name-length 60)
  (add-hook! 'eshell-first-time-mode-hook
    (defun eshell-detach-init-keymap-h ()
      (map! :map eshell-mode-map
            :ni [C-return] #'eshell-detach-attach
            :ni [S-return] #'eshell-detach-send-input))))


;;
;; Auto-suggestion
(use-package! esh-autosuggest
  :after eshell
  :config
  (add-hook 'eshell-mode-hook #'esh-autosuggest-mode)
  (when (and (executable-find "fish")
             (require 'fish-completion nil t))
    (global-fish-completion-mode)))
