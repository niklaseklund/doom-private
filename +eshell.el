;;; +eshell.el -*- lexical-binding: t; -*-

(after! eshell
  ;; enable sudo through TRAMP
  (require 'em-tramp)

  ;;
  ;; DOOM overrides
  ;; disable def-advice from DOOM, I want to use authinfo for sudo
  (advice-remove 'tramp-read-passwd #'+default-inhibit-authinfo-for-sudo-a)
  ;; remove DOOM pcomplete setup
  (remove-hook 'eshell-mode-hook '+eshell-init-company-h)


  ;;
  ;; Custom functions
  (defun eshell/fd (regexp)
    "Recursively find items matching REGEXP and open in dired."
    (fd-dired default-directory regexp))

  (defun nc/eshell-bat (file)
    "Like `cat' but output the content of FILE with Emacs syntax highlighting."
    (with-temp-buffer
      (insert-file-contents file)
      (let ((buffer-file-name file))
        (delay-mode-hooks
          (set-auto-mode)
          (if (fboundp 'font-lock-ensure)
              (font-lock-ensure)
            (with-no-warnings
              (font-lock-fontify-buffer)))))
      (buffer-string)))


  ;;
  ;; Customize variables
  (setq eshell-hist-ignoredups t
        eshell-buffer-maximum-lines 1024
        eshell-history-size 10000)
  (after! em-term
    (pushnew! eshell-visual-commands "bluetoothctl" "vlccast" "tizonia"))


  ;;
  ;; Keybindings
  ;; Keys must be bound in a hook because eshell resets its keymap every
  ;; time `eshell-mode' is enabled.
  (add-hook! 'eshell-first-time-mode-hook :append
    (defun nc/+eshell-init-keymap-h ()
      (map!
       :map eshell-mode-map
       :i "C-p" 'eshell-previous-input
       :i "C-n" 'eshell-next-input
       :i "M-c" 'counsel-projectile-find-dir
       :ni "C-k" #'evil-window-up
       :ni "C-j" #'evil-window-down
       :ni "C-h" #'evil-window-left
       :ni "C-l" #'evil-window-right
       :i "TAB" #'completion-at-point
       :i [tab] #'completion-at-point)))

  ;;
  ;; Aliases
  (set-eshell-alias!
   "d" "dired $1"
   "gl" "(call-interactively 'magit-log-current)"
   "gs" "magit-status"
   "gc" "magit-commit"
   "bat" "nc/eshell-bat $1"
   "sudo" "eshell/sudo $*"
   "nm" "nc/enwc"
   "locate" "counsel-locate $1"
   "tm" "transmission"
   "cal" "calendar"
   "pass" "(pass)"
   "fd" "eshell/fd $1"
   "mountdrives" "nc/mount-drives")
  (setenv "PAGERQ" "cat")


  ;;
  ;; History (always save it)
  (add-hook! 'eshell-first-time-mode-hook
    (lambda () (add-hook 'eshell-pre-command-hook 'eshell-save-some-history))))


;;
;; Detach
(use-package! detached
  :load-path "~/src/detached"
  :ensure nil
  :config
  (setq detached-database-file (expand-file-name "detached.db" doom-etc-dir))

  ;; Customize popups
  (set-popup-rule! detached-shell-command-buffer :size 0.3 :side 'bottom :select t :autosave t)
  (set-popup-rule! detached-shell-output-buffer :size 0.3 :side 'bottom :select t :autosave t)
  (set-popup-rule! detached-sessions-buffer :size 0.3 :side 'bottom :select t :autosave t))


;; ;;
;; ;; Detach
;; (use-package! eshell-detach
;;   :after eshell
;;   :config
;;   (setq eshell-detach-max-file-name-length 60)
;;   (add-hook! 'eshell-first-time-mode-hook
;;     (defun eshell-detach-init-keymap-h ()
;;       (map! :map eshell-mode-map
;;             :ni [C-return] #'eshell-detach-attach
;;             :ni [S-return] #'eshell-detach-send-input))))


;;
;; Auto-suggestion
(use-package! esh-autosuggest
  :after eshell
  :config
  (add-hook 'eshell-mode-hook #'esh-autosuggest-mode)
  ;; enable fish-completion
  (use-package! fish-completion
    :config
    (setq fish-completion-fallback-on-bash-p t)
    (global-fish-completion-mode))
  ;; enable fallback, bash-completion
  (use-package! bash-completion
    :config
    (setq bash-completion-prog (executable-find "bash")))
  ;; Use ivy for completion (esh-autosuggest page)
  (setq ivy-do-completion-in-region t)
  ;; this is the default
  (defun setup-eshell-ivy-completion ()
    (map! :map eshell-mode-map
          [remap eshell-pcomplete] 'completion-at-point)
    ;; only if you want to use the minibuffer for completions instead of the
    ;; in-buffer interface
    (setq-local ivy-display-functions-alist
                (remq (assoc 'ivy-completion-in-region ivy-display-functions-alist)
                      ivy-display-functions-alist)))
  (add-hook 'eshell-mode-hook #'setup-eshell-ivy-completion))



;;
;; Term-mode (used for visual commands)
(after! term
  (add-hook 'term-mode-hook #'hide-mode-line-mode))


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
