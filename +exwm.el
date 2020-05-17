;;; ~/.config/doom/+x.el -*- lexical-binding: t; -*-

;;
;; Window Manager
(use-package! exwm
  :config

  ;; exwm buffers should be considered real buffers.
  (add-hook! exwm-mode #'doom-mark-buffer-as-real-h)

  ;; configure
  (setq exwm-workspace-show-all-buffers t
        exwm-layout-show-all-buffers t)

  ;; Leader key access in exwm with super+space
  (setq doom-leader-alt-key "s-SPC")
  (push ?\s-\  exwm-input-prefix-keys)
  (evil-set-initial-state 'exwm-mode 'emacs)

  ;; Add super+space in normal mode as well
  (let ((map general-override-mode-map))
    (evil-define-key* '(normal visual motion) map (kbd "s-SPC") 'doom/leader))

  ;; Keep track of time
  (display-time-mode 1)

  ;; Workspace keybindings
  (mapcar (lambda (i)
            (exwm-input-set-key (kbd (format "s-%d" i))
                                `(lambda ()
                                   (interactive)
                                   (exwm-workspace-switch-create ,i))))
          (number-sequence 0 9))

  ;; Hooks
  (add-hook 'exwm-update-title-hook #'+exwm/rename-buffer-to-title-h)
  (add-hook 'exwm-update-class-hook #'+exwm/update-class-h)
  (add-hook 'exwm-update-title-hook #'+exwm/update-title-h)

  ;;  keybindings.
  (map!
   "s-;" #'counsel-linux-app
   "s-," #'+ivy/switch-buffer
   "s-c" #'org-capture
   "s-x" #'doom/open-scratch-buffer
   "s-l" #'evil-window-right
   "s-h" #'evil-window-left
   "s-k" #'evil-window-up
   "s-j" #'evil-window-down
   "s-u" #'winner-undo
   "s-m" #'doom/window-maximize-buffer
   "s-r" #'winner-redo
   "s-`" #'+eshell/toggle
   "s-q" #'evil-window-delete
   "s-\\" #'ivy-pass
   "s-o" #'evil-switch-to-windows-last-buffer
   "s-a" (λ! () (org-agenda nil "a"))
   "s-e" (lambda! (notmuch-search "tag:inbox"))
   "s-&" (lambda (command)
           (interactive (list (read-shell-command "$ ")))
           (start-process-shell-command command nil command))
   (:map exwm-mode-map
    "s-;" #'counsel-linux-app
    "s-," #'+ivy/switch-buffer
    "s-c" #'org-capture
    "s-x" #'doom/open-scratch-buffer
    "s-l" #'evil-window-right
    "s-h" #'evil-window-left
    "s-k" #'evil-window-up
    "s-j" #'evil-window-down
    "s-s" #'evil-window-split
    "s-v" #'evil-window-vsplit
    "s-q" #'evil-window-delete
    "s-m" #'doom/window-maximize-buffer
    "s-u" #'winner-undo
    "s-r" #'winner-redo
    "s-`" #'+eshell/toggle
    "s-e" (lambda! (notmuch-search "tag:inbox"))
    "s-a" (λ! () (org-agenda nil "a"))
    "s-o" #'evil-switch-to-windows-last-buffer
    "s-&" (lambda (command)
            (interactive (list (read-shell-command "$ ")))
            (start-process-shell-command command nil command))
    "s-\\" #'ivy-pass)))


;;
;; Screen setup
(use-package! exwm-randr
  :after exwm
  :config
  (add-hook 'exwm-randr-screen-change-hook '+exwm/change-screen-h)
  (exwm-randr-enable))


;;
;; Edit text with Emacs
(use-package! exwm-edit
  :config
  (setq exwm-edit-yank-delay 0.3
        exwm-edit-paste-delay 0.3)
  (add-hook 'exwm-edit-compose-hook #'+exwm/edit-compose-h)
  (advice-add 'exwm-edit--compose :before '+exwm/edit-compose-a)
  (set-popup-rule! "\\*exwm-edit*" :size 0.3 :side 'bottom :select t :autosave t))


;;
;; Keyboard layout
;; Automatic switch-back to English layout in normal mode
;; Requires `xkb-switch' to be installed on remote machine if used via TRAMP.
(let* ((normal-mode-keyboard-layout "us")
       (insert-mode-keyboard-layout normal-mode-keyboard-layout))
  (when (executable-find "xkb\-switch")
    (add-hook 'evil-insert-state-entry-hook
              (lambda () (start-process "switch-to-previous-language" nil "xkb-switch" "-s" insert-mode-keyboard-layout)))
    (add-hook 'evil-insert-state-exit-hook
              (lambda () (setq insert-mode-keyboard-layout (with-temp-buffer
                                                        (call-process "xkb-switch" nil t "-p")
                                                        (goto-char (point-min))
                                                        (string-trim-right (buffer-substring-no-properties (point) (line-end-position)))))
                (start-process "switch-to-normal" nil "xkb-switch" "-s" normal-mode-keyboard-layout)))))

;;
;; Screencast
(use-package! gif-screencast
  :defer t
  :config
  (with-eval-after-load 'gif-screencast
    (define-key gif-screencast-mode-map (kbd "<f12>") 'gif-screencast-toggle-pause)
    (define-key gif-screencast-mode-map (kbd "<f11>") 'gif-screencast-stop)))


;;
;; Lock screen
(use-package! zone
  :defer t
  :config
  (map!
   :desc "Lock and run" :nvi "<f2>" '+zone/lock-screen))
