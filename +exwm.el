;;; ~/.config/doom/+x.el -*- lexical-binding: t; -*-

;;
;; Window Manager
(use-package! exwm
  :config

  ;; exwm buffers should be considered real buffers.
  (add-hook! exwm-mode #'doom-mark-buffer-as-real-h)

  ;; make window titles a bit more understandable.
  (defun jethro/exwm-rename-buffer-to-title () (exwm-workspace-rename-buffer (format "%s - %s" exwm-class-name exwm-title)))
  (add-hook 'exwm-update-title-hook 'jethro/exwm-rename-buffer-to-title)
  (add-hook 'exwm-update-class-hook
            (defun my-exwm-update-class-hook ()
              (unless (or (string-prefix-p "sun-awt-X11-" exwm-instance-name)
                          (string= "gimp" exwm-instance-name)
                          (string= "Firefox" exwm-class-name))
                (exwm-workspace-rename-buffer exwm-class-name))))
  (add-hook 'exwm-update-title-hook
            (defun my-exwm-update-title-hook ()
              (cond ((or (not exwm-instance-name)
                         (string-prefix-p "sun-awt-X11-" exwm-instance-name)
                         (string= "gimp" exwm-instance-name)
                         (string= "Firefox" exwm-class-name))
                     (exwm-workspace-rename-buffer exwm-title)))))



  ;;  keybindings.
  (map!
   "s-SPC" #'counsel-linux-app
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
    "s-SPC" #'counsel-linux-app
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
    "s-\\" #'ivy-pass))
  (exwm-enable))


;;
;; Edit text with Emacs
(use-package! exwm-edit
  :after exwm
  :config
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
