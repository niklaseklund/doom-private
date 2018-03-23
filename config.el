;; Zen experienced writing
(def-package! writeroom-mode)

;; Custom functions
(defun evil-unimpaired/insert-space-above (count)
  (interactive "p")
  (dotimes (_ count) (save-excursion (evil-insert-newline-above))))

(defun evil-unimpaired/insert-space-below (count)
  (interactive "p")
  (dotimes (_ count) (save-excursion (evil-insert-newline-below))))

;; Override other keys
(map! (:leader
      ;; Git
      (:prefix "g"
      :n "s" #'magit-status
      :n "S" #'git-gutter:stage-hunk)
      ;; Find
      (:prefix "f"
          :desc "Find file in project"      :n "f" #'projectile-find-file
          :desc "Find string in project"    :n "s" #'counsel-rg)
      ;; Toggle
      (:prefix "t"
        :desc "Zen writing" :n "z" #'writeroom-mode
        :desc "Wrap lines to fit screen" :n "v" #'visual-line-mode)
      ;; Window
      (:prefix "w"
        :desc "Close all other windows" :n "o" #'delete-other-windows
        :desc "Doom/window/enlargen" :n "O" #'doom/window-enlargen))
      ;; Vimesque keys
      (:prefix "["
      :n "SPC" #'evil-unimpaired/insert-space-above)
      (:prefix "]"
      :n "SPC" #'evil-unimpaired/insert-space-below)
      ;; Company
      :i "C-k"  #'+company/complete
      ;; Swedish escape
      :i "C-å" #'evil-escape)

;; Hooks
;; Automatically switch back to English in normal mode
(cond (IS-LINUX
  (setq prev_lang (substring (shell-command-to-string
                              "gsettings get org.gnome.desktop.input-sources current")
                             7 -1))
  (add-hook 'evil-insert-state-entry-hook
            (lambda ()
              (shell-command (concat
                              "/usr/bin/gsettings set org.gnome.desktop.input-sources current " prev_lang)
                             )
              )
            )

  (add-hook 'evil-insert-state-exit-hook
            (lambda ()
              (setq prev_lang (substring (shell-command-to-string
                                          "gsettings get org.gnome.desktop.input-sources current")
                                         7 -1))
              (shell-command (concat
                              "/usr/bin/gsettings set org.gnome.desktop.input-sources current 1")
                             )
              )
            )
))
