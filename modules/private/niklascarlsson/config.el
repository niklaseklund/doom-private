;; Zen experienced writing
(def-package! writeroom-mode)

;; Custom functions
(defun evil-unimpaired/insert-space-above (count)
  (interactive "p")
  (dotimes (_ count) (save-excursion (evil-insert-newline-above))))

(defun evil-unimpaired/insert-space-below (count)
  (interactive "p")
  (dotimes (_ count) (save-excursion (evil-insert-newline-below))))

;; Define flycheck
(setq company-irony-c-headers--compiler-executable "clang++-4.0")
;; Don't ask when killing emacs
(setq confirm-kill-emacs nil)
;; Remove trailing whitespaces
(add-hook 'before-save-hook 'delete-trailing-whitespace)


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
      (:prefix "/"
          :desc "Swiper in buffer"          :n "/" #'swiper)
      ;; Toggle
      (:prefix "t"
        :desc "Zen writing" :n "z" #'writeroom-mode
        :desc "Wrap lines to fit screen" :n "v" #'visual-line-mode)
      ;; Window
      (:prefix "w"
        :desc "Close all other windows" :n "O" #'delete-other-windows
        :desc "Doom/window/enlargen" :n "o" #'doom/window-enlargen)
      ;; Navigation
      (:prefix "["
       :n "t" #'multi-term-prev)
      (:prefix "]"
       :n "t" #'multi-term-next)
      )
      ;; Vimesque keys
      (:prefix "["
      :n "SPC" #'evil-unimpaired/insert-space-above)
      (:prefix "]"
      :n "SPC" #'evil-unimpaired/insert-space-below)
      ;; Company
      :i "C-k"  #'+company/complete
      ;; Swedish escape
      :i "C-å" #'evil-escape)

;; Set settings
(after! org
  (setq outline-blank-line nil)
  (setq org-cycle-separator-lines 2)
  (setq org-log-done 'time))

;; Hooks
;; Turn of hihglight line in org-mode
 (add-hook 'org-mode-hook (lambda () (hl-line-mode -1)))
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

;; Append the git-commit hook
(add-hook 'git-commit-setup-hook 'git-commit-turn-on-flyspell)
 ;; Limit commit message summary to 50 columns, and wrap content after 72 columns.
(def-package! git-commit
  :init (add-hook 'git-commit-mode-hook
                  '(lambda ()
                     (setq-local git-commit-summary-max-length 50)
                     (setq-local fill-column 72)))
  )
