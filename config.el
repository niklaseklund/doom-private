;;; config.el -*- lexical-binding: t; -*-

;; This file keeps my custom settings

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
        :desc "Zen writing" :n "z" #'writeroom-mode))
      ;; Vimesque keys
      (:prefix "["
      :n "SPC" #'evil-unimpaired/insert-space-above)
      (:prefix "]"
      :n "SPC" #'evil-unimpaired/insert-space-below)
      ;; Company
      :i "C-k"  #'+company/complete
      ;; Swedish escape
      :i "C-å" #'evil-escape)

