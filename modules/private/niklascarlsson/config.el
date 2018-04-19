;; Basics
;; Tabs
(setq-default indent-tabs-mode nil)
(setq-default tab-width 4)
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
;; Customize flyspell
(setq flyspell-issue-message-flag nil)
;; Don't ask when killing emacs
(setq confirm-kill-emacs nil)
;; Remove trailing whitespaces
(add-hook 'before-save-hook 'delete-trailing-whitespace)
;; Add spell checking for comments in programming mode
(add-hook 'prog-mode-hook 'flyspell-prog-mode)

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
