;; Zen experienced writing
(def-package! writeroom-mode)

;; Override other keys
(map! :leader
      :prefix "g"
      :n "s" #'magit-status
      :n "S" #'git-gutter:stage-hunk)

;; Different languages in insert and normal state
;; (setq lang_source "com.apple.keylayout.US")                     ;set default var lang_source for issw arg
;; (add-hook 'evil-insert-state-entry-hook                         ;what we do when enter insert mode
;;           (lambda ()
;;             (shell-command (concat "issw " lang_source))))      ;
;; ;
;; (add-hook 'evil-insert-state-exit-hook                          ;what we do when enter normal mode
;;           (lambda ()
;;             (setq lang_source (shell-command-to-string "issw"))
;;             (shell-command "issw com.apple.keylayout.US")))

;; (setq lang_source "com.apple.keylayout.US")                     ;set default var lang_source for issw arg
;; (add-hook 'evil-replace-state-entry-hook                         ;what we do when enter insert mode
;;           (lambda ()
;;             (shell-command (concat "issw " lang_source))))      ;
;; ;
;; (add-hook 'evil-replace-state-exit-hook                          ;what we do when enter normal mode
;;           (lambda ()
;;             (setq lang_source (shell-command-to-string "issw"))
;;             (shell-command "issw com.apple.keylayout.US")))
