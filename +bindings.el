;;;  -*- lexical-binding: t; -*-

; (map!

;; Override other keys
(map! (:leader
      ;; File
      (:prefix "f"
      :desc "Find with fzf" :n "zf" #'counsel-fzf
      :desc "Find with projectile" :n "f" #'counsel-projectile-find-file)
      ;; Open
      (:prefix "o"
        :desc "Open mail" :n "m" #'=mail
        :desc "Open agenda" :n "a" #'org-agenda-show-daily)
      ;; Toggle
      (:prefix "t"
        :desc "Zen writing" :n "z" #'writeroom-mode
        :desc "Wrap lines to fit screen" :n "v" #'visual-line-mode
        :desc "Column-indicator" :n "c" #'fci-mode)
      ;; Noter
      (:prefix "n"
        :desc "Open org-noter" :n "o" #'org-noter
        :desc "Kill org-noter session" :n "k" #'org-noter-kill-session
        :desc "Insert org-note" :n "i" #'org-noter-insert-note
        :desc "Insert precise org-note" :n "p" #'org-noter-insert-precise-note
        :desc "Sync current note" :n "." #'org-noter-sync-current-note
        :desc "Sync next note" :n "]" #'org-noter-sync-next-note
        :desc "Sync previous note" :n "[" #'org-noter-sync-prev-note
        )
      ;; Window
      (:prefix "w"
        :desc "Close all other windows" :n "O" #'delete-other-windows
        :desc "Doom/window/enlargen" :n "o" #'doom/window-enlargen)
      ;; Navigation
      (:prefix "["
       :n "t" #'multi-term-prev)
      (:prefix "]"
       :n "t" #'multi-term-next)
      ;; Open
      (:prefix "r"
       :n "i" #'ivy-resume)
      )
      ;; Vimesque keys
      (:prefix "["
      :n "SPC" #'evil-unimpaired/insert-space-above)
      (:prefix "]"
      :n "SPC" #'evil-unimpaired/insert-space-below)
      ;; Swedish escape
      :i "C-å" #'evil-normal-state
      ;; counsel
      (:after ivy
        (:map ivy-minibuffer-map
          "C-l"      #'ivy-call-and-recenter))
      )

;; Bring back the leader in pdf-tools by unbinding comma
(map!
 :after pdf-view
 :map pdf-view-mode-map
 :nvmei "," nil
 )
