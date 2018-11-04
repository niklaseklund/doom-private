;;;  -*- lexical-binding: t; -*-

; (map!

;; Override other keys
(map! (:leader
      ;; File
      (:prefix "f"
      :desc "Find with projectile" :n "f" #'+helm/projectile-find-file)
      ;; Open
      (:prefix "o"
        :desc "Open mail" :n "m" #'=mail
        :desc "Open agenda" :n "a" #'org-agenda
        :desc "Open calendar" :n "c" #'=calendar
        :desc "Open brain" :n "b" #'org-brain-visualize)
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
        :desc "Sync previous note" :n "[" #'org-noter-sync-prev-note)
      ;; Search
      (:prefix "/"
        :desc "Helm rifle the org files" :n "o" #'helm-org-rifle)
      ;; Window
      (:prefix "w"
        :desc "Close all other windows" :n "O" #'delete-other-windows
        :desc "Doom/window/enlargen" :n "o" #'doom/window-enlargen)
      )
      ;; Vimesque keys
      (:prefix "["
      :n "SPC" #'evil-unimpaired/insert-space-above)
      (:prefix "]"
      :n "SPC" #'evil-unimpaired/insert-space-below)
      ;; Swedish escape
      :i "C-å" #'evil-normal-state
      )

;; Bring back the leader in pdf-tools by unbinding comma
(map!
 :after pdf-view
 :map pdf-view-mode-map
 :nvmei "," nil
 )

(map!
 (:after org-agenda
   (:map org-agenda-mode-map
     :mn                                      "t"     #'org-agenda-todo
     :mn                                      "j"     #'org-agenda-next-item
     :mn                                      "k"     #'org-agenda-previous-item
     :mn                                      "z"     #'org-agenda-view-mode-dispatch
     :mn                                      "o"     #'org-agenda-open-link
     :iemnv "C-k" #'evil-window-up
     :iemnv "C-j" #'evil-window-down
     :iemnv "C-h" #'evil-window-left
     :iemnv "C-l" #'evil-window-right
     (:prefix "d"
       :m         "s"     #'org-agenda-schedule
       (:desc "refile:"   :prefix "r"
         :desc "targets"        :m "t"  #'org-agenda-refile
         :desc "GTD"            :m "g"  (λ! (aj/org-agenda-refile-to-file-dont-ask +GTD))
         :desc "journal"        :m "j"  (λ! (aj/org-agenda-refile-to-datetree "~/org/JOURNAL.org"))
         :desc "file"           :m "f"  #'aj/org-agenda-refile-to-file
         :desc "project readme" :m "p"  #'aj/org-agenda-refile-to-project-readme
         :desc "someday"        :m "s"  (λ! (aj/org-agenda-refile-to-file-as-top-level +SOMEDAY))
         :desc "maybe"          :m "m"  (λ! (aj/org-agenda-refile-to-file-as-top-level +MAYBE))
         )
       )
     )
   )
 )

;;; ~/.doom.d/+bindings.el -*- lexical-binding: t; -*-
