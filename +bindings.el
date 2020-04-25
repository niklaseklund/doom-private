;;;  -*- lexical-binding: t; -*-

(map! :n "C-h" 'evil-window-left
      :n "C-j" 'evil-window-down
      :n "C-k" 'evil-window-up
      :n "C-l" 'evil-window-right

      ;; Use this binding to harmonize with ivy/swiper
      :n "C-'" 'avy-goto-line

      ;; window
      (:leader
        (:prefix "w"
          :desc "Select window" :n "w" #'ace-window))

      ;; create custom leader bindings
      (:leader
        :desc "Resume Avy" :n "\"" #'avy-resume
        (:prefix "o"
          :desc "Open brain" :n "b" #'org-brain-visualize
          :desc "Open mail" :n "m" (lambda! (notmuch-search "tag:inbox"))
          :desc "Open (pass-)store" :n "S" #'pass
          :desc "Open (ivy-pass-)store" :n "s" #'ivy-pass)
        (:prefix "s"
          :desc "Search Youtube" :n "y" #'ivy-youtube)))
