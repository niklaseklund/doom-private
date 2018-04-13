;;;  -*- lexical-binding: t; -*-

; (map!

;; Override other keys
(map! (:leader
      ;; Git
      (:prefix "g"
      :n "s" #'magit-status
      :n "S" #'git-gutter:stage-hunk)
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
      ;; Open
      (:prefix "r"
       :n "i" #'ivy-resume)
      )
      ;; Vimesque keys
      (:prefix "["
      :n "SPC" #'evil-unimpaired/insert-space-above)
      (:prefix "]"
      :n "SPC" #'evil-unimpaired/insert-space-below)
      ;; Company
      :i "C-k"  #'+company/complete
      ;; Swedish escape
      :i "C-å" #'evil-escape

      ;; counsel
      (:after ivy
        (:map ivy-minibuffer-map
          "C-l"      #'ivy-call-and-recenter))
      )
