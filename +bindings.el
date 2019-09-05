;;;  -*- lexical-binding: t; -*-

(map! :n "C-h" 'evil-window-left
      :n "C-j" 'evil-window-down
      :n "C-k" 'evil-window-up
      :n "C-l" 'evil-window-right

      ;; evilify movement
      (:map evil-treemacs-state-map
        "C-h" 'evil-window-left
        "C-l" 'evil-window-right)

      ;; window
      (:leader
        (:prefix "w"
          :desc "Select window" :n "w" #'ace-window
          ))

      (:map minibuffer-local-map
        "C-n" 'next-line-or-history-element
        "C-p" 'previous-line-or-history-element)

      ;; use ediff for diffing in ranger
      (:map ranger-mode-map
        :nvmei ";=" #'+ora/ediff-files)

      ;; create custom leader bindings
      (:leader
        (:prefix "o"
          :desc "Open debugger" :n "d" #'+dap-hydra/body
          :desc "Open mail" :n "m" #'=notmuch
          :desc "Open brain" :n "b" #'org-brain-visualize
          :desc "Default browser" :n "B" #'browse-url-of-file
          :desc "Open (Password-)Store" :n "s" #'pass)))
