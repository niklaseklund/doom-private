;;;  -*- lexical-binding: t; -*-

;; Override other keys
(map! :n "C-h" 'evil-window-left
      :n "C-j" 'evil-window-down
      :n "C-k" 'evil-window-up
      :n "C-l" 'evil-window-right

      (:map evil-treemacs-state-map
        "C-h" 'evil-window-left
        "C-l" 'evil-window-right)

      (:map minibuffer-local-map
        "C-n" 'next-line-or-history-element
        "C-p" 'previous-line-or-history-element)

      (:map ranger-mode-map
        :nvmei ";=" #'+ora/ediff-files)

      (:leader
        ;; Open
        (:prefix "o"
          :desc "Open debugger" :n "d" #'+dap-hydra/body
          :desc "Open mail" :n "m" #'=notmuch)
        ;; Toggle
        (:prefix "t"
          :desc "Zen writing" :n "z" #'my/writeroom
          :desc "Wrap lines to fit screen" :n "v" #'visual-line-mode)
        ;; Search
        (:prefix "/"
          :desc "Search github code base" :n "g" #'my/github-search-code)))
