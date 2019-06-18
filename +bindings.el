;;;  -*- lexical-binding: t; -*-

; (map!

;; Override other keys
(map! (:leader
        ;; Open
        (:prefix "o"
          :desc "Open mail" :n "m" #'=notmuch)
        ;; Toggle
        (:prefix "t"
          :desc "Zen writing" :n "z" #'writeroom-mode
          :desc "Wrap lines to fit screen" :n "v" #'visual-line-mode)
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
          :desc "Search github code base" :n "g" #'my/github-search-code))
      ;; Swedish escape
      :i "C-å" #'evil-normal-state
      ;; avy go to char
      :nvmei "C-;" #'evil-avy-goto-char-2)

;; Bring back the leader in pdf-tools by unbinding comma
(map!
 :after pdf-view
 :map pdf-view-mode-map
 :nvmei "," nil
 )

;; Modify ranger map
(map!
 :after ranger
 :map ranger-mode-map
 :nvmei ";=" #'+ora/ediff-files)

;; Easier window movement
(map! :n "C-h" 'evil-window-left
      :n "C-j" 'evil-window-down
      :n "C-k" 'evil-window-up
      :n "C-l" 'evil-window-right

      (:map evil-treemacs-state-map
        "C-h" 'evil-window-left
        "C-l" 'evil-window-right))

;; Align minbuffer-local-map with other minbuffer-maps
(map! (:map minibuffer-local-map
        "C-n" 'next-line-or-history-element
        "C-p" 'previous-line-or-history-element))

;; ;; Debugger map
(map!
 (:leader
   (:prefix "o"
     :desc "Open debugger" :n "d" #'hydra-debugger-control/body)))

;; Evil bindings
(map! (:map override
        :textobj "f" #'my/textobj-outer-defun #'my/textobj-outer-defun))

;;; ~/.doom.d/+bindings.el -*- lexical-binding: t; -*-
