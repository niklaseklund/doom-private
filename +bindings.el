;;;  -*- lexical-binding: t; -*-

(map!
 ;; create custom leader bindings
 (:leader
   :desc "Resume Avy" :n "\"" #'avy-resume
   (:prefix "o"
     :desc "Open brain" :n "b" #'org-brain-visualize
     :desc "Open mail" :n "m" (lambda! (notmuch-search "tag:inbox"))
     :desc "Open (pass-)store" :n "S" #'pass
     :desc "Open (ivy-pass-)store" :n "s" #'ivy-pass)
   (:prefix "s"
     :desc "Search Youtube" :n "y" #'ivy-youtube)
   (:prefix "w"
     :desc "Select window" :n "w" #'ace-window)))
