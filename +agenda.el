;;; +agenda.el -*- lexical-binding: t; -*-

;;
;; The org super agenda
(def-package! org-super-agenda
  :after org
  :init
  (setq org-super-agenda-groups
        '((:name "Today"
                 :time-grid t
                 :scheduled today)
          (:name "Due Today"
                 :deadline today)
          (:name "Important"
                 :priority "A")
          (:name "Due soon"
                 :deadline future)))
  :config
  (org-super-agenda-mode)
  ;; evilify bindings for header-map
  (map!
   :map org-super-agenda-header-map
   "k" #'org-agenda-previous-line
   "j" #'org-agenda-next-line
   "SPC" nil))
