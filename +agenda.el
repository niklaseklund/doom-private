;;; +agenda.el -*- lexical-binding: t; -*-

;;
;; The org super agenda
(use-package! org-super-agenda
  :after org-agenda
  :config
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
  (org-super-agenda-mode)
  ;; evilify bindings for header-map
  (map!
   :map org-super-agenda-header-map
   "k" #'org-agenda-previous-line
   "j" #'org-agenda-next-line
   "SPC" nil)
  ;; Add bindings for org-agenda
  (map! :after org-agenda
   :map org-agenda-mode-map
   :desc "Avy goto line" :mn "C-'" #'avy-goto-line))
