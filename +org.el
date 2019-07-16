;;;  -*- lexical-binding: t; -*-

;;
;; Org-mode
(after! org
  (setq outline-blank-line nil
        org-cycle-separator-lines 2
        org-log-done 'time
        org-directory "~/org"
        org-agenda-files '("~/org" "~/org/brain")
        org-latex-caption-above nil)
  ;; Org-links to emails
  (require 'org-notmuch))
;; Hooks
(add-hook 'org-mode-hook (lambda ()
                           (hl-line-mode -1)))
(add-hook 'org-babel-after-execute-hook 'org-redisplay-inline-images)
(add-hook 'org-mode-hook #'doom|disable-line-numbers)


;;
;; Notes
(def-package! org-noter
  :after org
  :config
  (setq org-noter-always-create-frame nil
        org-noter-auto-save-last-location t)
  (map! :localleader
        :map org-mode-map
        (:prefix-map ("n" . "org-noter")
          :desc "Open org-noter" :n "o" #'org-noter
          :desc "Kill org-noter session" :n "k" #'org-noter-kill-session
          :desc "Insert org-note" :n "i" #'org-noter-insert-note
          :desc "Insert precise org-note" :n "p" #'org-noter-insert-precise-note
          :desc "Sync current note" :n "." #'org-noter-sync-current-note
          :desc "Sync next note" :n "]" #'org-noter-sync-next-note
          :desc "Sync previous note" :n "[" #'org-noter-sync-prev-note)))


;;
;; Blog
(def-package! ox-hugo
  :defer t                      ;Auto-install the package from Melpa (optional)
  :after ox)


;;
;; Jira
(def-package! org-jira
  :defer t
  :config
  ;; Fix access
  (setq jiralib-url "https://jira.zenuity.com"
        org-jira-users `("Niklas Carlsson" . ,(shell-command-to-string "printf %s \"$(pass show work/zenuity/login | sed -n 2p | awk '{print $2}')\""))
        jiralib-token `("Cookie". ,(my/init-jira-cookie)))
  ;; Customization jira query
  (setq org-jira-custom-jqls
        '(
          (:jql " project = DUDE AND issuetype = Sub-task AND sprint in openSprints() AND sprint NOT IN futureSprints()"
                :limit 50
                :filename "dude-current-sprint-sub-tasks")
          (:jql " project = DUDE AND assignee = currentuser() order by created DESC "
                :limit 20
                :filename "dude-niklas")
          ))
  ;; Customize the flow
  (defconst org-jira-progress-issue-flow
    '(("To Do" . "In Progress")
      ("In Progress" . "Review")
      ("Review" . "Done"))))
