;;;  -*- lexical-binding: t; -*-

;;
;; Org-mode
(after! org
  (setq outline-blank-line nil
        org-cycle-separator-lines 2
        org-log-done 'time
        org-directory "~/org"
        org-agenda-files '("~/org" "~/org/brain" "~/sync/org")
        org-latex-caption-above nil
        org-ditaa-jar-path "~/.emacs.d/.local/straight/repos/org/contrib/scripts/ditaa.jar")
  ;; Org-links to emails
  (require 'ol-notmuch))
;; Hooks
(add-hook 'org-mode-hook (lambda ()
                           (hl-line-mode -1)))
(add-hook 'org-mode-hook (lambda ()
                           (flycheck-mode -1))
          (add-hook 'org-mode-hook #'doom-disable-line-numbers-h))

;;
;; Links
(after! org-compat
          (defvar yt-iframe-format
            ;; You may want to change your width and height.
            (concat "<iframe width=\"1600\""
                    " height=\"1000\""
                    " src=\"https://www.youtube.com/embed/%s\""
                    " frameborder=\"0\""
                    " allowfullscreen>%s</iframe>"))
(org-add-link-type
 "yt"
 (lambda (handle)
   (browse-url
    (concat "https://www.youtube.com/embed/"
            handle)))
 (lambda (path desc backend)
   (cl-case backend
     (html (format yt-iframe-format
                   path (or desc "")))
     (latex (format "\href{%s}{%s}"
                    path (or desc "video")))))))


;;
;; Notes
(use-package! org-noter
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
;; LaTeX
(after! org
  (require 'ox-latex)
  ;; Make it possible to reference code blocks, figures etc.
  (require 'org-ref)
  ;; Fontify source code, use minted package opposed to listings.
  (setq org-latex-listings 'minted)
  ;; Specify options section, newfloat to minted to be able to setup the listing
  ;; environment for the code blocks properly
  (add-to-list 'org-latex-packages-alist '("section,newfloat" "minted" t))
  ;; Shell commands to run upon compilation
  (setq org-latex-pdf-process
        '("xelatex -shell-escape -interaction nonstopmode %f"
          "xelatex -shell-escape -interaction nonstopmode %f"
          "xelatex -shell-escape -interaction nonstopmode %f")))

;;
;; Hugo
(after! ox-hugo
  (defun org-hugo-publish ()
    "Publish changes to the blog."
    (interactive)
    (let ((default-directory "~/src/emacs-blog/"))
      ;; Build the project
      (shell-command "hugo")
      (let ((default-directory (concat default-directory "public/"))
            (commit-message (format "rebuilding site %s" (shell-command-to-string "echo -n $(date +%Y%m%d)"))))
        ;; Add modified files
        (apply #'call-process `("git" nil "*add-debug*" nil "add" "."))
        ;; Create commit
        (apply #'call-process `("git" nil "*commit-debug*" nil "commit" "-m" ,commit-message))
        ;; Publish
        (apply #'call-process `("git" nil "*push-debug*" nil "push" "origin" "master"))))))


;;
;; Jira
(use-package! ejira
  :init
  (setq jiralib2-url              "https://jira.zenuity.com"
        jiralib2-auth             'basic
        jiralib2-user-login-name  "niklas.carlsson@zenuity.com"
        jiralib2-token            nil

        ejira-org-directory       (expand-file-name "ejira" doom-etc-dir)
        ejira-projects            '("DUDE")

        ejira-priorities-alist    '(("Highest" . ?A)
                                    ("High"    . ?B)
                                    ("Medium"  . ?C)
                                    ("Low"     . ?D)
                                    ("Lowest"  . ?E))
        ejira-todo-states-alist   '(("To Do"       . 1)
                                    ("In Progress" . 2)
                                    ("Review"      . 3)
                                    ("Done"        . 4)))

  :config
  ;; Tries to auto-set custom fields by looking into /editmeta
  ;; of an issue and an epic.
  (add-hook 'jiralib2-post-login-hook #'ejira-guess-epic-sprint-fields)

  ;; They can also be set manually if autoconfigure is not used.
  ;; (setq ejira-sprint-field       'customfield_10001
  ;;       ejira-epic-field         'customfield_10002
  ;;       ejira-epic-summary-field 'customfield_10004)

  ;; Customize sprint
  (setq ejira-sprint-field 'customfield_10004
        ejira-scrum-project "DUDE")

  ;; Agenda integration
  (require 'ejira-agenda)
  (add-to-list 'org-agenda-files ejira-org-directory)

  ;; Add an agenda view with customized sections based on sprint content
  (org-add-agenda-custom-command
   '("j" "JIRA issues"
     ((ejira-jql "project = DUDE and status != Done and assignee = currentUser()"
                 ((org-agenda-overriding-header "Assigned to me")))
      (ejira-jql "project = DUDE and sprint = \"Rap-a-map\" and status != Done and assignee is EMPTY"
                 ((org-agenda-overriding-header "Unassigned")))
      (ejira-jql "project = DUDE and sprint = \"Rap-a-map\" and status != Done and assignee != currentUser()"
                 ((org-agenda-overriding-header "Others")))))))
