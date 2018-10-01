;;; private/niklascarlsson/+functions.el -*- lexical-binding: t; -*-

(defun my/git-branch-match (name-regexp)
;; Creates a list of all git branches that matches in input NAME-REGEXP
  (with-temp-buffer (shell-command "git branch" t)
                    (goto-char (point-min))
                    (let ((branch-matches '()))
                      (while (not (eobp))
                        ;; use string-trim to remove starting and ending whitespace
                        (let ((branch (string-trim (thing-at-point 'line))))
                          (if (string-match name-regexp branch)
                              ;; append string to list
                              (setq branch-matches (cons (format "%s" branch) branch-matches))))
                        (forward-line 1))
                      branch-matches)))

(defun my/git-branch-show (name)
  ;; Display git-branches matching input regular expression NAME
  (interactive "sEnter git branch pattern: ")
  (message "%s" (my/git-branch-match name)))

(defun my/git-delete-branch (branch)
;; Deletes BRANCH from the current git project. Has guarding against removal of
;; master branch or current branch.
  (if (and (string-match "^[^*]" branch)
       (not (string-match "master" branch)))
      (progn
        (shell-command (format "git branch -D %s" branch))
        (format "Branch %s was removed" branch))))

(defun my/git-branch-delete-regexp (name-regexp)
  ;; Removes all git branches which matches NAME-REGEXP
  (interactive "sEnter git branch pattern: ")
  (let ((branches (my/git-branch-match name-regexp)))
    (mapcar 'my/git-delete-branch branches)))
