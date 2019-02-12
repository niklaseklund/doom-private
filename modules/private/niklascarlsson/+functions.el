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


(defun my/docker-match (name-regexp)
  ;; return the name of the last docker image which matches the input
  ;; NAME-REGEXP
  (with-temp-buffer (shell-command "docker ps" t)
                    (goto-char (point-min))
                    (let ((name-match '()))
                      (while (not (eobp))
                        (let ((current-name (string-trim (thing-at-point 'line))))
                          (if (string-match name-regexp current-name)
                              (progn
                                (end-of-line)
                                (setq name-match (format "%s" (thing-at-point 'symbol))))))
                        (forward-line 1))
                      name-match)))

(defun my/docker-path (name-regexp  &optional extended-path)
  (if extended-path
      (format "/docker:%s:/%s" (my/docker-match name-regexp) extended-path)
    (format "/docker:%s:/" (my/docker-match name-regexp))))


(defun my/org-babel-previous-session ()
  "Find the previous src code block which contains the session argument and
return it together with the language"
  (interactive)
  (save-excursion
    (let ((session nil)
          (language nil))
      (while (and (re-search-backward org-babel-src-block-regexp nil t) (not session))
        (goto-char (match-beginning 0))
        (let* ((block-info (org-babel-get-src-block-info))
               (block-lang (nth 0 block-info))
               (block-params (nth 2 block-info))
               (block-session (cdr (assoc :session block-params))))
          (when (not (string= "none" block-session))
            (setq session block-session)
            (setq language block-lang))))
      (format "%s :session %s" language session))))


(defun my/firefox-profile-directory ()
  "Find the path to the Firefox profile directory where settings recide."
  (let ((profile-directory '())
        (firefox-path (expand-file-name "~/.mozilla/firefox/")))
    (with-temp-buffer (shell-command (concat "ls " firefox-path) t)
                      (goto-char (point-min))
                      (while (not (eobp))
                          (let ((content (string-trim (thing-at-point 'line))))
                            (if (string-match "default" content)
                                (setq profile-directory (concat firefox-path content))))
                          (forward-line 1)))
    profile-directory))


(defun my/tangle-os (oses &optional filename)
  "The input OSES is a list of valid operating systems. The values are remaped
to a regexp that will be used in the conditional lambda function"
  (let* ((os-map '((arch . "arch")
                      (ubuntu . "Ubuntu")
                      (macos . "Darwin")))
    (remaped-oses (mapcar (lambda (os) (cdr (assoc os os-map))) oses))
    (cond-func (lambda (os) (string-match-p os (shell-command-to-string "uname -a")))))
    (my/tangle-cond remaped-oses cond-func filename)))


(defun my/tangle-cond (conditionals cond-func &optional filename)
  ;; http://ergoemacs.org/misc/emacs_lisp_some_and_every.html
  (require 'cl-extra)
  (let ((tangle-file "none"))
    (when (cl-some #'identity (mapcar cond-func conditionals))
      (if filename
          (setq tangle-file filename)
        (setq tangle-file (my/tangle-get-filename)))
      (when (null tangle-file)
        (error "You haven't specified a tangle filename")))
    tangle-file))


(defun my/github-search-code (begin end)
  "Search GitHub's code base. If the function is called with a visual selection
that will be used as the search query. Otherwise the user is prompted for search
words"
  ;; Improvements
  ;; 1) Should region text be treated as one word or many? Should wrap it in
  ;; quotes in case of one
  (interactive "r")
  (let ((language nil)
        (query nil))
    ;; Set search query
    (if (eq begin end)
        (setq query (read-string "Search for: "))
      (setq query (buffer-substring-no-properties begin end)))
    (setq query (replace-regexp-in-string " " "+" query))
    ;; Set language for query
    (setq language
          (cond ((string-match-p "emacs-lisp-mode" (symbol-name major-mode)) "Emacs+Lisp")
                ((string-match-p "python-mode" (symbol-name major-mode)) "Python")
                ;; Default is Emacs Lisp
                (t "Emacs+Lisp")))
    (browse-url-firefox (format "https://github.com/search?l=%s&q=%s&type=Code" language query))
    (quiet! (shell-command "wmctrl -x -a Navigator.Firefox"))))


(defun my/tangle-get-filename ()
  "Returns the filename for tangling that applies to the current subtree."
  (let ((subtree-properties nil)
        (tangle-param nil))
    ;; org-babel-params-from-properties returns the properties that applies to
    ;; the current subtree. It will handle a tree structure with multiple
    ;; definitions of properties.
    (setq subtree-properties (org-babel-params-from-properties))
    (setq tangle-param (cdr (assoc :tangle (car subtree-properties))))
    tangle-param))
