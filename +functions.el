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
                   (macos . "Darwin")
                   (android . "Android")))
    (remaped-oses (mapcar (lambda (os) (cdr (assoc os os-map))) oses))
    (cond-func (lambda (os) (string-match-p os (shell-command-to-string "uname -a")))))
    (my/tangle-cond remaped-oses cond-func filename)))


(defun my/tangle-cond (conditionals cond-func &optional filename)
  ;; http://ergoemacs.org/misc/emacs_lisp_some_and_every.html
  (require 'cl-extra)
  (let ((tangle-file "no"))
    (when (cl-some #'identity (mapcar cond-func conditionals))
      (if filename
          (setq tangle-file filename)
        (setq tangle-file (my/tangle-get-filename)))
      (when (null tangle-file)
        (error "You haven't specified a tangle filename")))
    tangle-file))

(defun my/tangle-app (apps &optional filename)
  "The input APPS is a list of valid applications."
  (let* ((cond-func (lambda (app) (executable-find  (prin1-to-string app)))))
    (my/tangle-cond apps cond-func filename)))

(defun my/tangle-not-app (apps &optional filename)
  "The input APPS is a list of invalid applications."
  (let* ((cond-func (lambda (app) (not (executable-find  (prin1-to-string app))))))
    (my/tangle-cond apps cond-func filename)))

(defun my/github-search-code (begin end)
  "Search GitHub's code base. If the function is called with a visual selection
that will be used as the search query. Multiple lines will be quoted separately.
If no visual selection has been made the user is prompted for search words"
  (interactive "r")
  (let ((language nil)
        (query nil)
        (unreserved `(,@url-unreserved-chars 32))
        (bool-vec (make-vector 256 nil)))
  (mapcar (lambda (index) (aset bool-vec index t)) unreserved)
    ;; Set search query
    (if (not mark-active)
        (setq query (read-string "Search for: "))
      ;; Removing potential line ending with string-trim, adding quotes with
      ;; prin1-to-string
      (setq query (prin1-to-string (string-trim (buffer-substring-no-properties begin end)))))
    ;; Remove new lines inside the string. Make it so each line is considered
    ;; it's own entity
    (setq query (replace-regexp-in-string "\n" "\" \"" query))
    (setq language
          (cond ((string-match-p "emacs-lisp-mode" (symbol-name major-mode)) "Emacs+Lisp")
                ((string-match-p "python-mode" (symbol-name major-mode)) "Python")
                ((string-match-p (regexp-quote "c++-mode") (symbol-name major-mode)) "C++")
                ;; Default is Emacs Lisp
                (t "Emacs+Lisp")))
    ;; hexify query and language
    (setq query (url-hexify-string query bool-vec))
    (setq language (url-hexify-string language bool-vec))
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


;; https://oremacs.com/2017/03/18/dired-ediff/
(defun +ora/ediff-files ()
  (interactive)
  (let ((files (dired-get-marked-files))
        (wnd (current-window-configuration)))
    (if (<= (length files) 2)
        (let ((file1 (car files))
              (file2 (if (cdr files)
                         (cadr files)
                       (read-file-name
                        "file: "
                        (dired-dwim-target-directory)))))
          (if (file-newer-than-file-p file1 file2)
              (ediff-files file2 file1)
            (ediff-files file1 file2))
          (add-hook 'ediff-after-quit-hook-internal
                    (lambda ()
                      (setq ediff-after-quit-hook-internal nil)
                      (set-window-configuration wnd))))
      (error "no more than 2 files should be marked"))))

(defun my/init-jira-cookie ()
  (let* ((token nil)
         (id nil)
         (header (prin1-to-string "Content-Type: application/json"))
         (name (prin1-to-string (shell-command-to-string "printf %s \"$(pass show work/zenuity/login | sed -n 2p | awk '{print $2}')\"")))
         (passwd (prin1-to-string (shell-command-to-string "printf %s \"$(pass show work/zenuity/login | sed -n 1p)\""))))

    (with-temp-buffer (shell-command (concat (format "curl -s -H %s " header)
                                             (format "-c - ")
                                             (format "-d \'{\"username\":%s, \"password\":%s}\' " name passwd)
                                             "-X POST https://jira.zenuity.com/rest/auth/latest/session") t)
                      (goto-char (point-min))
                      (search-forward-regexp (regexp-quote "atlassian.xsrf.token"))
                      (setq token (car (last (split-string (string-trim (thing-at-point 'line))))))
                      (forward-line 1)
                      (setq id (car (last (split-string (string-trim (thing-at-point 'line))))))
                      (format "atlasian.xsrf.token=%s;JSESSIONID=%s" token id))))


(defun my/eshell-bat (file)
  "Like `cat' but output with Emacs syntax highlighting."
  (with-temp-buffer
    (insert-file-contents file)
    (let ((buffer-file-name file))
      (delay-mode-hooks
        (set-auto-mode)
        (if (fboundp 'font-lock-ensure)
            (font-lock-ensure)
          (with-no-warnings
            (font-lock-fontify-buffer)))))
    (buffer-string)))


;; http://seanbowman.me/blog/emacs-evil-function-objects/
;; (evil-define-text-object my/textobj-inner-defun (count &optional beg end type)
;;   (save-excursion
;;     (mark-defun)
;;     (re-search-forward "{")
;;     (exchange-point-and-mark)
;;     (re-search-backward "}")
;;     (evil-range (region-beginning) (region-end) type :expanded t)))
(evil-define-text-object my/textobj-outer-defun (count &optional beg end type)
  :type line
  (save-excursion
    (mark-defun)
    (if (looking-at "[:space:]*$")
        (forward-line))
    (exchange-point-and-mark)
    (unless (save-excursion
              (forward-line)
              (looking-at "[:space:]*$"))
      (forward-line))
    (evil-range (region-beginning) (region-end) type :expanded t)))
