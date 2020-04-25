;;; private/niklascarlsson/+functions.el -*- lexical-binding: t; -*-

(defun nc/init-jira-cookie ()
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


(defun nc/multi-screen-setup-p ()
  ( > (string-to-number (shell-command-to-string "printf %s \"$(xrandr -q | grep -c ' connected')\"")) 1))

(defun nc/buffer-copy (new-buffer-name)
  "Copy a buffer content into a buffer named NEW-BUFFER-NAME."
  (interactive)
  (let ((text (buffer-substring (point-min) (point-max)))
        (outbuf (get-buffer-create new-buffer-name))
        (current-point (point)))
    (switch-to-buffer outbuf)
    (erase-buffer)
    (insert text)
    (nc/set-region-writeable (point-min) (point-max))
    (goto-char current-point)))


(defun nc/set-region-writeable (begin end)
  "Removes the read-only text property from the marked region."
  ;; See http://stackoverflow.com/questions/7410125
  (interactive "r")
  (let ((modified (buffer-modified-p))
        (inhibit-read-only t))
    (remove-text-properties begin end '(read-only t))
    (set-buffer-modified-p modified)))
