;;; autoload.el -*- lexical-binding: t; -*-

;;;###autoload
(defun +transmission-start-daemon-a ()
  "Start the transmission daemon.
The function is intended to be used to advice the transmission command. Ensuring
that the daemon always runs when needed."
  (unless (member "transmission-da"
                  (mapcar
                   (lambda (pid) (alist-get 'comm (process-attributes pid)))
                   (list-system-processes)))
    (call-process "transmission-daemon")
    (sleep-for 1)))


;;;###autoload
(defun +bluetooth-pop-to-buffer-a ()
  "Use `pop-to-buffer' instead of `switch-to-buffer' to open buffer.'"
  (pop-to-buffer "*Bluetooth*"))


;;;###autoload
(defun +compile-notify-finish-h (_buffer string)
    "Conditionally show a notification when a compilation finish.
Always notify about compilations that are failing. Notify about successful ones
if they have been running for enough time."
    (message "hook compile called")
    (let* ((duration-threshold 10)
           (compilation-end-time (time-to-seconds))
           (compile-duration (float-time (time-subtract compilation-end-time compile-start-time))))
      (if (string-match "^finished" string)
          (when (> compile-duration duration-threshold)
            (alert "Compilation finished OK!" :title "Compilation Successful" :severity 'moderate :category 'compile :id 'compile-ok))
        (alert "Compilation Failed" :title "Compilation Failed" :severity 'high :category 'compile :id 'compile-fail))))


;;;###autoload
(defun +compile-start-time-h (_process)
    "Record the start time of the compilation."
    (setq-local compile-start-time (time-to-seconds)))


;;;###autoload
(defun +eshell/info-manual ()
  "Select and open an info manual."
  (info "dir")
  (call-interactively #'Info-menu))


;;;###autoload
(defun +eshell/fd (regexp)
  "Recursively find items matching REGEXP and open in dired."
  (fd-dired default-directory regexp))


;;;###autoload
(defun +eshell/bat (file)
    "Like `cat' but output the content of FILE with Emacs syntax highlighting."
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


;;;###autoload
(defun +dired/ediff-files ()
  "Ediff two marked files in a dired buffer.
The function originates from, https://oremacs.com/2017/03/18/dired-ediff/"
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


;;;###autoload
(defun +emacs-conflict/show-conflicts-dired-at-point ()
    "Dired show conflicts at point."
    (interactive)
    (emacs-conflict-show-conflicts-dired (dired-get-file-for-visit)))


;;;###autoload
(defun nc/updatedb ()
  "Update the database that locate uses."
  (interactive)
  (let ((default-directory  "/sudo:root@localhost:"))
    (shell-command "sudo updatedb")))


;;;###autoload
(defun +notmuch-dont-confirm-on-kill-process-a (orig-fn &rest args)
  "Don't prompt for confirmation when killing notmuch sentinel."
  (let (confirm-kill-processes)
    (apply orig-fn args)))


;;;###autoload
(defun nc/org-babel-previous-session ()
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


;;;###autoload
(defun nc/blog-publish ()
  "Publish changes to the blog.
Builds the project with command `hugo' then creates a commit and pushes it
upstreams."
  (interactive)
  (let ((default-directory (projectile-project-root)))
    (shell-command "hugo")
    (let ((default-directory (concat default-directory "public/"))
          (commit-message (format "rebuilding site %s" (shell-command-to-string "echo -n $(date +%Y%m%d)"))))
      (apply #'call-process `("git" nil nil nil "add" "."))
      (apply #'call-process `("git" nil nil nil "commit" "-m" ,commit-message))
      (apply #'call-process `("git" nil nil nil "push" "origin" "master")))))


;;;###autoload
(defun +dap/python-poetry-a (orig-fn &rest args)
"Use the Python binary from the current virtual environment."
(if (getenv "VIRTUAL_ENV")
      (executable-find (car args))
    (apply orig-fn args)))


;;;###autoload
(defun +python/open-poetry-repl-a (orig-fn &rest args)
  "Use the Python binary from the current virtual environment."
  (if (getenv "VIRTUAL_ENV")
      (let ((python-shell-interpreter (executable-find "python")))
        (apply orig-fn args))
    (apply orig-fn args)))


;;;###autoload
(defun +dap/hide-debug-windows-h (_session)
    "Hide debug windows when all debug sessions are dead."
    (unless (-filter 'dap--session-running (dap--get-sessions))
      (and (get-buffer dap-ui--sessions-buffer)
           (kill-buffer dap-ui--sessions-buffer))
      (and (get-buffer dap-ui--locals-buffer)
           (kill-buffer dap-ui--locals-buffer))
      (and (get-buffer "*Breakpoints*")
           (kill-buffer "*Breakpoints*"))))


;;;###autoload
(defun +dap/window-visible (b-name)
    "Return whether B-NAME is visible."
    (-> (-compose 'buffer-name 'window-buffer)
        (-map (window-list))
        (-contains? b-name)))


;;;###autoload
(defun +dap/show-debug-windows-h (session)
    "Show debug windows."
    (let ((lsp--cur-workspace (dap--debug-session-workspace session)))
      (save-excursion
        (unless (+dap/window-visible dap-ui--locals-buffer)
          (dap-ui-locals))
        (unless (+dap/window-visible dap-ui--sessions-buffer)
          (dap-ui-sessions)))))
