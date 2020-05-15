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


;;;###autoload
(defun +dap/company-h ()
    (setq-local company-minimum-prefix-length 1))


;;;###autoload
(defun +zone/all-windows ()
    "Make zone clone the current buffer on to all windows before running zone."
    (interactive)
    (let* ((current-window (car (window-list)))
           (all-windows (window-list))
           (zone-buffer (get-buffer-create "*zone*")))
      ;; Make a copy of the current buffer,
      ;; this way it works even for buffers like eshell
      (+zone/buffer-copy "*zone-copy*")
      ;; Visit all windows and switch to the soon to be used zone-buffer
      (while all-windows
        (select-window (car all-windows))
        (setq all-windows (cdr all-windows))
        (switch-to-buffer "*zone*"))
      ;; Switch back to the starting window and change that to zone-copy buffer
      (select-window current-window)
      (switch-to-buffer "*zone-copy*")
      ;; Start zone on current buffer, creates the buffer *zone*
      (zone)))


;;;###autoload
(defun +zone/all-frames ()
    "Make current buffer be shown on all windows/frames and run zone."
    ;; TODO: integrate with bspwm later, don't need to run on frames that are
    ;; hidden, or on desktop that aren't currently shown.
    (interactive)
    (let ((start-frame (selected-frame))
          (start-window (selected-window))
          (current-frame)
          (windows)
          (zone-buffer (get-buffer-create "*zone*")))
      ;; copy the current window
      (+zone/buffer-copy "*zone-copy*")
      ;; As long as we haven't returned to the starting frame
      (while (not (eq current-frame start-frame))
        ;; get all the windows on the current frame
        (setq windows (window-list))
        (while windows
          (select-window (car windows))
          (setq windows (cdr windows))
          (switch-to-buffer "*zone*"))
        ;; switch to next frame
        (select-frame (next-frame current-frame nil))
        ;; make next frame current
        (setq current-frame (selected-frame)))
      ;; Switch back to the starting frame and window and change that to zone-copy buffer
      (select-frame start-frame)
      (select-window start-window)
      (switch-to-buffer "*zone-copy*")
      ;; Give the windows a chance to catch up, the position becomes inacurate otherwise
      (sit-for 0.1)
      ;; Start zone on current buffer, creates the buffer *zone*
      (zone)))


;;;###autoload
(defun +zone/lock-screen ()
    "Lock screen using (zone) and pyxtrlock calls +zone/all-windows and runs pyxtrlock."
    (interactive)
    (save-window-excursion
      (set-process-sentinel
       (start-process "my-lock" nil "my-lock")
       '(lambda (process event)
          ;; Kill the *zone-copy* upon unlocking (don't need it anymore)
          (kill-buffer "*zone-copy*")))
      (+zone/all-frames)))


;;;###autoload
(defun +zone/buffer-copy (new-buffer-name)
  "Copy a buffer content into a buffer named NEW-BUFFER-NAME."
  (interactive)
  (let ((text (buffer-substring (point-min) (point-max)))
        (outbuf (get-buffer-create new-buffer-name))
        (current-point (point)))
    (switch-to-buffer outbuf)
    (erase-buffer)
    (insert text)
    (+zone/set-region-writeable (point-min) (point-max))
    (goto-char current-point)))


;;;###autoload
(defun +zone/set-region-writeable (begin end)
    "Removes the read-only text property from the marked region."
    ;; See http://stackoverflow.com/questions/7410125
    (interactive "r")
    (let ((modified (buffer-modified-p))
          (inhibit-read-only t))
      (remove-text-properties begin end '(read-only t))
      (set-buffer-modified-p modified)))


;;;###autoload
(defun waylandp ()
  "Return t if environmental variable WAYLAND is 1."
  (string= (getenv "WAYLAND") "1"))


;;;###autoload
(defun +exwm/edit-compose-a (&optional no-copy)
  "Override partly original implementation.
Use `switch-to-buffer-other-window' to be able to customize the popup window."
  (let* ((title (exwm-edit--buffer-title (buffer-name)))
         (existing (get-buffer title))
         (inhibit-read-only t)
         (save-interprogram-paste-before-kill t)
         (selection-coding-system 'utf-8)) ; required for multilang-support
    (when (derived-mode-p 'exwm-mode)
      (setq exwm-edit--last-exwm-buffer (buffer-name))
      (unless (bound-and-true-p global-exwm-edit-mode)
        (global-exwm-edit-mode 1))
      (unless existing
        (exwm-input--fake-key ?\C-a)
        (exwm-input--fake-key ?\C-c)
        (let ((buffer (get-buffer-create title)))
          (with-current-buffer buffer
            (run-hooks 'exwm-edit-compose-hook)
            (exwm-edit-mode 1)
            (switch-to-buffer-other-window (get-buffer-create title))
            (setq-local header-line-format
                        (substitute-command-keys
                         "Edit, then exit with `\\[exwm-edit--finish]' or cancel with \ `\\[exwm-edit--cancel]'"))
            (unless no-copy
              (exwm-edit--yank))))))))

;;;###autoload
(defun +exwm/edit-compose-h ()
  "Customize exwm-edit."
  (markdown-mode))

;;;###autoload
(defun +exwm/rename-buffer-to-title-h ()
  "Make sure that the exwm buffers name convays its content."
  (exwm-workspace-rename-buffer
   (format "%s - %s" exwm-class-name exwm-title)))

;;;###autoload
(defun +exwm/update-class-h ()
  (unless (or (string-prefix-p "sun-awt-X11-" exwm-instance-name)
              (string= "gimp" exwm-instance-name)
              (string= "Firefox" exwm-class-name))
    (exwm-workspace-rename-buffer exwm-class-name)))

;;;###autoload
(defun +exwm/update-title-h ()
  (cond ((or (not exwm-instance-name)
             (string-prefix-p "sun-awt-X11-" exwm-instance-name)
             (string= "gimp" exwm-instance-name)
             (string= "Firefox" exwm-class-name))
         (exwm-workspace-rename-buffer exwm-title))))

;;;###autoload
(defun +exwm/change-screen-h ()
  (let ((xrandr-output-reaexp "\n\\([^ ]+\\) connected ")
        default-output)
    (with-temp-buffer
      (call-process "xrandr" nil t nil)
      (goto-char (point-min))
      (re-search-forward xrandr-output-reaexp nil 'noerror)
      (setq default-output (match-string 1))
      (forward-line)
      (if (not (re-search-forward xrandr-output-reaexp nil 'noerror))
          (call-process "xrandr" nil nil nil "--output" default-output "--auto")
        (call-process
         "xrandr" nil nil nil
         "--output" (match-string 1)
         "--right-of" default-output "--auto"
         "--output" default-output "--off")
        (setq exwm-randr-workspace-monitor-plist (list 1 (match-string 1)))))))
