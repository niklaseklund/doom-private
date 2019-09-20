;;; +gdb.el -*- lexical-binding: t; -*-

;;
;; Emacs gdb
(after! gdb-mi
  (setq-default gdb-show-main nil))

(defun nc/gdb-mi ()
  "Use build folder from project root as start for selection of binary to debug."
  (let* ((start-directory (concat (projectile-project-root) "build/bin"))
         (file-name (read-file-name "Select binary to debug: " start-directory))
         (cwd (concat " --cd=" (projectile-project-root))))
    (gdb (concat "gdb -i=mi " file-name cwd))
    (gdb-many-windows)))

(defun nc/gdb-mi-new-frame ()
  "Use build folder from project root as start for selection of binary to debug."
  (interactive)
  (let ((command (split-string-and-unquote  "bspc node -d ^7 -f"))
        (cur-buffer buffer-file-name))
    ;; create a new frame
    (select-frame (make-frame))
    ;; send it to a new place
    (apply 'start-process "create-gdb-frame" nil command)
    ;; select a binary
    (find-file cur-buffer)
    (nc/gdb-mi)))

  (defun nc/gud-kill-all-buffers ()
    "Kill all gud buffers including Debugger, Locals, Frames, Breakpoints."
    ;; https://emacs.stackexchange.com/questions/7991/how-can-i-delete-all-the-gdb-related-windows-buffers-after-q-in-gdb-cli-window
    (interactive)
    (let ((gud-buffers '(gud-mode comint-mode gdb-locals-mode gdb-frames-mode gdb-breakpoints-mode)))
      (save-excursion
        (let ((count 0))
          (dolist (buffer (buffer-list))
            (set-buffer buffer)
            (when (member major-mode gud-buffers)
              (setq count (1+ count))
              (kill-buffer buffer)
              (delete-other-windows))) ;; fix the remaining two windows issue
          (message "Killed %i buffer(s)." count)))))
