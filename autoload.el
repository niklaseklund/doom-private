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
