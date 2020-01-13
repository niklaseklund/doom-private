;;; autoload.el -*- lexical-binding: t; -*-

;;;###autoload
(defun nc/transmission-start-daemon-a ()
      "Start the transmission daemon."
      (unless (member "transmission-da"
                      (mapcar
                       (lambda (pid) (alist-get 'comm (process-attributes pid)))
                       (list-system-processes)))
        (call-process "transmission-daemon")
        (sleep-for 1)))
