;;; +syncthing.el -*- lexical-binding: t; -*-

(defun nc/syncthing-resolve-conflicts (directory)
  "Resolve all conflicts under given DIRECTORY."
  (interactive "D")
  (let* ((all (nc/syncthing--get-sync-conflicts directory))
        (chosen (nc/syncthing--pick-a-conflict all)))
    (nc/syncthing-resolve-conflict chosen)))


(defun nc/syncthing-show-conflicts-dired (directory)
  "Open dired buffer at DIRECTORY showing all syncthing conflicts."
  (interactive "D")
  (find-name-dired directory "*.sync-conflict-*"))


(defun nc/syncthing-resolve-conflict-dired (&optional arg)
  "Resolve conflict of first marked file in dired or close to point with ARG."
  (interactive "P")
  (let* ((chosen (car (dired-get-marked-files nil arg))))
    (nc/syncthing-resolve-conflict chosen)))


(defun nc/syncthing-resolve-conflict (conflict)
  "Resolve CONFLICT file using ediff."
  (let* ((normal (nc/syncthing--get-normal-filename conflict)))
    (nc/ediff-files
     (list conflict normal)
     `(lambda ()
       (when (y-or-n-p "Delete conflict file? ")
         (kill-buffer (get-file-buffer ,conflict))
         (delete-file ,conflict))))))



(defun nc/syncthing--get-sync-conflicts (directory)
  "Return a list of all sync conflict files in a DIRECTORY."
  (directory-files-recursively directory "\\.sync-conflict-"))


(defvar nc/syncthing--conflict-history
  "Completion conflict history")

(defun nc/syncthing--pick-a-conflict (conflicts)
  "Let user choose the next conflict from CONFLICTS to investigate."
  (completing-read "Choose the conflict to investigate: " conflicts
                   nil t nil nc/syncthing--conflict-history))


(defun nc/syncthing--get-normal-filename (conflict)
  "Get non-conflict filename matching the given CONFLICT."
  (replace-regexp-in-string "\\.sync-conflict-.*\\(\\..*\\)$" "\\1" conflict))


(defun nc/ediff-files (&optional files quit-hook)
  (interactive)
  (lexical-let ((files (or files (dired-get-marked-files)))
                (quit-hook quit-hook)
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
                      (when quit-hook (funcall quit-hook))
                      (set-window-configuration wnd))))
      (error "no more than 2 files should be marked"))))
