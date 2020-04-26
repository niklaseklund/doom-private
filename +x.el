;;; ~/.config/doom/+x.el -*- lexical-binding: t; -*-

;;
;; Keyboard layout
;; Automatic switch-back to English layout in normal mode
;; Requires `xkb-switch' to be installed on remote machine if used via TRAMP.
(let* ((normal-mode-keyboard-layout "us")
       (insert-mode-keyboard-layout normal-mode-keyboard-layout))
  (when (executable-find "xkb\-switch")
    (add-hook 'evil-insert-state-entry-hook
              (lambda () (start-process "switch-to-previous-language" nil "xkb-switch" "-s" insert-mode-keyboard-layout)))
    (add-hook 'evil-insert-state-exit-hook
              (lambda () (setq insert-mode-keyboard-layout (with-temp-buffer
                                                        (call-process "xkb-switch" nil t "-p")
                                                        (goto-char (point-min))
                                                        (string-trim-right (buffer-substring-no-properties (point) (line-end-position)))))
                (start-process "switch-to-normal" nil "xkb-switch" "-s" normal-mode-keyboard-layout)))))

;;
;; Screencast
(use-package! gif-screencast
  :defer t
  :config
  (with-eval-after-load 'gif-screencast
    (define-key gif-screencast-mode-map (kbd "<f12>") 'gif-screencast-toggle-pause)
    (define-key gif-screencast-mode-map (kbd "<f11>") 'gif-screencast-stop)))


;;
;; Lock screen
(use-package! zone
  :defer t
  :config
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

  (defun +zone/set-region-writeable (begin end)
    "Removes the read-only text property from the marked region."
    ;; See http://stackoverflow.com/questions/7410125
    (interactive "r")
    (let ((modified (buffer-modified-p))
          (inhibit-read-only t))
      (remove-text-properties begin end '(read-only t))
      (set-buffer-modified-p modified)))

  ;; bindings
  (map!
   :desc "Lock and run" :nvi "<f2>" '+zone/lock-screen))
