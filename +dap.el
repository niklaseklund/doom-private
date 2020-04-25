;;;  -*- lexical-binding: t; -*-

;;
;; Debug Adapter Protocol
(after! dap-mode
  (set-company-backend! 'dap-ui-repl-mode 'company-dap-ui-repl))

;; Customize more after it's b
(with-eval-after-load 'dap-mode

  ;; customize company in repl-mode
  (add-hook 'dap-ui-repl-mode-hook
            (lambda ()
              (setq-local company-minimum-prefix-length 1)))

  ;; Display some debug windows on session startup
  (defun +dap/window-visible (b-name)
    "Return whether B-NAME is visible."
    (-> (-compose 'buffer-name 'window-buffer)
        (-map (window-list))
        (-contains? b-name)))

  (defun +dap/show-debug-windows (session)
    "Show debug windows."
    (let ((lsp--cur-workspace (dap--debug-session-workspace session)))
      (save-excursion
        (unless (+dap/window-visible dap-ui--locals-buffer)
          (dap-ui-locals))
        (unless (+dap/window-visible dap-ui--sessions-buffer)
          (dap-ui-sessions)))))

  ;; hide windows upon session termination
  (defun +dap/hide-debug-windows (session)
    "Hide debug windows when all debug sessions are dead."
    (unless (-filter 'dap--session-running (dap--get-sessions))
      (and (get-buffer dap-ui--sessions-buffer)
           (kill-buffer dap-ui--sessions-buffer))
      (and (get-buffer dap-ui--locals-buffer)
           (kill-buffer dap-ui--locals-buffer))
      (and (get-buffer "*Breakpoints*")
           (kill-buffer "*Breakpoints*"))))

  (add-hook 'dap-stopped-hook '+dap/show-debug-windows)
  (add-hook 'dap-terminated-hook '+dap/hide-debug-windows))
