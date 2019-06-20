;;;  -*- lexical-binding: t; -*-

;;
;; dap
(def-package! dap-mode
  :after lsp-mode
  :config
  ;; c++
  (require 'dap-gdb-lldb)
  (dap-gdb-lldb-setup)
  ;; python
  (require 'dap-python)
  ;; configure dap
  (set-company-backend! 'dap-ui-repl-mode 'company-dap-ui-repl)
  (dap-mode t)
(dap-ui-mode t))


;; aditional configuration
;; TODO: add binding for toggling the repl window, and for terminating debugging and closing all windows
(with-eval-after-load 'dap-mode
;; The following is from zsxh's configuration found at:
;; https://github.com/zsxh/emacs.d/blob/acb1e760656210de69a93a1a8ab670f7800a96e6/lisp/init-debugger.el
  ;; Debugger Cheatsheet
  (defhydra hydra-debugger-control (:color purple :hint nil :foreign-keys run)
    "
^Stepping^             ^Switch^           ^Breakpoints^          ^Eval^                        ^Debug
^^^^^^^^------------------------------------------------------------------------------------------------------------
_n_: Next          _ss_: Session          _bt_: Toggle          _ee_: Eval                     _dd_: Debug
_i_: Step in       _st_: Thread           _bd_: Delete          _er_: Eval region              _de_: Debug edit
_o_: Step out      _sf_: Stack frame      _ba_: Add             _es_: Eval thing at point      ^ ^
_c_: Continue      _sl_: List locals      _bc_: Set condition   _eii_: Inspect                 ^ ^
_r_: Restart frame _sb_: List breakpoints _bh_: Set hit count   _eir_: Inspect region          ^ ^
_Q_: Disconnect    _sS_: List sessions    _bl_: Set log message _eis_: Inspect thing at point  ^ ^
^ ^                 ^ ^                   _bD_: Delete all      ^ ^                            ^ ^
"
    ("n" dap-next)
    ("i" dap-step-in)
    ("o" dap-step-out)
    ("c" dap-continue)
    ("r" dap-restart-frame)
    ("Q" dap-disconnect)

    ("ss" dap-switch-session)
    ("st" dap-switch-thread)
    ("sf" dap-switch-stack-frame)
    ("sl" dap-ui-locals)
    ("sb" dap-ui-breakpoints)
    ("sS" dap-ui-sessions)

    ("bt" dap-breakpoint-toggle)
    ("bd" dap-breakpoint-delete)
    ("bD" dap-breakpoint-delete-all)
    ("ba" dap-breakpoint-add)
    ("bc" dap-breakpoint-condition)
    ("bh" dap-breakpoint-hit-condition)
    ("bl" dap-breakpoint-log-message)

    ("ee" dap-eval)
    ("er" dap-eval-region)
    ("es" dap-eval-thing-at-point)
    ("eii" dap-ui-inspect)
    ("eir" dap-ui-inspect-region)
    ("eis" dap-ui-inspect-thing-at-point)

    ("dd" dap-debug)
    ("de" dap-debug-edit-template)

    ("q" nil "quit"))

  ;; Display debug windows on session startup
  ;; https://github.com/emacs-lsp/dap-mode/wiki/HowTo:-Display-debug-windows-on-session-startup
  (add-hook 'dap-ui-repl-mode-hook
            (lambda ()
              (setq-local company-minimum-prefix-length 1)))

  (defun +dap/window-visible (b-name)
    "Return whether B-NAME is visible."
    (-> (-compose 'buffer-name 'window-buffer)
        (-map (window-list))
        (-contains? b-name)))

  (defun +dap/show-debug-windows (session)
    "Show debug windows."
    (let ((lsp--cur-workspace (dap--debug-session-workspace session)))
      (save-excursion
        ;; display locals
        (unless (+dap/window-visible dap-ui--locals-buffer)
          (dap-ui-locals))
        ;; display sessions
        (unless (+dap/window-visible dap-ui--sessions-buffer)
          (dap-ui-sessions))
        ;; display repl
        ;; (dap-ui-repl)
        )
      ))

  (defun +dap/hide-debug-windows (session)
    "Hide debug windows when all debug sessions are dead."
    (unless (-filter 'dap--session-running (dap--get-sessions))
      (and (get-buffer dap-ui--sessions-buffer)
           (kill-buffer dap-ui--sessions-buffer))
      (and (get-buffer "*dap-ui-repl*")
           (kill-buffer "*dap-ui-repl*"))
      ;; (and (get-buffer "*compilation*")
      ;;      (kill-buffer "*compilation*"))
      (and (get-buffer dap-ui--locals-buffer)
           (kill-buffer dap-ui--locals-buffer))))

  ;; (add-hook 'dap-stopped-hook (lambda (debug-session) (hydra-debugger-control/body)))
  (add-hook 'dap-stopped-hook '+dap/show-debug-windows)
  (add-hook 'dap-terminated-hook '+dap/hide-debug-windows)

  ;; add debug templates
  (dap-register-debug-template "Python :: Run Free"
                             (list :type "python"
                                   :args "deep-fry -s 10 --force-evaluation tools/fry/scenarios/monovision_mega_vor.json"
                                   :cwd (projectile-project-root)
                                   :target-module "-m tools.free.free"
                                   :request "launch"
                                   :name "Python :: Run Free")))
