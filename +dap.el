;;;  -*- lexical-binding: t; -*-

;;
;; Debug Adapter Protocol
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

;; Customize more after it's b
(after! 'dap-mode
  ;; customize company in repl-mode
  (add-hook 'dap-ui-repl-mode-hook
            (lambda ()
              (setq-local company-minimum-prefix-length 1)))

  ;; Override the built-in dap hydra to add additional keybindings
  (defhydra +dap-hydra (:color purple :hint nil :foreign-keys run)
    "
^Stepping^             ^Switch^           ^Breakpoints^          ^Eval^                        ^Debug
^^^^^^^^--------------------------------------------------------------------------------------------------------------------
_n_: Next          _ss_: Session          _bt_: Toggle          _ee_: Eval                     _dd_: Debug
_i_: Step in       _st_: Thread           _bd_: Delete          _er_: Eval region              _dr_: Debug recent
_o_: Step out      _sf_: Stack frame      _ba_: Add             _es_: Eval thing at point      _dl_: Debug last
_c_: Continue      _sl_: List locals      _bc_: Set condition   _eii_: Inspect                 _dt_: Debug edit template
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
    ("dr" dap-debug-recent)
    ("dl" dap-debug-last)
    ("dt" dap-debug-edit-template)

    ("q" nil "quit"))

  ;; Display some debug windows on session startup
  ;; https://github.com/emacs-lsp/dap-mode/wiki/HowTo:-Display-debug-windows-on-session-startup
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
  (add-hook 'dap-terminated-hook '+dap/hide-debug-windows)

  ;; add custom debug templates
  (dap-register-debug-template "Python::Run Free"
                               (list :type "python"
                                     :args "deep-fry -s 10 --force-evaluation tools/fry/scenarios/monovision_mega_vor.json"
                                     :cwd (projectile-project-root)
                                     :target-module "-m tools.free.free"
                                     :request "launch"
                                     :name "Python :: Run Free"))
  (dap-register-debug-template "GDB::Run Stringent"
                               (list :type "gdb"
                                     :request "launch"
                                     :name "GDB::Run"
                                     :target nil
                                     :cwd (projectile-project-root))))


;;
;; Emacs gdb
(defun my/gdb-mi ()
  "Use build folder from project root as start for selection of binary to debug."
  (let* ((start-directory (concat (projectile-project-root) "build"))
         (file-name (read-file-name "Select binary to debug: " start-directory)))
    (gdb (concat "gdb -mi " file-name))))

(defun my/open-debug ()
  "A function that opens a debug hydra based on major mode."
  (interactive)
  (cond ((eq major-mode 'python-mode) (hydra-debugger-control/body))
        ((eq major-mode 'c++-mode) (message "Calling the C++ hydra"))
        (t (message "Not implemented"))))

(defhydra gdb-hydra (:color purple :hint nil :foreign-keys run)
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
  ("n" gud-next)
  ("i" gud-step)
  ("o" gud-finish)
  ("c" gud-cont)
  ("r" gud-run)
  ("Q" gud-disconnect)

  ("ss" nil)
  ("st" nil)
  ("sf" nil)
  ("sl" nil)
  ("sb" nil)
  ("sS" nil)

  ("bt" nil)
  ("bd" gud-remove)
  ("bD" nil)
  ("ba" gud-break)
  ("bc" nil)
  ("bh" nil)
  ("bl" nil)

  ("ee" gud-statement)
  ("er" nil)
  ("es" gud-print)
  ("eii" nil)
  ("eir" nil)
  ("eis" gud-pstar)

  ("dd" my/gdb-mi)
  ("de" nil)

  ("q" nil "quit"))
