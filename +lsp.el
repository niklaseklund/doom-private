;;;  -*- lexical-binding: t; -*-

;; lsp
;; lsp-ui
(with-eval-after-load 'lsp-mode
  (setq lsp-ui-sideline-enable nil
        lsp-ui-sideline-ignore-duplicate t
        lsp-ui-doc-enable nil
        lsp-ui-doc-include-signature nil
        lsp-ui-doc-header nil))
;; company
;; (with-eval-after-load 'company-lsp
  ;; (setq company-lsp-enable-snippet t))
;; lsp-flycheck
(with-eval-after-load 'lsp-mode
  (add-hook 'lsp-after-open-hook (lambda () (lsp-ui-flycheck-enable t)))
  (add-hook 'c-mode-common-hook 'flycheck-mode)) ;; Turn on flycheck for C++ buffers
;; lsp-formatting
(add-hook 'before-save-hook (lambda ()  (when (eq major-mode 'python-mode) (lsp-format-buffer))))

;; (with-eval-after-load 'lsp-mode
;;   (add-hook 'before-save-hook #'lsp-format-buffer))

;; ;; specific language server configurations
;; ;; ;; lsp-python configuration
;; (with-eval-after-load 'lsp-clients
;;   (setq lsp-clients-python-settings
;;         ;; https://github.com/tomv564/LSP/issues/239
;;         (plist-put lsp-clients-python-settings
;;                    :configurationSources ["pycodestyle"])
;;         lsp-clients-python-settings
;;         (plist-put lsp-clients-python-settings
;;                    :plugins.pycodestyle.enabled t)
;;         lsp-clients-python-settings
;;         (plist-put lsp-clients-python-settings
;;                    :plugins.pycodestyle.maxLineLength 80)
;;         ;; lsp-clients-python-settings
;;         ;; (plist-put lsp-clients-python-settings
;;         ;;            :plugins.yapf.enabled nil)
;;         ;; lsp-clients-python-settings
;;         ;; (plist-put lsp-clients-python-settings
;;         ;;            :plugins.jedi_definition.follow_imports t)
;;         ;; lsp-clients-python-settings
;;         ;; (plist-put lsp-clients-python-settings
;;         ;;            :plugins.jedi_definition.follow_builtin_imports t)
;;         ;; lsp-clients-python-settings
;;         ;; (plist-put lsp-clients-python-settings
;;         ;;            :plugins.jedi_signature_help.enabled t)
;;         ;; lsp-clients-python-settings
;;         ;; (plist-put lsp-clients-python-settings
;;         ;;            :plugins.jedi_symbols.enabled t)
;;         ;; lsp-clients-python-settings
;;         ;; (plist-put lsp-clients-python-settings
;;         ;;            :plugins.pydocstyle.matchDir "[^\\.].*")
;;         ;; lsp-clients-python-settings
;;         ;; (plist-put lsp-clients-python-settings
;;         ;;            :plugins.pyls_mypy.live_mode nil)
;;         lsp-clients-python-settings
;;         (plist-put lsp-clients-python-settings
;;                    :plugins.pydocstyle.enabled t)
;;         ;; lsp-clients-python-settings
;;         ;; (plist-put lsp-clients-python-settings
;;         ;;            :plugins.pydocstyle.ignore ["D209"])
;;         ;; lsp-clients-python-settings
;;         ;; (plist-put lsp-clients-python-settings
;;         ;;            :plugins.pydocstyle.ignore ["D209"])
;;         ;; lsp-clients-python-settings
;;         ;; (plist-put lsp-clients-python-settings
;;         ;;            :plugins.pydocstyle.convention "pep257")
;;         ;; lsp-clients-python-settings
;;         ;; (plist-put lsp-clients-python-settings
;;         ;;            :plugins.pydocstyle.select ["D209"])
;;         ))
;; ;; https://github.com/Seeker1911/dotfiles/blob/b28aa7c10c205cb1d3ad94d27787118e0ec68d3b/coc-settings.json

;; https://github.com/nasyxx/emacs.d/blob/f91655b31610cdcff4d896dacea6482862ba8d13/README.org
;; (with-eval-after-load 'lsp-clients
;;   (setq lsp-clients-python-settings
;;         (plist-put lsp-clients-python-settings
;;                    :configurationSources ["pydocstyle" "pycodestyle" "pyflakes" "pylint"])
;;         lsp-clients-python-settings
;;         (plist-put lsp-clients-python-settings
;;                    :plugins.pydocstyle.enabled t)
;;         lsp-clients-python-settings
;;         (plist-put lsp-clients-python-settings
;;                    :plugins.yapf.enabled nil)
;;         lsp-clients-python-settings
;;         (plist-put lsp-clients-python-settings
;;                    :plugins.jedi_definition.follow_imports t)
;;         lsp-clients-python-settings
;;         (plist-put lsp-clients-python-settings
;;                    :plugins.jedi_definition.follow_builtin_imports t)
;;         lsp-clients-python-settings
;;         (plist-put lsp-clients-python-settings
;;                    :plugins.jedi_signature_help.enabled t)
;;         lsp-clients-python-settings
;;         (plist-put lsp-clients-python-settings
;;                    :plugins.jedi_symbols.enabled t)
;;         lsp-clients-python-settings
;;         (plist-put lsp-clients-python-settings
;;                    :plugins.pydocstyle.matchDir "[^\\.].*")
;;         lsp-clients-python-settings
;;         (plist-put lsp-clients-python-settings
;;                    :plugins.pyls_mypy.live_mode nil)))

;; dap-mode
(def-package! dap-mode
  :defer t
  :commands dap-mode
  :after lsp-mode
  :hook
  ((dap-mode . dap-ui-mode))
  :config
  (set-company-backend! 'dap-ui-repl-mode 'company-dap-ui-repl)
  (require 'dap-python))
(add-hook 'python-mode-hook 'dap-mode)


;; The following is from zsxh's configuration found at:
;; https://github.com/zsxh/emacs.d/blob/acb1e760656210de69a93a1a8ab670f7800a96e6/lisp/init-debugger.el
(with-eval-after-load 'dap-mode

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
        (dap-ui-repl))
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
                                   :name "Python :: Run Free"))

  ;; (defvar +dap-running-session-mode-map
  ;;   (let ((map (make-sparse-keymap)))
  ;;     (define-key map (kbd "n") 'dap-next)
  ;;     (define-key map (kbd "i") 'dap-step-in)
  ;;     (define-key map (kbd "o") 'dap-step-out)
  ;;     (define-key map (kbd "c") 'dap-continue)
  ;;     (define-key map (kbd "r") 'dap-restart-frame)
  ;;     (define-key map (kbd "Q") 'dap-disconnect)
  ;;     (define-key map (kbd "b") 'dap-breakpoint-toggle)
  ;;     (define-key map (kbd "B") 'dap-breakpoint-condition)
  ;;     map)
  ;;   "Keybindings for +dap-runnong-session-mode")

  ;; ;; activate minor modes when stepping through code
  ;; ;; https://github.com/emacs-lsp/dap-mode/wiki/How-to-activate-minor-modes-when-stepping-through-code
  ;; (define-minor-mode +dap-running-session-mode
  ;;   "A mode for adding keybindings to running sessions"
  ;;   nil
  ;;   nil
  ;;   +dap-running-session-mode-map
  ;;   (with-eval-after-load 'evil
  ;;     (evil-normalize-keymaps) ;; if you use evil, this is necessary to update the keymaps
  ;;     (evil-make-overriding-map +dap-running-session-mode-map))
  ;;   ;; The following code adds to the dap-terminated-hook
  ;;   ;; so that this minor mode will be deactivated when the debugger finishes
  ;;   (when +dap-running-session-mode
  ;;     (let ((session-at-creation (dap--cur-active-session-or-die)))
  ;;       (add-hook 'dap-terminated-hook
  ;;                 (lambda (session)
  ;;                   (when (eq session session-at-creation)
  ;;                     (+dap-running-session-mode -1)))))))

  ;; ;; Activate this minor mode when dap is initialized
  ;; (add-hook 'dap-session-created-hook '+dap-running-session-mode)

  ;; ;; Activate this minor mode when hitting a breakpoint in another file
  ;; (add-hook 'dap-stopped-hook '+dap-running-session-mode)

  ;; ;; Activate this minor mode when stepping into code in another file
  ;; (add-hook 'dap-stack-frame-changed-hook (lambda (session)
  ;;                                           (when (dap--session-running session)
  ;;                                             (+dap-running-session-mode 1))))
)


;; Realgud
;; (defun my/realgud-gdb (binary-file)
;;   (projectile-expand-root binary-file))
