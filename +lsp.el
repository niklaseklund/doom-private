;;;  -*- lexical-binding: t; -*-

;; lsp
;; lsp-ui
(with-eval-after-load 'lsp-mode
  (setq lsp-ui-sideline-enable nil
        lsp-ui-sideline-ignore-duplicate t
        lsp-ui-doc-enable nil
        lsp-ui-doc-include-signature nil
        lsp-ui-doc-header nil
        lsp-file-watch-threshold 150000)
  (add-to-list 'lsp-file-watch-ignored "build")

  ;; remote
  (lsp-register-client
   (make-lsp-client :new-connection (lsp-tramp-connection "~/.pyenv/shims/pyls")
                    :major-modes '(python-mode)
                    :remote? t
                    :server-id 'pyls-remote)))



;;
;; Linting
;; Henrik's idea based of major-mode-local-vars-hook implementation
(after! lsp-ui
  (add-hook! 'lsp-ui-mode-hook
    (run-hooks (intern (format "%s-lsp-ui-hook" major-mode)))))

(defun nc/python-flycheck-setup ()
  (flycheck-add-next-checker 'lsp-ui 'python-pylint))
(add-hook 'python-mode-lsp-ui-hook
          #'nc/python-flycheck-setup)

;;
;; Formatting
(add-to-list '+format-on-save-enabled-modes 'python-mode t)

;;
;; Repl
;; Make sure that the virtual environment is used when opening the repl

(defadvice! python-setup-virtual-repl-a (orig-fn &rest args)
  :around #'+python/open-repl
  (if (getenv "VIRTUAL_ENV")
      (let ((python-shell-interpreter (executable-find "python")))
        (apply orig-fn args))
    (apply orig-fn args)))
