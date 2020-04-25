;;; +code.el -*- lexical-binding: t; -*-

;;
;;; LSP
(with-eval-after-load 'lsp-mode
  (setq lsp-ui-sideline-enable nil
        lsp-ui-sideline-ignore-duplicate t
        lsp-ui-doc-enable nil
        lsp-ui-doc-include-signature nil
        lsp-ui-doc-header nil
        lsp-file-watch-threshold 150000)
  (add-to-list 'lsp-file-watch-ignored "build")

  ;; Remote Python client
  (lsp-register-client
   (make-lsp-client :new-connection (lsp-tramp-connection "~/.pyenv/shims/pyls")
                    :major-modes '(python-mode)
                    :remote? t
                    :server-id 'pyls-remote)))

;;
;;; Python
(defadvice! +dap-python-poetry-executable-find-a (orig-fn &rest args)
  "Use the Python binary from the current virtual environment."
  :around #'dap-python--pyenv-executable-find
  (if (getenv "VIRTUAL_ENV")
      (executable-find (car args))
    (apply orig-fn args)))

(defadvice! +python-poetry-open-repl-a (orig-fn &rest args)
  "Use the Python binary from the current virtual environment."
  :around #'+python/open-repl
  (if (getenv "VIRTUAL_ENV")
      (let ((python-shell-interpreter (executable-find "python")))
        (apply orig-fn args))
    (apply orig-fn args)))

;;
;;; Emacs Lisp
(add-hook 'ielm-mode-hook 'lispy-mode)

;;
;; C++
(add-to-list '+format-on-save-enabled-modes 'c++-mode t)
