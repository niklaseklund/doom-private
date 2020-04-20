;;; +code.el -*- lexical-binding: t; -*-

;;; Description
;; This file contains packages and configuration related to coding. DOOM already
;; provides a good amount of support for programming but here I store my
;; customizations.


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
;;; DAP


;;
;;; Python

;; Lint
(after! lsp-ui
  (add-hook! 'lsp-ui-mode-hook
    (run-hooks (intern (format "%s-lsp-ui-hook" major-mode)))))

(defun +python-flycheck-setup-h ()
  (flycheck-add-next-checker 'lsp-ui 'python-pylint))
(add-hook 'python-mode-lsp-ui-hook
          #'+python-flycheck-setup-h)

;; Format
(add-to-list '+format-on-save-enabled-modes 'python-mode t)

;; Debug
(defadvice! +dap-python-poetry-executable-find-a (orig-fn &rest args)
  "Use the Python binary from the current virtual environment."
  :around #'dap-python--pyenv-executable-find
  (if (getenv "VIRTUAL_ENV")
      (executable-find (car args))
    (apply orig-fn args)))

;; REPL
(defadvice! +python-poetry-open-repl-a (orig-fn &rest args)
  "Use the Python binary from the current virtual environment."
  :around #'+python/open-repl
  (if (getenv "VIRTUAL_ENV")
      (let ((python-shell-interpreter (executable-find "python")))
        (apply orig-fn args))
    (apply orig-fn args)))


;;
;;; Common Lisp
(after! sly
  (add-to-list 'sly-contribs 'sly-retro nil #'eq))


;;
;;; Emacs Lisp
(add-hook 'ielm-mode-hook 'lispy-mode)
